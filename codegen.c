//
// Created by WTF on 2020/2/12.
//

#include "gemcc.h"

typedef enum Reg Reg;
enum Reg{
    RAX, RDI, RSI, RDX,
    RCX, RBX, R8, R9,
    R12, R13, R14, R15
};

static Reg argReg[] = {RDI, RSI, RDX, RCX, R8, R9};
static Reg storageReg[] = {RBX, R12, R13, R14, R15};
int regCount;   // record the use of storageReg

static char *regArray[][4] = {"al", "ax", "eax", "rax",
                              "dil", "di", "edi", "rdi",
                              "sil", "si", "esi", "rsi",
                              "dl", "dx", "edx", "rdx",
                              "cl", "cx", "ecx", "rcx",
                              "bl", "bx", "ebx", "rbx",
                              "r8b", "r8w", "r8d", "r8",
                              "r9b", "r9w", "r9d", "r9",
                              "r12b", "r12w", "r12d", "r12",
                              "r13b", "r13w", "r13d", "r13",
                              "r14b", "r14w", "r14d", "r14",
                              "r15b", "r15w", "r15d", "r15"};

Var *g_func;
int retLabel;
int breakLabel;
int continueLabel;
bool breakUsed;

typedef struct Context Context;
struct Context{
    int breakLabel;
    int continueLabel;
    bool breakUsed;
};

int convSizeIndex(int size){
    switch (size){
        case 1: return 0;
        case 2: return 1;
        case 4: return 2;
        case 8: return 3;
        default: assert(false);
    }
}

char *getReg(Reg reg, Type *type){
    int selector = convSizeIndex(type->size);
    return regArray[reg][selector];
}

char *allocStorageReg(Type *type){
    int total = sizeof(storageReg) / sizeof(storageReg[0]);
    if(regCount >= total){
        printf("  push r15\n");
        ++regCount;
        return getReg(R15, type);   // the last reg
    }
    return getReg(storageReg[regCount++], type);
}

char *freeStorageReg(Type *type){
    assert(regCount != 0);
    int total = sizeof(storageReg) / sizeof(storageReg[0]);
    type = type ? type : long_type;
    --regCount;
    if(regCount >= total){
        printf("  pop r15\n");
        return getReg(R15, type);
    }
    if(regCount > 0){
        return getReg(storageReg[regCount - 1], type);
    }
    else{
        return NULL;
    }
}

char *getArgReg(int index, Type *type){
    assert(index < sizeof(argReg) / sizeof(argReg[0]));
    return getReg(argReg[index], type);
}

int getLabelSeq(){
    static int a = 2;
    return a++;
}

void putLabel(int seq){
    printf(".L%d:\n", seq);
}

Context *newContext(){
    Context *context = calloc(1, sizeof(Context));
    context->breakLabel = breakLabel;
    context->continueLabel = continueLabel;
    context->breakUsed = breakUsed;
    breakLabel = getLabelSeq();
    continueLabel = getLabelSeq();
    return context;
}

void restoreContext(Context *context){
    breakLabel = context->breakLabel;
    continueLabel = context->continueLabel;
}

void jmp(int seq){
    printf("  jmp .L%d\n", seq);
}

void je(int seq){
    printf("  je .L%d\n", seq);
}

void jne(int seq){
    printf("  jne .L%d\n", seq);
}

static void inc(Type *ty) {
    if(ty->kind == TY_BOOL){
        printf("  mov al, 1\n");
        return;
    }
    printf("  add %s, %d\n",
           getReg(RAX, ty), ty->basetype ? ty->basetype->size : 1);
}

static void dec(Type *ty) {
    if(ty->kind == TY_BOOL){
        printf("  mov al, 0\n");
        return;
    }
    printf("  sub %s, %d\n",
           getReg(RAX, ty), ty->basetype ? ty->basetype->size : 1);
}

static void addNum(Type *ty, long num){
    printf("  add %s, %ld\n",
           getReg(RAX, ty), num);
}

static void not(Type *ty){
    printf("  not %s\n", getReg(RAX, ty));
}

static void cmp(char *oprand1, char *oprand2){
    printf("  cmp %s, %s\n", oprand1, oprand2);
}

static void cmpz(char *oprand1){
    printf("  cmp %s, 0\n", oprand1);
}

static void sete(){
    printf("  sete al\n");
}

static void setne(){
    printf("  setne al\n");
}

void initData(Type *type, Initializer *init){
    if(!isComplexType(type->kind)){
        if (init->label){
            if(init->addend){
                printf("  .quad %s%+ld\n", init->label, init->addend);
            }
            else{
                printf("  .quad %s\n", init->label);
            }
        }
        else if (type->size == 1)
            printf("  .byte %ld\n", init->val);
        else
            printf("  .%dbyte %ld\n", type->size, init->val);
        return;
    }
    else{
        if(type->kind == TY_ARRAY){
            Initializer *nested = init->nestedInit;
            int i;
            for(i = 0; i < type->arrayLen && nested; ++i){
                initData(type->basetype, nested);
                nested = nested->next;
            }
            if(i < type->arrayLen){
                printf("  .zero %d\n", type->basetype->size * (type->arrayLen - i));
            }
        }
        else if(type->kind == TY_STRUCT){
            Initializer *nested = init->nestedInit;
            VarList *mems = type->members;
            int offset = 0;
            while(nested){
                assert(mems);
                initData(mems->var->type, nested);
                nested = nested->next;
                offset = mems->var->offset + mems->var->type->size;
                VarList *prev = mems;
                mems = mems->next;
                if(mems){
                    int diff = mems->var->offset - offset;
                    if(diff){
                        printf("  .zero %d\n", diff);
                    }
                }
            }
            printf("  .zero %d\n", type->size - offset);
        }
        else if(type->kind == TY_UNION){
            initData(type->members->var->type, init);
        }
        else{
            assert(false);
        }
    }
}

void emitData(Program *prog){
    for (VarList *vl = prog->globals; vl; vl = vl->next){
        assert(vl->var->storage == STATIC || vl->var->storage == UNSPECIFIED);
        if (vl->var->storage == UNSPECIFIED){
            printf(".global %.*s\n", vl->var->len, vl->var->pos);
        }
    }

    printf(".bss\n");
    for (VarList *vl = prog->globals; vl; vl = vl->next) {
        Var *var = vl->var;
        if (!var->init){
            printf(".align %d\n", var->type->align);
            if(var->label){
                printf("%s:\n", var->label);
            }
            else{
                printf("%.*s:\n", var->len, var->pos);
            }
            printf("  .zero %d\n", var->type->size);
        }
    }

    printf(".data\n");
    for (VarList *vl = prog->globals; vl; vl = vl->next) {
        Var *var = vl->var;
        if (var->init){
            printf(".align %d\n", var->type->align);
            if(var->label){
                printf("%s:\n", var->label);
            }
            else{
                printf("%.*s:\n", var->len, var->pos);
            }
            initData(var->type, var->init);
        }
    }
}

void gen(Node *node);

int countArg(Node *arg){
    int cnt = 0;
    while(arg){
        ++cnt;
        arg = arg->next;
    }
    return cnt;
}

void cast(Type *newType, Type *oldType){
    if(isArrayOrFunc(oldType)){
        if(newType->kind == TY_BOOL){
            // warning: Address of 'a' will always evaluate to 'true'
            printf("  mov al, 1\n");
        }
        return;
    }

    if(newType->kind == TY_BOOL){
        cmp(getReg(RAX, oldType), "0");
        setne();
        return;
    }

    if(oldType->size >= newType->size){
        return;
    }

    if (oldType->size == 1) {
        printf("  movsx %s, al\n", getReg(RAX, newType));
    }
    else if (oldType->size == 2) {
        printf("  movsx %s, ax\n", getReg(RAX, newType));
    }
    else if (oldType->size == 4) {
        printf("  movsxd rax, eax\n");
    }
    else {
        assert(false);
    }
}

static void load(Type *ty) {
    if(ty->kind == TY_FUNCTION || ty->kind == TY_ARRAY){
        return;
    }

    if (ty->size == 1) {
        printf("  mov al, [rax]\n");
    }
    else if (ty->size == 2) {
        printf("  mov ax, [rax]\n");
    }
    else if (ty->size == 4) {
        printf("  mov eax, [rax]\n");
    }
    else {
        assert(ty->size == 8);
        printf("  mov rax, [rax]\n");
    }
}

static void store(Type *ty) {
    if (ty->size == 1) {
        printf("  mov [rdi], al\n");
    }
    else if (ty->size == 2) {
        printf("  mov [rdi], ax\n");
    }
    else if (ty->size == 4) {
        printf("  mov [rdi], eax\n");
    }
    else {
        assert(ty->size == 8);
        printf("  mov [rdi], rax\n");
    }
}

static void mov(char *oprand1, char *oprand2){
    printf("  mov %s, %s\n", oprand1, oprand2);
}

static void add(char *oprand1, char *oprand2){
    printf("  add %s, %s\n", oprand1, oprand2);
}

static void sub(char *oprand1, char *oprand2){
    printf("  sub %s, %s\n", oprand1, oprand2);
}

static void and(char *oprand1, char *oprand2){
    printf("  and %s, %s\n", oprand1, oprand2);
}

static void xor(char *oprand1, char *oprand2){
    printf("  and %s, %s\n", oprand1, oprand2);
}

static void or(char *oprand1, char *oprand2){
    printf("  and %s, %s\n", oprand1, oprand2);
}

static void xchg(char *oprand1, char *oprand2){
    printf("  xchg %s, %s\n", oprand1, oprand2);
}

static void idiv(char *oprand1){
    printf("  idiv %s\n", oprand1);
}

static void imul(char *oprand1, char *oprand2){
    printf("  imul %s, %s\n", oprand1, oprand2);
}

void genaddr(Node *node) {
    if(node->var->type->kind != TY_FUNCTION){
        if(node->var->scopeDepth == 0){
            if(node->var->label){
                printf("  lea rax, [rip + %s]\n", node->var->label);
            }
            else{
                printf("  lea rax, [rip + %.*s]\n", node->var->len, node->var->pos);
            }
        }
        else{
            printf("  lea rax, [rbp - %d]\n", node->var->offset);
        }
    }
    else{
        if(node->var->storage == STATIC){
            assert(node->var->type->isComplete);
            printf("  lea rax, [rip + %.*s]\n", node->var->len, node->var->pos);
        }
        else{
            assert(node->var->storage == EXTERN);
            if(node->var->type->isComplete){
                printf("  lea rax, [rip + %.*s]\n", node->var->len, node->var->pos);
            }
            else {
                printf("  lea rax, [rip + %.*s@PLT]\n", node->var->len, node->var->pos);
            }
        }
    }
}

void expandDividend(Type *type){
    if(type->size == 1){
        printf("  cbw\n");
    }
    else if(type->size == 2){
        printf("  cwd\n");
        printf("  mov edx, eax\n");
        printf("  ror edx, 16\n");
    }
    else if(type->size == 4){
        printf("  cdq\n");
        printf("  mov rdx, rax\n");
        printf("  ror rdx, 32\n");
    }
    else {
        assert(type->size == 8);
        printf("  cqo\n");
    }
}

void genlval(Node *node){
    switch (node->kind){
        case ND_MEMBER_DOT:
            genlval(node->lhs);
            addNum(long_type, node->member->offset);
            return;
        case ND_MEMBER_PTR:
            gen(node->lhs);
            addNum(long_type, node->member->offset);
            return;
        case ND_DEREF:
            gen(node->lhs);
            return;
        case ND_VAR:
            genaddr(node);
            return;
        default:
            assert(false);
    }
}

void gen(Node *node){
    assert(node);
    int labelSeq, elseLabel, endLabel, condLabel, thenLabel;
    Context *oldContext;
    char *reg;
    Node *lhs = node->lhs;
    Node *rhs = node->rhs;
    int saveBreakSeq, saveContinueSeq;
    switch (node->kind){
        case ND_NUM:
            if(node->type->size == 1) {
                if(node->type->kind == TY_BOOL){
                    printf("  mov al, %d\n", (_Bool)node->val);
                } else{
                    assert(node->type->kind == TY_CHAR);
                    printf("  mov al, %d\n", (char)node->val);
                }
            } else if(node->type->size == 2){
                printf("  mov ax, %d\n", (short)node->val);
            }
            else if(node->type->size == 4){
                printf("  mov eax, %d\n", (int)node->val);
            }
            else {
                assert(node->type->size == 8);
                printf("  mov rax, %ld\n", node->val);
            }
            return;
        case ND_VAR:
            genlval(node);
            load(node->type);
            return;
        case ND_CALL:
        {
            Node *args = node->args;
            int count = countArg(args);
            int savedCount = count;
            gen(lhs);     // func address
            printf("  mov %s, rax\n", allocStorageReg(long_type));
            while(count > 6){
                gen(args);
                printf("  push rax\n");
                args = args->next;
                --count;
            }
            Node **argsArr = calloc(count, sizeof(Node *));
            char *regStr = NULL;
            while(count > 0){
                gen(args);
                regStr = allocStorageReg(args->type);
                printf("  mov %s, %s\n", regStr, getReg(RAX, args->type));
                argsArr[count - 1] = args;
                args = args->next;
                --count;
            }
            count = savedCount > 6 ? 6 : savedCount;
            for(int i = 0; i < count; ){
                printf("  mov %s, %s\n", getArgReg(i, argsArr[i]->type), regStr);
                ++i;
                if(i < count){
                    regStr = freeStorageReg(argsArr[i]->type);
                }
            }
            printf("  call %s\n", freeStorageReg(long_type));
            if(savedCount > 6){
                printf("  add rsp, %d\n", (savedCount - 6) * 8);
            }
            return;
        }
        case ND_MEMBER_DOT:
        case ND_MEMBER_PTR:
            genlval(node);
            load(node->type);
            return;
        case ND_POS:
        case ND_NEG:
        case ND_BITNOT:
            gen(lhs);
            not(node->type);
            return;
        case ND_PRE_INC:
            genlval(node);
            mov("rdi", "rax");
            load(lhs->type);
            inc(lhs->type);
            store(lhs->type);
            return;
        case ND_POST_INC:
            genlval(node);
            mov("rdi", "rax");
            load(lhs->type);
            mov("rsi", "rax");
            inc(lhs->type);
            store(lhs->type);
            mov("rax", "rsi");
            return;
        case ND_PRE_DEC:
            genlval(node);
            mov("rdi", "rax");
            load(lhs->type);
            dec(lhs->type);
            store(lhs->type);
            return;
        case ND_POST_DEC:
            genlval(node);
            mov("rdi", "rax");
            load(lhs->type);
            mov("rsi", "rax");
            dec(lhs->type);
            store(lhs->type);
            mov("rax", "rsi");
            return;
        case ND_DEREF:
            genlval(node);
            load(node->type);
            return;
        case ND_ADDR:
            genlval(lhs);
            return;
        case ND_LOGNOT:
            gen(lhs);
            cmp(getReg(RAX, lhs->type), "0");
            sete();
            printf("  movzx eax, al\n");
            return;
        case ND_CAST:
            if(isArrayOrFunc(lhs->type)){
                if(lhs->type->kind == TY_ARRAY){
                    assert(equalType(node->type->basetype, lhs->type->basetype));
                }
                else{
                    assert(lhs->type->kind == TY_FUNCTION);
                    assert(equalType(node->type->basetype, lhs->type));
                }
            }
            gen(lhs);
            cast(node->type, lhs->type);
            return;
        case ND_DIV:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            xchg(reg, getReg(RAX, node->type));
            expandDividend(node->type);
            printf("  idiv %s\n", reg);
            freeStorageReg(NULL);
            return;
        case ND_MOD:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            xchg(reg, getReg(RAX, node->type));
            expandDividend(node->type);
            printf("  idiv %s\n", reg);
            if(node->type->size == 4){
                printf("  ror rax, 32\n");
            }
            else{
                assert(node->type->size == 8);
                printf("  mov rax, rdx\n");
            }
            freeStorageReg(NULL);
            return;
        case ND_MUL:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            printf("  imul %s, %s\n", getReg(RAX, node->type), reg);
            freeStorageReg(NULL);
            return;
        case ND_ADD:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            if(isAddressType(lhs->type)){
                printf("  imul rax, %d\n", node->type->basetype->size);
            }
            else if(isAddressType(rhs->type)){
                printf("  imul %s, %d\n", reg, node->type->basetype->size);
            }
            printf("  add %s, %s\n", getReg(RAX, node->type), reg);
            freeStorageReg(NULL);
            return;
        case ND_SUB:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            if(isAddressType(lhs->type)){
                if(isAddressType(rhs->type)) {
                    assert(equalType(lhs->type, rhs->type));
                    printf("  sub %s, %s\n", reg, "rax");
                    mov("rax", reg);
                    printf("  mov %s, %s\n", "rax", reg);
                    printf("  mov edi, %d\n", lhs->type->basetype->size);
                    printf("  idiv edi\n");
                    printf("  cdq\n");
                }
                else{
                    printf("  mov rdi, %d\n", node->type->basetype->size);
                    printf("  imul rax, rdi\n");
                    printf("  sub %s, %s\n", reg, "rax");
                    mov("rax", reg);
                }
            }
            else{
                printf("  sub %s, %s\n", reg, getReg(RAX, node->type));
                printf("  mov %s, %s\n", getReg(RAX, node->type), reg);
            }
            freeStorageReg(NULL);
            return;
        case ND_SHL:
        case ND_SHR:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            printf("  mov cl, al\n");
            switch (node->kind){
                case ND_SHL:
                    printf("  sal %s, cl\n", reg);
                    break;
                case ND_SHR:
                    printf("  sar %s, cl\n", reg);
                    break;
                default:
                    assert(false);
            }
            printf("  mov %s, %s\n", getReg(RAX, node->type), reg);
            freeStorageReg(NULL);
            return;
        case ND_GT:
        case ND_GE:
        case ND_LT:
        case ND_LE:
        case ND_EQ:
        case ND_NEQ:
            gen(lhs);
            reg = allocStorageReg(node->type);
            mov(reg, getReg(RAX, node->type));
            gen(rhs);
            printf("  cmp %s, %s\n", reg, getReg(RAX, lhs->type));
            switch (node->kind){
                case ND_GT:
                    printf("  setg al\n");
                    break;
                case ND_GE:
                    printf("  setge al\n");
                    break;
                case ND_LT:
                    printf("  setl al\n");
                    break;
                case ND_LE:
                    printf("  setle al\n");
                    break;
                case ND_EQ:
                    printf("  sete al\n");
                    break;
                case ND_NEQ:
                    printf("  setne al\n");
                    break;
                default:
                    assert(false);
            }
            printf("  movzx eax, al\n");
            freeStorageReg(NULL);
            return;
        case ND_BITAND:
        case ND_BITXOR:
        case ND_BITOR:
            gen(lhs);
            reg = allocStorageReg(node->type);
            printf("  mov %s, %s\n", reg, getReg(RAX, node->type));
            gen(rhs);
            switch (node->kind){
                case ND_BITAND:
                    and(getReg(RAX, node->type), reg);
                    break;
                case ND_BITXOR:
                    xor(getReg(RAX, node->type), reg);
                    break;
                case ND_BITOR:
                    or(getReg(RAX, node->type), reg);
                    break;
                default:
                    assert(false);
            }
            freeStorageReg(NULL);
            return;
        case ND_LOGAND:
        case ND_LOGOR:
            labelSeq = getLabelSeq();
            gen(lhs);
            printf("  cmp %s, 0\n", getReg(RAX, lhs->type));
            switch (node->kind){
                case ND_LOGAND:
                    je(labelSeq);
                    break;
                case ND_LOGOR:
                    jne(labelSeq);
                    break;
                default:
                    assert(false);
            }
            gen(rhs);
            printf("  cmp %s, 0\n", getReg(RAX, rhs->type));
            putLabel(labelSeq);
            printf("  setne al\n");
            printf("  movzx eax, al\n");
            return;
        case ND_COND:
            elseLabel = getLabelSeq();
            endLabel = getLabelSeq();
            gen(node->cond);
            printf("  cmp %s, 0\n", getReg(RAX, node->cond->type));
            printf("  je .L%d\n", elseLabel);
            gen(node->then);
            printf("  jmp .L%d\n", endLabel);
            putLabel(elseLabel);
            gen(node->els);
            putLabel(endLabel);
            return;
        case ND_ASSIGN:
            assert(lhs->type->size == rhs->type->size);
            genlval(lhs);
            reg = allocStorageReg(long_type);
            printf("  mov %s, rax\n", reg);
            gen(rhs);
            printf("  mov [%s], %s\n", reg, getReg(RAX, node->type));
            freeStorageReg(NULL);
            return;
        case ND_ADD_ASSIGN:
            genlval(lhs);
            reg = allocStorageReg(long_type);
            printf("  mov %s, rax\n", reg);
            gen(rhs);

            if(isAddressType(lhs->type)){
                assert(rhs->type == long_type);
                printf("  imul rax, %d\n", lhs->type->basetype->size);
                mov("rsi", "rax");
                mov("rax", reg);
                load(lhs->type);
            }
            else if(isAddressType(rhs->type)){
                mov("rsi", "rax");
                mov("rax", reg);
                load(lhs->type);
                cast(long_type, lhs->type);
                printf("  imul rax, %d\n", rhs->type->basetype->size);
            }
            else{
                // attention: rhs already cast
                mov(getReg(RSI, rhs->type), getReg(RAX, rhs->type));
                mov("rax", reg);
                load(lhs->type);
                cast(rhs->type, lhs->type);
            }
            add(getReg(RAX, rhs->type), getReg(RSI, rhs->type));
            cast(node->type, rhs->type);
            mov("rdi", reg);
            store(node->type);
            freeStorageReg(NULL);
            return;
        case ND_SUB_ASSIGN:
            genlval(lhs);
            reg = allocStorageReg(long_type);
            mov(reg, "rax");
            gen(rhs);
            if(isAddressType(lhs->type)){
                if(isAddressType(rhs->type)){
                    mov("rsi", "rax");
                    mov("rax", reg);
                    load(lhs->type);
                    sub("rax", "rsi");
                    expandDividend(long_type);
                    printf("  mov rsi, %d\n", lhs->type->basetype->size);
                    idiv("rsi");
                    mov("rdi", reg);
                }
                else{
                    printf("  imul rax, %d\n", lhs->type->basetype->size);
                    mov("rsi", "rax");
                    mov("rax", reg);
                    load(lhs->type);
                    sub("rax", "rsi");
                    mov("rdi", reg);
                }
            }
            else{
                assert(!isAddressType(rhs->type));
                mov(getReg(RSI, rhs->type), getReg(RAX, rhs->type));
                mov("rax", reg);
                load(lhs->type);
                cast(rhs->type, lhs->type);
                sub(getReg(RAX, rhs->type), getReg(RSI, rhs->type));
                cast(node->type, rhs->type);
                mov("rdi", reg);
            }
            store(node->type);
            freeStorageReg(NULL);
            return;
        case ND_MUL_ASSIGN:
        case ND_DIV_ASSIGN:
        case ND_MOD_ASSIGN:
        case ND_SHL_ASSIGN:
        case ND_SHR_ASSIGN:
        case ND_BITAND_ASSIGN:
        case ND_BITXOR_ASSIGN:
        case ND_BITOR_ASSIGN:
            genlval(lhs);
            reg = allocStorageReg(long_type);
            mov(reg, "rax");
            gen(rhs);
            mov(getReg(RSI, rhs->type), getReg(RAX, rhs->type));
            mov("rax", reg);
            load(lhs->type);
            cast(rhs->type, lhs->type);
            switch (node->kind){
                case ND_MUL_ASSIGN:
                    imul(getReg(RAX, rhs->type), getReg(RSI, rhs->type));
                    break;
                case ND_DIV_ASSIGN:
                    expandDividend(rhs->type);
                    idiv(getReg(RSI, rhs->type));
                    break;
                case ND_MOD_ASSIGN:
                    expandDividend(rhs->type);
                    idiv(getReg(RSI, rhs->type));
                    if(rhs->type->size == 4){
                        printf("  ror rax, 32\n");
                    }
                    else{
                        assert(rhs->type->size == 8);
                        printf("  mov rax, rdx\n");
                    }
                    break;
                case ND_SHL_ASSIGN:
                    mov("cl", "sil");
                    printf("  sal %s, cl\n", getReg(RAX, rhs->type));
                    break;
                case ND_SHR_ASSIGN:
                    mov("cl", "sil");
                    printf("  sar %s, cl\n", getReg(RAX, rhs->type));
                    break;
                case ND_BITAND_ASSIGN:
                    and(getReg(RAX, rhs->type), getReg(RSI, rhs->type));
                    break;
                case ND_BITXOR_ASSIGN:
                    xor(getReg(RAX, rhs->type), getReg(RSI, rhs->type));
                    break;
                case ND_BITOR_ASSIGN:
                    or(getReg(RAX, rhs->type), getReg(RSI, rhs->type));
                    break;
                default:
                    assert(false);
            }
            cast(node->type, rhs->type);
            store(node->type);
            freeStorageReg(NULL);
            return;
        case ND_COMMA:
            gen(lhs);
            gen(rhs);
            return;
        case ND_IF:
            elseLabel = getLabelSeq();
            endLabel = getLabelSeq();
            gen(node->cond);
            if(node->then){
                cmpz(getReg(RAX, node->cond->type));
                je(elseLabel);
                gen(node->then);
                if(node->els){
                    jmp(endLabel);
                }
                putLabel(elseLabel);
                if(node->els){
                    gen(node->els);
                    putLabel(endLabel);
                }
            }
            return;
        case ND_DO:
            thenLabel = getLabelSeq();
            oldContext = newContext();
            if(node->then){
                putLabel(thenLabel);
                gen(node->then);
            }
            putLabel(continueLabel);
            gen(node->cond);
            cmpz(getReg(RAX, node->cond->type));
            if(node->then){
                jne(thenLabel);
            }
            else{
                jne(continueLabel);
            }
            if(breakUsed){
                putLabel(breakLabel);
            }
            restoreContext(oldContext);
            return;
        case ND_FOR:
            if(node->init){
                gen(node->init);
            }
            thenLabel = getLabelSeq();
            oldContext = newContext();
            if(node->then){
                jmp(continueLabel);
                putLabel(thenLabel);
                gen(node->then);
                if(node->incre){
                    gen(node->incre);
                }
            }
            putLabel(continueLabel);
            gen(node->cond);
            cmpz(getReg(RAX, node->cond->type));
            if(node->then){
                jne(thenLabel);
            }
            else{
                jne(continueLabel);
            }
            if(breakUsed){
                putLabel(breakLabel);
            }
            restoreContext(oldContext);
            return;
        case ND_WHILE:
            thenLabel = getLabelSeq();
            oldContext = newContext();
            if(node->then){
                jmp(continueLabel);
                putLabel(thenLabel);
                gen(node->then);
            }
            putLabel(continueLabel);
            gen(node->cond);
            cmpz(getReg(RAX, node->cond->type));
            if(node->then){
                jne(thenLabel);
            }
            else{
                jne(continueLabel);
            }
            if(breakUsed){
                putLabel(breakLabel);
            }
            restoreContext(oldContext);
            return;
        case ND_SWITCH:
            gen(node->cond);
            oldContext = newContext();
            Node *exprs = node->then;
            Node *defaultCase = NULL;
            while(exprs) {
                if (exprs->kind == ND_CASE || exprs->kind == ND_DEFAULT) {
                    exprs->labelSeq = getLabelSeq();
                    if (exprs->kind == ND_CASE) {
                        switch (node->cond->type->kind) {
                            case TY_BOOL:
                                printf("  cmp al, %d\n", (bool) exprs->val);
                            case TY_CHAR:
                                printf("  cmp al, %d\n", (char) exprs->val);
                            case TY_SHORT:
                                printf("  cmp ax, %d\n", (short) exprs->val);
                            case TY_INT:
                                printf("  cmp eax, %d\n", (int) exprs->val);
                            case TY_LONG:
                                printf("  cmp rax, %ld\n", exprs->val);
                            default:
                                assert(false);
                        }
                        je(exprs->labelSeq);
                    }
                    else if (exprs->kind == ND_DEFAULT) {
                        defaultCase = exprs;
                    }
                }
                exprs = exprs->next;
            }

            if (defaultCase) {
                jmp(defaultCase->labelSeq);
            }

            exprs = node->then;
            while(exprs){
                gen(exprs);
                exprs = exprs->next;
            }
            if(breakUsed){
                putLabel(breakLabel);
            }
            restoreContext(oldContext);
            return;
        case ND_GOTO:
        {
            Node *label = findLabel(g_func->exprs, node->pos, node->len);
            assert(label);
            if(label->labelSeq < 2){
                label->labelSeq = getLabelSeq();
            }
            jmp(label->labelSeq);
            return;
        }
        case ND_RETURN:
            assert(retLabel >= 2);
            if(lhs){
                gen(lhs);
            }
            jmp(retLabel);
            return;
        case ND_BREAK:
            assert(breakLabel >= 2);
            jmp(breakLabel);
            breakUsed = true;
            return;
        case ND_CONTINUE:
            assert(continueLabel >= 2);
            jmp(continueLabel);
            return;
        case ND_LABEL:
            if(node->labelSeq < 2){
                node->labelSeq = getLabelSeq();
            }
            putLabel(node->labelSeq);
            return;
    }
}

void emitText(Program *prog){
    // align
    for (VarList *funcList = prog->functions; funcList; funcList = funcList->next) {
        Var *func = funcList->var;
        int count = func->type->argc >= 6 ? 6 : func->type->argc;
        int offset = func->type->hasVarArgs ? 48 : count * 8;
        for (VarList *vl = func->locals; vl; vl = vl->next) {
            Var *var = vl->var;
            offset += var->type->size;
            offset = alignTo(offset, var->type->align);
            var->offset = offset;
        }
        func->stackSize = alignTo(offset, 8);
    }

    printf(".text\n");
    for (VarList *funcList = prog->functions; funcList; funcList = funcList->next) {
        g_func = funcList->var;
        retLabel = getLabelSeq();
        if (g_func->storage == EXTERN){
            printf(".global %.*s\n", g_func->len, g_func->pos);
        }
        printf("%.*s:\n", g_func->len, g_func->pos);

        printf("  push rbp\n");
        printf("  push rbx\n");
        printf("  push r12\n");
        printf("  push r13\n");
        printf("  push r14\n");
        printf("  push r15\n");
        printf("  mov rbp, rsp\n");
        printf("  sub rsp, %d\n", g_func->stackSize);
        if (g_func->type->hasVarArgs) {
            printf("  mov [rbp - 48], rdi\n");
            printf("  mov [rbp - 40], rsi\n");
            printf("  mov [rbp - 32], rdx\n");
            printf("  mov [rbp - 24], rcx\n");
            printf("  mov [rbp - 16], r8\n");
            printf("  mov [rbp - 8], r9\n");
        }
        else{
            VarList *params = g_func->type->params;
            int index = 0;
            while(params && index < 6){
                printf("  mov [rbp - %d], %s\n", params->var->offset, getArgReg(index, params->var->type));
                ++index;
                params = params->next;
            }
        }

        for (Node *node = g_func->exprs; node; node = node->next){
            gen(node);
        }

        putLabel(retLabel);
        printf("  mov rsp, rbp\n");
        printf("  pop r15\n");
        printf("  pop r14\n");
        printf("  pop r13\n");
        printf("  pop r12\n");
        printf("  pop rbx\n");
        printf("  pop rbp\n");
        printf("  ret\n");
    }
}

void codegen(Program *prog){
    printf(".intel_syntax noprefix\n");
    emitData(prog);
    emitText(prog);
}
