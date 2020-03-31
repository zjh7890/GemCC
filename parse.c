#pragma clang diagnostic push
#pragma ide diagnostic ignored "hicpp-signed-bitwise"
#include "gemcc.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wparentheses"
VarList *globals;
VarList *functions;

VarList *varList;
Type *tagList;
int scopeDepth;

int couldBreak;
int couldContinue;

typedef struct Scope Scope;
struct Scope{
    VarList *varScope;
    Type *tagScope;
};

Type *handleDeclarator(Type *basetype, Var *output);
void handleDeclaration(Var *func);
Type *getBaseType();
Node *unary();
Initializer *handleInit(Type *type, bool outlayer);
void handleExpression(Var *func);

Scope *enterScope(){
    Scope *sc = calloc(1, sizeof(Scope));
    sc->varScope = varList;
    sc->tagScope = tagList;
    ++scopeDepth;
    return sc;
}

void leaveScope(Scope *sc){
    varList = sc->varScope;
    tagList = sc->tagScope;
    --scopeDepth;
    assert(scopeDepth >= 0);
}

Type *newTag(TypeKind kind, char *pos, int len){
    Type *type = calloc(1, sizeof(Type));
    type->kind = kind;
    type->pos = pos;
    type->len = len;
    type->scopeDepth = scopeDepth;
    return type;
}

Var *newVar(Type *ty, char *pos, int len){
    Var *var = calloc(1, sizeof(Var));
    var->type = ty;
    var->pos = pos;
    var->len = len;
    return var;
}

Var *findVar(VarList *list, char *pos, int len){
    while(list){
        Var *var = list->var;
        if(var->len == len && !strncmp(var->pos, pos, len)){
            return var;
        }
        list = list->next;
    }
    return NULL;
}

char *newLabel(){
    static int count = 0;
    char buf[20];
    sprintf(buf, "L.data.%d", count++);
    return strndup(buf, sizeof(buf));
}

char *getLabel(Var *var){
    assert(var);
    if(var->label){
        return var->label;
    }

    return strndup(var->pos, var->len + 1);
}

long evalConst(Node *expr){
    long val;
    switch (expr->kind){
        case ND_NUM:
            return expr->val;
        case ND_ADD:
            if(isAddressType(expr->lhs->type) || isAddressType(expr->rhs->type)){
                if(isAddressType(expr->lhs->type)){
                    return evalConst(expr->lhs) + evalConst(expr->rhs) * expr->lhs->type->basetype->size;
                }
                else{
                    return evalConst(expr->rhs) + evalConst(expr->lhs) * expr->rhs->type->basetype->size;
                }
            }
            return evalConst(expr->lhs) + evalConst(expr->rhs);
        case ND_SUB:
            if(isAddressType(expr->lhs->type)){
                if(isAddressType(expr->rhs->type)){
                    return (evalConst(expr->lhs) - evalConst(expr->rhs)) / expr->lhs->type->basetype->size;
                }
                else{
                    return evalConst(expr->lhs) - evalConst(expr->rhs) * expr->lhs->type->basetype->size;
                }
            }
            return evalConst(expr->lhs) - evalConst(expr->rhs);
        case ND_BITNOT:
            return ~evalConst(expr->lhs);
        case ND_LOGNOT:
            return !evalConst(expr->lhs);
        case ND_CAST:
            assert(expr->type->kind != TY_STRUCT && expr->type->kind != TY_UNION
                   && expr->type->kind != TY_FUNCTION && expr->type->kind != TY_ARRAY);
            val = evalConst(expr->lhs);

            if(expr->type->size <= expr->lhs->type->size){
                return val;
            }
            else{
                switch (expr->lhs->type->size){
                    case 1:
                        return (long)(char)val;
                    case 2:
                        return (long)(short)val;
                    case 4:
                        return (long)(int)val;
                    default:
                        assert(false);
                }
            }
        case ND_MUL:
            return evalConst(expr->lhs) * evalConst(expr->rhs);
        case ND_DIV:
            // fixme: zero division
            return evalConst(expr->lhs) / evalConst(expr->rhs);
        case ND_MOD:
            // fixme: zero division
            return evalConst(expr->lhs) % evalConst(expr->rhs);
        case ND_SHL:
            return evalConst(expr->lhs) << evalConst(expr->rhs);
        case ND_SHR:
            return evalConst(expr->lhs) >> evalConst(expr->rhs);
        case ND_GT:
            return evalConst(expr->lhs) > evalConst(expr->rhs);
        case ND_GE:
            return evalConst(expr->lhs) >= evalConst(expr->rhs);
        case ND_LT:
            return evalConst(expr->lhs) < evalConst(expr->rhs);
        case ND_LE:
            return evalConst(expr->lhs) <= evalConst(expr->rhs);
        case ND_EQ:
            return evalConst(expr->lhs) == evalConst(expr->rhs);
        case ND_NEQ:
            return evalConst(expr->lhs) != evalConst(expr->rhs);
        case ND_BITAND:
            return evalConst(expr->lhs) & evalConst(expr->rhs);
        case ND_BITXOR:
            return evalConst(expr->lhs) ^ evalConst(expr->rhs);
        case ND_BITOR:
            return evalConst(expr->lhs) | evalConst(expr->rhs);
        case ND_LOGAND:
            return evalConst(expr->lhs) && evalConst(expr->rhs);
        case ND_LOGOR:
            return evalConst(expr->lhs) || evalConst(expr->rhs);
        case ND_COND:
            return evalConst(expr->cond) ? evalConst(expr->then) : evalConst(expr->els);
        case ND_COMMA:
            evalConst(expr->lhs);
            return evalConst(expr->rhs);
        default:
            assert(false);
    }
}

long eval(Node *expr, char **label){
    char *label1 = NULL, *label2 = NULL;
    long val;
    long lval, rval;
    switch (expr->kind){
        case ND_VAR:
            assert(expr->var->type->kind == TY_ARRAY || expr->var->type->kind == TY_FUNCTION);
            *label = getLabel(expr->var);
            return 0;
        case ND_NUM:
            return expr->val;
        case ND_ADD:
            lval = eval(expr->lhs, &label1);
            rval = eval(expr->rhs, &label2);
            assert(!(label1 && label2));
            *label = label1 ? label1 : label2;
            if(isAddressType(expr->lhs->type)){
                if(isAddressType(expr->rhs->type)){
                    assert(false);
                }
                else{
                    assert(!label2);
                    assert(expr->lhs->type->basetype->isComplete);
                    val = lval + rval * expr->lhs->type->basetype->size;
                }
            }
            else{
                if(isAddressType(expr->rhs->type)){
                    assert(!label1);
                    assert(expr->rhs->type->kind == TY_POINTER);
                    assert(expr->rhs->type->basetype->isComplete);
                    val = rval + lval * expr->rhs->type->basetype->size;
                }
                else{
                    val = lval + rval;
                }
            }
            return val;
        case ND_SUB:
            lval = eval(expr->lhs, &label1);
            rval = eval(expr->rhs, &label2);
            assert(!label2);
            *label = label1;
            if(isAddressType(expr->lhs->type)){
                if(isAddressType(expr->rhs->type)){
                    if(label1){
                        assert(false);
                    }
                    val = (lval - rval) / expr->lhs->type->basetype->size;
                }
                else{
                    assert(expr->lhs->type->basetype->isComplete);
                    val = lval - rval * expr->lhs->type->basetype->size;
                }
            }
            else{
                if(isAddressType(expr->rhs->type)){
                    assert(false);
                }
                else{
                    val = lval - rval;
                }
            }
            return val;
        case ND_BITNOT:
            val = ~eval(expr->lhs, &label1);
            assert(!label1);
            return val;
        case ND_ADDR:
            assert(expr->lhs->kind == ND_VAR);  // should be promised while generating AST
            *label = getLabel(expr->lhs->var);
            return 0;
        case ND_LOGNOT:
            val = !eval(expr->lhs, &label1);
            if(label1){
                // warning      !&a;
                return 0;
            }
            return !val;
        case ND_CAST:
            assert(expr->type->kind != TY_STRUCT && expr->type->kind != TY_UNION
                   && expr->type->kind != TY_FUNCTION && expr->type->kind != TY_ARRAY);
            val = eval(expr->lhs, &label1);
            *label = label1;
            if(label1){
                assert(expr->type->size ==8);
                return val;
            }

            if(expr->type->size < expr->lhs->type->size){
                return val;
            }
            else{
                switch (expr->lhs->type->size){
                    case 1:
                        return (long)(char)val;
                    case 2:
                        return (long)(short)val;
                    case 4:
                        return (long)(int)val;
                    case 8:
                        return val;
                    default:
                        assert(false);
                }
            }
        case ND_MUL:
            val = eval(expr->lhs, &label1) * eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_DIV:
            val = eval(expr->lhs, &label1) / eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_SHL:
            val = eval(expr->lhs, &label1) << eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_SHR:
            val = eval(expr->lhs, &label1) >> eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_GT:
            val = eval(expr->lhs, &label1) > eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_GE:
            val = eval(expr->lhs, &label1) >= eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_LT:
            val = eval(expr->lhs, &label1) < eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_LE:
            val = eval(expr->lhs, &label1) <= eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_EQ:
            val = eval(expr->lhs, &label1) == eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_NEQ:
            val = eval(expr->lhs, &label1) != eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_BITAND:
            val = eval(expr->lhs, &label1) & eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_BITXOR:
            val = eval(expr->lhs, &label1) ^ eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_BITOR:
            val = eval(expr->lhs, &label1) | eval(expr->rhs, &label2);
            assert(!label1 && !label2);
            *label = NULL;
            return val;
        case ND_LOGAND:
            val = eval(expr->lhs, &label1);
            if(!label1 && !val){
                return false;
            }

            val = eval(expr->rhs, &label1);
            if(!label1 && !val){
                return false;
            }
            return true;
        case ND_LOGOR:
            val = eval(expr->lhs, &label1);
            if(label1 || val){
                return true;
            }

            val = eval(expr->rhs, &label1);
            if(label1 || val){
                return true;
            }
            return false;
        case ND_COND:
            val = eval(expr->cond, &label1) ? eval(expr->then, &label2) : eval(expr->els, &label2);
            *label = label2;
            return val;
        case ND_COMMA:
            eval(expr->lhs, &label1);
            val = eval(expr->rhs, &label2);
            *label = label2;
            return val;
        default:
            assert(false);
    }
}

int getOpPriority(TokenKind kind){
    switch (kind){
        case TK_DIV:
        case TK_MUL_DEREF:
        case TK_MOD:
            return 140;
        case TK_ADD:
        case TK_SUB:
            return 130;
        case TK_SHL:
        case TK_SHR:
            return 120;
        case TK_GE:
        case TK_GT:
        case TK_LE:
        case TK_LT:
            return 110;
        case TK_EQ:
        case TK_NEQ:
            return 100;
        case TK_BIT_AND:
            return 80;
        case TK_BIT_XOR:
            return 70;
        case TK_BIT_OR:
            return 60;
        case TK_LOG_AND:
            return 50;
        case TK_LOG_OR:
            return 40;
        case TK_QUESTION:
            return 30;
        case TK_ASSIGN:
        case TK_ADD_ASSIGN:
        case TK_SUB_ASSIGN:
        case TK_MUL_ASSIGN:
        case TK_DIV_ASSIGN:
        case TK_SHL_ASSIGN:
        case TK_SHR_ASSIGN:
        case TK_BITAND_ASSIGN:
        case TK_BITXOR_ASSIGN:
        case TK_BITOR_ASSIGN:
            return 20;
        case TK_COMMA:
            return 10;
        default:
            return -1;
    }
}

// pre uanry op only
NodeKind unaryTokenToNode(TokenKind kind){
    switch (kind){
        case TK_ADD:
            return ND_POS;
        case TK_SUB:
            return ND_NEG;
        case TK_INC:
            return ND_PRE_INC;
        case TK_DEC:
            return ND_PRE_DEC;
        case TK_BIT_NOT:
            return ND_BITNOT;
        case TK_MUL_DEREF:
            return ND_DEREF;
        case TK_BIT_AND:
            return ND_ADDR;
        case TK_LOG_NOT:
            return ND_LOGNOT;
        default:
            assert(false);
    }
}

// binary only
NodeKind binTokenToNode(TokenKind kind){
    switch (kind){
        case TK_MUL_DEREF:
            return ND_MUL;
        case TK_DIV:
            return ND_DIV;
        case TK_MOD:
            return ND_MOD;
        case TK_SUB:
            return ND_SUB;
        case TK_ADD:
            return ND_ADD;
        case TK_SHR:
            return ND_SHR;
        case TK_SHL:
            return ND_SHL;
        case TK_GE:
            return ND_GE;
        case TK_GT:
            return ND_GT;
        case TK_LE:
            return ND_LE;
        case TK_LT:
            return ND_LT;
        case TK_NEQ:
            return ND_NEQ;
        case TK_EQ:
            return ND_EQ;
        case TK_BIT_AND:
            return ND_BITAND;
        case TK_BIT_XOR:
            return ND_BITXOR;
        case TK_BIT_OR:
            return ND_BITOR;
        case TK_LOG_AND:
            return ND_LOGAND;
        case TK_LOG_OR:
            return ND_LOGOR;
        case TK_QUESTION:
            return ND_COND;
        case TK_ASSIGN:
            return ND_ASSIGN;
        case TK_ADD_ASSIGN:
            return ND_ADD_ASSIGN;
        case TK_SUB_ASSIGN:
            return ND_SUB_ASSIGN;
        case TK_MUL_ASSIGN:
            return ND_MUL_ASSIGN;
        case TK_DIV_ASSIGN:
            return ND_DIV_ASSIGN;
        case TK_MOD_ASSIGN:
            return ND_MOD_ASSIGN;
        case TK_SHL_ASSIGN:
            return ND_SHL_ASSIGN;
        case TK_SHR_ASSIGN:
            return ND_SHR_ASSIGN;
        case TK_BITAND_ASSIGN:
            return ND_BITAND_ASSIGN;
        case TK_BITXOR_ASSIGN:
            return ND_BITXOR_ASSIGN;
        case TK_BITOR_ASSIGN:
            return ND_BITOR_ASSIGN;
        case TK_COMMA:
            return ND_COMMA;
        default:
            assert(false);
    }
}


TypeKind convTokenToType(TokenKind tok){
    switch (tok){
        case TK_STRUCT:
            return TY_STRUCT;
        case TK_ENUM:
            return TY_ENUM;
        case TK_UNION:
            return TY_UNION;
        default:
            assert(false);
    }
}

Type *convToPointer(Type *type){
    assert(isAddressType(type));
    if(type->kind == TY_FUNCTION){
        type = pointTo(type);
    }
    else if(type->kind == TY_ARRAY){
        type = pointTo(type->basetype);
    }
    return type;
}

VarList *addHead(VarList *list, Var *var){
    VarList *head = calloc(1, sizeof(VarList));
    head->var = var;
    head->next = list;
    return head;
}

VarList *addTail(VarList *list, Var *var){
    VarList *tail = calloc(1, sizeof(VarList));
    tail->var = var;
    list->next = tail;
    return tail;
}

int isTypeToken(Token *tok){
    assert(tok->kind != TK_EOF);
    switch (tok->kind){
        case TK_VOID:
        case TK_BOOL:
        case TK_CHAR:
        case TK_SHORT:
        case TK_INT:
        case TK_LONG:
        case TK_SIGNED:
        case TK_STRUCT:
        case TK_UNION:
        case TK_ENUM:
            return 1;
        case TK_IDENTIFIER:{
            Var *var = findVar(varList, tok->pos, tok->len);
            if(var && var->storage == TYPEDEF){
                return 1;
            }
            else{
                return 0;
            }
        }
        default:
            return 0;
    }
}

int isDeclToken(Token *tok){
    switch (tok->kind){
        case TK_EXTERN:
        case TK_STATIC:
        case TK_TYPEDEF:
            return 1;
        default:
            return isTypeToken(tok);
    }
}

// binary and ternary
Node *parseExpr(int priority){
    Node *lhs, *rhs;
    Type *type;
    lhs = unary();
    if(isArrayOrFunc(lhs->type)){
        type = convToPointer(lhs->type);
        lhs = newCastNode(lhs, type, NULL);
    }

    while(1){
        if(priority >= getOpPriority(token->kind)){
            return lhs;
        }
        Token *tok = token;
        switch (token->kind){
            case TK_DIV:
            case TK_MUL_DEREF:
            case TK_MOD:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                type = decideType(lhs->type, rhs->type);
                lhs = newCastNode(lhs, type, NULL);
                rhs = newCastNode(rhs, type, NULL);
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, type, tok);
                break;
            case TK_ADD:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                if(isAddressType(lhs->type)){
                    if(isAddressType(rhs->type)){
                        assert(false);
                    }
                    else{
                        rhs = newCastNode(rhs, long_type, NULL);
                        type = lhs->type;
                    }
                }
                else{
                    if(isAddressType(rhs->type)){
                        lhs = newCastNode(lhs, long_type, NULL);
                        type = rhs->type;
                    }
                    else{
                        type = decideType(lhs->type, rhs->type);
                        lhs = newCastNode(lhs, type, NULL);
                        rhs = newCastNode(rhs, type, NULL);  // attention: only cast rhs
                    }
                }
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, type, tok);
                break;
            case TK_SUB:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                if(isAddressType(lhs->type)){
                    if(isAddressType(rhs->type)){
                        assert(equalType(lhs->type, rhs->type));
                    }
                    else{
                        rhs = newCastNode(rhs, long_type, NULL);
                    }
                    type = lhs->type;
                }
                else{
                    if(isAddressType(rhs->type)){
                        assert(false);
                    }
                    else{
                        type = decideType(lhs->type, rhs->type);
                        lhs = newCastNode(lhs, type, NULL);
                        rhs = newCastNode(rhs, type, NULL);
                    }
                }
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, type, tok);
                break;
            case TK_SHL:
            case TK_SHR:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                type = decideType(lhs->type, int_type);     // attention
                lhs = newCastNode(lhs, type, NULL);
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, type, tok);
                break;
            case TK_GE:
            case TK_GT:
            case TK_LE:
            case TK_LT:
            case TK_EQ:
            case TK_NEQ:
                // fixme: comparision between signed and unsigned
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                if(isAddressType(lhs->type) && isAddressType(rhs->type)) {
                    // nothing here
                }
                else if(isAddressType(lhs->type) || isAddressType(rhs->type)){
                    if(isAddressType(lhs->type)){
                        rhs = newCastNode(rhs, long_type, NULL);
                    }
                    else{
                        lhs = newCastNode(rhs, long_type, NULL);
                    }
                }
                else{
                    type = decideType(lhs->type, int_type);
                    lhs = newCastNode(lhs, type, NULL);
                    rhs = newCastNode(rhs, type, NULL);
                }
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, int_type, tok);
                break;
            case TK_BIT_AND:
            case TK_BIT_XOR:
            case TK_BIT_OR:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                type = decideType(lhs->type, rhs->type);
                lhs = newCastNode(lhs, type, NULL);
                rhs = newCastNode(rhs, type, NULL);
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, type, tok);
                break;
            case TK_LOG_AND:
            case TK_LOG_OR:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, int_type, tok);
                break;
            case TK_QUESTION:
                // fixme: donno how to fix ...
                token = token->next;
                Node *then = parseExpr(getOpPriority(tok->kind) - 1);
                expectToken(TK_COLON);
                Node *els = parseExpr(getOpPriority(tok->kind) - 1);
                assert(typeSupportOp(then->type, binTokenToNode(tok->kind), false));
                assert(typeSupportOp(els->type, binTokenToNode(tok->kind), false));
                assert(equalType(then->type, els->type));
                lhs = newCondNode(lhs, then, els, then->type, tok);
                break;
            case TK_ASSIGN:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind) - 1);
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, newCastNode(rhs, lhs->type, NULL), lhs->type, tok);
                break;
            case TK_ADD_ASSIGN:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind) - 1);
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                if(isAddressType(lhs->type)){
                    if(isAddressType(rhs->type)){
                        assert(false);
                    }
                    else{
                        rhs = newCastNode(rhs, long_type, NULL);
                    }
                }
                else{
                    if(isAddressType(rhs->type)){
                        // nothing here
                    }
                    else{
                        type = decideType(lhs->type, rhs->type);
                        rhs = newCastNode(rhs, type, NULL);  // attention: only cast rhs
                    }
                }
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, lhs->type, tok);
                break;
            case TK_SUB_ASSIGN:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind) - 1);
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                if(isAddressType(lhs->type)){
                    if(isAddressType(rhs->type)){
                        assert(equalType(lhs->type, rhs->type));
                    }
                    else{
                        rhs = newCastNode(rhs, long_type, NULL);
                    }
                }
                else{
                    if(isAddressType(rhs->type)){
                        assert(false);
                    }
                    else{
                        type = decideType(lhs->type, rhs->type);
                        rhs = newCastNode(rhs, type, NULL);  // attention: only cast rhs
                    }
                }
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, lhs->type, tok);
                break;
            case TK_MUL_ASSIGN:
            case TK_DIV_ASSIGN:
            case TK_MOD_ASSIGN:
            case TK_SHL_ASSIGN:
            case TK_SHR_ASSIGN:
            case TK_BITAND_ASSIGN:
            case TK_BITXOR_ASSIGN:
            case TK_BITOR_ASSIGN:
                // attention: collaborate with codegen
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind) - 1);
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                type = decideType(lhs->type, rhs->type);
                rhs = newCastNode(rhs, type, NULL);  // attention: only cast rhs
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, lhs->type, tok);
                break;
            case TK_COMMA:
                token = token->next;
                rhs = parseExpr(getOpPriority(tok->kind));
                assert(typeSupportOp(lhs->type, binTokenToNode(tok->kind), true));
                assert(typeSupportOp(rhs->type, binTokenToNode(tok->kind), false));
                lhs = newBinaryNode(binTokenToNode(tok->kind), lhs, rhs, rhs->type, tok);
                break;
            default:
                assert(false);
        }
    }
}

Node *handleArgus(Type *type){
    assert(typeSupportOp(type, ND_CALL, false));
    Node *cur = NULL;
    VarList *params;
    if(type->kind == TY_FUNCTION){
        params = type->params;
    }
    else {
        params = type->basetype->params;
    }

    while(params){
        Node *tmp = parseExpr(getOpPriority(TK_COMMA));
        assert(typeSupportOp(tmp->type, ND_CAST, false));
        tmp = newCastNode(tmp, params->var->type, NULL);

        tmp->next = cur;
        cur = tmp;

        if(params->next){
            expectToken(TK_COMMA);
            params = params->next;
        }
        else{
            if(consumeToken(TK_R_PAREN)){
                break;
            }
            else{
                assert(type->hasVarArgs);
                while(!consumeToken(TK_R_PAREN)){
                    expectToken(TK_COMMA);
                    tmp = parseExpr(getOpPriority(TK_COMMA));
                    assert(typeSupportOp(tmp->type, ND_CAST, false));
                    tmp->next = cur;
                    cur = tmp;
                }
                break;
            }
        }
    }
    return cur;
}

// cast sizeof fun() A.a A->a a[10] (expression)
Node *primary(){
    Node *lhs;
    Token *tok = token;

    // compound-literal
    if(tok = consumeToken(TK_L_PAREN)){
        if(isDeclToken(token)){
            // cast
            Type *type = getBaseType();
            type = handleDeclarator(type, NULL);
            expectToken(TK_R_PAREN);
            // fixme: type check
            if(peekToken(TK_L_BRACE)){
                Var *var = newVar(type, "", 0);
                var->storage = scopeDepth == 0 ? STATIC : UNSPECIFIED;
                var->label = newLabel();
                var->init = handleInit(var->type, true);
                globals = addTail(globals, var);
                return newVarNode(var, tok);
            }
            else{
                lhs = unary();
                return newUnaryNode(ND_CAST, lhs, type, tok);
            }
        }
        else{
            lhs = parseExpr(0);
            expectToken(TK_R_PAREN);
            return lhs;
        }
    }
    else if(tok = consumeToken(TK_IDENTIFIER)){
        Var *var = findVar(varList, tok->pos, tok->len);
        assert(var);
        lhs = newVarNode(var, tok);
        assert(var && var->storage != TYPEDEF);
        // fixme: add node type
        if(tok = consumeToken(TK_L_BRACKET)){
            assert(false);

            Node *tmp = parseExpr(0);
            Type *type = NULL;
            expectToken(TK_R_BRACKET);
            int count = 0;

            if(var->type->kind == TY_POINTER || var->type->kind == TY_ARRAY){
                ++count;
                type = var->type;
            }
            if(tmp->type->kind == TY_POINTER || tmp->type->kind == TY_ARRAY){
                ++count;
                type = tmp->type;
            }
            assert(count == 1);
            assert(type);
            // warning 2[a]

            lhs = newBinaryNode(ND_ADD, lhs, tmp, type, tok);
            return newUnaryNode(ND_DEREF, lhs, type->basetype, tok);
        }
        else if(tok = consumeToken(TK_L_PAREN)){
            assert(typeSupportOp(var->type, ND_CALL, false));
            Type *type;
            if(var->type->kind == TY_FUNCTION){
                type = var->type;
            }
            else if(var->type->kind == TY_POINTER){
                type = var->type->basetype;
            }
            else{
                assert(false);
            }
            Node *argus = handleArgus(type);
            lhs = newUnaryNode(ND_CALL, lhs, var->type->retType, tok);
            lhs->args = argus;
            return lhs;
        }
        else if(consumeToken(TK_DOT)){
            assert(typeSupportOp(var->type, ND_MEMBER_DOT, true));
            Var *member = findVar(var->type->members, token->pos, token->len);
            return newMemDotNode(member, lhs, token);
        }
        else if(consumeToken(TK_MEMBER)){
            assert(typeSupportOp(var->type, ND_MEMBER_PTR, true));
            Var *member = findVar(var->type->members, token->pos, token->len);
            return newMemPtrNode(member, lhs, token);
        }
        else{
            if(var->type->kind == TY_ENUM){
                return newNumNode(var->enumVal, int_type, tok);
            }
            return lhs;
        }
    }
    else if(tok = consumeToken(TK_SIZEOF)){
        if(consumeToken(TK_L_PAREN)){
            if(isDeclToken(token)){
                Type *type = getBaseType();
                type = handleDeclarator(type, NULL);
                expectToken(TK_R_PAREN);
                return newNumNode(type->size, long_type, tok);
            }
            else{
                lhs = parseExpr(0);
                expectToken(TK_R_PAREN);
                return newNumNode(lhs->type->size, long_type, tok);
            }
        }
        else{
            lhs = unary();
            return newNumNode(lhs->type->size, long_type, tok);
        }
    }
    else if(tok = consumeToken(TK_ALIGNOF)){
        if(consumeToken(TK_L_PAREN)){
            if(isDeclToken(token)){
                Type *type = getBaseType();
                type = handleDeclarator(type, NULL);
                expectToken(TK_R_PAREN);
                return newNumNode(type->align, long_type, tok);
            }
            else{
                lhs = parseExpr(0);
                expectToken(TK_R_PAREN);
                return newNumNode(lhs->type->align, long_type, tok);
            }
        }
        else{
            lhs = unary();
            return newNumNode(lhs->type->align, long_type, tok);
        }
    }
    else if(tok = consumeToken(TK_STR)){
        Var *var = newVar(arrayOf(char_type, false, 0), "", 0);
        var->storage = STATIC;
        var->label = newLabel();
        token = tok;    // attention
        var->init = handleInit(var->type, true);
        globals = addTail(globals, var);
        return newVarNode(var, tok);
    }
    else if(tok = consumeToken(TK_NUM)){
        return newNumNode(tok->val, tok->type, tok);
    }
}

Node *unary(){
    Node *lhs;
    Token *tok = token;
    Type *type;
    NodeKind nodekind;
    // fixme: typeSupport
    switch (tok->kind) {
        case TK_SUB:
        case TK_ADD:
        case TK_BIT_NOT:
            token = token->next;
            lhs = unary();
            nodekind = unaryTokenToNode(tok->kind);
            assert(typeSupportOp(lhs->type, nodekind, false));
            lhs = newUnaryNode(nodekind, lhs, lhs->type, tok);
            type = decideType(lhs->type, int_type);
            return newCastNode(lhs, type, NULL);
        case TK_INC:
        case TK_DEC:
            token = token->next;
            lhs = unary();
            nodekind = unaryTokenToNode(tok->kind);
            assert(typeSupportOp(lhs->type, nodekind, false));
            assert(nodeSupportOp(nodekind, lhs->kind));
            if(lhs->type->kind == TY_POINTER){
                type = lhs->type;
                return newUnaryNode(nodekind, lhs, type, tok);
            }
            else{
                type = decideType(lhs->type, int_type);
                lhs = newUnaryNode(nodekind, lhs, lhs->type, tok);
                return newCastNode(lhs, type, NULL);
            }
        case TK_LOG_NOT:
            token = token->next;
            lhs = unary();
            nodekind = unaryTokenToNode(tok->kind);
            assert(typeSupportOp(lhs->type, nodekind, false));
            return newUnaryNode(ND_LOGNOT, lhs, int_type, tok);
        case TK_MUL_DEREF:
            token = token->next;
            lhs = unary();
            nodekind = unaryTokenToNode(tok->kind);
            assert(typeSupportOp(lhs->type, nodekind, false));
            type = lhs->type->basetype;
            return newUnaryNode(nodekind, lhs, type, tok);
        case TK_BIT_AND:
            token = token->next;
            lhs = unary();
            assert(lhs->kind == ND_VAR);
            type = pointTo(lhs->type);
            return newUnaryNode(ND_ADDR, lhs, type, tok);
        default:
            return primary();
    }
}

Type *findTag(char *pos, int len){
    Type *tag = tagList;

    while(tag){
        if(tag->len == len && !strncmp(tag->pos, pos, len)){
            return tag;
        }
        tag = tag->next;
    }

    return NULL;
}

Type *findScopeTag(char *pos, int len){
    Type *tag = tagList;

    while(tag){
        if(tag->scopeDepth != scopeDepth){
            break;
        }
        if(tag->len == len && !strncmp(tag->pos, pos, len)){
            return tag;
        }
        tag = tag->next;
    }

    return NULL;
}

void handleStructDef(Type *type){
    type->isComplete = true;
    VarList mem = {};
    VarList *cur = &mem;
    int offset = 0;
    int align = 0;
    while(1){
        if(consumeToken(TK_R_BRACE)){
            break;
        }
        Type *ty = getBaseType();
        // will not default to int_type
        assert(ty);
        if(consumeToken(TK_SEMICOLON)){
            continue;
        }

        while(1){
            Var *var = calloc(1, sizeof(Var));
            handleDeclarator(ty, var);
            offset = alignTo(offset, var->type->align);
            var->offset = offset;
            offset += ty->size;
            align = ty->align > align? ty->align : align;
            cur = addTail(cur, var);

            if (consumeToken(TK_COMMA)) {
                continue;
            }
            else if (consumeToken(TK_SEMICOLON)) {
                break;
            }
            else {
                assert(false);
            }
        }
    }

    type->members = mem.next;
    type->size = alignTo(offset, align);
    type->align = align;
}

void handleUnionDef(Type *type){
    type->isComplete = true;
    VarList mem = {};
    VarList *cur = &mem;
    int align = 0;
    int size = 0;
    while(1){
        if(consumeToken(TK_R_BRACE)){
            break;
        }
        Type *ty = getBaseType();
        // will not default to int_type
        assert(ty);
        if(consumeToken(TK_SEMICOLON)){
            continue;
        }

        while(1){
            Var *var = calloc(1, sizeof(Var));
            handleDeclarator(ty, var);
            var->offset = 0;
            align = ty->align > align? ty->align : align;
            size = ty->size > size ? ty->size : size;
            cur = addTail(cur, var);

            if (consumeToken(TK_COMMA)) {
                continue;
            } else if (consumeToken(TK_SEMICOLON)) {
                break;
            } else {
                assert(false);
            }
        }
    }

    type->members = mem.next;
    type->size = alignTo(size, align);
    type->align = align;
}

void handleEnumDef(Type *ty){
    // contains at least one enumerator
    ty->isComplete = true;
    Token *tok;
    int accumulator = 0;
    if(consumeEnd()){
        assert(false);
    }

    while(1){
        tok = expectToken(TK_IDENTIFIER);
        Var *var = newVar(ty, tok->pos, tok->len);
        // fixme: redefinition of var
        if(consumeToken(TK_ASSIGN)){
            accumulator = evalConst(parseExpr(getOpPriority(TK_ASSIGN) - 1));
        }
        var->enumVal = accumulator++;
        varList = addHead(varList, var);
        if(consumeEnd()){
            break;
        }
        expectToken(TK_COMMA);
    }
}

Type *getBaseType(){
    Type *basetype = NULL;
    if(!isTypeToken(token)){
        return basetype;
    }

    enum {
        VOID = 1 << 0,
        BOOL = 1 << 2,
        CHAR = 1 << 4,
        SHORT = 1 << 6,
        INT = 1 << 8,
        LONG = 1 << 10,
        SIGNED = 1 << 12,
        OTHER = 1 << 14
    };

    int count = 0;
    do{
        switch (token->kind) {
            case TK_VOID:
                token = token->next;
                count += VOID;
                break;
            case TK_BOOL:
                token = token->next;
                count += BOOL;
                break;
            case TK_CHAR:
                token = token->next;
                count += CHAR;
                break;
            case TK_SHORT:
                token = token->next;
                count += SHORT;
                break;
            case TK_INT:
                token = token->next;
                count += INT;
                break;
            case TK_LONG:
                token = token->next;
                count += LONG;
                break;
            case TK_SIGNED:
                token = token->next;
                count |= SIGNED;
                break;
            default:
                count += OTHER;
                break;
        }

        if(count == OTHER) {
            break;
        }

        switch (count){
            case VOID:
                basetype = void_type;
                break;
            case BOOL:
                basetype = bool_type;
                break;
            case CHAR:
            case SIGNED + CHAR:
                basetype = char_type;
                break;
            case SHORT:
            case SHORT + INT:
            case SIGNED + SHORT:
            case SIGNED + SHORT + INT:
                basetype = short_type;
                break;
            case INT:
            case SIGNED:
            case SIGNED + INT:
                basetype = int_type;
                break;
            case LONG:
            case LONG + INT:
            case SIGNED + LONG:
            case SIGNED + LONG + INT:
            case LONG + LONG:
            case LONG + LONG + INT:
            case SIGNED + LONG + LONG:
            case SIGNED + LONG + LONG + INT:
                basetype = long_type;
                break;
            default:
                assert(false);
        }
    } while (isTypeToken(token));

    if(count == OTHER){
        Token *prev, *tok;
        switch (token->kind){
            case TK_ENUM:
            case TK_STRUCT:
            case TK_UNION:
                prev = token;
                token = token->next;
                // struct A {}
                if(tok = consumeToken(TK_IDENTIFIER)){
                    if(consumeToken(TK_L_BRACE)){
                        basetype = findScopeTag(tok->pos, tok->len);
                        if(basetype){
                            assert(!basetype->isComplete);
                            assert(basetype->kind == convTokenToType(prev->kind));
                        }
                        else{
                            basetype = newTag(convTokenToType(prev->kind), tok->pos, tok->len);
                            basetype->next = tagList;
                            tagList = basetype;
                        }

                        if(prev->kind == TK_STRUCT){
                            handleStructDef(basetype);
                        }
                        else if(prev->kind == TK_UNION){
                            handleUnionDef(basetype);
                        }
                        else{   // TK_ENUM
                            handleEnumDef(basetype);
                        }
                    }
                    else{
                        basetype = findTag(tok->pos, tok->len);
                        if(basetype){
                            if(basetype->kind != convTokenToType(prev->kind)){
                                assert(false);
                            }
                        }
                        else{
                            basetype = newTag(convTokenToType(prev->kind), tok->pos, tok->len);
                            basetype->next = tagList;
                            tagList = basetype;
                        }
                    }
                }
                    // struct {}
                else if(tok = consumeToken(TK_L_BRACE)){
                    basetype = newTag(convTokenToType(prev->kind), tok->pos, tok->len);
                    if(prev->kind == TK_STRUCT){
                        handleStructDef(basetype);
                    }
                    else if(prev->kind == TK_UNION){
                        handleUnionDef(basetype);
                    }
                    else{   // TK_ENUM
                        handleEnumDef(basetype);
                    }
                }
                else{
                    assert(false);
                }
                break;
            default:
                ;
                Var *var = findVar(varList, token->pos, token->len);
                assert(var && var->storage == TYPEDEF && var->type);
                basetype = var->type;
                token = token->next;
                break;
        }
    }
    assert(basetype);
    return basetype;
}

Type *handleSuffix(Type *basetype, int count, bool first){
    while(count > 0){
        expectToken(TK_R_PAREN);
        --count;
    }

    Type *type = calloc(1, sizeof(Type));
    if(consumeToken(TK_R_BRACKET)){
        assert(first);
    }
    else{
        type->arrayLen = evalConst(parseExpr(0));
        assert(type->arrayLen >= 0);
        expectToken(TK_R_BRACKET);
        type->isComplete = true;
    }

    while (count > 0){
        if(consumeToken(TK_R_PAREN)){
            --count;
        }
        else{
            break;
        }
    }

    if(consumeToken(TK_L_BRACKET)){
        basetype = handleSuffix(basetype, count, false);
    }
    else{
        assert(count == 0);
    }

    type->basetype = basetype;
    type->kind = TY_ARRAY;
    type->align = basetype->align;
    if(type->isComplete){
        type->size = basetype->size * type->arrayLen;
    }
    return type;
}

Type *handleParams(Type *basetype){
    VarList head;
    VarList *list = &head;
    int first = 1;
    bool hasVarArgs = false;
    int argc = 0;
    if(!consumeToken(TK_R_PAREN)){
        while(1){
            Type *type = getBaseType();
            assert(type);
            if(type->kind == TY_VOID){
                if(consumeToken(TK_R_PAREN)){
                    if(first){
                        return NULL;
                    }
                    assert(false);
                }
                first = 0;
            }
            Var *var = calloc(1, sizeof(Var));
            handleDeclarator(type, var);
            var->scopeDepth = 1;
            assert(!findVar(head.next, var->pos, var->len));
            assert(var->type->kind != TY_STRUCT && var->type->kind != TY_UNION && var->type->kind != TY_VOID);
            if(var->type->kind == TY_FUNCTION || var->type->kind == TY_ARRAY){
                var->type = convToPointer(var->type);
            }
            list = addTail(list, var);
            ++argc;

            if(consumeToken(TK_COMMA)){
                if(consumeToken(TK_ELLIPSIS)){
                    hasVarArgs = true;
                    expectToken(TK_R_PAREN);
                    break;
                }
                else{
                    continue;
                }
            }
            else if(consumeToken(TK_R_PAREN)){
                break;
            }
            else{
                assert(false);
            }
        }
    }

    Type *type = calloc(1, sizeof(Type));;
    type->params = head.next;
    type->kind = TY_FUNCTION;
    type->size = 1;
    type->align = 1;
    type->retType = basetype;
    type->hasVarArgs = hasVarArgs;
    type->argc = argc;
    int offset = argc >= 6 ? 48 : argc * 8;
    offset = hasVarArgs ? 48 : offset;
    list = head.next;
    while(offset > 0){
        if(!list){
            assert(hasVarArgs);
            break;
        }
        list->var->offset = offset;
        list = list->next;
        offset -= 8;
    }
    return type;
}

// when output == NULL, handle abstract type, (int (*)())
Type *handleDeclarator(Type *basetype, Var *output){
    Type *type = basetype;
    while(consumeToken(TK_MUL_DEREF)) {
        type = pointTo(type);
    }

    int count = 0;
    while(consumeToken(TK_L_PAREN)){
        ++count;
    }

    if(peekToken(TK_MUL_DEREF)){
        assert(count > 0);
        Type *placeholder = calloc(1, sizeof(Type));
        Type *innertype = handleDeclarator(placeholder, output);
        expectToken(TK_R_PAREN);
        --count;

        while(count > 0){
            expectToken(TK_R_PAREN);
            --count;
        }

        if(consumeToken(TK_L_PAREN)){
            type = handleParams(type);
            while(count > 0){
                expectToken(TK_R_PAREN);
                --count;
            }
        }
        else if(consumeToken(TK_L_BRACKET)){
            type = handleSuffix(type, count, true);
        }

        *placeholder = *type;
        return innertype;
    }
    else{
        if(output != NULL){
            Token *tok = expectToken(TK_IDENTIFIER);
            output->pos = tok->pos;
            output->len = tok->len;
            output->scopeDepth = scopeDepth;
            // arrayOf will take care of the incomplete condition

            while (count > 0 && consumeToken(TK_R_PAREN)){
                --count;
            }

            if(consumeToken(TK_L_PAREN)){
                // function
                output->type = handleParams(type);
                while (count > 0){
                    expectToken(TK_R_PAREN);
                    --count;
                }
            }
            else if(consumeToken(TK_L_BRACKET)){
                // array
                output->type = handleSuffix(type, count, true);
            }
            else{
                output->type = type;
                while (count > 0){
                    expectToken(TK_R_PAREN);
                    --count;
                }
            }

            return output->type;
        }
        else{
            if(count == 0){
                return type;
            }
            if(count == 1){
                type = handleParams(type);
                return type;
            }
            else{
                assert(false);
            }
        }
    }
}

//"break", "case", "char", "const", "continue", "default", "do",
//"else", "enum", "extern", "for", "goto", "if", "int",
//"long", "return", "short", "signed", "sizeof", "static", "struct",
//"switch", "typedef", "void", "while",
//"_Bool", "_Alignof"};

//goto语句:无条件转向;
//if语句:判断语句;
//while循环语句;
//do-while语句:先执行循环体,然后判断循环条件是否成立. 之后继续循环;
//for语句:循环,可替代while语句; 只是用法不同;
//break语句跳出本层的循环;(只跳出包含此语句的循环)
//continue语句:继续(一般放到循环语句里,不在执行它下面的语句,直接跳到判断语句例:
// for语句,就直接跳到第二个分号处,while语句,就直接跳到while()的括号里;
//        switch语句:多相选择;
//return语句:返回;

void handleStat(Var *func){
    while(1){
        while(consumeToken(TK_SEMICOLON));
        if(consumeToken(TK_R_BRACE)){
            break;
        }
        if(isDeclToken(token) && token->next->kind != TK_COLON){
            handleDeclaration(func);
        }
        else{
            handleExpression(func);
        }
    }
}

Node *extractNode(Var *func, Node *cur){
    Node *tmp = cur->next;
    cur->next = NULL;
    func->exprs = cur;
    return tmp;
}

void handleExpression(Var *func){
    Token *tok;
    Node *cur = func->exprs;
    Node *init = NULL, *cond = NULL, *incre = NULL, *then = NULL, *els = NULL;
    if(consumeToken(TK_L_BRACE)){
        Scope *sc = enterScope();
        handleStat(func);
        leaveScope(sc);
    }
    else if(tok = consumeToken(TK_IF)){
        expectToken(TK_L_PAREN);
        cond = parseExpr(0);
        expectToken(TK_R_PAREN);
        handleExpression(func);
        then = extractNode(func, cur);

        if(consumeToken(TK_ELSE)){
            handleExpression(func);
            els = extractNode(func, cur);
        }
        cur->next = newStatNode(ND_IF, tok, init, cond, then, incre, els);
        func->exprs = cur;
    }
    else if(tok = consumeToken(TK_DO)){
        couldContinue = 1;
        couldBreak = 1;
        handleExpression(func);
        couldContinue = 0;
        couldBreak = 0;
        then = extractNode(func, cur);
        expectToken(TK_WHILE);
        expectToken(TK_L_PAREN);
        cond = parseExpr(0);
        expectToken(TK_R_PAREN);
        expectToken(TK_SEMICOLON);
        cur->next = newStatNode(ND_DO, tok, init, cond, then, incre, els);
        func->exprs = cur;
    }
    else if(tok = consumeToken(TK_FOR)){
        Scope *sc = enterScope();
        expectToken(TK_L_PAREN);
        if(isDeclToken(token)){
            assert(isTypeToken(token));
            handleDeclaration(func);
            init = extractNode(func, cur);
        }
        else{
            if(!consumeToken(TK_SEMICOLON)){
                init = parseExpr(0);
                expectToken(TK_SEMICOLON);
            }
        }

        if(!consumeToken(TK_SEMICOLON)){
            cond = parseExpr(0);
            expectToken(TK_SEMICOLON);
        }

        if(!consumeToken(TK_R_PAREN)){
            incre = parseExpr(0);
            expectToken(TK_R_PAREN);
        }
        couldContinue = 1;
        couldBreak = 1;
        handleExpression(func);
        couldContinue = 0;
        couldBreak = 0;
        then = extractNode(func, cur);
        cur->next = newStatNode(ND_FOR, tok, init, cond, then, incre, els);
        func->exprs = cur;
        leaveScope(sc);
    }
    else if(tok = consumeToken(TK_WHILE)){
        expectToken(TK_L_PAREN);
        cond = parseExpr(0);
        expectToken(TK_R_PAREN);
        couldContinue = 1;
        couldBreak = 1;
        handleExpression(func);
        couldContinue = 0;
        couldBreak = 0;
        then = extractNode(func, cur);

        cur->next = newStatNode(ND_WHILE, tok, init, cond, then, incre, els);
        func->exprs = cur;
    }
    else if(tok = consumeToken(TK_SWITCH)) {
        expectToken(TK_L_PAREN);
        cond = parseExpr(0);
        assert(isIntegerType(cond->type));
        expectToken(TK_R_PAREN);
        couldBreak = 1;
        handleExpression(func);
        couldBreak = 0;
        then = extractNode(func, cur);
        cur->next = newStatNode(ND_SWITCH, tok, init, cond, then, incre, els);
        func->exprs = cur;
    }
    else if(tok = consumeToken(TK_GOTO)){
        Node *node = newNode(ND_GOTO, NULL, tok);
        node->pos = token->pos;
        node->len = token->len;
        expectToken(TK_IDENTIFIER);
        expectToken(TK_SEMICOLON);
        func->exprs->next = node;
        func->exprs = node;
    }
    else if(tok = consumeToken(TK_RETURN)){
        Node *node = newNode(ND_RETURN, NULL, tok);
        if(func->type->kind != TY_VOID){
            node->lhs = parseExpr(0);
        }
        expectToken(TK_SEMICOLON);
        func->exprs->next = node;
        func->exprs = node;
    }
    else if(tok = consumeToken(TK_BREAK)){
        assert(couldBreak);
        Node *node = newNode(ND_BREAK, NULL, tok);
        expectToken(TK_SEMICOLON);
        func->exprs->next = node;
        func->exprs = node;
    }
    else if(tok = consumeToken(TK_CONTINUE)){
        assert(couldContinue);
        Node *node = newNode(ND_CONTINUE, NULL, tok);
        expectToken(TK_SEMICOLON);
        func->exprs->next = node;
        func->exprs = node;
    }
    else {
        Node *defaultCase = NULL;
        while(1){
            tok = token;
            if(consumeToken(TK_IDENTIFIER) && consumeToken(TK_COLON)){
                Node *node = newNode(ND_LABEL, NULL, tok);
                func->exprs->next = node;
                func->exprs = node;
                continue;
            }

            token = tok;
            if(consumeToken(TK_CASE)){
                assert(couldBreak && !couldContinue);   // switch
                long val = evalConst(parseExpr(getOpPriority(TK_COMMA)));
                Node *node = newNode(ND_CASE, NULL, tok);
                node->val = val;
                func->exprs->next = node;
                func->exprs = node;
                expectToken(TK_COLON);
            }
            else if(consumeToken(TK_DEFAULT)){
                assert(!defaultCase);
                assert(couldBreak && !couldContinue);   // switch
                Node *node = newNode(ND_DEFAULT, NULL, tok);
                defaultCase = node;
                func->exprs->next = node;
                func->exprs = node;
                expectToken(TK_COLON);
            }
            else{
                break;
            }
        }

        assert(!isDeclToken(token));
        func->exprs->next = parseExpr(0);
        func->exprs = func->exprs->next;
        expectToken(TK_SEMICOLON);
    }
}

void handleFuncBody(Var *func){
    Scope *sc = enterScope();
    VarList vlHead = {};
    Node exprHead = {};
    func->locals = &vlHead;     // used to save the tail of list temporarily, set it to head in the end
    func->exprs = &exprHead;
    for(VarList *vl = func->type->params; vl; vl = vl->next){
        Var *var = findVar(varList, vl->var->pos, vl->var->len);
        if(var){
            assert(var->scopeDepth != vl->var->scopeDepth);
            // warning: shadows
        }
        varList = addHead(varList, vl->var);
    }
    handleStat(func);
    leaveScope(sc);
    func->locals = vlHead.next;
    func->exprs = exprHead.next;
}

void handleExtraInit(){
    while(1){
        if(consumeToken(TK_L_BRACE)){
            // int a = {};
            if(consumeToken(TK_R_BRACE)){
                break;
            }

            handleExtraInit();
            assert(consumeEnd());
        }
        else{
            parseExpr(0);
        }

        if(peekEnd()){
            break;
        }
        expectToken(TK_COMMA);
    }
}

// int a = 1;
Initializer *handleOneInit(Type *type){
    char *label = NULL;
    Node *exp = parseExpr(getOpPriority(TK_COMMA));

    assert(typeSupportOp(type, ND_ASSIGN, true));
    assert(typeSupportOp(exp->type, ND_ASSIGN, false));

    if(needCast(type, exp->type)){
        exp = newCastNode(exp, type, NULL);
    }
    long val = eval(exp, &label);
    if(label){
        return newLabelInit(label, val);
    }
    else{
        return newValInit(val);
    }
}

// int a = 1;
// int b = {1}; # b has a outlayer
Initializer *handleInit(Type *type, bool outlayer) {
    if(isComplexType(type->kind)){
        if(outlayer){
            // make sure the outlayer brace, int a[2] = { 1, 2 };
            // reset the position at the same time
            if(type->kind != TY_ARRAY || type->basetype != char_type || token->kind != TK_STR){
                token = expectToken(TK_L_BRACE);
            }
        }

        Initializer head = {};
        Initializer *cur = &head;
        if(consumeToken(TK_L_BRACE)){
            // int a[2] = {};
            if(consumeToken(TK_R_BRACE)){
                return newValInit(0);
            }

            if(type->kind == TY_ARRAY){
                int count = 0;
                type->arrayLen = type->isComplete ? type->arrayLen : INT32_MAX;

                if(type->basetype == char_type && peekToken(TK_STR)){
                    char *content = token->content;
                    int len = MIN(type->arrayLen, token->contentLen + 1);
                    for(int i = 0; i < len + 1; ++i){
                        cur->next = newValInit(content[i]);
                        cur = cur->next;
                    }
                }
                else{
                    while(count < type->arrayLen){
                        cur->next = handleInit(type->basetype, false);
                        cur = cur->next;
                        ++count;
                        if(peekEnd()){
                            break;
                        }
                        expectToken(TK_COMMA);
                    }
                }

                if(consumeEnd()){
                    handleExtraInit();
                    assert(consumeEnd());
                }

                type->arrayLen = type->isComplete ? type->arrayLen : count;
                type->isComplete = true;
                return newNestInit(head.next);
            }
            else if(type->kind == TY_UNION){
                VarList *mems = type->members;
                if(mems){
                    cur->next = handleInit(mems->var->type, false);
                    cur = cur->next;
                }

                if(consumeEnd()){
                    // warning
                    handleExtraInit();
                    assert(consumeEnd());
                }
                return head.next;
            }
            // TY_STRUCT
            else{
                VarList *mems = type->members;
                while(mems){
                    cur->next = handleInit(mems->var->type, false);
                    cur = cur->next;
                    mems = mems->next;
                    if(peekEnd()){
                        break;
                    }
                    expectToken(TK_COMMA);
                }
                if(!consumeEnd()){
                    handleExtraInit();
                    assert(consumeEnd());
                }
                return newNestInit(head.next);
            }
        }
        else{
            if(type->kind == TY_ARRAY){
                int count = 0;
                if(type->basetype != char_type || token->kind != TK_STR || !outlayer){
                    assert(type->isComplete);
                }
                if(type->basetype == char_type && peekToken(TK_STR)){
                    char *content = token->content;
                    if(!type->isComplete){
                        type->arrayLen = INT32_MAX;
                    }
                    int len = MIN(type->arrayLen, token->contentLen + 1);
                    for(int i = 0; i < len; ++i){
                        cur->next = newValInit(content[i]);
                        cur = cur->next;
                    }
                    type->isComplete = true;
                    type->arrayLen = len;
                    token = token->next;
                }
                else{
                    while(count < type->arrayLen){
                        cur->next = handleInit(type->basetype, false);
                        cur = cur->next;
                        ++count;
                        if(peekEnd()){
                            break;
                        }
                        expectToken(TK_COMMA);
                    }
                }

                return newNestInit(head.next);
            }
            else if(type->kind == TY_UNION){
                assert(type->isComplete);
                VarList *mems = type->members;
                if(mems){
                    cur->next = handleInit(mems->var->type, false);
                    cur = cur->next;
                }
                return head.next;
            }
            else{   // TY_STRUCT
                assert(type->isComplete);
                VarList *mems = type->members;
                while(mems){
                    cur->next = handleInit(mems->var->type, false);
                    cur = cur->next;
                    mems = mems->next;
                    if(peekEnd()){
                        break;
                    }
                    expectToken(TK_COMMA);
                }
                return newNestInit(head.next);
            }
        }
    }

    if(consumeToken(TK_L_BRACE)){
        if(!outlayer) {
            // warning  Too many braces around scalar initializer
        }
        // int a = {};
        if(consumeToken(TK_R_BRACE)){
            assert(false);  // Scalar initializer cannot be empty
            return newValInit(0);
        }

        Initializer *init = handleInit(type, false);;
        if(!consumeEnd()){
            // warning
            expectToken(TK_COMMA);
            handleExtraInit();
            assert(consumeEnd());
        }
        return init;
    }
    else{
        return handleOneInit(type);
    }
}

Node *movNodeToTail(Node *lhs){
    assert(lhs);
    while(lhs->next){
        lhs = lhs->next;
    }
    return lhs;
}

Node *indexArray(Node *lhs, long index){
    assert(false);
    assert(lhs->type->kind == TY_ARRAY);
    lhs = newBinaryNode(ND_ADD, lhs, newNumNode(index, long_type, NULL), lhs->type->basetype, NULL);
    return newUnaryNode(ND_DEREF, lhs, NULL, NULL);
}

Node *zeroInit(Node *lhs){
    Type *type = lhs->type;
    if(isComplexType(type->kind)){
        Node head = {};
        Node *cur = &head;
        if(type->kind == TY_ARRAY){
            assert(type->isComplete);
            int count = 0;
            while(count < type->arrayLen){
                cur->next = zeroInit(indexArray(lhs, count));
                cur = movNodeToTail(cur);
                ++count;
            }
            return head.next;
        }
        else if(type->kind == TY_UNION){
            VarList *members = type->members;
            if(members){
                Node *member = newMemDotNode(members->var, lhs, NULL);
                cur->next = zeroInit(member);
                cur = movNodeToTail(cur);
            }
            return head.next;
        }
            // TY_STRUCT
        else{
            VarList *members = type->members;
            while(members){
                Node *member = newMemDotNode(members->var, lhs, NULL);
                cur->next = zeroInit(member);
                cur = movNodeToTail(cur);
                members = members->next;
            }
            return head.next;
        }
    }
    else{
        return newBinaryNode(ND_ASSIGN, lhs, newNumNode(0, long_type, NULL), lhs->type, NULL);
    }
}

// lhs: ND_VAR ND_MEMBER like a, a[0]
Node *handleOneInitExpr(Node *lhs){
    Node *exp = parseExpr(getOpPriority(TK_COMMA));

    if(exp->type != lhs->type){
        exp = newCastNode(exp, lhs->type, NULL);
    }

    return newBinaryNode(ND_ASSIGN, lhs, exp, lhs->type, NULL);
}

Node *handleInitExpr(Node *lhs, bool outlayer) {
    Type *type = lhs->type;
    if(isComplexType(type->kind)){
        if(outlayer){
            // make sure the outlayer brace, int a[2] = { 1, 2 };
            // reset the position at the same time
            token = expectToken(TK_L_BRACE);
        }

        Node head = {};
        Node *cur = &head;
        if(consumeToken(TK_L_BRACE)){
            // int a[2] = {};
            if(consumeToken(TK_R_BRACE)){
                return zeroInit(lhs);
            }

            if(type->kind == TY_ARRAY){
                int count = 0;
                if(type->isComplete){
                    while(count < type->arrayLen){
                        cur->next = handleInitExpr(indexArray(lhs, count), false);
                        cur = movNodeToTail(cur);
                        ++count;
                        if(peekEnd()){
                            break;
                        }
                        expectToken(TK_COMMA);
                    }
                    if(!consumeEnd()){
                        handleExtraInit();
                        assert(consumeEnd());
                    }

                    while(count < type->arrayLen){
                        cur->next = zeroInit(indexArray(lhs, count));
                        cur = movNodeToTail(cur);
                        ++count;
                    }
                }
                else{
                    while(1){
                        cur->next = handleInitExpr(indexArray(lhs, count), false);
                        cur = movNodeToTail(cur);
                        ++count;
                        if(consumeEnd()){
                            break;
                        }
                        expectToken(TK_COMMA);
                    }

                    type->arrayLen = count;
                    type->isComplete = true;
                }
                return head.next;
            }
            else if(type->kind == TY_UNION){
                VarList *mems = type->members;
                if(mems){
                    Node *node = newMemDotNode(mems->var, lhs, NULL);
                    cur->next = handleInitExpr(node, false);
                    cur = movNodeToTail(cur);;
                }

                if(consumeEnd()){
                    // warning
                    handleExtraInit();
                    assert(consumeEnd());
                }
                return head.next;
            }
                // TY_STRUCT
            else{
                VarList *mems = type->members;
                while(mems){
                    Node *node = newMemDotNode(mems->var, lhs, NULL);
                    cur->next = handleInitExpr(node, false);
                    cur = movNodeToTail(cur);;
                    mems = mems->next;
                    if(peekEnd()){
                        break;
                    }
                    expectToken(TK_COMMA);
                }
                if(consumeEnd()){
                    handleExtraInit();
                    assert(consumeEnd());
                }
                while(mems){
                    Node *node = newMemDotNode(mems->var, lhs, NULL);
                    cur->next = zeroInit(node);
                    cur = movNodeToTail(cur);
                    mems = mems->next;
                }
                return head.next;
            }
        }
        else{
            if(type->kind == TY_ARRAY){
                int count = 0;
                assert(type->isComplete);
                while(count < type->arrayLen){
                    cur->next = handleInitExpr(indexArray(lhs, count), false);
                    cur = movNodeToTail(cur);
                    ++count;
                    if(peekEnd()){
                        break;
                    }
                    expectToken(TK_COMMA);
                }

                while(count < type->arrayLen){
                    cur->next = zeroInit(indexArray(lhs, count));
                    cur = movNodeToTail(cur);
                    ++count;
                }

                return head.next;
            }
            else if(type->kind == TY_UNION){
                assert(type->isComplete);
                VarList *mems = type->members;
                if(mems){
                    Node *node = newMemDotNode(mems->var, lhs, NULL);
                    cur->next = handleInitExpr(node, false);
                    cur = movNodeToTail(cur);
                }
                return head.next;
            }
            else{   // TY_STRUCT
                assert(type->isComplete);
                VarList *mems = type->members;
                while(mems){
                    Node *node = newMemDotNode(mems->var, lhs, NULL);
                    cur->next = handleInitExpr(node, false);
                    cur = movNodeToTail(cur);
                    mems = mems->next;
                    if(peekEnd()){
                        break;
                    }
                    expectToken(TK_COMMA);
                }

                while(mems){
                    Node *node = newMemDotNode(mems->var, lhs, NULL);
                    cur->next = zeroInit(node);
                    cur = movNodeToTail(cur);
                    mems = mems->next;
                }
                return head.next;
            }
        }
    }
    else{
        if(consumeToken(TK_L_BRACE)){
            if(!outlayer) {
                // warning  Too many braces around scalar initializer
            }
            // int a = {};
            if(consumeToken(TK_R_BRACE)){
                assert(false);
            }

            Node *expr = handleInitExpr(lhs, false);;
            if(!consumeEnd()){
                // warning
                handleExtraInit();
                assert(consumeEnd());
            }
            return expr;
        }
        else{
            return handleOneInitExpr(lhs);
        }
    }
}

// one sentence
void handleDeclaration(Var *func){
    StorageClass storage = UNSPECIFIED;
    if(consumeToken(TK_TYPEDEF)){
        storage = TYPEDEF;
    }
    else if(consumeToken(TK_STATIC)){
        storage = STATIC;
    }
    else if(consumeToken(TK_EXTERN)){
        storage = EXTERN;
    }

    if(consumeToken(TK_SEMICOLON)){
        // warning
        return;
    }

    Type *basetype = getBaseType();
    if(consumeToken(TK_SEMICOLON)){
        // warning
        return;
    }
    basetype = basetype ? basetype : int_type;

    VarList *varHead = NULL;
    if(func){
        varHead = func->locals;
    }
    int first = 1;
    while(1) {
        Var *var = calloc(1, sizeof(Var));
        var->storage = storage;
        handleDeclarator(basetype, var);

        if(consumeToken(TK_ASSIGN)) {
            assert(var->type->kind != TY_FUNCTION);
            assert(storage != TYPEDEF);
            if(scopeDepth == 0){
                if(storage == EXTERN){
                    // warning EXTERN
                    var->storage = UNSPECIFIED;
                }
                // top-level
                var->init = handleInit(var->type, true);
            }
            else{
                assert(storage != EXTERN);
                // in function
                var->initExpr = handleInitExpr(newVarNode(var, NULL), true);
            }
        }

        Var *find = findVar(varList, var->pos, var->len);
        if(var->type->kind != TY_FUNCTION){
            if(find){
                // warning: shadows
                assert(var->scopeDepth != find->scopeDepth);
                assert(var->storage != EXTERN);
                if(var->storage == STATIC){
                    var->label = newLabel();
                    varList = addHead(varList, var);
                    globals = addTail(globals, var);
                }
                else if(var->storage == TYPEDEF){
                    varList = addHead(varList, var);
                }
                else{
                    assert(var->storage == UNSPECIFIED);
                    varList = addHead(varList, var);
                    func->locals = addTail(func->locals, var);
                }
            }
            else{
                if(var->storage == EXTERN){
                    assert(var->scopeDepth == 0);
                    varList = addHead(varList, var);
                }
                else if(var->storage == STATIC){
                    if(scopeDepth != 0) {
                        var->label = newLabel();
                    }
                    varList = addHead(varList, var);
                    globals = addTail(globals, var);
                }
                else if(var->storage == TYPEDEF){
                    varList = addHead(varList, var);
                }
                else{
                    assert(var->storage == UNSPECIFIED);
                    varList = addHead(varList, var);
                    if(scopeDepth == 0){
                        globals = addTail(globals, var);
                    }
                    else{
                        func->locals = addTail(func->locals, var);
                    }
                }
            }
        }
        else{
            assert(var->scopeDepth == 0);
            var->storage = var->storage == UNSPECIFIED ? EXTERN : var->storage;
            if(peekToken(TK_L_BRACE)){
                assert(var->storage != TYPEDEF);
                assert(var->scopeDepth == 0);
                var->type->isComplete = true;
                functions = addTail(functions, var);
            }

            if(find){
                assert(equalType(find->type, var->type));
                assert(find->storage == var->storage);
                if(find->type->isComplete){
                    if(var->type->isComplete){
                        assert(false);
                    }
                    else{
                        // warning: redundant
                    }
                }
                else{
                    if(var->type->isComplete){
                        var = find;
                    }
                    else{
                        // warning: redundant
                    }
                }
            }
            else{
                varList = addHead(varList, var);
            }
        }

        if (consumeToken(TK_COMMA)) {
            first = 0;
            continue;
        }
        else if (consumeToken(TK_SEMICOLON)){
            break;
        }
        else if (first && consumeToken(TK_L_BRACE)) {
            assert(var->type->kind == TY_FUNCTION);
            handleFuncBody(var);
            break;
        }
        else {
            assert(false);
        }
    }

    if(func){
        varHead = varHead->next;
        while(varHead){
            func->exprs->next = varHead->var->initExpr;
            while(func->exprs->next){
                func->exprs = func->exprs->next;
            }
            varHead = varHead->next;
        }
    }
}

Program *parse(){
    VarList varHead = {};
    VarList funHead = {};

    globals = &varHead;
    functions = &funHead;

    while(1) {
        while (consumeToken(TK_SEMICOLON));
        if (consumeToken(TK_EOF)) {
            break;
        }
        handleDeclaration(NULL);
    }

    Program *program = calloc(1, sizeof(Program));
    program->globals = varHead.next;
    program->functions = funHead.next;
    return program;
}

#pragma clang diagnostic pop
#pragma clang diagnostic pop