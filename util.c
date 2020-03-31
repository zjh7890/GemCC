//
// Created by WTF on 2020/2/16.
//

#include "gemcc.h"

bool equalTokenKind(Token *tok, TokenKind kind){
    assert(token);
    return tok->kind == kind;
}

bool typeSupportOp(Type *type, NodeKind kind, bool left){
    switch (type->kind){
        case TY_VOID:
            return false;
        case TY_STRUCT:
        case TY_UNION:
            switch (kind){
                case ND_MEMBER_DOT:
                case ND_DEREF:
                    return true;
                default:
                    return false;
            }
        case TY_BOOL:
        case TY_CHAR:
        case TY_SHORT:
        case TY_INT:
        case TY_LONG:
            switch (kind){
                case ND_CALL:
                case ND_MEMBER_DOT:
                case ND_MEMBER_PTR:
                case ND_DEREF:
                    return false;
                default:
                    return true;
            }
        case TY_FUNCTION:
            switch (kind){
                case ND_CALL:
                case ND_DEREF:
                case ND_ADDR:
                case ND_LOGNOT:
                case ND_CAST:
                case ND_ADD:
                case ND_SUB:
                case ND_GE:
                case ND_GT:
                case ND_LE:
                case ND_LT:
                case ND_NEQ:
                case ND_EQ:
                case ND_LOGAND:
                case ND_LOGOR:
                case ND_COND:
                    return true;
                case ND_ASSIGN:
                case ND_ADD_ASSIGN:
                case ND_SUB_ASSIGN:
                    if(!left){
                        return true;
                    }
                    else{
                        return false;
                    }
                default:
                    return false;
            }
        case TY_POINTER:
            assert(type->basetype);
            switch (kind){
                case ND_CALL:
                    if(type->basetype->kind == TY_FUNCTION){
                        return true;
                    }
                    return false;
                case ND_MEMBER_PTR:
                    if(type->basetype->kind == TY_UNION || type->basetype->kind == TY_STRUCT){
                        return true;
                    }
                    return false;
                case ND_PRE_INC:
                case ND_PRE_DEC:
                case ND_POST_INC:
                case ND_POST_DEC:
                case ND_DEREF:
                case ND_ADDR:
                case ND_LOGNOT:
                case ND_CAST:
                case ND_ADD:
                case ND_SUB:
                case ND_GE:
                case ND_GT:
                case ND_LE:
                case ND_LT:
                case ND_NEQ:
                case ND_EQ:
                case ND_LOGAND:
                case ND_LOGOR:
                case ND_COND:
                case ND_ASSIGN:
                case ND_ADD_ASSIGN:
                case ND_SUB_ASSIGN:
                    return true;
                default:
                    return false;
            }
        case TY_ARRAY:
            switch (kind){
                case ND_DEREF:
                case ND_ADDR:
                case ND_LOGNOT:
                case ND_CAST:
                case ND_ADD:
                case ND_SUB:
                case ND_GE:
                case ND_GT:
                case ND_LE:
                case ND_LT:
                case ND_NEQ:
                case ND_EQ:
                case ND_LOGAND:
                case ND_LOGOR:
                case ND_COND:
                    return true;
                case ND_ASSIGN:
                case ND_ADD_ASSIGN:
                case ND_SUB_ASSIGN:
                    if(!left){
                        return true;
                    }
                    else{
                        return false;
                    }
                default:
                    return false;
            }
        default:
            assert(false);
    }
}

// fixme
bool nodeSupportOp(NodeKind operatorKind, NodeKind oprandKind){
    return true;
}

char *readFile(){
    FILE *fp = fopen(filename, "r");
    if(!fp){
        printf("%s(): %s\n", __FUNCTION__, strerror(errno));
        exit(1);
    }

    char buf[PROG_SIZE];
    int count = fread(buf, 1, sizeof(buf), fp);
    if(count == PROG_SIZE){
        printf("%s(): %s\n", __FUNCTION__, "Input file too big");
        exit(1);
    }
    buf[count] = '\0';
    return strdup(buf);
}

Type *decideType(Type *ltype, Type *rtype){
    if(ltype->size == 8 || rtype->size == 8){
        return long_type;
    }
    return int_type;
}

Node *findLabel(Node *exprs, char *name, int len){
    // fixme: cannot find nested label
    while(exprs){
        if(exprs->kind != ND_LABEL){
            exprs = exprs->next;
            continue;
        }

        if(exprs->len == len &&
           !strncmp(exprs->pos, name, len)){
            return exprs;
        }
        exprs = exprs->next;
    }
    return NULL;
}

int alignTo(int offset, int align){
    return (offset + align - 1) & (~(align - 1));
}

int isComplexType(TypeKind kind){
    return kind == TY_ARRAY || kind == TY_UNION || kind == TY_STRUCT;
}

bool checkVar(Var *var, StorageClass storage, char *name){
    if(var->storage != storage){
        return false;
    }

    return !strncmp(var->pos, name, MAX(var->len, strlen(name)));
}
