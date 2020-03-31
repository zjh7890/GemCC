//
// Created by WTF on 2020/3/2.
//

#include "gemcc.h"

Type *void_type = &(Type){  NULL, TY_VOID, 1, 1, false};
Type *bool_type = &(Type){  NULL, TY_BOOL, 1, 1, true};
Type *char_type = &(Type){  NULL, TY_CHAR, 1, 1, true};
Type *short_type = &(Type){ NULL, TY_SHORT, 2, 2, true};
Type *int_type = &(Type){   NULL, TY_INT, 4, 4, true};
Type *long_type = &(Type){  NULL, TY_LONG, 8, 8, true};

// fixme: same function type means same return type and params type
bool equalType(Type *ltype, Type *rtype){
    if(ltype->kind != rtype->kind){
        return false;
    }
    if(ltype == rtype){
        return true;
    }
    if(ltype->kind == TY_ARRAY && ltype->arrayLen != rtype->arrayLen){
        return false;
    }
    if(ltype->kind == TY_FUNCTION){
        if(equalType(ltype->retType, rtype->retType)){
            if(ltype->params){
                VarList *list1 = ltype->params;
                VarList *list2 = rtype->params;
                while(list1){
                    if(!list2){
                        return false;
                    }
                    assert(equalType(list1->var->type, list2->var->type));
                    list1 = list1->next;
                    list2 = list2->next;
                }
                if(!list1 && !list2){
                    return true;
                }
                return false;
            }
            else{
                return !rtype->params;
            }
        }
        else{
            return false;
        }
    }
    return equalType(ltype->basetype, rtype->basetype);
}

bool isAddressType(Type *type){
    return type->kind == TY_FUNCTION || type->kind == TY_POINTER
           || type->kind == TY_ARRAY;
}

bool isArrayOrFunc(Type *type){
    return type->kind == TY_FUNCTION || type->kind == TY_ARRAY;
}

bool isIntegerType(Type *type){
    return type->kind == TY_BOOL || type->kind == TY_CHAR ||
    type->kind == TY_SHORT || type->kind == TY_INT || type->kind == TY_LONG;
}

Type *newType(TypeKind kind, int size, int align){
    Type *type = calloc(1, sizeof(Type));
    type->kind = kind;
    type->size = size;
    type->align = align;
    return type;
}

Type *arrayOf(Type *basetype, bool isComplete, int len){
    Type *type = newType(TY_ARRAY, basetype->size * len, basetype->align);
    type->basetype = basetype;
    type->isComplete = isComplete;
    type->arrayLen = len;
    return type;
}

Type *pointTo(Type *basetype){
    assert(basetype);
    Type *type = newType(TY_POINTER, 8, 8);
    type->basetype = basetype;
    return type;
}

// fixme
bool needCast(Type *ltype, Type *rtype){
    if(ltype == rtype){
        return false;
    }

    if(ltype->size == rtype->size){
        return false;
    }

    if(isAddressType(ltype) && isAddressType(rtype)){
        return false;
    }

    return true;
}
