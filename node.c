//
// Created by WTF on 2020/3/26.
//
#include "gemcc.h"

Node *newNode(NodeKind kind, Type *type, Token *tok){
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    if(tok){
        node->pos = tok->pos;
        node->len = tok->len;
    }
    node->type = type;
    return node;
}

Node *newBinaryNode(NodeKind kind, Node *lhs, Node *rhs, Type *type, Token *tok){
    Node *node = newNode(kind, type, tok);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *newUnaryNode(NodeKind kind, Node *lhs, Type *type, Token *tok){
    Node *node = newNode(kind, type, tok);
    node->lhs = lhs;
    return node;
}

Node *newNumNode(long val, Type *type, Token *tok) {
    Node *node = newNode(ND_NUM, type, tok);
    node->val = val;
    return node;
}

Node *newCastNode(Node *node, Type *type, Token *tok){
    if(node->type != type){
        Node *tmp = newNode(ND_CAST, type, tok);
        tmp->type = type;
        tmp->lhs = node;
        node = tmp;
    }
    return node;
}

Node *newVarNode(Var *var, Token *tok){
    Node *node = newNode(ND_VAR, var->type, tok);
    node->var = var;
    return node;
}

Node *newMemDotNode(Var *member, Node *lhs, Token *tok){
    Node *node = newUnaryNode(ND_MEMBER_DOT, lhs, member->type, tok);
    node->member = member;
    return node;
}

Node *newMemPtrNode(Var *member, Node *lhs, Token *tok){
    Node *node = newUnaryNode(ND_MEMBER_PTR, lhs, member->type, tok);
    node->member = member;
    return node;
}

Node *newCondNode(Node *cond, Node *then, Node *els, Type *type, Token *tok){
    Node *node = newNode(ND_COND, type, tok);
    node->then = then;
    node->els = els;
    return node;
}

Node *newStatNode(NodeKind kind, Token *tok, Node *init, Node *cond, Node *then, Node *incre, Node *els){
    Node *node = newNode(kind, NULL, tok);
    node->init = init;
    node->cond = cond;
    node->then = then;
    node->incre = incre;
    node->els = els;
    return node;
}
