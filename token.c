//
// Created by WTF on 2020/3/26.
//
#include "gemcc.h"

int peekEnd(){
    if(token->kind == TK_EOF){
        return 0;
    }

    if(token->kind == TK_R_BRACE){
        return 1;
    }

    if(token->kind == TK_COMMA &&
       token->next->kind == TK_R_BRACE){
        return 1;
    }

    return 0;
}

Token *consumeEnd(){
    if(peekEnd()){
        Token *tok;
        if(token->kind == TK_R_BRACE){
            tok = token;
            token = token->next;
            return tok;
        }

        if(token->kind == TK_COMMA){
            tok = token;
            token = token->next->next;
            return tok;
        }
        assert(false);
    }

    return NULL;
}

Token *expectToken(TokenKind kind){
    assert(token->kind == kind);
    Token *tok = token;
    token = token->next;
    return tok;
}

int peekToken(TokenKind kind){
    return token->kind == kind;
}

Token *consumeToken(TokenKind kind){
    if(token->kind == kind){
        Token *tok = token;
        token = token->next;
        return tok;
    }
    return NULL;
}

Token *expectTokenStatic(Token *tok, TokenKind kind){
    if(!(tok && tok->kind == kind)){
        printf("hit\n");
    }
    assert(tok && tok->kind == kind);
    return tok->next;
}
