//
// Created by WTF on 2020/2/12.
//

#ifndef UNTITLED60_GEMCC_H
#define UNTITLED60_GEMCC_H

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define PROG_SIZE 1024 * 1024
#define STR_SIZE 1024

typedef struct Type Type;
typedef struct Var Var;
typedef struct VarList VarList;
typedef struct Node Node;
typedef struct Initializer Initializer;
typedef struct Label Label;

//"break", "case", "char", "const", "continue", "default", "do",
//"else", "enum", "extern", "for", "goto", "if", "int",
//"long", "return", "short", "signed", "sizeof", "static", "struct",
//"switch", "typedef", "void", "while"};
typedef enum TokenKind TokenKind;
enum TokenKind{
    TK_EOF,
    TK_STR,

    // C reserved
            TK_BREAK,
    TK_CASE,
    TK_CHAR,
    TK_CONST,
    TK_CONTINUE,
    TK_DEFAULT,
    TK_DO,
    TK_ELSE,
    TK_ENUM,
    TK_EXTERN,
    TK_FOR,
    TK_GOTO,
    TK_IF,
    TK_INT,
    TK_LONG,
    TK_RETURN,
    TK_SHORT,
    TK_SIGNED,
    TK_SIZEOF,
    TK_STATIC,
    TK_STRUCT,
    TK_UNION,
    TK_SWITCH,
    TK_TYPEDEF,
    TK_VOID,
    TK_WHILE,
    TK_BOOL,
    TK_ALIGNOF,

    // C delimiter
            TK_L_BRACE,     // {
    TK_R_BRACE,     // }

    // C punctuation
            TK_L_BRACKET,   // [
    TK_R_BRACKET,   // ]
    TK_L_PAREN,     // (
    TK_R_PAREN,     // )
    TK_DOT,
    TK_MEMBER,      // ->

    TK_BIT_NOT,
    TK_INC,
    TK_DEC,
    TK_LOG_NOT,
    TK_DIV,
    TK_MUL_DEREF,
    TK_MOD,
    TK_ADD,
    TK_SUB,
    TK_SHL,
    TK_SHR,
    TK_GT,
    TK_GE,
    TK_LT,
    TK_LE,
    TK_EQ,
    TK_NEQ,
    TK_BIT_AND,
    TK_BIT_XOR,
    TK_BIT_OR,
    TK_LOG_AND,
    TK_LOG_OR,
    TK_QUESTION,
    TK_COLON,
    TK_ASSIGN,
    TK_DIV_ASSIGN,
    TK_MUL_ASSIGN,
    TK_MOD_ASSIGN,
    TK_ADD_ASSIGN,
    TK_SUB_ASSIGN,
    TK_SHL_ASSIGN,
    TK_SHR_ASSIGN,
    TK_BITAND_ASSIGN,
    TK_BITOR_ASSIGN,
    TK_BITXOR_ASSIGN,
    TK_COMMA,
    TK_SEMICOLON,

    TK_IDENTIFIER,
    TK_NUM,
    TK_ELLIPSIS,

    TK_ARGU,
};

typedef struct Token Token;
struct Token{
    TokenKind kind;
    char *pos;
    int len;

    long val;
    Type *type;

    char *content;
    int contentLen;

    Token *next;

    int index; // macro
};

extern char *user_input;
extern char *filename;
extern Token *token;


typedef enum StorageClass StorageClass;
enum StorageClass{
    UNSPECIFIED,
    TYPEDEF,
    EXTERN,
    STATIC
};

// regard all kinds of variables as well as function as Var
typedef enum TypeKind TypeKind;
enum TypeKind{
    // builtin type
    TY_VOID,
    TY_BOOL,
    TY_CHAR,
    TY_SHORT,
    TY_INT,
    TY_LONG,
    // function
    TY_FUNCTION,
    // pointer
    TY_POINTER,
    TY_ARRAY,
    // user-defined type, struct enum union
    TY_STRUCT,
    TY_UNION,
    TY_ENUM
};

typedef struct Type Type;
struct Type{
    Type *next;
    TypeKind kind;
    int size;
    int align;
    bool isComplete;
    // for pointer and array
    Type *basetype;
    int arrayLen;
    // for struct enum and union
    char *pos;
    int len;
    // struct or union
    VarList *members;
    int scopeDepth;
    // function
    bool hasVarArgs;
    VarList *params;
    int argc;
    Type *retType;
};

Type *void_type;
Type *bool_type;
Type *char_type;
Type *short_type;
Type *int_type;
Type *long_type;

struct Var{
    Type *type;
    char *pos;
    int len;
    int scopeDepth;    // local: scopeDepth != 0  global: == 0
    StorageClass storage;
    Initializer *init;
    Node *initExpr;

    char *label;
    int offset;     // for struct member
    int enumVal;    // for enumerator
    // for function
    int stackSize;
    VarList *locals;
    Node *exprs;    // expressions
};

struct VarList{
    VarList *next;
    Var *var;
};

typedef enum NodeKind NodeKind;
enum NodeKind{
    ND_NUM,
    ND_VAR,

    ND_CALL,
    ND_MEMBER_DOT,
    ND_MEMBER_PTR,

    ND_POS,
    ND_NEG,
    ND_BITNOT,
    ND_PRE_INC,
    ND_POST_INC,
    ND_PRE_DEC,
    ND_POST_DEC,
    ND_DEREF,
    ND_ADDR,
    ND_LOGNOT,
    ND_CAST,

    ND_DIV,
    ND_MUL,
    ND_MOD,

    ND_ADD,
    ND_SUB,

    ND_SHL,
    ND_SHR,

    ND_GT,
    ND_GE,
    ND_LT,
    ND_LE,

    ND_EQ,
    ND_NEQ,

    ND_BITAND,
    ND_BITXOR,
    ND_BITOR,
    ND_LOGAND,
    ND_LOGOR,

    ND_COND,    // ? :

    ND_ASSIGN,
    ND_DIV_ASSIGN,
    ND_MUL_ASSIGN,
    ND_MOD_ASSIGN,
    ND_ADD_ASSIGN,
    ND_SUB_ASSIGN,
    ND_SHL_ASSIGN,
    ND_SHR_ASSIGN,
    ND_BITAND_ASSIGN,
    ND_BITXOR_ASSIGN,
    ND_BITOR_ASSIGN,

    ND_COMMA,

    ND_IF,
    ND_WHILE,
    ND_DO,
    ND_FOR,
    ND_GOTO,
    ND_RETURN,
    ND_SWITCH,
    ND_BREAK,
    ND_LABEL,
    ND_CASE,
    ND_CONTINUE,
    ND_DEFAULT,
};

typedef struct Node Node;
struct Node{
    Node *next;
    Node *nested;
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    Type *type;
    long val;

    char *pos;  // for goto expr, pos stores the dest label
    int len;
    Node *args;

    Var *member;

    Var *var;

    // conditional
    Node *cond;
    Node *then;
    Node *els;
    Node *init;
    Node *incre;
    // switch, expressions at then
    int labelSeq;
};

struct Initializer{
    Initializer *next;
    Initializer *nestedInit;

    long val;

    char *label;
    long addend;
};

typedef struct Program Program;
struct Program{
    VarList *globals;
    VarList *functions;
};

Token *tokenize(char * buf, char *end, bool expandMacro, bool addEof);
Program *parse();
char *readFile();

void codegen(Program *program);
int alignTo(int offset, int align);

int isComplexType(TypeKind kind);

Type *decideType(Type *ltype, Type *rtype);
Node *findLabel(Node *labels, char *name, int len);
Token *consumeTk(TokenKind kind);

bool checkVar(Var *var, StorageClass storage, char *name);
bool typeSupportOp(Type *type, NodeKind kind, bool left);
bool nodeSupportOp(NodeKind operatorKind, NodeKind oprandKind);

bool equalType(Type *ltype, Type *rtype);

bool isAddressType(Type *type);

bool isArrayOrFunc(Type *type);

bool isIntegerType(Type *type);

// fixme
bool needCast(Type *ltype, Type *rtype);

Type *newType(TypeKind kind, int size, int align);

Type *arrayOf(Type *basetype, bool isComplete, int len);

Type *pointTo(Type *basetype);


// node.c
Node *newNode(NodeKind kind, Type *type, Token *tok);

Node *newBinaryNode(NodeKind kind, Node *lhs, Node *rhs, Type *type, Token *tok);

Node *newUnaryNode(NodeKind kind, Node *lhs, Type *type, Token *tok);

Node *newNumNode(long val, Type *type, Token *tok);

Node *newCastNode(Node *node, Type *type, Token *tok);

Node *newVarNode(Var *var, Token *tok);

Node *newMemDotNode(Var *member, Node *lhs, Token *tok);

Node *newMemPtrNode(Var *member, Node *lhs, Token *tok);

Node *newCondNode(Node *cond, Node *then, Node *els, Type *type, Token *tok);

Node *newStatNode(NodeKind kind, Token *tok, Node *init, Node *cond, Node *then, Node *incre, Node *els);

// initializer.c

Initializer *newLabelInit(char *label, long addend);

Initializer *newValInit(long val);

Initializer *newNestInit(Initializer *nested);


// token.c

int peekEnd();

Token *consumeEnd();

Token *expectToken(TokenKind kind);

int peekToken(TokenKind kind);

Token *consumeToken(TokenKind kind);

Token *expectTokenStatic(Token *tok, TokenKind kind);

#endif //UNTITLED60_GEMCC_H
