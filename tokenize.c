#include "gemcc.h"

typedef enum MacroKind MacroKind;
enum MacroKind {
    MACRO_WITH_PARAMS,
    MACRO_WITHOUT_PARAMS
};

typedef struct Macro Macro;
struct Macro {
    Macro *next;
    MacroKind kind;
    char *name;
    int len;

    int argc;   // for func-like macro
    Token *toReplace;
};

Macro *macros;
int puncTable[128];
char *reserved[] = {"break", "case", "char", "const", "continue", "default", "do",
                    "else", "enum", "extern", "for", "goto", "if", "int",
                    "long", "return", "short", "signed", "sizeof", "static", "struct",
                    "switch", "typedef", "void", "while", "union",
                    "_Bool", "_Alignof"};

TokenKind reservedToken[] = {
        TK_BREAK, TK_CASE, TK_CHAR, TK_CONST, TK_CONTINUE, TK_DEFAULT, TK_DO,
        TK_ELSE, TK_ENUM, TK_EXTERN, TK_FOR, TK_GOTO, TK_IF, TK_INT,
        TK_LONG, TK_RETURN, TK_SHORT, TK_SIGNED, TK_SIZEOF, TK_STATIC, TK_STRUCT,
        TK_SWITCH, TK_TYPEDEF, TK_VOID, TK_WHILE, TK_UNION,
        TK_BOOL, TK_ALIGNOF
};

char *strnstr(char *str, char *match, int n){
    // fixme use kmp
    int len = strlen(match);
    if(len > n){
        return NULL;
    }
    for(int i = 0; i <= n - len; ++i){
        if(!strncmp(str + i, match, len)){
            return str + i;
        }
    }
    return NULL;
}

Macro *newMacro(MacroKind kind, char *name, int len){
    Macro *tmp = calloc(1, sizeof(Macro));
    tmp->kind = kind;
    tmp->name = name;
    tmp->len = len;
    return tmp;
}

void pushMacro(Macro *mac){
    mac->next = macros;
    macros = mac;
}

Macro *findMacro(MacroKind kind, char *name, int len){
    Macro *cur = macros;
    while(cur){
        if(cur->kind == kind &&
                !strncmp(name, cur->name, MAX(cur->len, len))){
            return cur;
        }
        cur = cur->next;
    }
    return NULL;
}

Token *dupTokenList(Token *tok){
    Token head;
    Token *cur = &head;
    while(tok){
        Token *newTk = calloc(1, sizeof(Token));
        *newTk = *tok;
        cur->next = newTk;
        cur = newTk;
        tok = tok->next;
    }
    return head.next;
}

Token *moveToTail(Token *tok){
    assert(tok);
    while(tok->next){
        tok = tok->next;
    }
    return tok;
}

Token *replaceMacro(Macro *mac, Token **vec){
    Token head = {};
    Token *cur = &head;
    Token *toReplace = mac->toReplace;
    while(toReplace){
        if(toReplace->kind != TK_ARGU){
            Token *newTK = calloc(1, sizeof(Token));
            *newTK = *toReplace;
            cur->next = newTK;
            cur = newTK;
        }
        else{
            cur->next = dupTokenList(vec[toReplace->index]);
            cur = moveToTail(cur);
        }
        toReplace = toReplace->next;
    }
    return head.next;
}

Token *newToken(TokenKind kind, char *pos, int len){
    Token *tmp = calloc(1, sizeof(Token));
    tmp->kind = kind;
    tmp->pos = pos;
    tmp->len = len;
    return tmp;
}

Token *newArguToken(int index){
    Token *tmp = calloc(1, sizeof(Token));
    tmp->kind = TK_ARGU;
    tmp->index = index;
    return tmp;
}

int isIdChar(int c){
    return isalnum(c) || c == '_';
}

int isIdStartChar(int c){
    return isalpha(c) || c == '_';
}

int isCRLF(int c){
    return c == '\r' || c == '\n';
}

void initPuncTable(){
    puncTable['!'] = 1;
    puncTable['%'] = 1;
    puncTable['&'] = 1;
    puncTable['('] = 1;
    puncTable[')'] = 1;
    puncTable['*'] = 1;
    puncTable['+'] = 1;
    puncTable[','] = 1;
    puncTable['-'] = 1;
    puncTable['.'] = 1;
    puncTable['/'] = 1;
    puncTable[':'] = 1;
    puncTable[';'] = 1;
    puncTable['<'] = 1;
    puncTable['='] = 1;
    puncTable['>'] = 1;
    puncTable['?'] = 1;
    puncTable['['] = 1;
    puncTable[']'] = 1;
    puncTable['{'] = 1;
    puncTable['|'] = 1;
    puncTable['}'] = 1;
    puncTable['~'] = 1;
}

int isCPunct(int c){
    if(c < 0 || c > 127){
        return 0;
    }
    return puncTable[c];
}

int findReserved(char *buf, int len){
    for(int i = 0; i < sizeof(reserved) / sizeof(reserved[0]); i++){
        int idlen = MAX(len, strlen(reserved[i]));
        if(!strncmp(buf, reserved[i], idlen)){
            return i;
        }
    }
    return -1;
}

// \f \n \r \t \v ' '
char *eatSpace(char *pos){
    while(*pos && isspace(*pos)){
        ++pos;
    }
    return pos;
}

// \t \v ' '
char *eatBlank(char *pos){
    while(*pos && isblank(*pos)){
        ++pos;
    }
    return pos;
}

char *eatLine(char *pos){
    while(*pos && !isCRLF(*pos)){
        ++pos;
    }
    return pos;
}

char *eatUntilChar(char *pos, int c){
    while(*pos && *pos != c){
        ++pos;
    }
    return pos;
}

char *eatIdChar(char *pos){
    while(*pos && isIdChar(*pos)){
        ++pos;
    }
    return pos;
}

char *expectUntilChar(char *pos, int c){
    char *p = eatUntilChar(pos, c);
    assert(*p == c);
    return p;
}

char *searchDelimiter(char *pos, char *end, int c){
    while(*pos != c){
        if(pos >= end){
            return NULL;
        }

        if(*pos == '"'){
            ++pos;
            pos = expectUntilChar(pos, '"');
        } else if(*pos == '\''){
            ++pos;
            pos = expectUntilChar(pos, '\'');
        }
        ++pos;
    }
    return pos;
}

char *expectBlank(char *pos){
    assert(isblank(*pos));
    return eatBlank(pos);
}

char *expectIdentifier(char *pos){
    assert(isIdStartChar(*pos));
    return eatIdChar(pos);
}

char *expectChar(char *pos, int c){
    assert(*pos == c);
    ++pos;
    return pos;
}

int locateToken(Token *head, char *name, int len){
    Token *cur = head;
    int index = 0;
    while(cur){
        if(!strncmp(cur->pos, name, MAX(cur->len, len))){
            return index;
        }
        ++index;
        cur = cur->next;
    }
    return -1;
}

int getEscapeChar(int c){
    assert(c);
    switch (c){
        case 'a': return '\a';
        case 'b': return '\b';
        case 't': return '\t';
        case 'n': return '\n';
        case 'v': return '\v';
        case 'f': return '\f';
        case 'r': return '\r';
        case 'e': return 27;
        case '0': return 0;
        default: return c;
    }
}

// tokenize buf from 0 to end - 1 or '\0'
Token *tokenize(char * buf, char *end, bool expandMacro, bool addEof){
    initPuncTable();    // fixme init multiple times
    Token head = {};
    Token *cur = &head;
    char *pos = buf;
    char *posBeforeEat;  // for general use: position before eat
    while(*pos && pos < end){
        pos = eatSpace(pos);
        if(!*pos){
            break;
        }

        if(*pos == '#') {
            // fixme
            // 123 #define A 123    // # should be seperate line
            ++pos;
            pos = eatBlank(pos);
            posBeforeEat = pos;
            pos = expectIdentifier(pos);
            if(!strncmp(posBeforeEat, "define", MAX(pos - posBeforeEat, strlen("define")))){
                pos = expectBlank(pos);
                posBeforeEat = pos;
                pos = expectIdentifier(pos);

                Macro *mac;
                if(*pos == '('){
                    mac = newMacro(MACRO_WITH_PARAMS, posBeforeEat, pos - posBeforeEat);
                    ++pos;
                    pos = eatBlank(pos);
                    Token argvHead = {};
                    Token *curArg = &argvHead;
                    int argCount = 0;
                    if(*pos != ')'){
                        while(1){
                            ++argCount;
                            posBeforeEat = pos;
                            pos = expectIdentifier(pos);
                            curArg->next = newToken(TK_IDENTIFIER, posBeforeEat, pos - posBeforeEat);
                            curArg = curArg->next;

                            pos = eatBlank(pos);
                            if(*pos == ')'){
                                break;
                            }

                            pos = expectChar(pos, ',');
                            pos = eatBlank(pos);
                        }
                    }
                    mac->argc = argCount;
                    ++pos;

                    pos = eatBlank(pos);
                    if(isCRLF(*pos)){
                        mac->toReplace = NULL;
                    } else {
                        posBeforeEat = pos;
                        pos = eatLine(pos);
                        // #define A /* ...
                        char *newpos = strnstr(posBeforeEat, "/*", pos - posBeforeEat);
                        pos = newpos ? newpos : pos;
                        Token *toReplace = tokenize(posBeforeEat, pos, false, false);
                        mac->toReplace = toReplace;
                        while(toReplace){
                            int index;
                            if(toReplace->kind == TK_IDENTIFIER &&
                               (index = locateToken(argvHead.next, toReplace->pos, toReplace->len)) >= 0){
                                Token *next = toReplace->next;
                                *toReplace = *newArguToken(index);
                                toReplace->next = next;
                            }
                            toReplace = toReplace->next;
                        }
                    }
                } else {
                    mac = newMacro(MACRO_WITHOUT_PARAMS, posBeforeEat, pos - posBeforeEat);
                    // #define A\n
                    if(isCRLF(*pos)){
                        mac->toReplace = NULL;
                    } else {
                        pos = expectBlank(pos);
                        if(isCRLF(*pos)){
                            mac->toReplace = NULL;
                        }
                        else{
                            posBeforeEat = pos;
                            pos = eatLine(pos);
                            // #define A /* ...
                            char *newpos = strnstr(posBeforeEat, "/*", pos - posBeforeEat);
                            pos = newpos ? newpos : pos;
                            mac->toReplace = tokenize(posBeforeEat, pos, false, false);
                        }
                    }
                }
                pushMacro(mac);
            }
            else if(!strncmp(posBeforeEat, "include", MAX(pos - posBeforeEat, strlen("include")))){
                pos = expectBlank(pos);
                pos = expectChar(pos, '"');
                posBeforeEat = pos;
                pos = expectUntilChar(pos, '"');

                char *saveFilename = filename;
                char *saveInput = user_input;

                filename = strndup(posBeforeEat, pos - posBeforeEat);
                user_input = readFile(filename);

                cur->next = tokenize(user_input, user_input + PROG_SIZE, true, false);
                while(cur->next != NULL){
                    cur = cur->next;
                }

                ++pos;
                filename = saveFilename;
                user_input = saveInput;
            }
            else {
                printf("%s(): %s\n", __FUNCTION__, "Error keyword: '#'");
                exit(1);
            }
        }
        else if(!strncmp(pos, "//", 2)){
            pos += 2;
            pos = eatLine(pos);
        }
        else if(!strncmp(pos, "/*", 2)){
            pos += 2;
            pos = strstr(pos, "*/");
            if(!pos){
                printf("%s(): %s\n", __FUNCTION__, "unclosed /* */");
                exit(1);
            }
            pos += 2;
        }
        else if(*pos == '"'){
            posBeforeEat = pos;
            ++pos;
            char str[STR_SIZE];
            int count = 0;
            while(*pos != '"'){
                assert(*pos && count != STR_SIZE);
                assert(!isCRLF(*pos));
                if(*pos == '\\'){
                    ++pos;
                    assert(!isCRLF(*pos));
                    str[count++] = getEscapeChar(*pos++);
                } else {
                    str[count++] = *pos++;
                }
            }
            str[count] = 0;
            ++pos;
            Token *newTK = newToken(TK_STR, posBeforeEat, pos - posBeforeEat);
            newTK->content = strndup(str, count + 1);
            newTK->contentLen = count;
            cur->next = newTK;
            cur = newTK;
        }
        else if(*pos == '\''){
            posBeforeEat = pos;
            int ch;
            ++pos;
            assert(*pos);
            if(*pos == '\\'){
                ++pos;
                assert(*pos);
                assert(!isCRLF(*pos));
                ch = getEscapeChar(*pos++);
            }
            else{
                assert(!isCRLF(*pos));
                ch = *pos++;
            }
            pos = expectChar(pos, '\'');
            Token *newTk = newToken(TK_NUM, pos, pos - posBeforeEat);
            newTk->type = char_type;
            newTk->val = ch;
            cur->next = newTk;
            cur = newTk;
        }
        else if(isdigit(*pos)){
            posBeforeEat = pos;
            int base;
            Type *type = int_type;
            if(!strncasecmp(pos, "0x", 2)){
                pos += 2;
                base = 16;
                assert(isdigit(*pos));
            }
            else if(!strncasecmp(pos, "0b", 2)){
                pos += 2;
                base = 2;
                assert(isdigit(*pos));
            }
            else if(*pos == '0'){
                ++pos;
                base = 8;
            }
            else{
                base = 10;
            }

            char *suffix;
            long num = strtol(pos, &suffix, base);
            if(errno != 0){
                printf("%s(): %s\n", __FUNCTION__, "numeric constant too large");
            }

            pos = suffix;
            pos = eatIdChar(pos);

            if(pos != suffix){
                int one = MAX(pos - suffix, 1);
                int two = MAX(pos - suffix, 2);
                assert(!strncmp(suffix, "LL", two) || !strncmp(suffix, "ll", two) ||
                               !strncmp(suffix, "L", one) || !strncmp(suffix, "l", one));
                type = long_type;
            }

            if(num > 2147483647 || num < -2147483647){
                type = long_type;
            }

            Token *newTk = newToken(TK_NUM, posBeforeEat, pos - posBeforeEat);
            newTk->val = num;
            newTk->type = type;
            cur->next = newTk;
            cur = newTk;
        }
        else if(isIdStartChar(*pos)){
            posBeforeEat = pos;
            pos = expectIdentifier(pos);
            Token *newTK = NULL;
            int len = pos - posBeforeEat;
            newTK = newToken(TK_IDENTIFIER, posBeforeEat, len);

            if(expandMacro){
                // #define return 0
                // return; -> return 0;
                Token *savedTk = newTK;
                while(newTK){
                    if(newTK->kind != TK_IDENTIFIER){
                        newTK = newTK->next;
                        continue;
                    }

                    bool replace = false;
                    pos = eatSpace(pos);
                    if(newTK->next && newTK->next->kind == TK_L_PAREN ||
                            !newTK->next && *pos == '('){
                        Macro *mac = findMacro(MACRO_WITH_PARAMS, newTK->pos, newTK->len);
                        if(mac){
                            replace = true;
                            bool findDelimiter = false;
                            Token *tmpTk = newTK;
                            while(tmpTk){
                                if(tmpTk->kind == TK_R_PAREN){
                                    findDelimiter = true;
                                    break;
                                }
                                tmpTk = tmpTk->next;
                            }

                            if(!findDelimiter){
                                char *delimiter = searchDelimiter(pos, end, ')');
                                assert(delimiter);
                                tmpTk = moveToTail(newTK);
                                tmpTk->next = tokenize(pos, delimiter + 1, false, false);
                                pos = delimiter + 1;
                            }

                            tmpTk = newTK;
                            tmpTk = tmpTk->next;
                            tmpTk = expectTokenStatic(tmpTk, TK_L_PAREN);

                            int argc = mac->argc;
                            if(argc == 0){
                                tmpTk = expectTokenStatic(tmpTk, TK_R_PAREN);
                                Token *rest = tmpTk->next;
                                tmpTk = dupTokenList(mac->toReplace);
                                *newTK = *tmpTk;
                                tmpTk = moveToTail(tmpTk);
                                tmpTk->next = rest;
                            }
                            else{
                                Token **tokenVec = calloc(argc, sizeof(Token));
                                for(int index = 0; index < argc; ++index){
                                    assert(tmpTk->kind != TK_COMMA &&
                                        tmpTk->kind != TK_R_PAREN);
                                    Token *tmptmpToken = tmpTk;
                                    if(index != argc - 1){
                                        while (tmptmpToken->next->kind != TK_COMMA){
                                            assert(tmptmpToken->next->kind != TK_R_PAREN);
                                            tmptmpToken = tmptmpToken->next;
                                        }
                                        Token *anoTok = tmptmpToken->next;
                                        tmptmpToken->next = NULL;
                                        tokenVec[index] = tmpTk;
                                        tmpTk = anoTok;
                                        tmpTk = expectTokenStatic(tmpTk, TK_COMMA);
                                    }
                                    else{
                                        while (tmptmpToken->next->kind != TK_R_PAREN){
                                            assert(tmptmpToken->next->kind != TK_COMMA);
                                            tmptmpToken = tmptmpToken->next;
                                        }
                                        Token *anoTok = tmptmpToken->next;
                                        tmptmpToken->next = NULL;
                                        tokenVec[index] = tmpTk;
                                        tmpTk = anoTok;
                                        tmpTk = expectTokenStatic(tmpTk, TK_R_PAREN);
                                    }
                                }

                                *newTK = *replaceMacro(mac, tokenVec);
                                moveToTail(newTK)->next = tmpTk;
                            }
                        }
                    }
                    else {
                        Macro *mac = findMacro(MACRO_WITHOUT_PARAMS, newTK->pos, newTK->len);
                        if(mac){
                            replace = true;
                            Token *next = newTK->next;
                            *newTK = *dupTokenList(mac->toReplace);
                            moveToTail(newTK)->next = next;
                        }
                    }

                    if(!replace){
                        newTK = newTK->next;
                    }
                }
                newTK = savedTk;
            }

            cur->next = newTK;
            if(expandMacro){
                while(newTK){
                    if(newTK->kind == TK_IDENTIFIER){
                        int ret = findReserved(newTK->pos, newTK->len);
                        if(ret >= 0){
                            newTK->kind =  reservedToken[ret];
                        }
                    }
                    newTK = newTK->next;
                }
            }
            cur = moveToTail(cur);
        }
        else if(isCPunct(*pos)){
            Token *newTK;
            switch(*pos){
                case '[':
                    newTK = newToken(TK_L_BRACKET, pos, 1);
                    ++pos;
                    break;
                case ']':
                    newTK = newToken(TK_R_BRACKET, pos, 1);
                    ++pos;
                    break;
                case '(':
                    newTK = newToken(TK_L_PAREN, pos, 1);
                    ++pos;
                    break;
                case ')':
                    newTK = newToken(TK_R_PAREN, pos, 1);
                    ++pos;
                    break;
                case '.':
                    if(!strncmp(pos, "...", 3)){
                        newTK = newToken(TK_ELLIPSIS, pos, 3);
                        pos += 3;
                    }
                    else{
                        newTK = newToken(TK_DOT, pos, 1);
                        ++pos;
                    }
                    break;
                case '-':
                    // -> --  -= -
                    if(!strncmp(pos, "->", 2)){
                        newTK = newToken(TK_MEMBER, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, "--", 2)){
                        newTK = newToken(TK_DEC, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, "-=", 2)){
                        newTK = newToken(TK_SUB_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_SUB, pos, 1);
                        pos += 1;
                    }
                    break;
                case '~':
                    newTK = newToken(TK_BIT_NOT, pos, 1);
                    ++pos;
                    break;
                case '+':
                    // ++ += +
                    if(!strncmp(pos, "++", 2)){
                        newTK = newToken(TK_INC, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, "+=", 2)){
                        newTK = newToken(TK_ADD_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_ADD, pos, 1);
                        pos += 1;
                    }
                    break;
                case '*':
                    // *= *
                    if(!strncmp(pos, "*=", 2)){
                        newTK = newToken(TK_MUL_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_MUL_DEREF, pos, 1);
                        pos += 1;
                    }
                    break;
                case '|':
                    // || |= |
                    if(!strncmp(pos, "||", 2)){
                        newTK = newToken(TK_LOG_OR, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, "|=", 2)){
                        newTK = newToken(TK_BITOR_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_BIT_OR, pos, 1);
                        pos += 1;
                    }
                    break;
                case '&':
                    // &= && &
                    if(!strncmp(pos, "&&", 2)){
                        newTK = newToken(TK_LOG_AND, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, "&=", 2)){
                        newTK = newToken(TK_BITAND_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_BIT_AND, pos, 1);
                        pos += 1;
                    }
                    break;
                case '/':
                    if(!strncmp(pos, "/=", 2)){
                        newTK = newToken(TK_DIV_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_DIV, pos, 1);
                        pos += 1;
                    }
                    break;
                case '%':
                    if(!strncmp(pos, "%=", 2)){
                        newTK = newToken(TK_MOD_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_MOD, pos, 1);
                        pos += 1;
                    }
                    break;
                case '<':
                    // <<= << <= <
                    if(!strncmp(pos, "<<=", 3)){
                        newTK = newToken(TK_SHL_ASSIGN, pos, 3);
                        pos += 3;
                    }
                    else if(!strncmp(pos, "<=", 2)){
                        newTK = newToken(TK_LE, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, "<<", 2)){
                        newTK = newToken(TK_SHL, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_LT, pos, 1);
                        pos += 1;
                    }
                    break;
                case '>':
                    if(!strncmp(pos, ">>=", 3)){
                        newTK = newToken(TK_SHR_ASSIGN, pos, 3);
                        pos += 3;
                    }
                    else if(!strncmp(pos, ">=", 2)){
                        newTK = newToken(TK_GE, pos, 2);
                        pos += 2;
                    }
                    else if(!strncmp(pos, ">>", 2)){
                        newTK = newToken(TK_SHR, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_GT, pos, 1);
                        pos += 1;
                    }
                    break;
                case '=':
                    // == =
                    if(!strncmp(pos, "==", 2)){
                        newTK = newToken(TK_EQ, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_ASSIGN, pos, 1);
                        pos += 1;
                    }
                    break;
                case '!':
                    // != !
                    if(!strncmp(pos, "!=", 2)){
                        newTK = newToken(TK_NEQ, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_LOG_NOT, pos, 1);
                        pos += 1;
                    }
                    break;
                case '^':
                    // ^= ^
                    if(!strncmp(pos, "^=", 2)){
                        newTK = newToken(TK_BITXOR_ASSIGN, pos, 2);
                        pos += 2;
                    }
                    else{
                        newTK = newToken(TK_BIT_XOR, pos, 1);
                        pos += 1;
                    }
                    break;
                case '?':
                    newTK = newToken(TK_QUESTION, pos, 1);
                    ++pos; break;
                case ':':
                    newTK = newToken(TK_COLON, pos, 1);
                    ++pos; break;
                case ',':
                    newTK = newToken(TK_COMMA, pos, 1);
                    ++pos; break;
                case ';':
                    newTK = newToken(TK_SEMICOLON, pos, 1);
                    ++pos; break;
                case '{':
                    newTK = newToken(TK_L_BRACE, pos, 1);
                    ++pos; break;
                case '}':
                    newTK = newToken(TK_R_BRACE, pos, 1);
                    ++pos; break;
                default:
                    printf("%s(): %s\n", __FUNCTION__, "unknown punctuation");
                    exit(1);
            }
            cur->next = newTK;
            cur = newTK;
        }
        else{
            printf("%s(): %s\n", __FUNCTION__, "unknown character");
            exit(1);
        }

        assert(pos <= end);
    }

    if(addEof){
        cur->next = newToken(TK_EOF, pos, 0);
    }
    return head.next;
}
