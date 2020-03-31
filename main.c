#include "gemcc.h"

char *user_input;
char *filename;
Token *token;

int main() {
    static_assert(sizeof(long) == 8, "");
    filename = "../test.txt";
    if(1){
        user_input = readFile();
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        Program *program = parse(token);

        VarList *globals = program->globals;
        VarList *funcs = program->functions;

//        assert(checkVar(globals->var, STATIC, "var2"));
//        globals = globals->next;
//        assert(checkVar(globals->var, STATIC, "var3"));
//        globals = globals->next;
//        assert(checkVar(globals->var, STATIC, ".L.data.0"));
//        globals = globals->next;
//        assert(checkVar(globals->var, UNSPECIFIED, "p"));
//        globals = globals->next;
//        assert(checkVar(globals->var, UNSPECIFIED, "var5"));
//        globals = globals->next;
//        assert(checkVar(globals->var, UNSPECIFIED, "var6"));
//        globals = globals->next;
//        assert(checkVar(globals->var, STATIC, ".L.data.1"));
//        globals = globals->next;
//        assert(checkVar(globals->var, UNSPECIFIED, "var7"));
//        globals = globals->next;
//        assert(checkVar(globals->var, UNSPECIFIED, "result"));
//        globals = globals->next;
//        assert(checkVar(funcs->var, EXTERN, "add"));
//        funcs = funcs->next;
//        assert(checkVar(globals->var, STATIC, ".L.data.2"));
//        globals = globals->next;
//        assert(checkVar(globals->var, STATIC, ".L.data.3"));
//        globals = globals->next;
//        assert(!globals);
//
//
//        assert(checkVar(funcs->var, EXTERN, "main"));
//        funcs = funcs->next;
//        assert(!funcs);

        codegen(program);


//        for(VarList *functions = program->functions; functions; functions = functions->next){
//            int offset = functions->var->type->hasVarArgs ? 56 : 0;
//            for(VarList *vl = functions->var->locals; vl; vl = vl->next){
//                offset += vl->var->type->size;
//                offset = alignTo(offset, vl->var->type->align);
//                vl->var->offset = offset;
//            }
//            functions->var->stackSize = alignTo(offset, 8);
//        }
//        codegen(program);
    } else {
        filename = "testcase 1";
        user_input = "// nice";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(consumeTk(TK_EOF));

        filename = "testcase 2";
        user_input = "/* hhh * '' \n * / */";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(consumeTk(TK_EOF));

        filename = "testcase 3";
        user_input = "#define A 123\n"
                     "A";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(token->val == 123);
        assert(consumeTk(TK_NUM));
        assert(consumeTk(TK_EOF));

        filename = "testcase 4";
        user_input = "#define ADD(x, y) ((x) + (y))\n"
                     "ADD(x, y)";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(consumeTk(TK_L_PAREN));
        assert(consumeTk(TK_L_PAREN));
        assert(consumeTk(TK_IDENTIFIER));
        assert(consumeTk(TK_R_PAREN));
        assert(consumeTk(TK_ADD));
        assert(consumeTk(TK_L_PAREN));
        assert(consumeTk(TK_IDENTIFIER));
        assert(consumeToken(TK_R_PAREN));
        assert(consumeToken(TK_R_PAREN));
        assert(consumeToken(TK_EOF));

        filename = "testcase 6";
        user_input = "\"literal string\"";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(consumeToken(TK_STR));
        assert(consumeToken(TK_EOF));

        filename = "testcase 7";
        user_input = "'\\n'";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(token->val == '\n');
        assert(consumeToken(TK_NUM));
        assert(consumeToken(TK_EOF));

        filename = "testcase 8";
        user_input = "#include \"../header.h\"\n"
                     "A";
        char *savedFilename = filename;
        char *savedInput = user_input;
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(consumeToken(TK_STR));
        assert(filename == savedFilename);
        assert(user_input == savedInput);
        assert(consumeToken(TK_EOF));

        filename = "testcase 9";
        user_input = "+";
        token = tokenize(user_input, user_input + PROG_SIZE, true, true);
        assert(consumeToken(TK_ADD));
        assert(consumeToken(TK_EOF));
    }

    return 0;
}
