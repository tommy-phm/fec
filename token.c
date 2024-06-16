#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "rustgram.tab.h"

// Return name of token with id number
const char* tokenName(int token) {
    switch (token) {
        case YYEMPTY: return "YYEMPTY";
        case YYEOF: return "YYEOF";
        case YYerror: return "YYerror";
        case YYUNDEF: return "YYUNDEF";
        case SHL: return "SHL";
        case SHR: return "SHR";
        case LE: return "LE";
        case EQEQ: return "EQEQ";
        case NE: return "NE";
        case GE: return "GE";
        case ANDAND: return "ANDAND";
        case OROR: return "OROR";
        case SHLEQ: return "SHLEQ";
        case SHREQ: return "SHREQ";
        case MINUSEQ: return "MINUSEQ";
        case ANDEQ: return "ANDEQ";
        case OREQ: return "OREQ";
        case PLUSEQ: return "PLUSEQ";
        case STAREQ: return "STAREQ";
        case SLASHEQ: return "SLASHEQ";
        case CARETEQ: return "CARETEQ";
        case PERCENTEQ: return "PERCENTEQ";
        case DOTDOT: return "DOTDOT";
        case DOTDOTDOT: return "DOTDOTDOT";
        case MOD_SEP: return "MOD_SEP";
        case RARROW: return "RARROW";
        case LARROW: return "LARROW";
        case FAT_ARROW: return "FAT_ARROW";
        case LIT_BYTE: return "LIT_BYTE";
        case LIT_CHAR: return "LIT_CHAR";
        case LIT_INTEGER: return "LIT_INTEGER";
        case LIT_FLOAT: return "LIT_FLOAT";
        case LIT_STR: return "LIT_STR";
        case LIT_STR_RAW: return "LIT_STR_RAW";
        case LIT_BYTE_STR: return "LIT_BYTE_STR";
        case LIT_BYTE_STR_RAW: return "LIT_BYTE_STR_RAW";
        case IDENT: return "IDENT";
        case UNDERSCORE: return "UNDERSCORE";
        case LIFETIME: return "LIFETIME";
        case SHEBANG_LINE: return "SHEBANG_LINE";
        case SELF: return "SELF";
        case STATIC: return "STATIC";
        case ABSTRACT: return "ABSTRACT";
        case ALIGNOF: return "ALIGNOF";
        case AS: return "AS";
        case BECOME: return "BECOME";
        case BREAK: return "BREAK";
        case CATCH: return "CATCH";
        case CRATE: return "CRATE";
        case DO: return "DO";
        case ELSE: return "ELSE";
        case ENUM: return "ENUM";
        case EXTERN: return "EXTERN";
        case FALSE: return "FALSE";
        case FINAL: return "FINAL";
        case FN: return "FN";
        case FOR: return "FOR";
        case IF: return "IF";
        case IMPL: return "IMPL";
        case IN: return "IN";
        case LET: return "LET";
        case LOOP: return "LOOP";
        case MACRO: return "MACRO";
        case MATCH: return "MATCH";
        case MOD: return "MOD";
        case MOVE: return "MOVE";
        case MUT: return "MUT";
        case OFFSETOF: return "OFFSETOF";
        case OVERRIDE: return "OVERRIDE";
        case PRIV: return "PRIV";
        case PUB: return "PUB";
        case PURE: return "PURE";
        case REF: return "REF";
        case RETURN: return "RETURN";
        case SIZEOF: return "SIZEOF";
        case STRUCT: return "STRUCT";
        case SUPER: return "SUPER";
        case UNION: return "UNION";
        case UNSIZED: return "UNSIZED";
        case TRUE: return "TRUE";
        case TRAIT: return "TRAIT";
        case TYPE: return "TYPE";
        case UNSAFE: return "UNSAFE";
        case VIRTUAL: return "VIRTUAL";
        case YIELD: return "YIELD";
        case DEFAULT: return "DEFAULT";
        case USE: return "USE";
        case WHILE: return "WHILE";
        case CONTINUE: return "CONTINUE";
        case PROC: return "PROC";
        case BOX: return "BOX";
        case CONST: return "CONST";
        case WHERE: return "WHERE";
        case TYPEOF: return "TYPEOF";
        case INNER_DOC_COMMENT: return "INNER_DOC_COMMENT";
        case OUTER_DOC_COMMENT: return "OUTER_DOC_COMMENT";
        case SHEBANG: return "SHEBANG";
        case STATIC_LIFETIME: return "STATIC_LIFETIME";
        case LAMBDA: return "LAMBDA";
        case SHIFTPLUS: return "SHIFTPLUS";
        case FORTYPE: return "FORTYPE";
        case RANGE: return "RANGE";
        default: return "UNKOWN";
    }
}

// Initialize token array
void initTokenArray(TokenArray *a) {
    a->array = (Token **)malloc(sizeof(Token *));
    if (a->array == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    a->used = 0;
    a->size = 1;
}

// Add token to array
void insertTokenArray(TokenArray *a, Token *token) {
    if (a->used == a->size) {
        a->size *= 2;
        a->array = (Token **)realloc(a->array, a->size * sizeof(Token *));
        if (a->array == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }
    }
    a->array[a->used++] = token;
}

// Clear token array
void freeTokenArray(TokenArray *a) {
    for (size_t i = 0; i < a->used; ++i) {
        if((a->array[i]) != NULL)
            freeToken(a->array[i]);
            a->array[i] = NULL;
    }
    free(a->array);
    a->array = NULL;
    a->used = a->size = 0;
}

// Clear token
void freeToken(Token *token) {
    if (token != NULL) {
        if (token->text != NULL) 
            free(token->text);

        switch(token->category) {
            case LIT_BYTE:
            case LIT_CHAR:
            case LIT_STR:
            case LIT_STR_RAW:
            case LIT_BYTE_STR:
            case LIT_BYTE_STR_RAW:
                if(token->val.sval != NULL)
                    free(token->val.sval);    
        }
        free(token);
    }
}