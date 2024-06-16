#ifndef TOKEN_H
#define TOKEN_H
#include <stddef.h>

extern char *filename;

typedef union {
  int ival;    /* for integer constants, store binary value here */
  double dval; /* for real constants, store binary value here */
  char *sval;  /* for string constants, malloc space, de-escape, store the string (less quotes and after escapes) here */
} val;

typedef struct {
  int category;   /* the integer code returned by yylex */
  char *text;     /* the actual string (lexeme) matched */
  int lineno;     /* the line number on which the token occurs */
  char *filename; /* the source file in which the token occurs */
  val val;    
} Token;

typedef struct {
  Token **array;
  size_t used;
  size_t size;
} TokenArray;

extern TokenArray tokens;

const char* tokenName(int token);

void initTokenArray(TokenArray *a);
void insertTokenArray(TokenArray *a, Token *token);
void freeTokenArray(TokenArray *a);
void freeToken(Token *token);

#endif