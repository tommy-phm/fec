%{

// Adapted from https://github.com/bleibig/rust-grammar/blob/master/lexer.l
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
//
// The grammer rules was written by the The Rust Project Developers
// and has been modfy to work with my configuration
#include <stdio.h>
#include <ctype.h>
#include "token.h"
#include "tree.h"
#include "symtab.h"
#include "rustgram.tab.h"

static int num_hashes;
static int end_hashes;
static int saw_non_hash;
extern YYSTYPE yylval;

int pre;

void yyerror(const char *s) {
    fprintf(stderr, "\033[0;31m[Error]\033[0m Syntax error at line %d: %s\n", yylineno, s);
    exit(-1);
}

char *purge(char *str, int category){
    size_t len = strlen(str);
    char *modified_str = (char *)malloc(len - 1);
    if (modified_str == NULL) 
        return NULL;
    switch(category) {
        case LIT_CHAR:
        case LIT_STR:
            strncpy(modified_str, str + 1, len - 2);
            modified_str[len - 2] = '\0';
            break;
        case LIT_STR_RAW:
            strncpy(modified_str, str + 2, len - 4); 
            modified_str[len - 4] = '\0';
            break;
        case LIT_BYTE:
        case LIT_BYTE_STR:
            strncpy(modified_str, str + 2, len - 3);
            modified_str[len - 3] = '\0';
            break;
        case LIT_BYTE_STR_RAW:
            strncpy(modified_str, str + 4, len - 6);
            modified_str[len - 6] = '\0';
            break;
    }
    char *read_ptr = modified_str;
    char *write_ptr = modified_str;
    while (*read_ptr) {
        if (*read_ptr == '\\' && *(read_ptr + 1) != '\0') {
            switch (*(read_ptr + 1)) {
                case 'x':
                case 'n':
                case 'r':
                case 't':
                    read_ptr += 2; 
                    break;
                case '\\':
                    *write_ptr++ = '\\';
                    read_ptr += 2;
                    break;
                default:
                    *write_ptr++ = *read_ptr++; 
                    break;
            }
        } else {
            *write_ptr++ = *read_ptr++;
        }
    }
    *write_ptr = '\0';
    return modified_str;
}

int create_token(int category){
    pre = category;
    Token *t = (Token*)malloc(sizeof(Token));
    if (t == NULL) 
        exit(EXIT_FAILURE);
    insertTokenArray(&tokens, t);
    t->category = category;
    t->text = strdup(yytext);
    t->lineno = yylineno;
    t->filename = filename;
    t->val.sval = NULL;
    switch(category) {
        case FALSE:
            t->category = LIT_INTEGER;
            t->text = strdup("0");
            t->val.ival = 0;
            break;
        case TRUE:
            t->category = LIT_INTEGER;
            t->text = strdup("1");
            t->val.ival = 1;
            break;
        case LIT_INTEGER:
            t->val.ival = atoi(yytext);
            break;
        case LIT_FLOAT:
            t->val.dval = atof(yytext);
            break;
        case LIT_BYTE:
        case LIT_CHAR:
        case LIT_STR:
        case LIT_STR_RAW:
        case LIT_BYTE_STR:
        case LIT_BYTE_STR_RAW:
            t->val.sval = purge(yytext, category);
            break;
    }
    yylval.token = t;
    return t->category;
}

%}

%option stack
%option yylineno
%option noyywrap
%option noinput
%option nounput

%x str
%x rawstr
%x rawstr_esc_begin
%x rawstr_esc_body
%x rawstr_esc_end
%x byte
%x bytestr
%x rawbytestr
%x rawbytestr_nohash
%x pound
%x shebang_or_attr
%x ltorchar
%x linecomment
%x doc_line
%x blockcomment
%x doc_block
%x suffix

ident [a-zA-Z\x80-\xff_][a-zA-Z0-9\x80-\xff_]*

%%

<suffix>{ident}            { BEGIN(INITIAL); }
<suffix>(.|\n)  { yyless(0); BEGIN(INITIAL); }

[ \n\t\r]             { }

\xef\xbb\xbf {
// UTF-8 byte order mark (BOM), ignore if in line 1, error otherwise
if (yyget_lineno() != 1)
    return -1;
}

\/\/(\/|\!)           { BEGIN(doc_line); yymore(); }
<doc_line>\n          { BEGIN(INITIAL);
                        yyleng--;
                        yytext[yyleng] = 0;
                        return ((yytext[2] == '!') ? INNER_DOC_COMMENT : OUTER_DOC_COMMENT);
                    }
<doc_line>[^\n]*      { yymore(); }

\/\/|\/\/\/\/         { BEGIN(linecomment); }
<linecomment>\n       { BEGIN(INITIAL); }
<linecomment>[^\n]*   { }

\/\*(\*|\!)[^*]       { yy_push_state(INITIAL); yy_push_state(doc_block); yymore(); }
<doc_block>\/\*       { yy_push_state(doc_block); yymore(); }
<doc_block>\*\/       {
    yy_pop_state();
    if (yy_top_state() == doc_block) {
        yymore();
    } else {
        return ((yytext[2] == '!') ? INNER_DOC_COMMENT : OUTER_DOC_COMMENT);
    }
}
<doc_block>(.|\n)     { yymore(); }

\/\*                  { yy_push_state(blockcomment); }
<blockcomment>\/\*    { yy_push_state(blockcomment); }
<blockcomment>\*\/    { yy_pop_state(); }
<blockcomment>(.|\n)   { }

_        { return UNDERSCORE; }
abstract { return ABSTRACT; }
alignof  { return ALIGNOF; }
as       { return AS; }
become   { return BECOME; }
box      { return BOX; }
break    { return BREAK; }
catch    { return CATCH; }
const    { return CONST; }
continue { return CONTINUE; }
crate    { return CRATE; }
default  { return DEFAULT; }
do       { return DO; }
else     { return ELSE; }
enum     { return ENUM; }
extern   { return EXTERN; }
false    { return create_token(FALSE); }
final    { return FINAL; }
fn       { return FN; }
for      { return FOR; }
if       { return IF; }
impl     { return IMPL; }
in       { return IN; }
let      { return LET; }
loop     { return LOOP; }
macro    { return MACRO; }
match    { return MATCH; }
mod      { return MOD; }
move     { return MOVE; }
mut      { return MUT; }
offsetof { return OFFSETOF; }
override { return OVERRIDE; }
priv     { return PRIV; }
proc     { return PROC; }
pure     { return PURE; }
pub      { return PUB; }
ref      { return REF; }
return   { return RETURN; }
self     { return SELF; }
sizeof   { return SIZEOF; }
static   { return STATIC; }
struct   { return STRUCT; }
super    { return SUPER; }
trait    { return TRAIT; }
true     { return create_token(TRUE); }
type     { return TYPE; }
typeof   { return TYPEOF; }
union    { return UNION; }
unsafe   { return UNSAFE; }
unsized  { return UNSIZED; }
use      { return USE; }
virtual  { return VIRTUAL; }
where    { return WHERE; }
while    { return WHILE; }
yield    { return YIELD; }

{ident}  { return create_token(IDENT); }

0x[0-9a-fA-F_]+                                    { BEGIN(suffix); return create_token(LIT_INTEGER); }
0o[0-7_]+                                          { BEGIN(suffix); return create_token(LIT_INTEGER); }
0b[01_]+                                           { BEGIN(suffix); return create_token(LIT_INTEGER); }
[0-9][0-9_]*                                       { BEGIN(suffix); return create_token(LIT_INTEGER); }
[0-9][0-9_]*\.(\.|[a-zA-Z])    { yyless(yyleng - 2); BEGIN(suffix); return create_token(LIT_INTEGER); }

[0-9][0-9_]*\.[0-9_]*([eE][-\+]?[0-9_]+)?          { BEGIN(suffix); return create_token(LIT_FLOAT); }
[0-9][0-9_]*(\.[0-9_]*)?[eE][-\+]?[0-9_]+          { BEGIN(suffix); return create_token(LIT_FLOAT); }

;      { return create_token(';'); }
,      { return create_token(','); }
\.\.\. { return DOTDOTDOT; }
\.\.   { return DOTDOT; }
\.     { return create_token('.'); }
\(     { return create_token('('); }
\)     { return create_token(')');  }
\{     { return create_token('{'); }
\}     { return create_token('}'); }
\[     { return create_token('['); }
\]     { return create_token(']'); }
@      { return create_token('@'); }
#      { BEGIN(pound); yymore(); }
<pound>\! { BEGIN(shebang_or_attr); yymore(); }
<shebang_or_attr>\[ {
BEGIN(INITIAL);
yyless(2);
return SHEBANG;
}
<shebang_or_attr>[^\[\n]*\n {
yyless(yyleng - 1);
if (yyget_lineno() == 1) {
    BEGIN(INITIAL);
    return create_token(SHEBANG_LINE);
} else {
    BEGIN(INITIAL);
    yyless(2);
    return SHEBANG;
}
}
<pound>. { BEGIN(INITIAL); yyless(1); return create_token('#'); }

\~     { return create_token('~'); }
::     { return create_token(MOD_SEP); }
:      { return create_token(':'); }
\$     { return create_token('$'); }
\?     { return create_token('?'); }

==    { return create_token(EQEQ); }
=>    { return create_token(FAT_ARROW); }
=     { return create_token('='); }
\!=   { return create_token(NE); }
\!    { if(pre != IDENT)
            return create_token('!'); 
        else
            pre = '!';
    }
\<=   { return create_token(LE); }
\<\<  { return create_token(SHL); }
\<\<= { return create_token(SHLEQ); }
\<    { return create_token('<'); }
\>=   { return create_token(GE); }
\>\>  { return create_token(SHR); }
\>\>= { return create_token(SHREQ); }
\>    { return create_token('>'); }

\x27                                      { BEGIN(ltorchar); yymore(); }
<ltorchar>static                          { BEGIN(INITIAL); return create_token(STATIC_LIFETIME); }
<ltorchar>{ident}                         { BEGIN(INITIAL); return create_token(LIFETIME); }
<ltorchar>\\[nrt\\\x27\x220]\x27          { BEGIN(suffix); return create_token(LIT_CHAR); }
<ltorchar>\\x[0-9a-fA-F]{2}\x27           { BEGIN(suffix); return create_token(LIT_CHAR); }
<ltorchar>\\u\{([0-9a-fA-F]_*){1,6}\}\x27 { BEGIN(suffix); return create_token(LIT_CHAR); }
<ltorchar>.\x27                           { BEGIN(suffix); return create_token(LIT_CHAR); }
<ltorchar>[\x80-\xff]{2,4}\x27            { BEGIN(suffix); return create_token(LIT_CHAR); }
<ltorchar><<EOF>>                         { BEGIN(INITIAL); return -1; }

b\x22              { BEGIN(bytestr); yymore(); }
<bytestr>\x22      { BEGIN(suffix); return LIT_BYTE_STR; }

<bytestr><<EOF>>                     { return -1; }
<bytestr>\\[n\nrt\\\x27\x220]        { yymore(); }
<bytestr>\\x[0-9a-fA-F]{2}           { yymore(); }
<bytestr>\\u\{([0-9a-fA-F]_*){1,6}\} { yymore(); }
<bytestr>\\[^n\nrt\\\x27\x220]       { return -1; }
<bytestr>(.|\n)                      { yymore(); }

br\x22                      { BEGIN(rawbytestr_nohash); yymore(); }
<rawbytestr_nohash>\x22     { BEGIN(suffix); return LIT_BYTE_STR_RAW; }
<rawbytestr_nohash>(.|\n)   { yymore(); }
<rawbytestr_nohash><<EOF>>  { return -1; }

br/# {
    BEGIN(rawbytestr);
    yymore();
    num_hashes = 0;
    saw_non_hash = 0;
    end_hashes = 0;
}
<rawbytestr># {
    if (!saw_non_hash) {
        num_hashes++;
    } else if (end_hashes != 0) {
        end_hashes++;
        if (end_hashes == num_hashes) {
            BEGIN(INITIAL);
            return create_token(LIT_BYTE_STR_RAW);
        }
    }
    yymore();
}
<rawbytestr>\x22# {
    end_hashes = 1;
    if (end_hashes == num_hashes) {
        BEGIN(INITIAL);
        return create_token(LIT_BYTE_STR_RAW);
    }
    yymore();
}
<rawbytestr>(.|\n) {
    if (!saw_non_hash) {
        saw_non_hash = 1;
    }
    if (end_hashes != 0) {
        end_hashes = 0;
    }
    yymore();
}
<rawbytestr><<EOF>> { return -1; }

b\x27                           { BEGIN(byte); yymore(); }
<byte>\\[nrt\\\x27\x220]\x27    { BEGIN(INITIAL); return create_token(LIT_BYTE); }
<byte>\\x[0-9a-fA-F]{2}\x27     { BEGIN(INITIAL); return create_token(LIT_BYTE); }
<byte>\\u([0-9a-fA-F]_*){4}\x27 { BEGIN(INITIAL); return create_token(LIT_BYTE); }
<byte>\\U([0-9a-fA-F]_*){8}\x27 { BEGIN(INITIAL); return create_token(LIT_BYTE); }
<byte>.\x27                     { BEGIN(INITIAL); return create_token(LIT_BYTE); }
<byte><<EOF>>                   { BEGIN(INITIAL); return -1; }

r\x22           { BEGIN(rawstr); yymore(); }
<rawstr>\x22    { BEGIN(suffix); return create_token(LIT_STR_RAW); }
<rawstr>(.|\n)  { yymore(); }
<rawstr><<EOF>> { return -1; }

r/#             {
    BEGIN(rawstr_esc_begin);
    yymore();
    num_hashes = 0;
    saw_non_hash = 0;
    end_hashes = 0;
}

<rawstr_esc_begin># {
    num_hashes++;
    yymore();
}
<rawstr_esc_begin>\x22 {
    BEGIN(rawstr_esc_body);
    yymore();
}
<rawstr_esc_begin>(.|\n) { return -1; }

<rawstr_esc_body>\x22/# {
BEGIN(rawstr_esc_end);
yymore();
}
<rawstr_esc_body>(.|\n) {
yymore();
}

<rawstr_esc_end># {
end_hashes++;
if (end_hashes == num_hashes) {
    BEGIN(INITIAL);
    return create_token(LIT_STR_RAW);
}
yymore();
}
<rawstr_esc_end>[^#] {
end_hashes = 0;
BEGIN(rawstr_esc_body);
yymore();
}

<rawstr_esc_begin,rawstr_esc_body,rawstr_esc_end><<EOF>> { return -1; }

\x22                     { BEGIN(str); yymore(); }
<str>\x22                { BEGIN(suffix); return create_token(LIT_STR); }

<str><<EOF>>                     { return -1; }
<str>\\[n\nr\rt\\\x27\x220]      { yymore(); }
<str>\\x[0-9a-fA-F]{2}           { yymore(); }
<str>\\u\{([0-9a-fA-F]_*){1,6}\} { yymore(); }
<str>\\[^n\nrt\\\x27\x220]       { return -1; }
<str>(.|\n)                      { yymore(); }

\<-  { return LARROW; }
-\>  { return RARROW; }
-    { return '-'; }
-=   { return MINUSEQ; }
&&   { return ANDAND; }
&    { return '&'; }
&=   { return ANDEQ; }
\|\| { return OROR; }
\|   { return '|'; }
\|=  { return OREQ; }
\+   { return '+'; }
\+=  { return PLUSEQ; }
\*   { return '*'; }
\*=  { return STAREQ; }
\/   { return '/'; }
\/=  { return SLASHEQ; }
\^   { return '^'; }
\^=  { return CARETEQ; }
%    { return '%'; }
%=   { return PERCENTEQ; }

<<EOF>> { return 0; }

%%