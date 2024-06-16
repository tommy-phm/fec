%{
// Adapted from https://github.com/bleibig/rust-grammar/blob/master/parser-lalr.y
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.



#define YYERROR_VERBOSE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "symtab.h"
enum yysymbol_kind_t;
int yylex();
void yyerror(const char *s);
%}

%union {
  Token *token;
  tree *tree;
}

%token <token>  '#' ';' ',' '.' '(' ')' '{' '}' '[' ']' '@' '~' ':' '$' '?' '=' '!' '<' '>' '-' '&' '|' '+' '*' '/' '^' '%'  
SHL SHR LE EQEQ NE GE ANDAND
OROR SHLEQ SHREQ MINUSEQ ANDEQ OREQ PLUSEQ STAREQ SLASHEQ CARETEQ PERCENTEQ
DOTDOT DOTDOTDOT MOD_SEP RARROW LARROW FAT_ARROW
LIT_BYTE LIT_CHAR LIT_INTEGER LIT_FLOAT  LIT_STR LIT_STR_RAW LIT_BYTE_STR LIT_BYTE_STR_RAW
IDENT UNDERSCORE LIFETIME SHEBANG_LINE
SELF STATIC ABSTRACT ALIGNOF AS BECOME BREAK CATCH CRATE DO ELSE ENUM EXTERN FALSE FINAL FN FOR IF IMPL IN LET LOOP MACRO 
MATCH MOD MOVE MUT OFFSETOF OVERRIDE PRIV PUB PURE REF RETURN SIZEOF STRUCT SUPER UNION UNSIZED TRUE TRAIT TYPE UNSAFE VIRTUAL 
YIELD DEFAULT USE WHILE CONTINUE PROC BOX CONST WHERE TYPEOF INNER_DOC_COMMENT OUTER_DOC_COMMENT SHEBANG STATIC_LIFETIME

%type <tree> crate maybe_shebang maybe_inner_attrs inner_attrs inner_attr maybe_outer_attrs outer_attrs outer_attr meta_item 
meta_seq maybe_mod_items mod_items attrs_and_vis mod_item item stmt_item item_static item_const item_macro view_item 
extern_fn_item use_item view_path block_item maybe_ty_ascription maybe_init_expr item_struct struct_decl_args struct_tuple_args 
struct_decl_fields struct_decl_field struct_tuple_fields struct_tuple_field item_enum enum_defs enum_def enum_args item_union 
item_mod item_foreign_mod maybe_abi maybe_foreign_items foreign_items foreign_item item_foreign_static item_foreign_fn 
fn_decl_allow_variadic fn_params_allow_variadic visibility idents_or_self ident_or_self item_type for_sized item_trait 
maybe_trait_items trait_items trait_item trait_const maybe_const_default trait_type maybe_unsafe maybe_default_maybe_unsafe 
trait_method type_method method impl_method item_impl maybe_impl_items impl_items impl_item maybe_default impl_const impl_type 
item_fn item_unsafe_fn fn_decl fn_decl_with_self fn_decl_with_self_allow_anon_params fn_params fn_anon_params fn_params_with_self 
fn_anon_params_with_self maybe_params params param inferrable_params inferrable_param maybe_comma_params maybe_comma_anon_params 
maybe_anon_params anon_params anon_param anon_params_allow_variadic_tail named_arg ret_ty generic_params maybe_where_clause 
where_clause where_predicates where_predicate maybe_for_lifetimes ty_params path_no_types_allowed path_generic_args_without_colons 
generic_args generic_values maybe_ty_sums_and_or_bindings maybe_bindings pat pats_or binding_mode lit_or_path pat_field pat_fields 
pat_struct pat_tup pat_tup_elts pat_vec pat_vec_elts ty ty_prim ty_bare_fn ty_fn_decl ty_closure for_in_type for_in_type_suffix 
maybe_mut maybe_mut_or_const ty_qualified_path_and_generic_values ty_qualified_path maybe_ty_sums ty_sums ty_sum ty_sum_elt 
ty_prim_sum ty_prim_sum_elt maybe_ty_param_bounds ty_param_bounds boundseq polybound bindings binding ty_param maybe_bounds bounds 
bound maybe_ltbounds ltbounds maybe_ty_default maybe_lifetimes lifetimes lifetime_and_bounds lifetime trait_ref inner_attrs_and_block 
block maybe_stmts stmts stmt maybe_exprs maybe_expr exprs path_expr path_generic_args_with_colons macro_expr nonblock_expr expr 
expr_nostruct nonblock_prefix_expr_nostruct nonblock_prefix_expr expr_qualified_path maybe_qpath_params maybe_as_trait_ref lambda_expr 
lambda_expr_no_first_bar lambda_expr_nostruct lambda_expr_nostruct_no_first_bar vec_expr struct_expr_fields maybe_field_inits 
field_inits field_init default_field_init block_expr full_block_expr block_expr_dot expr_match match_clauses match_clause 
nonblock_match_clause block_match_clause maybe_guard expr_if expr_if_let block_or_if expr_while expr_while_let expr_loop 
expr_for maybe_label let lit str maybe_ident ident unpaired_token token_trees token_tree delimited_token_trees 
parens_delimited_token_trees braces_delimited_token_trees brackets_delimited_token_trees

/*
  Quoting from the Bison manual:

  "Finally, the resolution of conflicts works by comparing the precedence
  of the rule being considered with that of the lookahead token. If the
  token's precedence is higher, the choice is to shift. If the rule's
  precedence is higher, the choice is to reduce. If they have equal
  precedence, the choice is made based on the associativity of that
  precedence level. The verbose output file made by ‘-v’ (see Invoking
  Bison) says how each conflict was resolved"
*/

// We expect no shift/reduce or reduce/reduce conflicts in this grammar;
// all potential ambiguities are scrutinized and eliminated manually.
%expect 0

// fake-precedence symbol to cause '|' bars in lambda context to parse
// at low precedence, permit things like |x| foo = bar, where '=' is
// otherwise lower-precedence than '|'. Also used for proc() to cause
// things like proc() a + b to parse as proc() { a + b }.
%precedence LAMBDA

%precedence SELF

// MUT should be lower precedence than IDENT so that in the pat rule,
// "& MUT pat" has higher precedence than "binding_mode ident [@ pat]"
%precedence MUT

// IDENT needs to be lower than '{' so that 'foo {' is shifted when
// trying to decide if we've got a struct-construction expr (esp. in
// contexts like 'if foo { .')
//
// IDENT also needs to be lower precedence than '<' so that '<' in
// 'foo:bar . <' is shifted (in a trait reference occurring in a
// bounds list), parsing as foo:(bar<baz>) rather than (foo:bar)<baz>.
%precedence IDENT
// Put the weak keywords that can be used as idents here as well
%precedence CATCH
%precedence DEFAULT
%precedence UNION

// A couple fake-precedence symbols to use in rules associated with +
// and < in trailing type contexts. These come up when you have a type
// in the RHS of operator-AS, such as "foo as bar<baz>". The "<" there
// has to be shifted so the parser keeps trying to parse a type, even
// though it might well consider reducing the type "bar" and then
// going on to "<" as a subsequent binop. The "+" case is with
// trailing type-bounds ("foo as bar:A+B"), for the same reason.
%precedence SHIFTPLUS

%precedence MOD_SEP
%precedence RARROW ':'

// In where clauses, "for" should have greater precedence when used as
// a higher ranked constraint than when used as the beginning of a
// for_in_type (which is a ty)
%precedence FORTYPE
%precedence FOR

// Binops & unops, and their precedences
%precedence '?'
%precedence BOX
%nonassoc DOTDOT

// RETURN needs to be lower-precedence than tokens that start
// prefix_exprs
%precedence RETURN YIELD

%right '=' SHLEQ SHREQ MINUSEQ ANDEQ OREQ PLUSEQ STAREQ SLASHEQ CARETEQ PERCENTEQ
%right LARROW
%left OROR
%left ANDAND
%left EQEQ NE
%left '<' '>' LE GE
%left '|'
%left '^'
%left '&'
%left SHL SHR
%left '+' '-'
%precedence AS
%left '*' '/' '%'
%precedence '!'

%precedence '{' '[' '(' '.'

%precedence RANGE

%start crate

%%

////////////////////////////////////////////////////////////////////////
// Part 1: Items and attributes
////////////////////////////////////////////////////////////////////////

crate
: maybe_shebang inner_attrs maybe_mod_items  { root = linkTree("crate", 2, $2, $3); }
| maybe_shebang maybe_mod_items  { root = linkTree("crate", 1, $2); }
;

maybe_shebang
: SHEBANG_LINE {}
| %empty       {}
;

maybe_inner_attrs
: inner_attrs              {}
| %empty                   { $$ = linkTree("maybe_inner_attrs", 0, NULL); }
;

inner_attrs
: inner_attr               { $$ = linkTree("InnerAttrs", 1, $1); }
| inner_attrs inner_attr   { $$ = graftTree($1, 1, $2); }
;

inner_attr
: SHEBANG '[' meta_item ']'   { $$ = linkTree("InnerAttr", 1, $3); }
| INNER_DOC_COMMENT           { $$ = linkTree("InnerAttr", 1, $1); }
;

maybe_outer_attrs
: outer_attrs
| %empty                   { $$ = linkTree("maybe_outer_attrs", 0, NULL); }
;

outer_attrs
: outer_attr               { $$ = linkTree("OuterAttrs", 1, $1); }
| outer_attrs outer_attr   { $$ = graftTree($1, 1, $2); }
;

outer_attr
: '#' '[' meta_item ']'    { $$ = $3; }
| OUTER_DOC_COMMENT        { $$ = linkTree("doc-comment", 1, $1); }
;

meta_item
: ident                      { $$ = linkTree("MetaWord", 1, $1); }
| ident '=' lit              { $$ = linkTree("MetaNameValue", 2, $1, $3); }
| ident '(' meta_seq ')'     { $$ = linkTree("MetaList", 2, $1, $3); }
| ident '(' meta_seq ',' ')' { $$ = linkTree("MetaList", 2, $1, $3); }
;

meta_seq
: %empty                   { $$ = linkTree("meta_seq", 0, NULL); }
| meta_item                { $$ = linkTree("MetaItems", 1, $1); }
| meta_seq ',' meta_item   { $$ = graftTree($1, 1, $3); }
;

maybe_mod_items
: mod_items
| %empty             { $$ = linkTree("maybe_mod_items", 0, NULL); }
;

mod_items
: mod_item                               { $$ = linkTree("Items", 1, $1); }
| mod_items mod_item                     { $$ = graftTree($1, 1, $2); }
;

attrs_and_vis
: maybe_outer_attrs visibility           { $$ = linkTree("AttrsAndVis", 2, $1, $2); }
;

mod_item
: attrs_and_vis item    { $$ = linkTree("Item", 2, $1, $2); }
;

// items that can appear outside of a fn block
item
: stmt_item
| item_macro
;

// items that can appear in "stmts"
stmt_item
: item_static
| item_const
| item_type
| block_item
| view_item
;

item_static
: STATIC ident ':' ty '=' expr ';'  { $$ = linkTree("ItemStatic", 3, $2, $4, $6); }
| STATIC MUT ident ':' ty '=' expr ';'  { $$ = linkTree("ItemStatic", 3, $3, $5, $7); }
;

item_const
: CONST ident ':' ty '=' expr ';'  { $$ = linkTree("ItemConst", 3, $2, $4, $6); }
;

item_macro
: path_expr '!' maybe_ident parens_delimited_token_trees ';'  { $$ = linkTree("ItemMacro", 3, $1, $3, $4); }
| path_expr '!' maybe_ident braces_delimited_token_trees      { $$ = linkTree("ItemMacro", 3, $1, $3, $4); }
| path_expr '!' maybe_ident brackets_delimited_token_trees ';'{ $$ = linkTree("ItemMacro", 3, $1, $3, $4); }
;

view_item
: use_item
| extern_fn_item
| EXTERN CRATE ident ';'                      { $$ = linkTree("ViewItemExternCrate", 1, $3); }
| EXTERN CRATE ident AS ident ';'             { $$ = linkTree("ViewItemExternCrate", 2, $3, $5); }
;

extern_fn_item
: EXTERN maybe_abi item_fn                    { $$ = linkTree("ViewItemExternFn", 2, $2, $3); }
;

use_item
: USE view_path ';'                           { $$ = linkTree("ViewItemUse", 1, $2); }
;

view_path
: path_no_types_allowed                                    { $$ = linkTree("ViewPathSimple", 1, $1); }
| path_no_types_allowed MOD_SEP '{'                '}'     { $$ = linkTree("ViewPathList", 2, $1, linkTree("ViewPathListEmpty", 0, NULL)); }
|                       MOD_SEP '{'                '}'     { $$ = linkTree("ViewPathList", 1, linkTree("ViewPathListEmpty", 0, NULL)); }
| path_no_types_allowed MOD_SEP '{' idents_or_self '}'     { $$ = linkTree("ViewPathList", 2, $1, $4); }
|                       MOD_SEP '{' idents_or_self '}'     { $$ = linkTree("ViewPathList", 1, $3); }
| path_no_types_allowed MOD_SEP '{' idents_or_self ',' '}' { $$ = linkTree("ViewPathList", 2, $1, $4); }
|                       MOD_SEP '{' idents_or_self ',' '}' { $$ = linkTree("ViewPathList", 1, $3); }
| path_no_types_allowed MOD_SEP '*'                        { $$ = linkTree("ViewPathGlob", 1, $1); }
|                       MOD_SEP '*'                        { $$ = linkTree("ViewPathGlob", 0, NULL); }
|                               '*'                        { $$ = linkTree("ViewPathGlob", 0, NULL); }
|                               '{'                '}'     { $$ = linkTree("ViewPathListEmpty", 0, NULL); }
|                               '{' idents_or_self '}'     { $$ = linkTree("ViewPathList", 1, $2); }
|                               '{' idents_or_self ',' '}' { $$ = linkTree("ViewPathList", 1, $2); }
| path_no_types_allowed AS ident                           { $$ = linkTree("ViewPathSimple", 2, $1, $3); }
;

block_item
: item_fn
| item_unsafe_fn
| item_mod
| item_foreign_mod          { $$ = linkTree("ItemForeignMod", 1, $1); }
| item_struct
| item_enum
| item_union
| item_trait
| item_impl
;

maybe_ty_ascription
: ':' ty_sum { $$ = $2; }
| %empty { $$ = linkTree("maybe_ty_ascription", 0, NULL); }
;

maybe_init_expr
: '=' expr { $$ = $2; }
| %empty   { $$ = linkTree("maybe_init_expr", 0, NULL); }
;

// structs
item_struct
: STRUCT ident generic_params maybe_where_clause struct_decl_args
{
  $$ = linkTree("ItemStruct", 4, $2, $3, $4, $5);
}
| STRUCT ident generic_params struct_tuple_args maybe_where_clause ';'
{
  $$ = linkTree("ItemStruct", 4, $2, $3, $4, $5);
}
| STRUCT ident generic_params maybe_where_clause ';'
{
  $$ = linkTree("ItemStruct", 3, $2, $3, $4);
}
;

struct_decl_args
: '{' struct_decl_fields '}'                  { $$ = $2; }
| '{' struct_decl_fields ',' '}'              { $$ = $2; }
;

struct_tuple_args
: '(' struct_tuple_fields ')'                 { $$ = $2; }
| '(' struct_tuple_fields ',' ')'             { $$ = $2; }
;

struct_decl_fields
: struct_decl_field                           { $$ = linkTree("StructFields", 1, $1); }
| struct_decl_fields ',' struct_decl_field    { $$ = graftTree($1, 1, $3); }
| %empty                                      { $$ = linkTree("struct_decl_fields", 0, NULL); }
;

struct_decl_field
: attrs_and_vis ident ':' ty_sum              { $$ = linkTree("StructField", 3, $1, $2, $4); }
;

struct_tuple_fields
: struct_tuple_field                          { $$ = linkTree("StructFields", 1, $1); }
| struct_tuple_fields ',' struct_tuple_field  { $$ = graftTree($1, 1, $3); }
| %empty                                      { $$ = linkTree("struct_tuple_fields", 0, NULL); }
;

struct_tuple_field
: attrs_and_vis ty_sum                    { $$ = linkTree("StructField", 2, $1, $2); }
;

// enums
item_enum
: ENUM ident generic_params maybe_where_clause '{' enum_defs '}'     { $$ = linkTree("ItemEnum", 0); }
| ENUM ident generic_params maybe_where_clause '{' enum_defs ',' '}' { $$ = linkTree("ItemEnum", 0); }
;

enum_defs
: enum_def               { $$ = linkTree("EnumDefs", 1, $1); }
| enum_defs ',' enum_def { $$ = graftTree($1, 1, $3); }
| %empty                 { $$ = linkTree("EnumDefs", 0, NULL); }
;

enum_def
: attrs_and_vis ident enum_args { $$ = linkTree("EnumDef", 3, $1, $2, $3); }
;

enum_args
: '{' struct_decl_fields '}'     { $$ = linkTree("EnumArgs", 1, $2); }
| '{' struct_decl_fields ',' '}' { $$ = linkTree("EnumArgs", 1, $2); }
| '(' maybe_ty_sums ')'          { $$ = linkTree("EnumArgs", 1, $2); }
| '=' expr                       { $$ = linkTree("EnumArgs", 1, $2); }
| %empty                         { $$ = linkTree("EnumArgs", 0, NULL); }
;

// unions
item_union
: UNION ident generic_params maybe_where_clause '{' struct_decl_fields '}'     { $$ = linkTree("ItemUnion", 0); }
| UNION ident generic_params maybe_where_clause '{' struct_decl_fields ',' '}' { $$ = linkTree("ItemUnion", 0); }

item_mod
: MOD ident ';'                                 { $$ = linkTree("ItemMod", 1, $2); }
| MOD ident '{' maybe_mod_items '}'             { $$ = linkTree("ItemMod", 2, $2, $4); }
| MOD ident '{' inner_attrs maybe_mod_items '}' { $$ = linkTree("ItemMod", 3, $2, $4, $5); }
;

item_foreign_mod
: EXTERN maybe_abi '{' maybe_foreign_items '}'             { $$ = linkTree("ItemForeignMod", 1, $4); }
| EXTERN maybe_abi '{' inner_attrs maybe_foreign_items '}' { $$ = linkTree("ItemForeignMod", 2, $4, $5); }
;

maybe_abi
: str
| %empty { $$ = linkTree("maybe_abi", 0, NULL); }
;

maybe_foreign_items
: foreign_items
| %empty { $$ = linkTree("maybe_foreign_items", 0, NULL); }
;

foreign_items
: foreign_item               { $$ = linkTree("ForeignItems", 1, $1); }
| foreign_items foreign_item { $$ = graftTree($1, 1, $2); }
;

foreign_item
: attrs_and_vis STATIC item_foreign_static { $$ = linkTree("ForeignItem", 2, $1, $3); }
| attrs_and_vis item_foreign_fn            { $$ = linkTree("ForeignItem", 2, $1, $2); }
| attrs_and_vis UNSAFE item_foreign_fn     { $$ = linkTree("ForeignItem", 2, $1, $3); }
;

item_foreign_static
: maybe_mut ident ':' ty ';'               { $$ = linkTree("StaticItem", 3, $1, $2, $4); }
;

item_foreign_fn
: FN ident generic_params fn_decl_allow_variadic maybe_where_clause ';' { $$ = linkTree("ForeignFn", 4, $2, $3, $4, $5); }
;

fn_decl_allow_variadic
: fn_params_allow_variadic ret_ty { $$ = linkTree("FnDecl", 2, $1, $2); }
;

fn_params_allow_variadic
: '(' ')'                      { $$ = linkTree("fn_params_allow_variadic", 0, NULL); }
| '(' params ')'               { $$ = $2; }
| '(' params ',' ')'           { $$ = $2; }
| '(' params ',' DOTDOTDOT ')' { $$ = $2; }
;

visibility
: PUB      { $$ = linkTree("Public", 0, NULL); }
| %empty   { $$ = linkTree("Inherited", 0, NULL); }
;

idents_or_self
: ident_or_self                    { $$ = linkTree("IdentsOrSelf", 1, $1); }
| idents_or_self AS ident          { $$ = linkTree("IdentsOrSelf", 2, $1, $3); }
| idents_or_self ',' ident_or_self { $$ = graftTree($1, 1, $3); }
;

ident_or_self
: ident
| SELF  { $$ = linkTree("SELF", 0, $1); }
;

item_type
: TYPE ident generic_params maybe_where_clause '=' ty_sum ';'  { $$ = linkTree("ItemTy", 4, $2, $3, $4, $6); }
;

for_sized
: FOR '?' ident { $$ = linkTree("ForSized", 1, $3); }
| FOR ident '?' { $$ = linkTree("ForSized", 1, $2); }
| %empty        { $$ = linkTree("for_sized", 0, NULL); }
;

item_trait
: maybe_unsafe TRAIT ident generic_params for_sized maybe_ty_param_bounds maybe_where_clause '{' maybe_trait_items '}'
{
  $$ = linkTree("ItemTrait", 7, $1, $3, $4, $5, $6, $7, $9);
}
;

maybe_trait_items
: trait_items
| %empty { $$ = linkTree("maybe_trait_items", 0, NULL); }
;

trait_items
: trait_item               { $$ = linkTree("TraitItems", 1, $1); }
| trait_items trait_item   { $$ = graftTree($1, 1, $2); }
;

trait_item
: trait_const
| trait_type
| trait_method
| maybe_outer_attrs item_macro { $$ = linkTree("TraitMacroItem", 2, $1, $2); }
;

trait_const
: maybe_outer_attrs CONST ident maybe_ty_ascription maybe_const_default ';' { $$ = linkTree("ConstTraitItem", 4, $1, $3, $4, $5); }
;

maybe_const_default
: '=' expr { $$ = linkTree("ConstDefault", 1, $2); }
| %empty   { $$ = linkTree("maybe_const_default", 0, NULL); }
;

trait_type
: maybe_outer_attrs TYPE ty_param ';' { $$ = linkTree("TypeTraitItem", 2, $1, $3); }
;

maybe_unsafe
: UNSAFE { $$ = linkTree("Unsafe", 0, NULL); }
| %empty { $$ = linkTree("maybe_unsafe", 0, NULL); }
;

maybe_default_maybe_unsafe
: DEFAULT UNSAFE { $$ = linkTree ("DefaultUnsafe", 0, NULL); }
| DEFAULT        { $$ = linkTree ("Default", 0, NULL); }
|         UNSAFE { $$ = linkTree("Unsafe", 0, NULL); }
| %empty { $$ = linkTree("maybe_default_maybe_unsafe", 0, NULL); }

trait_method
: type_method { $$ = linkTree("Required", 1, $1); }
| method      { $$ = linkTree("Provided", 1, $1); }
;

type_method
: maybe_outer_attrs maybe_unsafe FN ident generic_params fn_decl_with_self_allow_anon_params maybe_where_clause ';'
{
  $$ = linkTree("TypeMethod", 6, $1, $2, $4, $5, $6, $7);
}
| maybe_outer_attrs CONST maybe_unsafe FN ident generic_params fn_decl_with_self_allow_anon_params maybe_where_clause ';'
{
  $$ = linkTree("TypeMethod", 6, $1, $3, $5, $6, $7, $8);
}
| maybe_outer_attrs maybe_unsafe EXTERN maybe_abi FN ident generic_params fn_decl_with_self_allow_anon_params maybe_where_clause ';'
{
  $$ = linkTree("TypeMethod", 7, $1, $2, $4, $6, $7, $8, $9);
}
;

method
: maybe_outer_attrs maybe_unsafe FN ident generic_params fn_decl_with_self_allow_anon_params maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("Method", 7, $1, $2, $4, $5, $6, $7, $8);
}
| maybe_outer_attrs CONST maybe_unsafe FN ident generic_params fn_decl_with_self_allow_anon_params maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("Method", 7, $1, $3, $5, $6, $7, $8, $9);
}
| maybe_outer_attrs maybe_unsafe EXTERN maybe_abi FN ident generic_params fn_decl_with_self_allow_anon_params maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("Method", 8, $1, $2, $4, $6, $7, $8, $9, $10);
}
;

impl_method
: attrs_and_vis maybe_default maybe_unsafe FN ident generic_params fn_decl_with_self maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("Method", 8, $1, $2, $3, $5, $6, $7, $8, $9);
}
| attrs_and_vis maybe_default CONST maybe_unsafe FN ident generic_params fn_decl_with_self maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("Method", 8, $1, $2, $4, $6, $7, $8, $9, $10);
}
| attrs_and_vis maybe_default maybe_unsafe EXTERN maybe_abi FN ident generic_params fn_decl_with_self maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("Method", 9, $1, $2, $3, $5, $7, $8, $9, $10, $11);
}
;

// There are two forms of impl:
//
// impl (<...>)? TY { ... }
// impl (<...>)? TRAIT for TY { ... }
//
// Unfortunately since TY can begin with '<' itself -- as part of a
// TyQualifiedPath type -- there's an s/r conflict when we see '<' after IMPL:
// should we reduce one of the early rules of TY (such as maybe_once)
// or shall we continue shifting into the generic_params list for the
// impl?
//
// The production parser disambiguates a different case here by
// permitting / requiring the user to provide parens around types when
// they are ambiguous with traits. We do the same here, regrettably,
// by splitting ty into ty and ty_prim.
item_impl
: maybe_default_maybe_unsafe IMPL generic_params ty_prim_sum maybe_where_clause '{' maybe_inner_attrs maybe_impl_items '}'
{
  $$ = linkTree("ItemImpl", 6, $1, $3, $4, $5, $7, $8);
}
| maybe_default_maybe_unsafe IMPL generic_params '(' ty ')' maybe_where_clause '{' maybe_inner_attrs maybe_impl_items '}'
{
  $$ = linkTree("ItemImpl", 6, $1, $3, 5, $6, $9, $10);
}
| maybe_default_maybe_unsafe IMPL generic_params trait_ref FOR ty_sum maybe_where_clause '{' maybe_inner_attrs maybe_impl_items '}'
{
  $$ = linkTree("ItemImpl", 6, $3, $4, $6, $7, $9, $10);
}
| maybe_default_maybe_unsafe IMPL generic_params '!' trait_ref FOR ty_sum maybe_where_clause '{' maybe_inner_attrs maybe_impl_items '}'
{
  $$ = linkTree("ItemImplNeg", 7, $1, $3, $5, $7, $8, $10, $11);
}
| maybe_default_maybe_unsafe IMPL generic_params trait_ref FOR DOTDOT '{' '}'
{
  $$ = linkTree("ItemImplDefault", 3, $1, $3, $4);
}
| maybe_default_maybe_unsafe IMPL generic_params '!' trait_ref FOR DOTDOT '{' '}'
{
  $$ = linkTree("ItemImplDefaultNeg", 3, $1, $3, $4);
}
;

maybe_impl_items
: impl_items
| %empty { $$ = linkTree("maybe_impl_items", 0, NULL); }
;

impl_items
: impl_item               { $$ = linkTree("ImplItems", 1, $1); }
| impl_item impl_items    { $$ = graftTree($1, 1, $2); }
;

impl_item
: impl_method
| attrs_and_vis item_macro { $$ = linkTree("ImplMacroItem", 2, $1, $2); }
| impl_const
| impl_type
;

maybe_default
: DEFAULT { $$ = linkTree("Default", 0, NULL); }
| %empty { $$ = linkTree("maybe_default", 0, NULL); }
;

impl_const
: attrs_and_vis maybe_default item_const { $$ = linkTree("ImplConst", 3, $1, $2, $3); }
;

impl_type
: attrs_and_vis maybe_default TYPE ident generic_params '=' ty_sum ';'  { $$ = linkTree("ImplType", 5, $1, $2, $4, $5, $7); }
;

item_fn
: FN ident generic_params fn_decl maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("ItemFn", 5, $2, $3, $4, $5, $6);
}
| CONST FN ident generic_params fn_decl maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("ItemFn", 5, $3, $4, $5, $6, $7);
}
;

item_unsafe_fn
: UNSAFE FN ident generic_params fn_decl maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("ItemUnsafeFn", 5, $3, $4, $5, $6, $7);
}
| CONST UNSAFE FN ident generic_params fn_decl maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("ItemUnsafeFn", 5, $4, $5, $6, $7, $8);
}
| UNSAFE EXTERN maybe_abi FN ident generic_params fn_decl maybe_where_clause inner_attrs_and_block
{
  $$ = linkTree("ItemUnsafeFn", 6, $3, $5, $6, $7, $8, $9);
}
;

fn_decl
: fn_params ret_ty   { $$ = linkTree("FnDecl", 2, $1, $2); }
;

fn_decl_with_self
: fn_params_with_self ret_ty   { $$ = linkTree("FnDecl", 2, $1, $2); }
;

fn_decl_with_self_allow_anon_params
: fn_anon_params_with_self ret_ty   { $$ = linkTree("FnDecl", 2, $1, $2); }
;

fn_params
: '(' maybe_params ')'  { $$ = $2; }
;

fn_anon_params
: '(' anon_param anon_params_allow_variadic_tail ')' { $$ = graftTree($2, 1, $3); }
| '(' ')'                                            { $$ = linkTree("fn_anon_params", 0, NULL); }
;

fn_params_with_self
: '(' maybe_mut SELF maybe_ty_ascription maybe_comma_params ')'              { $$ = linkTree("SelfValue", 3, $2, $4, $5); }
| '(' '&' maybe_mut SELF maybe_ty_ascription maybe_comma_params ')'          { $$ = linkTree("SelfRegion", 3, $3, $5, $6); }
| '(' '&' lifetime maybe_mut SELF maybe_ty_ascription maybe_comma_params ')' { $$ = linkTree("SelfRegion", 4, $3, $4, $6, $7); }
| '(' maybe_params ')'                                                       { $$ = linkTree("SelfStatic", 1, $2); }
;

fn_anon_params_with_self
: '(' maybe_mut SELF maybe_ty_ascription maybe_comma_anon_params ')'              { $$ = linkTree("SelfValue", 3, $2, $4, $5); }
| '(' '&' maybe_mut SELF maybe_ty_ascription maybe_comma_anon_params ')'          { $$ = linkTree("SelfRegion", 3, $3, $5, $6); }
| '(' '&' lifetime maybe_mut SELF maybe_ty_ascription maybe_comma_anon_params ')' { $$ = linkTree("SelfRegion", 4, $3, $4, $6, $7); }
| '(' maybe_anon_params ')'                                                       { $$ = linkTree("SelfStatic", 1, $2); }
;

maybe_params
: params
| params ','
| %empty  { $$ = linkTree("maybe_params", 0, NULL); }
;

params
: param                { $$ = linkTree("Args", 1, $1); }
| params ',' param     { $$ = graftTree($1, 1, $3); }
;

param
: pat ':' ty_sum   { $$ = linkTree("Arg", 2, $1, $3); }
;

inferrable_params
: inferrable_param                       { $$ = linkTree("InferrableParams", 1, $1); }
| inferrable_params ',' inferrable_param { $$ = graftTree($1, 1, $3); }
;

inferrable_param
: pat maybe_ty_ascription { $$ = linkTree("InferrableParam", 2, $1, $2); }
;

maybe_comma_params
: ','            { $$ = linkTree("maybe_comma_params", 0, NULL); }
| ',' params     { $$ = $2; }
| ',' params ',' { $$ = $2; }
| %empty         { $$ = linkTree("maybe_comma_params", 0, NULL); }
;

maybe_comma_anon_params
: ','                 { $$ = linkTree("maybe_comma_anon_params", 0, NULL); }
| ',' anon_params     { $$ = $2; }
| ',' anon_params ',' { $$ = $2; }
| %empty              { $$ = linkTree("maybe_comma_anon_params", 0, NULL); }
;

maybe_anon_params
: anon_params
| anon_params ','
| %empty      { $$ = linkTree("maybe_anon_params", 0, NULL); }
;

anon_params
: anon_param                 { $$ = linkTree("Args", 1, $1); }
| anon_params ',' anon_param { $$ = graftTree($1, 1, $3); }
;

// anon means it's allowed to be anonymous (type-only), but it can
// still have a name
anon_param
: named_arg ':' ty   { $$ = linkTree("Arg", 2, $1, $3); }
| ty
;

anon_params_allow_variadic_tail
: ',' DOTDOTDOT                                  { $$ = linkTree("anon_params_allow_variadic_tail", 0, NULL); }
| ',' anon_param anon_params_allow_variadic_tail { $$ = linkTree("Args", 2, $2, $3); }
| %empty                                         { $$ = linkTree("anon_params_allow_variadic_tail", 0, NULL); }
;

named_arg
: ident
| UNDERSCORE        { $$ = linkTree("PatWild", 0, NULL); }
| '&' ident         { $$ = $2; }
| '&' UNDERSCORE    { $$ = linkTree ("PatWild", 0, NULL); }
| ANDAND ident      { $$ = $2; }
| ANDAND UNDERSCORE { $$ = linkTree("PatWild", 0, NULL); }
| MUT ident         { $$ = $2; }
;

ret_ty
: RARROW '!'         { $$ = linkTree("ret_ty", 0, NULL); }
| RARROW ty          { $$ = linkTree("ret-ty", 1, $2); }
| %prec IDENT %empty { $$ = linkTree("ret_ty", 0, NULL); }
;

generic_params
: '<' '>'                             {  }
| '<' lifetimes '>'                   {  }
| '<' lifetimes ',' '>'               {  }
| '<' lifetimes SHR                   {  }
| '<' lifetimes ',' SHR               {  }
| '<' lifetimes ',' ty_params '>'     {  }
| '<' lifetimes ',' ty_params ',' '>' {  }
| '<' lifetimes ',' ty_params SHR     {  }
| '<' lifetimes ',' ty_params ',' SHR {  }
| '<' ty_params '>'                   {  }
| '<' ty_params ',' '>'               {  }
| '<' ty_params SHR                   {  }
| '<' ty_params ',' SHR               {  }
| %empty                              { $$ = linkTree("Generics", 0, NULL); }
;

maybe_where_clause
: %empty                              { $$ = linkTree("maybe_where_clause", 0, NULL); }
| where_clause
;

where_clause
: WHERE where_predicates              { $$ = linkTree("WhereClause", 1, $2); }
| WHERE where_predicates ','          { $$ = linkTree("WhereClause", 1, $2); }
;

where_predicates
: where_predicate                      { $$ = linkTree("WherePredicates", 1, $1); }
| where_predicates ',' where_predicate { $$ = graftTree($1, 1, $3); }
;

where_predicate
: maybe_for_lifetimes lifetime ':' bounds    { $$ = linkTree("WherePredicate", 3, $1, $2, $4); }
| maybe_for_lifetimes ty ':' ty_param_bounds { $$ = linkTree("WherePredicate", 3, $1, $2, $4); }
;

maybe_for_lifetimes
: FOR '<' lifetimes '>' { $$ = linkTree("maybe_for_lifetimes", 0, NULL); }
| %prec FORTYPE %empty  { $$ = linkTree("maybe_for_lifetimes", 0, NULL); }

ty_params
: ty_param               { $$ = linkTree("TyParams", 1, $1); }
| ty_params ',' ty_param { $$ = graftTree($1, 1, $3); }
;

// A path with no type parameters; e.g. `foo::bar::Baz`
//
// These show up in 'use' view-items, because these are processed
// without respect to types.
path_no_types_allowed
: ident                               { $$ = linkTree("ViewPath", 1, $1); }
| MOD_SEP ident                       { $$ = linkTree("ViewPath", 1, $2); }
| SELF                                { $$ = linkTree("ViewPath", 1, linkTree("Self", 0, NULL)); }
| MOD_SEP SELF                        { $$ = linkTree("ViewPath", 1, linkTree("Self", 0, NULL)); }
| SUPER                               { $$ = linkTree("ViewPath", 1, linkTree("Super", 0, NULL)); }
| MOD_SEP SUPER                       { $$ = linkTree("ViewPath", 1, linkTree("Super", 0, NULL)); }
| path_no_types_allowed MOD_SEP ident { $$ = graftTree($1, 1, $3); }
;

// A path with a lifetime and type parameters, with no double colons
// before the type parameters; e.g. `foo::bar<'a>::Baz<T>`
//
// These show up in "trait references", the components of
// type-parameter bounds lists, as well as in the prefix of the
// path_generic_args_and_bounds rule, which is the full form of a
// named typed expression.
//
// They do not have (nor need) an extra '::' before '<' because
// unlike in expr context, there are no "less-than" type exprs to
// be ambiguous with.
path_generic_args_without_colons
: %prec IDENT
  ident                                                                       { $$ = linkTree("components", 1, $1); }
| %prec IDENT
  ident generic_args                                                          { $$ = linkTree("components", 2, $1, $2); }
| %prec IDENT
  ident '(' maybe_ty_sums ')' ret_ty                                          { $$ = linkTree("components", 2, $1, $3); }
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident                              { $$ = graftTree($1, 1, $3); }
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident generic_args                 { $$ = graftTree($1, 2, $3, $4); }
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident '(' maybe_ty_sums ')' ret_ty { $$ = graftTree($1, 2, $3, $5); }
;

generic_args
: '<' generic_values '>'   { $$ = $2; }
| '<' generic_values SHR   { $$ = $2; }
| '<' generic_values GE    { $$ = $2; }
| '<' generic_values SHREQ { $$ = $2; }
// If generic_args starts with "<<", the first arg must be a
// TyQualifiedPath because that's the only type that can start with a
// '<'. This rule parses that as the first ty_sum and then continues
// with the rest of generic_values.
| SHL ty_qualified_path_and_generic_values '>'   { $$ = $2; }
| SHL ty_qualified_path_and_generic_values SHR   { $$ = $2; }
| SHL ty_qualified_path_and_generic_values GE    { $$ = $2; }
| SHL ty_qualified_path_and_generic_values SHREQ { $$ = $2; }
;

generic_values
: maybe_ty_sums_and_or_bindings { $$ = linkTree("GenericValues", 1, $1); }
;

maybe_ty_sums_and_or_bindings
: ty_sums
| ty_sums ','
| ty_sums ',' bindings { $$ = linkTree("TySumsAndBindings", 2, $1, $3); }
| bindings
| bindings ','
| %empty               { $$ = linkTree("maybe_ty_sums_and_or_bindings", 0, NULL); }
;

maybe_bindings
: ',' bindings { $$ = $2; }
| %empty       { $$ = linkTree("maybe_bindings", 0, NULL); }
;

////////////////////////////////////////////////////////////////////////
// Part 2: Patterns
////////////////////////////////////////////////////////////////////////

pat
: UNDERSCORE                                      { $$ = linkTree ("PatWild", 0, NULL); }
| '&' pat                                         { $$ = linkTree("PatRegion", 1, $2); }
| '&' MUT pat                                     { $$ = linkTree("PatRegion", 1, $3); }
| ANDAND pat                                      { $$ = linkTree("PatRegion", 1, linkTree("PatRegion", 1, $2)); }
| '(' ')'                                         { $$ = linkTree ("PatUnit", 0, NULL); }
| '(' pat_tup ')'                                 { $$ = linkTree("PatTup", 1, $2); }
| '[' pat_vec ']'                                 { $$ = linkTree("PatVec", 1, $2); }
| lit_or_path
| lit_or_path DOTDOTDOT lit_or_path               { $$ = linkTree("PatRange", 2, $1, $3); }
| path_expr '{' pat_struct '}'                    { $$ = linkTree("PatStruct", 2, $1, $3); }
| path_expr '(' ')'                               { $$ = linkTree("PatEnum", 2, $1, linkTree("", 0, NULL)); }
| path_expr '(' pat_tup ')'                       { $$ = linkTree("PatEnum", 2, $1, $3); }
| path_expr '!' maybe_ident delimited_token_trees { $$ = linkTree("PatMac", 3, $1, $3, $4); }
| binding_mode ident                              { $$ = linkTree("PatIdent", 2, $1, $2); }
|              ident '@' pat                      {  }
| binding_mode ident '@' pat                      { $$ = linkTree("PatIdent", 3, $1, $2, $4); }
| BOX pat                                         { $$ = linkTree("PatUniq", 1, $2); }
| '<' ty_sum maybe_as_trait_ref '>' MOD_SEP ident { $$ = linkTree("PatQualifiedPath", 3, $2, $3, $6); }
| SHL ty_sum maybe_as_trait_ref '>' MOD_SEP ident maybe_as_trait_ref '>' MOD_SEP ident
{
  $$ = linkTree("PatQualifiedPath", 3, linkTree("PatQualifiedPath", 3, $2, $3, $6), $7, $10);
}
;

pats_or
: pat              { $$ = linkTree("Pats", 1, $1); }
| pats_or '|' pat  { $$ = graftTree($1, 1, $3); }
;

binding_mode
: REF         { $$ = linkTree("BindByRef", 1, linkTree ("MutImmutable", 0, NULL)); }
| REF MUT     { $$ = linkTree("BindByRef", 1, linkTree ("MutMutable", 0, NULL)); }
| MUT         { $$ = linkTree("BindByValue", 1, linkTree ("MutMutable", 0, NULL)); }
;

lit_or_path
: path_expr    { $$ = linkTree("PatLit", 1, $1); }
| lit          { $$ = linkTree("PatLit", 1, $1); }
| '-' lit      { $$ = linkTree("PatLit", 1, $2); }
;

pat_field
:                  ident        { $$ = linkTree("PatField", 1, $1); }
|     binding_mode ident        { $$ = linkTree("PatField", 2, $1, $2); }
| BOX              ident        { $$ = linkTree("PatField", 2, linkTree ("box", 0, NULL), $2); }
| BOX binding_mode ident        { $$ = linkTree("PatField", 3, linkTree ("box", 0, NULL), $2, $3); }
|              ident ':' pat    { $$ = linkTree("PatField", 2, $1, $3); }
| binding_mode ident ':' pat    { $$ = linkTree("PatField", 3, $1, $2, $4); }
|        LIT_INTEGER ':' pat    { $$ = linkTree("PatField", 2, linkTree("LIT_INTEGER", 0, $1), $3); }
;

pat_fields
: pat_field                  { $$ = linkTree("PatFields", 1, $1); }
| pat_fields ',' pat_field   { $$ = graftTree($1, 1, $3); }
;

pat_struct
: pat_fields                 { $$ = linkTree("PatStruct", 2, $1, linkTree ("false", 0, NULL)); }
| pat_fields ','             { $$ = linkTree("PatStruct", 2, $1, linkTree ("false", 0, NULL)); }
| pat_fields ',' DOTDOT      { $$ = linkTree("PatStruct", 2, $1, linkTree ("true", 0, NULL)); }
| DOTDOT                     { $$ = linkTree("PatStruct", 1, linkTree ("true", 0, NULL)); }
| %empty                     { $$ = linkTree("PatStruct", 0, NULL); }
;

pat_tup
: pat_tup_elts                                  { $$ = linkTree("PatTup", 2, $1, linkTree("", 0, NULL)); }
| pat_tup_elts                             ','  { $$ = linkTree("PatTup", 2, $1, linkTree("", 0, NULL)); }
| pat_tup_elts     DOTDOT                       { $$ = linkTree("PatTup", 2, $1, linkTree("", 0, NULL)); }
| pat_tup_elts ',' DOTDOT                       { $$ = linkTree("PatTup", 2, $1, linkTree("", 0, NULL)); }
| pat_tup_elts     DOTDOT ',' pat_tup_elts      { $$ = linkTree("PatTup", 2, $1, $4); }
| pat_tup_elts     DOTDOT ',' pat_tup_elts ','  { $$ = linkTree("PatTup", 2, $1, $4); }
| pat_tup_elts ',' DOTDOT ',' pat_tup_elts      { $$ = linkTree("PatTup", 2, $1, $5); }
| pat_tup_elts ',' DOTDOT ',' pat_tup_elts ','  { $$ = linkTree("PatTup", 2, $1, $5); }
|                  DOTDOT ',' pat_tup_elts      { $$ = linkTree("PatTup", 2, linkTree("", 0, NULL), $3); }
|                  DOTDOT ',' pat_tup_elts ','  { $$ = linkTree("PatTup", 2, linkTree("", 0, NULL), $3); }
|                  DOTDOT                       { $$ = linkTree("PatTup", 2, linkTree("", 0, NULL), linkTree("", 0, NULL)); }
;

pat_tup_elts
: pat                    { $$ = linkTree("PatTupElts", 1, $1); }
| pat_tup_elts ',' pat   { $$ = graftTree($1, 1, $3); }
;

pat_vec
: pat_vec_elts                                  { $$ = linkTree("PatVec", 2, $1, linkTree("", 0, NULL)); }
| pat_vec_elts                             ','  { $$ = linkTree("PatVec", 2, $1, linkTree("", 0, NULL)); }
| pat_vec_elts     DOTDOT                       { $$ = linkTree("PatVec", 2, $1, linkTree("", 0, NULL)); }
| pat_vec_elts ',' DOTDOT                       { $$ = linkTree("PatVec", 2, $1, linkTree("", 0, NULL)); }
| pat_vec_elts     DOTDOT ',' pat_vec_elts      { $$ = linkTree("PatVec", 2, $1, $4); }
| pat_vec_elts     DOTDOT ',' pat_vec_elts ','  { $$ = linkTree("PatVec", 2, $1, $4); }
| pat_vec_elts ',' DOTDOT ',' pat_vec_elts      { $$ = linkTree("PatVec", 2, $1, $5); }
| pat_vec_elts ',' DOTDOT ',' pat_vec_elts ','  { $$ = linkTree("PatVec", 2, $1, $5); }
|                  DOTDOT ',' pat_vec_elts      { $$ = linkTree("PatVec", 2, linkTree("", 0, NULL), $3); }
|                  DOTDOT ',' pat_vec_elts ','  { $$ = linkTree("PatVec", 2, linkTree("", 0, NULL), $3); }
|                  DOTDOT                       { $$ = linkTree("PatVec", 2, linkTree("", 0, NULL), linkTree("", 0, NULL)); }
| %empty                                        { $$ = linkTree("PatVec", 2, linkTree("", 0, NULL), linkTree("", 0, NULL)); }
;

pat_vec_elts
: pat                    { $$ = linkTree("PatVecElts", 1, $1); }
| pat_vec_elts ',' pat   { $$ = graftTree($1, 1, $3); }
;

////////////////////////////////////////////////////////////////////////
// Part 3: Types
////////////////////////////////////////////////////////////////////////

ty
: ty_prim
| ty_closure
| '<' ty_sum maybe_as_trait_ref '>' MOD_SEP ident                                      { $$ = linkTree("TyQualifiedPath", 3, $2, $3, $6); }
| SHL ty_sum maybe_as_trait_ref '>' MOD_SEP ident maybe_as_trait_ref '>' MOD_SEP ident { $$ = linkTree("TyQualifiedPath", 3, linkTree("TyQualifiedPath", 3, $2, $3, $6), $7, $10); }
| '(' ty_sums ')'                                                                      { $$ = linkTree("TyTup", 1, $2); }
| '(' ty_sums ',' ')'                                                                  { $$ = linkTree("TyTup", 1, $2); }
| '(' ')'                                                                              { $$ = linkTree("TyNil", 0, NULL); }
;

ty_prim
: %prec IDENT path_generic_args_without_colons                                               { $$ = linkTree("TyPath", 2, linkTree("global", 1, linkTree ("false", 0, NULL)), $1); }
| %prec IDENT MOD_SEP path_generic_args_without_colons                                       { $$ = linkTree("TyPath", 2, linkTree("global", 1, linkTree ("true", 0, NULL)), $2); }
| %prec IDENT SELF MOD_SEP path_generic_args_without_colons                                  { $$ = linkTree("TyPath", 2, linkTree("self", 1, linkTree ("true", 0, NULL)), $3); }
| %prec IDENT path_generic_args_without_colons '!' maybe_ident delimited_token_trees         { $$ = linkTree("TyMacro", 3, $1, $3, $4); }
| %prec IDENT MOD_SEP path_generic_args_without_colons '!' maybe_ident delimited_token_trees { $$ = linkTree("TyMacro", 3, $2, $4, $5); }
| BOX ty                                                                                     { $$ = linkTree("TyBox", 1, $2); }
| '*' maybe_mut_or_const ty                                                                  { $$ = linkTree("TyPtr", 2, $2, $3); }
| '&' ty                                                                                     { $$ = linkTree("TyRptr", 2, linkTree ("MutImmutable", 0, NULL), $2); }
| '&' MUT ty                                                                                 { $$ = linkTree("TyRptr", 2, linkTree ("MutMutable", 0, NULL), $3); }
| ANDAND ty                                                                                  { $$ = linkTree("TyRptr", 1, linkTree("TyRptr", 2, linkTree ("MutImmutable", 0, NULL), $2)); }
| ANDAND MUT ty                                                                              { $$ = linkTree("TyRptr", 1, linkTree("TyRptr", 2, linkTree ("MutMutable", 0, NULL), $3)); }
| '&' lifetime maybe_mut ty                                                                  { $$ = linkTree("TyRptr", 3, $2, $3, $4); }
| ANDAND lifetime maybe_mut ty                                                               { $$ = linkTree("TyRptr", 1, linkTree("TyRptr", 3, $2, $3, $4)); }
| '[' ty ']'                                                                                 { $$ = linkTree("TyVec", 1, $2); }
| '[' ty ',' DOTDOT expr ']'                                                                 { $$ = linkTree("TyFixedLengthVec", 2, $2, $5); }
| '[' ty ';' expr ']'                                                                        { $$ = linkTree("TyFixedLengthVec", 2, $2, $4); }
| TYPEOF '(' expr ')'                                                                        { $$ = linkTree("TyTypeof", 1, $3); }
| UNDERSCORE                                                                                 { $$ = linkTree ("TyInfer", 0, NULL); }
| ty_bare_fn
| for_in_type
;

ty_bare_fn
:                         FN ty_fn_decl { $$ = $2; }
| UNSAFE                  FN ty_fn_decl { $$ = $3; }
|        EXTERN maybe_abi FN ty_fn_decl { $$ = $4; }
| UNSAFE EXTERN maybe_abi FN ty_fn_decl { $$ = $5; }
;

ty_fn_decl
: generic_params fn_anon_params ret_ty { $$ = linkTree("TyFnDecl", 3, $1, $2, $3); }
;

ty_closure
: UNSAFE '|' anon_params '|' maybe_bounds ret_ty { $$ = linkTree("TyClosure", 3, $3, $5, $6); }
|        '|' anon_params '|' maybe_bounds ret_ty { $$ = linkTree("TyClosure", 3, $2, $4, $5); }
| UNSAFE OROR maybe_bounds ret_ty                { $$ = linkTree("TyClosure", 2, $3, $4); }
|        OROR maybe_bounds ret_ty                { $$ = linkTree("TyClosure", 2, $2, $3); }
;

for_in_type
: FOR '<' maybe_lifetimes '>' for_in_type_suffix { $$ = linkTree("ForInType", 2, $3, $5); }
;

for_in_type_suffix
: ty_bare_fn
| trait_ref
| ty_closure
;

maybe_mut
: MUT              { $$ = linkTree ("MutMutable", 0, NULL); }
| %prec MUT %empty { $$ = linkTree ("MutImmutable", 0, NULL); }
;

maybe_mut_or_const
: MUT    { $$ = linkTree ("MutMutable", 0, NULL); }
| CONST  { $$ = linkTree ("MutImmutable", 0, NULL); }
| %empty { $$ = linkTree ("MutImmutable", 0, NULL); }
;

ty_qualified_path_and_generic_values
: ty_qualified_path maybe_bindings
{
  $$ = linkTree("GenericValues", 3, linkTree("", 0, NULL), linkTree("TySums", 1, linkTree("TySum", 1, $1)), $2);
}
| ty_qualified_path ',' ty_sums maybe_bindings
{
  $$ = linkTree("GenericValues", 3, linkTree("", 0, NULL), linkTree("TySums", 2, $1, $3), $4);
}
;

ty_qualified_path
: ty_sum AS trait_ref '>' MOD_SEP ident                     { $$ = linkTree("TyQualifiedPath", 3, $1, $3, $6); }
| ty_sum AS trait_ref '>' MOD_SEP ident '+' ty_param_bounds { $$ = linkTree("TyQualifiedPath", 3, $1, $3, $6); }
;

maybe_ty_sums
: ty_sums
| ty_sums ','
| %empty { $$ = linkTree("maybe_ty_sums", 0, NULL); }
;

ty_sums
: ty_sum             { $$ = linkTree("TySums", 1, $1); }
| ty_sums ',' ty_sum { $$ = graftTree($1, 1, $3); }
;

ty_sum
: ty_sum_elt            { $$ = linkTree("TySum", 1, $1); }
| ty_sum '+' ty_sum_elt { $$ = graftTree($1, 1, $3); }
;

ty_sum_elt
: ty
| lifetime
;

ty_prim_sum
: ty_prim_sum_elt                 { $$ = linkTree("TySum", 1, $1); }
| ty_prim_sum '+' ty_prim_sum_elt { $$ = graftTree($1, 1, $3); }
;

ty_prim_sum_elt
: ty_prim
| lifetime
;

maybe_ty_param_bounds
: ':' ty_param_bounds { $$ = $2; }
| %empty              { $$ = linkTree("maybe_ty_param_bounds", 0, NULL); }
;

ty_param_bounds
: boundseq
| %empty { $$ = linkTree("ty_param_bounds", 0, NULL); }
;

boundseq
: polybound
| boundseq '+' polybound { $$ = graftTree($1, 1, $3); }
;

polybound
: FOR '<' maybe_lifetimes '>' bound { $$ = linkTree("PolyBound", 2, $3, $5); }
| bound
| '?' FOR '<' maybe_lifetimes '>' bound { $$ = linkTree("PolyBound", 2, $4, $6); }
| '?' bound { $$ = $2; }
;

bindings
: binding              { $$ = linkTree("Bindings", 1, $1); }
| bindings ',' binding { $$ = graftTree($1, 1, $3); }
;

binding
: ident '=' ty { linkTree("Binding", 2, $1, $3); }
;

ty_param
: ident maybe_ty_param_bounds maybe_ty_default           { $$ = linkTree("TyParam", 3, $1, $2, $3); }
| ident '?' ident maybe_ty_param_bounds maybe_ty_default { $$ = linkTree("TyParam", 4, $1, $3, $4, $5); }
;

maybe_bounds
: %prec SHIFTPLUS
  ':' bounds             { $$ = $2; }
| %prec SHIFTPLUS %empty { $$ = linkTree("maybe_bounds", 0, NULL); }
;

bounds
: bound            { $$ = linkTree("bounds", 1, $1); }
| bounds '+' bound { $$ = graftTree($1, 1, $3); }
;

bound
: lifetime
| trait_ref
;

maybe_ltbounds
: %prec SHIFTPLUS
  ':' ltbounds       { $$ = $2; }
| %empty             { $$ = linkTree("maybe_ltbounds", 0, NULL); }
;

ltbounds
: lifetime              { $$ = linkTree("ltbounds", 1, $1); }
| ltbounds '+' lifetime { $$ = graftTree($1, 1, $3); }
;

maybe_ty_default
: '=' ty_sum { $$ = linkTree("TyDefault", 1, $2); }
| %empty     { $$ = linkTree("maybe_ty_default", 0, NULL); }
;

maybe_lifetimes
: lifetimes
| lifetimes ','
| %empty { $$ = linkTree("maybe_lifetimes", 0, NULL); }
;

lifetimes
: lifetime_and_bounds               { $$ = linkTree("Lifetimes", 1, $1); }
| lifetimes ',' lifetime_and_bounds { $$ = graftTree($1, 1, $3); }
;

lifetime_and_bounds
: LIFETIME maybe_ltbounds         { $$ = linkTree("lifetime", 2, linkTree("LIFETIME", 0, $1), $2); }
| STATIC_LIFETIME                 { $$ = linkTree ("static_lifetime", 0, NULL); }
;

lifetime
: LIFETIME         { $$ = linkTree("lifetime", 1, $1); }
| STATIC_LIFETIME  { $$ = linkTree ("static_lifetime", 0, NULL); }
;

trait_ref
: %prec IDENT path_generic_args_without_colons
| %prec IDENT MOD_SEP path_generic_args_without_colons { $$ = $2; }
;

////////////////////////////////////////////////////////////////////////
// Part 4: Blocks, statements, and expressions
////////////////////////////////////////////////////////////////////////

inner_attrs_and_block
: '{' maybe_inner_attrs maybe_stmts '}'        { $$ = linkTree("ExprBlock", 2, $2, $3); }
;

block
: '{' maybe_stmts '}'                          { $$ = linkTree("ExprBlock", 1, $2); }
;

maybe_stmts
: stmts
| stmts nonblock_expr { $$ = graftTree($1, 1, $2); }
| nonblock_expr
| %empty              { $$ = linkTree("maybe_stmts", 0, NULL); }
;

// There are two sub-grammars within a "stmts: exprs" derivation
// depending on whether each stmt-expr is a block-expr form; this is to
// handle the "semicolon rule" for stmt sequencing that permits
// writing
//
//     if foo { bar } 10
//
// as a sequence of two stmts (one if-expr stmt, one lit-10-expr
// stmt). Unfortunately by permitting juxtaposition of exprs in
// sequence like that, the non-block expr grammar has to have a
// second limited sub-grammar that excludes the prefix exprs that
// are ambiguous with binops. That is to say:
//
//     {10} - 1
//
// should parse as (progn (progn 10) (- 1)) not (- (progn 10) 1), that
// is to say, two statements rather than one, at least according to
// the mainline rust parser.
//
// So we wind up with a 3-way split in exprs that occur in stmt lists:
// block, nonblock-prefix, and nonblock-nonprefix.
//
// In non-stmts contexts, expr can relax this trichotomy.

stmts
: stmt           { $$ = linkTree("stmts", 1, $1); }
| stmts stmt     { $$ = graftTree($1, 1, $2); }
;

stmt
: maybe_outer_attrs let     { $$ = $2; }
|                 stmt_item
|             PUB stmt_item { $$ = $2; }
| outer_attrs     stmt_item { $$ = $2; }
| outer_attrs PUB stmt_item { $$ = $3; }
| full_block_expr
| maybe_outer_attrs block   { $$ = $2; }
|             nonblock_expr ';'
| outer_attrs nonblock_expr ';' { $$ = $2; }
| ';'                   { $$ = linkTree("stmt", 0, NULL); }
;

maybe_exprs
: exprs
| exprs ','
| %empty { $$ = linkTree("maybe_exprs", 0, NULL); }
;

maybe_expr
: expr
| %empty { $$ = linkTree("maybe_expr", 0, NULL); }
;

exprs
: expr                                                        { $$ = linkTree("exprs", 1, $1); }
| exprs ',' expr                                              { $$ = graftTree($1, 1, $3); }
;

path_expr
: path_generic_args_with_colons
| MOD_SEP path_generic_args_with_colons      { $$ = $2; }
| SELF MOD_SEP path_generic_args_with_colons { $$ = linkTree("SelfPath", 1, $3); }
;

// A path with a lifetime and type parameters with double colons before
// the type parameters; e.g. `foo::bar::<'a>::Baz::<T>`
//
// These show up in expr context, in order to disambiguate from "less-than"
// expressions.
path_generic_args_with_colons
: ident                                              { $$ = linkTree("components", 1, $1); }
| SUPER                                              { $$ = linkTree ("Super", 0, NULL); }
| path_generic_args_with_colons MOD_SEP ident        { $$ = graftTree($1, 1, $3); }
| path_generic_args_with_colons MOD_SEP SUPER        { $$ = graftTree($1, 1, linkTree ("Super", 0, NULL)); }
| path_generic_args_with_colons MOD_SEP generic_args { $$ = graftTree($1, 1, $3); }
;

// the braces-delimited macro is a block_expr so it doesn't appear here
macro_expr
: path_expr '!' maybe_ident parens_delimited_token_trees   { $$ = linkTree("MacroExpr", 3, $1, $3, $4); }
| path_expr '!' maybe_ident brackets_delimited_token_trees { $$ = linkTree("MacroExpr", 3, $1, $3, $4); }
;

nonblock_expr
: lit                                                           { $$ = linkTree("ExprLit", 1, $1); }
| %prec IDENT
  path_expr                                                     { $$ = linkTree("ExprPath", 1, $1); }
| SELF                                                          { $$ = linkTree("ExprPath", 1, linkTree("ident", 1, linkTree ("self", 0, NULL))); }
| macro_expr                                                    { $$ = linkTree("ExprMac", 1, $1); }
| path_expr '{' struct_expr_fields '}'                          { $$ = linkTree("ExprStruct", 2, $1, $3); }
| nonblock_expr '?'                                             { $$ = linkTree("ExprTry", 1, $1); }
| nonblock_expr '.' path_generic_args_with_colons               { $$ = linkTree("ExprField", 2, $1, $3); }
| nonblock_expr '.' LIT_INTEGER                                 { $$ = linkTree("ExprTupleIndex", 1, $1); }
| nonblock_expr '[' maybe_expr ']'                              { $$ = linkTree("ExprIndex", 2, $1, $3); }
| nonblock_expr '(' maybe_exprs ')'                             { $$ = linkTree("ExprCall", 2, $1, $3); }
| '[' vec_expr ']'                                              { $$ = linkTree("ExprVec", 1, $2); }
| '(' maybe_exprs ')'                                           { $$ = linkTree("ExprParen", 1, $2); }
| CONTINUE                                                      { $$ = linkTree("ExprAgain", 0); }
| CONTINUE lifetime                                             { $$ = linkTree("ExprAgain", 1, $2); }
| RETURN                                                        { $$ = linkTree("ExprRet", 0); }
| RETURN expr                                                   { $$ = linkTree("ExprRet", 1, $2); }
| BREAK                                                         { $$ = linkTree("ExprBreak", 0); }
| BREAK lifetime                                                { $$ = linkTree("ExprBreak", 1, $2); }
| YIELD                                                         { $$ = linkTree("ExprYield", 0); }
| YIELD expr                                                    { $$ = linkTree("ExprYield", 1, $2); }
| nonblock_expr LARROW expr                                     { $$ = linkTree("ExprInPlace", 2, $1, $3); }
| nonblock_expr '=' expr                                        { $$ = linkTree("ExprAssign", 2, $1, $3); }
| nonblock_expr SHLEQ expr                                      { $$ = linkTree("ExprAssignShl", 2, $1, $3); }
| nonblock_expr SHREQ expr                                      { $$ = linkTree("ExprAssignShr", 2, $1, $3); }
| nonblock_expr MINUSEQ expr                                    { $$ = linkTree("ExprAssignSub", 2, $1, $3); }
| nonblock_expr ANDEQ expr                                      { $$ = linkTree("ExprAssignBitAnd", 2, $1, $3); }
| nonblock_expr OREQ expr                                       { $$ = linkTree("ExprAssignBitOr", 2, $1, $3); }
| nonblock_expr PLUSEQ expr                                     { $$ = linkTree("ExprAssignAdd", 2, $1, $3); }
| nonblock_expr STAREQ expr                                     { $$ = linkTree("ExprAssignMul", 2, $1, $3); }
| nonblock_expr SLASHEQ expr                                    { $$ = linkTree("ExprAssignDiv", 2, $1, $3); }
| nonblock_expr CARETEQ expr                                    { $$ = linkTree("ExprAssignBitXor", 2, $1, $3); }
| nonblock_expr PERCENTEQ expr                                  { $$ = linkTree("ExprAssignRem", 2, $1, $3); }
| nonblock_expr OROR expr                                       { $$ = linkTree("ExprBinary", 3, linkTree ("BiOr", 0, NULL), $1, $3); }
| nonblock_expr ANDAND expr                                     { $$ = linkTree("ExprBinary", 3, linkTree ("BiAnd", 0, NULL), $1, $3); }
| nonblock_expr EQEQ expr                                       { $$ = linkTree("ExprBinary", 3, linkTree ("BiEq", 0, NULL), $1, $3); }
| nonblock_expr NE expr                                         { $$ = linkTree("ExprBinary", 3, linkTree ("BiNe", 0, NULL), $1, $3); }
| nonblock_expr '<' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiLt", 0, NULL), $1, $3); }
| nonblock_expr '>' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiGt", 0, NULL), $1, $3); }
| nonblock_expr LE expr                                         { $$ = linkTree("ExprBinary", 3, linkTree ("BiLe", 0, NULL), $1, $3); }
| nonblock_expr GE expr                                         { $$ = linkTree("ExprBinary", 3, linkTree ("BiGe", 0, NULL), $1, $3); }
| nonblock_expr '|' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiBitOr", 0, NULL), $1, $3); }
| nonblock_expr '^' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiBitXor", 0, NULL), $1, $3); }
| nonblock_expr '&' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiBitAnd", 0, NULL), $1, $3); }
| nonblock_expr SHL expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiShl", 0, NULL), $1, $3); }
| nonblock_expr SHR expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiShr", 0, NULL), $1, $3); }
| nonblock_expr '+' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiAdd", 0, NULL), $1, $3); }
| nonblock_expr '-' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiSub", 0, NULL), $1, $3); }
| nonblock_expr '*' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiMul", 0, NULL), $1, $3); }
| nonblock_expr '/' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiDiv", 0, NULL), $1, $3); }
| nonblock_expr '%' expr                                        { $$ = linkTree("ExprBinary", 3, linkTree ("BiRem", 0, NULL), $1, $3); }
| nonblock_expr DOTDOT                                          { $$ = linkTree("ExprRange", 2, $1, linkTree("", 0, NULL)); }
| nonblock_expr DOTDOT expr                                     { $$ = linkTree("ExprRange", 2, $1, $3); }
|               DOTDOT expr                                     { $$ = linkTree("ExprRange", 2, linkTree("", 0, NULL), $2); }
|               DOTDOT                                          { $$ = linkTree("ExprRange", 2, linkTree("", 0, NULL), linkTree("", 0, NULL)); }
| nonblock_expr AS ty                                           { $$ = linkTree("ExprCast", 2, $1, $3); }
| nonblock_expr ':' ty                                          { $$ = linkTree("ExprTypeAscr", 2, $1, $3); }
| BOX expr                                                      { $$ = linkTree("ExprBox", 1, $2); }
| expr_qualified_path
| nonblock_prefix_expr
;

expr
: lit                                                 { $$ = linkTree("ExprLit", 1, $1); }
| %prec IDENT
  path_expr                                           { $$ = linkTree("ExprPath", 1, $1); }
| SELF                                                { $$ = linkTree("ExprPath", 1, linkTree("ident", 1, linkTree ("self", 0, NULL))); }
| macro_expr                                          { $$ = linkTree("ExprMac", 1, $1); }
| path_expr '{' struct_expr_fields '}'                { $$ = linkTree("ExprStruct", 2, $1, $3); }
| expr '?'                                            { $$ = linkTree("ExprTry", 1, $1); }
| expr '.' path_generic_args_with_colons              { $$ = linkTree("ExprField", 2, $1, $3); }
| expr '.' LIT_INTEGER                                { $$ = linkTree("ExprTupleIndex", 1, $1); }
| expr '[' maybe_expr ']'                             { $$ = linkTree("ExprIndex", 2, $1, $3); }
| expr '(' maybe_exprs ')'                            { $$ = linkTree("ExprCall", 2, $1, $3); }
| '(' maybe_exprs ')'                                 { $$ = linkTree("ExprParen", 1, $2); }
| '[' vec_expr ']'                                    { $$ = linkTree("ExprVec", 1, $2); }
| CONTINUE                                            { $$ = linkTree("ExprAgain", 0); }
| CONTINUE ident                                      { $$ = linkTree("ExprAgain", 1, $2); }
| RETURN                                              { $$ = linkTree("ExprRet", 0); }
| RETURN expr                                         { $$ = linkTree("ExprRet", 1, $2); }
| BREAK                                               { $$ = linkTree("ExprBreak", 0); }
| BREAK ident                                         { $$ = linkTree("ExprBreak", 1, $2); }
| YIELD                                               { $$ = linkTree("ExprYield", 0); }
| YIELD expr                                          { $$ = linkTree("ExprYield", 1, $2); }
| expr LARROW expr                                    { $$ = linkTree("ExprInPlace", 2, $1, $3); }
| expr '=' expr                                       { $$ = linkTree("ExprAssign", 2, $1, $3); }
| expr SHLEQ expr                                     { $$ = linkTree("ExprAssignShl", 2, $1, $3); }
| expr SHREQ expr                                     { $$ = linkTree("ExprAssignShr", 2, $1, $3); }
| expr MINUSEQ expr                                   { $$ = linkTree("ExprAssignSub", 2, $1, $3); }
| expr ANDEQ expr                                     { $$ = linkTree("ExprAssignBitAnd", 2, $1, $3); }
| expr OREQ expr                                      { $$ = linkTree("ExprAssignBitOr", 2, $1, $3); }
| expr PLUSEQ expr                                    { $$ = linkTree("ExprAssignAdd", 2, $1, $3); }
| expr STAREQ expr                                    { $$ = linkTree("ExprAssignMul", 2, $1, $3); }
| expr SLASHEQ expr                                   { $$ = linkTree("ExprAssignDiv", 2, $1, $3); }
| expr CARETEQ expr                                   { $$ = linkTree("ExprAssignBitXor", 2, $1, $3); }
| expr PERCENTEQ expr                                 { $$ = linkTree("ExprAssignRem", 2, $1, $3); }
| expr OROR expr                                      { $$ = linkTree("ExprBinary", 3, linkTree("BiOr", 0, NULL), $1, $3); }
| expr ANDAND expr                                    { $$ = linkTree("ExprBinary", 3, linkTree("BiAnd", 0, NULL), $1, $3); }
| expr EQEQ expr                                      { $$ = linkTree("ExprBinary", 3, linkTree("BiEq", 0, NULL), $1, $3); }
| expr NE expr                                        { $$ = linkTree("ExprBinary", 3, linkTree("BiNe", 0, NULL), $1, $3); }
| expr '<' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiLt", 0, NULL), $1, $3); }
| expr '>' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiGt", 0, NULL), $1, $3); }
| expr LE expr                                        { $$ = linkTree("ExprBinary", 3, linkTree("BiLe", 0, NULL), $1, $3); }
| expr GE expr                                        { $$ = linkTree("ExprBinary", 3, linkTree("BiGe", 0, NULL), $1, $3); }
| expr '|' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiBitOr", 0, NULL), $1, $3); }
| expr '^' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiBitXor", 0, NULL), $1, $3); }
| expr '&' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiBitAnd", 0, NULL), $1, $3); }
| expr SHL expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiShl", 0, NULL), $1, $3); }
| expr SHR expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiShr", 0, NULL), $1, $3); }
| expr '+' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiAdd", 0, NULL), $1, $3); }
| expr '-' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiSub", 0, NULL), $1, $3); }
| expr '*' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiMul", 0, NULL), $1, $3); }
| expr '/' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiDiv", 0, NULL), $1, $3); }
| expr '%' expr                                       { $$ = linkTree("ExprBinary", 3, linkTree("BiRem", 0, NULL), $1, $3); }
| expr DOTDOT                                         { $$ = linkTree("ExprRange", 2, $1, linkTree("", 0, NULL)); }
| expr DOTDOT expr                                    { $$ = linkTree("ExprRange", 2, $1, $3); }
|      DOTDOT expr                                    { $$ = linkTree("ExprRange", 2, linkTree("", 0, NULL), $2); }
|      DOTDOT                                         { $$ = linkTree("ExprRange", 2, linkTree("", 0, NULL), linkTree("", 0, NULL)); }
| expr AS ty                                          { $$ = linkTree("ExprCast", 2, $1, $3); }
| expr ':' ty                                         { $$ = linkTree("ExprTypeAscr", 2, $1, $3); }
| BOX expr                                            { $$ = linkTree("ExprBox", 1, $2); }
| expr_qualified_path
| block_expr
| block
| nonblock_prefix_expr
;

expr_nostruct
: lit                                                 { $$ = linkTree("ExprLit", 1, $1); }
| %prec IDENT
  path_expr                                           { $$ = linkTree("ExprPath", 1, $1); }
| SELF                                                { $$ = linkTree("ExprPath", 1, linkTree("ident", 1, linkTree ("self", 0, NULL))); }
| macro_expr                                          { $$ = linkTree("ExprMac", 1, $1); }
| expr_nostruct '?'                                   { $$ = linkTree("ExprTry", 1, $1); }
| expr_nostruct '.' path_generic_args_with_colons     { $$ = linkTree("ExprField", 2, $1, $3); }
| expr_nostruct '.' LIT_INTEGER                       { $$ = linkTree("ExprTupleIndex", 1, $1); }
| expr_nostruct '[' maybe_expr ']'                    { $$ = linkTree("ExprIndex", 2, $1, $3); }
| expr_nostruct '(' maybe_exprs ')'                   { $$ = linkTree("ExprCall", 2, $1, $3); }
| '[' vec_expr ']'                                    { $$ = linkTree("ExprVec", 1, $2); }
| '(' maybe_exprs ')'                                 { $$ = linkTree("ExprParen", 1, $2); }
| CONTINUE                                            { $$ = linkTree("ExprAgain", 0); }
| CONTINUE ident                                      { $$ = linkTree("ExprAgain", 1, $2); }
| RETURN                                              { $$ = linkTree("ExprRet", 0); }
| RETURN expr                                         { $$ = linkTree("ExprRet", 1, $2); }
| BREAK                                               { $$ = linkTree("ExprBreak", 0); }
| BREAK ident                                         { $$ = linkTree("ExprBreak", 1, $2); }
| YIELD                                               { $$ = linkTree("ExprYield", 0); }
| YIELD expr                                          { $$ = linkTree("ExprYield", 1, $2); }
| expr_nostruct LARROW expr_nostruct                  { $$ = linkTree("ExprInPlace", 2, $1, $3); }
| expr_nostruct '=' expr_nostruct                     { $$ = linkTree("ExprAssign", 2, $1, $3); }
| expr_nostruct SHLEQ expr_nostruct                   { $$ = linkTree("ExprAssignShl", 2, $1, $3); }
| expr_nostruct SHREQ expr_nostruct                   { $$ = linkTree("ExprAssignShr", 2, $1, $3); }
| expr_nostruct MINUSEQ expr_nostruct                 { $$ = linkTree("ExprAssignSub", 2, $1, $3); }
| expr_nostruct ANDEQ expr_nostruct                   { $$ = linkTree("ExprAssignBitAnd", 2, $1, $3); }
| expr_nostruct OREQ expr_nostruct                    { $$ = linkTree("ExprAssignBitOr", 2, $1, $3); }
| expr_nostruct PLUSEQ expr_nostruct                  { $$ = linkTree("ExprAssignAdd", 2, $1, $3); }
| expr_nostruct STAREQ expr_nostruct                  { $$ = linkTree("ExprAssignMul", 2, $1, $3); }
| expr_nostruct SLASHEQ expr_nostruct                 { $$ = linkTree("ExprAssignDiv", 2, $1, $3); }
| expr_nostruct CARETEQ expr_nostruct                 { $$ = linkTree("ExprAssignBitXor", 2, $1, $3); }
| expr_nostruct PERCENTEQ expr_nostruct               { $$ = linkTree("ExprAssignRem", 2, $1, $3); }
| expr_nostruct OROR expr_nostruct                    { $$ = linkTree("ExprBinary", 3, linkTree("BiOr", 0, NULL), $1, $3); }
| expr_nostruct ANDAND expr_nostruct                  { $$ = linkTree("ExprBinary", 3, linkTree("BiAnd", 0, NULL), $1, $3); }
| expr_nostruct EQEQ expr_nostruct                    { $$ = linkTree("ExprBinary", 3, linkTree("BiEq", 0, NULL), $1, $3); }
| expr_nostruct NE expr_nostruct                      { $$ = linkTree("ExprBinary", 3, linkTree("BiNe", 0, NULL), $1, $3); }
| expr_nostruct '<' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiLt", 0, NULL), $1, $3); }
| expr_nostruct '>' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiGt", 0, NULL), $1, $3); }
| expr_nostruct LE expr_nostruct                      { $$ = linkTree("ExprBinary", 3, linkTree("BiLe", 0, NULL), $1, $3); }
| expr_nostruct GE expr_nostruct                      { $$ = linkTree("ExprBinary", 3, linkTree("BiGe", 0, NULL), $1, $3); }
| expr_nostruct '|' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiBitOr", 0, NULL), $1, $3); }
| expr_nostruct '^' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiBitXor", 0, NULL), $1, $3); }
| expr_nostruct '&' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiBitAnd", 0, NULL), $1, $3); }
| expr_nostruct SHL expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiShl", 0, NULL), $1, $3); }
| expr_nostruct SHR expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiShr", 0, NULL), $1, $3); }
| expr_nostruct '+' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiAdd", 0, NULL), $1, $3); }
| expr_nostruct '-' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiSub", 0, NULL), $1, $3); }
| expr_nostruct '*' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiMul", 0, NULL), $1, $3); }
| expr_nostruct '/' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiDiv", 0, NULL), $1, $3); }
| expr_nostruct '%' expr_nostruct                     { $$ = linkTree("ExprBinary", 3, linkTree("BiRem", 0, NULL), $1, $3); }
| expr_nostruct DOTDOT               %prec RANGE      { $$ = linkTree("ExprRange", 2, $1, linkTree("", 0, NULL)); }
| expr_nostruct DOTDOT expr_nostruct                  { $$ = linkTree("ExprRange", 2, $1, $3); }
|               DOTDOT expr_nostruct                  { $$ = linkTree("ExprRange", 2, linkTree("", 0, NULL), $2); }
|               DOTDOT                                { $$ = linkTree("ExprRange", 2, linkTree("", 0, NULL), linkTree("", 0, NULL)); }
| expr_nostruct AS ty                                 { $$ = linkTree("ExprCast", 2, $1, $3); }
| expr_nostruct ':' ty                                { $$ = linkTree("ExprTypeAscr", 2, $1, $3); }
| BOX expr                                            { $$ = linkTree("ExprBox", 1, $2); }
| expr_qualified_path
| block_expr
| block
| nonblock_prefix_expr_nostruct
;

nonblock_prefix_expr_nostruct
: '-' expr_nostruct                         { $$ = linkTree("ExprUnary", 2, linkTree("UnNeg", 0, NULL), $2); }
| '!' expr_nostruct                         { $$ = linkTree("ExprUnary", 2, linkTree("UnNot", 0, NULL), $2); }
| '*' expr_nostruct                         { $$ = linkTree("ExprUnary", 2, linkTree("UnDeref", 0, NULL), $2); }
| '&' maybe_mut expr_nostruct               { $$ = linkTree("ExprAddrOf", 2, $2, $3); }
| ANDAND maybe_mut expr_nostruct            { $$ = linkTree("ExprAddrOf", 1, linkTree("ExprAddrOf", 2, $2, $3)); }
| lambda_expr_nostruct
| MOVE lambda_expr_nostruct                 { $$ = $2; }
;

nonblock_prefix_expr
: '-' expr                         { $$ = linkTree("ExprUnary", 2, linkTree("UnNeg", 0, NULL), $2); }
| '!' expr                         { $$ = linkTree("ExprUnary", 2, linkTree("UnNot", 0, NULL), $2); }
| '*' expr                         { $$ = linkTree("ExprUnary", 2, linkTree("UnDeref", 0, NULL), $2); }
| '&' maybe_mut expr               { $$ = linkTree("ExprAddrOf", 2, $2, $3); }
| ANDAND maybe_mut expr            { $$ = linkTree("ExprAddrOf", 1, linkTree("ExprAddrOf", 2, $2, $3)); }
| lambda_expr
| MOVE lambda_expr                 { $$ = $2; }
;

expr_qualified_path
: '<' ty_sum maybe_as_trait_ref '>' MOD_SEP ident maybe_qpath_params
{
  $$ = linkTree("ExprQualifiedPath", 4, $2, $3, $6, $7);
}
| SHL ty_sum maybe_as_trait_ref '>' MOD_SEP ident maybe_as_trait_ref '>' MOD_SEP ident
{
  $$ = linkTree("ExprQualifiedPath", 3, linkTree("ExprQualifiedPath", 3, $2, $3, $6), $7, $10);
}
| SHL ty_sum maybe_as_trait_ref '>' MOD_SEP ident generic_args maybe_as_trait_ref '>' MOD_SEP ident
{
  $$ = linkTree("ExprQualifiedPath", 3, linkTree("ExprQualifiedPath", 4, $2, $3, $6, $7), $8, $11);
}
| SHL ty_sum maybe_as_trait_ref '>' MOD_SEP ident maybe_as_trait_ref '>' MOD_SEP ident generic_args
{
  $$ = linkTree("ExprQualifiedPath", 4, linkTree("ExprQualifiedPath", 3, $2, $3, $6), $7, $10, $11);
}
| SHL ty_sum maybe_as_trait_ref '>' MOD_SEP ident generic_args maybe_as_trait_ref '>' MOD_SEP ident generic_args
{
  $$ = linkTree("ExprQualifiedPath", 4, linkTree("ExprQualifiedPath", 4, $2, $3, $6, $7), $8, $11, $12);
}

maybe_qpath_params
: MOD_SEP generic_args { $$ = $2; }
| %empty               { $$ = linkTree("maybe_qpath_params", 0, NULL); }
;

maybe_as_trait_ref
: AS trait_ref { $$ = $2; }
| %empty       { $$ = linkTree("maybe_as_trait_ref", 0, NULL); }
;

lambda_expr
: %prec LAMBDA
  OROR ret_ty expr                                    { $$ = linkTree("ExprFnBlock", 3, linkTree("", 0, NULL), $2, $3); }
| %prec LAMBDA
  '|' '|' ret_ty expr                                 { $$ = linkTree("ExprFnBlock", 3, linkTree("", 0, NULL), $3, $4); }
| %prec LAMBDA
  '|' inferrable_params '|' ret_ty expr               { $$ = linkTree("ExprFnBlock", 3, $2, $4, $5); }
| %prec LAMBDA
  '|' inferrable_params OROR lambda_expr_no_first_bar { $$ = linkTree("ExprFnBlock", 3, $2, linkTree("", 0, NULL), $4); }
;

lambda_expr_no_first_bar
: %prec LAMBDA
  '|' ret_ty expr                                 { $$ = linkTree("ExprFnBlock", 3, linkTree("", 0, NULL), $2, $3); }
| %prec LAMBDA
  inferrable_params '|' ret_ty expr               { $$ = linkTree("ExprFnBlock", 3, $1, $3, $4); }
| %prec LAMBDA
  inferrable_params OROR lambda_expr_no_first_bar { $$ = linkTree("ExprFnBlock", 3, $1, linkTree("", 0, NULL), $3); }
;

lambda_expr_nostruct
: %prec LAMBDA
  OROR expr_nostruct                                           { $$ = linkTree("ExprFnBlock", 2, linkTree("", 0, NULL), $2); }
| %prec LAMBDA
  '|' '|' ret_ty expr_nostruct                                 { $$ = linkTree("ExprFnBlock", 3, linkTree("", 0, NULL), $3, $4); }
| %prec LAMBDA
  '|' inferrable_params '|' expr_nostruct                      { $$ = linkTree("ExprFnBlock", 2, $2, $4); }
| %prec LAMBDA
  '|' inferrable_params OROR lambda_expr_nostruct_no_first_bar { $$ = linkTree("ExprFnBlock", 3, $2, linkTree("", 0, NULL), $4); }
;

lambda_expr_nostruct_no_first_bar
: %prec LAMBDA
  '|' ret_ty expr_nostruct                                 { $$ = linkTree("ExprFnBlock", 3, linkTree("", 0, NULL), $2, $3); }
| %prec LAMBDA
  inferrable_params '|' ret_ty expr_nostruct               { $$ = linkTree("ExprFnBlock", 3, $1, $3, $4); }
| %prec LAMBDA
  inferrable_params OROR lambda_expr_nostruct_no_first_bar { $$ = linkTree("ExprFnBlock", 3, $1, linkTree("", 0, NULL), $3); }
;

vec_expr
: maybe_exprs
| exprs ';' expr { $$ = linkTree("VecRepeat", 2, $1, $3); }
;

struct_expr_fields
: field_inits
| field_inits ','
| maybe_field_inits default_field_init { $$ = graftTree($1, 1, $2); }
| %empty                               { $$ = linkTree("struct_expr_fields", 0, NULL); }
;

maybe_field_inits
: field_inits
| field_inits ','
| %empty { $$ = linkTree("maybe_field_inits", 0, NULL); }
;

field_inits
: field_init                 { $$ = linkTree("FieldInits", 1, $1); }
| field_inits ',' field_init { $$ = graftTree($1, 1, $3); }
;

field_init
: ident                { $$ = linkTree("FieldInit", 1, $1); }
| ident ':' expr       { $$ = linkTree("FieldInit", 2, $1, $3); }
| LIT_INTEGER ':' expr { $$ = linkTree("FieldInit", 2, linkTree("LIT_INTEGER", 0, $1), $3); }
;

default_field_init
: DOTDOT expr   { $$ = linkTree("DefaultFieldInit", 1, $2); }
;

block_expr
: expr_match
| expr_if
| expr_if_let
| expr_while
| expr_while_let
| expr_loop
| expr_for
| UNSAFE block                                           { $$ = linkTree("UnsafeBlock", 1, $2); }
| path_expr '!' maybe_ident braces_delimited_token_trees { $$ = linkTree("Macro", 3, $1, $3, $4); }
;

full_block_expr
: block_expr
| block_expr_dot
;

block_expr_dot
: block_expr     '.' path_generic_args_with_colons %prec IDENT         { $$ = linkTree("ExprField", 2, $1, $3); }
| block_expr_dot '.' path_generic_args_with_colons %prec IDENT         { $$ = linkTree("ExprField", 2, $1, $3); }
| block_expr     '.' path_generic_args_with_colons '[' maybe_expr ']'  { $$ = linkTree("ExprIndex", 3, $1, $3, $5); }
| block_expr_dot '.' path_generic_args_with_colons '[' maybe_expr ']'  { $$ = linkTree("ExprIndex", 3, $1, $3, $5); }
| block_expr     '.' path_generic_args_with_colons '(' maybe_exprs ')' { $$ = linkTree("ExprCall", 3, $1, $3, $5); }
| block_expr_dot '.' path_generic_args_with_colons '(' maybe_exprs ')' { $$ = linkTree("ExprCall", 3, $1, $3, $5); }
| block_expr     '.' LIT_INTEGER                                       { $$ = linkTree("ExprTupleIndex", 1, $1); }
| block_expr_dot '.' LIT_INTEGER                                       { $$ = linkTree("ExprTupleIndex", 1, $1); }
;

expr_match
: MATCH expr_nostruct '{' '}'                                     { $$ = linkTree("ExprMatch", 1, $2); }
| MATCH expr_nostruct '{' match_clauses                       '}' { $$ = linkTree("ExprMatch", 2, $2, $4); }
| MATCH expr_nostruct '{' match_clauses nonblock_match_clause '}' { $$ = linkTree("ExprMatch", 2, $2, graftTree($4, 1, $5)); }
| MATCH expr_nostruct '{'               nonblock_match_clause '}' { $$ = linkTree("ExprMatch", 2, $2, linkTree("Arms", 1, $4)); }
;

match_clauses
: match_clause               { $$ = linkTree("Arms", 1, $1); }
| match_clauses match_clause { $$ = graftTree($1, 1, $2); }
;

match_clause
: nonblock_match_clause ','
| block_match_clause
| block_match_clause ','
;

nonblock_match_clause
: maybe_outer_attrs pats_or maybe_guard FAT_ARROW nonblock_expr  { $$ = linkTree("ArmNonblock", 4, $1, $2, $3, $5); }
| maybe_outer_attrs pats_or maybe_guard FAT_ARROW block_expr_dot { $$ = linkTree("ArmNonblock", 4, $1, $2, $3, $5); }
;

block_match_clause
: maybe_outer_attrs pats_or maybe_guard FAT_ARROW block      { $$ = linkTree("ArmBlock", 4, $1, $2, $3, $5); }
| maybe_outer_attrs pats_or maybe_guard FAT_ARROW block_expr { $$ = linkTree("ArmBlock", 4, $1, $2, $3, $5); }
;

maybe_guard
: IF expr_nostruct           { $$ = $2; }
| %empty                     { $$ = linkTree("maybe_guard", 0, NULL); }
;

expr_if
: IF expr_nostruct block                              { $$ = linkTree("ExprIf", 2, $2, $3); }
| IF expr_nostruct block ELSE block_or_if             { $$ = linkTree("ExprIf", 3, $2, $3, $5); }
;

expr_if_let
: IF LET pat '=' expr_nostruct block                  { $$ = linkTree("ExprIfLet", 3, $3, $5, $6); }
| IF LET pat '=' expr_nostruct block ELSE block_or_if { $$ = linkTree("ExprIfLet", 4, $3, $5, $6, $8); }
;

block_or_if
: block
| expr_if
| expr_if_let
;

expr_while
: maybe_label WHILE expr_nostruct block               { $$ = linkTree("ExprWhile", 3, $1, $3, $4); }
;

expr_while_let
: maybe_label WHILE LET pat '=' expr_nostruct block   { $$ = linkTree("ExprWhileLet", 4, $1, $4, $6, $7); }
;

expr_loop
: maybe_label LOOP block                              { $$ = linkTree("ExprLoop", 2, $1, $3); }
;

expr_for
: maybe_label FOR pat IN expr_nostruct block          { $$ = linkTree("ExprForLoop", 4, $1, $3, $5, $6); }
;

maybe_label
: lifetime ':'
| %empty { $$ = linkTree("maybe_label", 0, NULL); }
;

let
: LET pat maybe_ty_ascription maybe_init_expr ';' { $$ = linkTree("DeclLocal", 3, $2, $3, $4); }
;

////////////////////////////////////////////////////////////////////////
// Part 5: Macros and misc. rules
////////////////////////////////////////////////////////////////////////

lit
: LIT_BYTE                   { $$ = linkTree("LitByte", 0, $1); $$->type = STRING_TYPE; }
| LIT_CHAR                   { $$ = linkTree("LitChar", 0, $1); $$->type = STRING_TYPE; }
| LIT_INTEGER                { $$ = linkTree("LitInteger", 0, $1); $$->type = INT_TYPE; }
| LIT_FLOAT                  { $$ = linkTree("LitFloat", 0, $1); $$->type = DOUBLE_TYPE; }
| TRUE                       { $$ = linkTree("LitBool", 0, $1); $$->type = INT_TYPE; }
| FALSE                      { $$ = linkTree("LitBool", 0, $1); $$->type = INT_TYPE; }
| str
;

str
: LIT_STR                    { $$ = linkTree("LitStr", 0, $1); $$->type = STRING_TYPE; }
| LIT_STR_RAW                { $$ = linkTree("LitStr", 0, $1); $$->type = STRING_TYPE; }
| LIT_BYTE_STR                 { $$ = linkTree("LitByteStr", 0, $1); $$->type = STRING_TYPE; }
| LIT_BYTE_STR_RAW             { $$ = linkTree("LitByteStr", 0, $1); $$->type = STRING_TYPE; }
;

maybe_ident
: %empty { $$ = linkTree("maybe_ident", 0, NULL); }
| ident  {}
;

ident
: IDENT                      { $$ = linkTree("ident", 1, linkTree("IDENT", 0, $1)); }
// Weak keywords that can be used as identifiers
| CATCH                      { $$ = linkTree("ident", 1, linkTree("CATCH", 0, $1)); }
| DEFAULT                    { $$ = linkTree("ident", 1, linkTree("DEFAULT", 0, $1)); }
| UNION                      { $$ = linkTree("ident", 1, linkTree("UNION", 0, $1)); }
;

unpaired_token
: SHL                        { $$ = linkTree("SHL", 0, $1); }
| SHR                        { $$ = linkTree("SHR", 0, $1); }
| LE                         { $$ = linkTree("LE", 0, $1); }
| EQEQ                       { $$ = linkTree("EQEQ", 0, $1); }
| NE                         { $$ = linkTree("NE", 0, $1); }
| GE                         { $$ = linkTree("GE", 0, $1); }
| ANDAND                     { $$ = linkTree("ANDAND", 0, $1); }
| OROR                       { $$ = linkTree("OROR", 0, $1); }
| LARROW                     { $$ = linkTree("LARROW", 0, $1); }
| SHLEQ                      { $$ = linkTree("SHLEQ", 0, $1); }
| SHREQ                      { $$ = linkTree("SHREQ", 0, $1); }
| MINUSEQ                    { $$ = linkTree("MINUSEQ", 0, $1); }
| ANDEQ                      { $$ = linkTree("ANDEQ", 0, $1); }
| OREQ                       { $$ = linkTree("OREQ", 0, $1); }
| PLUSEQ                     { $$ = linkTree("PLUSEQ", 0, $1); }
| STAREQ                     { $$ = linkTree("STAREQ", 0, $1); }
| SLASHEQ                    { $$ = linkTree("SLASHEQ", 0, $1); }
| CARETEQ                    { $$ = linkTree("CARETEQ", 0, $1); }
| PERCENTEQ                  { $$ = linkTree("PERCENTEQ", 0, $1); }
| DOTDOT                     { $$ = linkTree("DOTDOT", 0, $1); }
| DOTDOTDOT                  { $$ = linkTree("DOTDOTDOT", 0, $1); }
| MOD_SEP                    { $$ = linkTree("MOD_SEP", 0, $1); }
| RARROW                     { $$ = linkTree("RARROW", 0, $1); }
| FAT_ARROW                  { $$ = linkTree("FAT_ARROW", 0, $1); }
| LIT_BYTE                   { $$ = linkTree("LIT_BYTE", 0, $1); $$->type = STRING_TYPE; }
| LIT_CHAR                   { $$ = linkTree("LIT_CHAR", 0, $1); $$->type = STRING_TYPE; }
| LIT_INTEGER                { $$ = linkTree("LIT_INTEGER", 0, $1); $$->type = INT_TYPE; }
| LIT_FLOAT                  { $$ = linkTree("LIT_FLOAT", 0, $1); $$->type = DOUBLE_TYPE; }
| LIT_STR                    { $$ = linkTree("LIT_STR", 0, $1); $$->type = DOUBLE_TYPE; }
| LIT_STR_RAW                { $$ = linkTree("LIT_STR_RAW", 0, $1); $$->type = STRING_TYPE; }
| LIT_BYTE_STR               { $$ = linkTree("LIT_BYTE_STR", 0, $1); $$->type = STRING_TYPE; }
| LIT_BYTE_STR_RAW           { $$ = linkTree("LIT_BYTE_STR_RAW", 0, $1); $$->type = STRING_TYPE; }
| IDENT                      { $$ = linkTree("IDENT", 0, $1); }
| UNDERSCORE                 { $$ = linkTree("UNDERSCORE", 0, $1); }
| LIFETIME                   { $$ = linkTree("LIFETIME", 0, $1); }
| SELF                       { $$ = linkTree("SELF", 0, $1); }
| STATIC                     { $$ = linkTree("STATIC", 0, $1); }
| ABSTRACT                   { $$ = linkTree("ABSTRACT", 0, $1); }
| ALIGNOF                    { $$ = linkTree("ALIGNOF", 0, $1); }
| AS                         { $$ = linkTree("AS", 0, $1); }
| BECOME                     { $$ = linkTree("BECOME", 0, $1); }
| BREAK                      { $$ = linkTree("BREAK", 0, $1); }
| CATCH                      { $$ = linkTree("CATCH", 0, $1); }
| CRATE                      { $$ = linkTree("CRATE", 0, $1); }
| DEFAULT                    { $$ = linkTree("DEFAULT", 0, $1); }
| DO                         { $$ = linkTree("DO", 0, $1); }
| ELSE                       { $$ = linkTree("ELSE", 0, $1); }
| ENUM                       { $$ = linkTree("ENUM", 0, $1); }
| EXTERN                     { $$ = linkTree("EXTERN", 0, $1); }
| FALSE                      { $$ = linkTree("FALSE", 0, $1); $$->type = INT_TYPE;}
| FINAL                      { $$ = linkTree("FINAL", 0, $1); }
| FN                         { $$ = linkTree("FN", 0, $1); }
| FOR                        { $$ = linkTree("FOR", 0, $1); }
| IF                         { $$ = linkTree("IF", 0, $1); }
| IMPL                       { $$ = linkTree("IMPL", 0, $1); }
| IN                         { $$ = linkTree("IN", 0, $1); }
| LET                        { $$ = linkTree("LET", 0, $1); }
| LOOP                       { $$ = linkTree("LOOP", 0, $1); }
| MACRO                      { $$ = linkTree("MACRO", 0, $1); }
| MATCH                      { $$ = linkTree("MATCH", 0, $1); }
| MOD                        { $$ = linkTree("MOD", 0, $1); }
| MOVE                       { $$ = linkTree("MOVE", 0, $1); }
| MUT                        { $$ = linkTree("MUT", 0, $1); }
| OFFSETOF                   { $$ = linkTree("OFFSETOF", 0, $1); }
| OVERRIDE                   { $$ = linkTree("OVERRIDE", 0, $1); }
| PRIV                       { $$ = linkTree("PRIV", 0, $1); }
| PUB                        { $$ = linkTree("PUB", 0, $1); }
| PURE                       { $$ = linkTree("PURE", 0, $1); }
| REF                        { $$ = linkTree("REF", 0, $1); }
| RETURN                     { $$ = linkTree("RETURN", 0, $1); }
| STRUCT                     { $$ = linkTree("STRUCT", 0, $1); }
| SIZEOF                     { $$ = linkTree("SIZEOF", 0, $1); }
| SUPER                      { $$ = linkTree("SUPER", 0, $1); }
| TRUE                       { $$ = linkTree("TRUE", 0, $1); $$->type = INT_TYPE;}
| TRAIT                      { $$ = linkTree("TRAIT", 0, $1); }
| TYPE                       { $$ = linkTree("TYPE", 0, $1); }
| UNION                      { $$ = linkTree("UNION", 0, $1); }
| UNSAFE                     { $$ = linkTree("UNSAFE", 0, $1); }
| UNSIZED                    { $$ = linkTree("UNSIZED", 0, $1); }
| USE                        { $$ = linkTree("USE", 0, $1); }
| VIRTUAL                    { $$ = linkTree("VIRTUAL", 0, $1); }
| WHILE                      { $$ = linkTree("WHILE", 0, $1); }
| YIELD                      { $$ = linkTree("YIELD", 0, $1); }
| CONTINUE                   { $$ = linkTree("CONTINUE", 0, $1); }
| PROC                       { $$ = linkTree("PROC", 0, $1); }
| BOX                        { $$ = linkTree("BOX", 0, $1); }
| CONST                      { $$ = linkTree("CONST", 0, $1); }
| WHERE                      { $$ = linkTree("WHERE", 0, $1); }
| TYPEOF                     { $$ = linkTree("TYPEOF", 0, $1); }
| INNER_DOC_COMMENT          { $$ = linkTree("INNER_DOC_COMMENT", 0, $1); }
| OUTER_DOC_COMMENT          { $$ = linkTree("OUTER_DOC_COMMENT", 0, $1); }
| SHEBANG                    { $$ = linkTree("SHEBANG", 0, $1); }
| STATIC_LIFETIME            { $$ = linkTree("STATIC_LIFETIME", 0, $1); }
| ';'                        { $$ = linkTree("SEMICOLON", 0, $1); }
| ','                        { $$ = linkTree("COMMA", 0, $1); }
| '.'                        { $$ = linkTree("DOT", 0, $1); }
| '@'                        { $$ = linkTree("AT", 0, $1); }
| '#'                        { $$ = linkTree("HASH", 0, $1); }
| '~'                        { $$ = linkTree("TILDE", 0, $1); }
| ':'                        { $$ = linkTree("COLON", 0, $1); }
| '$'                        { $$ = linkTree("DOLLAR_SIGN", 0, $1); }
| '='                        { $$ = linkTree("EQUALS", 0, $1); }
| '?'                        { $$ = linkTree("QUESTION_MARK", 0, $1); }
| '!'                        { $$ = linkTree("EXCLAMATION_MARK", 0, $1); }
| '<'                        { $$ = linkTree("LESS_THAN", 0, $1); }
| '>'                        { $$ = linkTree("GREATER_THAN", 0, $1); }
| '-'                        { $$ = linkTree("MINUS", 0, $1); }
| '&'                        { $$ = linkTree("AMPERSAND", 0, $1); }
| '|'                        { $$ = linkTree("PIPE", 0, $1); }
| '+'                        { $$ = linkTree("PLUS", 0, $1); }
| '*'                        { $$ = linkTree("ASTERISK", 0, $1); }
| '/'                        { $$ = linkTree("SLASH", 0, $1); }
| '^'                        { $$ = linkTree("CARET", 0, $1); }
| '%'                        { $$ = linkTree("PERCENT", 0, $1); }
;

token_trees
: %empty                     { $$ = linkTree("TokenTrees", 0, NULL); }
| token_trees token_tree     { $$ = graftTree($1, 1, $2); }
;

token_tree
: delimited_token_trees  {}
| unpaired_token         { $$ = linkTree("TTTok", 1, $1); }
;

delimited_token_trees
: parens_delimited_token_trees
| braces_delimited_token_trees
| brackets_delimited_token_trees
;

parens_delimited_token_trees
: '(' token_trees ')'
{
  $$ = linkTree("TTDelim", 3, linkTree("TTTok", 0, $1), $2, linkTree("TTTok", 0, $3));
}
;

braces_delimited_token_trees
: '{' token_trees '}'
{
  $$ = linkTree("TTDelim", 3, linkTree("TTTok", 0, $1), $2, linkTree("TTTok", 0, $3));
}
;

brackets_delimited_token_trees
: '[' token_trees ']'
{
  $$ = linkTree("TTDelim", 3, linkTree("TTTok", 0, $1),  $2,  linkTree("TTTok", 0, $3));
}
;
