#ifndef SYMTAB_H
#define SYMTAB_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "tac.h"

#define TABLE_SIZE 43

enum Type {
    UNSUPPORTED_TYPE = 1000,
    NULL_TYPE,
    BOOL_TYPE,
    INT_TYPE,
    DOUBLE_TYPE,
    STRING_TYPE,
    ARRAY_TYPE,
    FUNC_TYPE,
};
typedef struct table table;
typedef struct data data;

typedef struct data {
    int type;
    int mut;
    int assign;    
    int show;
    addr *place;
    union model{
        struct varible {
            val val;
        } var;
        struct array {
            int type;
            int size;
        } arr;
        struct function{
            tree *tree;
            table *table;
            entry **params;
            int nparms;
            int type;
        } fn;
    } model;
} data;

typedef struct entry {
    char *name;
    data *data;
    struct entry *next; 
} entry;

typedef struct table{
    int n;                              
    entry **entries;            
    table *pre;
} table;

entry *insertEntry(table *t, const char *name, data *d);
entry* findEntry(table *t, const char *name);
entry *initSymTab(tree *t);
void printSymTab(entry *e);
void freeTable(table *t);
void freeEntry(entry *e);
#endif