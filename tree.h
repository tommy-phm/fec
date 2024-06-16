#ifndef TREE_H
#define TREE_H
#include <stdio.h>
#include "token.h"
#include "tac.h"

typedef struct tree tree;

typedef struct tree{
    int prodrule;
    char *symbolname;
    int type;
    int nkids;
    tree *kids[100];
    Token *leaf;
    instr *block;
    addr *place;
    addr *label;
} tree;

typedef struct {
    tree **array;
    size_t used;
    size_t size;
} treeArray;

extern treeArray trees;
extern tree *root;

void printDot(tree *t);
tree *linkTree(const char *name, int n, ...);
tree *setType(tree *t, int type);
tree *graftTree(tree *t, int n, ...);
void printTree(tree *t, int n);
tree *findTree(tree *t, char *text);

void initTreeArray(treeArray *a);
void insertTreeArray(treeArray *a, tree *t);
void freeTreeArray(treeArray *a);

void freeTree(tree *t);

void color(int color);
#endif 