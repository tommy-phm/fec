#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "tac.h"
#include "symtab.h"
#include "rustgram.tab.h"
int serial = 0;

// Print string without escape chacters
char *escape(char *s) {
    size_t len = strlen(s);
    
    char *s2 = malloc(2 * len + 1);
    if (!s2) {
        perror("malloc failed");
        return NULL;
    }

    char *p = s2;
    for (size_t i = 0; i < len; i++) {
        switch (s[i]) {
            case '\"': *p++ = '\\'; *p++ = '\"'; break;
            case '\\': *p++ = '\\'; *p++ = '\\'; break;
            case '\n': *p++ = '\\'; *p++ = 'n'; break;
            case '\t': *p++ = '\\'; *p++ = 't'; break;
            default:   *p++ = s[i]; break;
        }
    }
    
    *p = '\0';
    return s2;
}

// Print name of symbole or leaf text
char *pretty_print_name(tree *t) {
    char *s2 = malloc(40);
    if (t->leaf == NULL) {
        sprintf(s2, "%s#%d", t->symbolname, t->prodrule%10);
        return s2;
    }
    else {
        sprintf(s2,"%s:%d", escape(t->leaf->text), t->leaf->category);
        return s2;
    }
}

// Print branch
void print_branch(tree *t, FILE *f) {
    fprintf(f, "N%d [shape=box label=\"%s\"];\n", t->prodrule, pretty_print_name(t));
}

// Print Leaft
void print_leaf(tree *t, FILE *f) {
    const char *s = tokenName(t->leaf->category);
    fprintf(f, "N%d [shape=box style=dotted label=\" %s \\n ", t->prodrule, s);
    int category = t->leaf->category;
    if(category == LIT_STR || category == LIT_STR_RAW || category == LIT_BYTE_STR || category == LIT_BYTE_STR_RAW)
        fprintf(f, "text = %s \\l lineno = %d \\l\"];\n", escape(t->leaf->text), t->leaf->lineno);
    else
        fprintf(f, "text = %s \\l lineno = %d \\l\"];\n", escape(t->leaf->text), t->leaf->lineno);
}

// Print graph
void print_graph2(tree *t, FILE *f) {
    int i;
    if (t->leaf != NULL) {
        print_leaf(t, f);
        return;
    }
    /* not a leaf ==> internal node */
    print_branch(t, f);
    for(i=0; i < t->nkids; i++) {
        if (t->kids[i] != NULL) {
            tree *kid = t->kids[i];
            fprintf(f, "N%d -> N%d;\n", t->prodrule, kid->prodrule);
            print_graph2(kid, f);
        }
        else { /* NULL kid, epsilon production or something */
            fprintf(f, "N%d -> N%d%d;\n", t->prodrule, t->prodrule, serial);
            fprintf(f, "N%d%d [label=\"%s\"];\n", t->prodrule, serial, "Empty rule");
            serial++;
            fprintf(stderr, "INCREMENT %d\n", serial);
        }
    }
}

// Print dot file
void printDot(tree *t){
    char *dot_filename = malloc(strlen(filename) + strlen(".dot") + 1);
    strcpy(dot_filename, filename);
    strcat(dot_filename, ".dot");
    FILE *dot_file = fopen(dot_filename, "w");
    if (!dot_file) {
        perror("\033[0;31m[Error]\033[0m Unable to open dot file");
        free(dot_filename);
        exit(1);
    }
    fprintf(dot_file, "digraph G {\n");
    print_graph2(t, dot_file);
    fprintf(dot_file, "}\n");
    fclose(dot_file);
    free(dot_filename);
}

int id = 0;// NOTE: REPLACE WITH PRODUCTION 

// Initialize tree
tree *linkTree(const char *name, int n, ...) {
    va_list args;
    va_start(args, n);
    tree *t = (tree *)malloc(sizeof(tree) + (n * sizeof(tree *)));
    if (t == NULL)
        exit(EXIT_FAILURE);
    insertTreeArray(&trees, t);
    t->prodrule = id++;
    t->symbolname = strdup(name); 
    t->nkids = n;
    t->leaf = NULL;
    t->type = NULL_TYPE;
    t->label = NULL;
    t->block = NULL;
    t->place = NULL;
    if (n == 0){
        if(strcmp(name,"ExprRet"))
            t->leaf = va_arg(args, Token *);       
    }
    else {
        for (int i = 0; i < n; i++) 
            t->kids[i] = (struct tree *)va_arg(args, tree *);
    }     
    va_end(args);
    return t;
}

// Set type of tree
tree *setType(tree *t, int type){
    t->type = type;
    return t;
}

// Combine trees
tree *graftTree(tree *t, int n, ...){
    va_list args;
    va_start(args, n);
    for (int i = 0; i < n; i++) 
        t->kids[t->nkids++] = (struct tree *)va_arg(args, tree *);
    va_end(args);
    return t;
}

// Print code 
void color(int color) {
    switch(color) {
        case 0:
            printf("\033[0;37m"); // White
            break;
        case 1:
            printf("\033[0;31m"); // Red
            break;
        case 2:
            printf("\033[0;32m"); // Green
            break;
        case 3:
            printf("\033[0;33m"); // Yellow
            break;
        case 4:
            printf("\033[0;34m"); // Blue
            break;
        case 5:
            printf("\033[0;35m"); // Magenta
            break;
        case 6:
            printf("\033[0;36m"); // Cyan
            break;
        case 7:
            printf("\033[0;30m"); // Black
            break;
    }
}

// Print tree
void printTree(tree *t, int n) {
    for (int i = 0; i < n; i++){
        color(i % 8);
        printf("|  ");
    }
    color(n % 8);
    printf("+-[%s] \033[0m", t->symbolname);

    switch (t->type) {
    case NULL_TYPE:
        printf("<> ");
        break;
    case INT_TYPE:
        printf("<int> ");
        break;
    case DOUBLE_TYPE:
        printf("<double> ");
        break;
    case STRING_TYPE:
        printf("<string> ");
        break;
    case ARRAY_TYPE:
        printf("<array> ");
        break;    
    case FUNC_TYPE:
        printf("<func> ");
        break;
    }
    if (t->nkids == 0) {
        if(t->leaf == NULL)
            printf("NULL\n");
        else
            printf("%s\n", t->leaf->text);
    } else {
        printf("\n");
        for (int i = 0; i < t->nkids; i++) 
            printTree(t->kids[i], n + 1);
    }

}

// Frind tree with specified name
tree *findTree(tree *t, char *text){
    if(!strcmp(t->symbolname, text))
        return t;
    for (int i = 0; i < t->nkids; i++){
        tree *tmp = findTree(t->kids[i], text);
        if (tmp != NULL)
            return tmp;
    }
    return NULL;
}

// Intilzie tree array
void initTreeArray(treeArray *a) {
    a->array = (tree **)malloc(sizeof(tree *));
    if (a->array == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    a->used = 0;
    a->size = 1;
}

// Add to tree array
void insertTreeArray(treeArray *a, tree *t) {
    if (a->used == a->size) {
        a->size *= 2;
        a->array = (tree **)realloc(a->array, a->size * sizeof(tree *));
        if (a->array == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }
    }
    a->array[a->used++] = t;
}

// Clear tree array
void freeTreeArray(treeArray *a) {
    for (size_t i = 0; i < a->used; ++i) {
        if((a->array[i]) != NULL)
            freeTree(a->array[i]);
            a->array[i] = NULL;
    }
    free(a->array);
    a->array = NULL;
    a->used = a->size = 0;
}

// Clear tree
void freeTree(tree *t) {
    if (t != NULL) {
        if (t->symbolname != NULL)
            free(t->symbolname);
        free(t);
    }
}