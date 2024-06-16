#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "symtab.h"
#include "rustgram.tab.h"

char *typename[] = {"unsupported", "null", "bool", "int", "double", "string", "array", "function"};

// Return index for hash table
unsigned int hash(const char *name) {
    unsigned int hash = 0;
    for (int i = 0; name[i] != '\0'; i++) {
        hash = 31 * hash + name[i];
    }
    return hash % TABLE_SIZE;
}

// Initlize Table
table *initTable(table *pre) {
    table *newTable = malloc(sizeof(table));
    newTable->n = 0;
    newTable->entries = malloc(TABLE_SIZE * sizeof(entry *));
    for (int i = 0; i < TABLE_SIZE; i++) 
        newTable->entries[i] = NULL;
    newTable->pre = pre;
    return newTable;
}

// Search symbol in table
entry* findEntry(table *t, const char *name) {
    for (int i = 0; i < TABLE_SIZE; i++) {
        entry *e = t->entries[i];
        while (e != NULL) {
            if (strcmp(e->name, name) == 0) 
                return e; 
            e = e->next;         
        }
    }
    if(t->pre != NULL){
        return findEntry(t->pre, name);}
    return NULL;
}

// Add symbol to table
entry *insertEntry(table *t, const char *name, data *d) {
    if(findEntry(t, name) != NULL){
        fprintf(stderr, "\033[0;31m[Error]\033[0m Symbol %s already declare. \n", name);
        exit(3);
    }

    unsigned int index = hash(name);
    entry *e = malloc(sizeof(entry));
    e->name = strdup(name);
    e->data = d;
    e->next = t->entries[index];
    t->entries[index] = e;
    t->n ++;
    return e;
}

// Find Data type
int convert(char *text){
    if(text == NULL)
        return NULL_TYPE;
    else if(!strcmp(text,"i8") 
    || !strcmp(text,"i16")
    || !strcmp(text,"i32")
    || !strcmp(text,"i64")
    || !strcmp(text,"u8") 
    || !strcmp(text,"u16")
    || !strcmp(text,"u32")
    || !strcmp(text,"u64")
    || !strcmp(text,"bool"))
        return INT_TYPE;
    else if(!strcmp(text,"f32") || !strcmp(text,"f64"))
        return DOUBLE_TYPE;
    else if(!strcmp(text, "String"))
        return STRING_TYPE;
    else 
        return NULL_TYPE;
}

// Type Check Expersion
void epxr(tree *t, table* tab){
    if(!strcmp(t->symbolname,"ExprCall")){
        char *name = findTree(t, "IDENT")->leaf->text;
        entry *e = findEntry(tab, name);
        if(e == NULL){
            fprintf(stderr, "\033[0;31m[Error]\033[0m Function (%s) not declare. \n", name);
            exit(3);
        }
        else{
            if(e->data->type == FUNC_TYPE){
                t->type = e->data->model.fn.type;
            }else{
                fprintf(stderr, "\033[0;31m[Error]\033[0m Incompatible type. Intilized symbol (%s) is not function.\n", name);
                exit(3);
            }
        }
        tree *tmp = findTree(t, "exprs");
        if(tmp != NULL && tmp->nkids != 0){
            for (int i = 0; i < tmp->nkids; i++) 
                epxr(tmp->kids[i], tab);
            if(e->data->model.fn.params != NULL){
                if(e->data->model.fn.nparms < tmp->nkids){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Function (%s) given more parameter than expected.\n", name);
                exit(3);
            }
                for (int i = 0; i < e->data->model.fn.nparms; i++){
                    if(e->data->model.fn.params[i]->data->type != NULL_TYPE){
                        if(e->data->model.fn.params[i]->data->type != tmp->kids[i]->type){
                            fprintf(stderr, "\033[0;31m[Error]\033[0m Function (%s) - parameter (%d) mismatch type.\n", name, i);
                            exit(3);
                        }
                    }
                }
            }
        }
    }
    else if(!strcmp(t->symbolname,"ExprPath")){
        char *name = findTree(t, "IDENT")->leaf->text;
        entry *e = findEntry(tab, name);
        if(e == NULL){
            fprintf(stderr, "\033[0;31m[Error]\033[0m Var (%s) not declare. \n", name);
            exit(3);
        }
        if(e->data->type != FUNC_TYPE)
            t->type = e->data->type;
        else{
            fprintf(stderr, "\033[0;31m[Error]\033[0m Incompatible type. Intilized symbol (%s) is function not array\n", name);
            exit(3);
        }
    }else if(!strcmp(t->symbolname,"ExprIndex")){
        char *name = findTree(t, "IDENT")->leaf->text;
        entry *e = findEntry(tab, name);
        if(e == NULL){
            fprintf(stderr, "\033[0;31m[Error]\033[0m Array (%s) not declare. \n", name);
            exit(3);
        }
        if(e->data->type != FUNC_TYPE)
            t->type = e->data->model.arr.type;
        else{
            fprintf(stderr, "\033[0;31m[Error]\033[0m Incompatible type. Intilized symbol (%s) is function not array.\n", name);
            exit(3);
        }
        //TODO: Check for out of bond index
    }else if(!strcmp(t->symbolname,"ExprLit")){
        t->type = t->kids[0]->type;
    }else if (t->nkids != 0) {
        int i = 0;
        if(!strcmp(t->symbolname,"ExprBinary"))
            i = 1;
        else if(!strcmp(t->symbolname,"exprs")){
            if(!strcmp(t->kids[0]->symbolname,"ExprPath")) 
                i = 1;
        }
        for (; i < t->nkids; i++) {
            epxr(t->kids[i], tab);
            if(t->type == NULL_TYPE)
                t->type = t->kids[i]->type;
            else{
                if(t->type != t->kids[i]->type && strcmp(t->symbolname,"VecRepeat")){
                    if(t->type == ARRAY_TYPE)
                        return;
                    fprintf(stderr, "\033[0;31m[Error]\033[0m Mismatch type.\n");
                    exit(3);
                }
            }
        }
    }
}


// Initialize varibles
void var(tree *t, table* tab){
    char *name = findTree(t->kids[0], "IDENT")->leaf->text;
    data *d = malloc(sizeof(data));
    if(findTree(t->kids[0], "MutMutable") == NULL)
        d->mut = 0;
    else
        d->mut = 1;
    d->assign = 0;
    d->show = 0;
    d->type = NULL_TYPE;
    d->place = NULL;
    tree *tmp = findTree(t->kids[1],"IDENT");
    if(tmp == NULL){
        d->type = NULL_TYPE;}
    else{ 
        if(!strcmp(t->kids[1]->kids[0]->symbolname,"TyFixedLengthVec")){
            d->type = ARRAY_TYPE;
            d->model.arr.type = convert(tmp->leaf->text);
            if(d->model.arr.type == NULL_TYPE){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Array (%s) has invalid data type. \n", name);
                exit(3);
            }
            tmp = findTree(t->kids[1]->kids[0]->kids[1],"LitInteger");
            if(tmp == NULL){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Array (%s) size is not int. \n", name);
                exit(3);
            }
            d->model.arr.size = tmp->leaf->val.ival;
        }else
            d->type = convert(tmp->leaf->text);
            t->kids[1]->type = d->type;
        if(d->type == NULL_TYPE){
            fprintf(stderr, "\033[0;31m[Error]\033[0m Varible %s has invalid data type (%s). \n", name, tmp->leaf->text);
            exit(3);
        }
    }
    if(t->nkids > 2 && t->kids[2]->nkids != 0){
        epxr(t->kids[2], tab);
        if(!strcmp(t->kids[2]->symbolname,"ExprVec")){
            int size;
            if(!strcmp(t->kids[2]->kids[0]->symbolname,"VecRepeat")){
                if(strcmp(t->kids[2]->kids[0]->kids[1]->kids[0]->symbolname,"LitInteger")){
                    fprintf(stderr, "\033[0;31m[Error]\033[0m Array (%s) size is not int. \n", name);
                    exit(3);
                }
                size = t->kids[2]->kids[0]->kids[1]->kids[0]->leaf->val.ival;
            }
            else{
                size = t->kids[2]->kids[0]->nkids;
            }
            if(d->type == ARRAY_TYPE){
                if(d->model.arr.size != size){
                    fprintf(stderr, "\033[0;31m[Error]\033[0m Array (%s) size is mismatch. \n", name);
                    exit(3);
                }
                if(d->model.arr.type != t->kids[2]->type){
                    printf("%d %d \n", d->model.arr.type, t->kids[2]->type);
                    fprintf(stderr, "\033[0;31m[Error]\033[0m Array (%s) type is mismatch. \n", name);
                    exit(3);
                }
            }
            else if(d->type == NULL_TYPE){
                d->type = ARRAY_TYPE;          
                d->model.arr.type = t->kids[2]->type;
                d->model.arr.size = size;       
            }
            else{
                fprintf(stderr, "\033[0;31m[Error]\033[0m Varible (%s) has mismatch type.\n", name);
                exit(3);
            }
        } 
        else if(d->type != NULL_TYPE && d->type != t->kids[2]->type){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Varible %s has mismatch type.\n", name);
                exit(3);
        }
        else{
            d->assign = 1;
            d->type = t->kids[2]->type;
            if(!strcmp(t->kids[2]->symbolname,"ExprLit")){
                d->show = 1;
                d->model.var.val = t->kids[2]->kids[0]->leaf->val;
            }
        }

    }
    if(d->type == ARRAY_TYPE && d->model.arr.type == NULL_TYPE){
        fprintf(stderr, "\033[0;31m[Error]\033[0m Array %s has no type.\n", name);
        exit(3);
    }else if(d->type == NULL_TYPE){
        fprintf(stderr, "\033[0;31m[Error]\033[0m Varible %s has no type.\n", name);
        exit(3);
    }
    insertEntry(tab, name, d); 
}

// Add varible for control statement if needed
void varCondition(tree *t, table* tab){
    tree *tmp = findTree(t, "IDENT");
    if(tmp == NULL)
        return;

    char *name = tmp->leaf->text;
    entry *e = findEntry(tab, name);
    if(e != NULL)
        return;
    data *d = malloc(sizeof(data));
    d->mut = 1;
    d->assign = 0;
    d->show = 0;
    d->type = INT_TYPE;
    e = insertEntry(tab, name, d); 
    e->data->place = NULL;
}

// Initialize function
void prefn(table* tab, char *name, int type, int nparms, ...){
    data *d = malloc(sizeof(data));
    d->type = FUNC_TYPE;
    d->mut = 0;
    d->show = 0;
    d->model.fn.tree = NULL;
    d->model.fn.table = initTable(tab);; 
    d->model.fn.nparms = nparms;
    d->model.fn.params = malloc(nparms * sizeof(entry *));
    d->place = NULL;
    va_list args;
    va_start(args, nparms);
    for(int i = 0; i < nparms; i++){         
        data *tmp = malloc(sizeof(data));
        d->model.fn.params[i] = insertEntry(d->model.fn.table, va_arg(args, char *), tmp);
        tmp->show = 0;
        tmp->assign = 0;
        tmp->mut = 1;
        tmp->type = va_arg(args, int);
        tmp->place = NULL;
    }
    va_end(args);
    d->model.fn.type = type;
    insertEntry(tab, name, d);
}

//Add build in functions
void stage1(table *tab){
    prefn(tab, "println", NULL_TYPE, 2, "s", STRING_TYPE, "x", NULL_TYPE);
    prefn(tab, "format", STRING_TYPE, 3, "s", STRING_TYPE, "x1", NULL_TYPE, "x2", NULL_TYPE);
    prefn(tab, "read", INT_TYPE, 1, "s", STRING_TYPE);
}

//Add global varibles and functions
void stage2(tree *t, table* tab){
    if(!strcmp(t->symbolname,"ItemConst") || !strcmp(t->symbolname,"ItemStatic"))
        var(t, tab);
    else if(!strcmp(t->symbolname,"ItemFn")){
        char *name = t->kids[0]->kids[0]->leaf->text;
        data *d = malloc(sizeof(data));
        d->type = FUNC_TYPE;
        d->mut = 0;
        d->show = 0;
        d->model.fn.tree = t;
        d->model.fn.table = initTable(tab); 
        d->model.fn.nparms = t->kids[2]->kids[0]->nkids;
        d->model.fn.params = malloc(d->model.fn.nparms * sizeof(entry *));
        d->model.fn.type = NULL_TYPE; 
        d->place = NULL;
        for (int i = 0; i < d->model.fn.nparms; i++){ 
            var(t->kids[2]->kids[0]->kids[i], d->model.fn.table);
            char *nn = findTree(t->kids[2]->kids[0]->kids[i], "IDENT")->leaf->text;
            entry *e = findEntry(d->model.fn.table, nn);
            d->model.fn.params[i] = e;
            }
        if(t->kids[2]->kids[1]->nkids == 0)
            d->model.fn.type = NULL_TYPE;
        else{
            int type = convert(findTree(t->kids[2]->kids[1], "IDENT")->leaf->text);
            if(type == NULL_TYPE){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Function %s has invalid data type. \n", name);
                exit(3);
            }
            d->model.fn.type = type; 
        }        

        insertEntry(tab, name, d);
    }
    if (t->nkids != 0) {
        for (int i = 0; i < t->nkids; i++) 
            stage2(t->kids[i], tab);
    }
};


//Add var and eval expression
void stage3(tree *t, table *tab){
    for (int i = 0; i < t->nkids; i++){
        char *name = t->kids[i]->symbolname;
        if(!strcmp(name,"DeclLocal"))
            var(t->kids[i], tab);
        else if(!strcmp(name,"ExprAssign")){
            char *name = findTree(t->kids[i], "IDENT")->leaf->text;
            entry *e = findEntry(tab, name);
            if(e == NULL){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Var (%s) not declare. \n", name);
                exit(3);
            }
            if(e->data->type != FUNC_TYPE){
                t->type = e->data->type;
            }else{
                fprintf(stderr, "\033[0;31m[Error]\033[0m Incompatible type. Intilized symbol (%s) is function.\n", name);
                exit(3);
            }
            if(!e->data->mut){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Varible (%s) is not mutable.\n", name);
                exit(3);
            }
            epxr(t->kids[i], tab);
            t->kids[i]->type = NULL_TYPE;
        }
        else if(!strcmp(name,"ExprLit")
            || !strcmp(name,"ExprBinary")
            || !strcmp(name,"ExprPath")
            || !strcmp(name,"ExprCall")
            || !strcmp(name,"ExprMac")
            || !strcmp(name,"ExprVec")
            || !strcmp(name,"ExprIndex"))
            epxr(t->kids[i], tab);
        else if(!strcmp(name,"ExprForLoop")){
            varCondition(t->kids[i]->kids[1], tab);
            stage3(t->kids[i]->kids[3]->kids[0], tab);
            }
        else if(!strcmp(name,"ExprWhile")){
            varCondition(t->kids[i]->kids[1], tab);
            stage3(t->kids[i]->kids[2]->kids[0], tab);
            }
        else if(!strcmp(name,"ExprIf")){
            stage3(t->kids[i]->kids[1]->kids[0], tab);
            stage3(t->kids[i], tab);
            if(t->kids[i]->nkids == 3 && !strcmp(t->kids[i]->kids[2]->symbolname,"ExprBlock"))
                stage3(t->kids[i]->kids[2]->kids[0], tab);
        }
    }
};

// Intilize Symbol Table
entry *initSymTab(tree *t){
    entry *e = malloc(sizeof(entry));
    e->name = strdup(filename);
    data *d = malloc(sizeof(data));
    d->type = FUNC_TYPE;
    d->mut = 0;
    d->show = 0;
    d->place = NULL;
    d->model.fn.type = NULL_TYPE;
    d->model.fn.table = initTable(NULL);
    d->model.fn.params = NULL;
    d->model.fn.nparms = 0;
    d->model.fn.tree = t;
    e->data = d;
    e->next = NULL;
    stage1(d->model.fn.table);
    stage2(t, d->model.fn.table);
    for (int i = 0; i < TABLE_SIZE; i++) {
        entry *e = d->model.fn.table->entries[i];
        while (e != NULL) {
            if(e->data->type == FUNC_TYPE && e->data->model.fn.tree != NULL){
                stage3(e->data->model.fn.tree->kids[4]->kids[1], e->data->model.fn.table);
                }
            e = e->next;            
        }
    }
    return e;
}

// Print Symbol Table
void printSymTab(entry *tab) {
    if(tab->data->model.fn.table == NULL)
        return;    
    if(tab->data->model.fn.table->pre != NULL)
        printf("\n");    
    printf("\033[0;35m%s\033[0m\n", tab->name);
    printf("Return Type: \033[0;36m%s\033[0m\n", typename[tab->data->model.fn.type - 1000]);
    printf("Symbol #: %d\n", tab->data->model.fn.table->n);
    for (int i = 0; i < TABLE_SIZE; i++) {
        entry *e = tab->data->model.fn.table->entries[i];
        while (e != NULL) {
            printf("- \033[0;34m[%s]\033[0m \033[0;36m(%s)\033[0m", e->name, typename[e->data->type - 1000]);
            if(e->data->mut)
                printf(" mut");
            if(e->data->type == ARRAY_TYPE)
                printf(" size: %d, type %s", e->data->model.arr.size, typename[e->data->model.arr.type - 1000]);
            if(e->data->show)
                switch (e->data->type) {
                    case INT_TYPE:
                        printf(" %d", e->data->model.var.val.ival);
                        break;
                    case DOUBLE_TYPE:
                        printf(" %f", e->data->model.var.val.dval);
                        break;
                    case STRING_TYPE:
                        printf(" %s", e->data->model.var.val.sval);
                        break; 
                }
            printf("\n");
            e = e->next;            
        }
    }        
    for (int i = 0; i < TABLE_SIZE; i++) {
        entry *e = tab->data->model.fn.table->entries[i];
        while (e != NULL) {
            if(e->data->type == FUNC_TYPE){
                
                printSymTab(e);
                }
            e = e->next;            
        }
    }
}

// Free symbol table
void freeTable(table *t) {
    if (t == NULL)
        return;
    for (int i = 0; i < TABLE_SIZE; i++) {
        entry *e = t->entries[i];
        while (e != NULL) {
            entry *nextEntry = e->next;
            freeEntry(e);
            e = nextEntry;
        }
    }
    free(t->entries);
    free(t);
}

// Free entry in symbol table
void freeEntry(entry *e){
    free(e->name);
    if(e->data->type == FUNC_TYPE){
        freeTable(e->data->model.fn.table);
        if(e->data->model.fn.params != NULL)
            free(e->data->model.fn.params);
    }
    free(e->data);
    free(e);
};