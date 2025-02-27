#define YYDEBUG 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "token.h"
#include "tree.h"
#include "symtab.h"
#include "tac.c"
#include "rustgram.tab.h"

char DOT = 0;
char TREE = 0;
char SYMTAB = 0;
char ASM = 0;
char COMP = 0;

extern int yylex(), yyparse(), yylex_destroy();
void yyerror(const char *s);

extern FILE *yyin;  
char *filename;
TokenArray tokens;
treeArray trees;
tree *root = NULL;
entry *crate = NULL;
instrAddr addrs;
instrArray instrs;

int main(int argc, char *argv[]) {
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-dot") == 0) 
            DOT = 1;
        else if (strcmp(argv[i], "-tree") == 0) 
            TREE = 1;
        else if (strcmp(argv[i], "-symtab") == 0)
            SYMTAB = 1;
        else if (strcmp(argv[i], "-asm") == 0)
            ASM = 1;  
        else if (strcmp(argv[i], "-comp") == 0)
            COMP = 1;
        else {
            if (strncmp(argv[i], "-", 1) == 0){
                fprintf(stderr, "\033[0;31m[Error]\033[0m Argument \"%s\" is invalid. \nUsage: %s [-dot] [-tree] [-symtab] [-asm] [-comp] <input_file>\n", argv[i], argv[0]);
                return 1;
            }
            filename = argv[i];
            FILE *file = fopen(filename, "r");
            char *tmp = strrchr(filename, '/');
            if (tmp != NULL) 
                filename = tmp + 1;            
            if (!file) {
                fprintf(stderr, "\033[0;31m[Error]\033[0m File \"%s\" can't be found. \nUsage: %s [-dot] [-tree] [-symtab] [-asm] [-comp] <input_file>\n", argv[i], argv[0]);
                return 1;
            }
            printf("\033[0;32m[FEC]\033[0m Processing File: %s\n\n", filename);
            initTokenArray(&tokens);
            initTreeArray(&trees);
            initAddrArray(&addrs);
            initInstrArray(&instrs);
            yyin = file;
            yyparse();
            yylex_destroy();
            fclose(file);

            crate = initSymTab(root);

            if (DOT) {
                printDot(root);
            } 

            if(TREE){
                printf("\033[0;34m[Syntax Tree]\033[0m\n");
                printTree(root, 0);
                printf("\n");
            }
            
            if(SYMTAB){
                printf("\033[0;34m[Symbol Table]\033[0m\n");
                printSymTab(crate);
                printf("\n");
            }

            initCode(crate);

            if(ASM){
                printf("\033[0;34m[Symbol Table (Assembly Code)]\033[0m\n");
                printCode(crate);
                printf("\n");
            }

            writeCode(crate);
        
            freeTokenArray(&tokens);
            freeTreeArray(&trees);
            freeEntry(crate);
            freeAddrArray(&addrs);
            freeInstrArray(&instrs);
            freeLit();

            if(COMP){
                char *filename = extractFileName();
                char compile_command[100];
                sprintf(compile_command, "gcc -c %s.c -o %s.o", filename, filename);
                if (system(compile_command) == 0) {
                    char link_command[100];
                    sprintf(link_command, "gcc %s.o -o %s", filename, filename);
                    if (system(link_command) != 0) {
                        fprintf(stderr, "\033[0;31m[Error]\033[0m Linking Failed failed.\n");
                        return 5;
                    }
                } 
                else {
                    fprintf(stderr, "\033[0;31m[Error]\033[0m Compilation failed.\n");
                    return 5;
                }
            }

        }
    }
    printf("\033[0;32m[FEC]\033[0m Finish. No error.\n");
    return 0;
}


