/*
* Three Address Code - skeleton for CSE 423
*/
#ifndef TAC_H
#define TAC_H


typedef struct addr {
  int region;
  int offset;
  char *data;
  int type;
}addr;

typedef struct {
    addr **array;
    size_t used;
    size_t size;
} instrAddr;

extern instrAddr addrs;

/* Regions: */
#define R_GLOBAL 2001 /* can assemble as relative to the pc */
#define R_LOCAL  2002 /* can assemble as relative to the ebp */
#define R_CLASS  2003 /* can assemble as relative to the 'this' register */
#define R_LABEL  2004 /* pseudo-region for labels in the code region */
#define R_CONST  2005 /* pseudo-region for immediate mode constants */
#define R_NAME   2006 /* pseudo-region for source names */
#define R_NONE   2007 
#define R_STRING 2008


typedef struct instr instr;
typedef struct instr {
  int opcode;
  addr *dst, *src1, *src2;
  int label;
  instr *next;
} instr;

typedef struct {
    instr **array;
    size_t used;
    size_t size;
} instrArray;

extern instrArray instrs;

#define O_ADD   3001
#define O_SUB   3002
#define O_MUL   3003
#define O_DIV   3004
#define O_NEG   3005
#define O_ASN   3006
#define O_ADDR  3007
#define O_LCONT 3008
#define O_SCONT 3009
#define O_GOTO  3010
#define O_BLT   3011
#define O_BLE   3012
#define O_BGT   3013
#define O_BGE   3014
#define O_BEQ   3015
#define O_BNE   3016
#define O_BIF   3017
#define O_BNIF  3018
#define O_PARM  3019
#define O_CALL  3020
#define O_RET   3021

#define D_GLOB  3022
#define D_PROC  3023
#define D_LOCAL 3024
#define D_LABEL 3025
#define D_PARAM 3026
#define D_PROT  3027 

typedef struct entry entry;
void initCode(entry *e);
void printCode(entry *e);
void writeCode(entry *e);
void printFCode(entry *e);
void initAddrArray(instrAddr *a);
void initInstrArray(instrArray *a);
void insertAddrArray(instrAddr *a, addr *A);
void insertInstrArray(instrArray *a, instr *i);
void freeAddrArray(instrAddr *a);
void freeInstrArray(instrArray *i);
void freeLit();
char* extractFileName();

typedef struct lit lit;
typedef struct lit {
  addr *place;
  lit *next; 
} lit;

#endif