#include <stdio.h>
#include <stdlib.h>
#include "tree.h"
#include "symtab.h"
#include "tac.h"
#include "rustgram.tab.h"

char *regionnames[] = {"global", "loc", "class", "lab", "const", "name", "none", "string"};
char *opcodenames[] = {
   "ADD","SUB", "MUL", "DIV", "NEG", "ASN", "ADDR", "LCONT", "SCONT", "GOTO",
   "BLT", "BLE", "BGT", "BGE", "BEQ", "BNE", "BIF", "BNIF", "PARM", "CALL",
   "RETURN"
};
char *opcodesymbol[] = {
   "+","-", "*", "/", "NEG", "ASN", "ADDR", "LCONT", "SCONT", "GOTO",
   "<", "<=", ">", ">=", "==", "!=", "BIF", "BNIF", "PARM", "CALL",
};

int countGlobal, countLoc, countClass, countLab, countConst, countString;
lit *head = NULL;
addr *lab = NULL;
addr *exitlab = NULL;
char *dataname1[] = {"i","f","s"};
char *dataname2[] = {"void", "int", "int","float","char *"};

// Initialize memory address
addr *initAddr(int region, int size, int type){
   addr *a = malloc(sizeof(addr));
   if (a == NULL) {
      fprintf(stderr, "out of memory\n");
      exit(4);
   }
   insertAddrArray(&addrs, a);
   a->region = region;
   switch(region){
      case R_GLOBAL:
         a->offset = countGlobal;
         countGlobal += size;
         break;
      case R_LOCAL:
         a->offset = countLoc;
         countLoc += size;
         break;
      case R_CLASS:
         a->offset = countClass;
         countClass += size;
         break;
      case R_LABEL:
         a->offset = countLab;
         countLab += size;
         break;   
      case R_NONE:
         a->offset = size;
      case R_STRING:
         a->offset = countString;
         countString += size;
         break;
   }
   a->type = type;
   a->data = NULL;
   return a;
}

// Initialize instruction
instr *initInstr(int op, addr *a1, addr *a2, addr *a3){
instr *i = malloc(sizeof (instr));
if (i == NULL) {
   fprintf(stderr, "out of memory\n");
   exit(4);
   }
   insertInstrArray(&instrs, i);
   i->opcode = op;
   i->dst = a1;
   i->src1 = a2;
   i->src2 = a3;
   i->next = NULL;
   return i;
}

// Add instruction
void addInstr(instr **block, instr *i){
   if (*block == NULL) {
      *block = i;
      return;
   }

   instr *tmp = *block;
   while (tmp->next != NULL) {
      tmp = tmp->next;
   }
   tmp->next = i;
}

// Add constant value
addr *addConst(int region, int offset, char *data, int type){
   addr *a = malloc(sizeof(addr));
   if (a == NULL) {
      fprintf(stderr, "out of memory\n");
      exit(4);
   }
   insertAddrArray(&addrs, a);
   a->region = region;
   a->offset = offset;
   a->data = data;
   a->type = type;
   return a;
}

// Add literal value (number or strings)
tree *addLit(tree *t, addr *dst){
   addr *a = NULL;
   if(t->kids[0]->type == BOOL_TYPE){
      a = addConst(R_CONST, 0, t->kids[0]->leaf->text, INT_TYPE);
   }else{
      switch(t->kids[0]->leaf->category) {
         case LIT_INTEGER:
            a = addConst(R_CONST, 0, t->kids[0]->leaf->text, INT_TYPE);
            break;
         case LIT_FLOAT:
            a = addConst(R_CONST, 0, t->kids[0]->leaf->text, DOUBLE_TYPE);
            break;
         case LIT_BYTE:
         case LIT_CHAR:
         case LIT_STR:
         case LIT_STR_RAW:
         case LIT_BYTE_STR:
         case LIT_BYTE_STR_RAW:
            a = addConst(R_STRING, countString++, t->kids[0]->leaf->text, STRING_TYPE);
            lit *l = malloc(sizeof (instr));
            l->next = NULL;
            l->place = a;
            if (head == NULL) 
               head = l;
            else{
               lit *tmp = head;
               while (tmp->next != NULL) 
                  tmp = tmp->next;
               tmp->next = l;
            }
            break;
         case DEFAULT:
            return NULL;
      }
   }
   dst->type = a->type;
   addInstr(&(t->kids[0]->block), initInstr(O_ASN, dst, a, NULL));

   return t->kids[0];
}

tree *op(tree *t, table *tab, addr *a, addr *first, addr *follow);
// Add instuctions to retrieve data
tree *retrieve(tree *t, table *tab, addr *dst, addr *first, addr *follow){
   tree *last = NULL;
   if(!strcmp(t->symbolname,"ExprLit"))
      return addLit(t, dst);
   else if(!strcmp(t->symbolname,"ExprPath") || !strcmp(t->symbolname,"PatLit")){
      last = findTree(t, "IDENT");
      char *name = last->leaf->text;
      entry *e = findEntry(tab, name);
      if(e == NULL){
         data *d = malloc(sizeof(data));
         d->show = 0;
         d->assign = 0;
         d->mut = 1;
         d->type = NULL_TYPE;
         e = insertEntry(tab, name, d); 
         e->data->place = NULL;
         //exit(4);
      }
      if(e->data->place == NULL)
         initAddr(R_LOCAL, 1, e->data->type);
      dst->type = e->data->type;
      addInstr(&(last->block), initInstr(O_ASN, dst, e->data->place, NULL));       
   }else if(!strcmp(t->symbolname,"ExprCall")){
      last = findTree(t, "IDENT");
      char *name = last->leaf->text;
      entry *e = findEntry(tab, name);
      e->data->place->type = dst->type = e->data->model.fn.type;
      if(!strcmp(name, "println") || !strcmp(name, "format")){
         graftTree(t->kids[1], 1, linkTree ("", 0, NULL));  
         char buffer[256] = "";
         strcat(buffer, t->kids[1]->kids[0]->kids[0]->leaf->text);
         int i = 0, n = 1;
         while (i < strlen(t->kids[1]->kids[0]->kids[0]->leaf->text)) {
            if (buffer[i] == '{' && buffer[i+1] == '}') {
               if(n < t->kids[1]->nkids - 1){
                  addr *tmp = initAddr(R_LOCAL, 1, e->data->model.fn.type);
                  t->kids[1]->kids[n+1]->label= initAddr(R_LABEL, 1, NULL_TYPE);
                  last = retrieve(t->kids[1]->kids[n], tab, tmp, t->kids[1]->kids[n+1]->label, t->kids[1]->kids[n+1]->label);
                  buffer[i] = '%';
                  char *result = malloc(32);
                  sprintf(result, ", %s[%d].%s", regionnames[tmp->region - R_GLOBAL], tmp->offset, dataname1[tmp->type - INT_TYPE]);
                  switch(tmp->type){
                     case INT_TYPE:
                        buffer[i + 1] = 'd';
                        strcat(buffer, result);
                        break;
                     case DOUBLE_TYPE:
                        buffer[i + 1]  = 'f';
                        strcat(buffer, result);
                        break;
                     case STRING_TYPE:
                        buffer[i + 1]  = 's';
                        strcat(buffer, result);
                        break;
                  }
                  free(result);
                  n++;
               }
            }
            i++;
         }
         addr *tmp = initAddr(R_NONE, 0, NULL_TYPE);
         tmp->data = strdup(buffer);
         e->data->place->data = (!strcmp(name, "println")) ? strdup("printf") : strdup("sprintf");
         last = t->kids[1]->kids[t->kids[1]->nkids - 1];
         addInstr(&(last->block), initInstr(O_CALL, e->data->place, tmp, dst));
      }else if(!strcmp(name, "read")){
         char *result = malloc(32);
         addr *tmp = initAddr(R_LOCAL, 1, e->data->model.fn.type);
         last = retrieve(t->kids[1]->kids[0], tab, tmp, first, follow);
         sprintf(result, "%s[%d].%s", regionnames[tmp->region - R_GLOBAL], tmp->offset, dataname1[tmp->type - INT_TYPE]);
         addr *a = initAddr(R_NONE, 0, NULL_TYPE);
         a->data = strdup(result);
         e->data->place->data = strdup("atoi");
         addInstr(&(last->block), initInstr(O_CALL, e->data->place, a, dst));
      }
      else{
         for(int i = 0; i < t->kids[1]->nkids; i++){
            t->kids[1]->kids[i]->label= initAddr(R_LABEL, 1, NULL_TYPE);
            addr *tmp = initAddr(R_LOCAL, 1, e->data->model.fn.type);
            last = retrieve(t->kids[1]->kids[i], tab, tmp, first, follow);
            addInstr(&(last->block), initInstr(O_ASN, e->data->model.fn.params[i]->data->place, tmp, NULL));
         }
         graftTree(t->kids[1], 1, linkTree ("", 0, NULL));  
         last = t->kids[1]->kids[t->kids[1]->nkids - 1];
         addInstr(&(last ->block), initInstr(O_CALL, e->data->place, NULL, dst));
      }
   }else if(!strcmp(t->symbolname,"ExprVec")){
      if(!strcmp(t->kids[0]->symbolname,"exprs")){
         for(int i = 0; i < t->kids[0]->nkids; i++){
            addr *a = malloc(sizeof(addr));
            insertAddrArray(&addrs, a);
            a->region = dst->region;
            a->offset = dst->offset+i;
            a->data = NULL;
            addLit(t->kids[0]->kids[i], a);
         }
      }
   }else if(!strcmp(t->symbolname,"ExprIndex")){
      last = t->kids[1]->kids[0];
      char *name = findTree(t, "IDENT")->leaf->text;
      entry *e = findEntry(tab, name);
      addr *src = malloc(sizeof(addr));
      insertAddrArray(&addrs, src);
      src->region = e->data->place->region;
      src->offset = e->data->place->offset + t->kids[1]->kids[0]->leaf->val.ival;
      src->data = NULL;
      src->type = e->data->model.arr.type;
      addInstr(&(last->block), initInstr(O_ASN, dst, src, NULL));
      dst->type = e->data->model.arr.type;
   }else if(!strcmp(t->symbolname,"ExprBinary")){
      return op(t, tab, dst, first, follow);
   }else if(!strcmp(t->symbolname,"ExprUnary")){
      last = retrieve(t->kids[1], tab, dst, first, follow);
      addInstr(&(last->block), initInstr(O_NEG, dst, NULL, NULL));
   }
   return last;
}
// Add isntruction for operations
tree *op(tree *t, table *tab, addr *dst, addr *first, addr *follow){
   addr *src1 = initAddr(R_LOCAL, 1, NULL_TYPE);
   addr *src2 = initAddr(R_LOCAL, 1, NULL_TYPE);
   tree *s1 = NULL, *s2 = NULL;

   if(!strcmp(t->kids[0]->symbolname,"BiOr") || !strcmp(t->kids[0]->symbolname,"BiAnd")){
      if(!strcmp(t->kids[2]->symbolname,"ExprBinary")){
         tree *tmp = t->kids[1];
         t->kids[1] = t->kids[2];
         t->kids[2] = tmp;
      }
      if(!strcmp(t->kids[1]->symbolname,"ExprBinary")){
         t->kids[2]->label = initAddr(R_LABEL, 1, NULL_TYPE);
         if(!strcmp(t->kids[0]->symbolname,"BiOr"))
            s1 = retrieve(t->kids[1], tab, src1, first, t->kids[2]->label);
         else
            s1 = retrieve(t->kids[1], tab, src1, t->kids[2]->label, follow);
      }
      else
         s1 = retrieve(t->kids[1], tab, src1, first, follow);
   }else{
      s1 = retrieve(t->kids[1], tab, src1, first, follow);}
   s2 = retrieve(t->kids[2], tab, src2, first, follow);
   
   if(!strcmp(t->kids[0]->symbolname,"BiAdd"))
      addInstr(&(s2->block), initInstr(O_ADD, dst, src1, src2));
   else if(!strcmp(t->kids[0]->symbolname,"BiSub"))
      addInstr(&(s2->block), initInstr(O_SUB, dst, src1, src2));
   else if(!strcmp(t->kids[0]->symbolname,"BiMul"))
      addInstr(&(s2->block), initInstr(O_MUL, dst, src1, src2));
   else if(!strcmp(t->kids[0]->symbolname,"BiDiv"))
      addInstr(&(s2->block), initInstr(O_DIV, dst, src1, src2));
   else if(!strcmp(t->kids[0]->symbolname,"BiLt")){
      dst->type = INT_TYPE;
      addInstr(&(s2->block), initInstr(O_BLT, dst, src1, src2));
      dst->type = INT_TYPE;
      }
   else if(!strcmp(t->kids[0]->symbolname,"BiLe")){
      addInstr(&(s2->block), initInstr(O_BLE, dst, src1, src2));
      dst->type = INT_TYPE;
      }
   else if(!strcmp(t->kids[0]->symbolname,"BiGt")){
      addInstr(&(s2->block), initInstr(O_BGT, dst, src1, src2));
      dst->type = INT_TYPE;
      }
   else if(!strcmp(t->kids[0]->symbolname,"BiGe")){
      addInstr(&(s2->block), initInstr(O_BGE, dst, src1, src2));
      dst->type = INT_TYPE;
      }
   else if(!strcmp(t->kids[0]->symbolname,"BiEq")){
      addInstr(&(s2->block), initInstr(O_BEQ, dst, src1, src2));
      dst->type = INT_TYPE;
      }
   else if(!strcmp(t->kids[0]->symbolname,"BiNe")){
      addInstr(&(s2->block), initInstr(O_BNE, dst, src1, src2));
      dst->type = INT_TYPE;
      }
   else if(!strcmp(t->kids[0]->symbolname,"BiOr") || !strcmp(t->kids[0]->symbolname,"BiAnd")){
      if(strcmp(t->kids[1]->symbolname,"ExprBinary")){
         if(!strcmp(t->kids[0]->symbolname,"BiOr"))
            addInstr(&(s1->block), initInstr(O_BIF, src1, first, NULL));
         else
            addInstr(&(s1->block), initInstr(O_BIF, src1, follow, NULL));
      }       
      dst->type = INT_TYPE;
      addInstr(&(s2->block), initInstr(O_BIF, src2, first, NULL));
      if(!strcmp(t->kids[0]->symbolname,"BiOr")){
         addInstr(&(t->block), initInstr(O_ASN, dst, addConst(R_CONST, 0, "1", INT_TYPE), NULL));
         addInstr(&(s2->block), initInstr(O_ASN, dst, addConst(R_CONST, 0, "0", INT_TYPE), NULL));
      }
      else{
         addInstr(&(t->block), initInstr(O_ASN, dst, addConst(R_CONST, 0, "0", INT_TYPE), NULL));
         addInstr(&(s2->block), initInstr(O_ASN, dst, addConst(R_CONST, 0, "1", INT_TYPE), NULL));
      }
      addInstr(&(s2->block), initInstr(O_GOTO, follow, NULL, NULL));    
      return s2;
   }
   else if(!strcmp(t->symbolname,"ExprUnary")){
      retrieve(t, tab, dst, first, follow);
   }
   if(dst->type == NULL_TYPE)
      dst->type = src1->type;
   return s2;
}

// Intilize varibles
tree *declare(tree *t, table* tab, int region, addr *lab){
   char *name = findTree(t->kids[0], "IDENT")->leaf->text;
   entry *e = findEntry(tab, name);
      if(e == NULL){
      printf("Declare Fail\n");
      return t;
   }
   addr *a;
   if(e->data->type == ARRAY_TYPE)
      a = initAddr(region, e->data->model.arr.size, e->data->model.arr.type);
   else
      a = initAddr(region, 1, e->data->type);
   e->data->place = a;
   if(t->nkids == 3){
      return retrieve(t->kids[2], tab, a, lab, lab);}
   else return t;
}

// Assign a varible for control statement if it has not be declare
addr *conditionDeclare(tree *t1, tree *t2, table *tab){
   tree *tmp = findTree(t2, "IDENT");
   if(tmp == NULL)
      return initAddr(R_LOCAL, 1, INT_TYPE);;
   char *name = tmp->leaf->text;
   entry *e = findEntry(tab, name);
   if(e == NULL || e->data->place != NULL)
      return e->data->place;
   e->data->place = initAddr(R_LOCAL, 1, INT_TYPE);
   addInstr(&(t1->block), initInstr(O_ASN, e->data->place, addConst(R_CONST, 0, "0", INT_TYPE), NULL));
   return e->data->place;
}

// Add intermediate code for varible, operations, and control statements
tree *travel(tree *t, table* tab){
   tree *last = t;
   if(lab != NULL){
      t->label = lab;
      lab = NULL;
   }   
   if(!strcmp(t->symbolname,"ItemConst") || !strcmp(t->symbolname,"ItemStatic")){
      lab = initAddr(R_LABEL, 1, NULL_TYPE);
      last = declare(t, tab, R_GLOBAL, lab);
   }
   else if(!strcmp(t->symbolname,"DeclLocal")){
      lab = initAddr(R_LABEL, 1, NULL_TYPE);
      last = declare(t, tab, R_LOCAL, lab);
   }
   else if(!strcmp(t->symbolname,"ExprAssign")){
      char *name = findTree(t->kids[0], "IDENT")->leaf->text;
      entry *e = findEntry(tab, name);
      if(!strcmp(t->kids[0]->symbolname,"ExprPath")){
         last = retrieve(t->kids[1], tab, e->data->place, NULL, NULL);
      }else if(!strcmp(t->kids[0]->symbolname,"ExprIndex")){
         addr *dst = malloc(sizeof(addr));
         insertAddrArray(&addrs, dst);
         dst->region = e->data->place->region;
         dst->offset = e->data->place->offset + t->kids[0]->kids[1]->kids[0]->leaf->val.ival;
         dst->data = NULL;
         last = retrieve(t->kids[1], tab, dst, NULL, NULL);
      }
   }
   else if(!strcmp(t->symbolname,"ExprLit") 
      || !strcmp(t->symbolname,"ExprPath") 
      || !strcmp(t->symbolname,"ExprCall") 
      || !strcmp(t->symbolname,"ExprVec") 
      || !strcmp(t->symbolname,"ExprIndex")){
      last = retrieve(t, tab, initAddr(R_LOCAL, 1, NULL_TYPE), NULL, NULL);
   } 
   else if(!strcmp(t->symbolname,"ExprBinary")){
      lab = initAddr(R_LABEL, 1, NULL_TYPE);
      last = op(t, tab, initAddr(R_LOCAL, 1, NULL_TYPE), lab, lab);
      }
   else if(!strcmp(t->symbolname,"ExprIf")){
      if(t->nkids == 2){
         t->kids[1]->label = initAddr(R_LABEL, 1, NULL_TYPE);
         addr *follow;
         if(exitlab == NULL){
            exitlab = initAddr(R_LABEL, 1, NULL_TYPE);
            follow = exitlab;}
         else
            follow = exitlab;
         addr *eval = initAddr(R_LOCAL, 1, NULL_TYPE);
         last = op(t->kids[0], tab, eval, t->kids[1]->label, follow);
         addInstr(&(last->block), initInstr(O_BIF, eval, t->kids[1]->label, NULL));
         addInstr(&(last->block), initInstr(O_GOTO, follow, NULL, NULL));
         last = travel(t->kids[1], tab);
         lab = follow;
      }else if(!strcmp(t->kids[2]->symbolname,"ExprIf")){
         t->kids[1]->label = initAddr(R_LABEL, 1, NULL_TYPE);
         t->kids[2]->label = initAddr(R_LABEL, 1, NULL_TYPE);
         addr *follow;
         if(exitlab == NULL){
            exitlab = initAddr(R_LABEL, 1, NULL_TYPE);
            follow = exitlab;}
         else
            follow = exitlab;
         addr *eval = initAddr(R_LOCAL, 1, NULL_TYPE);
         last = op(t->kids[0], tab, eval, t->kids[1]->label, t->kids[2]->label);
         addInstr(&(last->block), initInstr(O_BIF, eval, t->kids[1]->label, NULL));
         addInstr(&(last->block), initInstr(O_GOTO, t->kids[2]->label, NULL, NULL));
         last = travel(t->kids[1], tab);
         addInstr(&(last->block), initInstr(O_GOTO, follow, NULL, NULL));
         last = travel(t->kids[2], tab);
         lab = follow;
      }else{
         t->kids[1]->label = initAddr(R_LABEL, 1, NULL_TYPE);
         t->kids[2]->label = initAddr(R_LABEL, 1, NULL_TYPE);
         addr *follow;
         if(exitlab == NULL){
            exitlab = initAddr(R_LABEL, 1, NULL_TYPE);
            follow = exitlab;}
         else
            follow = exitlab;
         addr *eval = initAddr(R_LOCAL, 1, NULL_TYPE);
         last = op(t->kids[0], tab, eval, t->kids[1]->label, follow);
         addInstr(&(last->block), initInstr(O_BIF, eval, t->kids[1]->label, NULL));
         addInstr(&(last->block), initInstr(O_GOTO, t->kids[2]->label, NULL, NULL));
         last = travel(t->kids[1], tab);
         addInstr(&(last->block), initInstr(O_GOTO, follow, NULL, NULL));
         last = travel(t->kids[2], tab);
         lab = follow;
      }
      exitlab = NULL;
      return last;
   }   
   else if(!strcmp(t->symbolname,"ExprWhile")){
      conditionDeclare(t->kids[0], t->kids[1], tab);
      t->kids[1]->label = initAddr(R_LABEL, 1, NULL_TYPE);
      t->kids[2]->label = initAddr(R_LABEL, 1, NULL_TYPE);
      addr *follow = initAddr(R_LABEL, 1, NULL_TYPE);
      addr *eval = initAddr(R_LOCAL, 1, NULL_TYPE);
      last = op(t->kids[1], tab, eval, t->kids[2]->label, follow);
      addInstr(&(last->block), initInstr(O_BIF, eval, t->kids[2]->label, NULL));
      addInstr(&(last->block), initInstr(O_GOTO, follow, NULL, NULL));
      last = travel(t->kids[2], tab);
      addInstr(&(last->block), initInstr(O_GOTO, t->kids[1]->label, NULL, NULL));
      lab = follow;
   }
   else if(!strcmp(t->symbolname,"ExprForLoop")){
      t->kids[3]->label = initAddr(R_LABEL, 1, NULL_TYPE);
      addr *follow = initAddr(R_LABEL, 1, NULL_TYPE); 
      addr *num = conditionDeclare(t->kids[0], t->kids[1], tab);
      addr *min = initAddr(R_LOCAL, 1, INT_TYPE);
      addr *max = initAddr(R_LOCAL, 1, INT_TYPE);
      addr *eval = initAddr(R_LOCAL, 1, INT_TYPE);
      addInstr(&(t->kids[2]->kids[0]->block), initInstr(O_ASN, num, min, NULL));
      last = t->kids[3];
      addInstr(&(last->block), initInstr(O_BLT, eval, num, min));
      addInstr(&(last->block), initInstr(O_BIF, eval, follow, NULL));  

      if(!strcmp(t->kids[2]->kids[0]->symbolname,"ExprLit")){
         retrieve(t->kids[2]->kids[0], tab, min, NULL, NULL);
         retrieve(t->kids[2]->kids[1], tab, max, NULL, NULL);
         addInstr(&(last->block), initInstr(O_BGE, eval, num, max));
      }else{
         retrieve(t->kids[2]->kids[0]->kids[0], tab, min, NULL, NULL);
         retrieve(t->kids[2]->kids[1], tab, max, NULL, NULL);
         addInstr(&(last->block), initInstr(O_BGT, eval, num, max));
      }
      addInstr(&(last->block), initInstr(O_BIF, eval, follow, NULL));
      last = travel(t->kids[3], tab);
      addInstr(&(last->block), initInstr(O_ADD, num, num, addConst(R_CONST, 0, "1", INT_TYPE)));   
      addInstr(&(last->block), initInstr(O_GOTO, t->kids[3]->label, NULL, NULL));
      lab = follow;
   }
   else if(!strcmp(t->symbolname,"ExprRet")){
      if(t->nkids == 0)
         addInstr(&(t->block), initInstr(O_RET, NULL, NULL, NULL));
      else{
         addr *tmp = initAddr(R_LOCAL, 1, NULL_TYPE);
         tree *tt = retrieve(t->kids[0], tab, tmp, NULL, NULL);
         if(tt == NULL)
            addInstr(&(t->block), initInstr(O_RET, tmp, NULL, NULL)); 
         else 
            addInstr(&(tt->block), initInstr(O_RET, tmp, NULL, NULL));         
      }
   }
   else if (t->nkids != 0) {
      tree *tmp;
      for (int i = 0; i < t->nkids; i++){
         if(strcmp(t->kids[i]->symbolname,"ItemFn") ){
            tmp = travel(t->kids[i], tab);  
            if(strcmp(t->kids[i]->symbolname,""))
               last = tmp;
         }
      }
   }
   return last;
}

//Initialize Code
void initCode(entry *e){
   travel(e->data->model.fn.tree, e->data->model.fn.table);
   for (int i = 0; i < TABLE_SIZE; i++) {
      entry *tmp = e->data->model.fn.table->entries[i];
      while (tmp != NULL) {
         if(tmp->data->type == FUNC_TYPE){
            tmp->data->place = initAddr(R_CLASS, 1, NULL_TYPE);
            tmp->data->place->type = e->data->model.fn.type;
            tmp->data->place->data = tmp->name;
            if (tmp->data->model.fn.tree != NULL){
               for(int n = 0; n < tmp->data->model.fn.nparms; n++)
                  declare(tmp->data->model.fn.tree->kids[2]->kids[0]->kids[n], tmp->data->model.fn.table, R_LOCAL, NULL);
               graftTree(tmp->data->model.fn.tree->kids[4]->kids[1], 1, linkTree ("", 0, NULL));               
            }
         }
         tmp = tmp->next;            
      }
   }
   for (int i = 0; i < TABLE_SIZE; i++) {
      entry *tmp = e->data->model.fn.table->entries[i];
      while (tmp != NULL) {
         if(tmp->data->type == FUNC_TYPE){
            if (tmp->data->model.fn.tree != NULL){
               tree *t = travel(tmp->data->model.fn.tree, tmp->data->model.fn.table);
               if(tmp->data->model.fn.type != NULL_TYPE && t != NULL){
                  char *name = tmp->data->model.fn.tree->kids[4]->kids[1]->kids[tmp->data->model.fn.tree->kids[4]->kids[1]->nkids - 2]->symbolname;
                  if(!strcmp(name,"ExprLit") || !strcmp(name,"ExprPath") || !strcmp(name,"ExprCall") || !strcmp(name,"ExprIndex") || !strcmp(name,"ExprBinary")){
                     if (t->block != NULL) {
                        instr *i = t->block;
                        while (i->next != NULL) 
                           i = i->next;
                        addInstr(&(t->block), initInstr(O_RET, i->dst, NULL, NULL));
                     }
                  }
               }
            }
         }
         tmp = tmp->next;            
      }
   }
}

// Print Address
void printAddr(addr *a){
   if(a != NULL && a->region != R_NONE){
      if(a->region == R_CONST)
         printf(" (const: %s)", a->data);
      else
         printf(" (%s: %d)", regionnames[a->region - R_GLOBAL], a->offset);
   }

}


// Print a label and instructions
void printBlock(tree *t, int n){
   instr *tmp = t->block;
   if(t->label != NULL){
      for (int i = 0; i < n+1; i++){
         color(i % 8);
         printf("|  ");
      }
      printf("> Label: %d\n", t->label->offset);
   }
   while(tmp != NULL){
      for (int i = 0; i < n + 1; i++){
         color(i % 8);
         printf("|  ");
      }
      color(n+1 % 8);
      printf("- %s",  opcodenames[tmp->opcode - O_ADD]);
      printAddr(tmp->dst);
      printAddr(tmp->src1);
      printAddr(tmp->src2);
      printf("\n");   
      tmp = tmp->next; 
   }
}

// Traverse tree to print code
void printTreeCode(tree *t, int n) {
   for (int i = 0; i < n; i++){
      color(i % 8);
      printf("|  ");
   }
   color(n % 8);
   printf("+-[%s] \033[0m", t->symbolname);

   if (t->nkids == 0) {
      if(t->leaf == NULL)
         printf("NULL\n");
      else
         printf("%s\n", t->leaf->text);
      printBlock(t, n + 1);
   } else {
      printf("\n");
      printBlock(t, n+1);
      for (int i = 0; i < t->nkids; i++) 
         printTreeCode(t->kids[i], n + 1);
   }
}

// Write address or constant
void writeAddr(FILE *file, addr *a){
   if(a == NULL)
      return;
   if(a->type == NULL_TYPE){
      //printf("ADDR NULL\n");
      //exit(4);
   }
   if(a->data != NULL)
      fprintf(file, "%s", a->data);
   else
      fprintf(file, "%s[%d].%s", regionnames[a->region - R_GLOBAL], a->offset, dataname1[a->type - INT_TYPE]);
}

// Create C instuction
addr *writeInstr(FILE *file, instr * i){
   fprintf(file, "    ");
   switch(i->opcode) {
      case O_ADD:
      case O_SUB:      
      case O_MUL:
      case O_DIV:
         writeAddr(file, i->dst);
         fprintf(file, " = ");
         writeAddr(file, i->src1);
         fprintf(file, " %s ", opcodesymbol[i->opcode - O_ADD]);
         writeAddr(file, i->src2);
         fprintf(file, ";\n");
         break;
      case O_NEG:
         writeAddr(file, i->dst);
         fprintf(file, " = -");
         writeAddr(file, i->dst);
         fprintf(file, ";\n");
         break;
      case O_ASN:
         if(i->dst->type == STRING_TYPE){
            fprintf(file, "strcpy(");   
            writeAddr(file, i->dst);
            fprintf(file, ", ");
            writeAddr(file, i->src1);
            fprintf(file, ");\n");
            return i->dst;     
         }
         else{
            writeAddr(file, i->dst);
            fprintf(file, " = ");
            writeAddr(file, i->src1);
            fprintf(file, ";\n");
            return i->dst;        
         }
         break;
      case O_GOTO:
         fprintf(file, "goto label%d;\n", i->dst->offset);
         break;
      case O_BLT:
      case O_BLE:
      case O_BGT:
      case O_BGE:
      case O_BEQ:
      case O_BNE:
         writeAddr(file, i->dst);
         fprintf(file, " = (");
         writeAddr(file, i->src1);
         fprintf(file, " %s ", opcodesymbol[i->opcode - O_ADD]);
         writeAddr(file, i->src2);
         fprintf(file, ") ? 1 : 0;\n");
         break;
      case O_BIF:
         fprintf(file, "if(");
         writeAddr(file, i->dst);
         fprintf(file, ") goto label%d;\n", i->src1->offset);
         break;
      case O_PARM:
         break;
      case O_CALL:
         if(i->src2->type != NULL_TYPE && strcmp(i->dst->data,"sprintf")){
            writeAddr(file, i->src2);
            fprintf(file, " = ");
         }
         writeAddr(file, i->dst);
         fprintf(file, "(");
         if(i->src1 != NULL){
            if(!strcmp(i->dst->data,"sprintf")){
               writeAddr(file, i->src2);
               fprintf(file, ", ");
               }
            fprintf(file, "%s", i->src1->data);
            }
         fprintf(file, ");\n");
         break;
      case O_RET:
         fprintf(file, "return ");
         writeAddr(file, i->dst);
         fprintf(file, ";\n");         
         break;
      default:
         break;
   }
   return NULL;
}

addr *writeTreeFCode(FILE *file, table *tab, tree *t) {
   addr *last = NULL;
   if (!strcmp(t->symbolname, "ItemFn"))
      return last;
   if (t->label != NULL)
      fprintf(file, "\n    label%d:\n", t->label->offset);
   instr *tmp = t->block;
   while (tmp != NULL) {
      last = writeInstr(file, tmp);
      tmp = tmp->next;
   }
   for (int i = 0; i < t->nkids; i++)
      last = writeTreeFCode(file, tab, t->kids[i]);
   return last;
}

// Code C code
void writeCode(entry *e){
   char modified_filename[256]; 
   strcpy(modified_filename, extractFileName());
   strcat(modified_filename, ".c");
   FILE *file = fopen(modified_filename, "w");
   if (file == NULL) {
      printf("Error opening file!");
      exit(4);
   }
   fprintf(file, "#include <stdio.h>\n#include <string.h>\n#include <stdlib.h>\n");
   fprintf(file, "typedef union  {int i; float f; char s[64];} data;\n");
   fprintf(file, "data global[%d]; data loc[%d]; data string[%d];\n", countGlobal, countLoc, countString);

   for (int i = 0; i < TABLE_SIZE; i++) {
      entry *tmp = e->data->model.fn.table->entries[i];
      while (tmp != NULL) {
         if(tmp->data->type == FUNC_TYPE && tmp->data->model.fn.tree != NULL){
            fprintf(file, "\n%s %s(){\n", dataname2[tmp->data->model.fn.type - NULL_TYPE], tmp->name);
            if(!strcmp(tmp->name, "main"))
               writeTreeFCode(file, e->data->model.fn.table, e->data->model.fn.tree);
            graftTree(tmp->data->model.fn.tree->kids[4]->kids[1], 1, linkTree ("", 0, NULL));
            addr *last = NULL;
            last = writeTreeFCode(file, tmp->data->model.fn.table, tmp->data->model.fn.tree->kids[4]->kids[1]);
            if(last != NULL){
               fprintf(file, "    return ");
               writeAddr(file, last);
               fprintf(file, ";\n");       
            }
            fprintf(file, "}\n");
            }
         tmp = tmp->next;            
      }
   }
   fclose(file);
}

// Print C code
void printCode(entry *e){
   int i = 0;
   lit *tmp = head;
   while (tmp != NULL) {
      printf(".string %d: %s\n", tmp->place->offset, tmp->place->data);
      i++;
      tmp = tmp->next;
   }
   printTreeCode(e->data->model.fn.tree, 0);
}

// Initialize address array
void initAddrArray(instrAddr *a) {
   a->array = (addr **)malloc(sizeof(addr *));
   if (a->array == NULL) {
      fprintf(stderr, "Memory allocation failed\n");
      exit(EXIT_FAILURE);
   }
   a->used = 0;
   a->size = 1;
}

// Initialize instruction array
void initInstrArray(instrArray *a) {
   a->array = (instr **)malloc(sizeof(instr *));
   if (a->array == NULL) {
      fprintf(stderr, "Memory allocation failed\n");
      exit(EXIT_FAILURE);
   }
   a->used = 0;
   a->size = 1;
}

// Add address to array
void insertAddrArray(instrAddr *a, addr *A) {
   if (a->used == a->size) {
      a->size *= 2;
      a->array = (addr **)realloc(a->array, a->size * sizeof(addr *));
      if (a->array == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
      }
   }
   a->array[a->used++] = A;
}


// Add instruction to array
void insertInstrArray(instrArray *a, instr *i) {
   if (a->used == a->size) {
      a->size *= 2;
      a->array = (instr **)realloc(a->array, a->size * sizeof(instr *));
      if (a->array == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
      }
   }
   a->array[a->used++] = i;
}

// Clear address array
void freeAddrArray(instrAddr *a){
   for (size_t i = 0; i < a->used; ++i) {
      if((a->array[i]) != NULL)
         free(a->array[i]);
      a->array[i] = NULL;
   }
   free(a->array);
   a->array = NULL;
   a->used = a->size = 0;
}

// Clear instruction array
void freeInstrArray(instrArray *a){
   for (size_t i = 0; i < a->used; ++i) {
      if((a->array[i]) != NULL)
         free(a->array[i]);
   a->array[i] = NULL;
   }
   free(a->array);
   a->array = NULL;
   a->used = a->size = 0;
}

// Clear lit array
void freeLit(){
   lit *current = head;
   while (current != NULL) {
      lit *next = current->next;
      free(current); 
      current = next;
   }
}

// Return file name
char* extractFileName() {
   char* fileName = strrchr(filename, '/');
   if(fileName != NULL) {
      fileName++;
   } else {
      fileName = filename;
   }
   char* extension = strchr(fileName, '.');
   if(extension != NULL) {
      *extension = '\0';
   }
   return fileName;
}