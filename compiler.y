%{
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>



int yylex(void);
void yyerror(char *);

enum dataType { integer, boolean, invalid, incomplete, voidtype};
//invalid - symbols that do not have a type
//incomplete - symbols whose type is not known yet


struct symbol {
	char name[256];
	enum dataType type;
	int isArray;
	int nElem; //if array, no of elements
	struct symbol* next;
};

struct symtab {
	struct symbol* symbols;
	struct symtab* next;
};

struct symtab* symStack = NULL; //points to top of symbol table stack


	

struct quadtab
{
	int idx;
	char opcode[256];
	struct symbol* src1;
	struct symbol* src2;
	struct symbol* dst;
	struct quadtab* next;
	
	
};

struct quadtab* quads = NULL; //list of quadruples
struct quadtab* quadTail = NULL;

struct backpatchList
{
	struct quadtab* quad;
	struct backpatchList* next;
};

struct argList
{
	struct symbol* arg;
	struct argList* next;
};

struct info {
	struct symbol* sym;
	struct symbol* offset;
	char lexeme[256];
	enum dataType type;
	struct backpatchList* truelist;
	struct backpatchList* falselist;
	struct backpatchList* nextlist;
	struct backpatchList* brklist; //for break statements
	struct backpatchList* cntlist; //for continue statements
	struct argList* args; //for call statements
	
	
};

#define YYSTYPE struct info*

int IncLabel();

int GetLabel();

struct symbol* InstallLabel();

void Backpatch(struct backpatchList* l, struct symbol* s);

void InsertBackpatch(struct backpatchList** x, struct backpatchList* y);

void InsertTarget(struct backpatchList** x, struct quadtab* y);

void InsertArg(struct argList** x, struct symbol* y);

void InsertArgList(struct argList** x, struct argList* y);

struct symbol* GenParams (struct argList* x);

void PrintQuads();

void PushSymTab();

void PopSymTab();

struct symbol* AddSym (char* name, enum dataType ty, int isArray, int nElem);

void UpdateType(enum dataType ty);

struct symbol* GenSym(enum dataType ty, int isArray, int nElem);

struct quadtab* GenQuad(char* opcode, struct symbol* src1, struct symbol* src2, struct symbol* dst) ;

struct info* Init (struct info** x);

void PrintSymbols();

struct symbol* FindSymbol(char* lexeme);

%}

%token hex_literal
%token id
%token string_literal
%token char_literal
%token decimal_literal
%token bool_literal
%token assign_op arith_op eq_op rel_op cond_op
%token CLASS PROGRAM VOID IF ELSE FOR RETURN BREAK CONTINUE CALLOUT INT BOOL


%right '='
%left '+' '-'
%left '*' '/'
%right UMINUS


%%

program	:	CLASS PROGRAM '{' global_decls '}'	{	PrintQuads();   }	
	;

global_decls	:	field_decls
		|	field_decls method_decls
		|	method_decls
		|
		;




field_decls	:	type id next_field_decls ';' 	{AddSym($2->lexeme, $1->type, 0, 0); UpdateType($1->type);}	
		|	type id '[' int_literal ']' next_field_decls ';'	{ AddSym($2->lexeme, $1->type, 1, atoi($4->sym->name));  UpdateType($2->type);}	
		|	type id assign_op literal M ';' { struct symbol* s = AddSym($2->lexeme, $1->type, 0, 0);  GenQuad("=", $4->sym, NULL, s); 
							  if ($1->type == boolean) {
								Backpatch($4->truelist, $5->sym);
								Backpatch($4->falselist, $5->sym);
							  }
							}
		|	field_decls type id next_field_decls ';' {AddSym($2->lexeme, $1->type, 0, 0); UpdateType($1->type);}
		|	field_decls type id '[' int_literal ']' next_field_decls ';' { AddSym($2->lexeme, $1->type, 1, atoi($4->sym->name));  UpdateType($2->type);}	
		|	field_decls type id assign_op literal M ';' {  struct symbol* s = AddSym($3->lexeme, $2->type, 0, 0);  GenQuad("=", $5->sym, NULL, s); 
								      if ($2->type == boolean) {
									    Backpatch($5->truelist, $6->sym);
									    Backpatch($5->falselist, $6->sym);
								      }
								    }
		;



next_field_decls	: next_field_decls ',' id	{ AddSym($3->lexeme, incomplete, 0, 0); }
			| next_field_decls ',' id '[' int_literal ']' { AddSym($3->lexeme, incomplete, 1, atoi($5->sym->name)); }
			|
			;


method_decls	:	type id 	{	struct symbol* s = AddSym($2->lexeme, $1->type, 0, 0); 
						PushSymTab(); 
						GenQuad("function", NULL, NULL, s); } '('  method_args ')' block  { PopSymTab(); }	
		|	VOID id		{	struct symbol* s = AddSym($2->lexeme, voidtype, 0, 0); 
						PushSymTab(); 
						GenQuad("function", NULL, NULL, s); } '('  method_args ')' block { PopSymTab(); }
		|	method_decls type id	{	struct symbol* s = AddSym($3->lexeme, $2->type, 0, 0); 
							PushSymTab(); 
							GenQuad("function", NULL, NULL, s); } '(' method_args ')' block { PopSymTab(); }
		|	method_decls VOID id 	{	struct symbol* s = AddSym($3->lexeme, voidtype, 0, 0); 
							PushSymTab(); 
							GenQuad("function", NULL, NULL, s); } '('  method_args ')' block  { PopSymTab(); }
		;


method_args	:	type id next_method_args	{ AddSym($2->lexeme, $1->type, 0, 0);	}	
		|	 
		;


next_method_args	:	',' type id next_method_args	{ AddSym($3->lexeme, $2->type, 0, 0); }
			|	
			;



block	:	 '{'  {PushSymTab(); } var_decls statements '}' 	{  	Init(&$$);   
								   	InsertBackpatch(&($$->nextlist), $4->nextlist); 
									InsertBackpatch(&($$->brklist), $4->brklist);
									InsertBackpatch(&($$->cntlist), $4->cntlist);
									PopSymTab(); 
								}								
		;


var_decls	:	 var_decls type id next_ids ';'	{	AddSym($3->lexeme, $2->type, 0, 0); 
								UpdateType($2->type); }
		|	
		;

next_ids	:	',' id next_ids	{	AddSym($2->lexeme, incomplete, 0, 0);}
		|	
		;
statements	:	statements M statement	{	Init (&$$); 
							Backpatch($1->nextlist, $2->sym);
							InsertBackpatch(&($$->nextlist), $3->nextlist);
							InsertBackpatch(&($$->brklist), $1->brklist);
							InsertBackpatch(&($$->brklist), $3->brklist);
							InsertBackpatch(&($$->cntlist), $1->cntlist);
							InsertBackpatch(&($$->cntlist), $3->cntlist);

							
						}
		|	{ Init (&$$); }
		;



statement	:	location assign_op expr ';' 	{ 		Init(&$$);
									if ($1->sym->type == boolean) {
										struct symbol* st = FindSymbol("true");
										if (!st) st = AddSym("true", boolean, 0, 0);
										struct symbol* sf = FindSymbol("false");
										if (!sf) sf = AddSym("false", boolean, 0, 0);
										struct symbol* s1 = InstallLabel();
										struct quadtab* q1 = NULL;
										if (!$1->offset) q1 = GenQuad("=", st, NULL, $1->sym);
										else {
											q1 = GenQuad("[]*", st, $1->offset, $1->sym);
										}
										struct quadtab* q2 = GenQuad("goto", NULL, NULL, NULL);
										struct symbol* s3 = InstallLabel();
										struct quadtab* q3 = NULL;
										if (!$1->offset) q3 = GenQuad("=", sf, NULL, $1->sym);
										else {
											q3 = GenQuad("[]*", sf, $1->offset, $1->sym);
										}
										Backpatch($3->truelist, s1);
										Backpatch($3->falselist, s3);
										InsertTarget(&($$->nextlist), q2);
									} else if (strcmp($2->lexeme, "=") == 0) {
										if (!$1->offset) GenQuad("=", $3->sym, NULL, $1->sym);
										else GenQuad("[]*", $3->sym, $1->offset, $1->sym);
									} else if (strcmp($2->lexeme, "+=") == 0) {
										if (!$1->offset) GenQuad("+", $1->sym, $3->sym, $1->sym);
										else {
											struct symbol* sr = GenSym(integer, 0, 0);
											GenQuad("[]", $1->sym, $1->offset, sr);
											GenQuad("+", sr, $3->sym, sr);
											GenQuad("[]*", sr, $1->offset, $1->sym);
										}
									} else {
										if (!$1->offset) GenQuad("-", $1->sym, $3->sym, $1->sym);
										else {
                                                                                        struct symbol* sr = GenSym(integer, 0, 0);
                                                                                        GenQuad("[]", $1->sym, $1->offset, sr);
                                                                                        GenQuad("-", sr, $3->sym, sr);
                                                                                        GenQuad("[]*", sr, $1->offset, $1->sym);
                                                                                }

									}
							}
		|	method_call ';'			{ $$ = $1; }
		|	IF '(' expr ')' M block	{			Init (&$$); 
									Backpatch($3->truelist, $5->sym); 
									InsertBackpatch(&($$->nextlist), $3->falselist); 
									InsertBackpatch(&($$->nextlist), $6->nextlist);
									InsertBackpatch(&($$->brklist), $6->brklist);
									InsertBackpatch(&($$->cntlist), $6->cntlist);
						}
		|	IF '(' expr ')' M block  N ELSE M block	{	Init (&$$); 
									Backpatch($3->truelist, $5->sym); 
									Backpatch($3->falselist, $9->sym); 
									InsertBackpatch(&($$->nextlist), $6->nextlist);
									InsertBackpatch(&($$->nextlist), $7->nextlist);
									InsertBackpatch(&($$->nextlist), $10->nextlist);
									InsertBackpatch(&($$->brklist), $6->brklist);
									InsertBackpatch(&($$->cntlist), $6->cntlist);
									InsertBackpatch(&($$->brklist), $10->brklist);
									InsertBackpatch(&($$->cntlist), $10->cntlist);
									
								}
		|	FOR  id  assign_op expr {GenQuad("=", $4->sym, NULL, FindSymbol($2->lexeme));} 
			',' M expr {
				struct symbol* c = GenSym(integer, 0, 0);
				GenQuad ("<", FindSymbol($2->lexeme), $8->sym, c);
				struct quadtab* j1 = GenQuad ("if", c, NULL, NULL);	
				struct quadtab* j2 = GenQuad ("ifFalse", c, NULL, NULL);
				InsertTarget(&($8->truelist), j1);
				InsertTarget(&($8->falselist), j2);
				}
			M block		
									{	Init (&$$);
										Backpatch($8->truelist, $10->sym);
										struct symbol* l = InstallLabel();
										struct symbol* one = FindSymbol("1");
										if (!one) one = AddSym("1", integer, 0, 0);
										GenQuad("+", FindSymbol($2->lexeme), one, FindSymbol($2->lexeme));
										GenQuad("goto", NULL, NULL, $7->sym);
										Backpatch($11->nextlist, l);
										Backpatch($11->cntlist, l);
										InsertBackpatch(&($$->nextlist), $8->falselist);
										InsertBackpatch(&($$->nextlist), $11->brklist);
									}
		|	RETURN ';'	{	GenQuad("ret", NULL, NULL, NULL); }	
		|	RETURN expr ';' 	 { GenQuad("ret", NULL, NULL, $2->sym);	}
		|	BREAK ';'	{	Init(&$$); 
						struct quadtab* q = GenQuad ("goto", NULL, NULL, NULL); 
						InsertTarget(&($$->brklist), q);}	
		|	CONTINUE ';'	{	Init(&$$); 
						struct quadtab* q = GenQuad ("goto", NULL, NULL, NULL); 
						InsertTarget(&($$->cntlist), q);}	
		|	block		{	$$ = $1; }


		;
method_call	:	id '(' call_args ')'	{ 	Init(&$$);
							struct symbol* nArgs = GenParams($3->args);
							struct symbol* f = FindSymbol($1->lexeme);
							if (f->type != voidtype) $$->sym = GenSym(f->type, 0, 0);
							GenQuad("call", f, nArgs , $$->sym);
						}
		|	CALLOUT '(' string_literal callout_args ')'	{ 	Init(&$$);
										struct symbol* nArgs = GenParams($4->args);
										struct symbol* f = FindSymbol($3->lexeme);
										if (!f) f = AddSym($3->lexeme, integer, 0, 0);
										$$->sym = GenSym(f->type, 0, 0);
										GenQuad("call", f, nArgs , $$->sym);
									}	
		;

call_args	:	expr next_call_args { Init (&$$); InsertArg(&($$->args), $1->sym);  InsertArgList(&($$->args), $2->args); }
		|	{Init (&$$); }
		;

next_call_args	:	next_call_args ',' expr 		{ Init (&$$); InsertArgList(&($$->args), $1->args); 
							  	  InsertArg(&($$->args), $3->sym); }
		|						{ Init (&$$); }
		;
callout_args	:	callout_args ',' string_literal 	{ 	Init(&$$);
									struct symbol* s = FindSymbol ($3->lexeme); 
									if (!s) s = AddSym($3->lexeme, voidtype, 0, 0); 
									InsertArgList(&($$->args), $1->args);
									InsertArg(&($$->args), s);
								}
		|	callout_args ',' expr 		{ Init (&$$); InsertArgList(&($$->args), $1->args); 
							  InsertArg(&($$->args), $3->sym); }
		|	{ Init (&$$); }
		;

location	:	id	{   Init (&$$); $$->sym = FindSymbol($1->lexeme); }	
		|	id '[' expr ']'	{	Init(&$$); 
						struct symbol* t1 = FindSymbol("8");
						if (t1 == NULL) t1 = AddSym("8", integer, 0, 0);
						struct symbol* t2 = GenSym(integer, 0, 0); 
						GenQuad("*", $3->sym, t1, t2);
						$$->sym = FindSymbol($1->lexeme);
						$$->offset = t2;	}
		;

expr	:	location	{	Init(&$$);  
					if ($1->offset) {
						$$->sym = GenSym($1->sym->type, 0, 0);
						GenQuad("[]", $1->sym, $1->offset, $$->sym);
					} else {
						$$->sym = $1->sym;
					}
					if ($1->sym->type == boolean) {
						struct quadtab* q1 = GenQuad("if", $1->sym, NULL, NULL);
						InsertTarget(&($$->truelist), q1);
						struct quadtab* q2 = GenQuad("ifFalse", $1->sym, NULL, NULL);
						InsertTarget(&($$->falselist), q2);
					}
					
				}		
	|	method_call	{	$$ = $1; }	
	|	'-' expr %prec UMINUS	{	Init (&$$); 
						if (FindSymbol("-1") == NULL) AddSym("-1", integer, 0, 0); 
						$$->sym = GenSym(integer, 0, 0); 
						GenQuad("*", FindSymbol("-1"), $2->sym, $$->sym);	}
	|	expr bin_op M expr	{
						Init (&$$);
						$$->sym = GenSym(integer, 0, 0); 
						if (strcmp($2->lexeme, "%") == 0) {
							struct symbol* quotient = GenSym(integer, 0, 0);
							GenQuad("/", $1->sym, $4->sym, quotient);
							struct symbol* product = GenSym(integer, 0, 0);
							GenQuad("*", $4->sym, quotient, product);
							GenQuad("-", $1->sym, product, $$->sym);
						} else if (strcmp($2->lexeme, "&&") == 0) {
							Backpatch($1->truelist, $3->sym);
							InsertBackpatch(&($$->truelist), $4->truelist);
							InsertBackpatch(&($$->falselist), $1->falselist);
							InsertBackpatch(&($$->falselist), $4->falselist);
						} else if (strcmp($2->lexeme, "||") == 0) {
							Backpatch($1->falselist, $3->sym);
							InsertBackpatch(&($$->truelist), $1->truelist);
							InsertBackpatch(&($$->truelist), $4->truelist);
							InsertBackpatch(&($$->falselist), $4->falselist);
						} else if ((strcmp($2->lexeme, "<") == 0) || (strcmp($2->lexeme, ">") == 0) || (strcmp($2->lexeme, "<=") == 0) || (strcmp($2->lexeme, ">=") == 0) 
								|| (strcmp($2->lexeme, "==") == 0) || (strcmp($2->lexeme, "!=") == 0))
						{
							GenQuad($2->lexeme, $1->sym, $4->sym, $$->sym);
							struct quadtab* q1 = GenQuad("if", $$->sym, NULL, NULL);
							struct quadtab* q2 = GenQuad("ifFalse", $$->sym, NULL, NULL);
							InsertTarget(&($$->truelist), q1);
							InsertTarget(&($$->falselist), q2);
					
						} else {  
							GenQuad($2->lexeme, $1->sym, $4->sym, $$->sym);
						}
					}
	|	'!' expr		{ 	Init (&$$);
					 	InsertBackpatch(&($$->truelist), $2->falselist);
						InsertBackpatch(&($$->falselist), $2->truelist);
					}
	|	'(' expr ')'		{	$$ = $2;	}
	|	literal			{	$$ = $1; 	}
	;

bin_op	:	arith_op	{	Init (&$$); strcpy($$->lexeme, $1->lexeme); }
	|	'-'		{	Init (&$$); strcpy($$->lexeme, $1->lexeme); }
	|	rel_op		{	Init (&$$); strcpy($$->lexeme, $1->lexeme); }	
	|	eq_op		{	Init (&$$); strcpy($$->lexeme, $1->lexeme); }
	|	cond_op		{	Init (&$$); strcpy($$->lexeme, $1->lexeme); }	
	;

literal	:	int_literal			{ $$ = $1; }		
	|	char_literal			{ Init (&$$); $$->sym = FindSymbol($1->lexeme); if (!($$->sym)) $$->sym = AddSym($1->lexeme, integer, 0, 0);}
	|	bool_literal			{ Init (&$$); $$->sym = FindSymbol($1->lexeme); if (!($$->sym)) $$->sym = AddSym($1->lexeme, boolean, 0, 0);
						  if (strcmp($1->lexeme, "true") == 0) {
							struct quadtab* q = GenQuad("goto", NULL, NULL, NULL);
							InsertTarget(&($$->truelist), q);
							
						  } else {
							struct quadtab* q = GenQuad("goto", NULL, NULL, NULL);
							InsertTarget(&($$->falselist), q);
						  }  
						  
						}
	;

int_literal	:	decimal_literal		{ Init (&$$); $$->sym = FindSymbol($1->lexeme); if (!($$->sym)) $$->sym = AddSym($1->lexeme, integer, 0, 0); }
		|	hex_literal		{ Init (&$$); $$->sym = FindSymbol($1->lexeme); if (!($$->sym)) $$->sym = AddSym($1->lexeme, integer, 0, 0); }
		;

type	:	INT	{ Init(&$$);	$$->type = integer;	}
	|	BOOL	{	Init(&$$);	$$->type = boolean;	}
	;		
		
M	:	{	Init (&$$);  $$->sym = InstallLabel();}
	;

N	:	{	Init (&$$); struct quadtab* q = GenQuad("goto", NULL, NULL, NULL); 
			InsertTarget(&($$->nextlist), q); }

%%
#include "lex.yy.c"



int quadid = 1;

int IncLabel()
{
	quadid ++;
	return quadid;
}

int GetLabel()
{
	return quadid;
}

struct symbol* InstallLabel()
{
	char label[256]; 
	sprintf(label, "L%d", GetLabel()); 
	return(AddSym(label, integer, 0, 0));
}

void Backpatch(struct backpatchList* l, struct symbol* s)
{
	struct backpatchList* b = l;
	while (b != NULL)
	{
		//printf("backpatch L%d to %s\n", b->quad->idx, s->name);
		b->quad->dst = s;
		b = b->next;
	}
}//Backkpatch

void InsertBackpatch(struct backpatchList** x, struct backpatchList* y)
{
	

	if (*x == NULL) {  *x = y;  return; }
	struct backpatchList* b = *x;
	while (b->next != NULL)
	{
		
		b = b->next;
	}

	b->next = y;
	
}//InsertBackpatch

void InsertTarget(struct backpatchList** x, struct quadtab* y)
{
	if (*x == NULL)
	{
		*x = (struct backpatchList*)malloc(sizeof(struct backpatchList));
		(*x)->quad = y;
		(*x)->next = NULL;
		return;
	}

	struct backpatchList* z = (struct backpatchList*)malloc(sizeof(struct backpatchList));
	z->quad = y;
	z->next = *x;

	*x = z;
}//InsertTarget

void InsertArg(struct argList** x, struct symbol* y)
{
	struct argList* z = (struct argList*) malloc (sizeof (struct argList));
	z->arg = y;
	z->next = *x;

	*x = z;
	
}//InsertArg

void InsertArgList(struct argList** x, struct argList* y)
{
	if (*x == NULL) 	{	
		*x = y;
		return;
	}

	struct argList* z = *x;
	while (z->next != NULL) {
		z = z->next;
	}
	z->next = y;
}//InsertArgList

struct symbol* GenParams (struct argList* x)
{
	int nArgs = 0;
	struct argList* z = x;
	while (z != NULL) {
		GenQuad("param", NULL, NULL, z->arg);
		nArgs ++;
		z = z->next;
	}
	char a[256]; sprintf(a, "%d", nArgs);
	struct symbol* s = FindSymbol(a);
	if (!s) s = AddSym(a, integer, 0, 0);
	return s;
}//GenParams



struct symbol* FindSymbol(char* lexeme)
{
	struct symtab* s = symStack;
	while (s != NULL) {
		struct symbol* sym = s->symbols;
		while (sym != NULL)
		{
				if (strcmp(lexeme, sym->name) == 0) return sym;
				sym = sym->next;
		}	
		s = s->next;
	}
	return NULL;
}

void PrintQuads()
{
	
	struct quadtab* q = quads;
	
	while (q != NULL)
	{

		if (strcmp(q->opcode, "=") == 0) printf("L%d: %s = %s\n", q->idx, q->dst->name, q->src1->name);
		else if (strcmp(q->opcode, "if") == 0) printf("L%d: if %s goto %s\n", q->idx, q->src1->name, q->dst->name);				
		else if (strcmp(q->opcode, "ifFalse") == 0) printf("L%d: ifFalse %s goto %s\n", q->idx, q->src1->name, q->dst->name);
		else if (strcmp(q->opcode, "goto") == 0) printf("L%d: goto %s\n", q->idx, q->dst->name);
		else if (strcmp(q->opcode, "function") == 0)  printf("%s:\n", q->dst->name);
		else if ((strcmp(q->opcode, "ret") == 0) && (q->dst)) printf("L%d: ret %s\n", q->idx, q->dst->name);
		else if (strcmp(q->opcode, "ret") == 0) printf("L%d: ret\n", q->idx);
		else if ((strcmp(q->opcode, "call") == 0) && (q->dst)) printf("L%d: %s = call %s, %s\n", q->idx, q->dst->name, q->src1->name, q->src2->name);
		else if (strcmp(q->opcode, "call") == 0) printf("L%d: call %s, %s\n", q->idx, q->src1->name, q->src2->name);
		else if (strcmp(q->opcode, "param") == 0) printf("L%d: param %s\n", q->idx, q->dst->name);
		else if (strcmp(q->opcode, "[]") == 0) printf("L%d: %s = %s[%s]\n", q->idx, q->dst->name, q->src1->name, q->src2->name);
		else if (strcmp(q->opcode, "[]*") == 0) printf("L%d: %s[%s] = %s\n", q->idx, q->dst->name, q->src2->name, q->src1->name);

		else if (q->src2 == NULL) printf("L%d: %s = %s %s\n", q->idx, q->dst->name, q->opcode, q->src1->name);
		else printf ("L%d: %s = %s %s %s\n", q->idx, q->dst->name, q->src1->name, q->opcode, q->src2->name);
		
		q = q->next;
		
	}
	printf ("\n\n");
}//PrintQuads

void PushSymTab() //push new symbol table to symbol table stack
{
	struct symtab* s = (struct symtab*) malloc(sizeof( struct symtab));

	

	s->next = symStack;
	symStack = s;

}//PushSymTab

void PopSymTab() //pop from symbol table stack
{
	symStack = symStack->next;
	


}//PushSymTab

struct symbol* AddSym (char* name, enum dataType ty, int isArray, int nElem)
{
	struct symbol* var = (struct symbol*) malloc(sizeof( struct symbol));
	strcpy(var->name, name);
	var->type = ty;
	var->isArray = isArray;
	var->nElem = nElem;
	


	var->next = symStack->symbols;
	symStack->symbols = var;
	
	return var;

}//AddSym

void UpdateType(enum dataType ty)
{
	struct symbol* s = symStack->symbols;
	while (s != NULL)
	{
		if (s->type == incomplete) s->type = ty;
		s = s->next;
	}
}//UpdateType




struct symbol* GenSym(enum dataType ty, int isArray, int nElem)
{
	static int tempid = 0;
	tempid ++;
	struct symbol* temp = (struct symbol*) malloc (sizeof( struct symbol));
	sprintf(temp->name, "t%d", tempid);
	temp->type = ty;
	temp->isArray = isArray;
	temp->nElem = nElem;
	temp->next = symStack->symbols;
	symStack->symbols = temp;
	return temp;
}//GenSym

struct quadtab*	GenQuad(char* opcode, struct symbol* src1, struct symbol* src2, struct symbol* dst) 
{
	int quadid = GetLabel();
	struct quadtab* q = (struct quadtab*) malloc(sizeof( struct quadtab));
	strcpy(q->opcode, opcode);
	q->src1 = src1;
	q->src2 = src2;
	q->dst = dst;
	q->idx = quadid;
	q->next = NULL;

	if (quads == NULL)
	{
		quads = q;
		quadTail = q;
	}
	else
	{
		quadTail->next = q;
		quadTail = q;
	}

	IncLabel();
	return q;
	
}//GenQuad

struct info* Init (struct info** x)
{
	*x = (struct info*) malloc (sizeof(struct info));
	(*x)->sym = NULL;
	(*x)->offset = NULL;
	(*x)->type = invalid;
	strcpy((*x)->lexeme, "");
	(*x)->truelist = NULL;
	(*x)->falselist = NULL;
	(*x)->nextlist = NULL;
	(*x)->brklist = NULL;
	(*x)->cntlist = NULL;
	(*x)->args = NULL;
	return *x;
}//Init



void PrintSymbols()
{

	printf("SYMBOLS BEGIN:\n");

	struct symtab* s = symStack;
	while (s != NULL) {
		struct symbol* sym = s->symbols;
		while (sym != NULL)
		{
				if (sym->type == integer) printf("%s integer\n", sym->name);
				if (sym->type == boolean) printf("%s boolean\n", sym->name);
				if (sym->type == incomplete) printf("%s incomplete\n", sym->name);
				if (sym->type == invalid) printf("%s invalid\n", sym->name);
				sym = sym->next;
		}	
		s = s->next;
	}
	printf("SYMBOLS END\n");
}


void yyerror (char *s) 
{
	printf("error: %s\n", s);   
}

int main(void) 
{
	//yylval = (char*) malloc(1024 * sizeof(char));
	symStack = (struct symtab*) malloc (sizeof(struct symtab));
	yyparse();
	return 0;
}















