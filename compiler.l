%{
#include <string.h>
int n = 0;
int i;
extern struct info* yylval;
%}
%option noyywrap
delim	[ \t\n]
ws	{delim}+
letter	[A-Za-z]
digit	[0-9]
hexdigit [0-9a-fA-F]
hex_literal	0[x|X]{hexdigit}+
id	{letter}({letter}|{digit})*
decimal_literal	{digit}+(\.{digit}+)?(E[+-]?{digit}+)?
stringl	\"[^\"\n]*\"
charl	'.'

%%

{ws}
"\""
"\\"
"class"	{ return CLASS; }
"Program"	{ return PROGRAM; }
"void"	{return VOID; }
"if"	{ return IF; }
"while"	{ return WHILE; }
"do"	{ return DO; }
"else"	{ return ELSE; }
"for"	{ return FOR;}
"return"	{ return RETURN; }
"break"	{ return BREAK; }
"continue" {return CONTINUE; }
"true"	{Init(&yylval); strcpy(yylval->lexeme, yytext); return bool_literal; }
"false"	{Init(&yylval); strcpy(yylval->lexeme, yytext);  return bool_literal; }
"callout"	{ return CALLOUT; }
"int"	{ return INT; }
"boolean"	{ return BOOL;}
{hex_literal}	{Init(&yylval); strcpy(yylval->lexeme, yytext); return hex_literal; }
{decimal_literal}	{Init(&yylval); strcpy(yylval->lexeme, yytext); return decimal_literal; }
{id}	{Init(&yylval); strcpy(yylval->lexeme, yytext); return id;}
"="	{Init(&yylval); strcpy(yylval->lexeme, yytext); return assign_op; }
"+="	{Init(&yylval); strcpy(yylval->lexeme, yytext); return assign_op; }
"-="	{Init(&yylval); strcpy(yylval->lexeme, yytext); return assign_op; }
"+"	{Init(&yylval); strcpy(yylval->lexeme, yytext); return arith_op; }
"-"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return yytext[0]; }
"*"	{Init(&yylval); strcpy(yylval->lexeme, yytext); return arith_op; }
"/"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return arith_op; }
"%"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return arith_op;}
"<"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return rel_op; }
">"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return rel_op; }
"<="	{Init(&yylval); strcpy(yylval->lexeme, yytext);return rel_op; }
">="	{Init(&yylval); strcpy(yylval->lexeme, yytext);return rel_op; }
"=="	{Init(&yylval); strcpy(yylval->lexeme, yytext);return eq_op; }
"!="	{Init(&yylval); strcpy(yylval->lexeme, yytext);return eq_op; }
"&&"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return cond_op; }
"||"	{Init(&yylval); strcpy(yylval->lexeme, yytext);return cond_op; }
";"	{return yytext[0]; }
"("	{return yytext[0]; }
")"	{return yytext[0]; }
"["	{return yytext[0]; }
"]"	{return yytext[0]; }
"{"	{return yytext[0]; }
"}"	{return yytext[0]; }
","	{return yytext[0]; }
"!"	{return yytext[0]; }


{stringl}	{Init(&yylval); strcpy(yylval->lexeme, yytext);  return string_literal; }
{charl}	{Init(&yylval); strcpy(yylval->lexeme, yytext);return char_literal;  }

%%
