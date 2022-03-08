%option noyywrap
%option yylineno

%{
	#include "y.tab.h"
	int count = 0; // For counting quotation marks
%}

KW_AND 		and
KW_OR 		or
KW_NOT 		not
KW_EQUAL 	equal
KW_LESS 	less
KW_NIL 		nil
KW_LIST 	list
KW_APPEND 	append
KW_CONCAT 	concat
KW_SET 		set
KW_DEFFUN 	deffun
KW_FOR 		for
KW_IF 		if
KW_EXIT 	exit
KW_LOAD		load
KW_DISP  	disp
KW_TRUE 	true
KW_FALSE 	false

OP_PLUS 	\+ 
OP_MINUS 	\-
OP_DIV 		\/
OP_MULT 	\*
OP_OP 		\(
OP_CP 		\)
OP_DBLMULT 	\*\*
OP_QUO		\"
OP_COMMA 	\,

COMMENT 	\;\;.*

VALUE 	([1-9][0-9]*|0)

VALUE_STARTS_WITH_ZERO 	([0][0-9]+)

IDENTIFIER 	([a-z,A-Z][a-z,A-Z,0-9]*)

IDENTIFIER_STARTS_WITH_NUM ([0-9]+[a-z,A-Z]+)

ERROR		.

%%

[\t\n ]+		// Ignore tabs, newlines, and whitespaces
{KW_AND} 		{ return(KW_AND); }
{KW_OR} 		{ return(KW_OR); }
{KW_NOT} 		{ return(KW_NOT); }
{KW_EQUAL} 		{ return(KW_EQUAL); }
{KW_LESS} 		{ return(KW_LESS); }
{KW_NIL} 		{ return(KW_NIL); }
{KW_LIST} 		{ return(KW_LIST); }
{KW_APPEND} 	{ return(KW_APPEND); }
{KW_CONCAT} 	{ return(KW_CONCAT); }
{KW_SET} 		{ return(KW_SET); }
{KW_DEFFUN} 	{ return(KW_DEFFUN); }
{KW_FOR} 		{ return(KW_FOR); }
{KW_IF} 		{ return(KW_IF); }
{KW_EXIT} 		{ return(KW_EXIT); }
{KW_LOAD} 		{ return(KW_LOAD); }
{KW_DISP} 		{ return(KW_DISP); }
{KW_TRUE} 		{ return(KW_TRUE); }
{KW_FALSE} 		{ return(KW_FALSE); }
{OP_PLUS} 		{ return(OP_PLUS); }
{OP_MINUS} 		{ return(OP_MINUS); }
{OP_DIV} 		{ return(OP_DIV); }
{OP_MULT} 		{ return(OP_MULT); }
{OP_OP} 		{ return(OP_OP); }
{OP_CP} 		{ return(OP_CP); }
{OP_DBLMULT} 	{ return(OP_DBLMULT); }
{OP_QUO}		{
					count % 2 == 0 ? printf("OP_OC") : printf("OP_CC"); 
					++count;
				}
{OP_COMMA} 		{ return(OP_COMMA); }

{COMMENT} 		// Ignore comments

{VALUE} 		{ 
					yylval = atoi(yytext); 
					return(VALUE); 
				}

{IDENTIFIER} 	{
					yylval = *yytext - 'a'; 
					return(IDENTIFIER); 
				}

{VALUE_STARTS_WITH_ZERO}		{ printf("Value starts with 0 in line %d, '%s' cannot be tokenized!\n", yylineno, yytext); }
{IDENTIFIER_STARTS_WITH_NUM}	{ printf("Identifier starts with number in line %d, '%s' cannot be tokenized!\n", yylineno, yytext); }
{ERROR}							{ printf("Error in line %d, '%s' cannot be tokenized!\n", yylineno, yytext); }

%%

#ifndef yywrap

int yywrap(void) { return 1; }

#endif