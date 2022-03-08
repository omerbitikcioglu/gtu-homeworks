%{
	#include <stdio.h>
	int yylex(void);
	int sym[26]; // symbol table
	void yyerror(const char *s);
%}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_CC OP_COMMA COMMENT VALUE IDENTIFIER

%%

START : INPUT START
	| INPUT
;

INPUT : EXPI { printf("Syntax OK.\nResult: %d\n", $1); }
    | EXPLISTI { printf("Syntax OK.\nResult: %d\n", $1); }
    | EXPB { printf("Syntax OK.\nResult: %d\n", $1); }
;

EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP { $$ = $3 + $4; }
    | OP_OP OP_MINUS EXPI EXPI OP_CP { $$ = $3 - $4; }
    | OP_OP OP_MULT EXPI EXPI OP_CP { $$ = $3 * $4; }
    | OP_OP OP_DIV EXPI EXPI OP_CP { $$ = $3 / $4; }
    | IDENTIFIER { $$ = sym[$1]; }
    | VALUE
    | OP_OP IDENTIFIER EXPLISTI OP_CP
    | OP_OP KW_DEFFUN IDENTIFIER IDLIST EXPLISTI OP_CP
    | OP_OP KW_IF EXPB EXPLISTI OP_CP
    | OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP
    | OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP
    | OP_OP KW_SET IDENTIFIER EXPI OP_CP { sym[$3] = $4; }
;

IDLIST : OP_OP IDS OP_CP
;

IDS : IDS IDENTIFIER
    | IDENTIFIER
;

EXPB : OP_OP KW_AND EXPB EXPB OP_CP { $$ = $3 && $4; }
    | OP_OP KW_OR EXPB EXPB OP_CP { $$ = $3 || $4; }
    | OP_OP KW_NOT EXPB OP_CP { $$ = !$3; }
    | OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = $3 == $4; }
    | OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = $3 == $4; }
    | BinaryValue
;

BinaryValue : KW_TRUE { $$ = 1; }
    | KW_FALSE { $$ = 0; }
;

EXPLISTI : OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP
    | OP_OP KW_APPEND EXPI EXPLISTI OP_CP
    | LISTVALUE
;

LISTVALUE : OP_OP KW_LIST VALUES OP_CP
    | OP_OP KW_LIST OP_CP
    | KW_NIL
;

VALUES : VALUES VALUE
    | VALUE VALUE
;

%%

#include "lex.yy.c"

void yyerror(const char *s)
{
	fprintf(stderr, "%s\n", s);
}

int main()
{
	yyparse();
	return 0;
}