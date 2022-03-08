%option noyywrap

%{
int count = 0; // For counting quotation marks
int lines = 1; // For counting rows
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

VALUE 		([1-9][0-9]*|0)
IDENTIFIER 	([a-z,A-Z][a-z,A-Z,0-9]*)

ERROR 		([0][0-9,a-z,A-Z]*)

%%

{KW_AND} 		{ fprintf(yyout, "KW_AND\n"); }
{KW_OR} 		{ fprintf(yyout, "KW_OR\n"); }
{KW_NOT} 		{ fprintf(yyout, "KW_NOT\n"); }
{KW_EQUAL} 		{ fprintf(yyout, "KW_EQUAL\n"); }
{KW_LESS} 		{ fprintf(yyout, "KW_LESS\n"); }
{KW_NIL} 		{ fprintf(yyout, "KW_NIL\n"); }
{KW_LIST} 		{ fprintf(yyout, "KW_LIST\n"); }
{KW_APPEND} 	{ fprintf(yyout, "KW_APPEND\n"); }
{KW_CONCAT} 	{ fprintf(yyout, "KW_CONCAT\n"); }
{KW_SET} 		{ fprintf(yyout, "KW_SET\n"); }
{KW_DEFFUN} 	{ fprintf(yyout, "KW_DEFFUN\n"); }
{KW_FOR} 		{ fprintf(yyout, "KW_FOR\n"); }
{KW_IF} 		{ fprintf(yyout, "KW_IF\n"); }
{KW_EXIT} 		{ fprintf(yyout, "KW_EXIT\n"); }
{KW_LOAD} 		{ fprintf(yyout, "KW_LOAD\n"); }
{KW_DISP} 		{ fprintf(yyout, "KW_DISP\n"); }
{KW_TRUE} 		{ fprintf(yyout, "KW_TRUE\n"); }
{KW_FALSE} 		{ fprintf(yyout, "KW_FALSE\n"); }
{OP_PLUS} 		{ fprintf(yyout, "OP_PLUS\n"); }
{OP_MINUS} 		{ fprintf(yyout, "OP_MINUS\n"); }
{OP_DIV} 		{ fprintf(yyout, "OP_DIV\n"); }
{OP_MULT} 		{ fprintf(yyout, "OP_MULT\n"); }
{OP_OP} 		{ fprintf(yyout, "OP_OP\n"); }
{OP_CP} 		{ fprintf(yyout, "OP_CP\n"); }
{OP_DBLMULT} 	{ fprintf(yyout, "OP_DBLMULT\n"); }

{OP_QUO}		{ 
					count % 2 == 0 ? fprintf(yyout, "OP_OC\n") : fprintf(yyout, "OP_CC\n"); 
					++count;
				}

{OP_COMMA} 		{ fprintf(yyout, "OP_COMMA\n"); }
{COMMENT} 		{ fprintf(yyout, "COMMENT\n"); }
{VALUE} 		{ fprintf(yyout, "VALUE\n"); }
{IDENTIFIER} 	{ fprintf(yyout, "IDENTIFIER\n"); }
{ERROR}			{ fprintf(yyout, "ERROR in line %d, %s cannot be tokenized!\n", lines, yytext); }
.				{ fprintf(yyout, "ERROR in line %d, %s cannot be tokenized!\n", lines, yytext); }

[\n]			{ ++lines; }

[ \t\n]+		// Ignore whitespaces


%%

int main( int argc, char **argv )
{
	++argv, --argc;  // Skip over program name
	
	if ( argc > 0 )
	        yyin = fopen(argv[0], "r");
	else
	        yyin = stdin;

	yyout = fopen("parsed_cpp.txt", "w");

	yylex();
}