gpp_interpreter: y.tab.c
	gcc -o gpp_interpreter y.tab.c
y.tab.c: gpp_interpreter.y lex.yy.c
	yacc -d gpp_interpreter.y
lex.yy.c: gpp_lexer.l
	flex gpp_lexer.l
clean: 
	rm *.c *.h

