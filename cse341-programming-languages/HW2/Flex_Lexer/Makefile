gpp_lexer.out: gpp_lexer.c
	gcc -o gpp_lexer.out gpp_lexer.c 

gpp_lexer.c: gpp_lexer.l
	flex -o gpp_lexer.c gpp_lexer.l 

clean: 
	rm *.c *.out parsed_cpp.txt