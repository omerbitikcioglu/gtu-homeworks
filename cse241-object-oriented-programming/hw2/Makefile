output: main.o move.o board.o game.o
	g++ main.o move.o board.o game.o -o output

main.o: main.cpp functions.h
	g++ -c main.cpp

move.o: move.cpp functions.h
	g++ -c move.cpp
	
board.o: board.cpp functions.h
	g++ -c board.cpp
	
game.o: game.cpp functions.h
	g++ -c game.cpp
	
clean: 
	rm *.o output
