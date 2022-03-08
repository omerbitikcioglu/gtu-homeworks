output: main.o npuzzle.o
	g++ -std=c++11 main.o npuzzle.o -o output

main.o: main.cpp npuzzle.h
	g++ -std=c++11 -c main.cpp

npuzzle.o: npuzzle.cpp npuzzle.h
	g++ -std=c++11 -c npuzzle.cpp
	
clean: 
	rm *.o output
