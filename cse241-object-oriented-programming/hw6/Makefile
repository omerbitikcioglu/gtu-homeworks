output: main.o GTUIterator.o GTUIteratorConst.o GTUSet.o GTUVector.o
	g++ -std=c++11 main.o GTUIterator.o GTUIteratorConst.o GTUSet.o GTUVector.o -o output

main.o: main.cpp
	g++ -std=c++11 -c main.cpp

GTUIterator.o: GTUIterator.cpp GTUIterator.h
	g++ -std=c++11 -c GTUIterator.cpp

GTUIteratorConst.o: GTUIteratorConst.cpp GTUIteratorConst.h
	g++ -std=c++11 -c GTUIteratorConst.cpp

GTUSet.o: GTUSet.cpp GTUSet.h
	g++ -std=c++11 -c GTUSet.cpp

GTUVector.o: GTUVector.cpp GTUVector.h
	g++ -std=c++11 -c GTUVector.cpp

clean: 
	rm *.o output