//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 5 ~
//	===========================================

#ifndef N_PUZZLE_H
#define N_PUZZLE_H

#include <iostream>
#include <string>
#include <vector>
using std::string;
using std::vector;
using std::istream;
using std::ostream;

class NPuzzle
{
public:
	// Default value of boardIndex is 0
	NPuzzle(): boardIndex(0){ boardObjs.resize(1); }

	// Prints the current configuration on the screen by sending it to cout
	void print() const;
	// Prints a report about how many moves have been done since reset and
	// if the solution is found
	void printReport();
	
	// Reads the current configuration from the file given as function parameter. 
	// The file format is defined as in HW2.
	void readFromFile(string fileName);
	
	// Writes the current configuration to the file given as function parameter
	void writeToFile(string fileName);
	
	// Makes N random moves to shuffle the board. N is given as a function parameter.
	void shuffle(int N);
	
	// Resets the current configuration to the solution.
	void reset();
	
	// Sets the board size to given values. The values are given as parameters 
	// and they can be at most 9x9. After setting the size, the boards should be reset.
	void setSize(int rowSize, int columnSize);
	void setSize(int rowSize);
	
	// Makes a valid random move
	void moveRandom();
	
	// Makes a move according to the given char parameter. If the parameters is ‘L’ then, 
	// the blank tiles moves left, ..., etc, as defined in HW1.
	void move(char movep);
	
	// Makes an attempt to solve the puzzle using your own algorithm from HW2.
	void solvePuzzle();
	
	// Play the game until player types Q or q to quit
	void play();

	// Overloading << and >> operators
	friend ostream& operator <<(ostream& outputStream, const NPuzzle& game);
	friend istream& operator >>(istream& inputStream, NPuzzle& game);

private:
	class AbstractBoard // Base class
	{
	public:
		AbstractBoard(char theLastMove, int howManyMoves): lastMove(theLastMove), numOfMoves(howManyMoves){ numOfBoards++; }
		AbstractBoard(): lastMove('S'), numOfMoves(0){ numOfBoards++; }

		void print() const;
		void readFromFile(string fileName);
		void writeToFile(string fileName);
		void reset();
		void setSize(int rowSize, int columnSize);
		void setSize(int rowSize);
		void move(char movep);
		bool isSolved() const;
		
		string operator()(int x, int y) const;
		bool operator==(const Board& arg) const;
		static int numberOfBoards(){ return numOfBoards; }
		char getLastMove(){ return lastMove; }
		int numberOfMoves(){ return numOfMoves; }
		
		char pickRandomMove();
		bool isValidMove(char move);
		void setNumOfMoves(int num){ numOfMoves=num; }
		int getNumOfMoves(){ return numOfMoves; }
		void findBlankTile(int& blank_x, int& blank_y);
		vector<vector<string> >& getBoard(){ return board; }
		const vector<vector<string> >& getBoardConst()const { return board; }

	private:
		int findTileSol(int tile_x, int tile_y) const;
		
		char lastMove;
		int numOfMoves;
		static int numOfBoards;
	};

	class BoardVector : public AbstractBoard
	{
	public:
		BoardVector(char theLastMove, int howManyMoves): AbstractBoard(theLastMove, howManyMoves){/*Blank by design*/}
		BoardVector(): AbstractBoard(){/*Blank by design*/}

	private:
		vector<vector<string> > board;
	};

	class BoardArray1D : public AbstractBoard
	{
	public:
		BoardArray1D(char theLastMove, int howManyMoves): AbstractBoard(theLastMove, howManyMoves){/*Blank by design*/}
		BoardArray1D(): AbstractBoard(){/*Blank by design*/}

	private:
		string *board;
	};

	class BoardArray2D : public AbstractBoard
	{
	public:
		BoardArray2D(char theLastMove, int howManyMoves): AbstractBoard(theLastMove, howManyMoves){/*Blank by design*/}
		BoardArray2D(): AbstractBoard(){/*Blank by design*/}

	private:
		string **board;
	};

	vector<Board> boardObjs;
	int boardIndex;
};

#endif // N_PUZZLE_H