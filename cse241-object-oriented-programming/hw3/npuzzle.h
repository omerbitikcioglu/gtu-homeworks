#ifndef N_PUZZLE_H
#define N_PUZZLE_H

class NPuzzle
{
public:
	// Prints the current configuration on the screen by sending it to cout
	void print();
	// Prints a report about how many moves have been done since reset and
	// if the solution is found
	void printReport();
	// Reads the current configuration from the file given as function parameter. 
	// The file format is defined as in HW2.
	void readFromFile(std::string fileName);
	// Writes the current configuration to the file given as function parameter
	void writeToFile(std::string fileName);
	// Makes N random moves to shuffle the board. N is given as a function parameter.
	void shuffle(int N);
	// Resets the current configuration to the solution.
	void reset();
	// Sets the board size to given values. The values are given as parameters 
	// and they can be at most 9x9. After setting the size, the boards should be reset.
	void setSize(int rowSize=0, int columnSize=0);
	// Makes a valid random move
	void moveRandom();
	// Makes an “intelligent” move
	void moveIntelligent();
	// Makes a move according to the given char parameter. If the parameters is ‘L’ then, 
	// the blank tiles moves left, ..., etc, as defined in HW1.
	void move(char movep);
	// Makes an attempt to solve the puzzle using your own algorithm from HW2.
	void solvePuzzle();
	// Play the game until player types Q or q to quit
	void play();
private:
	class Board
	{
	public:
		void print();
		void readFromFile(std::string fileName);
		void writeToFile(std::string fileName);
		void reset();
		void setSize(int rowSize=0, int columnSize=0);
		void move(char movep);
		bool isSolved(int toTheRow);
		char pickRandomMove();
		char findIntelMove();
		bool isValidMove(char move);
		void setNumOfMoves(int num){numOfMoves=num;}
		int getNumOfMoves(){return numOfMoves;}
	private:
		int findTileSol(int tile_x, int tile_y);
		void intelCheck(int& sub, int tileSol, int tile, char move, char& intelMove);
		std::string board[9][9];
		std::ifstream inpFile;
		std::ofstream outpFile;
		int rows, columns;
		int blank_x, blank_y;
		int numOfMoves;
	};
	Board boardObj;
};

#endif // N_PUZZLE_H