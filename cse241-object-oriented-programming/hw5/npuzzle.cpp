//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 5 ~
//	===========================================

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include "npuzzle.h"
using namespace std;

int NPuzzle::Board::numOfBoards = 0; // Static

void NPuzzle::print() const
{
	boardObjs[boardIndex].print();
}

void NPuzzle::printReport()
{
	cout << "Total number of moves " << boardObjs[boardIndex].numberOfMoves() << endl;

	if(boardObjs[boardIndex].isSolved()) 
		cout << "Problem solved!\n";
	else 
		cout << "Problem hasn't solved yet!\n";
}

void NPuzzle::readFromFile(string fileName)
{	
	boardObjs.resize(1);
	boardIndex = 0;
	boardObjs[boardIndex].readFromFile(fileName);
}

void NPuzzle::writeToFile(string fileName)
{
	boardObjs[boardIndex].writeToFile(fileName);
}

void NPuzzle::shuffle(int N)
{
	// Move randomly N times
	char randMove;
	for(int i = 0; i < N; ++i) 
	{
		randMove = boardObjs[boardIndex].pickRandomMove();
		boardObjs[boardIndex].move(randMove);
	}

	// Remove counted moves
	boardObjs[boardIndex].setNumOfMoves(boardObjs[boardIndex].getNumOfMoves()-N);

	// Set the last board obj as the first board obj
	boardObjs[0] = boardObjs[boardIndex];

	// The vector of Board objects is resized to 1
	boardObjs.resize(1);
	boardIndex = 0;	 
}

void NPuzzle::reset()
{
	boardObjs[boardIndex].reset();
	
	// The vector of Board objects is resized to 1
	boardObjs[0] = boardObjs[boardIndex];
	boardObjs.resize(1);
	boardIndex = 0;
}

void NPuzzle::setSize(int rowSize, int columnSize)
{
	if(rowSize <= 9 && rowSize>=0 && columnSize <= 9 && columnSize>=0)
		boardObjs[boardIndex].setSize(rowSize, columnSize);
	else
	{
		cerr << "Invalid board size\n";
		exit(1); 
	}
}

void NPuzzle::setSize(int rowSize)
{
	if(rowSize <= 9 && rowSize>=0)
		boardObjs[boardIndex].setSize(rowSize);
	else
	{
		cerr << "Invalid board size\n";
		exit(1); 
	}
}

void NPuzzle::moveRandom()
{
	char randMove;
	randMove = boardObjs[boardIndex].pickRandomMove();
	move(randMove);
}

void NPuzzle::move(char movep)
{
	switch(movep)
	{
		case 'V':
		case 'v':
			solvePuzzle();
			break;
		case 'T':
		case 't':
			printReport();
			break;
		case 'S':
		case 's':
			shuffle(100);
			// shuffle counts as 1 move
			boardObjs[boardIndex].setNumOfMoves(boardObjs[boardIndex].getNumOfMoves()+1); 
			break;
		case 'E':
		case 'e': // Save Board
		{
			string fileName;
			cout << "Enter file name to save (*.txt):\n";
			cin >> fileName;
			writeToFile(fileName);
			break;
		}
		case 'O':
		case 'o': // Load Board
		{
			string fileName;
			cout << "Enter file name to load (*.txt):\n";
			cin >> fileName;
			readFromFile(fileName);
			break;
		}
		default: // 'L' 'R' 'U' 'D' moves
			boardObjs[boardIndex].move(movep);
			break;
	}
}

void NPuzzle::solvePuzzle()
{
	char moves[] = {'R', 'L', 'U', 'D'};
	
	Board chosenBoard;
	Board newBoard;
	
	bool addIt = true; 
	bool solutionFound = false;

	// Algorithm can stop in two cases: solution is found or there are no elements to take from the vector
	for(auto chosenIndex = 0; !solutionFound && ( chosenIndex < boardObjs.size() ); ++chosenIndex)
	{
		chosenBoard = boardObjs[chosenIndex];

		// Try valid moves
		for(int i = 0; i < 4 && !solutionFound; ++i)
		{
			if(chosenBoard.isValidMove(moves[i]))
			{
				move(moves[i]);
				newBoard = boardObjs[chosenIndex];

				// Check if the same board is already in the vector
				for(auto i = 0; i < boardObjs.size(); ++i)
				{
					if(boardObjs[i] == newBoard && i != chosenIndex)
						addIt = false;
				}

				if(addIt)
				{
					boardObjs[chosenIndex] = chosenBoard;
					boardObjs.push_back(newBoard);
					++boardIndex;
				}
			}
			cout << *this;
			solutionFound = newBoard.isSolved();
		}
	}
}

void NPuzzle::play()
{	
	char userMove = 'R'; // Random initial value

	while(userMove !='Q' && userMove != 'q' && !(boardObjs[boardIndex].isSolved()))
	{
		cout << "Your move:";
		cin >> userMove;
		if(userMove != 'Q' && userMove != 'q')
		{
			if(boardObjs[boardIndex].isValidMove(userMove))
				move(userMove);
			else
				cout << "Invalid move!\n";
			
			print();

			if(boardObjs[boardIndex].isSolved())
			{
				cout << "Problem Solved!\n"
					 << "Total number of moves "
					 << boardObjs[boardIndex].numberOfMoves() << endl;
			}
		}
		else
			cout << "You quit the game\n";
	}
}

ostream& operator <<(ostream& outputStream, const NPuzzle& game)
{
	vector<vector<string> > board = game.boardObjs[game.boardIndex].getBoardConst();
	int h = board.size();
	int w = board[0].size();

	for(int i = 0; i < h; ++i)
	{
		for(int j = 0; j < w; ++j)
		{
			if(board[i][j] == "bb") // Blank tile
				outputStream << "   ";
			else
				outputStream << board[i][j] << " ";
		}
		outputStream << endl;
	}
	outputStream << endl;

	return outputStream;
}

istream& operator >>(istream& inputStream, NPuzzle& gameArg)
{
	gameArg.boardObjs.resize(1);
	gameArg.boardIndex = 0;

	// Check if the stream is available
	if (!inputStream)
	{
	    cerr << "Input stream is unavailable!" << endl;
	    exit(1);
	}

	// Set the sizes 0x0 initially
	gameArg.setSize(0, 0); 
	
	// Read the stream, and fill the board array
	string boardRow, tile;
	for(int i = 0; getline(inputStream, boardRow); ++i)
	{
		gameArg.setSize(i+1);
		
		stringstream s(boardRow);
		for(int j = 0; s >> tile; ++j)
			((gameArg.boardObjs[gameArg.boardIndex]).getBoard())[i].push_back(tile);
	}

	return inputStream;
}

void NPuzzle::Board::print() const
{
	int h = board.size();
	int w = board[0].size();

	cout << "h: " << h << " w: " << w << endl;
	for(int i = 0; i < h; ++i)
	{
		for(int j = 0; j < w; ++j)
		{
			if(board[i][j] == "bb") // Blank tile
				cout << "   ";
			else
				cout << board[i][j] << " ";
		}
		cout << endl;
	}
	cout << endl;
}

void NPuzzle::Board::readFromFile(string fileName)
{
	// Open the file
	ifstream inpFile;
	inpFile.open(fileName);

	// Check if the file is opened
	if (!inpFile)
	{
	    cerr << "Unable to open file " << fileName << endl;
	    exit(1);
	}

	// Set the sizes 0x0 initially
	setSize(0, 0); 
	
	// Read the file, and fill the board array
	string boardRow, tile;
	for(int i = 0; getline(inpFile, boardRow); ++i)
	{
		setSize(i+1);
		
		stringstream s(boardRow);
		for(int j = 0; s >> tile; ++j)
			board[i].push_back(tile);
	}

	inpFile.close();
}

void NPuzzle::Board::writeToFile(string fileName)
{
	// Open the file
	ofstream outpFile;
	outpFile.open(fileName);

	// Check if the file is opened
	if (!outpFile)
	{
	    cerr << "Unable to open file " << fileName << endl;
	    exit(1);
	}

	// Write the board to the file tile by tile
	string tile;
	int h = board.size();
	int w = board[0].size();
	for(int i = 0; i < h; ++i)
	{
		for(int j = 0; j < w; ++j)
		{
			tile = board[i][j];
			outpFile << tile + " ";
		}
		outpFile << endl;
	}

	outpFile.close();	
}

void NPuzzle::Board::reset()
{
	int tileSol;
	string tileStr;

	int blank_x, blank_y;

	int h = board.size();
	int w = board[0].size();

	// Solve the board
	for(int i = 0; i < h; ++i) // Rows
	{
		for(int j = 0; j < w; ++j) // Columns
		{
			if(board[i][j] != "00")
			{   
				// Blank tile will be the last tile
				blank_x = j;
				blank_y = i;

				// Find the solution of the tile
				tileSol = findTileSol(j, i);
				tileStr = to_string(tileSol);

				if(tileSol<10)
					tileStr = "0" + tileStr;
				
				// Write the solution on the tile
				board[i][j] = tileStr;	
			}
		}
	}

	board[blank_y][blank_x] = "bb"; // Set the blank tile as the last tile except "00"s
}

void NPuzzle::Board::setSize(int rowSize, int columnSize)
{
	if(rowSize <= 9 && rowSize>=0 && columnSize <= 9 && columnSize>=0)
	{
		board.resize(rowSize);

		for(int i = 0; i < rowSize; ++i)
			board[i].resize(columnSize);		
	}
	else
	{
		cerr << "Invalid board size\n";
		exit(1); 
	}
}

void NPuzzle::Board::setSize(int rowSize)
{
	if(rowSize <= 9 && rowSize>=0)
		board.resize(rowSize);
	else
	{
		cerr << "Invalid board size\n";
		exit(1); 
	}
}

void NPuzzle::Board::move(char movep)
{	
	// Find the blank tile...
	int blank_x, blank_y;
	findBlankTile(blank_x, blank_y);

	switch(movep)
	{
		// For L, R, U, and D moves, 
		// Swap the blank tile and the desired tile
		// Increase number of moves +1, set the lastMove
		case 'L':
		case 'l':
		{
			string tempTile;
			tempTile = board[blank_y][blank_x-1];
			board[blank_y][blank_x-1] = "bb";
			board[blank_y][blank_x] = tempTile;
			
			lastMove = 'L';
			++numOfMoves;
			break;			
		}

		case 'R':
		case 'r':
		{
			string tempTile;
			tempTile = board[blank_y][blank_x+1];
			board[blank_y][blank_x+1] = "bb";
			board[blank_y][blank_x] = tempTile;
			
			lastMove = 'R';
			++numOfMoves;
			break;		
		}

		case 'U':
		case 'u':
		{
			string tempTile;
			tempTile = board[blank_y-1][blank_x];
			board[blank_y-1][blank_x] = "bb";
			board[blank_y][blank_x] = tempTile;
			
			lastMove = 'U';
			++numOfMoves;
			break;		
		}

		case 'D':
		case 'd':
		{
			string tempTile;
			tempTile = board[blank_y+1][blank_x];
			board[blank_y+1][blank_x] = "bb";
			board[blank_y][blank_x] = tempTile;
			
			lastMove = 'D';
			++numOfMoves;
			break;		
		}
		
		default:
			cout << "Wrong input! Try again.\n";
			break;
	}
}	

// If board is solved toTheRow, it returns true
bool NPuzzle::Board::isSolved() const
{
	bool isSolved = true; // Initial value
	
	int tileValue, tileSol = 1;
	string tile;

	// In solution, all the tiles except 00"s,
	// 	must start from 1 and increase one by one
	int h = board.size();
	int w = board[0].size();
	for(int i = 0; i < h; ++i)
	{
		for(int j = 0; j < w; ++j)
		{
			tile = board[i][j];
			
			if(tile != "00")
			{
				// Convert string to int
				stringstream s(tile);
				s >> tileValue;

				if(tileValue == tileSol) 
					++tileSol;
				else if(tile == "bb") 
					tileSol = 0; // "bb" must be the last tile
				else 
					isSolved = false;
			}
		}
	}

	return isSolved;
}

string NPuzzle::Board::operator()(int x, int y) const
{
	int h = board.size();
	int w = board[0].size();

	if(x >= w || y >= h)
	{
		cerr << "Indices are not valid!\n";
		exit(1);
	}

	string cellContent;
	for(int i = 0; i < h; ++i)
	{
		for(int j = 0; j < w; ++j)
		{
			if(i == y && j == x)
			{
				cellContent = board[i][j]; // Tile is found
				
				// To break the loops
				j = w;
				i = h;
			}
		}
	}

	return cellContent;
}

bool NPuzzle::Board::operator==(const Board& arg) const
{
	bool isEqual = true;

	int h = board.size();
	int w = board[0].size();

	int h_arg = arg.board.size();
	int w_arg = arg.board[0].size();

	if(h != h_arg || w != w_arg) // Sizes must be same
		isEqual = false;
	
	// Tiles must be same
	for(int i = 0; i < h && isEqual; ++i)
	{
		for(int j = 0; j < w && isEqual; ++j)
		{
			if(board[i][j] != arg.board[i][j])
				isEqual = false;				
		}
	}

	return isEqual;
}

char NPuzzle::Board::pickRandomMove()
{
	char moves[4] = {'L', 'R', 'U', 'D'}; // Left , Right, Up and Down moves
	int randMove;

	do{
		randMove = rand()%4; // Pick a random move (L,R,U,D)
	} while(!isValidMove(moves[randMove]));
	
	return moves[randMove]; // Returns which move it picked
}

bool NPuzzle::Board::isValidMove(char move)
{
	bool isValid = true;

	// Find blank_x and blank_y
	int blank_x, blank_y;

	findBlankTile(blank_x, blank_y);

	// Find edges
	int edge_x = board[blank_y].size();
	int edge_y = board.size();

	// Out of the board and "00" tiles are not valid
	switch(move)
	{
		case 'L':
		case 'l':	if(blank_x-1 < 0 || board[blank_y][blank_x-1] == "00")
						isValid = false;
					break;

		case 'U':
		case 'u':	if(blank_y-1 < 0 || board[blank_y-1][blank_x] == "00")
						isValid = false;
					break;

		case 'R':
		case 'r':	if(blank_x+1 >= edge_x || board[blank_y][blank_x+1] == "00") 
						isValid = false;
					break;

		case 'D':
		case 'd':	if(blank_y+1 >= edge_y || board[blank_y+1][blank_x] == "00")
						isValid = false;
					break;
	}

	return isValid;
}

void NPuzzle::Board::findBlankTile(int& blank_x, int& blank_y)
{
	int h = board.size();
	int w = board[0].size();

	// Look for the blank tile...
	for(int i = 0; i < h; ++i)
	{
		for(int j = 0; j < w; ++j)
		{
			if(board[i][j] == "bb") // Blank tile is found
			{
				blank_x = j;
				blank_y = i;

				// To break the loops
				j = w;
				i = h;
			}
		}
	}
}

int NPuzzle::Board::findTileSol(int tile_x, int tile_y) const
{
	int tileSol = 1;
	bool isFound = false;
	string tile;

	// Count until find the desired tile and find its solution
	for(int i = 0; i <= tile_y; ++i)
	{
		for(int j = 0; j < board[i].size() && !isFound; ++j)
		{
			tile = board[i][j];
			if(i==tile_y && j==tile_x) // Found
				isFound = true;
			else if(tile != "00")
				++tileSol;
		}
	}
	return tileSol;
}