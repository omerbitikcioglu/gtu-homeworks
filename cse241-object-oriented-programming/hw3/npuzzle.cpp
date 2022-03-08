#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "npuzzle.h"
using namespace std;

void NPuzzle::print()
{
	boardObj.print();
}

void NPuzzle::Board::print()
{
	for(int i = 0; i < rows; ++i)
	{
		for(int j = 0; j < columns; ++j)
		{
			if(board[i][j] != "bb")
				cout << board[i][j] << " ";
			else
				cout << "   "; // Print empty when the value is 0
		}
		cout << endl;
	}
	cout << endl;
}

void NPuzzle::printReport()
{
	cout << "Total number of moves " << boardObj.getNumOfMoves() << endl;

	if(boardObj.isSolved(-1)) 
		cout << "Problem solved!\n";
	else 
		cout << "Problem hasn't solved yet!\n";
}

void NPuzzle::readFromFile(string fileName)
{	
	boardObj.readFromFile(fileName);
}

void NPuzzle::Board::readFromFile(string fileName)
{
	// Open the file
	inpFile.open(fileName);

	// Check if the file is opened
	if (!inpFile)
	{
	    cerr << "Unable to open file " << fileName << endl;
	    exit(1);
	}

	setSize(); // Initialize rows and columns as 0
	
	// Read the file, fill the board array, and determine row and column sizes
	string boardRow, tile;
	while(getline(inpFile, boardRow))
	{
		stringstream s(boardRow);
		setSize(rows); // Set columns as 0
		while(s >> tile)
		{
			board[rows][columns] = tile;
			if(tile == "bb") // Blank tile found
			{
				blank_x = columns;
				blank_y = rows;
			}
			++columns;
		}
		++rows;
	}

	inpFile.close();
}

void NPuzzle::writeToFile(string fileName)
{
	boardObj.writeToFile(fileName);
}

void NPuzzle::Board::writeToFile(string fileName)
{
	// Open the file
	outpFile.open(fileName);

	// Check if the file is opened
	if (!outpFile)
	{
	    cerr << "Unable to open file " << fileName << endl;
	    exit(1);
	}

	string tile;
	for(int i = 0; i < rows; ++i)
	{
		for(int j = 0; j < columns; ++j)
		{
			tile = board[i][j];
			outpFile << tile + " ";
		}
		outpFile << endl;
	}

	outpFile.close();	
}

void NPuzzle::shuffle(int N)
{
	char randMove;
	for(int i = 0; i < N; ++i) // Move randomly N times
	{
		randMove = boardObj.pickRandomMove();
		boardObj.move(randMove);
	}
}

void NPuzzle::reset()
{
	boardObj.reset();
}

void NPuzzle::Board::reset()
{
	int tileSol;
	string tileStr;
	for(int i = 0; i < rows; ++i)
	{
		for(int j = 0; j < columns; ++j)
		{
			if(board[i][j] != "00")
			{
				blank_y = i;
				blank_x = j;

				tileSol = findTileSol(i, j);
				tileStr = to_string(tileSol);

				if(tileSol<10)
					tileStr = "0" + tileStr;
				
				board[i][j] = tileStr;	
			}
		}
	}
	board[blank_y][blank_x] = "bb";
}

void NPuzzle::setSize(int rowSize, int columnSize)
{
	boardObj.setSize(rowSize, columnSize);
}

void NPuzzle::Board::setSize(int rowSize, int columnSize)
{
	rows = rowSize;
	columns = columnSize;
}

void NPuzzle::moveRandom()
{
	char randMove;
	randMove = boardObj.pickRandomMove();
	move(randMove);
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

void NPuzzle::moveIntelligent()
{
	char intelMove = 0;
	intelMove = boardObj.findIntelMove();
	int dice = rand()%10+1; // Get a random value between 1-10 (inclusive)
	
	if(intelMove && dice <=7)
	{
		cout << "Intelligent move chooses " << intelMove << endl;
		move(intelMove);	
	}
	else
		moveRandom();
}

char NPuzzle::Board::findIntelMove()
{
	char intelMove = 0;

	int sub = rows*columns-1; // The value bigger than possible biggest difference
	int tileVal; // The value on the desired tile 
	int tileSol; // The value should be on the blank tile

	// Find the integer value should be on the tile
	if(blank_x == columns-1 && blank_y == rows-1) 
		tileSol = 0; // Last tile should be blank
	else
		tileSol = findTileSol(blank_x, blank_y);

	string tile;
	char moves[4] = {'L', 'R', 'U', 'D'};
	for(int i = 0; i < 4; ++i) // This loop finds the intelligent move
	{
		if(isValidMove(moves[i]))
		{
			switch(moves[i])
			{
				case 'L': 	{
								tile = board[blank_y][blank_x-1]; // The value on the left
								stringstream s(tile);
								s >> tileVal;
								if(findTileSol(blank_x-1, blank_y) != tileVal)
									intelCheck(sub, tileSol, tileVal, 'L', intelMove);
								break;
							}
				
				case 'R':	{
								tile = board[blank_y][blank_x+1]; // The value on the right
								stringstream s(tile);
								s >> tileVal;
								if(findTileSol(blank_x+1, blank_y) != tileVal)
									intelCheck(sub, tileSol, tileVal, 'R', intelMove);
								break;
							}
				
				case 'U':	{
								tile = board[blank_y-1][blank_x]; // The value on the up
								stringstream s(tile);
								s >> tileVal;
								if(findTileSol(blank_x, blank_y-1) != tileVal)
									intelCheck(sub, tileSol, tileVal, 'U', intelMove);
								break;
							}

				case 'D':	
							{
								tile = board[blank_y+1][blank_x]; // The value on the down
								stringstream s(tile);
								s >> tileVal;
								if(findTileSol(blank_x, blank_y+1) != tileVal)
									intelCheck(sub, tileSol, tileVal, 'D', intelMove);
								break;
							}
			}
		}
	}
	
	return intelMove;
}

void NPuzzle::Board::intelCheck(int& sub, int tileSol, int tile, char move, char& intelMove)
{
	int newSub = abs(tileSol - tile);
	
	if(newSub < sub)
	{
		sub = newSub; // Closer to solution
		intelMove = move;
	}
}

void NPuzzle::move(char movep)
{
	if(boardObj.isValidMove(movep))
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
			case 'I':
			case 'i':
				moveIntelligent();
				break;
			case 'S':
			case 's':
				reset();
				shuffle(100);
				boardObj.setNumOfMoves(0);
				break;
			default:
				boardObj.move(movep);
				break;
		}
	}
	else
		cout << "Desired tile is not valid!\n";
}

void NPuzzle::Board::move(char movep)
{
	string tempTile; // Value of the tile that will switch

	switch(movep)
	{
		case 'E':
		case 'e':
		{
			string fileName;
			cout << "Enter file name to save (*.txt):\n";
			cin >> fileName;
			writeToFile(fileName);
			break;
		}

		case 'O':
		case 'o':
		{
			string fileName;
			cout << "Enter file name to load (*.txt):\n";
			cin >> fileName;
			readFromFile(fileName);
			break;
		}

		// For L, R, U, and D moves, 
		// Swap the blank tile and the desired tile
		// Increase number of moves +1
		case 'L':
		case 'l':
			tempTile = board[blank_y][blank_x-1];
			board[blank_y][blank_x-1] = "bb";
			board[blank_y][blank_x] = tempTile;
			blank_x = blank_x-1;
			setNumOfMoves(numOfMoves+1);
			break;

		case 'R':
		case 'r':
			tempTile = board[blank_y][blank_x+1];
			board[blank_y][blank_x+1] = "bb";
			board[blank_y][blank_x] = tempTile;
			blank_x = blank_x+1;
			setNumOfMoves(numOfMoves+1);
			break;

		case 'U':
		case 'u':
			tempTile = board[blank_y-1][blank_x];
			board[blank_y-1][blank_x] = "bb";
			board[blank_y][blank_x] = tempTile;
			blank_y = blank_y-1;
			setNumOfMoves(numOfMoves+1);
			break;

		case 'D':
		case 'd':
			tempTile = board[blank_y+1][blank_x];
			board[blank_y+1][blank_x] = "bb";
			board[blank_y][blank_x] = tempTile;
			blank_y = blank_y+1;
			setNumOfMoves(numOfMoves+1);
			break;
		
		default:
			cout << "Wrong input! Try again.\n";
			break;
	}
}	

bool NPuzzle::Board::isValidMove(char move)
{
	bool isValid = true;

	// Out of the board and "00" tiles are not valid
	switch(move)
	{
		case 'L':
		case 'l':	if(blank_x-1 < 0 || board[blank_y][blank_x-1] == "00")
						isValid = false;
					break;

		case 'R':
		case 'r':	if(blank_x+1 >= columns || board[blank_y][blank_x+1] == "00") 
						isValid = false;
					break;

		case 'U':
		case 'u':	if(	blank_y-1 < 0 || 
						board[blank_y-1][blank_x] == "00" ||
						isSolved(blank_y) )
					{
						isValid = false;						
					} 
					break;

		case 'D':
		case 'd':	if(blank_y+1 >= rows || board[blank_y+1][blank_x] == "00")
						isValid = false;
					break;
	}

	return isValid;
}

void NPuzzle::solvePuzzle()
{
	while(!boardObj.isSolved(-1))
	{
		moveIntelligent();
		print();
	}
}

int NPuzzle::Board::findTileSol(int tile_y, int tile_x)
{
	int tileSol = 1;
	bool isFound = false;
	string tile;

	// Count until find the desired tile and find its solution
	for(int i = 0; i <= tile_y; ++i)
	{
		for(int j = 0; j < columns && !isFound; ++j)
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

void NPuzzle::play()
{
	boardObj.setNumOfMoves(0); // Set number of moves as 0 at the beginning of the game
	
	char userMove = 'R'; // Random initial value different from Q or q
	while(userMove !='Q' && userMove != 'q' && !(boardObj.isSolved(-1)))
	{
		cout << "Your move:";
		cin >> userMove;
		if(userMove != 'Q' && userMove != 'q')
		{
			move(userMove);
			print();

			if(boardObj.isSolved(-1))
			{
				cout << "Problem Solved!\n"
					 << "Total number of moves "
					 << boardObj.getNumOfMoves() << endl;
			}
		}
		else
			cout << "You quit the game\n";
	}
}

bool NPuzzle::Board::isSolved(int toTheRow)
{
	if(toTheRow == -1)
		toTheRow = rows;

	bool isSolved = true;
	int tileValue, tileSol = 1;
	
	string tile;

	// In solution, all the tiles except "bb" and "00"s,
	// must start from 1 and increase one by one.
	for(int i = 0; i < toTheRow; ++i)
	{
		for(int j = 0; j < columns; ++j)
		{
			tile = board[i][j];
			stringstream s(tile);
			s >> tileValue; // Convert std::string to int
			
			if(tile != "bb" && tile != "00") // Exception
			{
				if(tileValue == tileSol) ++tileSol;
				else isSolved = false;
			}
		}
	}

	return isSolved;
}