//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <sstream>
#include "functions.h"
using namespace std;

void makeMove(string board[maxSize][maxSize], int *blank_x, int *blank_y, char move, int rows, int columns, int *numOfMoves)
{
	string temp; // Value of the tile that will switch
	char intelMove = 0;
	char randMove;

	switch(move)
	{
		case 'L':
		case 'l':
			if(*blank_x-1 >= 0 && board[*blank_y][*blank_x-1] != "00") // Desired tile is valid?
			{
				temp = board[*blank_y][*blank_x-1];
				board[*blank_y][*blank_x-1] = "bb";
				board[*blank_y][*blank_x] = temp;
				*blank_x = *blank_x-1;
				++*numOfMoves;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'R':
		case 'r':
			if(*blank_x+1 < columns && board[*blank_y][*blank_x+1] != "00") // Desired tile is valid?
			{
				temp = board[*blank_y][*blank_x+1];
				board[*blank_y][*blank_x+1] = "bb";
				board[*blank_y][*blank_x] = temp;
				*blank_x = *blank_x+1;
				++*numOfMoves;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'U':
		case 'u':
			if(*blank_y-1 >= 0 && board[*blank_y-1][*blank_x] != "00") // Desired tile is valid?
			{
				temp = board[*blank_y-1][*blank_x];
				board[*blank_y-1][*blank_x] = "bb";
				board[*blank_y][*blank_x] = temp;
				*blank_y = *blank_y-1;
				++*numOfMoves;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'D':
		case 'd':
			if(*blank_y+1 < rows && board[*blank_y+1][*blank_x] != "00") // Desired tile is valid?
			{
				temp = board[*blank_y+1][*blank_x];
				board[*blank_y+1][*blank_x] = "bb";
				board[*blank_y][*blank_x] = temp;
				*blank_y = *blank_y+1;
				++*numOfMoves;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'S':
		case 's':
		{
			shuffleBoard(board, rows, columns, blank_x, blank_y);
			*numOfMoves = 0;
			break;
		}

		case 'I':
		case 'i':
		{
			intelMove = findIntelMove(board, *blank_x, *blank_y, rows, columns);
			int dice = rand()%10+1; // Get a random value between 1-10
			if(intelMove && dice <= 7)
			{
				cout << "Intelligent move chooses " << intelMove << endl;
				makeMove(board, blank_x, blank_y, intelMove, rows, columns, numOfMoves);	
			}
			else
			{
				randMove = pickRandomMove(board, blank_x, blank_y, rows, columns);	
				cout << "Intelligent move chooses " << randMove << endl;
				makeMove(board, blank_x, blank_y, randMove, rows, columns, numOfMoves);
			}
			break;
		}

		case 'V':
		case 'v':
		{
			while(!isGameOver(board, rows, columns))
			{
				makeMove(board, blank_x, blank_y, 'I', rows, columns, numOfMoves);
				printBoard(board, rows, columns);
			}
			break;
		}

		case 'T':
		case 't':
		{
			cout << "Total number of moves " << *numOfMoves << endl;
			
			if(isGameOver(board, rows, columns)) 
				cout << "Problem solved!\n";
			else 
				cout << "Problem hasn't solved yet!\n";
			
			break;
		}

		case 'E':
		case 'e':
			saveBoard(board, rows, columns);
			break;

		case 'Y':
		case 'y':
			rows = 0;
			columns = 0;
			loadBoard(board, blank_x, blank_y, &rows, &columns);
			break;
		
		default:
			cout << "Wrong input! Try again.\n";
			break;
	}
}

bool isValidMove(string board[maxSize][maxSize], int blank_x, int blank_y, char move, int rows, int columns)
{
	bool isValid = true;

	switch(move)
	{
		case 'L':	if(blank_x-1 < 0 || board[blank_y][blank_x-1] == "00")
						isValid = false;
					break;

		case 'R':	if(blank_x+1 >= columns || board[blank_y][blank_x+1] == "00") 
						isValid = false;
					break;

		case 'U':	if(blank_y-1 < 0 || board[blank_y-1][blank_x] == "00") 
						isValid = false;
					break;

		case 'D':	if(blank_y+1 >= rows || board[blank_y+1][blank_x] == "00")
						isValid = false;
					break;
	}

	return isValid;
}

char findIntelMove(string board[maxSize][maxSize], int blank_x, int blank_y, int rows, int columns)
{
	char intelMove = 0;
	char moves[4] = {'L', 'R', 'U', 'D'};

	int sub = maxSize*maxSize-1; // The value bigger than possible biggest difference
	int tileVal; // The value on the desired tile 
	int tileSol; // The value should be on the blank tile

	// Find the integer value should be on the tile
	if(blank_x == columns-1 && blank_y == rows-1) 
		tileSol = 0; // Last tile should be blank
	else
		tileSol = findTileSol(board, blank_x, blank_y, rows, columns);

	string tile;
	for(int i = 0; i < 4; ++i) // This loop finds the intelligent move
	{
		if(isValidMove(board, blank_x, blank_y, moves[i], rows, columns))
		{
			switch(moves[i])
			{
				case 'L': 	tile = board[blank_y][blank_x-1];
							tileVal = stringToInt(tile);
							if(findTileSol(board, blank_x-1, blank_y, rows, columns) != tileVal)
								intelCheck(&sub, tileSol, tileVal, 'L', &intelMove);
							break;

				case 'R':	tile = board[blank_y][blank_x+1];
							tileVal = stringToInt(tile);
							if(findTileSol(board, blank_x+1, blank_y, rows, columns) != tileVal)
								intelCheck(&sub, tileSol, tileVal, 'R', &intelMove);
							break;

				case 'U':	tile = board[blank_y-1][blank_x];
							tileVal = stringToInt(tile);
							if(findTileSol(board, blank_x, blank_y-1, rows, columns) != tileVal)
								intelCheck(&sub, tileSol, tileVal, 'U', &intelMove);
							break;

				case 'D':	tile = board[blank_y+1][blank_x];
							tileVal = stringToInt(tile);
							if(findTileSol(board, blank_x, blank_y+1, rows, columns) != tileVal)
								intelCheck(&sub, tileSol, tileVal, 'D', &intelMove);
							break;
			}
		}
	}
	
	return intelMove;
}

int findTileSol(string board[maxSize][maxSize], int tile_x, int tile_y, int rows, int columns)
{
	int tileSol = 1;
	bool exit = false;
	string tile;

	// Count until find blank tile and find its correct value
	for(int i = 0; i < rows && !exit; ++i)
	{
		for(int j = 0; j < columns && !exit; ++j)
		{
			tile = board[i][j];
			if(i==tile_y && j==tile_x) // Reached the desired tile
				exit = true;
			else if(tile != "00")
				++tileSol;
		}
	}
	return tileSol;
}

void intelCheck(int *sub, int tileSol, int tile, char move, char *intelMove)
{
	int newSub = abs(tileSol - tile);
	
	if(newSub < *sub)
	{
		*sub = newSub; // Closer to solution
		*intelMove = move;
	}
}

int stringToInt(string str)
{
	int intVal;

	stringstream s(str);
	s >> intVal;

	return intVal;
}

char pickRandomMove(string board[maxSize][maxSize], int *blank_x, int *blank_y, int rows, int columns)
{
	char moves[4] = {'L', 'R', 'U', 'D'}; // Left , Right, Up and Down moves
	int randMove;

	do{
		randMove = rand()%4; // Pick a random move (L,R,U,D)
	} while(!isValidMove(board, *blank_x, *blank_y, moves[randMove], rows, columns));
	
	return moves[randMove]; // Returns which move it picked
}