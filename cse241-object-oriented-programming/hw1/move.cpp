//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <iomanip>
#include "functions.h"
using namespace std;

void makeMove(int board[maxSize][maxSize], int *blank_x, int *blank_y, char move, int problemSize)
{
	int temp; // Value of the tile that will switch
	char intelMove = 0;

	switch(move)
	{
		case 'L':
		case 'l':
			if(*blank_x-1 >= 0) // Desired tile is valid?
			{
				temp = board[*blank_y][*blank_x-1];
				board[*blank_y][*blank_x-1] = 0;
				board[*blank_y][*blank_x] = temp;
				*blank_x = *blank_x-1;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'R':
		case 'r':
			if(*blank_x+1 < problemSize) // Desired tile is valid?
			{
				temp = board[*blank_y][*blank_x+1];
				board[*blank_y][*blank_x+1] = 0;
				board[*blank_y][*blank_x] = temp;
				*blank_x = *blank_x+1;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'U':
		case 'u':
			if(*blank_y-1 >= 0) // Desired tile is valid?
			{
				temp = board[*blank_y-1][*blank_x];
				board[*blank_y-1][*blank_x] = 0;
				board[*blank_y][*blank_x] = temp;
				*blank_y = *blank_y-1;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'D':
		case 'd':
			if(*blank_y+1 < problemSize) // Desired tile is valid?
			{
				temp = board[*blank_y+1][*blank_x];
				board[*blank_y+1][*blank_x] = 0;
				board[*blank_y][*blank_x] = temp;
				*blank_y = *blank_y+1;
			}
			else {
				cout << "Desired tile is not valid!\n";
			}
			break;

		case 'S':
		case 's':
			initBoard(board, problemSize);
			*blank_x = problemSize-1;
			*blank_y = problemSize-1;
			shuffleBoard(board, problemSize, blank_x, blank_y);
			break;

		case 'I':
		case 'i':
			intelMove = findIntelMove(board, *blank_x, *blank_y, problemSize);
			if(intelMove){
				cout << "Intelligent move chooses " << intelMove << endl;
				makeMove(board, blank_x, blank_y, intelMove, problemSize);
			}
			else
				cout << "Can't do intelligent move\n";
			break;

		default:
			cout << "Wrong input! Try again.\n";
			break;
	}
}

bool isValidMove(int board[maxSize][maxSize], int blank_x, int blank_y, char move, int problemSize)
{
	bool isValid = true;

	switch(move)
	{
		case 'L':	if(blank_x-1 < 0) // Desired tile is valid?
						isValid = false;
					break;

		case 'R':	if(blank_x+1 >= problemSize) // Desired tile is valid?
						isValid = false;
					break;

		case 'U':	if(blank_y-1 < 0) // Desired tile is valid?
						isValid = false;
					break;

		case 'D':	if(blank_y+1 >= problemSize) // Desired tile is valid?
						isValid = false;
					break;
	}

	return isValid;
}

char findIntelMove(int board[maxSize][maxSize], int blank_x, int blank_y, int problemSize)
{
	char intelMove = 0;
	char moves[4] = {'L', 'R', 'U', 'D'};

	int sub = 9999; // Random initial value
	int tile; // Value of the tile
	int tileSol;

	// Find the value should be on the tile
	if(blank_x == problemSize-1 && blank_y == problemSize-1) // Last tile
		tileSol = 0;
	else
		tileSol = (blank_y * problemSize + blank_x + 1); // Other tiles

	for(int i = 0; i < 4; ++i) // This loop finds the intelligent move
	{
		if(isValidMove(board, blank_x, blank_y, moves[i], problemSize))
		{
			switch(moves[i])
			{
				case 'L': 	tile = board[blank_y][blank_x-1];
							if(abs(tileSol - tile) < sub){
								sub = abs(tileSol - tile);
								intelMove = 'L';
							}
							break;

				case 'R':	tile = board[blank_y][blank_x+1];
							if(abs(tileSol - tile) < sub){
								sub = abs(tileSol - tile);
								intelMove = 'R';
							}
							break;

				case 'U':	tile = board[blank_y-1][blank_x];
							if(abs(tileSol - tile) < sub){
								sub = abs(tileSol - tile);
								intelMove = 'U';
							}
							break;

				case 'D':	tile = board[blank_y+1][blank_x];
							if(abs(tileSol - tile) < sub){
								sub = abs(tileSol - tile);
								intelMove = 'D';
							}
							break;
			}
		}
	}
	
	return intelMove;
}
