//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <iomanip>
#include "functions.h"
using namespace std;

void takeProblemSize(int *problemSize, int *blank_x, int *blank_y)
{
	do{
		cout << "Please enter the problem size (2-9)\n";
		cin >> *problemSize;
	}while(*problemSize > 9 || *problemSize < 2);

	*blank_x = *problemSize-1;
	*blank_y = *problemSize-1;
}

void initBoard(int board[maxSize][maxSize], int problemSize)
{
	for(int i = 0; i < problemSize; ++i) // Rows
	{
		for(int j = 0; j < problemSize; ++j) // Columns
		{
			board[i][j] = i * problemSize + j + 1;
		}
	}
	board[problemSize-1][problemSize-1] = 0; // Keep last tile empty
}

void shuffleBoard(int board[maxSize][maxSize], int problemSize, int *blank_x, int *blank_y)
{
	int randMove;
	char moves[4] = {'L', 'R', 'U', 'D'}; // Left , Right, Up and Down moves

	for(int i = 0; i < (maxSize*maxSize); ++i) // Move randomly N*N times
	{
		do{
			randMove = rand()%4; // Pick a random move (L,R,U,D)
		}while(!isValidMove(board, *blank_x, *blank_y, moves[randMove], problemSize));

		makeMove(board, blank_x, blank_y, moves[randMove], problemSize);
	}
}

void printBoard(int board[maxSize][maxSize], int problemSize)
{
	for(int i = 0; i < problemSize; ++i)
	{
		for(int j = 0; j < problemSize; ++j)
		{
			if(board[i][j])
				cout << setw(2) << board[i][j] << " ";
			else
				cout << "   "; // Print empty when the value is 0
		}
		cout << endl;
	}
}