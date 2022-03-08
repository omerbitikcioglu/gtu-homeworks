//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <iomanip>
#include "functions.h"
using namespace std;

void playGame(int board[maxSize][maxSize], int problemSize, int blank_x, int blank_y)
{
	char userMove = 'R'; // Random initial value
	int numOfMoves = 0;
	while(userMove !='Q' && userMove != 'q' && !isGameOver(board, problemSize))
	{
		cout << "Your move:";
		cin >> userMove;
		if(userMove != 'Q' && userMove != 'q')
		{
			makeMove(board, &blank_x, &blank_y, userMove, problemSize);
			printBoard(board, problemSize);

			if(userMove != 'S' && userMove != 's') ++numOfMoves;
			else numOfMoves = 0; // If user shuffles the game, numOfMoves will reset

			if(isGameOver(board, problemSize))
			{
				cout << "Problem Solved!\n"
					 << "Total number of moves "
					 << numOfMoves << endl;
			}
		}
		else
			cout << "You quit the game\n";
	}
}

bool isGameOver(int board[maxSize][maxSize], int problemSize)
{
	bool isOver = true;

	for(int i = 0; i < problemSize; ++i)
	{
		for(int j = 0; j < problemSize; ++j)
		{
			if(board[i][j] != (i * problemSize + j + 1))
			{
				if(!(i == problemSize-1 && j == problemSize-1))
					isOver = false; // The game is not over yet
			}
		}
	}

	return isOver;
}