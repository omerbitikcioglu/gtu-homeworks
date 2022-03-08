//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <sstream>
#include "functions.h"
using namespace std;

void playGame(string board[maxSize][maxSize], int blank_x, int blank_y, int rows, int columns)
{
	char userMove = 'R'; // Random initial value
	int numOfMoves = 0;
	while(userMove !='Q' && userMove != 'q' && !isGameOver(board, rows, columns))
	{
		cout << "Your move:";
		cin >> userMove;
		if(userMove != 'Q' && userMove != 'q')
		{
			makeMove(board, &blank_x, &blank_y, userMove, rows, columns, &numOfMoves);
			printBoard(board, rows, columns);

			if(isGameOver(board, rows, columns))
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

bool isGameOver(string board[maxSize][maxSize], int rows, int columns)
{
	bool isOver = true;
	int tileValue, correctValue = 1;
	string tile;

	for(int i = 0; i < rows; ++i)
	{
		for(int j = 0; j < columns; ++j)
		{
			tile = board[i][j];
			tileValue = stringToInt(tile);
			
			if(tile != "bb" && tile != "00")
			{
				if(tileValue == correctValue) ++correctValue;
				else isOver = false;
			}
			else if(tile == "bb")
			{
				if(!(i == rows - 1 && j == columns - 1)) isOver = false;
			}
		}
	}

	return isOver;
}