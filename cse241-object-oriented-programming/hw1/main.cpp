//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <iomanip>
#include "functions.h"
using namespace std;

int main()
{
	int problemSize, blank_x, blank_y;
	int board[maxSize][maxSize];

	takeProblemSize(&problemSize, &blank_x, &blank_y);
	
	cout << "Your initial random board is\n";
	initBoard(board, problemSize);
	shuffleBoard(board, problemSize, &blank_x, &blank_y);
	printBoard(board, problemSize);
	playGame(board, problemSize, blank_x, blank_y);
	
	return 0;
}