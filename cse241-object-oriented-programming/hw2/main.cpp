//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <string>
#include "functions.h"
using namespace std;

int main()
{
	int blank_x, blank_y, rows=0, columns=0;
	string board[maxSize][maxSize];

	loadBoard(board, &blank_x, &blank_y, &rows, &columns);
	//shuffleBoard(board, rows, columns, &blank_x, &blank_y); // Shuffle if you use solved board
	printBoard(board, rows, columns);
	
	playGame(board, blank_x, blank_y, rows, columns);
	
	return 0;
}