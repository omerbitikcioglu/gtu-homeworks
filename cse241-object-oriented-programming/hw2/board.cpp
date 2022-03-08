//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 1 ~
//	===========================================

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "functions.h"
using namespace std;

void loadBoard(std::string board[maxSize][maxSize], int *blank_x, int *blank_y, int *rows, int *columns)
{
	// Ask user to enter a file name
	string fileName;
	cout << "Enter file name to load (*.txt):\n";
	cin >> fileName;
	
	// Open the file
	ifstream boardFile;
	boardFile.open(fileName);

	// Check if the file is opened
	if (!boardFile)
	{
	    cerr << "Unable to open file " << fileName << endl;
	    exit(1);
	}

	// Read the file, fill the board array, and determine row and column sizes
	string boardRow, tile;
	while(getline(boardFile, boardRow))
	{
		stringstream s(boardRow);
		*columns = 0;
		while(s >> tile)
		{
			board[*rows][*columns] = tile;
			if(tile == "bb") // Blank tile found
			{
				*blank_x = *columns;
				*blank_y = *rows;
			}
			++(*columns);
		}
		++(*rows);
	}

	boardFile.close();
}

void saveBoard(std::string board[maxSize][maxSize], int rows, int columns)
{
	// Ask user to enter a file name
	string fileName;
	cout << "Enter file name to save (*.txt):\n";
	cin >> fileName;
	
	// Open the file
	ofstream boardFile;
	boardFile.open(fileName);

	// Check if the file is opened
	if (!boardFile)
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
			boardFile << tile + " ";
		}
		boardFile << endl;
	}
}

void printBoard(string board[maxSize][maxSize], int rows, int columns)
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

void shuffleBoard(string board[maxSize][maxSize], int rows, int columns, int *blank_x, int *blank_y)
{
	int temp; // Temp var, not important
	char randMove;
	for(int i = 0; i < (maxSize*maxSize); ++i) // Move randomly N*N times
	{
		randMove = pickRandomMove(board, blank_x, blank_y, rows, columns);
		makeMove(board, blank_x, blank_y, randMove, rows, columns, &temp);
	}
}

