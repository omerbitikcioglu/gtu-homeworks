//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 4 ~
//	===========================================

#include <iostream>
#include <fstream>
#include "npuzzle.h"
using namespace std;

int main()
{
	// Set the seed to the computer time
	srand(time(NULL));

	cout << " Welcome to N-Puzzle Game!\n\n";

	// Create a new NPuzzle object
	NPuzzle game;
	
	// User desides what kind of board to use
	cout << "\tSelect one:\n"
		<< "1) Use a random board\n"
		<< "2) Load board from a file\n"
		<< "Selection: ";

	int selection;
	cin >> selection;
	cout << endl;

	if(selection != 1 && selection != 2)
	{
		cerr << "Invalid input! Program will be terminated!\n";
		exit(1);
	}
	
	if(selection == 1) // Create a random board
	{
		// Pick random w and h both <= 9
		int width = rand()%9+1; // Columns
		int height = rand()%9+1; // Rows

		game.setSize(height, width);
		game.reset();
		game.shuffle(100);
	}
	else // Load from file
	{
		// Ask user to enter a file name to load
		cout << "Enter file name to load (*.txt):\n";
		string fileName;
		cin >> fileName;
		
		// Open and read the file
		ifstream inpFile;
		inpFile.open(fileName);
		inpFile >> game;
		inpFile.close();	
	}

	cout << game;
	game.play();
	
	return 0;
}