//	===========================================
// 	Author      : 	Ömer Faruk BİTİKÇİOĞLU
// 	Description : 	CSE241(OOP) Homework ~ 3 ~
//	===========================================

#include <iostream>
#include <fstream>
#include "npuzzle.h"
using namespace std;

int main()
{
	cout << "Welcome to the N-Puzzle Game!\n\n";

	NPuzzle game; // Create a new NPuzzle object
	
	// Ask user to enter a file name to load the board config
	std::string fileName;
	cout << "Enter file name to load (*.txt):\n";
	cin >> fileName;

	// Load the config
	game.readFromFile(fileName);

	// Shuffle if you used a solved board
	//game.shuffle(100); // You can change the parameter
	
	game.print();
	game.play();
	
	return 0;
}