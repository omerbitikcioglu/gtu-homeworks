#ifndef FUNCTIONS_H
#define	FUNCTIONS_H

const int maxSize = 9;

void takeProblemSize(int *problemSize, int *blank_x, int *blank_y);
void initBoard(int board[maxSize][maxSize], int problemSize);
void shuffleBoard(int board[maxSize][maxSize], int problemSize, int *blank_x, int *blank_y);
void printBoard(int board[maxSize][maxSize], int problemSize);
void playGame(int board[maxSize][maxSize], int problemSize, int blank_x, int blank_y);
void makeMove(int board[maxSize][maxSize], int *blank_x, int *blank_y, char move, int problemSize);
bool isValidMove(int board[maxSize][maxSize], int blank_x, int blank_y, char move, int problemSize);
bool isGameOver(int board[maxSize][maxSize], int problemSize);
char findIntelMove(int board[maxSize][maxSize], int blank_x, int blank_y, int problemSize);

#endif // FUNCTIONS_H