#ifndef FUNCTIONS_H
#define	FUNCTIONS_H
const int maxSize = 9;

void saveBoard(std::string board[maxSize][maxSize], int rows, int columns);
void loadBoard(std::string board[maxSize][maxSize], int *blank_x, int *blank_y, int *rows, int *columns);
void printBoard(std::string board[maxSize][maxSize], int rows, int columns);
void shuffleBoard(std::string board[maxSize][maxSize], int rows, int columns, int *blank_x, int *blank_y);
void playGame(std::string board[maxSize][maxSize], int blank_x, int blank_y, int rows, int columns);
void makeMove(std::string board[maxSize][maxSize], int *blank_x, int *blank_y, char move, int rows, int columns, int *numOfMoves);
bool isValidMove(std::string board[maxSize][maxSize], int blank_x, int blank_y, char move, int rows, int columns);
bool isGameOver(std::string board[maxSize][maxSize], int rows, int columns);
char findIntelMove(std::string board[maxSize][maxSize], int blank_x, int blank_y, int rows, int columns);
int findTileSol(std::string board[maxSize][maxSize], int tile_x, int tile_y, int rows, int columns);
void intelCheck(int *sub, int tileSol, int tile, char move, char *intelMove);
int stringToInt(std::string str);
char pickRandomMove(std::string board[maxSize][maxSize], int *blank_x, int *blank_y, int rows, int columns);


#endif // FUNCTIONS_H