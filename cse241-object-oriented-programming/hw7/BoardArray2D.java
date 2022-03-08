/**
 * BoardArray2D class for NPuzzle problem
 * @author Ömer Faruk Bitikçioğlu
 * @since 1.0
 */

import java.io.*;

public class BoardArray2D extends AbstractBoard
{
	private String[][] board; // 2D Array
	private int rows;
	private int columns;

	public BoardArray2D(char theLastMove, int howManyMoves, int rowS, int columnS)
	{
		super(theLastMove, howManyMoves);
		board = new String[rowS][columnS];
		rows = rowS;
		columns = columnS;
	}

	public BoardArray2D(int rowS, int columnS)
	{
		super();
		board = new String[rowS][columnS];
		rows = rowS;
		columns = columnS;
	}

	public BoardArray2D()
	{
		super();
		rows = 0;
		columns = 0;
	}

	// Produces the 1D board as String
	public String toString()
	{
		// Informations from superclass and later board itself
		String info = String.format("%s\n", super.toString());
		StringBuilder sb = new StringBuilder();
		String ls = System.lineSeparator();

		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				sb.append( String.format("%s ", board[i][j]) );
			}
			sb.append(ls);
		}

		String print = sb.toString();

		return new String(info + print);
	}

	// Reads the board from the file given as function parameter
	public void readFromFile(String fileName) throws IOException
	{
		FileReader inpFile = new FileReader(fileName);
		BufferedReader reader = new BufferedReader(inpFile);
		StringBuilder sb;
		String line;

		for(int i = 0; ( line = reader.readLine() ) != null; ++i)
		{
			board[i] = line.split(" ", 0);
			++rows; // Every line we read is a plus number of rows
		}

		// Find columns number
		int count;
		for(count = 0; board[0][count] != null; ++count);
		columns = count;

		inpFile.close();
	}

	// Writes the board to the file given as function parameter
	public void writeToFile(String fileName) throws IOException
	{
		FileWriter opFile = new FileWriter(fileName);
		String ls = System.lineSeparator();

		int count = 0;
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				opFile.write(board[i][j]);
				opFile.write(" ");
			}
			opFile.write(ls);
		}

		opFile.close();
	}

	// Resets the board to the solution
	public void reset()
	{
		int tileSol;
		String tileStr;
		int blank_x=0, blank_y=0;

		// Solve the board
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				if(board[i][j] != "00")
				{   
					// Blank tile will be the last tile
					blank_x = j;
					blank_y = i;

					// Find the solution of the tile
					tileSol = findTileSol(j, i);
					tileStr = Integer.toString(tileSol);

					if(tileSol<10)
						tileStr = new String("0" + tileStr);
					
					// Write the solution on the tile
					board[i][j] = tileStr;	
				}				
			}
		}

		// Set the blank tile as the last tile except "00"s
		board[blank_y][blank_x] = "bb"; 
	}

	public int findTileSol(int tile_x, int tile_y)
	{
		int tileSol = 1;
		boolean isFound = false;
		String tile;

		// Count until find the desired tile and find its solution
		for(int i = 0; i < tile_y; ++i)
		{
			for(int j = 0; j < tile_x; ++j)
			{
				if(board[i][j] != "00")
					++tileSol;	
			}
		}
		
		return tileSol;
	}	

	// Sets the board size to given values. The values are given as parameters
	// and there are no restrictions on the board size. The board is reset after
	// this operation
	public void setSize(int rowSize, int columnSize)
	{
		rows = rowSize;
		columns = columnSize;
	}

	// Makes a move according to the given char parameter. If the parameter
	// is ‘L’ then the blank tiles moves left, ..., etc
	public void move(char move)
	{	
		// Find the blank tile...
		int blank_x=0, blank_y=0;
		findBlankTile(blank_x, blank_y);

		switch(move)
		{
			// For L, R, U, and D moves, 
			// Swap the blank tile and the desired tile
			// Increase number of moves +1, set the lastMove
			case 'L':
			case 'l':
			{
				String tempTile;
				tempTile = board[blank_y][blank_x-1];
				board[blank_y][blank_x-1] = "bb";
				board[blank_y][blank_x] = tempTile;
				
				lastMove = 'L';
				++numOfMoves;
				break;			
			}

			case 'R':
			case 'r':
			{
				String tempTile;
				tempTile = board[blank_y][blank_x+1];
				board[blank_y][blank_x+1] = "bb";
				board[blank_y][blank_x] = tempTile;
				
				lastMove = 'R';
				++numOfMoves;
				break;		
			}

			case 'U':
			case 'u':
			{
				String tempTile;
				tempTile = board[blank_y-1][blank_x];
				board[blank_y-1][blank_x] = "bb";
				board[blank_y][blank_x] = tempTile;
				
				lastMove = 'U';
				++numOfMoves;
				break;		
			}

			case 'D':
			case 'd':
			{
				String tempTile;
				tempTile = board[blank_y+1][blank_x];
				board[blank_y+1][blank_x] = "bb";
				board[blank_y][blank_x] = tempTile;
				
				lastMove = 'D';
				++numOfMoves;
				break;		
			}
			
			default:
				System.out.println("Wrong input! Try again.");
				break;
		}
	}	

	// Finds the blank tile "bb" in the board 
	public void findBlankTile(int blank_x, int blank_y)
	{
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				if(board[i][j] == "bb") // Found
				{
					blank_x = j;
					blank_y = i;
				}	
			}
		}
	}	

	// If board is solved it returns true
	public boolean isSolved()
	{
		int tileValue, tileSol = 1;
		String tile;

		// In solution, all the tiles except 00"s,
		// must start from 1 and increase one by one
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				tile = board[i][j];
				
				if(tile != "00")
				{
					// Convert String to int
					tileValue = Integer.parseInt(tile);

					if(tileValue == tileSol) 
						++tileSol;
					else if(tile == "bb") 
						tileSol = 0; // "bb" must be the last tile
					else 
						return false;
				}
			}
		}

		return true;
	}

	// Returns the correcponding cell content
	// Terminates the program if the indices(x,y) are not valid 
	public String cell(int x, int y)
	{
		if( (x < 0 && x >= columns) || (y < 0 && y >= rows) )
			System.exit(1); // Terminates the program

		return board[y][x];			
	}

	// Two boards are equal, if the boards are the same
	public boolean Equals(AbstractBoard otherBoard)
	{
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				if(board[i][j] != otherBoard.cell(j,i))
					return false;
			}
		}
		return true;
	}
}
