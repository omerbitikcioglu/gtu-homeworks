/**
 * BoardArray1D class for NPuzzle problem
 * @author Ömer Faruk Bitikçioğlu
 * @since 1.0
 */

import java.io.*;

public class BoardArray1D extends AbstractBoard
{
	private String[] board; // 1D Array
	private int rows;
	private int columns;
	private int size;

	public BoardArray1D(char theLastMove, int howManyMoves, int arrSize)
	{
		super(theLastMove, howManyMoves);
		board = new String[arrSize];
		size = arrSize;
	}

	public BoardArray1D(int arrSize)
	{
		super();
		board = new String[arrSize];
		size = arrSize;
	}

	/** 
	 * Produces the board as string
	 */
	public String toString()
	{
		StringBuilder sb = new StringBuilder();

		sb.append("((");
		int index = 0;
		for(int i=0; i<rows; ++i)
		{
			for(int j=0; j<columns; ++j)
			{
				sb.append(board[index]);

				if(index == rows*columns-1)
					sb.append(String.format("),%c,%d)"), lastMove, numOfMoves);
				else
					sb.append(" ");

				++index;
			}
		}

		return sb.toString();
	}

	/** 
	 * Reads the board from the file given as function parameter
	 */
	public void readFromFile(String fileName) throws IOException
	{
		FileReader inpFile = new FileReader(fileName);
		BufferedReader reader = new BufferedReader(inpFile);
		StringBuilder sb = new StringBuilder();
		String line;

		int count = 0;
		int i;
		for(i = 0; ( line = reader.readLine() ) != null && i < size; ++i)
		{
			sb.append(line);
			sb.append(" ");

			++count; // Count rows
		}
		rows = count;

		String board1D = sb.toString();
		board = board1D.split(" ", size);

		count = 0;
		for(i = 0; board[i] != null; ++i)
			++count; // Count tiles

		columns = count / rows;

		inpFile.close();
	}

	/** 
	 * Writes the board to the file given as function parameter
	 */
	public void writeToFile(String fileName) throws IOException
	{
		FileWriter opFile = new FileWriter(fileName);
		String ls = System.lineSeparator();

		int count = 0;
		for(int i = 0; i < size; ++i)
		{
			opFile.write(board[i]);
			opFile.write(" ");

			++count;
			if(count % 3 == 0)
				opFile.write(ls);
		}

		opFile.close();
	}

	/** 
	 * Resets the board to the solution
	 */
	public void reset()
	{
		int tileSol=0;
		String tileStr;
		int blank_x=0, blank_y=0;

		// Solve the board
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				if(this.cell(j,i) != "00")
				{   
					// Blank tile will be the last tile
					blank_x = j;
					blank_y = i;

					// Find the solution of the tile
					tileSol = findTileSol(j, i);
					tileStr = Integer.toString(tileSol);

					if(tileSol<10)
						tileStr = "0" + tileStr;
					
					// Write the solution on the tile
					board[this.indexOf1D(j,i)] = tileStr;	
				}
			}
		}

		board[this.indexOf1D(blank_x, blank_y)] = "bb"; // Set the blank tile as the last tile except "00"s
	}

	/**
	 * Finds the tile solution of given indices
	 */
	public int findTileSol(int tile_x, int tile_y)
	{
		int tileSol = 1;
		boolean isFound = false;
		String tile;

		// Count until find the desired tile and find its solution
		for(int i = 0; i < tile_y*columns + tile_x; ++i)
		{
			if(board[i] != "00")
				++tileSol;
		}
		
		return tileSol;
	}	

	/** 
	 * Sets the board size to given values. The values are given as parameters
	 * and there are no restrictions on the board size. The board is reset after
	 * this operation
	 */
	public void setSize(int rowSize, int columnSize)
	{
		rows = rowSize;
		columns = columnSize;
	}

	/** 
	 * Makes a move according to the given char parameter. If the parameter
	 * is ‘L’ then the blank tiles moves left, ..., etc
	 */
	public void move(char move)
	{	
		/** 
		 * Find the blank tile...
		 */
		int blank_x=0, blank_y=0;
		findBlankTile(blank_x, blank_y);

		switch(move)
		{
			/**
			 * For L, R, U, and D moves, 
			 * Swap the blank tile and the desired tile
			 * Increase number of moves +1, set the lastMove
			 */
			case 'L':
			case 'l':
			{
				String tempTile = this.cell(blank_x-1,blank_y);
				board[this.indexOf1D(blank_x-1, blank_y)] = "bb";
				board[this.indexOf1D(blank_x, blank_y)] = tempTile;
				
				lastMove = 'L';
				++numOfMoves;
				break;			
			}

			case 'R':
			case 'r':
			{
				String tempTile = this.cell(blank_x+1, blank_y);
				board[this.indexOf1D(blank_x+1, blank_y)] = "bb";
				board[this.indexOf1D(blank_x, blank_y)] = tempTile;
				
				lastMove = 'R';
				++numOfMoves;
				break;		
			}

			case 'U':
			case 'u':
			{
				String tempTile = this.cell(blank_x, blank_y-1);
				board[this.indexOf1D(blank_x, blank_y-1)] = "bb";
				board[this.indexOf1D(blank_x, blank_y)] = tempTile;
				
				lastMove = 'U';
				++numOfMoves;
				break;		
			}

			case 'D':
			case 'd':
			{
				String tempTile = this.cell(blank_x, blank_y+1);
				board[this.indexOf1D(blank_x, blank_y+1)] = "bb";
				board[this.indexOf1D(blank_x, blank_y)] = tempTile;
				
				lastMove = 'D';
				++numOfMoves;
				break;		
			}
			
			default:
				System.out.println("Wrong input! Try again.");
				break;
		}
	}	

	public void findBlankTile(int blank_x, int blank_y)
	{
		for(int i = 0; i < size; ++i)
		{
			if(board[i] == "bb")
			{
				blank_x = i%columns;
				blank_y = i/columns;
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
				tile = this.cell(j,i);
				
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
			System.exit(1);

		int newIndex = this.indexOf1D(x,y);
		return board[newIndex];
	}

	// Converts 2D indices to 1D index and returns
	public int indexOf1D(int x, int y)
	{
		return (x+1)*(y+1)-1;
	}

	// Two boards are equal, if the boards are the same
	public boolean Equals(AbstractBoard otherBoard)
	{
		for(int i = 0; i < rows; ++i)
		{
			for(int j = 0; j < columns; ++j)
			{
				if(this.cell(j,i) != otherBoard.cell(j,i))
					return false;
			}
		}
		return true;
	}
}
