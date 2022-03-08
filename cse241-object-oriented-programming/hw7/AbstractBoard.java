/**
 * AbstractBoard class for NPuzzle problem
 * @author Ömer Faruk Bitikçioğlu
 * @since 1.0
 */

import java.io.*;

public abstract class AbstractBoard 
{	
	protected char lastMove;
	protected int numOfMoves;
	protected static int numOfBoards = 0;

	public AbstractBoard(char theLastMove, int howManyMoves)
	{
		lastMove = theLastMove;
		numOfMoves = howManyMoves;
		numOfBoards++;
	}
	
	public AbstractBoard()
	{
		lastMove='S';
		numOfMoves=0;
		numOfBoards++; 
	}

	/** 
	 * Produces the board as string
	 */
	public String toString()
	{
		return String.format("%s: %c\n%s: %d\n", 
			"Last Move", lastMove, 
			"Number Of Moves", numOfMoves); 
	}

	/** 
	 * @throws IOException
	 * Reads the board from the file given as function parameter
	 */
	public abstract void readFromFile(String fileName) throws IOException;

	/** 
	 * @throws IOException
	 * Writes the board to the file given as function parameter
	 */
	public abstract void writeToFile(String fileName) throws IOException;

	/** 
	 * Resets the board to the solution
	 */
	public abstract void reset();

	/** 
	 * Sets the board size to given values. The values are given as parameters
	 * and there are no restrictions on the board size. The board is reset after
	 * this operation
	 */
	public abstract void setSize(int rowSize, int columnSize);

	/** 
	 * Makes a move according to the given char parameter. If the parameter
	 * is ‘L’ then the blank tiles moves left, ..., etc
	 */
	public abstract void move(char move);	

	/** 
	 * @return true if board is solved
	 */
	public abstract boolean isSolved(); 

	/** 
	 * @return the correcponding cell content
	 * Terminates the program if the indices(x,y) are not valid 
	 */
	public abstract String cell(int x, int y);

	/**
	 * Two boards are equal, if the boards are the same
	 * @param otherBoard can be any board object
	 */
	public abstract boolean Equals(AbstractBoard otherBoard);

	/**
	 * Finds the tile solution of given indices
	 */
	public abstract int findTileSol(int tile_x, int tile_y);

	/** 
	 * @return the number of Board objects created so far
	 */
	public static int NumberOfBoards()
	{
		return numOfBoards;
	}

	/** 
	 * @return the last move
	 */
	public char lastMove()
	{
		return lastMove;
	}

	/**
	 * @return the number of steps (moves) this board made
	 */
	public int numberOfMoves()
	{
		return numOfMoves;
	}
}