/**
* <h1>CSE241(OOP) Homework ~ 7 ~</h1>
* @author Ömer Faruk Bitikçioğlu
* @since 1.0
*/

public class Main
{
	public static void main( String[] args )
	{
		// 1D Board Arrays
		BoardArray1D board1D_1, board1D_2, board1D_3;
		board1D_1 = new BoardArray1D('L', 2, 25);
		board1D_2 = new BoardArray1D('L', 1, 25);
		board1D_3 = new BoardArray1D(25);

		AbstractBoard boardRefs[] = new AbstractBoard[3];
		boardRefs[0] = board1D_1;
		boardRefs[1] = board1D_2;
		boardRefs[3] = board1D_1;

		if(isValidSeq(boardRefs))
			System.out.println("1D's are True");
		else
			System.out.println("1D's are False");

		// BoardArray2D board2D_1, board2D_2, board2D_3;	
	}

	public static boolean isValidSeq( AbstractBoard[] refs )
	{
		for(int i = refs.length-1; i > 0 ; --i)
		{
			// Last board must be the solution
			if(i == refs.length-1 && !refs[i].isSolved())
				return false;
			else
			{
				AbstractBoard temp = new AbstractBoard()
				refs[i-1].move()
				return false;
			}
		}
		return true;
	}
}
