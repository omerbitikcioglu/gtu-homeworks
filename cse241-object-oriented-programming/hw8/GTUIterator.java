import java.util.NoSuchElementException;

public class GTUIterator<E>
{
	private E[] arrRef;
	private int index;
	
	public GTUIterator(E[] anArrayRef)
	{
		arrRef = anArrayRef;
		
		// Indext starts from -1 so that 
		// we can use hasNext and next methods
		index = -1; 
	}

	/**
	 * @return true if the iteration has more elements
	 */
	public boolean hasNext()
	{
		return (index+1 < arrRef.length);
	}

	/**
	 * @return the next element in the iteration
	 * @throws NoSuchElementException
	 */
	public E next() throws NoSuchElementException
	{
		return arrRef[++index];
	}

	public int getIndex()
	{
		return index;
	}
}