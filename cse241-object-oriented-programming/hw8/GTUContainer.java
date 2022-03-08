import java.lang.IllegalArgumentException;

public abstract class GTUContainer<E>
{
	/**
	 * Test whether container is empty
	 */
	public abstract boolean empty();
	
	/**
	 * @return container size
	 */
	public abstract int size();

	/**
	 * @return maximum size
	 */
	public abstract int max_size();

	/**
	 * Insert element
	 * @throws IllegalArgumentException if there is a problem with insersion
	 */
	public abstract void insert(E element) throws IllegalArgumentException;

	/**
	 * Erase element
	 */
	public abstract void erase(E element);

	/**
	 * Clear all content
	 */
	public abstract void clear();

	/**
	 * @return iterator to beginning
	 */
	public abstract GTUIterator<E> iterator();

	/**
	 * @return true if this collection contains the specified element.
	 */
	public abstract boolean contains(Object o);
}
