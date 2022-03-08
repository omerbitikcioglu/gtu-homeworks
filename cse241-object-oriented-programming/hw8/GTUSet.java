public class GTUSet<E> extends GTUContainer<E>
{
	private E[] arr; // Contains data of the set
	private int size; // Size of the set
	private int maxSize; // Maximum size of the set

	public GTUSet()
	{
		arr = null;
		size = 0;
		maxSize = 5;
	}

	/**
	 * Constructor with an array paramater
	 * @param arrayRef is a referance to an array
	 */
	public GTUSet(E[] arrayRef)
	{
		arr = arrayRef;
		size = arr.length;
		maxSize = size + 5;
	}

	/**
	 * Test whether container is empty
	 */
	public boolean empty()
	{
		return (size==0);
	}

	/**
	 * @return container size
	 */
	public int size()
	{
		return size;
	}

	/**
	 * @return maximum size
	 */
	public int max_size()
	{
		return maxSize;
	}

	/** 
	 * Insert element, 
	 * @throws IllegalArgumentException if there is a problem with insersion
	 */
	public void insert(E element) throws IllegalArgumentException
	{	
		// An element can exist only once in a set
		boolean exists = false;
		GTUIterator<E> iter = this.iterator();
		while(iter.hasNext() && !exists)
		{
			E member = iter.next();
			if(member == element) 
				exists = true;
		}

		// If the element is not exists in the set, then add it
		if(!exists)
		{
			E[] temp = arr;
			arr = new E[++size];

			for(int i=0; i<size-1; ++i)
				arr[i] = temp[i];
			arr[i] = element;
			System.out.printf("%s %s %s\n", 
				"Element ", element, " added to the set!"); 
		}
	}

	/**
	 * Erase element
	 */
	public void erase(E element)
	{
		GTUIterator<E> iter = this.iterator();
		while(iter.hasNext())
		{
			E member = iter.next();
			if(member == element)
			{
				// Delete the element
				for(int i = iter.getIndex(); i < this.size; ++i)
					arr[i] = a[i+1]; // Shifting elements

				size--;
			}
		}
	}

	/**
	 * Clear all content
	 */
	public void clear()
	{
		GTUIterator<E> iter = new GTUIterator<E>(arr);
		for(iter = this.iterator(); iter.hasNext(); iter.next())
		{
			arr[iter.getIndex()+1] = 0;
			size = 0;
		}
	}

	/**
	 * @return iterator to beginning
	 */
	public GTUIterator<E> iterator()
	{
		GTUIterator<E> iter = new GTUIterator<E>(arr);
		return iter;
	}

	/**
	 * @return true if this collection contains the specified element.
	 */
	public boolean contains(Object o)
	{
		return (o == this);
	}
}