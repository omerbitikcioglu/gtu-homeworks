public class ContainerTest
{
	public static void main( String[] args)
	{
		GTUSet<Integer> mySet = new GTUSet<Integer>();
		for(Integer i=1; i<=3; ++i)
			mySet.insert(i);

		GTUVector<Character> myVector = new GTUVector<Character>();
		for(Character i='a'; i<='c'; ++i)
			myVector.insert(i); 

		GTUIterator<Integer> iSet;
		GTUIterator<Character> iVector;
		
		iSet = mySet.iterator();
		// Printing the set
		System.out.print("mySet=[");
		while(iSet.hasNext())
		{
			Integer intBox = new Integer(iSet.next());
			int a = intBox.intValue();
			System.out.printf("%d ", a);
		}
		System.out.print("]\n");

		iVector = myVector.iterator();
		// Printing the vector
		System.out.print("myVector=[");
		while(iVector.hasNext())
		{
			Character charBox = new Character(iVector.next());
			char a = charBox.charValue();
			System.out.printf("%c ", a);
		}
		System.out.print("]\n");
	}
}