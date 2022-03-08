#include <iostream>
#include "GTUContainer.h"
#include "GTUSet.h"
#include "GTUVector.h"
#include "GTUIterator.h"

using std::cout;
using std::endl;
using namespace BitikciogluHw6;

template<class Iterator, class T>
Iterator find (Iterator bg, Iterator end, const T& val)
{
	while(bg != end) 
	{
		if (*bg == val) 
			return bg;
		++bg;
	}
	return end;
}

int main()
{
	typedef int T;

	GTUSet<T> mySet;
	for(T i=1; i<=3; ++i)
		mySet.insert(i);

	GTUVector<T> myVector;
	for(T i=1; i<=3; ++i)
	{
		myVector.insert(i);
		cout << "myVector[" << i-1 << "] =" << i << endl; 
	}

	GTUIterator<T> i_bg;
	GTUIterator<T> i_end;
	GTUIterator<T> result;
	
	i_bg = mySet.begin();
	i_end = mySet.end();

	result = find(i_bg, i_end, 3);

	if(result != mySet.end())
		cout << *result;

	i_bg = myVector.begin();
	i_end = myVector.end();
	
	result = find(i_bg, i_end, 6);

	if(result != myVector.end())
		cout << *result;

	return 0;
}