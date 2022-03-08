#include <iostream>
#include "GTUVector.h"

using std::cout;

namespace BitikciogluHw6
{
	// Insert element, 
	// throws exception std::bad_pafram if there is a problem with insersion
	template<class T>
	void GTUVector<T>::insert(T element)
	{	
		// An element can exist only once in a set
		bool exists = false;
		GTUIterator<T> i;
		for(i = this->begin(); i != this->end(); ++i)
			if(*i == element) exists = true;

		if(!exists)
		{
			ptr = new T[++size];
			ptr[size-1] = element;
		}
	}

	// Erase element
	template<class T>
	void GTUVector<T>::erase(T element)
	{
		GTUIterator<T> i;
		for(i = this->begin(); i != this->end(); ++i)
		{
			if(*i == element)
			{
				T* temp = (i+1).ptr;
				delete i.ptr;
			}
		}
	}

	// Clear all content
	template<class T>
	void GTUVector<T>::clear()
	{
		GTUIterator<T> i;
		for(i = this->begin(); i != this->end(); ++i)
			*i = 0;
	}

	// Return iterator to beginning
	template<class T>
	GTUIterator<T> GTUVector<T>::begin()
	{
		GTUIterator<T> i(ptr);
		return i;
	}

	// Return const iterator to beginning
	template<class T>
	GTUIteratorConst<T> GTUVector<T>::begin() const
	{
		GTUIteratorConst<T> i(ptr);
		return i;
	}

	// Return iterator to end
	template<class T>
	GTUIterator<T> GTUVector<T>::end()
	{
		return GTUIterator<T>(ptr+sizee);
	}

	// Return const iterator to end
	template<class T>
	GTUIteratorConst<T> GTUVector<T>::end() const
	{
		return GTUIteratorConst<T>(ptr+sizee);
	}

	template<class T>
	T& GTUVector<T>::operator [](int index)
	{
		if(index >= sizee)
		{
			cout << "Unavailable index value !\n";
			exit(1);
		}
		return ptr[index];
	}
}