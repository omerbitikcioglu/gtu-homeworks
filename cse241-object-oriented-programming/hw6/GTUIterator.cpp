#include "GTUIterator.h"

namespace BitikciogluHw6
{
	template <class T>
	GTUIterator<T>& GTUIterator<T>::operator ++()
	{ 
		++ptr; 
		return *this; 
	}

	template <class T>
	GTUIterator<T> GTUIterator<T>::operator ++(T)
	{ 
		GTUIterator temp(ptr); 
		++ptr; 
		return temp; 
	}

	template <class T>
	GTUIterator<T>& GTUIterator<T>::operator --() 
	{ 
		--ptr; 
		return *this; 
	}
	
	template <class T>
	GTUIterator<T> GTUIterator<T>::operator --(T) 
	{ 
		GTUIterator temp(ptr); 
		--ptr; 
		return temp; 
	}

	template <class Typ>
	bool operator ==(const GTUIterator<Typ>& gtuit_1, const GTUIterator<Typ>& gtuit_2)
	{
		return (gtuit_1.ptr == gtuit_2.ptr);
	}

	template <class Typ>
	bool operator !=(const GTUIterator<Typ>& gtuit_1, const GTUIterator<Typ>& gtuit_2)
	{
		return (gtuit_1.ptr != gtuit_2.ptr);
	}
}