#include "GTUIteratorConst.h"

namespace BitikciogluHw6
{
	template <class T>
	const GTUIteratorConst<T>& GTUIteratorConst<T>::operator ++()
	{ 
		++ptr; 
		return *this; 
	}

	template <class T>
	GTUIteratorConst<T> GTUIteratorConst<T>::operator ++(T)
	{ 
		GTUIteratorConst temp(ptr); 
		++ptr; 
		return temp; 
	}

	template <class T>
	const GTUIteratorConst<T>& GTUIteratorConst<T>::operator --() 
	{ 
		--ptr; 
		return *this; 
	}
	
	template <class T>
	GTUIteratorConst<T> GTUIteratorConst<T>::operator --(T) 
	{ 
		GTUIteratorConst temp(ptr); 
		--ptr; 
		return temp; 
	}

	template <class Typ>
	bool operator ==(const GTUIteratorConst<Typ>& gtuit_1, const GTUIteratorConst<Typ>& gtuit_2)
	{
		return (gtuit_1.ptr == gtuit_2.ptr);
	}

	template <class Typ>
	bool operator !=(const GTUIteratorConst<Typ>& gtuit_1, const GTUIteratorConst<Typ>& gtuit_2)
	{
		return (gtuit_1.ptr != gtuit_2.ptr);
	}
}