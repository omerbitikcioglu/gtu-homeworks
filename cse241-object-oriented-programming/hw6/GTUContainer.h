#ifndef GTUCONTAINER_H
#define GTUCONTAINER_H 

#include "GTUIterator.h"

namespace BitikciogluHw6
{
	template<class T>
	class GTUContainer
	{
	public:
		// Test whether container is empty
		virtual bool empty() const = 0;
		
		// Return container size
		virtual size_t size() = 0;

		// Return maximum size
		virtual size_t max_size() = 0;

		// Insert element, 
		// throws exception std::bad_pafram if there is a problem with insersion
		virtual void insert(T element) = 0;

		// Erase element
		virtual void erase(T element) = 0;

		// Clear all content
		virtual void clear() = 0;

		// Return iterator to beginning
		virtual GTUIterator<T> begin() = 0;

		// Return iterator to end
		virtual GTUIterator<T> end() = 0;
	};
}

#endif // GTUCONTAINER_H