#ifndef GTUVECTOR_H
#define GTUVECTOR_H

#include <memory>

#include "GTUContainer.h"
#include "GTUIterator.h"
#include "GTUIteratorConst.h"

using std::shared_ptr;

namespace BitikciogluHw6
{
	template<class T>
	class GTUVector : public GTUContainer<T>
	{
	public:
		GTUVector() : ptr(nullptr), sizee(0), max_sizee(5) { }
		GTUVector(shared_ptr<T> p) : ptr(p) { }
		~GTUVector() { ptr = nullptr; }

		// Test whether container is empty
		bool empty() const { return(ptr == nullptr); }
		
		// Return container size
		size_t size() { return sizee; }

		// Return maximum size
		size_t max_size() { return max_sizee; }

		// Insert element, 
		// throws exception std::bad_pafram if there is a problem with insersion
		void insert(T element);

		// Erase element
		void erase(T element);

		// Clear all content
		void clear();

		// Return iterator to beginning
		GTUIterator<T> begin();
		// Return const iterator to beginning
		GTUIteratorConst<T> begin() const;

		// Return iterator to end
		GTUIterator<T> end();
		// Return const iterator to end
		GTUIteratorConst<T> end() const;

		T& operator [](int index);

	private:
		shared_ptr<T> ptr;
		size_t sizee;
		size_t max_sizee;
	};
}

#endif // GTUVECTOR_H