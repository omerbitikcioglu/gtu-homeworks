#ifndef GTUSET_H
#define GTUSET_H

#include <memory>
#include "GTUContainer.h"
#include "GTUIterator.h"
#include "GTUIteratorConst.h"

using std::shared_ptr;

namespace BitikciogluHw6
{
	template<class T>
	class GTUSet : public GTUContainer<T>
	{
	public:
		GTUSet() : ptr(nullptr), sizee(0), max_sizee(5) { }
		GTUSet(shared_ptr<T> p) : ptr(p) { }
		~GTUSet() { ptr = nullptr; }

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

	private:
		shared_ptr<T> ptr;
		size_t sizee;
		size_t max_sizee;
	};
}

#endif // GTUSET_H