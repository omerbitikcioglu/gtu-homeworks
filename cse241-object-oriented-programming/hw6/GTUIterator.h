#ifndef GTUITERATOR_H
#define GTUITERATOR_H

#include <memory>
using std::shared_ptr;

namespace BitikciogluHw6
{
	template<class T>
	class GTUIterator
	{
	public:
		GTUIterator() : ptr(nullptr) { }
		GTUIterator(shared_ptr<T> p) : ptr(p) { }
		GTUIterator(const GTUIterator& gtuit) : ptr(gtuit.ptr) { }

		GTUIterator<T>& operator ++();
		GTUIterator<T> operator ++(T);

		GTUIterator<T>& operator --();
		GTUIterator<T> operator --(T);

		template<class Typ>
		friend bool operator ==(const GTUIterator<Typ>& gtuit_1, const GTUIterator<Typ>& gtuit_2);
		
		template<class Typ>
		friend bool operator !=(const GTUIterator<Typ>& gtuit_1, const GTUIterator<Typ>& gtuit_2);

		T& operator *() { return *ptr; }
	private:
		shared_ptr<T> ptr;
	};
}

#endif // GTUITERATOR_H 