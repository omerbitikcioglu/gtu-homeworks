#ifndef GTUITERATORCONST_H
#define GTUITERATORCONST_H

#include <memory>
using std::shared_ptr;

namespace BitikciogluHw6
{
	template<class T>
	class GTUIteratorConst
	{
	public:
		GTUIteratorConst() : ptr(nullptr) { }
		GTUIteratorConst(shared_ptr<T> p) : ptr(p) { }
		GTUIteratorConst(const GTUIteratorConst& gtuit) : ptr(gtuit.ptr) { }

		const GTUIteratorConst<T>& operator ++();
		GTUIteratorConst<T> operator ++(T);

		const GTUIteratorConst<T>& operator --();
		GTUIteratorConst<T> operator --(T);

		template<class Typ>
		friend bool operator ==(const GTUIteratorConst<Typ>& gtuit_1, const GTUIteratorConst<Typ>& gtuit_2);
		
		template<class Typ>
		friend bool operator !=(const GTUIteratorConst<Typ>& gtuit_1, const GTUIteratorConst<Typ>& gtuit_2);

		const T& operator *() { return *ptr; }
	private:
		shared_ptr<T> ptr;
	};
}

#endif // GTUITERATORCONST_H 