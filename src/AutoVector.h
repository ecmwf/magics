/*
 * AutoVector.h
 *
 *  Created on: 17 Sep 2018
 *      Author: idris
 */

#ifndef AutoVector_h_
#define AutoVector_h_

#include	<vector>
#include	<memory>

template<typename E>
class AutoVector: public std::vector<std::unique_ptr<E>>
{
	public:
		AutoVector () = default;
		virtual ~AutoVector () = default;

		//enable move (but suppress copy)
		AutoVector(AutoVector &&) = default;
		AutoVector &operator= (AutoVector &&) = default;

		void push_back(E *ep) {std::vector<std::unique_ptr<E>>::emplace_back(ep);}
		void emplace_back(E *ep) {std::vector<std::unique_ptr<E>>::emplace_back(ep);}
		typename std::vector<std::unique_ptr<E>>::iterator emplace(typename std::vector<std::unique_ptr<E>>::const_iterator pos, E *ep) {return std::vector<std::unique_ptr<E>>::emplace(pos, ep);}
		typename std::vector<std::unique_ptr<E>>::iterator insert(typename std::vector<std::unique_ptr<E>>::const_iterator pos, E *ep) {return std::vector<std::unique_ptr<E>>::emplace(pos, ep);}

		void simple_clear()	//do not clear the pointers (not thread-safe)
		{
			for (auto &ptr : *this)
				ptr.release();
			std::vector<std::unique_ptr<E>>::clear();
		}
};

#endif /* AutoVector_h_ */
