/*
 * (C) Copyright 2018- ECMWF
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
 * \file AutoVector.h
 * \license Apache License 2.0
 *
 *  Created on: 17 Sep 2018
 *      Author: idris
 */

#ifndef AutoVector_h_
#define AutoVector_h_

#include	"Iterable.h"

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

template<typename E>
class AutoVectorIterable: public Iterable<AutoVector<E>>
{
	public:
		AutoVectorIterable(const AutoVector<E> &cont): Iterable<AutoVector<E>>(cont) {}
		AutoVectorIterable(AutoVectorIterable &&) = default;
};

#endif /* AutoVector_h_ */
