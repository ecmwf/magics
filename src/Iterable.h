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
* \file Iterable.h
* \license Apache License 2.0
*
*  Created on: 17 Sep 2018
*      Author: idris
*/

#ifndef	Iterable_h_
#define	Iterable_h_

template<typename Container>
class Iterable
{
		const Container &container;
	public:
		auto begin() const {return container.cbegin();}
		auto end() const {return container.cend();}

		Iterable(const Container &cont): container(cont) {}
		Iterable(Iterable &&) = default;	//allow move construction, suppress move assignment and copy operations
};

#endif	//Iterable_h_
