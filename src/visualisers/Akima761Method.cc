/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Akima761Method.cc
    \brief Implementation of the Template class Akima761Method.

    Magics Team - ECMWF 2004

    Started: Thu 11-Mar-2004

    Changes:

*/

#include "Akima761Method.h"

using namespace magics;

template <class P>
Akima761Method<P>::Akima761Method() {}

template <class P>
Akima761Method<P>::~Akima761Method() {}

/*!
 Class information are given to the output-stream.
*/
template <class P>
void Akima761Method<P>::print(ostream& out) const {
    out << "Akima761Method<P>";
}
