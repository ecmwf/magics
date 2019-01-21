/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Akima760Method.cc
    \brief Implementation of the Template class Akima760Method.

    Magics Team - ECMWF 2004

    Started: Thu 11-Mar-2004

    Changes:

*/

#include "Akima760Method.h"

using namespace magics;

template <class P>
Akima760Method<P>::Akima760Method() {}

template <class P>
Akima760Method<P>::~Akima760Method() {}

/*!
 Class information are given to the output-stream.
*/
template <class P>
void Akima760Method<P>::print(ostream& out) const {
    out << "Akima760Method<P>";
}
