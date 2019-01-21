/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Coordinate.cc
    \brief Implementation of the Template class Coordinate.

    Magics Team - ECMWF 2006

    Started: Thu 10-Aug-2006

    Changes:

*/

#include "Coordinate.h"

using namespace magics;

Coordinate::Coordinate() {
    if (automatic()) {
        minmax(std::numeric_limits<double>::max(), -std::numeric_limits<double>::max());
    }
}


Coordinate::~Coordinate() {}

/*!
 Class information are given to the output-stream.
*/
void Coordinate::print(ostream& out) const {
    out << "Coordinate[";
    out << "]";
}
