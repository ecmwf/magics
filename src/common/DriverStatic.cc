/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DriverStatic.cc
    \brief Implementation of the Template class DriverStatic.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Sep. 2006
*/

#include "DriverStatic.h"

using namespace magics;

DriverStatic::DriverStatic() {}

DriverStatic::~DriverStatic() {}

/*!
 Class information are given to the output-stream.
*/
void DriverStatic::print(ostream& out) const {
    out << "DriverStatic[";
    out << "]";
}
