/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisType.cc
    \brief Implementation of the Template class AxisType.

    Magics Team - ECMWF 2004

    Started: Fri 7-May-2004

    Changes:

*/


#include "AxisType.h"

using namespace magics;

AxisType::AxisType() {}


AxisType::~AxisType() {}

/*!
 Class information are given to the output-stream.
*/
void AxisType::print(ostream& out) const {
    out << "AxisType[";
    out << "]";
}

static SimpleObjectMaker<AxisType> regular("regular");
