/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisControl.cc
    \brief Implementation of the Template class AxisControl.

    Magics Team - ECMWF 2005

    Started: Thu 13-Oct-2005

    Changes:

*/


#include "AxisControl.h"
#include "AxisMethod.h"
#include "Transformation.h"

using namespace magics;

AxisControl::AxisControl() {}


AxisControl::~AxisControl() {}

/*!
 Class information are given to the output-stream.
*/
void AxisControl::print(ostream& out) const {
    out << "AxisControl[";
    out << "]";
}

void AutomaticAxisControl::horizontal(Layout& layout, Transformation& transformation, AxisMethod& method) {
    transformation.adjustXAxis(layout);
    method.updateX(transformation);
}


void AutomaticAxisControl::vertical(Layout& layout, Transformation& transformation, AxisMethod& method) {
    transformation.adjustYAxis(layout);
    method.updateY(transformation);
}

void AxisControl::vertical(Layout&, Transformation& transformation, AxisMethod& method) {
    method.updateY(transformation);
}

void AxisControl::horizontal(Layout&, Transformation& transformation, AxisMethod& method) {
    method.updateX(transformation);
}
