/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ValuePlot.cc
    \brief Implementation of the Template class ValuePlot.

    Magics Team - ECMWF 2004

    Started: Wed 3-Mar-2004

    Changes:

*/

#include "ValuePlot.h"
#include "BothValuePlotMethod.h"
#include "Data.h"
#include "MarkerValuePlotMethod.h"
#include "Symbol.h"

using namespace magics;


ValuePlot::ValuePlot() {}


ValuePlot::~ValuePlot() {}


/*!
 Class information are given to the output-stream.
*/

void ValuePlot::print(ostream& out) const {
    out << "ValuePlot[";
    ValuePlotAttributes::print(out);
    out << "]";
}


void ValuePlot::operator()(MatrixHandler& data, BasicGraphicsObjectContainer& parent) {
    (*(this->method_)).clear();

    (*(this->method_))(data, parent.transformation());

    // Now we feed the task
    for (vector<BasicGraphicsObject*>::const_iterator object = (*this->method_).begin();
         object != (*this->method_).end(); ++object)
        parent.push_back(*object);
}

void ValuePlot::operator()(Data& data, BasicGraphicsObjectContainer& parent) {
    (*(this->method_)).clear();
    (*(this->method_))(data.points(parent.transformation(), false), parent.transformation());

    // Now we feed the task
    for (vector<BasicGraphicsObject*>::const_iterator object = (*this->method_).begin();
         object != (*this->method_).end(); ++object)
        parent.push_back(*object);
}

void ValuePlot::visit(LegendVisitor&) {}
