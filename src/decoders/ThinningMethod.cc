/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ThinningMethod.h
    \brief Implementation of the Template class ThinningMethod.

    Magics Team - ECMWF 2010

    Started: Thu 28-Oct-2010

    Changes:

*/


#include "ThinningMethod.h"
#include "Data.h"

using namespace magics;

ThinningMethod::ThinningMethod() {}


ThinningMethod::~ThinningMethod() {}

/*!
 Class information are given to the output-stream.
*/
void ThinningMethod::print(ostream& out) const {
    out << "ThinningMethod[";
    out << "]";
}

AutomaticThinningMethod::AutomaticThinningMethod() {}


void ThinningMethod::operator()(Data&, const Transformation&, const std::set<string>&, CustomisedPointsList&) {}


void BasicThinningMethod::operator()(Data& data, const Transformation& transformation, const std::set<string>& request,
                                     CustomisedPointsList& points) {
    try {
        data.customisedPoints(*this, transformation, request, points);
    }

    catch (...) {
        data.customisedPoints(transformation, request, points, false);
    }
}


void AutomaticThinningMethod::operator()(Data& data, const Transformation& transformation,
                                         const std::set<string>& request, CustomisedPointsList& points) {
    try {
        data.customisedPoints(*this, transformation, request, points);
    }

    catch (...) {
        data.customisedPoints(transformation, request, points, false);
    }
}

AutomaticThinningMethod::~AutomaticThinningMethod() {}

void AutomaticThinningMethod::set(const ThinningMethodUI& ui) {
    nbPoints_ = ui.nbPoints_;
    x_        = ui.width_ * nbPoints_;
    y_        = ui.height_ * nbPoints_;
    units_    = nbPoints_;
    rawOnly_  = ui.rawOnly_;  // If true, do not thin .. only show raw data if resolution allows it!
}


BasicThinningMethod::BasicThinningMethod() {}


BasicThinningMethod::~BasicThinningMethod() {}

void BasicThinningMethod::set(const ThinningMethodUI& ui) {
    factor_ = ui.factor_;
}

static SimpleObjectMaker<AutomaticThinningMethod, ThinningMethod> automatic("automatic");
static SimpleObjectMaker<BasicThinningMethod, BasicThinningMethod> basic("basic");
static SimpleObjectMaker<BasicThinningMethod, BasicThinningMethod> data("data");
