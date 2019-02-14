/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphPlotting.cc
    \brief Implementation of the Template class GraphPlotting.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#include "GraphPlotting.h"

using namespace magics;

GraphPlotting::GraphPlotting() {
    type_->legend(legend_, legend_text_);
}


GraphPlotting::~GraphPlotting() {}

/*!
 Class information are given to the output-stream.
*/
void GraphPlotting::print(ostream& out) const {
    out << "GraphPlotting[";
    out << "]";
}
