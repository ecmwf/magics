/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PolyShadingMethod.h
    \brief Definition of the Template class PolyShadingMethod.

    Magics Team - ECMWF 2004

    Started: Wed 18-Aug-2004

    Changes:

*/


#include "PolyShadingTechnique.h"
#include "IsoPlot.h"

using namespace magics;
void ShadingTechnique::operator()(IsoPlot* iso, MatrixHandler& data, BasicGraphicsObjectContainer& parent) {
    if (!done_)
        iso->isoline(data, parent);
}

CellArray* PolyShadingTechnique::array(MatrixHandler& matrix, IntervalMap<int>& range,
                                       const Transformation& transformation, int width, int height, float resolution,
                                       const string& technique) {
    done_ = true;
    return new CellArray(matrix, range, transformation, width, height, resolution, technique);
}
void GridShading::visit(LegendVisitor& legend, const ColourTechnique& colour) {
    MagLog::dev() << "Create legend information"
                  << "\n";
    LegendEntryBuilder helper(legend, colour);

    std::adjacent_find(colour.begin(), colour.end(), helper);

    if (colour.size() == 1) {
        helper(*colour.begin(), *colour.begin());
    }
    legend.last();  // Flag the last entry as being the last! To get a nice labelling in cotinuous mode!!!
}
CellArray* GridShading::array(MatrixHandler& matrix, IntervalMap<int>& range, const Transformation& transformation,
                              int width, int height, float resolution, const string& technique) {
    done_ = true;
    return new GridArray(matrix, range, transformation, width, height, resolution, position_);
}
void GridShading::operator()(magics::Polyline* poly) const {
    int index = poly->index();

    poly->setStroke(false);
    poly->setFilled(true);
    poly->setFillColour(method_->colours_[index]);
    FillShadingProperties* shading = new FillShadingProperties();
    poly->setShading(shading);
}
