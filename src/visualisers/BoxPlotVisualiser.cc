/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotVisualiser.cc
    \brief Implementation of the Template class BoxPlotVisualiser.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/


#include "BoxPlotVisualiser.h"
#include "MagDateTime.h"
#include "PointsHandler.h"
#include "Text.h"


using namespace magics;


BoxPlotVisualiser::BoxPlotVisualiser() {}


BoxPlotVisualiser::~BoxPlotVisualiser() {}

/*!
 Class information are given to the output-stream.
*/
void BoxPlotVisualiser::print(ostream& out) const {
    out << "BoxPlotVisualiser[";
    out << "]";
}


void BoxPlotVisualiser::operator()(Data& data, BasicGraphicsObjectContainer& visitor) {
    CustomisedPointsList points;
    std::set<string> request;

    request.insert("min");
    request.insert("max");
    request.insert("lower");
    request.insert("upper");
    request.insert("median");

    const Transformation& transformation = visitor.transformation();
    data.customisedPoints(transformation, request, points, true);  // we want all the points!

    double user = (transformation.getAbsoluteMaxX() - transformation.getAbsoluteMinX()) / visitor.absoluteWidth();

    double max = transformation.getAbsoluteMaxY();
    double min = transformation.getAbsoluteMinY();


    if (points.empty())
        return;


    cm_ = user;



    for (const auto& point : points) {
        for (std::set<string>::iterator key = request.begin(); key != request.end(); ++key) {
            double val = (*point)[*key];
            if (val < min)
                (*point)[*key] = min;
            if (val > max)
                (*point)[*key] = max;
        }


        box(visitor, *point);

        if ( magCompare(whisker_, "line")) {
            whisker_line_top(visitor, *point);
            whisker_line_bottom(visitor, *point);
        }
         if ( magCompare(whisker_, "box")) {
            whisker_box_top(visitor, *point);
            whisker_box_bottom(visitor, *point);
        }
    }

    
    
}

void BoxPlotVisualiser::visit(LegendVisitor&) {
    // Not Yet!
}


void BoxPlotVisualiser::box(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const {
    if ( !box_)
        return;
    const Transformation& transformation = visitor.transformation();
    Polyline* box                        = new Polyline();
    box->setFilled(true);
    box->setFillColour(*box_colour_);
    box->setShading(new FillShadingProperties());

    double width                          = (box_width_ * cm_) / 2;  // Could later be expressed in %
    CustomisedPoint::const_iterator upper = point.find("upper");
    CustomisedPoint::const_iterator lower = point.find("lower");
    CustomisedPoint::const_iterator x     = point.find("x");
    if (x == point.end() || upper == point.end() || lower == point.end())
        return;
    box->push_back(transformation(UserPoint(x->second - width, upper->second)));
    box->push_back(transformation(UserPoint(x->second + width, upper->second)));
    box->push_back(transformation(UserPoint(x->second + width, lower->second)));
    box->push_back(transformation(UserPoint(x->second - width, lower->second)));
    box->push_back(transformation(UserPoint(x->second - width, upper->second)));

    box_border(*box);
    visitor.push_back(box);

    CustomisedPoint::const_iterator median = point.find("median");

    if (median == point.end())
        return;

    if ( median_ ) {
        Polyline* line = new Polyline();

        line->push_back(transformation(UserPoint(x->second - width, median->second)));
        line->push_back(transformation(UserPoint(x->second + width, median->second)));

        line->setColour(*median_colour_);
        line->setLineStyle(median_style_);
        line->setThickness(median_thickness_);
        visitor.push_back(line);
    }
}

void BoxPlotVisualiser::box_border(Polyline& box) const {
    if (!box_border_)
        return;
    box.setColour(*box_border_colour_);
    box.setLineStyle(box_border_style_);
    box.setThickness(box_border_thickness_);
}




void BoxPlotVisualiser::whisker_box_top(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const {
    const Transformation& transformation = visitor.transformation();
    Polyline* whisker                    = new Polyline();
    whisker->setFilled(true);
    whisker->setFillColour(*whisker_box_colour_);
    whisker->setShading(new FillShadingProperties());
    double width                          = (whisker_box_width_ * cm_) / 2;  // Could later be expressed in %
    CustomisedPoint::const_iterator max   = point.find("max");
    CustomisedPoint::const_iterator upper = point.find("upper");
    CustomisedPoint::const_iterator x     = point.find("x");
    if (x == point.end() || max == point.end() || upper == point.end())
        return;
    whisker->push_back(transformation(UserPoint(x->second - width, max->second)));
    whisker->push_back(transformation(UserPoint(x->second + width, max->second)));
    whisker->push_back(transformation(UserPoint(x->second + width, upper->second)));
    whisker->push_back(transformation(UserPoint(x->second - width, upper->second)));
    whisker->push_back(transformation(UserPoint(x->second - width, max->second)));

    whisker_border(*whisker);
    visitor.push_back(whisker);
}

void  BoxPlotVisualiser::whisker_box_bottom(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const {

    const Transformation& transformation = visitor.transformation();
    Polyline* whisker                    = new Polyline();
    whisker->setFilled(true);
    whisker->setFillColour(*whisker_box_colour_);
    whisker->setShading(new FillShadingProperties());
    double width                          = (whisker_box_width_ * cm_) / 2;  // Could later be expressed in %
    CustomisedPoint::const_iterator min   = point.find("min");
    CustomisedPoint::const_iterator lower = point.find("lower");
    CustomisedPoint::const_iterator x     = point.find("x");
    if (x == point.end() || min == point.end() || lower == point.end())
        return;
    whisker->push_back(transformation(UserPoint(x->second - width, min->second)));
    whisker->push_back(transformation(UserPoint(x->second + width, min->second)));
    whisker->push_back(transformation(UserPoint(x->second + width, lower->second)));
    whisker->push_back(transformation(UserPoint(x->second - width, lower->second)));
    whisker->push_back(transformation(UserPoint(x->second - width, min->second)));

    whisker_border(*whisker);
    visitor.push_back(whisker);
}


void BoxPlotVisualiser::whisker_line_top(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const {
    const Transformation& transformation = visitor.transformation();
    Polyline* whisker                    = new Polyline();
    whisker->setColour(*whisker_line_colour_);
    whisker->setLineStyle(whisker_line_style_);
    whisker->setThickness(whisker_line_thickness_);

    CustomisedPoint::const_iterator max   = point.find("max");
    CustomisedPoint::const_iterator upper = point.find("upper");
    CustomisedPoint::const_iterator x     = point.find("x");
    if (x == point.end() || max == point.end() || upper == point.end())
        return;
    whisker->push_back(transformation(UserPoint(x->second, max->second)));
    whisker->push_back(transformation(UserPoint(x->second, upper->second)));


    visitor.push_back(whisker);
}

void BoxPlotVisualiser::whisker_line_bottom(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const {
    const Transformation& transformation = visitor.transformation();
    Polyline* whisker                    = new Polyline();
    whisker->setColour(*whisker_line_colour_);
    whisker->setLineStyle(whisker_line_style_);
    whisker->setThickness(whisker_line_thickness_);

    CustomisedPoint::const_iterator min   = point.find("min");
    CustomisedPoint::const_iterator lower = point.find("lower");
    CustomisedPoint::const_iterator x     = point.find("x");
    if (x == point.end() || min == point.end() || lower == point.end())
        return;
    whisker->push_back(transformation(UserPoint(x->second, min->second)));
    whisker->push_back(transformation(UserPoint(x->second, lower->second)));


    visitor.push_back(whisker);
}

void BoxPlotVisualiser::whisker_border(Polyline& whisker) const {
    if ( ! whisker_box_border_ )
        return;
    whisker.setColour(*whisker_box_border_colour_);
    whisker.setLineStyle(whisker_box_border_style_);
    whisker.setThickness(whisker_box_border_thickness_);
}