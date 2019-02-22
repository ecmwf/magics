/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Cities.cc
    \brief Implementation of the Template class CitiesBase.

    Magics Team - ECMWF 2006

    Started: Tue 29-Aug-2006

    Changes:
         2010 JUL change from mapgen to shp

*/

#include <memory>

#include "Cities.h"
#include "ShapeDecoder.h"
#include "Symbol.h"
#include "UserPoint.h"

using namespace magics;


Cities::Cities() {}

Cities::~Cities() {}


/*!
 Class information are given to the output-stream.
*/
void Cities::print(ostream& out) const {
    out << "Cities[";
    CitiesAttributes::print(out);
    out << "]";
}

struct Radius {
    Radius(double radius, CustomisedPoint& reference) : radius_(radius), reference_(reference) {}
    ~Radius() {}
    bool operator()(const std::unique_ptr<CustomisedPoint>& point) { return operator()(point.get()); }
    bool operator()(CustomisedPoint* point) {
        double dist = distance(*point, reference_);
        if (zero(dist))
            return false;
        return dist < radius_ * radius_;
    }

    double distance(CustomisedPoint& p1, CustomisedPoint& p2) {
        return ((p1["x"] - p2["x"]) * (p1["x"] - p2["x"])) + ((p1["y"] - p2["y"]) * (p1["y"] - p2["y"]));
    }

    double radius_;
    CustomisedPoint& reference_;
};


void clean(CustomisedPointsList& filter, CustomisedPointsList::iterator point) {
    double radius                       = 1;
    CustomisedPointsList::iterator last = std::remove_if(filter.begin(), filter.end(), Radius(radius, **point));
    filter.erase(last, filter.end());
    point++;
    if (point != filter.end())
        clean(filter, point);
}

double distance(CustomisedPoint* pp1, CustomisedPoint* pp2) {
    CustomisedPoint& p1 = *pp1;
    CustomisedPoint& p2 = *pp2;
    return ((p1["x"] - p2["x"]) * (p1["x"] - p2["x"])) + ((p1["y"] - p2["y"]) * (p1["y"] - p2["y"]));
}


void Cities::operator()(const map<string, string>&, BasicGraphicsObjectContainer& task) {
    static map<string, Symbol::TextPosition> positions;

    if (positions.empty()) {
        positions["above"] = Symbol::M_ABOVE;
        positions["below"] = Symbol::M_BELOW;
        positions["right"] = Symbol::M_RIGHT;
        positions["left"]  = Symbol::M_LEFT;
    }

    if (magCompare(unit_, "percent")) {
        marker_height_ = (marker_height_ / 100) * task.absoluteHeight();
        font_size_     = (font_size_ / 100) * task.absoluteHeight();
    }

    const double radius = 2. * font_size_ * task.transformation().unitToCm(task.absoluteWidth(), task.absoluteHeight());

    position_ = lowerCase(position_);

    map<string, Symbol::TextPosition>::iterator pos = positions.find(position_);

    Symbol::TextPosition position = (pos != positions.end()) ? pos->second : Symbol::M_ABOVE;

    string cities = buildConfigPath("10m", "ne_10m_populated_places_simple");
    std::set<string> need;
    CustomisedPointsList points;
    const Transformation& transformation = task.transformation();

    ShapeDecoder decoder;
    decoder.setPath(cities);

    TextSymbol* text = new TextSymbol();
    text->position(position);
    text->setSymbol(lowerCase(marker_));
    text->setHeight(marker_height_);
    text->setColour(*marker_colour_);
    MagFont font(font_name_, font_style_, font_size_);
    font.colour(*font_colour_);
    text->font(font);
    text->blanking(blanking_);
    decoder.customisedPoints(need, points);
    vector<CustomisedPoint*> filter;
    for (auto& point : points) {
        UserPoint geo(point->longitude(), point->latitude());
        if (transformation.in(geo) && !point->identifier().empty()) {
            PaperPoint xy = transformation(geo);
            (*point)["x"] = xy.x();
            (*point)["y"] = xy.y();
            filter.push_back(point.get());
        }
    }


    for (vector<CustomisedPoint*>::iterator point = filter.begin(); point != filter.end(); ++point) {
        vector<CustomisedPoint*>::iterator last = std::remove_if(filter.begin(), filter.end(), Radius(radius, **point));
        filter.erase(last, filter.end());
    }

    for (vector<CustomisedPoint*>::iterator point = filter.begin(); point != filter.end(); ++point) {
        UserPoint geo((*point)->longitude(), (*point)->latitude());
        text->push_back(transformation(geo), (*point)->identifier());
    }
    task.push_back(text);
}

NoCities::NoCities() {}

NoCities::~NoCities() {}

void NoCities::print(ostream& out) const {
    out << "NoCities[";
    out << "]";
}
