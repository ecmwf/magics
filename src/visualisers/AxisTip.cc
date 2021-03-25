/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisTip.h
    \brief Implementation of the Template class NoAxisTip.

    Magics Team - ECMWF 2010

    Started: Tue 16-Nov-2010

    Changes:

*/


#include "AxisTip.h"
#include "Text.h"
#include "Transformation.h"
using namespace magics;

NoAxisTip::NoAxisTip() {}


NoAxisTip::~NoAxisTip() {}

/*!
 Class information are given to the output-stream.
*/
void NoAxisTip::print(ostream& out) const {
    out << "NoAxisTip[";
    out << "]";
}

AxisTip::AxisTip() {}


AxisTip::~AxisTip() {}

/*!
 Class information are given to the output-stream.
*/
void AxisTip::print(ostream& out) const {
    out << "AxisTip[";
    AxisTipAttributes::print(out);
    out << "]";
}
void AxisTip::horizontal(const Colour&, TopAxisVisitor& out) const {}

void AxisTip::horizontal(const Colour& colour, BottomAxisVisitor& out) const {
    Text* text = new Text();

    MagFont font;
    font.size(height_);
    font.colour((colour_->automatic()) ? colour : *colour_);
    text->addText(text_, font);
    text->setVerticalAlign(MTOP);

    double x = out.maxX() - (out.maxX() - out.minX()) * 0.05;

    text->push_back(PaperPoint(x, out.offsetTitle()));

    out.push_back(text);
}

void AxisTip::vertical(const Colour& colour, LeftAxisVisitor& out) const {
    double angle   = out.angleTip();
    const double x = out.offsetTip();

    Text* text = new Text();
    MagFont font;
    font.size(height_);
    font.colour((colour_->automatic()) ? colour : *colour_);
    text->addText(text_, font);

    text->setAngle((orientation_ == "horizontal") ? 0 : angle);

    double y = out.maxY() - (out.maxY() - out.minY()) * 0.05;
    ;


    text->push_back(PaperPoint(x, y));

    out.push_back(text);
}

void AxisTip::vertical(const Colour&, RightAxisVisitor& out) const {}
