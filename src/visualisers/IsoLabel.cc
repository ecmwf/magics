/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IsoLabel.cc
    \brief Implementation of the Template class IsoLabel.

    Magics Team - ECMWF 2004

    Started: Tue 9-Mar-2004

    Changes:

*/


#include "IsoLabel.h"
#include "Label.h"
#include "MagicsFormat.h"
#include "Polyline.h"
using namespace magics;

IsoLabel::IsoLabel() {
    methods_["text"]   = &IsoLabel::text;
    methods_["number"] = &IsoLabel::number;
    methods_["both"]   = &IsoLabel::both;
}


IsoLabel::~IsoLabel() {}

/*!
  Class information are given to the output-stream.
  */
void IsoLabel::print(ostream& out) const {
    out << "IsoLabel[";
    out << "]";
}

void IsoLabel::operator()(magics::Polyline& object, double l) const {
    const_iterator do_it = find(l);
    if (do_it == end()) {
        return;
    }
    Colour colour = (colour_ == "contour_line_colour") ? object.getColour() : Colour(colour_);
    std::map<string, Method>::const_iterator method = methods_.find(lowerCase(type_));

    string text;
    if (method == methods_.end()) {
        MagLog::warning() << "contour_label_type: " << type_ << " is unknown : use number instead" << endl;
        text = number(l);
    }
    else
        text = (this->*method->second)(l);

    Label label(text);

    label.setVisible(true);
    label.setHeight(height_);
    label.setBlanking(blanking_);
    MagFont font(font_, style_, height_);
    font.colour(colour);

    label.font(font);
    object.setLabel(label);
}


void NoIsoLabel::operator()(magics::Polyline& /*object*/, double) const {
    Label label("");
    label.setVisible(false);
}


string IsoLabel::number(double l) const {
    ostringstream nice;
    nice << MagicsFormat(format_, l);
    return nice.str();
}

string IsoLabel::text(double) const {
    return text_;
}

string IsoLabel::both(double l) const {
    return number(l) + " " + text_;
}
