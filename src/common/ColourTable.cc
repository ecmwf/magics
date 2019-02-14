/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTable.cc
    \brief Implementation of the Template class ColourTable.

    Magics Team - ECMWF 2005

    Started: Wed 6-Apr-2005

    Changes:

*/


#include "ColourTable.h"
#include "PaperPoint.h"
#include "Polyline.h"

using namespace magics;

ColourTable::ColourTable() {}


ColourTable::~ColourTable() {}

void ColourTable::prepare() {
#if 0  
	for ( int i = 1; i < 256; i++) 
		push_back(Rgb(1./i, 1./i, 1./i));
#else
    push_back(Colour(1., 0., 0.));
    push_back(Colour(0., 1., 0));
    push_back(Colour(0., 0., 1.));
    push_back(Colour(1., 1., 0.));
    push_back(Colour(1., 0., 1.));
    push_back(Colour(0., 1., 1.));
#endif
}
/*!
 Class information are given to the output-stream.
*/
void ColourTable::print(ostream& out) const {
    out << "ColourTable[\n";
    int i = 0;
    for (ColourIterator colour = begin(); colour != end(); ++colour) {
        out << "\t Colour " << i << ":" << *colour << "\n";
        i++;
    }
    out << "]\n";
}
/*
void ColourTable::visit(LegendEntryList& list) const
{
    for ( ColourIterator colour = begin(); colour != end(); ++colour )
        colour->visit(list);
}

void ColourTableEntry::visit(LegendEntryList& list) const
{

    Polyline*  box = new Polyline(0, "legend");
    FillShadingProperties* shading = new FillShadingProperties();
    box->setColour(colour_);

    shading->left_      = colour_;
    shading->right_     = colour_;
    shading->inColour_ = colour_;
    shading->outColour_ = Colour("NONE");

    box->setShading(shading);
    list.push_back(new BoxEntry(min_, max_, box));

}
*/
void ColourTableEntry::print(ostream& out) const {
    out << "[" << min_ << ", " << max_ << ", " << colour_ << "]";
}
