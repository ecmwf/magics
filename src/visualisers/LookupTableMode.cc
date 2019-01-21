/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LookupTableMode.cc
    \brief Implementation of the Template class LookupTableMode.

    Magics Team - ECMWF 2005

    Started: Tue 17-May-2005

    Changes:

*/

#include "LookupTableMode.h"
#include "IntervalMap.h"

using namespace magics;

LookupTableMode::LookupTableMode() {}


LookupTableMode::~LookupTableMode() {}

/*!
 Class information are given to the output-stream.
*/
void LookupTableMode::print(ostream& out) const {
    out << "LookupTableMode[";
    LookupTableModeAttributes::print(out);
    out << "]";
}
FixedTableMode::FixedTableMode() {}


FixedTableMode::~FixedTableMode() {}

/*!
 Class information are given to the output-stream.
*/
void FixedTableMode::print(ostream& out) const {
    out << "FixedTableMode[";
    FixedTableModeAttributes::print(out);
    LookupTableModeAttributes::print(out);
    out << "]";
}

void FixedTableMode::operator()(Image& im, Raster& rd) {
    int i;  // auxiliary variables

    MagLog::dev() << *this << endl;

    // Initialize lookuptable
    //    ColourTable& table = im.getColourTable();
    // int nlevels = table.size();

    // Create a map of image levels interval
    IntervalMap<int> map;
    int level1 = levels_[0];

    for (i = 1; i < (int)levels_.size(); i++) {
        map[Interval(level1, levels_[i])] = indexes_[i - 1];
        level1                            = levels_[i];
    }
    map[Interval(level1, level1)] = indexes_[indexes_.size()];

    // Create output image

    for (vector<double>::const_iterator val = rd.begin(); val != rd.end(); ++val) {
        short ii = map.find(*val, 0);
        im.push_back(ii);
    }
    return;
}
