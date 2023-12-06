/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LevelListSelectionType.cc
    \brief Implementation of the Template class LevelListSelectionType.

    Magics Team - ECMWF 2004

    Started: Wed 10-Mar-2004

    Changes:

*/


#include "LevelListSelectionType.h"

using namespace magics;

LevelListSelectionType::LevelListSelectionType() {}


LevelListSelectionType::~LevelListSelectionType() {}

/*!
 Class information are given to the output-stream.
*/
void LevelListSelectionType::print(ostream& out) const {
    out << "LevelListSelectionType[";
    LevelSelection::print(out);
    LevelListSelectionTypeAttributes::print(out);
    out << "]";
}

#define MINSET(v) !same(v, -1.0e+21)
#define MAXSET(v) !same(v, +1.0e+21)

void LevelListSelectionType::calculate(double min, double max, bool shading) {
    clear();

// Here have to take into account contour_min/max and comtour_shade_min/max : we take the max of the min and the min of the max. 

    double use_min = ( shade_min_ > min_ ) ? shade_min_ : min_;
    double use_max = ( shade_max_ < max_ ) ? shade_max_ : max_;


    doublearray::const_iterator last = list_.begin();
    double prevVal = use_min;
    for (doublearray::const_iterator val = list_.begin(); val != list_.end(); ++val) {
        MagLog::dev() << "LevelListSelectionType::calculate(double min, double max)--->" << *val << "\n";
        if (use_min <= *val && *val <= use_max) {
            if (*val < prevVal) {
                MagLog::error() << " level list values should increase, but " << *val << " follows " << prevVal << endl;
                break;
            }
            push_back(*val);
            prevVal = *val;
        }
        ++last;
    }

    // Just in case add another level to close the  last interval !
    if (last != list_.end())
        push_back(*last);


    ostringstream print;
    print << "LevelListSelectionType::calculate-->";
    string sep = "[";
    for (vector<double>::const_iterator val = begin(); val != end(); ++val) {
        print << sep << *val;
        sep = ", ";
    }
    print << "]";
    MagLog::dev() << print.str() << endl;
}
