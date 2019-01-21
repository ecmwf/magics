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


void LevelListSelectionType::calculate(double, double, bool) {
    clear();


    doublearray::const_iterator last = list_.begin();
    for (doublearray::const_iterator val = list_.begin(); val != list_.end(); ++val) {
        MagLog::dev() << "LevelListSelectionType::calculate(double min, double max)--->" << *val << "\n";
        if (min_ <= *val && *val <= max_)
            push_back(*val);
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
