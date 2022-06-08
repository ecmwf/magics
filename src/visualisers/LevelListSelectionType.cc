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

    double min_level=min;
    double max_level=max;

    if ( MINSET(min_) )
        min_level = min_;
    
    if ( MAXSET(max_) )
        max_level = max_;

    if ( shading && MINSET(shade_min_) )
        min_level = shade_min_;
    
    if ( shading && MAXSET(shade_max_) )
        max_level = shade_max_;

    minOutOfBond_ = oob_min_ > min_level;
    maxOutOfBond_ = oob_max_ < max_level;   

    
   
    double mini = min_;
    double maxi = max_;
    if (minOutOfBond_) {
        push_back(min);
        push_back(oob_min_);
        min_level = oob_min_;
    }

    if (maxOutOfBond_) {
        max_level = oob_max_;
    }



    doublearray::const_iterator last = list_.begin();
    double prevVal = min_;
    for (doublearray::const_iterator val = list_.begin(); val != list_.end(); ++val) {
        MagLog::dev() << "LevelListSelectionType::calculate(double min, double max)--->" << *val << "\n";
        if (mini <= *val && *val <= maxi) {
            if (*val < prevVal) {
                MagLog::error() << " level list values should increase, but " << *val << " follows " << prevVal << endl;
                break;
            }
            push_back(*val);
            prevVal = *val;
        }
        ++last;
    }
    if (maxOutOfBond_) {
        push_back(oob_max_);
        push_back(max);
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
    cout << print.str() << endl;
}
