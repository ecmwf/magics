/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IntervalSelectionType.cc
    \brief Implementation of the Template class IntervalSelectionType.

    Magics Team - ECMWF 2004

    Started: Tue 9-Mar-2004

    Changes:

*/


#include "IntervalSelectionType.h"
#include "PointsHandler.h"
#include "UserPoint.h"

using namespace magics;

IntervalSelectionType::IntervalSelectionType() {}


IntervalSelectionType::~IntervalSelectionType() {}

/*!
 Class information are given to the output-stream.
*/
void IntervalSelectionType::print(ostream& out) const {
    out << "IntervalSelectionType[";
    IntervalSelectionTypeAttributes::print(out);
    out << "]";
}

#define MINSET(v) !same(v, -1.0e+21)
#define MAXSET(v) !same(v, +1.0e+21)
void IntervalSelectionType::calculate(double min, double max, bool shading) {
    clear();
    std::set<double> levels;

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
    if ( minOutOfBond_ ) {
        levels.insert(min_level);
        min_level = oob_min_;
    }
    
    if ( maxOutOfBond_ ) {
        levels.insert(max_level);
        max_level = oob_max_;
    }
            


    levels.insert(min_level);
    levels.insert(max_level);


    double level = reference_;
    double newref;

    int i = 1;
    while (level < max_level && !same(level, max_level)) {
        if (level > min_level)
            levels.insert(level);
        level = reference_ + (i * interval_);
        i++;
    }
    level = reference_;
    i     = 1;
    while (level > min_level && !same(level, min_level)) {
        if (level < max_level)
            levels.insert(level);
        level = reference_ - (i * interval_);
        i++;
    }

    ostringstream out;
    out << "\nIntervalSelectionType-->[";
    for (std::set<double>::const_iterator level = levels.begin(); level != levels.end(); ++level) {
        out << *level << ", ";
        push_back(*level);
    }
    out << "]" << endl;
    // cout  << out.str() << endl;

    
}

double IntervalSelectionType::reference(int freq) const {
    if (empty())
        return reference_;
    // Now make sure that the reference is inside the interval ..

    const_iterator reflev = find(begin(), end(), reference_);

    if (reflev != end())
        return reference_;

    vector<double> values;
    double val = reference_;
    if (reference_ < front()) {
        while (val < back()) {
            values.push_back(val);
            val += (freq * interval_);
        }
    }
    if (reference_ > back()) {
        while (val > front()) {
            values.push_back(val);
            val -= (freq * interval_);
        }
        // revert
        std::reverse(values.begin(), values.end());
    }

    set_intersection(begin(), end(), values.begin(), values.end(), values.begin());

    return values.front();
}
