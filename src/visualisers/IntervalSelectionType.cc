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

void IntervalSelectionType::calculate(double min, double max, bool shading) {
    clear();
    std::set<double> levels;

    vector<double> minbounds = { min, oob_min_, min_, };
    vector<double> maxbounds = { max, oob_max_, max_ };

    for (auto i = maxbounds.begin(); i != maxbounds.end(); ++i) {
        cout << "max " << *i << endl;
    }

    for (auto i = minbounds.begin(); i != minbounds.end(); ++i) {
        cout << "min " << *i << endl;
    }

    auto lmin = *std::max_element(minbounds.begin(), minbounds.end());
    auto lmax = *std::min_element(maxbounds.begin(), maxbounds.end());
    
    minOutOfBond_ = oob_min_ > std::max(min, min_);
    maxOutOfBond_ = oob_max_ < std::min(max, max_);
    if ( minOutOfBond_ ) {
            lmin = oob_min_;
            levels.insert(min);
            cout << "HERE" << endl;
    }
    
    if ( maxOutOfBond_ ) {
            lmax = oob_max_;
            levels.insert(max);
    }


    levels.insert(lmin);
    levels.insert(lmax);


    double level = reference_;
    double newref;

    int i = 1;
    while (level < lmax && !same(level, lmax)) {
        if (level > lmin)
            levels.insert(level);
        level = reference_ + (i * interval_);
        i++;
    }
    level = reference_;
    i     = 1;
    while (level > lmin && !same(level, lmin)) {
        if (level < lmax)
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
    cout  << out.str() << endl;

    // Now make sure that the reference is inside the interval ..
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
