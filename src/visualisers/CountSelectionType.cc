/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file
.cc
    \brief Implementation of the Template class CountSelectionType.

    Magics Team - ECMWF 2004

    Started: Tue 9-Mar-2004

    Changes:

*/


#include "CountSelectionType.h"
#include "PointsHandler.h"
#include "UserPoint.h"

using namespace magics;


CountSelectionType::CountSelectionType() {}


void CountSelectionType::calculate(double min, double max, bool) {
    clear();
    double maxi = (max_ > 1000000000) ? max : max_;
    double mini = (min_ < -1000000000) ? min : min_;
    int i       = 0;

    i += (max_ > 1000000000) ? 1 : 0;
    i += (min_ < -1000000000) ? 1 : 0;


    double nb = levelCount_ - 1;
    if (same(maxi, mini)) {
        push_back(mini);
        return;
    }
    double step = (maxi - mini) / nb;
    double toleranceProportion =
        (double)tolerance_ /
        (double)(levelCount_ + 1 + i);  // tolerance as a proportion of the number of levels
                                        // we add 'i' here because it will be subtracted at the end (because this is the
                                        // number of levels we will not actually plot, ie the min/max contours).
    if (same(step, 0)) {
        // Only one isoline!
        push_back(mini);
        return;
    }


    // get the log of the step as an integer
    int log = log10(step);
    if (step < 1)
        log -= 1;

    double inc;
    double istep         = pow(10., log);
    double step0to10     = step / istep;
    double stepTolerance = step0to10 * toleranceProportion * 0.95;


    // we now have step0to10 as a number from 0 to 10, so we can
    // set it to the nearest 'nice' number for contouring, with
    // some bias for numbers such as 10 and 5.

    if (fabs(step0to10 - 10) < stepTolerance)
        inc = 10;
    else if (fabs(step0to10 - 5) < stepTolerance)
        inc = 5;
    else {
        inc = static_cast<int>(step0to10 + 0.5);  // round to nearest integer


        // will using this number be within our tolerance?
        // i.e. will this give us approx the right number of contours?
        // if not, then make an adjustment and test again. Do this
        // until the adjustments get too small (avoid infinite loops!)

        double diffFromIdeal = step0to10 - inc;
        double incAdjustment = 0.5;

        while (fabs(diffFromIdeal) > stepTolerance && incAdjustment > 0.1) {
            if (diffFromIdeal > stepTolerance) {
                inc += incAdjustment;
            }
            else if (diffFromIdeal < -stepTolerance) {
                inc -= incAdjustment;
            }

            diffFromIdeal = step0to10 - inc;
            incAdjustment /= 2.0;
        }
    }

    inc *= istep;  // convert back into proper range


    double first = floor(mini / inc) * inc;

    while (first > mini)
        first -= inc;

    push_back(mini);
    first += inc;
    double epsilon = inc / 10000.0;
    while (same(epsilon, 0)) {
        epsilon *= 10;
        EPSILON /= 10;
    }


    for (double val = first; val < maxi; val += inc) {
        // special case - if the value is close to zero then set it to precisely zero to avoid later formatting issues
        if (fabs(val) < epsilon)
            push_back(0.0);
        else if (same(val, maxi))
            push_back(maxi);
        else
            push_back(val);
    }

    if (maxi != back())
        push_back(maxi);
    int si = static_cast<int>(size());


    // last resort - if we have too many/few contours, then just do a 'blind' division
    // so as to ensure the correct number, even if they are not nice

    if (si - i < levelCount_ - tolerance_ || si - i > levelCount_ + tolerance_) {
        MagLog::debug() << "Not using nice levels[" << levelCount_ << ", " << tolerance_ << "]-->" << size() << endl;
        clear();
        step       = (maxi - mini) / (levelCount_ - 1);
        double val = mini;
        while (1) {
            push_back(val);
            if (same(val, maxi) || val > maxi)
                break;
            val += step;
        }
        MagLog::debug() << "New size-->" << size() << endl;
    }

    MagLog::debug() << "count=" << levelCount_ << ", tolerance=" << tolerance_ << ", result=" << size() << endl;

    ostringstream level;
    for (const_iterator l = begin(); l != end(); ++l)
        level << *l << " ";

    MagLog::debug() << level.str() << endl;
}


CountSelectionType::~CountSelectionType() {}

/*!
 Class information are given to the output-stream.
*/
void CountSelectionType::print(ostream& out) const {
    out << "CountSelectionType[";
    out << "]";
}

void CountSelectionType::set(const LevelSelectionInterface& from) {
    levelCount_ = from.getCount();
    tolerance_  = from.getTolerance();
    reference_  = from.getReference();
    min_        = from.getMin();
    max_        = from.getMax();
}
