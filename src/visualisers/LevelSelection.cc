/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LevelSelection.cc
    \brief Implementation of the Template class LevelSelection.

    Magics Team - ECMWF 2004

    Started: Tue 9-Mar-2004

    Changes:

*/

#include "LevelSelection.h"

using namespace magics;

LevelSelection::LevelSelection() {}


LevelSelection::~LevelSelection() {}

/*!
 Class information are given to the output-stream.
*/
void LevelSelection::print(ostream& out) const {
    out << "LevelSelection[";
    doublearray::print(out);
    out << "]";
}

void LevelSelection::thinLevels(int frequency, vector<double>& out) const {
    if (empty())
        return;
    double ref = reference(frequency);
    int count  = 0;

    // First is the reference in the levels list?
    const_iterator reflev = find(begin(), end(), ref);
    if (reflev == end())
        reflev = begin();

    for (const_iterator level = reflev; level != end(); ++level) {
        if (count % frequency == 0)
            out.push_back(*level);
        count++;
    }

    // now we have to reverse!
    const_reverse_iterator rlevel = rbegin();

    while (rlevel != rend()) {
        if (*rlevel == *reflev)
            break;
        ++rlevel;
    }
    count = 1;
    ++rlevel;

    for (; rlevel != rend(); ++rlevel) {
        if (count % frequency == 0) {
            out.push_back(*rlevel);
        }
        count++;
    }

    std::sort(out.begin(), out.end());
    ostringstream level;
    ostringstream sel;
    for (vector<double>::const_iterator l = begin(); l != end(); ++l)
        level << "  " << *l << "    ";

    for (vector<double>::iterator l = out.begin(); l != out.end(); ++l)
        sel << "  " << *l << "    ";

    MagLog::dev() << "Level --->" << level.str() << endl << "selection(" << frequency << ")--->" << sel.str() << endl;
}

double LevelSelection::reference(int) const {
    return empty() ? -9999 : front();
}
