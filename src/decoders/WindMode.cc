/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file WindMode.cc
    \brief Implementation of the Template class WindMode.

    Magics Team - ECMWF 2006

    Started: Wed 9-Aug-2006

    Changes:

*/


#include "WindMode.h"

using namespace magics;

WindMode::WindMode() {}


WindMode::~WindMode() {}

/*!
 Class information are given to the output-stream.
*/
void WindMode::print(ostream& out) const {
    out << "WindMode[";
    out << "]";
}

void UVWindMode::x(Matrix& c1, Matrix& c2) {}


void SDWindMode::x(Matrix& c1, Matrix& c2) {
    double x                             = 3.14 / 180.;
    vector<double>::const_iterator speed = c1.begin();
    vector<double>::const_iterator angle = c2.begin();
    vector<double> speeds;
    vector<double> directions;
    //	MagLog::dev()<< "missing1-->" << in1->missing() << endl;
    //	MagLog::dev()<< "missing2-->" << in2->missing() << endl;
    while (speed != c1.end() && angle != c2.end()) {
        if (*speed == c1.missing() || *angle == c2.missing()) {
            speeds.push_back(c2.missing());
            directions.push_back(c2.missing());
        }
        else {
            double a = 90 - (*angle);
            a *= x;
            speeds.push_back(*speed * -1 * cos(a));
            directions.push_back(*speed * -1 * sin(a));
        }
        speed++;
        angle++;
    }

    c1.clear();
    c2.clear();

    vector<double>::iterator d    = directions.begin();
    vector<double>::iterator send = speeds.end();
    for (vector<double>::iterator s = speeds.begin(); s != send; ++s) {
        c1.push_back(*s);
        c2.push_back(*d);
        ++d;
    }
}


pair<double, double> SDWindMode::operator()(double s, double d) {
    double a = 90 - (d);
    double x = 3.14 / 180.;
    a *= x;
    return std::make_pair(s * -1 * cos(a), s * -1 * sin(a));
}


void VDWindMode::x(Matrix&, Matrix&) {}

void VDWindMode::y(Matrix&, Matrix&) {}
