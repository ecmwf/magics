/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SimplePolylineInput.cc
    \brief Implementation of the Template class SimplePolylineInput.

    Magics Team - ECMWF 2007

    Started: Mon 19-Mar-2007

    Changes:

*/


#include "SimplePolylineInput.h"

using namespace magics;

SimplePolylineInput::SimplePolylineInput() {}


SimplePolylineInput::~SimplePolylineInput() {}

/*!
 Class information are given to the output-stream.
*/
void SimplePolylineInput::print(ostream& out) const {
    out << "SimplePolylineInput[";
    out << "]";
}

void SimplePolylineInput::decode() {
    if (latitudes_.empty() && longitudes_.empty() && values_.empty()) {
        ifstream position(position_filename_.c_str());
        double lat, lon;
        if (position) {
            while (!position.eof()) {
                position >> lon >> lat;
                if (!position.eof()) {
                    latitudes_.push_back(lat);
                    longitudes_.push_back(lon);
                }
            }
        }

        ifstream values(values_filename_.c_str());
        double value;
        if (values) {
            while (!values.eof()) {
                values >> value;
                if (!values.eof()) {
                    values_.push_back(value);
                }
            }
        }
    }

    floatarray::const_iterator lat = latitudes_.begin();
    floatarray::const_iterator lon = longitudes_.begin();
    floatarray::const_iterator val = values_.begin();

    int num_polylines = 0;

    if (!values_.empty()) {
        while (lat != latitudes_.end() && lon != longitudes_.end()) {
            if (same(*lat, breakvalue_) || same(*lon, breakvalue_)) {
                push_back(new UserPoint(0, 0, 0, true));
                num_polylines++;
                val++;

                // in the case where we don't have enough values,
                // we will repeat the last one

                if (val == values_.end()) {
                    val--;
                }
            }
            else
                push_back(new UserPoint(*lon, *lat, *val));
            lon++;
            lat++;
        }
    }

    // check for inconsistencies in numbers of points / values
}
