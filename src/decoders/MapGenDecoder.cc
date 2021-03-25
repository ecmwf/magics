/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MapGenDecoder.cc
    \brief Implementation of the Template class MapGenDecoder.

    Magics Team - ECMWF 2005

    Started: Mon 12-Dec-2005

    Changes:

*/


#include "MapGenDecoder.h"
#include "CustomisedPoint.h"

using namespace magics;


MapGenDecoder::MapGenDecoder() {}


MapGenDecoder::~MapGenDecoder() {}

/*!
 Class information are given to the output-stream.
*/

void MapGenDecoder::print(ostream& out) const {
    out << "MapGenDecoder[";
    out << "]";
}


void MapGenDecoder::decode() {
    if (!this->empty())
        return;
    try {
        char line[1024];
        ifstream in(path_.c_str());
        float lat, lon;
        int i = 0;
        while (in.good()) {
            in.getline(line, 1024);

            string test(line);

            if (test.empty())
                continue;
            if (strncmp(line, "# -b", 4) == 0)
                if (record_ < 0) {
                    if (this->empty())
                        push_back(new PointsList());
                    this->back()->push_back(new UserPoint(0, 0, 0, true));
                }
                else
                    push_back(new PointsList());
            else {
                sscanf(line, "%f %f", &lon, &lat);
                this->back()->push_back(new UserPoint(lon, lat, i++));
            }
        }

        in.close();
    }
    catch (...) {
        MagLog::error() << "MapGen file : can not open " << path_ << endl;
    }
    MagLog::dev() << "Map gen file--->" << this->size() << endl;
}


void MapGenDecoder::customisedPoints(const std::set<string>&, magics::CustomisedPointsList& out) {
    PointsHandler& list = points();

    list.setToFirst();
    int i = 0;
    while (list.more()) {
        UserPoint point         = list.current();
        CustomisedPoint* cpoint = new CustomisedPoint();
        (*cpoint)["x"]          = point.x();
        (*cpoint)["y"]          = point.y();
        (*cpoint)["value"]      = i++;
        (*cpoint)["x_lower"]    = 0;
        (*cpoint)["y_lower"]    = 0;
        (*cpoint)["x_upper"]    = point.x();
        (*cpoint)["y_upper"]    = point.y();
        out.push_back(cpoint);
        list.advance();
    }
}
