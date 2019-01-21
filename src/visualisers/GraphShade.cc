/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphShade.cc
    \brief Implementation of the Template class GraphShade.

    Magics Team - ECMWF 2006

    Started: Thu 17-Aug-2006

    Changes:

*/


#include "GraphShade.h"
#include "PaperPoint.h"
#include "Polyline.h"
#include "ReverseIterable.h"
#include "UserPoint.h"

using namespace magics;

GraphShade::GraphShade() {}


GraphShade::~GraphShade() {}

/*!
 Class information are given to the output-stream.
*/
void GraphShade::print(ostream& out) const {
    out << "GraphShade[";
    GraphShadeAttributes::print(out);
    out << "]";
}


void GraphShade::operator()(Polyline& box) {
    (*style_)(box);
}

void GraphShade::legend(Polyline& box) {
    if (!box.empty()) {
        // we make a box!
        float height     = 0.5;
        PaperPoint front = box.front();
        PaperPoint back  = box.back();

        double xb = back.x();
        double yb = back.y();
        double xf = front.x();
        double yf = front.y();

        box.push_back(PaperPoint(xb, yb + height));
        box.push_back(PaperPoint(xf, yf + height));
        box.push_back(front);
    }
    (*style_)(box);
}
void GraphShade::operator()(CustomisedPointsList& points, vector<UserPoint>& out) {
    for (auto& point : points)
        out.push_back(UserPoint((*point)["x"], (*point)["y"]));

    for (const auto& rpoint : reverseIterable(points))
        out.push_back(UserPoint((*rpoint)["x2"], (*rpoint)["y2"]));

    //	CustomisedPointsList::const_reverse_iterator rpoint = points.rbegin();
    //	CustomisedPointsList::const_reverse_iterator last = points.rend();
    //
    //	while ( rpoint != last ) {
    //		out.push_back(UserPoint((**rpoint)["x2"], (**rpoint)["y2"]));
    //		rpoint++;
    //	}
}

void NoGraphShade::operator()(CustomisedPointsList& points, vector<UserPoint>& out) {
    for (auto& point : points) {
        CustomisedPoint::iterator x = point->find("x");
        CustomisedPoint::iterator y = point->find("y");
        if (x != point->end() && y != point->end()) {
            out.push_back(UserPoint(x->second, y->second));
            if (point->missing())
                out.back().flagMissing();
        }
    }
}


void NoGraphShade::print(ostream& out) const {
    out << "NoGraphShade[";
    out << "]";
}
