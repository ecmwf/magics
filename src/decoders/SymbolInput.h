/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SymbolInput.h
    \brief Definition of the Template class SymbolInput.

    Magics Team - ECMWF 2005

    Started: Tue 19-Apr-2005

    Changes:

*/

#ifndef SymbolInput_H
#define SymbolInput_H

#include <stack>
#include "magics.h"


#include "Data.h"
#include "SymbolInputAttributes.h"


namespace magics {


class SymbolInput : public Data, public SymbolInputAttributes {
public:
    SymbolInput() {}
    virtual ~SymbolInput() {}

    // Implements the set method ...
    void set(const map<string, string>& map) { SymbolInputAttributes::set(map); }


    virtual void decodePoints() {
        if (value_.empty())
            for (doublearray::iterator x = x_.begin(); x != x_.end(); x++)
                value_.push_back(0);

        doublearray::iterator x = x_.begin();
        doublearray::iterator y = y_.begin();
        doublearray::iterator v = value_.begin();

        while (x != x_.end() && y != y_.end() && v != value_.end()) {
            points_.push_back(new UserPoint(*x, *y, *v));
            x++;
            y++;
            v++;
        }
    }

    virtual void decodePoints(const Transformation& projection) {
        if (value_.empty())
            for (doublearray::iterator x = x_.begin(); x != x_.end(); x++)
                value_.push_back(0);

        doublearray::iterator x = x_.begin();
        doublearray::iterator y = y_.begin();
        doublearray::iterator v = value_.begin();

        while (x != x_.end() && y != y_.end() && v != value_.end()) {
            UserPoint point(*x, *y, *v);
            std::stack<UserPoint> points;
            projection.wraparound(point, points);
            while (!points.empty()) {
                points_.push_back(new UserPoint(points.top()));
                points.pop();
            }
            x++;
            y++;
            v++;
        }
    }


    PointsHandler& points() {
        decodePoints();

        this->pointsHandlers_.push_back(new PointsHandler(points_));
        return (*this->pointsHandlers_.back());
    }

    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& out) {
        if (x_.empty())
            return;
        if (y_.empty())
            return;
        if (speed_.empty())
            return;
        if (direction_.empty())
            return;
        doublearray::iterator x         = x_.begin();
        doublearray::iterator y         = y_.begin();
        doublearray::iterator speed     = speed_.begin();
        doublearray::iterator direction = direction_.begin();
        doublearray::iterator v         = value_.empty() ? speed_.begin() : value_.begin();
        doublearray::iterator vend      = value_.empty() ? speed_.end() : value_.end();
        while (x != x_.end() && y != y_.end() && speed != speed_.end() && direction != direction_.end() && v != vend) {
            CustomisedPoint* point = new CustomisedPoint(*x, *y, "");
            double pi              = 3.14 / 180.;
            double a               = 90 - (*direction);
            a *= pi;
            double xc = *speed * -1 * cos(a);
            double yc = *speed * -1 * sin(a);
            point->insert(make_pair("x_component", xc));
            point->insert(make_pair("y_component", yc));
            point->insert(make_pair("colour_component", *v));
            ++x;
            ++y;
            ++speed;
            ++direction;
            ++v;
            out.push_back(point);
        }
    }


    PointsHandler& points(const Transformation& transformation) {
        decodePoints(transformation);

        this->pointsHandlers_.push_back(new PointsHandler(points_));
        return (*this->pointsHandlers_.back());
    }

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all) {
        customisedPoints(t, n, out);
    }
    PointsHandler& points(const Transformation& t, bool) { return points(t); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const {
        out << "SymbolInput[";
        SymbolInputAttributes::print(out);
        out << "]";
    }
    PointsList points_;

private:
    //! Copy constructor - No copy allowed
    SymbolInput(const SymbolInput&);
    //! Overloaded << operator to copy - No copy allowed
    SymbolInput& operator=(const SymbolInput&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolInput& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
