/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file UserPoint.h
    \brief Definition of Point (x, y, z, v).

    Magics Team - ECMWF 2004

    Started: Jan 2004

    Changes:

*/
#ifndef UserPoint_H
#define UserPoint_H

#include <stack>
#include "magics.h"

namespace magics {

class UserPoint {
public:
    UserPoint(double x, double y, double value = 0, bool missing = false, bool border = false,
              const string& name = "") :
        x_(x), y_(y), value_(value), name_(name), high_(false), low_(false), missing_(missing), border_(border) {}
    // ~UserPoint() {}
    UserPoint() : x_(0), y_(0), value_(0), name_(""), high_(false), low_(false), missing_(false), border_(false) {}

    void y(double y) { y_ = y; }
    void x(double x) { x_ = x; }
    void value(double value) { value_ = value; }


    inline double y() const { return y_; }
    inline double x() const { return x_; }
    inline double value() const { return value_; }
    inline const string& name() const { return name_; }

    void flagMissing() { missing_ = true; }
    bool missing() const { return missing_; }

    void flagBorder() { border_ = true; }
    bool border() const { return border_; }

    bool ignore() const { return border_ || missing_; }

    bool operator==(const UserPoint& other) const { return (same(x_, other.x()) && same(y_, other.y())); }
    bool operator!=(const UserPoint& other) const { return !(*this == other); }
    double x_;
    double y_;
    double value_;
    string name_;
    void high(bool high) { high_ = high; }
    void low(bool low) { low_ = low; }
    bool high() const { return high_; }
    bool low() const { return low_; }

    UserPoint shift(double left, double right) const {
        UserPoint point(*this);
        if (left <= x_ && x_ <= right)
            return point;
        while (point.x_ <= left && point.x_ <= right)
            point.x_ += 360;
        while (point.x_ >= right && point.x_ > left)
            point.x_ -= 360;
        return point;
    }
    string asLongitude() const {
        ostringstream lon;

        string ew      = "&#176;";
        UserPoint nice = shift(-180, 180);

        if (nice.x_ < 0)
            ew += "W";
        if (nice.x_ >= 0)
            ew += "E";
        float x = float(maground(abs(nice.x_) * 100)) / 100;
        lon << x << ew;
        return lon.str();
    }

    string asLatitude() const {
        ostringstream lat;

        string ns = "&#176;";
        if (y_ < 0)
            ns += "S";
        if (y_ >= 0)
            ns += "N";
        float y = float(maground(abs(y_) * 100)) / 100;
        lat << y << ns;
        return lat.str();
    }

protected:
    bool high_;
    bool low_;
    bool missing_;
    bool border_;


    void print(ostream& out) const {
        out << "UserPoint[";
        out << x_ << "(x), ";
        out << y_ << "(y),";
        out << value_ << "(value)]";
    }

private:
    // -- Friends
    friend ostream& operator<<(ostream& s, const UserPoint& p) {
        p.print(s);
        return s;
    }
};
}  // namespace magics
#endif
