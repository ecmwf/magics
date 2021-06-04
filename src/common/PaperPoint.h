/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PaperPoint.h
    \brief Definition of Point (x, y, z, v).

    Magics Team - ECMWF 2004

    Started: Jan 2004

    Changes:

*/
#ifndef PaperPoint_H
#define PaperPoint_H

#include "magics.h"


namespace magics {


class PaperPoint {
public:
    PaperPoint(double x, double y, double value = 0, bool missing = false, bool border = false, int range = 0,
               const string& name = "") :
        x_(x),
        y_(y),
        value_(value),
        range_(range),
        name_(name),
        high_(false),
        low_(false),
        missing_(missing),
        border_(border) {}

    PaperPoint() : x_(0), y_(0), value_(0), name_(""), high_(false), low_(false), missing_(false), border_(false) {}

    void y(double y) { y_ = y; }
    void x(double x) { x_ = x; }

    bool operator<(const PaperPoint& other) const {
        if (same(y_, other.y_))
            return (x_ < other.x_);
        return (y_ < other.y_);
    }

    inline double y() const { return y_; }
    inline double x() const { return x_; }
    inline double value() const { return value_; }
    inline string name() const { return name_; }
    inline int range() const { return range_; }

    void flagMissing() { missing_ = true; }
    void flagBorder() { border_ = true; }
    bool missing() const { return missing_; }
    bool border() const { return border_; }
    bool ignore() const { return missing_ || border_; }

    bool operator==(const PaperPoint& other) const { return (same(x_, other.x()) && same(y_, other.y())); }

    double x_;
    double y_;
    double value_;
    int range_;
    string name_;

    bool in(double left, double right, double bottom, double top) const {
        if (y_ > top)
            return false;
        if (y_ < bottom)
            return false;
        if (x_ < left)
            return false;
        if (x_ > right)
            return false;
        return true;
    }
    void high(bool high) { high_ = high; }
    void low(bool low) { low_ = low; }
    bool high() const { return high_; }
    bool low() const { return low_; }
    double distance(const PaperPoint& other) const;

protected:
    bool high_;
    bool low_;
    bool missing_;
    bool border_;

    void print(ostream& out) const {
        out << "PaperPoint[";
        out << x_ << "(x), ";
        out << y_ << "(y)]";
    }

private:
    // -- Friends
    friend ostream& operator<<(ostream& s, const PaperPoint& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics

#endif
