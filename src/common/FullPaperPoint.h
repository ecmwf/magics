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
#ifndef FullPaperPoint_H
#define FullPaperPoint_H

#include "magics.h"

namespace magics {

class FullPaperPoint {
public:
    FullPaperPoint(double x = 0, double y = 0, double val = 0, double z = 0) :
        x_(x),
        y_(y),
        z_(z),
        value_(val),
        colour_(val),
        marker_(val),
        size_(val) {}
    virtual ~FullPaperPoint() {}

    void y(double y) { y_ = y; }
    void x(double x) { x_ = x; }
    void z(double z) { z_ = z; }
    void value(double val) { value_ = val; }
    void colour(double col) { colour_ = col; }
    void marker(double marker) { marker_ = marker; }
    void size(double size) { marker_ = size_; }


    inline double y() const { return y_; }
    inline double x() const { return x_; }
    inline double z() const { return z_; }
    double value() const { return value_; }
    double colour() const { return colour_; }
    double marker() const { return marker_; }
    double size() const { return size_; }

    bool operator==(const FullPaperPoint& other) const {
        return ((x_ == other.x()) && (y_ == other.y()) && (z_ == other.z()) && (value_ == other.value()) &&
                (colour_ == other.colour()) && (marker_ == other.marker()) && (size_ == other.size()));
    }

    double x_;
    double y_;

protected:
    virtual void print(ostream& out) const {
        out << "FullPaperPoint[";
        out << x_ << "(x), ";
        out << y_ << "(y), ";
        out << z_ << "(z), ";
        out << value_ << "(val)]";
    }

private:
    double z_;
    double value_;
    double colour_;
    double marker_;
    double size_;

    // -- Friends
    friend ostream& operator<<(ostream& s, const FullPaperPoint& p) {
        p.print(s);
        return s;
    }
};
}  // namespace magics
#endif
