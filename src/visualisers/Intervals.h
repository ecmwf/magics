/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Intervals.h
    \brief Definition of the Template class Intervals.

    Magics Team - ECMWF 2004

    Started: Wed 9-Jun-2004

    Changes:

*/

#ifndef Intervals_H
#define Intervals_H

#include "MagLog.h"
#include "magics.h"


namespace magics {

struct Interval {
    Interval(double min = 0, double max = 0) : min_(min), max_(max) {
        MagLog::debug() << "Interval[" << min_ << "," << max_ << "]"
                        << "\n";
    }
    bool between(double val) const {
        // MagLog::debug() << min_ << "<" << val << "<" << max_ << "?" << "\n";
        return (min_ <= val && val < max_);
    }
    double min_;
    double max_;
};


class Intervals : public vector<Interval> {
public:
    Intervals(const doublearray&);
    virtual ~Intervals();
    double find(double val) const;
    void set(const doublearray&);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    Intervals(const Intervals&);
    //! Overloaded << operator to copy - No copy allowed
    Intervals& operator=(const Intervals&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Intervals& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
