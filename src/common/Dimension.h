/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Dimension.h
    \brief Definition of the Template class Dimension.

    Magics Team - ECMWF 2005

    Started: Thu 16-Jun-2005

    Changes:

*/

#ifndef Dimension_H
#define Dimension_H

#include "magics.h"


namespace magics {

class Dimension {
public:
    Dimension(const string&, double, double = 100.);
    virtual ~Dimension() {}
    double absolute() { return absolute_; }
    double percent() { return percent_; }
    string str() const {
        ostringstream percent;
        percent << percent_ << "%";
        return percent.str();
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "Dimension"; }
    double absolute_;
    double percent_;
    int pixel_;

private:
    //! Copy constructor - No copy allowed
    Dimension(const Dimension&);
    //! Overloaded << operator to copy - No copy allowed
    Dimension& operator=(const Dimension&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Dimension& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
