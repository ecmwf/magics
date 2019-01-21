/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file FloatSelection.h
    \brief Definition of the Template class FloatSelection.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef FloatSelection_H
#define FloatSelection_H

#include "magics.h"


namespace magics {

class FloatSelection : public vector<double> {
public:
    FloatSelection();
    virtual ~FloatSelection();
    virtual void prepare() {}


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    void calculate(double, double, int);
    void calculate(const doublearray&);


private:
    //! Copy constructor - No copy allowed
    FloatSelection(const FloatSelection&);
    //! Overloaded << operator to copy - No copy allowed
    FloatSelection& operator=(const FloatSelection&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const FloatSelection& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
