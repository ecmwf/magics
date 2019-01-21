/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DriverStatic.h
    \brief Definition of the Template class DriverStatic.

    Magics Team - ECMWF 2006

    Started: Thu 21-Sep-2006

    Changes:

*/

#ifndef DriverStatic_H
#define DriverStatic_H

#include "magics.h"


namespace magics {

class DriverStatic {
public:
    DriverStatic();
    virtual ~DriverStatic();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    DriverStatic(const DriverStatic&);
    //! Overloaded << operator to copy - No copy allowed
    DriverStatic& operator=(const DriverStatic&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const DriverStatic& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
