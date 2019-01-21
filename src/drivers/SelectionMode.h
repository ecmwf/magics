/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SelectionMode.h
    \brief Definition of the Template class SelectionMode.

    Magics Team - ECMWF 2007

    Started: Tue 20-Nov-2007

    Changes:

*/

#ifndef SelectionMode_H
#define SelectionMode_H

#include "magics.h"

#include "SelectionModeAttributes.h"

namespace magics {

class SelectionMode : public SelectionModeAttributes {
public:
    SelectionMode();
    virtual ~SelectionMode();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    SelectionMode(const SelectionMode&);
    //! Overloaded << operator to copy - No copy allowed
    SelectionMode& operator=(const SelectionMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SelectionMode& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
