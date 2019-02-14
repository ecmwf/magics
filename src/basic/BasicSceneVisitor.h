/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BasicSceneVisitor.h
    \brief Definition of the Template class BasicSceneVisitor.

    Magics Team - ECMWF 2008

    Started: Fri 19-Dec-2008

    Changes:

*/

#ifndef BasicSceneVisitor_H
#define BasicSceneVisitor_H

#include "magics.h"


namespace magics {

class BasicSceneVisitor {
public:
    BasicSceneVisitor();
    virtual ~BasicSceneVisitor();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    BasicSceneVisitor(const BasicSceneVisitor&);
    //! Overloaded << operator to copy - No copy allowed
    BasicSceneVisitor& operator=(const BasicSceneVisitor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BasicSceneVisitor& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
