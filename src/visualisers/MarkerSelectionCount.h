/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MarkerSelectionCount.h
    \brief Definition of the Template class MarkerSelectionCount.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef MarkerSelectionCount_H
#define MarkerSelectionCount_H

#include "magics.h"

#include "MarkerSelection.h"
#include "MarkerSelectionCountAttributes.h"

namespace magics {

class MarkerSelectionCount : public MarkerSelection, public MarkerSelectionCountAttributes {
public:
    MarkerSelectionCount();
    virtual ~MarkerSelectionCount();
    virtual void set(map<string, string> map) { MarkerSelectionCountAttributes::set(map); }
    void prepare();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    MarkerSelectionCount(const MarkerSelectionCount&);
    //! Overloaded << operator to copy - No copy allowed
    MarkerSelectionCount& operator=(const MarkerSelectionCount&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MarkerSelectionCount& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
