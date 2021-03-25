/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MarkerSelectionList.h
    \brief Definition of the Template class MarkerSelectionList.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef MarkerSelectionList_H
#define MarkerSelectionList_H

#include "magics.h"

#include "MarkerSelection.h"
#include "MarkerSelectionListAttributes.h"

namespace magics {

class MarkerSelectionList : public MarkerSelection, public MarkerSelectionListAttributes {
public:
    MarkerSelectionList();
    virtual ~MarkerSelectionList();
    virtual void set(map<string, string> map) { MarkerSelectionListAttributes::set(map); }
    void prepare();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    MarkerSelectionList(const MarkerSelectionList&);
    //! Overloaded << operator to copy - No copy allowed
    MarkerSelectionList& operator=(const MarkerSelectionList&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MarkerSelectionList& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
