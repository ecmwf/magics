/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourSelectionCount.h
    \brief Definition of the Template class ColourSelectionCount.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef ColourSelectionCount_H
#define ColourSelectionCount_H

#include "magics.h"

#include "ColourSelection.h"
#include "ColourSelectionCountAttributes.h"

namespace magics {

class ColourSelectionCount : public ColourSelection, public ColourSelectionCountAttributes {
public:
    ColourSelectionCount();
    virtual ~ColourSelectionCount();
    virtual void set(map<string, string> map) { ColourSelectionCountAttributes::set(map); }
    virtual void prepare();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    ColourSelectionCount(const ColourSelectionCount&);
    //! Overloaded << operator to copy - No copy allowed
    ColourSelectionCount& operator=(const ColourSelectionCount&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ColourSelectionCount& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
