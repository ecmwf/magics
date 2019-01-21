/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HeightSelectionCount.h
    \brief Definition of the Template class HeightSelectionCount.

    Magics Team - ECMWF 2004

    Started: Thu 20-May-2004

    Changes:

*/

#ifndef HeightSelectionCount_H
#define HeightSelectionCount_H

#include "magics.h"

#include "HeightSelection.h"
#include "HeightSelectionCountAttributes.h"

namespace magics {

class HeightSelectionCount : public HeightSelection, public HeightSelectionCountAttributes {
public:
    HeightSelectionCount();
    virtual ~HeightSelectionCount();
    virtual void set(map<string, string> map) { HeightSelectionCountAttributes::set(map); }
    void prepare();

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    HeightSelectionCount(const HeightSelectionCount&);
    //! Overloaded << operator to copy - No copy allowed
    HeightSelectionCount& operator=(const HeightSelectionCount&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const HeightSelectionCount& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
