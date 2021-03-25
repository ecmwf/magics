/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionListInterface.h
    \brief Definition of the Template class ColourTableDefinitionListInterface.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/

#ifndef ColourTableDefinitionListInterface_H
#define ColourTableDefinitionListInterface_H

#include "magics.h"


namespace magics {

class ColourTableDefinitionListInterface {
public:
    ColourTableDefinitionListInterface() {}
    virtual ~ColourTableDefinitionListInterface() {}
    virtual stringarray getValues() const = 0;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    ColourTableDefinitionListInterface(const ColourTableDefinitionListInterface&);
    //! Overloaded << operator to copy - No copy allowed
    ColourTableDefinitionListInterface& operator=(const ColourTableDefinitionListInterface&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ColourTableDefinitionListInterface& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
