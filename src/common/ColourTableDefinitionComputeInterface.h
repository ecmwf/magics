/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionComputeInterface.h
    \brief Definition of the Template class ColourTableDefinitionComputeInterface.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/

#ifndef ColourTableDefinitionComputeInterface_H
#define ColourTableDefinitionComputeInterface_H

#include "magics.h"

#include "Colour.h"


namespace magics {

class ColourTableDefinitionComputeInterface {
public:
    ColourTableDefinitionComputeInterface() {}
    virtual ~ColourTableDefinitionComputeInterface() {}

    virtual const Colour& getMax() const       = 0;
    virtual const Colour& getMin() const       = 0;
    virtual const string& getDirection() const = 0;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    ColourTableDefinitionComputeInterface(const ColourTableDefinitionComputeInterface&);
    //! Overloaded << operator to copy - No copy allowed
    ColourTableDefinitionComputeInterface& operator=(const ColourTableDefinitionComputeInterface&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ColourTableDefinitionComputeInterface& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
