/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TableDefinitionComputeInterface.h
    \brief Definition of the Template class TableDefinitionComputeInterface.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/

#ifndef TableDefinitionComputeInterface_H
#define TableDefinitionComputeInterface_H

#include "magics.h"


namespace magics {

template <class T>

class TableDefinitionComputeInterface {
public:
    TableDefinitionComputeInterface() {}
    virtual ~TableDefinitionComputeInterface() {}
    virtual int getCount() const = 0;
    virtual T getMin() const { return std::numeric_limits<T>::max(); }
    virtual T getMax() const { return std::numeric_limits<T>::min(); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    TableDefinitionComputeInterface(const TableDefinitionComputeInterface&);
    //! Overloaded << operator to copy - No copy allowed
    TableDefinitionComputeInterface& operator=(const TableDefinitionComputeInterface&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TableDefinitionComputeInterface& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
