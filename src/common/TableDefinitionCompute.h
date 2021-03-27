/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TableDefinitionCompute.h
    \brief Definition of the Template class TableDefinitionCompute.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/

#ifndef TableDefinitionCompute_H
#define TableDefinitionCompute_H

#include "magics.h"

#include "TableDefinition.h"
#include "TableDefinitionComputeInterface.h"

namespace magics {

template <class T>
class TableDefinitionCompute : public TableDefinition<T> {
public:
    TableDefinitionCompute() {}
    virtual ~TableDefinitionCompute() {}
    void set(const TableDefinitionComputeInterface<T>&);
    void set(const XmlNode&);
    void adjust(T min, T max);

    TableDefinition<T>* clone() const {
        TableDefinitionCompute<T>* object = new TableDefinitionCompute<T>();
        // What to do with the values!
        std::copy(this->begin(), this->end(), object->begin());
        return object;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}
    int count_;

private:
    //! Copy constructor - No copy allowed
    TableDefinitionCompute(const TableDefinitionCompute&);
    //! Overloaded << operator to copy - No copy allowed
    TableDefinitionCompute& operator=(const TableDefinitionCompute&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TableDefinitionCompute<T>& p) {
        p.print(s);
        return s;
    }
};

#include "TableDefinitionCompute.cc"

}  // namespace magics
#endif
