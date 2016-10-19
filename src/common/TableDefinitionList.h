/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TableDefinitionList.h
    \brief Definition of the Template class TableDefinitionList.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#ifndef TableDefinitionList_H
#define TableDefinitionList_H

#include "magics.h"

#include "TableDefinition.h"
#include "TableDefinitionListInterface.h"


namespace magics {

template <class T>
class TableDefinitionList: public TableDefinition<T> {

public:
	TableDefinitionList() {}
	virtual ~TableDefinitionList() {}
	void set(const TableDefinitionListInterface<T>&);
	void set(const XmlNode&);

	TableDefinition<T>* clone() const {
		TableDefinitionList<T>* object = new TableDefinitionList();
		// What to do with the values!
		std::copy(this->begin(), this->end(), object->begin());
		return object;
	}
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
	//! Copy constructor - No copy allowed
	TableDefinitionList(const TableDefinitionList<T>&);
	//! Overloaded << operator to copy - No copy allowed
	TableDefinitionList& operator=(const TableDefinitionList<T>&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TableDefinitionList<T>& p)
		{ p.print(s); return s; }

};


} // namespace magics

#include "TableDefinitionList.cc"
#endif
