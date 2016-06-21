/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TableDefinition.h
    \brief Definition of the Template class TableDefinition.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#ifndef TableDefinition_H
#define TableDefinition_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"


namespace magics {

class XmlNode;

template <class T>

class TableDefinition : public vector<T> {


public:
    typedef vector<double>::const_iterator TableIterator; 
	TableDefinition() {}
	virtual ~TableDefinition() {}
	virtual TableDefinition* clone() const = 0;
	virtual void set(const XmlNode&) = 0;
	virtual void prepare() {}
	virtual void adjust(double, double) {}
	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 
	
private:
    //! Copy constructor - No copy allowed
	TableDefinition(const TableDefinition&);
    //! Overloaded << operator to copy - No copy allowed
	TableDefinition& operator=(const TableDefinition&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TableDefinition& p)
		{ p.print(s); return s; }

};



template <class T>
class TableDefinitionInterface
{
public:
	TableDefinitionInterface() : helper_(0) {}
	virtual void set(const XmlNode& node) 
		{  ASSERT(helper_); helper_->set(node); } 
	int size() 								{ ASSERT(helper_); return helper_->size(); }
	typename TableDefinition<T>::TableIterator begin() { ASSERT(helper_); return helper_->begin(); }
	typename TableDefinition<T>::TableIterator end()   { ASSERT(helper_); return helper_->end(); }
	void adjust(T min, T max)     { ASSERT(helper_); helper_->adjust(min, max); }
	bool empty()     							{ ASSERT(helper_); return helper_->empty(); }
	void push_back(T val)     			{ ASSERT(helper_); helper_->push_back(val); }

	
protected:
	TableDefinition<T>* helper_;

	virtual void print(ostream& out) const { out << *helper_ ; } 
	

private:
	//! Copy constructor - No copy allowed
	TableDefinitionInterface(const TableDefinitionInterface&);
	//! Overloaded << operator to copy - No copy allowed
	TableDefinitionInterface& operator=(const TableDefinitionInterface&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TableDefinitionInterface& p)
		{ p.print(s); return s; }
		
};




} // namespace magics

#endif
