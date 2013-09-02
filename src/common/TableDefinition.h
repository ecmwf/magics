/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
		{  assert(helper_); helper_->set(node); } 
	int size() 								{ assert(helper_); return helper_->size(); }
	typename TableDefinition<T>::TableIterator begin() { assert(helper_); return helper_->begin(); }
	typename TableDefinition<T>::TableIterator end()   { assert(helper_); return helper_->end(); }
	void adjust(T min, T max)     { assert(helper_); helper_->adjust(min, max); }
	bool empty()     							{ assert(helper_); return helper_->empty(); }
	void push_back(T val)     			{ assert(helper_); helper_->push_back(val); }

	
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
