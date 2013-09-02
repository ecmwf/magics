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
class TableDefinitionCompute: public TableDefinition<T> {

public:
	TableDefinitionCompute() {}
	virtual ~TableDefinitionCompute() {}
	void set(const TableDefinitionComputeInterface<T>&);
	void set(const XmlNode&);
	void adjust(T min, T max);

	TableDefinition<T>* clone() const
	{
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
	friend ostream& operator<<(ostream& s,const TableDefinitionCompute<T>& p)
		{ p.print(s); return s; }
};

#include "TableDefinitionCompute.cc"

} // namespace magics
#endif
