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

/*! \file TableDefinitionList.cc
    \brief Implementation of the Template class TableDefinitionList.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#include "TableDefinitionList.h"
#include "XmlNode.h"

using namespace magics;


template <class T>
void TableDefinitionList<T>::set(const TableDefinitionListInterface<T>& attributes)
{
	const vector<T>& values = attributes.getValues();
	std::copy(values.begin(), values.end(), this->begin());
}

template <class T>
inline void fromString(const string& str, T& out) 
{
	std::stringstream ss(str);
	ss >> out;
}

template <class T>
void TableDefinitionList<T>::set(const XmlNode& node )
{
	MagLog::info() <<  "TableDefinitionList::set(const XmlNode&): to be implemented\n";
	MagLog::dev() << "Node to be interpreted ---> " << node << endl; 

	for (XmlNode::ElementIterator elt = node.firstElement(); elt != node.lastElement(); ++elt) {
		if ( magCompare((*elt)->name(), "value") ) {
			// convert the value ..
			double val;
			fromString((*elt)->data(), val);

			this->push_back(val);
		}
	}
}
