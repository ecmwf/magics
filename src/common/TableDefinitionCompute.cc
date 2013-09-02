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

/*! \file TableDefinitionCompute.cc
    \brief Implementation of the Template class TableDefinitionCompute.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#include "TableDefinitionCompute.h"
#include "XmlNode.h"

using namespace magics;


template <class T>
void TableDefinitionCompute<T>::set(const TableDefinitionComputeInterface<T>& attributes)
{
    count_ = attributes.getCount();
}

template <class T>
inline void fromString(const string& str, T& out) 
{
	std::stringstream ss(str);
	ss >> out;
}

template <class T>
void TableDefinitionCompute<T>::set(const XmlNode& node )
{
	MagLog::warning() <<  "TableDefinitionCompute::set(const XmlNode&): to be implemented\n";
	MagLog::dev() << "Node to be interpreted ---> " << node << endl; 

	fromString(node.getAttribute("count"), count_);
}

template <class T>
void TableDefinitionCompute<T>::adjust(T min, T max)
{
        double step = (max - min)/(count_+1);

        T log = log10(step);
        T istep = pow(10., int(log));	
        T inc = ceil(step/istep)*istep;
        T first = floor(min/inc)*inc;
  
       
        for (T val = first; val <= max + inc; val += inc)
	{
            MagLog::dev() << "Add level --->" << val << "\n";
            push_back(val);                
        }

        MagLog::dev() << "Numver of levels --->" << count_ << "---->" << this->size() << "\n";
}
