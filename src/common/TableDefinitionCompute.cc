/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
