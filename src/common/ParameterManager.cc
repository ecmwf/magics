/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ParameterManager.cc
 Magics Team - ECMWF 2004

*/

#include "ParameterManager.h"

using namespace magics;

ParameterManager* ParameterManager::table_ = 0;

ParameterManager::ParameterManager() 
{
}


ParameterManager::~ParameterManager() 
{
	
}

	
void ParameterManager::print(ostream& out)  const
{
	out << "ParameterManager";
	string sep = "[";
	for (const_iterator entry = begin(); entry != end(); ++entry) { 
		out << sep << (*(*entry).second);
		sep = ",";
	}
	out << "]";
}
void ParameterManager::resetAll()
{
	for (iterator entry = begin(); entry != end(); ++entry) { 
		entry->second->reset();
	}
}
void  ParameterManager::add(const string& name, BaseParameter* param)  
{
	if ( !table_ )  table_ = new ParameterManager();
	(*table_)[name] = param;
}
