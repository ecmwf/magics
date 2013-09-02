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

/*! \file ParameterManager.cc
 Magics Team - ECMWF 2004

*/

#include "ParameterManager.h"
#include "VectorOfPointers.h"

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
