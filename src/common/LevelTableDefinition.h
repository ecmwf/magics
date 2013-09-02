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

/*! \file LevelTableDefinition.h
    \brief Definition of Colour class.
    
    Magics Team - ECMWF 2004
    
    Started by Sylvie Lamy-Thepaut ECMWF 2002
    
    Changes:
    
    
    
*/
#ifndef LevelTableDefinition_H
#define LevelTableDefinition_H

#include "TableDefinition.h"
#include "TableDefinitionList.h"
#include "TableDefinitionCompute.h"

#include "Factory.h"
#include "MagTranslator.h"


namespace magics {

class LevelTableDefinition : public TableDefinitionInterface<double> 
{
public:
	LevelTableDefinition() {}
	LevelTableDefinition* clone() const { return new LevelTableDefinition(); }
	 virtual void toxml(ostream&, int)  const {}
};

class LevelTableDefinitionList : public LevelTableDefinition
{
public:
	LevelTableDefinitionList()  { helper_ = new TableDefinitionList<double>(); }
	LevelTableDefinition* clone() const { return new LevelTableDefinitionList(); }	
};

class LevelTableDefinitionCompute : public LevelTableDefinition
{
public:
	LevelTableDefinitionCompute()  { helper_ = new TableDefinitionCompute<double>(); }
	LevelTableDefinition* clone() const { return new LevelTableDefinitionCompute(); }
};


template<>
class MagTranslator<string, LevelTableDefinition>
{
public:
	LevelTableDefinition* operator()(const string& val)
	{
		return SimpleObjectMaker<LevelTableDefinition>::create(val);
		 
	}
	LevelTableDefinition* magics(const string& param)
	{
		LevelTableDefinition* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics

#endif
