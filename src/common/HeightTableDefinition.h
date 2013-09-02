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

/*! \file HeightTableDefinition.h
    \brief Definition of Colour class.
    
    Magics Team - ECMWF 2004
    
    Started by Sylvie Lamy-Thepaut ECMWF 2002
    
    Changes:
    
    
    
*/
#ifndef HeightTableDefinition_H
#define HeightTableDefinition_H

#include "TableDefinition.h"
#include "TableDefinitionList.h"
#include "TableDefinitionCompute.h"

#include "Factory.h"
#include "MagTranslator.h"


namespace magics {


class HeightTableDefinition : public TableDefinitionInterface<double> 
{
public:
	HeightTableDefinition() {}
	HeightTableDefinition* clone() const { return new HeightTableDefinition(); }
	 virtual void toxml(ostream&, int)  const {}
};

class HeightTableDefinitionList : public HeightTableDefinition
{
public:
	HeightTableDefinitionList()  { helper_ = new TableDefinitionList<double>(); }
	HeightTableDefinition* clone() const { return new HeightTableDefinitionList(); }	
};

class HeightTableDefinitionCompute : public HeightTableDefinition
{
public:
	HeightTableDefinitionCompute()  { helper_ = new TableDefinitionCompute<double>(); }
	HeightTableDefinition* clone() const { return new HeightTableDefinitionCompute(); }

		
};


template<>
class MagTranslator<string, HeightTableDefinition> { 
public:
	HeightTableDefinition* operator()(const string& val)
	{
		return SimpleObjectMaker<HeightTableDefinition>::create(val);
		 
	}
    HeightTableDefinition* magics(const string& param)
    {
        HeightTableDefinition* object;
		ParameterManager::update(param, object);
		return object;
    }
};

} // namespace magics

#endif
