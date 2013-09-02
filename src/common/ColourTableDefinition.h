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

/*! \file ColourTableDefinition.h
    \brief Definition of the Template class ColourTableDefinition.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#ifndef ColourTableDefinition_H
#define ColourTableDefinition_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "ColourTable.h"

namespace magics {

class XmlNode;

class ColourTableDefinition {


public:
	ColourTableDefinition() {}
	virtual ~ColourTableDefinition() {}
	virtual void set(ColourTable&, int) {};
	virtual ColourTableDefinition* clone() const { return new ColourTableDefinition(); }
	
	virtual void set(const XmlNode&) { }
	virtual void set(const map<string, string>&) { }
	virtual bool accept(const string&) { return false; }
	
	virtual void toxml(ostream&)  const {}
	virtual void prepare() {}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 

private:
    //! Copy constructor - No copy allowed
	ColourTableDefinition(const ColourTableDefinition&);
    //! Overloaded << operator to copy - No copy allowed
	ColourTableDefinition& operator=(const ColourTableDefinition&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourTableDefinition& p)
		{ p.print(s); return s; }

};


template<>
class MagTranslator<string, ColourTableDefinition> { 
public:
	ColourTableDefinition* operator()(const string& val ) {
		 return SimpleObjectMaker<ColourTableDefinition>::create(val);
	}     
    ColourTableDefinition* magics(const string& param)
    {
        ColourTableDefinition* object;
		ParameterManager::update(param, object);
		return object;
    }

};

} // namespace magics

#endif
