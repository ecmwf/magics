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

/*! \file HeightSelection.h
    \brief Definition of the Template class HeightSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef HeightSelection_H
#define HeightSelection_H

#include "magics.h"

#include "FloatSelection.h"
#include "MagTranslator.h"
#include "Factory.h"

namespace magics {

class XmlNode;

class HeightSelection: public FloatSelection {

public:
	HeightSelection();
	virtual ~HeightSelection();
    virtual void set(const map<string,string>&) {}
    virtual void set(const XmlNode&) {}
     virtual void toxml(ostream&, int)  const {}
    virtual HeightSelection* clone() const {
    	HeightSelection* object = new HeightSelection();
    	return object;
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	HeightSelection(const HeightSelection&);
    //! Overloaded << operator to copy - No copy allowed
	HeightSelection& operator=(const HeightSelection&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HeightSelection& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, HeightSelection> { 
public:
	HeightSelection* operator()(const string& val )
	{
		return SimpleObjectMaker<HeightSelection>::create(val);
	}     

	HeightSelection* magics(const string& param)
	{
		HeightSelection* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
