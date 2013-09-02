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

/*! \file ColourSelection.h
    \brief Definition of the Template class ColourSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef ColourSelection_H
#define ColourSelection_H

#include "magics.h"

#include "FloatSelection.h"
#include "MagTranslator.h"
#include "Factory.h"

namespace magics {

class XmlNode;

class ColourSelection: public FloatSelection {

public:
	ColourSelection();
	virtual ~ColourSelection();
    virtual void set(const map<string,string>&) {}
	virtual void set(const XmlNode&) {}
	 virtual void toxml(ostream&, int)  const {}
	virtual ColourSelection* clone() const {
    	ColourSelection* object = new ColourSelection();
    	return object;
    }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	ColourSelection(const ColourSelection&);
    //! Overloaded << operator to copy - No copy allowed
	ColourSelection& operator=(const ColourSelection&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourSelection& p)
		{ p.print(s); return s; }

};
template<>
class MagTranslator<string, ColourSelection> { 
public:
	ColourSelection* operator()(const string& val )
	{
		return SimpleObjectMaker<ColourSelection>::create(val);
	}     

	ColourSelection* magics(const string& param)
	{
		ColourSelection* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
