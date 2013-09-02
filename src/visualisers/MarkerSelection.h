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

/*! \file MarkerSelection.h
    \brief Definition of the Template class MarkerSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef MarkerSelection_H
#define MarkerSelection_H

#include "magics.h"

#include "FloatSelection.h"
#include "MagTranslator.h"
#include "Factory.h"

namespace magics {

class XmlNode;

class MarkerSelection: public FloatSelection {

public:
	MarkerSelection();
	virtual ~MarkerSelection();
    virtual void set(const map<string,string>&) {}
    virtual void set(const XmlNode&) {}
    virtual void toxml(ostream&, int)  const {}
    virtual MarkerSelection* clone() const {
    	MarkerSelection* object = new MarkerSelection();
    	return object;
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	MarkerSelection(const MarkerSelection&);
    //! Overloaded << operator to copy - No copy allowed
	MarkerSelection& operator=(const MarkerSelection&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MarkerSelection& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, MarkerSelection> { 
public:
	MarkerSelection* operator()(const string& val )
	{
		return SimpleObjectMaker<MarkerSelection>::create(val);
	}     

	MarkerSelection* magics(const string& param)
	{
		MarkerSelection* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
