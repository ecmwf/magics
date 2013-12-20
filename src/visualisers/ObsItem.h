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

/*! \file ObsItem.h
    \brief Definition of the Template class ObsItem.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 18-May-2005
    
    Changes:
    
*/

#ifndef ObsItem_H
#define ObsItem_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "CustomisedPoint.h"

#include "Symbol.h"



namespace magics {

class ObsPlotting;

class ObsItem {


public:
	ObsItem(): owner_(0) {}
	virtual ~ObsItem() {}
	
	virtual void set(const map<string, string>&) {}
	virtual void operator()(CustomisedPoint&, ComplexSymbol&) const {} 
	virtual void visit(std::set<string>&) {}
	
	string find(const map<string, string>& def, const string& keyword, const string& defaut = "")
	{
		map<string, string>::const_iterator val = def.find(keyword);
		return ( val != def.end() ) ?  val->second : defaut;
	}

	ObsPlotting* set(const ObsPlotting* owner) const { owner_ = owner; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "obsItem";  }
	mutable const ObsPlotting* owner_;


private:
    //! Copy constructor - No copy allowed
	ObsItem(const ObsItem&);
    //! Overloaded << operator to copy - No copy allowed
	ObsItem& operator=(const ObsItem&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ObsItem& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, ObsItem> { 
public:
	ObsItem* operator()(const string& val )
	{
		return SimpleObjectMaker<ObsItem>::create(val);
	}     

	ObsItem* magics(const string& param)
	{
		ObsItem* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
