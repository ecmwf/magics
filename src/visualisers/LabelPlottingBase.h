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

/*! \file LabelPlottingBase.h
    \brief Definition of the Template class LabelPlottingBase.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 9-Feb-2006
    
    Changes:
    
*/

#ifndef LabelPlottingBase_H
#define LabelPlottingBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "BasicSceneObject.h"



namespace magics {

class XmlNode;
class GridPlottingBase;

class LabelPlottingBase {

public:
	LabelPlottingBase() {}
	virtual ~LabelPlottingBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "LabelPlottingBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "LabelPlottingBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "LabelPlottingBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    } 
    virtual LabelPlottingBase* clone() const {
        MagLog::dev() << "LabelPlottingBase::set(const map<string, string&)---> to be checked!...\n";
        return new LabelPlottingBase();
    }
    virtual void operator()(const BasicSceneObject&, GraphicsList&)
    {
    	MagLog::dev() << "LabelPlottingBase::preparePlot(Task&)---> to be checked!...\n";
    }
    
    virtual void prepare(GridPlottingBase&) {
    	MagLog::dev() << "LabelPlottingBase::prepare(GridPlotting&)---> to be checked!...\n";
    }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "LabelPlottingBase\n"; } 

private:
    //! Copy constructor - No copy allowed
	LabelPlottingBase(const LabelPlottingBase&);
    //! Overloaded << operator to copy - No copy allowed
	LabelPlottingBase& operator=(const LabelPlottingBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LabelPlottingBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, LabelPlottingBase> { 
public:
	LabelPlottingBase* operator()(const string& val )
	{
		return SimpleObjectMaker<LabelPlottingBase>::create(val);
	}     

	LabelPlottingBase* magics(const string& param)
	{
		LabelPlottingBase* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
