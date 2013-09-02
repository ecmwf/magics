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

/*! \file LandShadingBase.h
    \brief Definition of the Template class LandShadingBase.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 9-Feb-2006
    
    Changes:
    
*/

#ifndef LandShadingBase_H
#define LandShadingBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "BaseGraphicsObject.h"
#include "PolylineSet.h"
#include "UserPoint.h"


namespace magics {

class XmlNode;

class PolyCoast;
class Polyline;

class LandShadingBase {

public:
	LandShadingBase() : shading_(0) {}
	virtual ~LandShadingBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "LandShadingBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "LandShadingBase::set(const map<string, string&)---> to be checked!...\n";
    }
    
    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "LandShadingBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    } 
    virtual LandShadingBase* clone() const {
        MagLog::dev() << "LandShadingBase::set(const map<string, string&)---> to be checked!...\n";
        return new LandShadingBase();
    }
    virtual void operator()(PolyCoast*) {
    	MagLog::dev() << "LandShadingBase::perator()(Polyline*)---> to be checked!...\n";
    }	
    virtual void setBoundingBox(const Transformation&) {
    	MagLog::dev() << "LandShadingBase::setBoundingBox(const Transformation&)---> to be checked!...\n";
    }
    virtual void operator()(GraphicsList&) { }
	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "LandShadingBase\n"; } 
	  PolylineSet<UserPoint>* shading_;

private:
    //! Copy constructor - No copy allowed
	LandShadingBase(const LandShadingBase&);
    //! Overloaded << operator to copy - No copy allowed
	LandShadingBase& operator=(const LandShadingBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LandShadingBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, LandShadingBase> { 
public:
	LandShadingBase* operator()(const string& val )
	{
		return SimpleObjectMaker<LandShadingBase>::create(val);
	}     

	LandShadingBase* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
