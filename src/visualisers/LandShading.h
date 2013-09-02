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

/*! \file LandShading.h
    \brief Definition of the Template class LandShading.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 7-Mar-2005
    
    Changes:
    
*/

#ifndef LandShading_H
#define LandShading_H

#include "magics.h"


#include "Transformation.h"
#include "LandShadingAttributes.h"
//#include "PolyCoast.h"

namespace magics {
	
class XmlNode;
class UserPoint;

class PolyCoast;




class LandShading: public LandShadingAttributes {

public:
	LandShading();
	virtual ~LandShading();
	virtual void set(const map<string, string>& map)
		{ LandShadingAttributes::set(map); }
	virtual void set(const XmlNode& node)
		{ LandShadingAttributes::set(node); }

  
    virtual LandShading* clone() const {
    	LandShading* object = new LandShading();
    	object->copy(*this);
    	return object;
    }
	
	void setBoundingBox(const Transformation&);

    virtual void operator()(PolyCoast*);
    virtual void sea(Polyline* sea, BasicGraphicsObjectContainer& drawing) { sea_->sea(sea, drawing); }
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	
    

private:
    //! Copy constructor - No copy allowed
	LandShading(const LandShading&);
    //! Overloaded << operator to copy - No copy allowed
	LandShading& operator=(const LandShading&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LandShading& p)
		{ p.print(s); return s; }

};


class NoLandShading: public LandShading {

public:
	NoLandShading() { colour_ = auto_ptr<Colour>(new Colour("none")); }
	virtual ~NoLandShading() {}
	
	virtual void set(const map<string, string>& map)
		{ LandShadingAttributes::set(map); colour_ = auto_ptr<Colour>(new Colour("none")); }
	virtual void set(const XmlNode& node)
		{ LandShadingAttributes::set(node); colour_ = auto_ptr<Colour>(new Colour("none")); }
	
	virtual LandShading* clone() const {
    	NoLandShading* object = new NoLandShading();
    	return object;
    }
	 virtual void operator()(PolyCoast* coast) {
		 if ( sea_->shading()  )
				 LandShading::operator()(coast);
	 }

};

template <>
class MagTranslator<string, LandShading> { 
public:
	LandShading* operator()(const string& val )
	{
		return SimpleObjectMaker<LandShading>::create(val);
	}     

	LandShading* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};


} // namespace magics
#endif
