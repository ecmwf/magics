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

/*! \file HatchPolyShadingMethod.h
    \brief Definition of the Template class HatchPolyShadingMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 25-Aug-2004
    
    Changes:
    
*/

#ifndef HatchPolyShadingMethod_H
#define HatchPolyShadingMethod_H

#include "magics.h"

#include "PolyShadingMethod.h"
#include "HatchPolyShadingMethodAttributes.h"
#include "IntervalMap.h"

namespace magics {


class HatchPolyShadingMethod: public PolyShadingMethod, public HatchPolyShadingMethodAttributes {

public:
	HatchPolyShadingMethod() {}
	virtual ~HatchPolyShadingMethod() {}
	
	
    
    virtual void set(const map<string, string>& map) { HatchPolyShadingMethodAttributes::set(map); }
    virtual void set(const XmlNode& node) { HatchPolyShadingMethodAttributes::set(node); }
    virtual bool accept(const string& node) { return HatchPolyShadingMethodAttributes::accept(node); }
    virtual PolyShadingMethod* clone() const {
		HatchPolyShadingMethod* object = new HatchPolyShadingMethod();
		object->copy(*this);
		return object;
	}

    virtual void prepare(const LevelSelection& levels, const ColourTechnique&);
    virtual void operator()(Polyline& poly) const;

protected:  
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 
	 vector<int> hatches_;
private:
    //! Copy constructor - No copy allowed
	HatchPolyShadingMethod(const HatchPolyShadingMethod&);
    //! Overloaded << operator to copy - No copy allowed
	HatchPolyShadingMethod& operator=(const HatchPolyShadingMethod&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HatchPolyShadingMethod& p)
		{ p.print(s); return s; }

};

} // namespace magics

#endif
