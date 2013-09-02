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

/*! \file DotPolyShadingMethod.h
    \brief Definition of the Template class DotPolyShadingMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 25-Aug-2004
    
    Changes:
    
*/

#ifndef DotPolyShadingMethod_H
#define DotPolyShadingMethod_H

#include "magics.h"
#include "MagException.h"

#include "PolyShadingMethod.h"
#include "DotPolyShadingMethodAttributes.h"
#include "IntervalMap.h"

namespace magics {


class DotPolyShadingMethod: public map<double, pair<double, double> >, public PolyShadingMethod, public DotPolyShadingMethodAttributes {

public:
	DotPolyShadingMethod() {}
	virtual ~DotPolyShadingMethod() {}
    
    virtual void set(const map<string, string>& map) { DotPolyShadingMethodAttributes::set(map); }
    virtual void set(const XmlNode& node) { DotPolyShadingMethodAttributes::set(node); }
    virtual bool accept(const string& node) { return DotPolyShadingMethodAttributes::accept(node); }


    virtual PolyShadingMethod* clone() const {
		DotPolyShadingMethod* object = new DotPolyShadingMethod();
		object->copy(*this);
		return object;
	}
    
    virtual void prepare(const LevelSelection& levels, const ColourTechnique& colours);

    virtual void operator()(Polyline& poly) const;
    	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 

	 vector<float> dots_;
private:
    //! Copy constructor - No copy allowed
	DotPolyShadingMethod(const DotPolyShadingMethod&);
    //! Overloaded << operator to copy - No copy allowed
	DotPolyShadingMethod& operator=(const DotPolyShadingMethod&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const DotPolyShadingMethod& p)
		{ p.print(s); return s; }

};

} // namespace magics
// Source in PolySghadingMethod.cc

#endif
