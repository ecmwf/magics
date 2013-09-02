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

/*! \file AxisMinorTick.h
    \brief Definition of the Template class AxisMinorTick.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 15-Dec-2005
    
    Changes:
    
*/

#ifndef AxisMinorTick_H
#define AxisMinorTick_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "AxisItem.h"

#include "AxisMinorTickAttributes.h"



namespace magics {

class HorizontalAxisVisitor;
class VerticalAxisVisitor;
class Transformation;

class AxisMinorTick: public AxisMinorTickAttributes {

public:
	AxisMinorTick();
	virtual ~AxisMinorTick();
    
    virtual void set(const XmlNode& node) {
        AxisMinorTickAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        AxisMinorTickAttributes::set(map);
    }
    virtual AxisMinorTick* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new AxisMinorTick();
    }
    
    virtual void vertical(const AxisItems&, const Colour&, VerticalAxisVisitor&);
    virtual void horizontal(const AxisItems&, const Colour&, HorizontalAxisVisitor&);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	AxisMinorTick(const AxisMinorTick&);
    //! Overloaded << operator to copy - No copy allowed
	AxisMinorTick& operator=(const AxisMinorTick&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AxisMinorTick& p)
		{ p.print(s); return s; }

};

class NoMinorAxisTick: public AxisMinorTick
{
public:
	NoMinorAxisTick() {}
	~NoMinorAxisTick() {};
protected:
	virtual void vertical(const AxisItems&, const Colour&, VerticalAxisVisitor&) {}
	virtual void horizontal(const AxisItems&, const Colour&, HorizontalAxisVisitor&) {} 
    
	AxisMinorTick* clone() const { return new NoMinorAxisTick(); }
    	
	
};
 

template <>
class MagTranslator<string, AxisMinorTick> { 
public:
	AxisMinorTick* operator()(const string& val )
	{
		return SimpleObjectMaker<AxisMinorTick>::create(val);
	}     

	AxisMinorTick* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
