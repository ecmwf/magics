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

/*! \file AxisControl.h
    \brief Definition of the Template class AxisControl.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 13-Oct-2005
    
    Changes:
    
*/

#ifndef AxisControl_H
#define AxisControl_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {


class Layout;
class Transformation;
class AxisMethod;

class AxisControl {



public:
	AxisControl();
	virtual ~AxisControl();
    
    virtual void set(const XmlNode&) { }
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string&) { return false; }
    virtual AxisControl* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new AxisControl();
    }
     virtual void toxml(ostream&)  const {}
    
    virtual void vertical(Layout&, Transformation&, AxisMethod&);
    virtual void horizontal(Layout&, Transformation&, AxisMethod&);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	AxisControl(const AxisControl&);
    //! Overloaded << operator to copy - No copy allowed
	AxisControl& operator=(const AxisControl&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AxisControl& p)
		{ p.print(s); return s; }

};

class AutomaticAxisControl : public AxisControl
{
public:
	AutomaticAxisControl() {}
	~AutomaticAxisControl() {}
	
protected:
	virtual AxisControl* clone() const {
        return new AutomaticAxisControl();
    }
    virtual void vertical(Layout&, Transformation&, AxisMethod&);
    virtual void horizontal(Layout&, Transformation&, AxisMethod&);
};

template <>
class MagTranslator<string, AxisControl> { 
public:
	AxisControl* operator()(const string& val)
	{
		return SimpleObjectMaker<AxisControl>::create(val);
	}     

	AxisControl* magics(const string& param)
	{
		AxisControl* object;
		ParameterManager::update(param, object);
		return object;
		
	}

};

} // namespace magics
#endif
