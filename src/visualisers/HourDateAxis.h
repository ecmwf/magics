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

/*! \file HourDateAxis.h
    \brief Definition of the Template class HourDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/

#ifndef HourDateAxis_H
#define HourDateAxis_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"


#include "HourDateAxisAttributes.h"
#include "DayDateAxis.h"
namespace magics {

class HourDateAxis: public DateAxisManipulator, public HourDateAxisAttributes {

public:
	HourDateAxis();
	virtual ~HourDateAxis();
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual HourDateAxis* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new HourDateAxis();
    }
    virtual void label(AxisItem&) const;
    
protected: 
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	HourDateAxis(const HourDateAxis&);
    //! Overloaded << operator to copy - No copy allowed
	HourDateAxis& operator=(const HourDateAxis&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HourDateAxis& p)
		{ p.print(s); return s; }

};


class NoHourDateAxis : public HourDateAxis
{
public: 
	NoHourDateAxis() {}
	~NoHourDateAxis() {}
	 virtual void position(int&) { }
	virtual void label(AxisItem&) const {}
protected:
	HourDateAxis* clone() const { return new NoHourDateAxis(); }
};

template <>
class MagTranslator<string, HourDateAxis> { 
public:
	HourDateAxis* operator()(const string& val )
	{
		return SimpleObjectMaker<HourDateAxis>::create(val);
	}     

	HourDateAxis* magics(const string& param)
	{
		HourDateAxis* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
