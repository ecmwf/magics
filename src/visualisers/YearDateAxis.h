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

/*! \file YearDateAxis.h
    \brief Definition of the Template class YearDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/

#ifndef YearDateAxis_H
#define YearDateAxis_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "DayDateAxis.h"


#include "YearDateAxisAttributes.h"

namespace magics {

class YearDateAxis: public YearDateAxisAttributes, public DateAxisManipulator {

public:
	YearDateAxis();
	virtual ~YearDateAxis();
    
	virtual void set(const XmlNode& node)
	{
		YearDateAxisAttributes::set(node);
	}
	virtual void set(const map<string, string>& map)
	{
		YearDateAxisAttributes::set(map);
		labelHeight_ = height_;
	}
	virtual YearDateAxis* clone() const 
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new YearDateAxis();
	}

	virtual void label(AxisItem&) const;
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	YearDateAxis(const YearDateAxis&);
    //! Overloaded << operator to copy - No copy allowed
	YearDateAxis& operator=(const YearDateAxis&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const YearDateAxis& p)
		{ p.print(s); return s; }

};

class NoYearDateAxis : public YearDateAxis
{
public: 
	NoYearDateAxis() {}
	~NoYearDateAxis() {}
protected:
	YearDateAxis* clone() const { return new NoYearDateAxis();}
	void label(AxisItem&) const {}
	void position(int&) {}
};

template <>
class MagTranslator<string, YearDateAxis> { 
public:
	YearDateAxis* operator()(const string& val )
	{
		return SimpleObjectMaker<YearDateAxis>::create(val);
	}     

	YearDateAxis* magics(const string& param)
	{
		YearDateAxis* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
