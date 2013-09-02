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

/*! \file MonthDateAxis.h
    \brief Definition of the Template class MonthDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/

#ifndef MonthDateAxis_H
#define MonthDateAxis_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "AxisItem.h"
#include "DayDateAxis.h"

#include "MonthDateAxisAttributes.h"

namespace magics {

class NoMonthDateAxis : public DateAxisManipulator
{
public: 
	NoMonthDateAxis() {}
	~NoMonthDateAxis() {}
	virtual void set(const XmlNode&) {}
	virtual void set(const map<string, string>& ) {}
	virtual bool accept(const string&) { return false; }

	virtual void toxml(ostream&) const {}
	virtual void labels(vector<AxisItem*>&) {}
	virtual void position(int&) {}
	virtual void label(AxisItem&) const {}
  	NoMonthDateAxis* clone() const { return new NoMonthDateAxis(); }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}
	
private:
	//! Copy constructor - No copy allowed
	NoMonthDateAxis(const NoMonthDateAxis&);
	//! Overloaded << operator to copy - No copy allowed
	NoMonthDateAxis& operator=(const NoMonthDateAxis&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoMonthDateAxis& p)
		{ p.print(s); return s; }
};

class MonthDateAxis: public MonthDateAxisAttributes, public NoMonthDateAxis {

public:
	MonthDateAxis();
	virtual ~MonthDateAxis();
    
    virtual void set(const XmlNode& node) {
        MonthDateAxisAttributes::set(node);
        labelHeight_ = height_;
    }
    virtual void set(const map<string, string>& map) {
        MonthDateAxisAttributes::set(map);
        labelHeight_ = height_;
    }
    bool accept(const string& node) {
        return MonthDateAxisAttributes::accept(node);
    }
    virtual NoMonthDateAxis* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new MonthDateAxis();
    }
    virtual void position(int& position) { position_ = position; position++; }
    virtual void label(AxisItem&) const;
    virtual void labels(vector<AxisItem*>& items) { 
    	DateAxisManipulator::labels(items); 
    }

    typedef void (MonthDateAxis::*Formatter)(AxisItem&) const;


    void one(AxisItem&) const;
    void full(AxisItem&) const;
    void three(AxisItem&) const;
    void user(AxisItem&) const;
    static map<string,  MonthDateAxis::Formatter > formatters_;
protected:
   virtual void print(ostream&) const;

};




template <>
class MagTranslator<string, NoMonthDateAxis> { 
public:
	NoMonthDateAxis* operator()(const string& val )
	{
		return SimpleObjectMaker<NoMonthDateAxis>::create(val);
	}     

	NoMonthDateAxis* magics(const string& param)
	{
		NoMonthDateAxis* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
