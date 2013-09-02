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

/*! \file DayDateAxis.h
    \brief Definition of the Template class DayDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/

#ifndef DayDateAxis_H
#define DayDateAxis_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "AxisItem.h"
#include "XmlNode.h"


#include "DayDateAxisAttributes.h"

namespace magics {


class DateAxisManipulator: public vector<AxisItem*>
{
public: 
    DateAxisManipulator() : position_(0) {}
	virtual ~DateAxisManipulator() {}
    
   
    
    virtual void labels(vector<AxisItem*>&);
    
    int position() { return position_; }
    virtual void position(int& position) { position_ = position++; }
    virtual void label(AxisItem&) const = 0;
    virtual double labelHeight() { return labelHeight_; }
    
protected:
	 string labelColour_;
	 double labelHeight_;
	 string date_format_;
	 int    position_;

private:
    
};


class DayDateAxis: public DateAxisManipulator, public DayDateAxisAttributes {

public:
	DayDateAxis();
	virtual ~DayDateAxis();
	
	virtual double labelHeight() { labelHeight_ = height_; return height_; }
	
	virtual bool accept(const string& node) { return DayDateAxisAttributes::accept(node); }
    
    virtual bool acceptNode(const string& node) { return DayDateAxisAttributes::accept(node); }
    
    virtual void set(const XmlNode& node) {
    	if ( acceptNode(node.name()) ) {
    		XmlNode setup = node;
    		setup.name("day");
        	DayDateAxisAttributes::set(setup);
    	}
    }
    virtual void set(const map<string, string>& map) {
         DayDateAxisAttributes::set(map);
    }
    virtual DayDateAxis* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new DayDateAxis();
    }
    
    virtual void label(AxisItem&) const;
    
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	

private:
    //! Copy constructor - No copy allowed
	DayDateAxis(const DayDateAxis&);
    //! Overloaded << operator to copy - No copy allowed
	DayDateAxis& operator=(const DayDateAxis&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const DayDateAxis& p)
		{ p.print(s); return s; }

};

class NoDayDateAxis : public DayDateAxis
{
public: 
	NoDayDateAxis() {}
	~NoDayDateAxis() {}
	virtual bool acceptNode(const string& node) { return magCompare(node, "off"); }
protected:
	DayDateAxis* clone() const { return new NoDayDateAxis(); }
	virtual void labels(vector<AxisItem*>&) {}
	virtual void position(int&) {}
	
};




template <>
class MagTranslator<string, DayDateAxis> { 
public:
	DayDateAxis* operator()(const string& val )
	{
		return SimpleObjectMaker<DayDateAxis>::create(val);
	}     

	DayDateAxis* magics(const string& param)
	{
		DayDateAxis* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
