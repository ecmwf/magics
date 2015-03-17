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

/*! \file AxisMethod.h
    \brief Definition of the Template class AxisMethod.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/

#ifndef AxisMethod_H
#define AxisMethod_H

#include <cmath>

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "AxisItem.h"
#include "Transformation.h"
#include "AxisMethodAttributes.h"


namespace magics {

class Box;
class Axis;

class AxisMethod: public AxisMethodAttributes {

public:
	AxisMethod();
	virtual ~AxisMethod();

	AxisMethod* clone() { assert(false);  return 0;}
	virtual void set(const map<string, string>& map) {
		AxisMethodAttributes::set(map);
	}
	virtual void set(const XmlNode& node) {
			AxisMethodAttributes::set(node);
		}
    virtual void updateX(const Transformation&);
    virtual void updateY(const Transformation&);
    virtual void prepare(const Axis&, AxisItems&);


    double min() const { return min_; }
    double max() const { return max_; }

    virtual void addItem(AxisItems& items, double val, const string& format) const
    	{ items.push_back(new AxisItem(val, format));  }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 virtual double value(double val) const { return val; }
	 double min_;
	 double max_;

private:
    //! Copy constructor - No copy allowed
	AxisMethod(const AxisMethod&);
    //! Overloaded << operator to copy - No copy allowed
	AxisMethod& operator=(const AxisMethod&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AxisMethod& p)
		{ p.print(s); return s; }

};

class LogarithmicAxisMethod : public AxisMethod
{
public:
	LogarithmicAxisMethod() {}
	virtual ~LogarithmicAxisMethod() {}
	void     prepare(const Axis&, AxisItems&);
	
    double value(double val) const { return ::pow(10.,val); }
	

  
}; 
class HyperAxisMethod : public AxisMethod
{
public:
	HyperAxisMethod() {}
	virtual ~HyperAxisMethod() {}
	void     prepare(const Axis&, AxisItems&);
	
	
	

	 void updateX(const Transformation&);
	 void updateY(const Transformation&);
	 
protected:
	vector<double> hyperMin_;
	vector<double> hyperMax_;
	
		
}; 
class PositionListAxisMethod : public AxisMethod
{
public:
	PositionListAxisMethod() {}
	virtual ~PositionListAxisMethod() {}
	void     prepare(const Axis&, AxisItems&);
}; 



template <>
class MagTranslator<string, AxisMethod> { 
public:
	AxisMethod* operator()(const string& val )
	{
		return SimpleObjectMaker<AxisMethod>::create(val);
	}     

	AxisMethod* magics(const string& param)
	{
		AxisMethod* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
