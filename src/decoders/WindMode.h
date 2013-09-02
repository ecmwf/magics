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

/*! \file WindMode.h
    \brief Definition of the Template class WindMode.
    
    Magics Team - ECMWF 2006
    
    Started: Wed 9-Aug-2006
    
    Changes:
    
*/

#ifndef WindMode_H
#define WindMode_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "Layer.h"


namespace magics {

class WindMode {

public:
	WindMode();
	virtual ~WindMode();

    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }

    virtual bool accept(const string&) { return false; }
    virtual WindMode* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new WindMode();
    }
    
    virtual void toxml(ostream&) {}

    virtual void x(Matrix**,  Matrix**, Matrix*, Matrix*) {}
    virtual pair<double, double> operator()(double x, double y) { return make_pair(x, y); }

    virtual ValuesCollectorData* values(double lon, double lat, double x, double y, double dist)
    { return new ValuesCollectorData(lon, lat, x, dist); }
    
    virtual double norm(double x, double y) const {return x;}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	WindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
	WindMode& operator=(const WindMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const WindMode& p)
		{ p.print(s); return s; }

};

class UVWindMode : public WindMode 
{
public:
	UVWindMode() {}
	virtual ~UVWindMode() {}
	WindMode* clone() const { return new UVWindMode(); }
	virtual void x(Matrix** out,  Matrix**, Matrix* in1, Matrix* in2);
	virtual pair<double, double> operator()(double x, double y) { return make_pair(x, y); }
	ValuesCollectorData* values(double lon, double lat, double x, double y, double dist)
	{ return new ValuesCollectorUVData(lon, lat, x, y, dist); }
	double norm(double x, double y) const {return sqrt(x*x+y*y);}
private:
    //! Copy constructor - No copy allowed
	UVWindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
	UVWindMode& operator=(const WindMode&);
};

    
class SDWindMode : public WindMode 
{
public:
	SDWindMode() {}
	virtual ~SDWindMode() {}
	WindMode* clone() const { return new SDWindMode(); }
	virtual void x(Matrix** out, Matrix**, Matrix* in1, Matrix* in2);
	virtual pair<double, double> operator()(double x, double y);
	ValuesCollectorData* values(double lon, double lat, double x, double y, double dist)
		{ return new ValuesCollectorSDData(lon, lat, x, y, dist); }
	double norm(double x, double y) const {return x;}
private:
    //! Copy constructor - No copy allowed
	SDWindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
	SDWindMode& operator=(const SDWindMode&);
};

class VDWindMode : public WindMode 
{
public:
	VDWindMode() {}
	virtual ~VDWindMode() {}
	WindMode* clone() const { return new VDWindMode(); }
	virtual void x(Matrix** out, Matrix* in1, Matrix* in2);
	    virtual void y(Matrix** out, Matrix* in1, Matrix* in2);
private:
    //! Copy constructor - No copy allowed
	VDWindMode(const WindMode&);
    //! Overloaded << operator to copy - No copy allowed
	VDWindMode& operator=(const SDWindMode&);
};

template <>
class MagTranslator<string, WindMode> { 
public:
	WindMode* operator()(const string& val )
	{
		return SimpleObjectMaker<WindMode>::create(val);
	}

	WindMode* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};

} // namespace magics
#endif
