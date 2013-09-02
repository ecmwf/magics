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

/*! \file EpsForecastPlot.h
    \brief Definition of the Template class EpsForecastPlot.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 15-Dec-2005
    
    Changes:
    
*/

#ifndef EpsForecastPlot_H
#define EpsForecastPlot_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class EpsForecastPlot {

public:
	EpsForecastPlot() {}
	virtual ~EpsForecastPlot() {}
    
    virtual void set(const XmlNode&) {        
    }
    virtual void set(const map<string, string>&) {        
    }
    virtual EpsForecastPlot* clone() const {        
        return new EpsForecastPlot();
    }
    
    virtual bool forecast() { return true; }
    virtual bool control() { return true; }
    
     virtual void toxml(ostream&, int)  const {}
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
	EpsForecastPlot(const EpsForecastPlot&);
    //! Overloaded << operator to copy - No copy allowed
	EpsForecastPlot& operator=(const EpsForecastPlot&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EpsForecastPlot& p)
		{ p.print(s); return s; }

};

class EpsForecastOnly : public EpsForecastPlot
{
public:
	EpsForecastOnly() {}
	virtual ~EpsForecastOnly() {}
    
    virtual EpsForecastPlot* clone() const {        
        return new EpsForecastOnly();
    }
    
    virtual bool forecast() { return true; }
    virtual bool control() { return false; }
};

class EpsControlOnly : public EpsForecastPlot
{
public:
	EpsControlOnly() {}
	virtual ~EpsControlOnly() {}
    
    virtual EpsForecastPlot* clone() const {        
        return new EpsControlOnly();
    }
    
    virtual bool forecast() { return false; }
    virtual bool control() { return true; }
};

class EpsNoForecast : public EpsForecastPlot
{
public:
	EpsNoForecast() {}
	virtual ~EpsNoForecast() {}
    
    virtual EpsForecastPlot* clone() const {        
        return new EpsNoForecast();
    }
    
    virtual bool forecast() { return false; }
    virtual bool control() { return false; }
};

template <>
class MagTranslator<string, EpsForecastPlot> { 
public:
	EpsForecastPlot* operator()(const string& val )
	{
		return SimpleObjectMaker<EpsForecastPlot>::create(val);
	}     

	EpsForecastPlot* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
