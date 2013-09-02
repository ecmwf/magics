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

/*! \file Cities.h
    \brief Definition of the Template class NoCities.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 29-Aug-2006
    
    Changes:
    
*/

#ifndef Cities_H
#define Cities_H

#include "CitiesAttributes.h"
#include "BasicGraphicsObject.h"
#include "SceneVisitor.h"

namespace magics {
	
class NoCities {

public:
	NoCities();
	virtual ~NoCities();
    
	virtual void set(const XmlNode&) {}
	virtual bool accept(const string&) { return false; }
	virtual void set(const map<string, string>&) {}
	virtual NoCities* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new NoCities();
	}

	virtual void toxml(ostream&, int = 0) const
	{
		MagLog::dev() << "NoCities::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
	}

	virtual void operator()(const map<string, string>&, BasicGraphicsObjectContainer&) {}
    
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	NoCities(const NoCities&);
	//! Overloaded << operator to copy - No copy allowed
	NoCities& operator=(const NoCities&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoCities& p)
		{ p.print(s); return s; }
};


template <>
class MagTranslator<string, NoCities> { 
public:
	NoCities* operator()(const string& val )
	{
		return SimpleObjectMaker<NoCities>::create(val);
	}     

	NoCities* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};


class Cities : public NoCities, public CitiesAttributes {

public:
	Cities();
	virtual ~Cities();
    
	virtual void set(const XmlNode& node)
	{
		CitiesAttributes::set(node);
	}

	virtual void set(const map<string, string>& map)
	{
		CitiesAttributes::set(map);
	}

	bool accept(const string& node) { return CitiesAttributes::accept(node); }   

	virtual NoCities* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new Cities();
	}

	void operator()(const map<string, string>&, BasicGraphicsObjectContainer&);

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	Cities(const Cities&);
	//! Overloaded << operator to copy - No copy allowed
	Cities& operator=(const Cities&);
};

} // namespace magics
#endif
