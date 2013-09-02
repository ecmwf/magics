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

/*! \file Boundaries.h
    \brief Definition of the Template class NoBoundaries.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 29-Aug-2006
    
    Changes:
    
*/

#ifndef Boundaries_H
#define Boundaries_H

#include "BoundariesAttributes.h"
#include "BasicGraphicsObject.h"
#include "SceneVisitor.h"

namespace magics {
	
class NoBoundaries {

public:
	NoBoundaries();
	virtual ~NoBoundaries();
    
	virtual void set(const XmlNode&) {}
	virtual bool accept(const string&) { return false; }
	virtual void set(const map<string, string>&) {}
	virtual NoBoundaries* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new NoBoundaries();
	}

	virtual void toxml(ostream&, int = 0) const
	{
		MagLog::dev() << "NoBoundaries::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
	}

	virtual void operator()(const map<string, string>&, BasicGraphicsObjectContainer&) {}
    
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	NoBoundaries(const NoBoundaries&);
	//! Overloaded << operator to copy - No copy allowed
	NoBoundaries& operator=(const NoBoundaries&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoBoundaries& p)
		{ p.print(s); return s; }
};


template <>
class MagTranslator<string, NoBoundaries> { 
public:
	NoBoundaries* operator()(const string& val )
	{
		return SimpleObjectMaker<NoBoundaries>::create(val);
	}     

	NoBoundaries* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};


class Boundaries : public NoBoundaries, public BoundariesAttributes {

public:
	Boundaries();
	virtual ~Boundaries();
    
	virtual void set(const XmlNode& node)
	{
		BoundariesAttributes::set(node);
	}

	virtual void set(const map<string, string>& map)
	{
		BoundariesAttributes::set(map);
	}

	bool accept(const string& node) { return BoundariesAttributes::accept(node); }   

	virtual NoBoundaries* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		return new Boundaries();
	}

	void operator()(const map<string, string>&, BasicGraphicsObjectContainer&);

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	Boundaries(const Boundaries&);
	//! Overloaded << operator to copy - No copy allowed
	Boundaries& operator=(const Boundaries&);
};

} // namespace magics
#endif
