/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
