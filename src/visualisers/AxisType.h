/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisType.h
    \brief Definition of the Template class AxisType.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 7-May-2004
    
    Changes:
    
*/

#ifndef AxisType_H
#define AxisType_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class XmlNode;

class AxisType {

public:
	AxisType();
	virtual ~AxisType();
	virtual void set(const map<string, string>& ) { }
	virtual void set(const XmlNode& ) { }
	virtual AxisType* clone() const { return new AxisType(); }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	AxisType(const AxisType&);
	//! Overloaded << operator to copy - No copy allowed
	AxisType& operator=(const AxisType&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AxisType& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, AxisType> { 
public:
	AxisType* operator()(const string& val )
	{
		 return SimpleObjectMaker<AxisType>::create(val);
	}
   
	AxisType* magics(const string& param)
	{
		AxisType* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
