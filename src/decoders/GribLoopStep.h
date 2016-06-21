/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribLoopStep.h
    \brief Definition of the Template class GribLoopStep
    
    Magics Team - ECMWF 2004
    
    Started: Tue 16-Mar-2004
    
    Changes:
    
*/

#ifndef GribLoop_H
#define GribLoop_H

#include "magics.h"
#include "MagException.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "DateGribLoopStepAttributes.h"
namespace magics {

class GribDecoder;
class XmlNode;
class LayerNode;

class GribLoopStep
{
public:
	GribLoopStep() {}
	virtual ~GribLoopStep() {}
	virtual void set(const map<string, string>& ) {}
	virtual void set(const XmlNode& ) {}
	virtual bool accept(const string& ) { return false;}
	//virtual void operator()(GribDecoder&) {}
	virtual GribLoopStep* clone() { return new GribLoopStep(); }
	virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << " const ---> to be checked!...\n";
    }

protected :
	virtual void print(ostream&) const;

private:
	//! Copy constructor - No copy allowed
	GribLoopStep(const GribLoopStep&);
	//! Overloaded << operator to copy - No copy allowed
	GribLoopStep& operator=(const GribLoopStep&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GribLoopStep& p)
		{ p.print(s); return s; }
};


class DateGribLoopStep : public GribLoopStep, public DateGribLoopStepAttributes
{
public:
	DateGribLoopStep() {}
	~DateGribLoopStep() {}
	void set(const XmlNode& node) { DateGribLoopStepAttributes::set(node); }
	bool accept(const string& node) { return DateGribLoopStepAttributes::accept(node);; }
	GribLoopStep* clone() { return new DateGribLoopStep(); }
	//virtual void operator()(GribDecoder&, LayerNode&);
};

class ParamGribLoopStep : public GribLoopStep
{
public:
	ParamGribLoopStep() {}
	~ParamGribLoopStep() {}
	//virtual void operator()(GribDecoder&, LayerNode&);
	GribLoopStep* clone() { return new ParamGribLoopStep(); }
};


template <>
class MagTranslator<string, GribLoopStep> { 
public:
	GribLoopStep* operator()(const string& val)
	{
		return SimpleObjectMaker<GribLoopStep>::create(val);
	}

	GribLoopStep* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};

} // namepace magics
#endif
