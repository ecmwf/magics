/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisTip.h
    \brief Definition of the Template class NoAxisTip.
    
    Magics Team - ECMWF 2010
    
    Started: Tue 16-Nov-2010
    
    Changes:
    
*/

#ifndef NoAxisTip_H
#define NoAxisTip_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "AxisTipAttributes.h"


namespace magics {

class RightAxisVisitor;
class LeftAxisVisitor;
class TopAxisVisitor;
class BottomAxisVisitor;

class NoAxisTip {
	
public:
	NoAxisTip();
	virtual ~NoAxisTip();
	bool accept(const string&) { return false; }
	virtual void set(const XmlNode&) {}
	virtual void set(const map<string, string>&) {}
	virtual NoAxisTip* clone() const {return new NoAxisTip();}
	void toxml(ostream&) const {}
	virtual void horizontal(const Colour&, TopAxisVisitor&) const {}
	virtual void horizontal(const Colour&,BottomAxisVisitor&) const {}
	virtual void horizontal(const Colour&, RightAxisVisitor&) const {}
	virtual void horizontal(const Colour&, LeftAxisVisitor&) const {}

	virtual void vertical(const Colour&, TopAxisVisitor&) const {}
	virtual void vertical(const Colour&,BottomAxisVisitor&) const {}
	virtual void vertical(const Colour&, RightAxisVisitor&) const {}
	virtual void vertical(const Colour&, LeftAxisVisitor&) const {}

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	NoAxisTip(const NoAxisTip&);
	//! Overloaded << operator to copy - No copy allowed
	NoAxisTip& operator=(const NoAxisTip&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoAxisTip& p)
		{ p.print(s); return s; }
};


class AxisTip : public NoAxisTip, public AxisTipAttributes {

public:
	AxisTip();
	virtual ~AxisTip();

	virtual void set(const XmlNode& node) {AxisTipAttributes::set(node);}
	virtual void set(const map<string, string>& map) {AxisTipAttributes::set(map);}

	bool accept(const string& node) { return AxisTipAttributes::accept(node); }

	virtual NoAxisTip* clone() const
	{
		MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
		AxisTip* tip = new AxisTip();
		tip->copy(*this);
		return tip;
	}

	void horizontal(const Colour&, TopAxisVisitor&) const;
	void horizontal(const Colour&,BottomAxisVisitor&) const;
	void vertical(const Colour&, RightAxisVisitor&) const;
	void vertical(const Colour&, LeftAxisVisitor&) const;

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AxisTip& p)
		{ p.print(s); return s; }
};


template <>
class MagTranslator<string, NoAxisTip> { 
public:
	NoAxisTip* operator()(const string& val )
	{
		return SimpleObjectMaker<NoAxisTip>::create(val);
	}     

	NoAxisTip* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
