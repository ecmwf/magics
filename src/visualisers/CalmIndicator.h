/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CalmIndicator.h
    \brief Definition of the Template class CalmIndicator.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#ifndef CalmIndicator_H
#define CalmIndicator_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "Symbol.h"


namespace magics {
	

class CalmIndicator {
public:
	CalmIndicator() { marker_ = 1; }
	virtual ~CalmIndicator() {}
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual bool accept(const string& node) { return magCompare(node, "calm"); }
	virtual void toxml(ostream&) {}
	virtual CalmIndicator* clone() { return new CalmIndicator(); }
	void colour(const Colour& colour) { colour_ = colour; }
	void marker(double marker)      { marker_ = marker; }
	void height(double height)      { height_ = height; }
	void below(double below)        { below_  = below * below; }
	
	
	virtual void prepare(BasicGraphicsObjectContainer& task) {
		calm_ = new Symbol();
		calm_->setColour(colour_);
	    calm_->setMarker(marker_);
	    calm_->setHeight(height_);
	    task.push_back(calm_);
	    
	    dot_ = new Symbol();
		dot_->setColour(colour_);
	    dot_->setMarker(15);
	    dot_->setHeight(height_/3.0);
		task.push_back(dot_);	
	}
		
	
	virtual bool operator()(const PaperPoint& point, double x, double y)
	{
		if ( ((x*x) + (y*y)) < below_ ) {
			calm_->push_back(point);
			dot_->push_back(point);
			return true;
		}
		return false; 
	}
	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "CalmIndicator"; } 
	 Symbol* calm_;
	 Symbol* dot_;
	 Colour     colour_;
	 int     marker_;
	 double   height_;
	 double   below_;
	 
private:
    //! Copy constructor - No copy allowed
	CalmIndicator(const CalmIndicator&);
    //! Overloaded << operator to copy - No copy allowed
	CalmIndicator& operator=(const CalmIndicator&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CalmIndicator& p)
		{ p.print(s); return s; }

};



class NoCalmIndicator : public CalmIndicator
{
public:
	NoCalmIndicator()  {}
	~NoCalmIndicator() {}
	virtual CalmIndicator* clone() { return new NoCalmIndicator(); }
	virtual bool accept(const string& node) { return magCompare(node, "nocalm"); }

	
	void prepare(BasicGraphicsObjectContainer&) {}
	virtual bool operator()(const PaperPoint&, double x, double y)
	{
	    return (((x*x) + (y*y)) < this->below_ ) ? true : false;
	}
	
	
};

template <>
class MagTranslator<string, CalmIndicator> { 
public:
	CalmIndicator* operator()(const string& val ) {
		 return SimpleObjectMaker<CalmIndicator >::create(val);
	}     
    CalmIndicator* magics(const string& param)
    {
        CalmIndicator* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};
} // namespace magics
#endif
