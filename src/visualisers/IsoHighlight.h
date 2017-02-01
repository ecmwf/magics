/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IsoHighlight.h
    \brief Definition of the Template class IsoHighlight.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 25-Aug-2004
    
    Changes:
    
*/

#ifndef IsoHighlight_H
#define IsoHighlight_H

#include "magics.h"

#include "IsoHighlightAttributes.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "Polyline.h"
#include "LevelSelection.h"

namespace magics {

class NoIsoHighlight  {

public:
	NoIsoHighlight() {}
	virtual ~NoIsoHighlight() {}
	virtual NoIsoHighlight* clone() const {
    	NoIsoHighlight* plot = new NoIsoHighlight();
    	return plot;
    }
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual void toxml(ostream&, int = 0) const {}
	virtual bool accept(const string&) { return true;}
		
    virtual void prepare(LevelSelection&) {} 
    virtual void visit(Polyline*& ) {
    
    } 
  
    
    virtual void operator()(Polyline&) {}
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
	NoIsoHighlight(const NoIsoHighlight&);
    //! Overloaded << operator to copy - No copy allowed
	NoIsoHighlight& operator=(const NoIsoHighlight&);
	// -- Friends
	    //! Overloaded << operator to call print().
		friend ostream& operator<<(ostream& s,const NoIsoHighlight& p)
			{ p.print(s); return s; }
};

class IsoHighlight: public NoIsoHighlight, public map<double, double>, public IsoHighlightAttributes {

public:
	IsoHighlight() {}
	virtual ~IsoHighlight() {}
	virtual NoIsoHighlight* clone() const {
    	IsoHighlight* plot = new IsoHighlight();
    	plot->copy(*this);
    	return plot;
    }
	virtual void set(const map<string, string>& map) { IsoHighlightAttributes::set(map); }
		virtual void set(const XmlNode& node) {  IsoHighlightAttributes::set(node); }

		virtual bool accept(const string& tag) { return  IsoHighlightAttributes::accept(tag);}
		 virtual void visit(Polyline*& line) {
			    line = new Polyline();
				line->setColour(*this->colour_);   
			    line->setLineStyle(style_);
				line->setThickness(this->thickness_);
				
		 }
    virtual void prepare(LevelSelection& levels) 
    { 
    	vector<double> todo;
    	clear();
    	levels.thinLevels(frequency_, todo);
    	for (LevelSelection::const_iterator level = todo.begin(); level != todo.end(); ++level) {
    	      (*this)[*level] = *level;
    	}
    }
    
    virtual void operator()(Polyline& poly) {
    	if (poly.empty()) return;
        
        //MagLog::dev() << "HIGHTLIGHT?--->" << point << "=" << point.value() << "\n";
        const_iterator high = find(poly.back().value());
        if ( high == end() ) return;
        poly.setColour(*colour_);
        poly.setLineStyle(style_);
        //MagLog::dev() << "set--->" << thickness_ << "\n";
        poly.setThickness(thickness_);
    }
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
	IsoHighlight(const IsoHighlight&);
    //! Overloaded << operator to copy - No copy allowed
	IsoHighlight& operator=(const IsoHighlight&);
    


};



template<>
class MagTranslator<string, NoIsoHighlight> { 
public:
	NoIsoHighlight* operator()(const string& val ) {
		 return SimpleObjectMaker<NoIsoHighlight>::create(val);
	}     
    NoIsoHighlight* magics(const string& param)
    {
        NoIsoHighlight* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};

} // namespace magics
#endif
