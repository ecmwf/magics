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
		
    virtual void prepare(const LevelSelection&) {} 
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
    virtual void prepare(const LevelSelection& levels) 
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
