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

/*! \file Bar.h
    \brief Definition of the Template class Bar.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef Bar_H
#define Bar_H

#include "magics.h"

#include "BarAttributes.h"
#include "GraphFlagAttributes.h"
#include "GraphArrowAttributes.h"
#include "Curve.h"
#include "Polyline.h"

namespace magics {

class XmlNode;


class Bar: public BarAttributes, public Graph {

public:
	Bar();
	virtual ~Bar();
    // Implements the set method ...
    void set(const XmlNode& node);
    void set(const map<string, string>& map) { 
    	BarAttributes::set(map); 
    	Graph::set(map);
	}
    
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    void visit(TopAxisVisitor&);
    void visit(Transformation&, Data&);
    
    void fullbar(double, double, double, BasicGraphicsObjectContainer&);
    void linebar(double, double, double, BasicGraphicsObjectContainer&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 vector<Text*> texts_;

	 typedef void (Bar::*Renderer)(double, double, double, BasicGraphicsObjectContainer&);
	 map<string,  Renderer> renderers_;





private:
    //! Copy constructor - No copy allowed
	Bar(const Bar&);
    //! Overloaded << operator to copy - No copy allowed
	Bar& operator=(const Bar&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Bar& p)
		{ p.print(s); return s; }

};
class GraphFlag: public GraphFlagAttributes, public Graph {

public:
	GraphFlag();
	virtual ~GraphFlag();
    
    void set(const XmlNode& node) { 
    	GraphFlagAttributes::set(node); 
    	Graph::set(node);
    }
    
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    
    
    // Implements the set method ... 
    void set(const map<string, string>& map ) { GraphFlagAttributes::set(map); }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
 
    

private:
    //! Copy constructor - No copy allowed
	GraphFlag(const GraphFlag&);
    //! Overloaded << operator to copy - No copy allowed
	GraphFlag& operator=(const GraphFlag&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GraphFlag& p)
		{ p.print(s); return s; }

};

class GraphArrow: public GraphArrowAttributes, public Graph {

public:
	GraphArrow();
	virtual ~GraphArrow();
    
    void set(const XmlNode& node) { 
    	GraphArrowAttributes::set(node); 
    	Graph::set(node);
    }
    
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    
    
    // Implements the set method ... 
    void set(const map<string, string>& map ) { GraphArrowAttributes::set(map); }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
 


private:
    //! Copy constructor - No copy allowed
	GraphArrow(const GraphArrow&);
    //! Overloaded << operator to copy - No copy allowed
	GraphArrow& operator=(const GraphArrow&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GraphArrow& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
