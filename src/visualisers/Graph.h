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

/*! \file Graph.h
    \brief Definition of the Template class Graph.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef Graph_H
#define Graph_H

#include "magics.h"

#include "Visdef.h"
#include "UserPoint.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "DateTime.h"
#include "GraphAttributes.h"

namespace magics {



struct DateToFloat {
	DateToFloat(const DateTime& reference) : reference_(reference) {}
	~DateToFloat() {}
	double operator()(const string& date) {
		DateTime val(date);
		return double(val - reference_);
	}
	const DateTime& reference_;
		 
};

class Graph :public GraphAttributes {

public:
	Graph() {}
	virtual ~Graph() {}
    
    virtual void set(const XmlNode& node ) { GraphAttributes::set(node); }
	virtual void set(const map<string, string>& map) { GraphAttributes::set(map); }
   
    virtual Graph* clone() const { return new Graph(); }
    virtual void toxml(ostream&)  const {}
	virtual void operator()(Data&, BasicGraphicsObjectContainer&) {}
	virtual void visit(LegendVisitor&) {}
	virtual void visit(TopAxisVisitor&) {}
	virtual void visit(Transformation&, Data&) {}
    void legend(bool legend, const string& text) { legend_ = legend; legend_text_= text; }


protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {} 
	 string legend_text_;
	 bool legend_;
private:
	//! Copy constructor - No copy allowed
	Graph(const Graph&);
	//! Overloaded << operator to copy - No copy allowed
	Graph& operator=(const Graph&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Graph& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, Graph> { 
public:
	Graph* operator()(const string& val ) {
		 return SimpleObjectMaker<Graph>::create(val);
	}
    
	Graph* magics(const string& param)
	{
		Graph* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
