/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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

class Graph  {

public:
	Graph() {}
	virtual ~Graph() {}
    
    virtual void set(const XmlNode& node ) {  }
	virtual void set(const map<string, string>& map) {  }
	virtual bool accept(const std::string&) { return true; }
   
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
