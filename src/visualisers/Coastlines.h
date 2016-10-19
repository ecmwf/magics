/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Coastlines.h
    \brief Definition of the Template class Coastlines.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 29-Jan-2004
    
    Changes:
    
*/

#ifndef Coastlines_H
#define Coastlines_H

#include "magics.h"

#include "SceneVisitor.h"
#include "CoastlinesAttributes.h"
#include "BasicSceneObject.h"
#include "MagicsEvent.h"

namespace magics {

class ProgressObject;
class StaticLayer;

class Coastlines: public CoastlinesAttributes, public BasicSceneObject {

public:
	Coastlines();
	virtual ~Coastlines();
    
	
	
	// New Interface!
	void visit(DrawingVisitor& list);	
	void visit(TextVisitor&);
	void visit(LegendVisitor& );
	void visit(LeftAxisVisitor& list);
	void visit(BottomAxisVisitor& list);
	void visit(TopAxisVisitor& list);
	void visit(RightAxisVisitor& list);
	void visit(MetaDataCollector& list);

	void visit(PreviewVisitor& list);
	void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors);
	void visit(Transformation& transformation);
	void set(const map<string, string>& map ) { CoastlinesAttributes::set(map); }
	void set(const XmlNode& node ) { CoastlinesAttributes::set(node); }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	StaticLayer* layer_;
private:
	//! Copy constructor - No copy allowed
	Coastlines(const Coastlines&);
	//! Overloaded << operator to copy - No copy allowed
	Coastlines& operator=(const Coastlines&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Coastlines& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
