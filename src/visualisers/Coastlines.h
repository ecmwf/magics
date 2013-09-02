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
