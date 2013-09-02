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

/*! \file BoxPlotVisualiser.h
    \brief Definition of the Template class BoxPlotVisualiser.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef BoxPlotVisualiser_H
#define BoxPlotVisualiser_H

#include "magics.h"


#include "BoxPlotVisualiserAttributes.h"


#include "magics.h"
#include "Polyline.h"
#include "Graph.h"

namespace magics {

class XmlNode;
class LegendVisitor;

class BoxPlotVisualiser: public BoxPlotVisualiserAttributes, public Visdef {



public:
	BoxPlotVisualiser();
	virtual ~BoxPlotVisualiser();
    
   
    
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    
    
    // Implements the set method ... 
    void set(const map<string, string>& map ) { BoxPlotVisualiserAttributes::set(map); }
    void set(const XmlNode& node) { BoxPlotVisualiserAttributes::set(node); }
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 double resolution_;
 
    

private:
    //! Copy constructor - No copy allowed
	BoxPlotVisualiser(const BoxPlotVisualiser&);
    //! Overloaded << operator to copy - No copy allowed
	BoxPlotVisualiser& operator=(const BoxPlotVisualiser&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BoxPlotVisualiser& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
