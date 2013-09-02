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

/*! \file GraphPlotting.h
    \brief Definition of the Template class GraphPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef GraphPlotting_H
#define GraphPlotting_H

#include "magics.h"

#include "GraphPlottingAttributes.h"
#include "Visdef.h"
#include "UserPoint.h"


namespace magics {
	

class DateTime;

class GraphPlotting: public GraphPlottingAttributes, public Visdef {

public:
	GraphPlotting();
	virtual ~GraphPlotting();
    
    // Implements the Visualiser interface  ... 
   
    void operator()(Data& data, BasicGraphicsObjectContainer& parent) {
    		return (*type_)(data, parent); }
    bool needLegend() { return legend_; }
    void visit(LegendVisitor& legend) { type_->legend(legend_, legend_text_); type_->visit(legend); }
    void visit(TopAxisVisitor& top) { type_->visit(top); }
    virtual void visit(Transformation& transformation, Data& data)  { type_->visit(transformation, data); }
    // Implements the set method ... 
    void set(const map<string, string>& map ) { GraphPlottingAttributes::set(map); type_->set(map); }
	void set(const XmlNode& node ) { GraphPlottingAttributes::set(node); type_->set(node);}

	
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
   
private:
    //! Copy constructor - No copy allowed
	GraphPlotting(const GraphPlotting&);
    //! Overloaded << operator to copy - No copy allowed
	GraphPlotting& operator=(const GraphPlotting&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GraphPlotting& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
