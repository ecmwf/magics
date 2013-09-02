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

/*! \file ValuePlot.h
    \brief Definition of the Template class ValuePlot.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 3-Mar-2004
    
    Changes:
    
*/

#ifndef ValuePlot_H
#define ValuePlot_H

#include "magics.h"
#include "ValuePlotBase.h"
#include "ValuePlotAttributes.h"
#include "BasicSceneObject.h"
#include "MagTranslator.h"
#include "Factory.h"

namespace magics {


class ValuePlot: public ValuePlotBase, public ValuePlotAttributes {

public:
	ValuePlot();
	virtual ~ValuePlot();
   
    virtual ValuePlotBase* clone() const {
    	ValuePlot* plot = new ValuePlot();
    	plot->copy(*this);
    	return plot;
    }
    virtual bool accept(const string& node) { return ValuePlotAttributes::accept(node);; }

   // Implements the VisualComponent Interface...
    void operator()(MatrixHandler&, BasicGraphicsObjectContainer&);
    void operator()(Data&, BasicGraphicsObjectContainer&);


    virtual void visit(LegendVisitor&);
    virtual void set(const map<string, string>& map ) { ValuePlotAttributes::set(map); }
	virtual void set(const XmlNode& node ) { ValuePlotAttributes::set(node); }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	ValuePlot(const ValuePlot&);
    //! Overloaded << operator to copy - No copy allowed
	ValuePlot& operator=(const ValuePlot&);
   
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ValuePlot& p)
		{ p.print(s); return s; }

};



class NoValuePlot : public ValuePlotBase
{
public:
    NoValuePlot() {};
    ~NoValuePlot() {};
    ValuePlotBase* clone() const
    { 
    	return new NoValuePlot();
    }
    bool accept(const string& node) { return magCompare(node, "nogridvalues"); }
 
    void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) {}
    void operator()(PointsHandler&, BasicGraphicsObjectContainer&) {}
    void visit(LegendVisitor&) {}
};

} // namespace magics

#endif
