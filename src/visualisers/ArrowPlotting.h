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

/*! \file ArrowPlotting.h
    \brief Definition of the Template class ArrowPlotting.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#ifndef ArrowPlotting_H
#define ArrowPlotting_H

#include "magics.h"

#include "WindPlotting.h"
#include "ArrowPlottingAttributes.h"
#include "Arrow.h"

namespace magics {


class ArrowPlotting: public WindPlotting, public ArrowPlottingAttributes {

public:
	ArrowPlotting()  { }
	virtual ~ArrowPlotting() {}

	virtual void set(const map<string, string>& map) 
		{ WindPlottingAttributes::set(map); ArrowPlottingAttributes::set(map); }

	virtual void set(const XmlNode& node) 
		{ 
		WindPlottingAttributes::set(node); 
		ArrowPlottingAttributes::set(node); }
	
	void copy(const ArrowPlotting& other) { WindPlottingAttributes::copy(other); ArrowPlottingAttributes::copy(other); }
	bool accept(const string& node) { return ArrowPlottingAttributes::accept(node); }

	virtual WindPlotting* clone() {
		ArrowPlotting* object = new ArrowPlotting();
		object->copy(*this);
		return object;
	}
	
	Arrow* southArrow(const Colour&);
	Arrow* northArrow(const Colour&);
	double minSpeed() { return min_speed_; }
	double maxSpeed() { return max_speed_; }
	


	

	virtual void operator()(bool, const PaperPoint& point, double x, double y, double val);

	virtual void prepare(BasicGraphicsObjectContainer& out, double res);	
	virtual void prepare(BasicGraphicsObjectContainer&);	
	virtual void finish(BasicGraphicsObjectContainer&);
	void visit(LegendVisitor& legend);
	void visit(Data&, PointsHandler&, HistoVisitor&);
	
protected:
        //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const
	 	{ out << "ArrowPlotting<P>"; } 
	map<Colour, Arrow*> southArrows_;
	map<Colour, Arrow*> northArrows_;
	double  maxVelocity_;

private:
	//! Copy constructor - No copy allowed
	ArrowPlotting(const ArrowPlotting&);
	//! Overloaded << operator to copy - No copy allowed
	ArrowPlotting& operator=(const ArrowPlotting&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ArrowPlotting& p)
		{ p.print(s); return s; }
};

} // namespace magics


#endif
