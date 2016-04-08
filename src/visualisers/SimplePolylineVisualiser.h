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

/*! \file SimplePolylineVisualiser.h
    \brief Definition of the Template class SimplePolylineVisualiser.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef SimplePolylineVisualiser_H
#define SimplePolylineVisualiser_H
#include "magics.h"


#include "SimplePolylineAttributes.h"


#include "magics.h"
#include "UserPoint.h"
#include "Visdef.h"
#include "Data.h"

namespace magics {

class XmlNode;


class SimplePolylineVisualiser: public SimplePolylineAttributes, public Visdef {



public:
	SimplePolylineVisualiser();
	virtual ~SimplePolylineVisualiser();
    
    
    
	void operator()(Data&, BasicGraphicsObjectContainer&);
	void visit(Data&, LegendVisitor&);
	bool needLegend() { return legend_; }
    int getCount() const  { return count_; }
    int getTolerance() const { return tolerance_; }
    double getReference() const { return reference_; }
    double getInterval() const  { return interval_; }
    doublearray getList() const{ return list_; };
    double getMin() const { return min_; }
    double getMax() const{ return max_; }
    
    const Colour&  getMinColour() const { return *minColour_; }
    const Colour&  getMaxColour() const { return *maxColour_; }
    const string& getDirection() const{ return direction_; }
     stringarray getColours() const { return colours_; }

     ListPolicy getPolicy() const { return M_LASTONE; }
    // Implements the set method ... 
    void set(const map<string, string>& map ) { SimplePolylineAttributes::set(map); }
    void set(const XmlNode& node) { SimplePolylineAttributes::set(node); }

    void basic(Data&, BasicGraphicsObjectContainer&);
    void smooth(Data&, BasicGraphicsObjectContainer&);
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
    
    typedef void (SimplePolylineVisualiser::*Method)(Data&, BasicGraphicsObjectContainer&);
    map<string, Method> map_;

private:
    //! Copy constructor - No copy allowed
	SimplePolylineVisualiser(const SimplePolylineVisualiser&);
    //! Overloaded << operator to copy - No copy allowed
	SimplePolylineVisualiser& operator=(const SimplePolylineVisualiser&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SimplePolylineVisualiser& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
