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

/*! \file Streamlines.h
    \brief Definition of the Template class Streamlines.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#ifndef Streamlines_H
#define Streamlines_H

#include "magics.h"

#include "WindPlotting.h"
#include "StreamlinesAttributes.h"


namespace magics {

class Streamlines: public WindPlotting, public StreamlinesAttributes
{

public:
	Streamlines() {}
	virtual ~Streamlines() {}
	virtual void set(const map<string, string>& map) 
		{ WindPlottingAttributes::set(map); StreamlinesAttributes::set(map); }
	virtual void set(const XmlNode& node) 
		{ WindPlottingAttributes::set(node); StreamlinesAttributes::set(node); }
	bool accept(const string& node) { return StreamlinesAttributes::accept(node);; }
	void copy(const Streamlines& other) { WindPlottingAttributes::copy(other); StreamlinesAttributes::copy(other); }
	virtual WindPlotting* clone() {
		Streamlines* object = new Streamlines();
		object->copy(*this);
		return object;
	}
	
	bool operator()(Data& data, BasicGraphicsObjectContainer& parent);

	void visit(LegendVisitor& legend);
	

protected:     	 
     	 //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const;

private:
    //! Copy constructor - No copy allowed
	Streamlines(const Streamlines&);
    //! Overloaded << operator to copy - No copy allowed
	Streamlines& operator=(const Streamlines&);
    
	// -- Friends
	    //! Overloaded << operator to call print().
		friend ostream& operator<<(ostream& s,const Streamlines& p)
			{ p.print(s); return s; }
};

} // namespace magics


#endif
