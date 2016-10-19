/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
