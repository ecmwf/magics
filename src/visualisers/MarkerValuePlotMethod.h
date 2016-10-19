/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MarkerValuePlotMethod.h
    \brief Definition of the Template class MarkerValuePlotMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 26-Aug-2004
    
    Changes:
    
*/

#ifndef MarkerValuePlotMethod_H
#define MarkerValuePlotMethod_H

#include "magics.h"

#include "ValuePlotMethod.h"
#include "MarkerValuePlotMethodAttributes.h"
#include "Symbol.h"
namespace magics {


class MarkerValuePlotMethod: public ValuePlotMethod, public MarkerValuePlotMethodAttributes {

public:
	MarkerValuePlotMethod() : marker_(0) {
       
	}
	virtual ~MarkerValuePlotMethod() {}
    virtual void set(const map<string, string>& map ) 
        { ValuePlotMethodAttributes::set(map);
          MarkerValuePlotMethodAttributes::set(map); }
    virtual void set(const XmlNode& node ) 
        { ValuePlotMethodAttributes::set(node);
          MarkerValuePlotMethodAttributes::set(node); }
    virtual ValuePlotMethod* clone() const {
    	MarkerValuePlotMethod* object = new MarkerValuePlotMethod();
    	object->clone(*this);
    	return object;
    }
    
     virtual void clone(const MarkerValuePlotMethod& from )
        { ValuePlotMethodAttributes::copy(from);
          MarkerValuePlotMethodAttributes::copy(from); }
    
    

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const {
         out << "MarkerValuePlotMethod[";
         MarkerValuePlotMethodAttributes::print(out);
         out << "]";
	 }
	 void reset() { marker_ = 0; }
     virtual void add(const PaperPoint& xy) {        
          if (!marker_) {
            marker_ = new Symbol();
            marker_->setMarker(markerIndex_);
            marker_->setColour(*markerColour_);
            marker_->setHeight(markerHeight_);
            this->push_back(marker_); 
         }   
         marker_->push_back(xy);
    }
    Symbol* marker_;

private:
    //! Copy constructor - No copy allowed
	MarkerValuePlotMethod(const MarkerValuePlotMethod&);
    //! Overloaded << operator to copy - No copy allowed
	MarkerValuePlotMethod& operator=(const MarkerValuePlotMethod&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MarkerValuePlotMethod& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
