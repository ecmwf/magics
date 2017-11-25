/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BothValuePlotMethod.h
    \brief Definition of the Template class BothValuePlotMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 26-Aug-2004
    
    Changes:
    
*/

#ifndef BothValuePlotMethod_H
#define BothValuePlotMethod_H

#include "magics.h"

#include "ValuePlotMethod.h"
#include "ValuePlotMethodAttributes.h"
#include "BothValuePlotMethodAttributes.h"
#include "Symbol.h"

namespace magics {
	
class Transformation;


class BothValuePlotMethod: public ValuePlotMethod, public BothValuePlotMethodAttributes {

public:
	BothValuePlotMethod() : marker_(0) {
        
    }
	virtual ~BothValuePlotMethod() {}
    virtual void set(const map<string, string>& map ) { 
        BothValuePlotMethodAttributes::set(map); 
        ValuePlotMethodAttributes::set(map);
    }
    virtual void set(const XmlNode& node ) { 
        BothValuePlotMethodAttributes::set(node); 
        ValuePlotMethodAttributes::set(node);
    }
     virtual ValuePlotMethod* clone() const {
    	BothValuePlotMethod* object = new BothValuePlotMethod();
    	object->clone(*this);
    	return object;
    }
    
     virtual void clone(const BothValuePlotMethod& from )
        { BothValuePlotMethodAttributes::copy(from); 
        ValuePlotMethodAttributes::copy(from);}
    
    

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const {
         out << "BothValuePlotMethod[";
         BothValuePlotMethodAttributes::print(out);
         ValuePlotMethodAttributes::print(out);
         out << "]";
	 }
	 void reset() { marker_ = 0; }
 
    virtual void add(const PaperPoint& xy) {    
        if (!marker_) {
            marker_ = new TextSymbol();
            marker_->position(Symbol::M_ABOVE);
            marker_->setMarker(markerIndex_);
            marker_->setColour(*markerColour_);
            marker_->setHeight(markerHeight_);
            marker_->blanking(false);
            MagFont font;
            font.size(this->height_);
            font.colour(*this->colour_);
            marker_->font(font);
            this->push_back(marker_); 
         }   
         marker_->push_back(xy, this->label(xy.value()));
     
    }
    
    TextSymbol* marker_;
    

private:
    //! Copy constructor - No copy allowed
	BothValuePlotMethod(const BothValuePlotMethod&);
    //! Overloaded << operator to copy - No copy allowed
	BothValuePlotMethod& operator=(const BothValuePlotMethod&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const BothValuePlotMethod& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
