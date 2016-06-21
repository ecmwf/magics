/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HiLoBoth.h
    \brief Definition of the Template class HiLoBoth.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 24-Jun-2004
    
    Changes:
    
*/

#ifndef HiLoBoth_H
#define HiLoBoth_H

#include "magics.h"

#include "HiLo.h"
#include "HiLoTechnique.h"

#include "HiLoTextAttributes.h"
#include "HiLoMarkerAttributes.h"
#include "MagicsFormat.h"
#include "Symbol.h"

namespace magics {


class HiLoBoth: public HiLoTechnique, public HiLoTextAttributes, public HiLoMarkerAttributes {

public:
	HiLoBoth() :high_(0), low_(0) {}
	virtual ~HiLoBoth() {}
    void set(const map<string, string>& map) { 
        HiLoTechnique::set(map);

        HiLoTextAttributes::set(map);
        HiLoMarkerAttributes::set(map);
    }
     void set(const XmlNode& node) { 
        HiLoTechnique::set(node);

        HiLoTextAttributes::set(node);
        HiLoMarkerAttributes::set(node);
    }
     virtual HiLoTechnique* clone() const {
		HiLoBoth* object = new HiLoBoth();
		object->clone(*this);
	    return object;
	}
	virtual void clone(const HiLoBoth& from)
	{
		 HiLoTechnique::copy(from);

        HiLoTextAttributes::copy(from);
        HiLoMarkerAttributes::copy(from);
	}
	void clear() {
		high_ = 0;
		low_ = 0;
	}
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "HiLoBoth"; }
     virtual void operator()(const PaperPoint& point, HiLo& hilo) {

        
        if (!high_) {
            // Create Text List containing the position of the High
            high_ = new TextSymbol();
            high_->setMarker(index_);
            high_->setColour(*colour_);
            high_->setHeight(height_);
            hilo.push_back(high_);
        }
        
        if (!low_) {
            // Create Text List containing the position of the High
            low_ = new TextSymbol();
            low_->setMarker(index_);
            low_->setColour(*colour_);
            low_->setHeight(height_);
            hilo.push_back(low_);
        }
       
        
        if ( point.high()) {         
            high_->push_back(point,   hi_text_);
        }
        else if ( point.low()) {
             low_->push_back(point,   hi_text_);
        } 
        else {
            MagLog::warning() << "high/low information not set in point-> the point is ignored" << "\n";
         }
        
       
     }
     TextSymbol* high_;
     TextSymbol* low_;
     
private:
    //! Copy constructor - No copy allowed
	HiLoBoth(const HiLoBoth&);
    //! Overloaded << operator to copy - No copy allowed
	HiLoBoth& operator=(const HiLoBoth&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoBoth& p)
		{ p.print(s); return s; }

};

} // namespace magics

#endif
