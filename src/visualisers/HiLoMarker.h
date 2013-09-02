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

/*! \file HiLoMarker.h
    \brief Definition of the Template class HiLoMarker.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 22-Jun-2004
    
    Changes:
    
*/

#ifndef HiLoMarker_H
#define HiLoMarker_H

#include "magics.h"
#include "MatrixHandler.h"
#include "PointsHandler.h"
#include "HiLoMarkerBase.h"
#include "HiLoMarkerAttributes.h"
#include "Symbol.h"
#include "HiLo.h"
namespace magics {



class HiLoMarker: public HiLoMarkerBase, public HiLoMarkerAttributes {

public:
	HiLoMarker() : marker_(0) {}
	virtual ~HiLoMarker() {}
    void set(const map<string, string>& map) { 
        HiLoMarkerAttributes::set(map); 
    }
    void set(const XmlNode& node) { 
        HiLoMarkerAttributes::set(node); 
    }
    virtual bool accept(const string& node) { return HiLoMarkerAttributes::accept(node); }
   
    virtual HiLoMarker* clone() const {
		HiLoMarker* object = new HiLoMarker();
		object->copy(*this);
	    return object;
	}
    virtual void clear() { marker_ = 0; }
   
    virtual void operator()(const PaperPoint& point, HiLo& hilo) {
        if ( !marker_) {
            marker_ = new Symbol();
            marker_->setMarker(index_);
            marker_->setColour(*colour_);
            marker_->setHeight(height_);
            hilo.push_back(marker_); 
        }
        marker_->push_back(point);
    }
    
    
    
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { 
         out << "HiLoMarker[";
         HiLoMarkerAttributes::print(out);
         out << "]";
	 }
     double scaling_;
     Symbol* marker_;
     

private:
    //! Copy constructor - No copy allowed
	HiLoMarker(const HiLoMarker&);
    //! Overloaded << operator to copy - No copy allowed
	HiLoMarker& operator=(const HiLoMarker&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoMarker& p)
		{ p.print(s); return s; }

};


class NoHiLoMarker: public HiLoMarkerBase{

public:
	NoHiLoMarker() {}
	virtual ~NoHiLoMarker() {}
	virtual HiLoMarkerBase* clone() const {
		HiLoMarkerBase* object = new NoHiLoMarker();
	    return object;
	}
	virtual bool accept(const string& node) { return magCompare(node, "nohilo"); }

    
    
        
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "No HiLo Marker defined"; } 
  

};


} // namespace magics
#endif
