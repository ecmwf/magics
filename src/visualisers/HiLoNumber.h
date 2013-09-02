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

/*! \file HiLoNumber.h
    \brief Definition of the Template class HiLoNumber.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 24-Jun-2004
    
    Changes:
    
*/

#ifndef HiLoNumber_H
#define HiLoNumber_H

#include "magics.h"

#include "HiLoTechnique.h"
#include "HiLoNumberAttributes.h"
#include "MagicsFormat.h"
#include "Text.h"
#include "HiLo.h"
namespace magics {


class HiLoNumber: public HiLoTechnique, public HiLoNumberAttributes {

public:
	HiLoNumber() {}
	virtual ~HiLoNumber() {}
    void set(const map<string, string>& map) { 
        HiLoTechnique::set(map);
        HiLoNumberAttributes::set(map);
    }
    void set(const XmlNode& node) { 
        HiLoTechnique::set(node);
        HiLoNumberAttributes::set(node);
    }
     virtual HiLoTechnique* clone() const {
		HiLoNumber* object = new HiLoNumber();
		object->clone(*this);
	    return object;
	}
	virtual void clone(const HiLoNumber& from)
	{
		HiLoTechnique::copy(from);
        HiLoNumberAttributes::copy(from);
      
	}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	  virtual void print(ostream& out) const { out << "HiLoNumber"; }
     virtual void operator()(const PaperPoint& point, HiLo& hilo) {
     	ostringstream nice;
    	nice << MagicsFormat(this->format_, point.value()); 
	
       
        if ( point.high()) {
             Text* text = new Text();
             text->addText(nice.str(), *this->hi_colour_, this->contour_hilo_height_);
             hilo.push_back(text);
             text->setBlanking(this->blanking_);
             text->push_back(point);
        }
        else if ( point.low()) {
             Text* text = new Text();
             text->addText(nice.str(), *this->lo_colour_, this->contour_hilo_height_);
             hilo.push_back(text);
             text->setBlanking(this->blanking_);
             text->push_back(point);
        } 
        else {
            MagLog::warning() << "high/low information not set in point-> the point is ignored" << "\n";
         }
     }
      
private:
    //! Copy constructor - No copy allowed
	HiLoNumber(const HiLoNumber&);
    //! Overloaded << operator to copy - No copy allowed
	HiLoNumber& operator=(const HiLoNumber&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoNumber& p)
		{ p.print(s); return s; }

};

} // namespace magics

#endif
