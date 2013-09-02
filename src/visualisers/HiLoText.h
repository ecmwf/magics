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

/*! \file HiLoText.h
    \brief Definition of the Template class HiLoText.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 24-Jun-2004
    
    Changes:
    
*/

#ifndef HiLoText_H
#define HiLoText_H

#include "magics.h"

#include "HiLoTechnique.h"
#include "HiLoTextAttributes.h"
#include "Text.h"
#include "HiLo.h"
namespace magics {


class HiLoText: public HiLoTechnique, public HiLoTextAttributes {

public:
	HiLoText() : high_(0), low_(0) {}
	virtual ~HiLoText() {}
	void operator()(const PaperPoint& point, HiLo& hilo)
	{
		if (!high_)
		{
			// Create Text List containing the position of the High
			high_ = new Text();
			high_->addText(hi_text_, *this->hi_colour_, this->contour_hilo_height_);		
			high_->setBlanking(this->blanking_);
			hilo.push_back(high_);
		}

		if (!low_)
		{
			// Create Text List containing the position of the High
			low_ = new Text();
			low_->addText(lo_text_, *this->lo_colour_, this->contour_hilo_height_);			
			low_->setBlanking(this->blanking_);
			hilo.push_back(low_);
		}
       
		

		if ( point.high() ) 
			high_->push_back(point);  
		else if ( point.low() ) 
			low_->push_back(point);
		else   
			MagLog::warning() << "high/low information not set in point-> the point is ignored" << "\n";
	}
	void clear() {
		high_ = 0;
		low_ = 0;
	}
	void set(const map<string, string>& map)
	{ 
		HiLoTechnique::set(map);
		HiLoTextAttributes::set(map);
	}


protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { out << "HiLoText"; } 
	Text* high_;
	Text* low_;

private:
	//! Copy constructor - No copy allowed
	HiLoText(const HiLoText&);
	//! Overloaded << operator to copy - No copy allowed
	HiLoText& operator=(const HiLoText&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoText& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
