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

/*! \file Image.h
    \brief Definition of the Template class Image.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 6-Apr-2005
    
    Changes:
    
*/

#ifndef Image_H
#define Image_H

#include "magics.h"
#include "BaseDriver.h"
#include "ImageProperties.h"

namespace magics {

class Image: public ImageProperties, public vector<short>  {

public:
	Image() {}
	virtual ~Image() {}
	
	// Implement the BaseGraphics Interface 
	virtual void redisplay(const BaseDriver& driver) const { driver.redisplay(*this); } 
	
	void set(int rows, int columns) { rows_ = rows; columns_ = columns; reserve(rows_*columns_); }

	int getNumberOfRows() const {return rows_;}
	int getNumberOfColumns() const {return columns_;}
	
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const {}
	int rows_;
	int columns_;
private:
	//! Copy constructor - No copy allowed
	Image(const Image&);
	//! Overloaded << operator to copy - No copy allowed
	Image& operator=(const Image&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Image& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
