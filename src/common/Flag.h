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

/*! \file Flag.h
    \brief Definition of the Template class Flag.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 16-Mar-2005
    
    Changes:
    
*/

#ifndef Flag_H
#define Flag_H

#include "magics.h"

#include "Arrow.h"

namespace magics {



class Flag: public Arrow {

public:
   
	Flag();
	~Flag();
	
	void redisplay(const BaseDriver& driver) const { driver.redisplay(*this); }
	
	double getLength() const                { return length_; }
	void setLength(double length)           { length_ = length; }
    
	FlagConvention getConvention() const           { return convention_; }
	void setConvention(FlagConvention convention)  { convention_ = convention; }

//private:
	//! Copy constructor - No copy allowed
	Flag(const Flag&);
	//! Overloaded << operator to copy - No copy allowed
	Flag& operator=(const Flag&);

protected:
	//! Method to print string about this class on to a stream of type ostream.
	void print(ostream&) const; 	 
	double         length_;
	FlagConvention convention_;
    
private:
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Flag& p)
		{ p.print(s); return s; }

};

} // namespace magics

#endif
