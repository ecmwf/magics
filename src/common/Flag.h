/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
