/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EquidistantTableMode.h
    \brief Definition of the Template class EquidistantTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 17-May-2005
    
    Changes:
    
*/

#ifndef EquidistantTableMode_H
#define EquidistantTableMode_H

#include "magics.h"

#include "LookupTableMode.h"
#include "EquidistantTableModeAttributes.h"

#define ETM_MLEN 1024 //maximum number of elements of a histogram

namespace magics {

//class EquidistantTableMode: public LookupTableMode, public EquidistantTableModeAttributes {
class EquidistantTableMode: public LookupTableMode {

public:
	EquidistantTableMode();
	virtual ~EquidistantTableMode();
	void set(const map<string, string>& ) // for MagML
	{
	   //LookupTableMode::set(map); 
	   //EquidistantTableModeAttributes::set(map);
	}

	virtual void operator()(Image&, Raster&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	EquidistantTableMode(const EquidistantTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	EquidistantTableMode& operator=(const EquidistantTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EquidistantTableMode& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
