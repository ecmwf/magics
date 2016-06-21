/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NormalTableMode.cc
    \brief Definition of the Template class NormalTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 14-July-2005
    
    Changes:
    
*/

#ifndef NormalTableMode_H
#define NormalTableMode_H

#include "magics.h"

#include "LookupTableMode.h"

namespace magics {

class NormalTableMode: public LookupTableMode {

public:
	NormalTableMode();
	virtual ~NormalTableMode();
	void set(const map<string, string>& map ) // for MagML
		 { LookupTableModeAttributes::set(map); }

	virtual void operator()(Image&, Raster&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	NormalTableMode(const NormalTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	NormalTableMode& operator=(const NormalTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NormalTableMode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
