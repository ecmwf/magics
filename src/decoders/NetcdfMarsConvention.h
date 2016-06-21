/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfMarsConvention.h
    \brief Definition of the Template class NetcdfMarsConvention.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfMarsConvention_H
#define NetcdfMarsConvention_H

#include "magics.h"

#include "NetcdfConvention.h"

namespace magics {

class NetcdfMarsConvention: public NetcdfConvention {

public:
	NetcdfMarsConvention();
	virtual ~NetcdfMarsConvention();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	NetcdfMarsConvention(const NetcdfMarsConvention&);
    //! Overloaded << operator to copy - No copy allowed
	NetcdfMarsConvention& operator=(const NetcdfMarsConvention&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfMarsConvention& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
