/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LinearTableMode.h
    \brief Definition of the Template class LinearTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 14-July-2005
    
    Changes:
    
*/

#ifndef LinearTableMode_H
#define LinearTableMode_H

#include "magics.h"

#include "LookupTableMode.h"

namespace magics {

class LinearTableMode: public LookupTableMode {

public:
	LinearTableMode();
	virtual ~LinearTableMode();
	void set(const XmlNode& node ) // for MagML
			 { LookupTableModeAttributes::set(node); }
	void set(const map<string, string>& map ) // for MagML
		 { LookupTableModeAttributes::set(map); }

	virtual void operator()(Image&, Raster&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	LinearTableMode(const LinearTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	LinearTableMode& operator=(const LinearTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LinearTableMode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
