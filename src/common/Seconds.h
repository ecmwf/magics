/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Seconds.h
// Baudouin Raoult - ECMWF Jul 96

#ifndef Seconds_H
#define Seconds_H
 

#include "magics.h"
#include "magics_windef.h"

#ifdef MAGICS_ON_WINDOWS
#include "win_time.h"
#endif

namespace magics {

class Seconds {
public:

// -- Contructors

	Seconds(double);
	Seconds(const timeval&);

// -- Operators

	operator string() const;
	operator double() const { return seconds_; }
    double seconds_;

	friend ostream& operator<<(ostream&,const Seconds&);

private:

// No copy allowed

	Seconds(const Seconds&);
	Seconds& operator=(const Seconds&);

// -- Members

	

};

} // namespace magics
#endif
