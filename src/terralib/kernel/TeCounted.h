/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeCounted.h
    \brief This file contains classes to support the counted object idiom
*/

//! Supports the counted object idiom
/*! 
	A counted class keeps track of how many abstract instances are pointing
	to the same implementation.
*/

#ifndef  __TERRALIB_INTERNAL_COUNTED_H
#define  __TERRALIB_INTERNAL_COUNTED_H

#include "TeDefines.h"

class TL_DLL TeCounted {
public:

// -- Contructors

	//! Constructor: sets zero references when the object is  built
	TeCounted(): refCount_ ( 0 ){}
	
// -- Methods

	//! Increases the number of references to this object
	void attach ()
		{ refCount_++; }

	//! Decreases the number of references to this object. Destroy it if there are no more references to it
	void detach ()
		{ if ( --refCount_ == 0 )
			delete this; }

	//! Returns the number of references to this object
	int refCount()
	{ return refCount_; }

	//! Destructor
	virtual ~TeCounted(){}

private:

	//! No copy allowed
	TeCounted(const TeCounted&);

	//! No copy allowed
	TeCounted& operator=(const TeCounted&){return *this;}

// -- Members
	int refCount_; 	//!< the number of references to this class
};

#endif

