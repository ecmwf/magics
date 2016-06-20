/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
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

