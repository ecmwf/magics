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
/*! \file  TePrecision.h
    \brief This file contains a singleton to manage precision in TerraLib.
*/
#ifndef  __TERRALIB_INTERNAL_PRECISION_H
#define  __TERRALIB_INTERNAL_PRECISION_H

#include "TeSingleton.h"
#include "TeDefines.h"

//! Describes a class for supportin scale and representation dependency 
/*! The entire set of geometric algorithms make use of this class,
    an so it must be properly set. It will be used when dealling with approximate calculus
	\sa TeSingleton 
*/
class TL_DLL TePrecision: public TeSingleton<TePrecision>
{
public:
	//! Destructor
	~TePrecision() {}

	friend class TeSingleton<TePrecision>;

	//! Sets the precision value to be considered
	void setPrecision ( double precision )
	{ precision_ = precision; }

	//! Returns the precision value in use
	double	precision ()
	{ return precision_; }

protected:
	//! Constructor
	TePrecision(): precision_ ( TeMINFLOAT )
	{}

	double precision_;	//!< Stores the precision.
};

#endif
