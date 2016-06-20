/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
