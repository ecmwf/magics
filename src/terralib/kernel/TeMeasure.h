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
/*! \file TeMeasure.h
    \brief This file defines a class for handling a measure
*/

#ifndef  __TERRALIB_INTERNAL_MEASURE_H
#define  __TERRALIB_INTERNAL_MEASURE_H

//! A class for handling a measure
/*!
	Provide support and definition for a measured value for All application that 
	require a field measure.
	\sa TeSample, TeCountourLine
*/

#include "TeDefines.h"
class TL_DLL TeMeasure 
{
public:

	//! Constructor
	TeMeasure( double value ) : value_( value ) {}

	//! Destructor
	~TeMeasure() {}

	//! Return the value associated to the measure
	double value()
	{ return value_; }

protected:
	double value_;
};


//typedef TeComposite <TeMeasure> TeMeasureComposite;

#endif
