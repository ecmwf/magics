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
/*! \file TeDatum.h
    \brief This file contains structures and definitions to support the concept of a planimetric datum
*/

#ifndef  __TERRALIB_INTERNAL_DATUM_H
#define  __TERRALIB_INTERNAL_DATUM_H

#include <string>
#include "TeUtils.h"

//! Number of supported datum in TerraLib
const int NUM_DATUM = 9; 

TL_DLL const char** TeGetDatumList();


//! Implements valid Datum options and related parameters which account for different earth spheroids.
/*! Specifies planimetric TeDatum parameters to be used for defining geodetic
	coordinates, which are necessary to establish a map projection system.
	It encapsulates the following important information regarding 
	a planimetric Datum such as:
	\li	Datum name;
	\li	Earth equatorial radius;
	\li	Earth flatenning;
	\li	Datum shifts. 

	\author
	 Ubirajara Freitas, Julio d'Alge.

	\sa TeDatumFactory
*/
class TL_DLL TeDatum
{
private:
	std::string	name_;		//!< Planimetric TeDatum name
	double	rd_,		//!< Earth equatorial radius (m) 
			flt_;		//!< Earth flattening 
public:
	double	dx_,		//!< TeDatum shift along x axis (m)
			dy_,		//!< TeDatum shift along y axis (m)
			dz_;		//!< TeDatum shift along z axis (m) 

	//!	Constructor.
	TeDatum ( std::string name = "Spherical", double rd = 6.371000e+06 , double flt = 0., 
			  double dx = 0., double dy= 0., double dz = 0.):
	name_ ( name ),
	rd_   (  rd  ),
	flt_  ( flt  ),
	dx_   (  dx  ),
	dy_   (  dy  ),
	dz_   (  dz  )
	{ }

	//! Copy constructor
	TeDatum(const TeDatum& other);

	//! Operator =
	TeDatum& operator=(const TeDatum& rhs);

	//!	Destructor.
	~TeDatum () {}

	//! Returns the datum name
	std::string 	name()	 const			
	{ return name_;} 

	//! Sets the datum name
	void name(const std::string& name)	 			
	{	name_ = name;	} 

	//! Returns Earth equatorial radius (m)
	double radius() const
	{ return rd_; }

	//! Sets Earth equatorial radius (m)
	void radius(const double rd) 
	{ rd_ = rd; }

	//! Returns the Earth equatorial radius (m) (m)
	double flattening() const
	{ return flt_; }

	//! Sets the Earth equatorial radius (m) (m)
	void flattening(const double flt) 
	{ flt_ = flt; }

	//! Returns the shift along x axis (m)
	double	xShift() const
	{ return dx_; }
 
	//! Returns the shift along y axis (m)
	double  yShift() const 
	{ return dy_; }

	//! Returns the shift along z axis (m)
	double zShift()  const
	{ return dz_; }


//!		Verifies if current TeDatum is equal to TeDatum& dat. 
	bool operator== (const TeDatum& dat) const 
	{
		return (TeFPEquals(rd_,dat.rd_,0.0000000001) 
			   && TeFPEquals(flt_,dat.flt_,0.0000000001)
			   && TeFPEquals(dx_,dat.dx_,0.0000000001) 
			   && TeFPEquals(dy_,dat.dy_,0.0000000001) 
			   && TeFPEquals(dz_,dat.dz_,0.0000000001)); 
	}

	//! Returns the Datum description in PROJ4 format
	string getProj4Description();

	//! Returns the Datum description in WKT format
	string getWKTDescription();

};

//! Produces a Datum accordingly to an specified name
class TL_DLL TeDatumFactory
{
public:
	//! Builds a datum from a TerraLib name
	static	TeDatum make ( const std::string& name );

	//! Builds a datum from its EPSG code
	static TeDatum makeFromEPSGCode(const std::string epsgCode);

	//! Builds a datum from its OGC's WKT description
	static TeDatum makeFromWKT(const std::string wktDatumDescription);
};

/*! 
   \fn   findDatum(double semiMajor, double flatenning, TeDatum& datum)
   \brief Tries to find the a TeDatum according to givem semi major axis and earth flattening
 */
TL_DLL bool findDatum(double semiMajor, double flatenning, TeDatum& datum);

#endif

