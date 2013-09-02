/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeRepresentation.h
    \brief This file contains structures and definitions to deal with a geometrical representation
*/
#ifndef  __TERRALIB_INTERNAL_REPRESENTATION_H
#define  __TERRALIB_INTERNAL_REPRESENTATION_H

#include "TeDataTypes.h"
#include "TeBox.h"

#include <algorithm>
#include <string>

//!  A class to supporting a geometrical representation 
struct TL_DLL TeRepresentation
{
	int id_;				//!< representation unique dabatase identifier
	TeGeomRep geomRep_;		//!< representation type (
	std::string	tableName_;		//!< name of the table that stores the geometries associated to this representation
	TeBox	box_;			//!< representation bounding box
	std::string	description_;	//!< representation description
	double	resX_;			//!< the x resolution of the layer cell set geometry
	double	resY_;			//!< the y resolution of the layer cell set geometry
	int		nCols_;			//!< number of columns in raster geometry
	int		nLins_;			//!< number of columns in raster geometry

	//! Constructor
	TeRepresentation() :
		resX_(0.0),
		resY_(0.0),
		nCols_(0),
		nLins_(0)
		{}
};

//! A functor class to compare representations by its type
class TL_DLL TeRepres_eq : public unary_function<TeRepresentation*,bool>
{
	TeGeomRep r;
public:
	explicit TeRepres_eq(TeGeomRep rr): r(rr) {}
	bool operator()(const TeRepresentation* rep) const { return rep->geomRep_ == r; }
};

//! An iterator to a std::vector of TeRepresentation 
typedef std::vector<TeRepresentation* >::iterator TeRepresPointerVectorIterator;

//! A std::vector of pointers to TeRepresentation
typedef std::vector<TeRepresentation* > TeRepresPointerVector;

//! Finds the first representation of a given type in a std::vector of representations
TL_DLL TeRepresPointerVectorIterator 
TeFindRepresentation(TeRepresPointerVectorIterator begin, TeRepresPointerVectorIterator end, TeGeomRep rep);

#endif


