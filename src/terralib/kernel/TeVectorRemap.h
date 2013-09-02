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
/*! \file TeVectorRemap.h
    \brief This file contains functions to remap vector structures
*/
#ifndef  __TERRALIB_INTERNAL_VECTORREMAP_H
#define  __TERRALIB_INTERNAL_VECTORREMAP_H

#include "TeProjection.h"
#include "TeGeometry.h"

//! Remaps a geometry. Templated by the type of geometry.
/*
	\param g1 the input geometry
	\param p1 the initial projection
	\param g2 to receive the remapped geometry
	\param p1 the destination projection
*/
template<typename G>
void TeVectorRemap (G& g1, TeProjection* p1, G& g2, TeProjection* p2)
{
	int nItens = g1.size();
	for (int i=0; i<nItens; i++)
	{
		typename G::value_type e1 = g1[i];
		typename G::value_type e2;
		TeVectorRemap(e1,p1,e2,p2);
		g2.add(e2);
	}
}

//! Especialized version to remap coordinates
TL_DLL void TeVectorRemap(TeCoord2D& pt1, TeProjection* p1, TeCoord2D& pt2, TeProjection* p2);

//! Especialized version to remap cells
TL_DLL void TeVectorRemap(TeCell& c1, TeProjection* p1, TeCell& c2, TeProjection* p2);

//! Especialized version to remap texts
TL_DLL void TeVectorRemap(TeText& t1, TeProjection* p1, TeText& t2, TeProjection* p2);

//! Remaps a box from one projection to another
/*
	\param box the input box
	\param projFrom the initial projection
	\param projTo the destination projection
	\returns the remmaped box
*/
TL_DLL TeBox TeRemapBox (TeBox& box, TeProjection* projFrom, TeProjection* projTo);

//! Remaps a box from plane coordinate to geodetic coordinate
/*
	\param box the input box
	\param proj the data projection
	\returns the remmaped box
*/
TL_DLL TeBox TeRemapBoxPC2Geodetic (const TeBox& box, TeProjection* proj);

//! Remaps a box from geodetic coordinate to plane coordinate
/*
	\param box the input box
	\param proj the data projection
	\returns the remmaped box
*/
TL_DLL TeBox TeRemapBoxGeodetic2PC (const TeBox& box, TeProjection* proj);

#endif
