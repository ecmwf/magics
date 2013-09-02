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
/*! \file TeSpatialOperations.h
    \brief This file contains functions to spatial operations with geographical objects that are in a SGBD 
*/

#ifndef  __TERRALIB_INTERNAL_SPATIALOPERATIONS_H
#define  __TERRALIB_INTERNAL_SPATIALOPERATIONS_H

#include "TeDatabase.h"
#include "TeRaster.h"

//! Keep the selected geometries in the canvas
typedef vector<TeGeometry*> TeSelectedGeom;

//! Return a string with the identifications of the selected geometries 
TL_DLL string  getStringIds(TeKeys& IdsIn);


//! Return a portal with the geometries of a spatial table that have a topological relation with specifics geometries of this spatial table
/*!
	Calculate the topological relation between all geometries of a spatial table and
	specifics geometries of this spatial table
	\param actGeomTable	spatial table name that contains all the geometries
	\param actRep		geometrical representation of the spatial table 
	\param actIdsIn		identifications of the specifics geometries (object_id)
	\param portal		returned with the resultants geometries 
	\param relation		topological relation that will be computed
	\param actColTable	collection table name that contains a set of geometries of the spatial table 
*/
TL_DLL bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   TeDatabasePortal *portal, int relation, const string& actColTable="");


//! Return a portal with the geometries of a spatial table that have a topological relation with specifics geometries of other spatial table 
/*!
	Calculate the topological relation between all geometries of a spatial table and
	specifics geometries of other spatial table
	\param actGeomTable	first spatial table name that contains the specifics geometries
	\param actRep		geometrical representation of the first spatial table 
	\param actIdsIn		identifications of specifics geometries of the first spatial table (object_id)
	\param visGeomTable	second spatial table name that contains the others geometries
	\param visRep		geometrical representation of the second spatial table 
	\param portal		returned with the resultants geometries 
	\param relation		topological relation that will be computed 
	\param visColTable	collection table name that contains a set of geometries of the second spatial table 
*/
TL_DLL bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   const string& visGeomTable, TeGeomRep visRep, TeDatabasePortal *portal, 
						   int relation, const string& visColTable="");


//! Return a portal with the geometries of a spatial table that have a topological relation with a specific geometry  
/*!
	Calculate the topological relation between all geometries of a spatial table and a specific geometry 
	\param actGeomTable	spatial table name that contains the geometries
	\param actRep		geometrical representation of the spatial table 
	\param geom			a specific geometry 
	\param portal		returned with the resultants geometries 
	\param relation		topological relation that will be computed 
	\param actCollTable	collection table name that contains a set of geometries of the spatial table 
*/
TL_DLL bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, 
						   TeDatabasePortal *portal, int relation, const string& actCollTable="");


//! Return the identications of the geometries of a spatial table that have a topological relation with specifics geometries of this spatial table
/*!
	Calculate the topological relation between all geometries of a spatial table and
	specifics geometries of this spatial table
	\param actGeomTable	spatial table name that contains all the geometries
	\param actRep		geometrical representation of the spatial table 
	\param actIdsIn		identifications of the specifics geometries (object_id)
	\param actIdsOut	returned with the identications of the resultants geometries (object_id)
	\param db			a TeDatabase pointer 
	\param relation		topological relation that will be computed 
	\param actCollTable	collection table name that contains a set of geometries of the spatial table 
*/
TL_DLL bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   TeKeys& actIdsOut, TeDatabase* db, int relation, const string& actCollTable="");


//! Return the identications of the geometries of a spatial table that have a topological relation with specifics geometries of other spatial table 
/*!
	Calculate the topological relation between all geometries of a spatial table and
	specifics geometries of other spatial table
	\param actGeomTable	first spatial table name that contains the specifics geometries
	\param actRep		geometrical representation of the first spatial table 
	\param actIdsIn		identifications of specifics geometries of the first spatial table (object_id)
	\param visGeomTable	second spatial table name that contains the others geometries
	\param visRep		geometrical representation of the second spatial table 
	\param visIdsOut	returned with the identications of the resultants geometries (object_id)
	\param db			a TeDatabase pointer 
	\param relation		topological relation that will be computed 
	\param visCollTable	collection table name that contains a set of geometries of the second spatial table 
*/
TL_DLL bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
						   const string& visGeomTable, TeGeomRep visRep, TeKeys& visIdsOut, 
						   TeDatabase* db, int relation, const string& visCollTable="", TeDatabase* = 0);

//! Return the identications of the geometries of a spatial table that have a topological relation with a specific geometry  
/*!
	Calculate the topological relation between all geometries of a spatial table and a specific geometry 
	\param actGeomTable	spatial table name that contains the geometries
	\param actRep		geometrical representation of the spatial table 
	\param geom			a specific geometry 
	\param actIdsOut	returned with the identications of the resultants geometries (object_id)
	\param db			a TeDatabase pointer 
	\param relation		topological relation that will be computed 
	\param actCollTable	collection table name that contains a set of geometries of the spatial table 
*/
TL_DLL bool TeTopologicalRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, 
						   TeKeys& actIdsOut, TeDatabase* db, int relation, const string& actCollTable="");

//! Return whether the specified topological relation holds between two polygon
/*!
	\param geom1	a polygon
	\param geom2	another polygon
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TePolygon* geom1, const TePolygon* geom2, int relation);

//! Return whether the specified topological relation holds between a polygon and a line
/*!
	\param geom1	a polygon
	\param geom2	a line
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TePolygon* geom1, const TeLine2D* geom2, int relation);

//! Return whether the specified topological relation holds between a polygon and a point
/*!
	\param geom1	a polygon
	\param geom2	a point
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TePolygon* geom1, const TePoint* geom2, int relation);

//! Return whether the specified topological relation holds between two lines
/*!
	\param geom1	a line
	\param geom2	another line
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TeLine2D* geom1, const TeLine2D* geom2, int relation);

//! Return whether the specified topological relation holds between a line and a point
/*!
	\param geom1	a line
	\param geom2	a point
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TeLine2D* geom1, const TePoint* geom2, int relation);

//! Return whether the specified topological relation holds between two points
/*!
	\param geom1	a point
	\param geom2	another point
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TePoint* geom1, const TePoint* geom2, int relation);

//! Return whether the specified topological relation holds between two cells
/*!
	\param geom1	a cell
	\param geom2	another cell
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TeCell* geom1, const TeCell* geom2, int relation);

//! Return whether the specified topological relation holds between a cell and a polygon
/*!
	\param geom1	a cell
	\param geom2	a polygon
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TeCell* geom1, const TePolygon* geom2, int relation);

//! Return whether the specified topological relation holds between a cell and a line
/*!
	\param geom1	a cell
	\param geom2	a line
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TeCell* geom1, const TeLine2D* geom2, int relation);

//! Return whether the specified topological relation holds between a cell and a point
/*!
	\param geom1	a cell
	\param geom2	a point
	\param relation		topological relation to be verified
*/
TL_DLL bool TeTopologicalRelation(const TeCell* geom1, const TePoint* geom2, int relation);

//! Returns the geometries of a geometric table (actGeomTable) that are within a specific distance from a point in memory 
/*!
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the actGeomTable table
	  \param point			a point in memory
	  \param IdsDistOut		structure that will contain the identifiers of the resulted geometries and their distances
	  \param db				a TeDatabase pointer 
	  \param max_dist		maximum distance 
	  \param actCollTable	collection table name associated with the actGeomTable table
*/
TL_DLL bool TeGetWithinDistance(const string& actGeomTable, TeGeomRep actRep, const TeCoord2D& point, TeKeysToDist& IdsDistOut,
						 TeDatabase* db, const double& max_dist, const string& actCollTable="");


//! Return the area of some specifics geometries of a spatial table 
/*!
	\param actGeomTable	spatial table name that contains the geometries
	\param actRep		geometrical representation of the spatial table 
	\param actIdsIn		identifications of the specifics geometries (object_id)
	\param db			a TeDatabase pointer 
	\param area			area returned
*/
TL_DLL bool TeGetArea(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, TeDatabase* db, double& area);


//! Return the length of some specifics geometries of a spatial table 
TL_DLL bool TeGetLength(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, TeDatabase* db, double& length);


//! Return the distance between two geometries
TL_DLL bool TeGetDistance(const string& actGeomTable, TeGeomRep actRep, TeKeys& IdsIn, TeDatabase* db, double& distance);

//! Return the convexhull geometries of specifics geometries of a spatial table 
/*!
	\param actGeomTable		spatial table name that contains the geometries
	\param actRep			geometrical representation of the spatial table 
	\param actIds			identifications of the specifics geometries (object_id)
	\param db				a TeDatabase pointer 
	\param convexHullSet	the convexhull geometries returned
*/
TL_DLL bool TeGetConvexHull(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeDatabase* db, 
					 TePolygonSet& convexHullSet);


//! Return the centroids of specifics geometries of a spatial table 
TL_DLL bool TeGetCentroid(const string& actGeomTable, TeGeomRep actRep, TeDatabase* db, TePointSet& centroidSet,
				   TeKeys& actIds, const string& actCollTable="");


//! Return the buffer 
TL_DLL bool TeGetBuffer(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeDatabase* db, 
				 TePolygonSet& bufferSet, double dist);


//! Return intersection
TL_DLL bool TeGetOverlay(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeDatabase* db, 
				  TeGeometryVect& geomVect, const short& operation);

/*!	
	\brief Clip a raster from a polygon. Return the clipped raster 
	\param rasterIn	TeRaster pointer that will be clipped
	\param poly		polygon that defines the clipping
	\param st		strategic to the iterator of the raster  
*/
TL_DLL TeRaster*
TeMask (TeRaster* rasterIn, TePolygon& poly, TeStrategicIterator st);

#endif

