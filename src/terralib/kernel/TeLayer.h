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
/*! \file TeLayer.h
    \brief This file contains structures and definitions to deal with a layer
*/
#ifndef __TERRALIB_INTERNAL_LAYER_H
#define __TERRALIB_INTERNAL_LAYER_H

#include "TeGeometry.h"
#include "TeDataTypes.h"
#include "TeTable.h"
#include "TeRepresentation.h"

#include <string>
#include <list>
#include <map>

class TeRaster;
class TeDatabase;
class TeRasterParams;
class TeProjection;

//!  A class for supporting a layer. 
/*!  
	 In TerraLib, a layer is a collection of geometries that share the same 
     geographical projection, and are related in some way (e.g. a shapefile).
	 A layer has a pointer to a database that effectively stores its atributes
	 and geometries.
	 
	\sa
     TeGeometry, TeProjection, TeBox, TeDatabase, TeTheme, TeTable
*/
class TL_DLL TeLayer {

public:

	//! Empty constructor
	TeLayer():
		id_(-1),
		db_(0),
		projection_ ( 0 ),
		raster_(0)
		{ }

		
	//! Constructor with parameters
	/* 
		\param name layer name (should be unique)
		\param db a TerraLib database connection w
		\param proj layer projection
		\note 
			\par If no database is informed, layer will exist only in memory.
			\par If a layer with this name doesnt exist a layer will be 
			 created and stored. If no projection is informed a default TeNoProjection
			 will be used as layer projection.
            \par If a layer with this name already exists it will be retrieved
	*/	 
	TeLayer(const string& name, TeDatabase* db=0, TeProjection* proj=0);

	//! Constructor with parameters
	/* 
		\param name layer name (should be unique)
		\param db TerraLib database connection where layer will be stored
		\param box layer bounding box
		\param proj layer projection
		\note 
			\par If no database is informed, layer will exist only in memory.
			\par If a layer with this name doesnt exist a layer will be 
			 created and stored. If no projection is informed a default TeNoProjection
			 will be used as layer projection.
            \par If a layer with this name already exists it will be retrieved
	*/
	TeLayer(const string& name, TeDatabase* db, TeBox& box, TeProjection* proj = 0);

	//! Destructor
	virtual ~TeLayer();

	//! Copy Constructor
	TeLayer ( const TeLayer& other );

	//! Operator =
	TeLayer& operator= ( const TeLayer& other );
	
	//! Retrieves the database associated to this layer
	virtual TeDatabase* const database() 
	{ return db_; }

	//! Sets the layer database
	virtual void setDatabase(TeDatabase* db)
	{ db_ = db; }

	//! Returns the layer id
	virtual int id()
	{ return id_; } 

	//! Sets the layer id
	virtual void id(int id)
	{ id_ = id; } 

	//! Returns the layer name
	virtual string name()
	{ return name_; }

	//! Sets the layer name
	virtual void name(const string &name)
	{  name_ = name; }

	//! Sets the  layer projection
	virtual void  setProjection ( TeProjection* proj );

	//! Retrieves the layer projection
	virtual TeProjection* projection()
	{	return projection_; }

	//! Returns the layer bounding box
	virtual TeBox&	box()
	{	return box_; }

	//! Sets the  bounding box of a layer
	virtual void setLayerBox ( const TeBox& box );

	//! Updates the bounding box of a layer
	virtual void updateLayerBox(const TeBox& box);

	//! Refreshes the bounding box of a layer according to its representation
	virtual void updateLayerBox();

	//! Returns a possible new object id based on the objects stored in database
	virtual int getNewObjectId();

	/** @name Attribute Tables
	*  Methods to deal with the attribute tables of the layer
	*/
	//@{

	//! Returns the number of distinct objects in the layer
	virtual int nObjects(const string& tName);

	//! Returns a vector with the definition of all attribute tables associated to layer
	virtual TeAttrTableVector& attrTables () 
	{ return attTables_; }

	//! Creates a new attribute table for the layer
	virtual bool createAttributeTable(TeTable& table);

    //! Adds an attribute table definition to layer vector of attribute tables (in memory)
	virtual bool addAttributeTable(TeTable& table);

    //! Remove an attribute table definition to layer vector of attribute tables (in memory)
	virtual bool removeAttributeTable(string tableName);

     //! Update an attribute table definition to layer vector of attribute tables (in memory)
	virtual void updateAttributeTable(TeTable& table);

	//! Reload the tables definition from database to memory
	virtual bool loadLayerTables();

	//! Saves an attribute table into the database where layer is stored 
	virtual bool saveAttributeTable(TeTable& table);

	//! Gets all attribute tables associated to this layer that are of a certain type
	virtual bool getAttrTables(TeAttrTableVector& atts, TeAttrTableType attType = TeAllAttrTypes);

	//! Gets a set of attribute table specifications associated to this layer that are of a certain type
	/*
		\param tableNames a vector that contains the name of the tables that are being searched
		\param attType type of table that is being searched
		\param atts returns a vector of tables found
		\return true if at least one table was found and false otherwise
	*/
	virtual bool getAttrTablesByName(vector<string> &tableNames, TeAttrTableVector& atts, TeAttrTableType attType = TeAllAttrTypes);

	//! Gets an attribute table associated to a layer
	/*
		\param tableName name of the table being searched
		\param attType type of table being searched
		\param table returns the table found
		\return true if table was found and false otherwise
	*/
	virtual bool getAttrTablesByName(const string& tableName, TeTable& table, TeAttrTableType attType = TeAllAttrTypes);

	//! Gets the name of the media table associated to layer
	virtual string mediaTable();

	//! Sets the name of the media table associated to layer
	virtual void mediaTable(const string& name);
	//@}

	/** @name Geometries
	*  Methods to deal with the geometries of the objects of a layer
	*/
	//! Returns a pointer to the raster geometry associated to an object of this layer
	/*!
		\param objectId unique identification of the object (if empty it will look
		for the first raster geometry found)
		\param mode permission access ('r', 'w')
	*/
	virtual TeRaster* raster(const string& objectId="",const char& mode = 'r');

	//! Sets the pointer to the raster representation
	virtual void raster( TeRaster* raster);

	//! Retrieves a polygon set given a selection criteria, expressed as a where clause 
	virtual bool getPolygons(TePolygonSet &ps, const string& whereClause = "");

	//! Retrieves a line set given a selection criteria, expressed as a where clause 
	virtual bool getLines(TeLineSet &ls, const string& whereClause = "");

	//! Retrieves a point set given a selection criteria, expressed as a where clause 
	virtual bool getPoints(TePointSet &ps, const string& whereClause = "");

	//! Retrieve a point set given a selection criteria, expressed as a where clause
	virtual bool getText(TeTextSet &ts, const string& whereClause = "");

	//! Retrieve a cell set given a selection criteria, expressed as a where clause
	virtual bool getCells(TeCellSet &cs, const string& whereClause = "");

	//! Locates a polygon that cointains a certain coordinate
	virtual bool locatePolygon(TeCoord2D &pt, TePolygon &polygon, const double& tol = 0.0);
	
	//! Locates a line that cointains a certain coordinate
	virtual bool locateLine(TeCoord2D &pt, TeLine2D &line, const double& tol = 0.0);

	//! Retrieves a point that cointains a certain coordinate
	virtual bool locatePoint(TeCoord2D &pt, TePoint &point, const double& tol = 0.0);

	//! Retrieves a TeText that cointains a certain coordinate
	virtual bool locateText(TeCoord2D &pt, TeText &text, const double& tol = 0.0);

	//! Retrieves a point that cointains a certain coordinate
	virtual bool locateCell(TeCoord2D &pt, TeCell &cell, const double& tol = 0.0);

	//! Retrieves the set of polygons with a certain geoid
	virtual bool loadGeometrySet	(const string& geoid, TePolygonSet &ps);

	//! Retrieves the set of lines with a certain geoid
	virtual bool loadGeometrySet (const string& geoid, TeLineSet &ls);

	//! Retrieves the set of points with a certain geoid
	virtual bool loadGeometrySet (const string& geoid, TePointSet &ps);

	//! Retrieves the set of points with a certain geoid
	virtual bool loadGeometrySet (const string& geoid, TeCellSet &cs);

	//! Retrieves the set of texts with a certain geoid
	virtual bool loadGeometrySet (const string& geoid, TeTextSet &cs);
	
	//! Adds a set of polygons to a layer
	virtual bool addGeometrySet(TePolygonSet& polySet, const string& /* tName */ = "")
	{ return addPolygons(polySet); }

	//! Adds a set of lines to a layer
	virtual bool addGeometrySet(TeLineSet& lineSet, const string& /* tName */ = "")
	{ return addLines(lineSet); }

	//! Adds a set of points to a layer
	virtual bool addGeometrySet(TePointSet& pointSet, const string& /* tName */ = "")
	{ return addPoints(pointSet); }

	//! Adds a set of text to a layer
	virtual bool addGeometrySet(TeTextSet& textSet, const string& tName = "")
	{ return addText(textSet,tName); }
	
	//! Adds a set of cells to a layer
	virtual bool addGeometrySet(TeCellSet& cellSet, const string& /* tName */ = "")
	{ return addCells(cellSet); }

	//! Adds a set of polygons to a layer
	virtual bool addPolygons(TePolygonSet& polySet);

	//! Adds a set of lines to a layer
	virtual bool addLines (TeLineSet& lineSet);

	//! Adds a set of points to a layer
	virtual bool addPoints (TePointSet& pointSet);

	//! Adds a set of text to a layer
	virtual bool addText (TeTextSet& textSet,const string& tName);
	
	//! Adds a set of cells to a layer
	virtual bool addCells(TeCellSet& cellSet);

	//@}

	/** @name Geometries
	*  Methods to deal with geometrical representations of a layer
	*/
	//! Removes a geometry from layer
	virtual bool removeGeometry (TeGeomRep repType, const string& tName="");

	//! Indicates if layer has a geometry representation
	virtual bool hasGeometry (TeGeomRep rep);

    //! Retrieves the table name associated to a geometry representation
	/*! \param rep geometrical representation
		\note When there is more than one representation of a given type
		 returns the table name associated to the first one encountered
	*/
	virtual string tableName(TeGeomRep rep); 

	//! Retrieves the number of geometries of a type
	/*! 
		\param rep geometrical representation
	*/	
	virtual int nGeometries(TeGeomRep rep);

	//! Gets all information about a representation
	virtual bool getRepresentation(TeGeomRep repType, TeRepresPointerVector& result);

	//! Gets an specific representation
	/*! 
		\param repType the geometrical representation being searched
		\param tableName the table associated to this representation
		\return the representation associated to a geometry with the given table name
		\note if no table name is specified the first found is returned
	*/
	virtual TeRepresentation* getRepresentation(TeGeomRep repType, const string& tableName="");
    
	//! Creates a new geometry representation to a layer
	/*
		\param repType the geometrical representation being searched
		\param tableName name of the table that will stored the geometries
		\param desc description of the representation
		\return true if success and false otherwise
	*/
	virtual bool addGeometry ( TeGeomRep repType, const string& tableName="", const string& desc = "");

	//! Creates a new raster geometry to a layer
	/*
		\param par raster parameters
		\param objectId identifier of the object that has the raster geometry
		\param tableName name of the table that will stored the geometries
		\param desc description of the representation
		\return true if success and false otherwise
	*/
	virtual bool addRasterGeometry(TeRasterParams& par, const string& objectId, 
						   const string& tableName="", const string& desc="");

	//! Inserts a raster as a geometry of an object of the layer
	/*
		\param raster a pointer to a raster object
		\param objectId identifier of the object that has the raster geometry
		\param tableName the name of the table that will store the geometry (optional)
		\return true if success and false otherwise
		\note if there is already raster geometry associated to the object with id
		object id, this reference is deleted.
	*/	
	bool addRasterGeometry(TeRaster* raster, const string& objectId="");

	//! Inserts a reference to a raster file as a raster geometry of an object of the layer
	/*
		\param raster a pointer to a raster object
		\param objectId identifier of the object that has the raster geometry
		\param desc the description of the geometry (optional)
		\return true if success and false otherwise
		\note if there is already raster geometry associated to the object with id
		object id, this reference is deleted.
	*/	
	bool addRasterFileGeometry(TeRaster* raster, const string& objectId="", const string& desc="");


	//! Returns a vector with the identification of all objects of the layer that has a raster representation
	/*
		\param objectIds a vector to return the identification of the objects with a raster representation
		\param tilingType used to express that only some types of representations are looked for: 
			   0 any kind, 1 only the expansible and 2 only the non-expansible
		\return true if layer has at least one raster object and false otherwise
	*/
	virtual bool getRasterGeometries(vector<string>& objectIds, unsigned int tilingType=0);

	//! Returns a combination of all representations in the layer
	virtual int geomRep();

	//! Adds a representation to vector of representation of the layer
	virtual void addVectRepres(TeRepresentation* rep)
	{	repVector_.push_back(rep); }

	//! Returns a vector with all representations in the layer
	virtual TeRepresPointerVector& vectRepres()
	{	return repVector_; }

	//@}

private:

// -- Members

// -- General Description of a layer

	string		name_;					//!< layer name
	int			id_;					//!< layer unique identification
	TeDatabase*	db_;					//!< database connection to this layer
	TeProjection*	projection_;		//!< layer projection
	TeBox		box_;					//!< layer bounding box
	TeRaster*	raster_;				//!< layer raster representation

	TeAttrTableVector	attTables_; 	//!< Attributes associated to a layer
	TeRepresPointerVector repVector_;	//!< vector of representations associated to this layer
};

//! A map from a integer unique identifier to a pointer to layer
typedef map<int,TeLayer*> TeLayerMap;

/** \example createLayer.cpp
	Shows how to create a layer in a TerraLib database, and insert some vectorial data in this new layer
 */

/** \example addGeomRepresentation.cpp
	Shows how to create a point representation (centroid of polygons) to a layer in a TerraLib database.
 */
#endif

