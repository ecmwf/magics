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
/*! \file TeTheme.h
	\brief This file contains definitions about a theme in TerraLib database
*/
#ifndef  __TERRALIB_INTERNAL_THEME_H
#define  __TERRALIB_INTERNAL_THEME_H

#include "TeAbstractTheme.h"
#include "TeLayer.h"

using namespace std;

//! TerraLib definition of a Theme
/*!
\par 
A TeTheme represents a collection of objects selected from a TeLayer. This selection is based
on restrictions that can be non-spatial (e.g. POP > 10000), spatial (e.g. "inside box(0,0,100,100)
or temporal. The most simple selection is "all" objects of a layer.
\par 
A TeTheme contains the list of attribute tables of the layer used by a theme.
\par 
A TeTheme contains the visual presentation parameters for the objects that contains, for
all of its geometrical representations.
\par 
A TeTheme can store the parameters associated to how to separate its objects in groups. 
\par 
A TeTheme can store parameters associated to the creation of individual graphs on its objects
(e.g. pie bars that relate two or more of its attributes).
\par 
A TeTheme store the range of scales within it should be visible. 
\sa TeView TeTable
*/
class TL_DLL TeTheme: public TeAbstractTheme
{
public:
	//! Constructor
    TeTheme( const string& name="", TeLayer* layer=0, TeViewNode* parent=0, const int& view=0, const int& id=0);

	//! Constructor
	TeTheme(const TeViewNodeParams& params); 

	//! Copy constructor
	TeTheme (const TeTheme& other);

	//! Destructor
	~TeTheme ();

	//! Assignment operator
	TeTheme& operator= (const TeTheme& other); 

	/** @name Layer
		Methods related to the layer that gives origin to this theme.
	*/
	//@{	
	//! Returns the id of the source layer
	virtual int layerId() 
	{	return layerId_; }

	//! Sets the id of the source layer
	virtual void layerId(int i)
	{	layerId_ = i; }

	//! Sets the layer that is the source of objects of the theme
	/*! 
		\param layer a pointer to a TeLayer
	*/
	virtual void layer(TeLayer* layer);

	//! Returns a pointer to the layer from which the theme get its objects
	virtual TeLayer* layer() { return layer_; }

	//! Returns a pointer to a projection that is the spatial reference for the objects of this theme: same as of its layer
	virtual TeProjection* getThemeProjection();
	//@}

	//! Sets the spatial restriction to be a spatial relation with a box
	virtual void setSpatialRest(TeBox& box, TeGeomRep rep = TeGEOMETRYNONE, TeSpatialRelation relation = TeWITHIN); 

	//! Sets the spatial restriction to be a spatial relation with a geometry
	virtual void setSpatialRest(TeGeometry* geom, TeGeomRep rep = TeGEOMETRYNONE, TeSpatialRelation relation = TeWITHIN); 

	//! Returns the clause WHERE derived from the combination of all restricitions (spatial, attribute and temporal)
	virtual string sqlWhereRestrictions(TeRepresentation* rep=0);

	//! Creates an appropriate visual presentation to the raster of the theme
    virtual void createRasterVisual(TeRaster* rst=0);

	/** @name Collection
		Methods related to the materialization in the database of the theme as a collection of objects 
	*/
	//@{
	//! Returns the name of a table used to store the ids of the objects belonging to the theme
	virtual string	collectionTable() { return collectionTable_; }

	//! Sets the name of a table used to store the ids of the objects belonging to the theme
	virtual void collectionTable(const string& s) { collectionTable_ = s; }

	//! Returns the name of the collection auxiliary table
	virtual string collectionAuxTable() { return collectionAuxTable_; }

	//! Sets the name of the collection auxiliary table
	virtual void collectionAuxTable(string name) { collectionAuxTable_ = name; }

	//! Generates a optimized position (x,y) in the spatial extention of each object to position label ou graphs
	virtual bool generateLabelPositions(const std::string& objectId = ""); 

	//! Fills the sqlGridJoin_ and sqlGridFrom_ statements according to the status of the database
	virtual void loadTablesJoin(const string& geomTable="");

	/**
		Returns a SQL JOIN statement to get all the attributes of the theme objects, 
	    the attributes of the collection table, and the attributes of the extended 
		collection table 
	**/
	virtual string sqlGridJoin() { return sqlGridJoin_; }

	/** Returns a FROM clause of a SQL statement to get attributes of the theme objects, the attributes of the 
	collection table, and the attributes of the extended collection table 
	**/
	virtual string sqlGridFrom(const string& geomTable=""); 

	//! Build the theme collection: materializes the selection described in the theme in a collection table
	virtual bool buildCollection(std::string objectId = "");

	//! Create the auxiliar collection table used to represent objects with multiple versions in time  
	virtual bool createCollectionAuxTable();

	//! Populate the auxiliar collection table used to represent objects with multiple versions in time
	virtual bool populateCollectionAux(std::string objectId = "");
	//@}
		
	/** @name Grouping
		Methods related to grouping of objects of the theme. 
	*/
	//@{
	//! Save the grouping parameters in memory when there is no chronon
	virtual bool buildGrouping(const TeGrouping& g, TeSelectedObjects selectedObjects = TeAll,
		               vector<double>* dValuesVec = 0);

	//! Save the grouping parameters in memory when there is chronon
	virtual bool buildGrouping(const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec);
	
	//! Build the grouping and associate each object to its group in the collection table 
	virtual bool saveGrouping(TeSelectedObjects selectedObjects = TeAll);

	//! Save the theme grouping legends in the collection table  
	virtual bool saveLegendInCollection(TeSelectedObjects selectedObjects = TeAll, std::string objectId = "");

	//! Set the legend id for each object of the theme 
	virtual void setLegendsForObjects();

	//! Set the own legend id for each object of the theme 
	virtual void setOwnLegendsForObjects();

	//! Delete grouping
	virtual bool deleteGrouping(); 
	//@}

	/** @name Attribute Tables
		A theme can use one or more attribute tables of the layer that gives its data.
		These methods are related to the manipulation of these tables.
	*/
	//@{

	//! Loads the theme tables in the database
	virtual bool loadThemeTables(); 

	//! Add a new attribute table to a theme 
	virtual bool addThemeTable (TeTable& table);

	//! Add a new attribute table to a theme
	virtual void addThemeTable (string tableName);

	//! Verify if an attribute table is part of a theme
	virtual bool isThemeTable(int tableId);

	//! Verify if an attribute table is part of a theme
	virtual bool isThemeTable(string tableName);

	//! Returns the list of attribute tables used by this theme 
	virtual TeAttrTableVector& attrTables()
	{	return attTableVector_; }

	//! Sets the entire list of attribute tables used by this theme 
	virtual bool setAttTables(TeAttrTableVector& attrs);  

	//! Returns a vector of attribute tables, of a specific type, used by this theme
	virtual bool getAttTables(TeAttrTableVector& attrs, TeAttrTableType attType = TeAllAttrTypes); 

	//! Returns a representation of an attribute table  given name 
	virtual bool getTable(TeTable& table, const string tableName);

	//! Clears the list of attribute tables used by this theme
	virtual void clearAttTableVector() 
	{	attTableVector_.clear();	}

	//! Returns the temporal attribute table of the theme (TeEvent or TeFixedGeomDynAttr)
	/*
		\note A theme supports only one temporal attribute table
	*/
	virtual bool getTemporalTable(TeTable& table);

	//! Removes an attribute table from the list of tables of a theme
	virtual bool removeThemeTable(unsigned int index);

	//! Returns the the name of an attribute table that contains a given attribute
	virtual string getTableName(const string& attrName);

	/**
		Returns the full name of the i-th attribute resulting of the join of all attribute
		tables associated to the theme tables 
	*/
	virtual string getAttribute(unsigned int i);

	/**
		Check if the name of the i-th attribute resulting of the join of all attribute
		tables is an index or not
	*/
	virtual bool isIndex(unsigned int i);

	//! Returns the list of attributes of theme tables 
	virtual TeAttributeList sqlAttList() { return sqlAttList_;}

	//! Clears the list of attributes associated to the theme tables 
	virtual void clearAttList() {sqlAttList_.clear();}

	//! Returns the list of numerical attributes of the theme tables 
	virtual TeAttributeList sqlNumAttList() { return sqlNumAttList_;}

	//! Clears the list of numerical attributes associated to the theme tables 
	virtual void clearNumAttList() {sqlNumAttList_.clear();}

	//! Returns a SQL JOIN statement to reach to all attribute tables used by this theme
	virtual string	sqlJoin() { return sqlJoin_;}

	//! Returns a SQL FROM CLAUSE that gives access to all attribute tables used by this theme
	virtual string	sqlFrom() { return sqlFrom_;}

	//! Returns the string containing the SQL WHERE clause 
	virtual string&	sqlWhere() { return sqlWhere_;}

	//! Returns the alias vector of the names of the theme tables
	virtual vector<string>&	aliasVector() { return aliasVector_; }

	//! fill aliasVector_
	virtual void loadAliasVector();

	//! Refresh list of attributes of all the theme tables.
	/*
		\note All attributes are stored into sqlAttList_ and numeric attributes are stored into sqlNumAttList_.
	*/
	virtual void loadAttrLists();
	//@}

	//! Refreshes the bounding box of a theme according to its representation
	/*
		\note This functions is being kept for compatibility reasons with derived
		classes. IT SHOULD NOT BE USED BY APPLICATION USING ONLY THE BASIC CLASS.
	*/
	virtual void updateThemeBox() {}

	//! Updates the bounding box of a theme (in the database)
	/*
		\note This functions is being kept for compatibility reasons with derived
		classes. IT SHOULD NOT BE USED BY APPLICATION USING ONLY THE BASIC CLASS.
	*/
	virtual void updateThemeBox(const TeBox& /*box*/) {}
	//@}

	/** @name Locate geometries
	    Returns the geometry(ies) of the theme given coordinate
	*/
	//@{ 	
	virtual bool locatePolygon		(TeCoord2D &pt, TePolygon &polygon, const double& tol = 0.0);
	virtual bool locatePolygonSet   (TeCoord2D &pt, double tol, TePolygonSet &polygons);
	virtual bool locateLine		(TeCoord2D &pt, TeLine2D &line, const double& tol = 0.0);
	virtual bool locatePoint	(TeCoord2D &pt, TePoint &point, const double& tol = 0.0);
	virtual bool locateCell		(TeCoord2D &pt, TeCell &c, const double& tol = 0.0);
	//@}
	
	//! Get the set of objects corresponding to the object selection criteria
	virtual set<string> getObjects(TeSelectedObjects selectedObjects = TeAll);

	//! Get the set of objects corresponding to the list of items
	virtual set<string> getObjects(const vector<string>& itemVec);

	//! Get the set of items corresponding to the object selection criteria
	virtual vector<string> getItemVector(TeSelectedObjects selectedObjects);

	//! Get the set of items corresponding to the set of objects
	virtual vector<string> getItemVector(const set<string>& oidSet);

	//! Verifies if there are objects without geometries of a specific geometry representation
	virtual bool hasObjectsWithoutGeometries(TeGeomRep geomRep);

	//! Removes the objects without geometries of a specific geometry representation
	virtual bool removeObjectsWithoutGeometries(TeGeomRep geomRep);

	//! Save the the theme parameters in the database
	virtual bool save(); 

	//! Get the number of objects acessible by this theme
	virtual unsigned int getNumberOfObjects();

	//! Save the theme metadata in database. In this case, this metadata is saved by TeDatabase
	virtual bool saveMetadata(TeDatabase*) { return true; } 
	
protected:
	
	//! Create the auxiliar collection table used to represent objects with multiple versions in time  
	virtual bool createCollectionAuxTable(TeDatabase* db);

	//! Save the grouping parameters in memory when there is no chronon
	virtual bool buildGrouping(TeDatabase* db, const TeGrouping& g, TeSelectedObjects selectedObjects = TeAll,
		                       vector<double>* dValuesVec = 0);

	//! Save the grouping parameters in memory when there is chronon
	virtual bool buildGrouping(TeDatabase* db, const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec);

	//! Build the grouping and associate each object to its group in the collection table 
	virtual bool saveGrouping(TeDatabase* db, TeSelectedObjects selectedObjects = TeAll);

	//! Save the theme grouping legends in the collection table  
	virtual bool saveLegendInCollection(TeDatabase* db, TeSelectedObjects selectedObjects = TeAll, std::string objectId = "");
	
	//! Fill the sqlJoin_ and sqlFrom_ 
	virtual void loadThemeTablesJoin();
	
	//! Populate the collection table based in the theme restrictions
	virtual bool populateCollection(std::string objectId = ""); 

	//! list of attribute tables of the theme
	TeAttrTableVector	attTableVector_;

	//! List containing all the attributes of the theme tables
	TeAttributeList	sqlAttList_;
	
	//! List containing only the numeric attributes of the theme tables
	TeAttributeList	sqlNumAttList_;		

	//! Pointer to the layer that gives origin to this theme
	TeLayer*	layer_;

	//! Layer id
	int			layerId_;	
	
	// collection table name
	string		collectionTable_;
	string		collectionAuxTable_;

	// ----------------- theme tables information -----------------

	//! clause FROM: join with the theme tables and collection table
	string	sqlFrom_;		
	
	//! clause SELECT and FROM: join with the theme tables and collection table
	string	sqlJoin_;
	
	//! string containing the WHERE clause
	string sqlWhere_;

	//! clause FROM: join with the theme tables and auxiliar collection table
	string	sqlGridFrom_;
	
	//! clause SELECT and FROM: join with the theme tables and auxiliar collection table
	string	sqlGridJoin_;

	//! vector of alias to the attribute tables that are used more than once  
	vector<string>	aliasVector_;

	//! Load the theme metadata from database. In this case, this metadata is loaded by TeDatabase
	virtual bool loadMetadata(TeDatabase* ) { return true; } 

	//! Erase the theme metadata in database. In this case, this metadata is erased by TeDatabase
	virtual bool eraseMetadata(TeDatabase* ) { return true; } 
};


//!  This class implements a factory to create theme objects. 
/*!  
	 This class is a factory that create view nodes 
	 of the type TeTHEME, that is, theme objects.

	\sa
     TeViewNodeFactory TeViewNodeParams TeTheme  
*/
class TL_DLL TeThemeFactory : public TeViewNodeFactory
{
public:
	//! Constructor 
	TeThemeFactory() : TeViewNodeFactory((int)TeTHEME)
	{}

	//! Created theme objects 
	TeViewNode* build(TeViewNodeParams* params)
	{	
		TeViewNodeParams auxParams = *params;
		return new TeTheme(auxParams);	
	}

	TeViewNode* build()
	{
		return new TeTheme();
	}
};


/*! \example createTheme.cpp
	Shows how to create themes in TerraLib.
 */

/*! \example themeGrouping.cpp
	Shows how to do a grouping on the objects of a TerraLib theme.
 */

/*! \example rasterSlicing.cpp
	Shows how to  create a legend over a raster data, stored in a layer TerraLib.
 */


#endif

