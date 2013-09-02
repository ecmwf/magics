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
/*! \file TeDatabase.h
    \brief This file contains structures and definitions about database support in TerraLib
*/
#ifndef  __TERRALIB_INTERNAL_DATABASE_H
#define  __TERRALIB_INTERNAL_DATABASE_H

#ifdef WIN32 
#pragma warning ( disable: 4786 ) 
#endif

#include "TeDefines.h"

#include "TeDefines.h"
#include "TeDataTypes.h"
#include "TeTable.h"
#include "TeGeometry.h"
#include "TeStatistics.h"
#include "TeProject.h"
#include "TeRaster.h"
#include "TeView.h"
#include "TeVisual.h"
#include "TeMetaModelCache.h"
#include "TeSharedPtr.h"

class TeLayer;
class TeDatabaseFactoryParams;
class TeRasterParams;
class TeTheme;
class TeTime;
class TeTimeInterval;

#include <set>
using namespace std;

          
/*! \enum TeCursorLocation
	Location of a cursor on record set
 */
enum TeCursorLocation { TeCLIENTESIDE, TeSERVERSIDE };

/*! \enum TeCursorType
	Type of cursor on record set
 */
enum TeCursorType { TeUNIDIRECTIONAL, TeBIDIRECTIONAL, TeRANDOMACCESS };

/*! \enum TeCursorEditType
	Type of edition on record set cursor
 */
enum TeCursorEditType { TeREADONLY, TeREADWRITE };

/*! \enum TeCursorDataType
	Type of edition on record set cursor
 */
enum TeCursorDataType { TeBINARYCURSOR, TeTEXTCURSOR };


/*! \enum TeSpatialIndexType
	Type of spatial index
 */
enum TeSpatialIndexType {TeRTREE, TeQUADTREE};

//! A vector of objects identifications (object_id)
typedef vector<string>				TeKeys;

//! A Map of objects identifications (object_id) to a distance value
typedef map<string, double>			TeKeysToDist;

//! A Vector of pointers to TeGeometry 
typedef vector<TeGeometry*> TeGeometryVect;

//! A Map of the objects identifications to a set of statistical values
typedef map<string, TeStatisticsDimensionVect>  TeObjectStatistics;


class TeDatabasePortal;


//! An abstract database class
/*!
	Instances of this classes represent connections
	to a database server. It includes the host name, 
	user name and password parameters of the connection.
	It should be implemented by the drivers to specific
	database servers as: MySQL, Ado, PostgreSQL or any
	other server to be used in TerraLib applications.
	When possible, default implementations using ANSI SQL
	are provided. 
	Methods that rely on special features of the derived
	driver are left as purely virtual.

  \sa
  TeTheme, TeView, TeLayer, TeAttribute
*/
class TL_DLL TeDatabase 
{
public: 
	//! Empty constructor
	TeDatabase();

	//! Destructor
	virtual ~TeDatabase();

	//! Assignment operator
	virtual TeDatabase& operator=(const TeDatabase& other);

	//! Operator ==
	virtual bool operator== (const TeDatabase& other) const
	{	
		if (dbmsName_ != other.dbmsName_ ||
			database_ != other.database_ ||
			host_  != other.host_ ||
			user_ != other.user_ )
			return false;
		return true;
	}
	/** @name Members
	*  Methods to access class private members
	*/
	//@{		
	//! Returns the error message from the server
	virtual string errorMessage ()
	{ return errorMessage_; }

	//! Returns the error number from the server
	virtual int	errorNum ()
	{	return errorNumber_; }

	//! Return the user name of the connection opened
	string	user () 
	{ return user_; }

	//! Set the current user name to view
	void user(string value) { user_=value;}

	//! Returns the host name of the connection opened
	string	host () 
	{ return host_; }

	//! Returns the user's password of the connection opened
	string	password () 
	{ return password_; }

	//! Returns the database name of the connection opened
	string	databaseName () 
	{ return database_; }
	
	//! Returns the Database Management System name
	string	dbmsName () 
	{ return dbmsName_; }

	//! Returns the port number of the connection opened
	int	portNumber () 
	{ return portNumber_; }

	//! Returns the  map of layers in the database
	TeLayerMap&	layerMap () 
	{ return metaModel_->layerMap(); }

	//! Returns the  map of views in the database
	TeViewMap&	viewMap () 
	{ return metaModel_->viewMap(); }

	//! Returns the  map of themes in the database
	TeThemeMap&	themeMap () 
	{ return metaModel_->themeMap(); }

	//! Returns the map of invalid themes in the database
	TeThemeMap&	invalidThemeMap() 
	{ return metaModel_->invalidThemeMap(); }

	//! Returns the  map of projects in the database
	TeProjectMap&  projectMap ()
   { return metaModel_->projectMap(); }

	//! Returns the  map of legends in the database
	TeLegendEntryMap& legendMap () 
	{ return metaModel_->legendMap(); }

	//! Returns the set of relations between tables
	multiset<int>& relationMSet () 
	{ return metaModel_->relationMSet(); }

	//! Clears structures in memory (legendMap, layerMap, viewMap...) used to optimize database access
	virtual void clear();

	//@}

	/** @name Database
	*  Methods related to database and connection creation. 
	*  These methods return TRUE when the operation was successfull.
	*  Otherwise return FALSE and when possible an error message is captured.
	*/
	//@{	
	//! Creates a new database and open a connection to it
	virtual bool newDatabase(const string& database, const string& user, const string& password, const string& host, const int &port=-1, bool terralibModel=true) = 0;
	
	//! Opens a connection to a database server passing all parameters needed
	virtual bool connect (const string& host, const string& user, const string& password, const string& database, int port = -1) = 0;

	//! Show the server databases (only for MySQL, Oracle and PostgreSQL)
	virtual bool showDatabases (const string& /* host */, const string& /* user */, const string& /* password */, vector<string>& /* dbNames */ , int /* port */ = -1)
	{ return true; }

	//! Opens a connection without parameters. When possible, should be implemented by the derived classes 
	virtual bool connect (const string&  = "") { return false; };

	//! Check if there is an opened connection
	bool isConnected () 
	{ return isConnected_; }

	//! Closes the connection
	virtual void close() = 0;

	//! Returns a string that the describes the parameters of the database connected
	/*
		The expected return value is similar to: dbms_server;hostname;portnumber;databasename;user;password
	*/
	virtual string getDatabaseDescription();
	
  
  /**
   *  This is for TeFactory compatibility ( Invalid TeDatabaseFactory
   *  requests will try to create an default object using this function ).
   */
  static TeDatabase* DefaultObject( const TeDatabaseFactoryParams& )
  {
    std::cout << std::endl << "TeDatabase::DefaultObject - " <<
      " - Trying to create an invalid TeDatabase default object." <<
      std::endl;
    throw;
      
    return 0;
  };
  
	//@}

	/** @name Tables
	  Methods related to table manipulation.
		\note refer to data model documentation in order to understand the meanning of 
		each tables and its fields
	*/
	//@{	

	//! Returns a list of tables in a database
	virtual bool listTables(vector<string>& /* tableList */) { return false; }

	//! Verifies if a table exist in the database
	virtual bool tableExist(const string& table) = 0;
	
	//! Verifies if a table has a column
	virtual bool columnExist(const string& table, const string& column, TeAttribute& attr) = 0;

	//! Update a column
	virtual bool allowEmptyString(const string& /* tableName */, const string& /* column */) { return false; }

	//! Verifies and modifies the column names of the table. It returns if the table was modified  
	virtual bool validTable (TeTable& table);

	//! Return the name of an attribute table given its identifier
	string getTableName(int tableId);

	//! Return a valid name for a table from a initial one
	string getNewTableName(const string& n);

	//! Concatenate field values
	/*!
      \param fNamesVec vector containing the fields to be concatenated
	*/
	virtual string getConcatFieldsExpression(const vector<string>& fNamesVec);

	//! Creates a table
	/*!
      \param table table name
      \param attr table list of attributes
	 */
	virtual bool createTable(const string& table, TeAttributeList &attr) = 0;

	//! Deletes a table. Do not allow the deletion of model tables
	 virtual bool deleteTable (const string& table);

	//! Adds a column to a table
	/*!
      \param table table name
      \param rep representation of the column being created
	*/
	virtual bool addColumn (const string& table, TeAttributeRep &rep) = 0;

	//! Deletes a column to a table
	/*!
      \param table table name
      \param colName name of the column being deleted
	*/
	virtual bool deleteColumn (const string& table, const string& colName);
	
	//! Creates a reationship between two tables
	/*!
      \param relName relationship name
      \param table table that will receive the foreign key
	  \param fieldName column that will be foreign key
	  \param relatedTable table that exports the foreign key
	  \param relatedField field that will the exported foreign key
	  \param cascadeDeletion flag that indicates if the deletion should be propagated
	*/
	virtual bool createRelation (const string& relName, const string& table, const string& fieldName, 
						const string& relatedTable, const string& relatedField, bool cascadeDeletion) = 0;

	//! Checks if a relation exist
	/*!
      \param tableName table where the relashionship exists
	  \param relName relationship name
	*/
	virtual TeDBRelationType existRelation(const string& tableName, const string& relName) = 0;

	//! Removes a relation exist
	/*!
	  \param name relationship name
      \param table table where the relashionship exists
	  \return TRUE if succeed and FALSE otherwise
	*/
	virtual bool deleteRelation(const string& name, const string& table);

	//@}
	
	/** @name Query
	*  Methods related to query the database
	*/
	//@{	
	//! Executes a SQL command that doesnt return a record set. Tipically a data definition comand
	virtual bool execute ( const string &sql) = 0;

	//! Returns a database portal.
	/*!
		A database portal is used to submit queries to the database and to navigate over the
		resulting record set
	*/
	virtual TeDatabasePortal* getPortal () = 0;

	//! Get the values that satisfy the query 
	virtual bool inClauseValues(const string& query , const string& attribute, vector<string>& inClauseVector);

	//@}

	/** @name  Data Model
		 Methods that create the database model suggested in TerraLib. 
		 Classes in TerraLib materialize the DataModel suggested.
	*/
	//@{
	//! Creates the entire TerraLib conceptual model.
	/*
      \param withIntegrity flag that indicates that the referencial integrity should be implemented.
	  \param createIndex flag that indicate that the indexes should be created
	*/
	virtual bool createConceptualModel(bool withIntegrity = true, bool newDatabase = true, bool createIndex = true);

	//! Creates the referencial integrity of the conceptual model
	virtual bool defineIntegrity(void);

	//! Create the indexes for the tables in the conceptual model
	virtual bool createIndex(const string& tableName, const string& indexName, const string& columnsName);

	//! Creates a table to store version database information 
	virtual bool createDatabaseTable(); 
	
	//! Creates a table to store projectinon information
	virtual bool createProjectionTable();

	//! Creates a table to store Layers information
	virtual bool createLayerTable ();

	//! Creates a table to store Representations information
	virtual bool createRepresentationTable ();

	//! Creates a table to store Views information
	virtual bool createViewTable ();

	//! Creates a table to store Themes information
	virtual bool createThemeTable ();

	//! Creates a table to store the groupings associated with a theme
	virtual bool createGroupingTable();

	//! Creates a table to store information about the attribute tables used by a theme
	virtual bool createThemeTablesTable();

	//! Creates a table to store legends
	virtual bool createLegendTable ();

	//! Creates a table to store visual definitions
	virtual bool createVisualTable();

	//! Creates a table to store raster visual definitions
	virtual bool createVisualRasterTable();

	//! Creates a table to store information about the non-spatial tables associated to this layer
	virtual bool createLayerTableTable();

	//! Creates a table to store information about the external tables related to non-spatial tables of layers
	virtual bool createTablesRelationTable();

	//! Creates a table to store information about projects: an structure that groups views
	virtual bool createProjectTable();

	//! Creates a table to store the relation between project/views information
	virtual bool createProjectViewTable();
	//@}

	/** @name  Project
		 Methods related to the manipulation of projects
	*/
	//@{
	//! Load information about all projects stored in the database
	virtual bool loadProjectSet(); 

	//! Load information about a particular project
	virtual bool loadProject(TeProject* project); 
	
	//! Insert information about a project
	virtual bool insertProject(TeProject *project) = 0;
	
	//! Update information about a project
	virtual bool updateProject(TeProject *project);	

	//! Delete a project from the database
	virtual bool deleteProject(int projectId);	

	//! Insert a project/view relation
	virtual bool insertProjectViewRel(int projectId, int viewId);

	//! Deletes a project/view relation
	virtual bool deleteProjectViewRel(int projectId, int viewId);

	//! Check whether a given project already exists in the database (it is not case sensitive).
	virtual bool projectExist(const string& projectName);
	//@}

	/** @name  Geometries
		 Methods that create tables that store the spatial data
	*/
	//@{

	//! Creates a table for a polygon geometries
	virtual bool createPolygonGeometry (const string& tableName);

	//! Creates a table for line geometries
	virtual bool createLineGeometry (const string& tableName);

	//! Creates a table for point geometries
	virtual bool createPointGeometry (const string& tableName);

	//! Creates a table for cell geometries
	virtual bool createCellGeometry (const string& tableName);
 
	//! Creates a table for a text geometries
	virtual bool createTextGeometry (const string& tableName);

	//! Creates a table for a arc geometries
	virtual bool createArcGeometry (const string& tableName);

	//! Creates a table for a node geometries
	virtual bool createNodeGeometry (const string& tableName);

	//! Creates a table to store raster representations of objects
	virtual bool createRasterGeometry(const string& tableName);

	//! Creates a table to store information about raster representation
	virtual bool createRasterMetadataTable(const string& tableName);
	
    //! Creates a table for raster geometries
	virtual bool createRasterTable (const string& tableName);

	//! Creates a table to store information about objects in a theme
	virtual bool createCollectionTable(const string& tableName);
	//@}

	/** @name Non-spatial tables
	*  Retrieving/Inserting/Modifying generic tables in the database. 
	*/
	//@{
	//! Retrieves information about some tables
	/*
		\param atts returning vector of tables information 
		\param attType type of the tables that are being looked for (optional)
		\return TRUE if any table was found and FALSE otherwise
	*/
	virtual bool getAttrTables(TeAttrTableVector& atts, TeAttrTableType attType = TeAllAttrTypes);

	//! Inserts information about a link to an external table
	/*
		\param tableId table identification (from te_layer_table)
		\param tField link column name of the static table
		\param eTable name of the external table
		\param eField name of the link column of the related table
		\param relId returns the identification of the relation
	*/
	virtual bool insertRelationInfo(const int tableId, const string& tField,
									const string& rTable, const string& rField, int& relId) = 0;

	//! Inserts information about an attribute table
	virtual bool insertTableInfo (int layerId, TeTable &table, const string& user="") = 0;

	//! Updates information about an attribute table
	virtual bool updateTableInfo (int layerId, TeTable &table, const string user="");

	//! Retrieves the metainformation about an attribute table
	virtual bool loadTableInfo(TeTable& table);

	//! Saves a table and its contents in the database
	virtual bool insertTable(TeTable &table);

	//! Alter a property of a table 
	virtual bool alterTable(const string& tableName, TeAttributeRep& rep, const string& oldColName="");

	//! Alter the table name 
	virtual bool alterTable(const string& oldTableName, const string& newTablename);

	//! Saves a large binary objects (BLOB) in a row table 
	virtual bool insertBlob (const string& tableName, const string& columnBlob, TeAttributeRep& columnId, const string& valueId, unsigned char* data, int size);

	//! Saves a large binary objects (BLOB) in a row table 
	virtual bool insertBlob (const string& tableName, const string& columnBlob, TeAttributeRep& columnId, const string& valueId, const string& fileName);

	//! Saves a large binary objects (BLOB) in a row table 
	virtual bool insertBlob (const string& tableName, const string& columnBlob, const string& whereClause, unsigned char* data, int size) = 0;

	//! Saves a large binary objects (BLOB) in a row table 
	virtual bool insertBlob (const string& tableName, const string& columnBlob, const string& whereClause, const string& fileName);
	
	//! Updates a table and its contents in the database
	virtual bool updateTable	(TeTable &table);

	//! Retrieves a table and its contents from the database
	virtual bool loadTable		(const string& tableName, TeTable &table);

	//! Retrieves a table (or part of it) accordingly to a criteria written in SQL
	virtual bool selectTable	(const string& tableName, const string& criteria, TeTable &table);

	//!	Deletes all tables, of a particular type, associated to a layer
	virtual bool deleteLayerTable	(int layerId, TeAttrTableType ttype = TeAttrStatic);
	//@}

	/** @name Projection
	*  Accessing/Inserting/Modifying projection information into the database. 
	*/
	//@{ 	

	//! Insert information about a geographical projection
	virtual bool insertProjection (TeProjection *proj) = 0;	

	//! Updates information about a geographical projection
	virtual bool updateProjection (TeProjection *proj);	

	//! Retrieves information about a geographical projection identified by its database id
	virtual TeProjection* loadProjection (int id);
	//@}

	/** @name Layers
	*  Retrieving/Inserting/Modifying/Deleting layers in the database. 
	*/
	//@{ 
	//! Insert information about a layer
	virtual bool insertLayer	(TeLayer *layer) = 0;
	
	//! Update information about a layer
	virtual bool updateLayer	(TeLayer *layer);	

	//! Load information about all layers stored in the database
	/*
		\param loadAttrList  indicates if the attribute list of each attribute table should be loaded
	*/
	virtual bool loadLayerSet	(const bool& loadAttrList = true);

	//! Load information about a particular layer 
	/*
		\param layer		a pointer to a layer that will be loaded from database
		\param loadAttrList indicates if the attribute list of each attribute table should be loaded
	*/
	virtual bool loadLayer		(TeLayer* layer, const bool& loadAttrList = true);

	//! Load layer tables 
	/*
		\param layer		a pointer to a layer whose attribute table will be loaded from database
		\param loadAttrList indicates if the attribute list of each attribute table should be loaded
	*/
	virtual bool loadLayerTable (TeLayer* layer, const bool& loadAttrList = true);

	//! Delete a particular layer passing its id
	virtual bool deleteLayer	(int layerId);

	//! Check if a particular layer exists (passing its id)
	virtual bool layerExist		(int id);

	//! Check if a particular layer exists (passing its name)
	virtual bool layerExist		(string layerName);

	//! Checks whether a given layer name is valid or already exists in the database
	/*!
		\param n		layer name to be checked
		\return the modified valid layer name
	*/	
	virtual string getNewLayerName(const string& n);
	//@}

	/** @name Representation
	*  Retrieving/Inserting/Modifying/Deleting representations in the database. 
	*/
	//@{ 
	//! Inserts information about a geometrical representation
	virtual bool insertRepresentation (int layerId, TeRepresentation& rep) = 0;	

	//! Updates information about a geometrical representation
	virtual bool updateRepresentation (int layerId, TeRepresentation& rep);	

	//! Updates the box information of a layer in the corresponding metadata table
	virtual bool updateLayerBox(TeLayer* layer);

	//@}

	/** @name Raster Tables
	*  Retrieving/Inserting/Modifying/Deleting raster representations in the database. 
	*/
	//@{ 
	//! Inserts information about a raster geometry associated to an object
	/*!
		\param tableName name of the table that stores the raster representation
		\param par raster parameters 
		\param objectId identification of the object associated to the raster geometry
	*/
	virtual bool insertRasterGeometry(const string& tableName, TeRasterParams& par, const string& objectId = "");

	//! Updates the information about the raster geometry associated to an object of a layer
	/*!
		\param layerId layer unique database identification
		\param par raster parameters 
		\param objectId identification of the object associated to the raster geometry
	*/
	virtual bool updateRasterRepresentation(int layerId, TeRasterParams& par, const string& objectId="");

	//! Returns the name of the table where the raster geometry associated to an object of a leyr
	/*!
		\param layerId layer unique database identification
		\param objectId identification of the object associated to the raster geometry
	*/
	virtual string getRasterTable(int layerId, const string& objectId);
	
	//! Inserts metadata information about a particular raster geometry
	/*!
		\param tableName name of the table that stores the metadata
		\param geomId  geometry unique database identification
		\param par raster parameters 
	*/
	virtual bool insertRasterMetadata (const string& tableName, int geomId, TeRasterParams& par);

	//! Updates metadata information about a particular raster geometry
	/*!
		\param tableName name of the table that stores the metadata
		\param geomId  geometry unique database identification
		\param par raster parameters 

	*/	
	virtual bool updateRasterMetadata (const string& tableName, int geomId, TeRasterParams& par);
	//@}
	/** @name Views
	*  Retrieving/Inserting/Modifying/Deleting views in the database. 
	*/
	//@{ 
	//! Inserts a view
	virtual bool insertView (TeView *view) = 0;	

	//! Updates a view
	virtual bool updateView (TeView *view);	

	//! Loads a set of views belonging to a user
	/*
		\param user			 the user name 
		\param loadAttrList  indicates if the attribute list of each attribute table should be loaded
		\param visualClass  identifies which concrete class od visual should be instantiated.
		The default is TerraLib basic visual.
	*/
	virtual bool loadViewSet (const string& user, const bool& loadAttrList = true, const string& visualClass="tevisual");

	//! Load a view
	/*
		\param view			a pointer to a view that will be loaded from database
		\param loadAttrList indicates if the attribute list of each attribute table should be loaded
		\param visualClass  identifies which concrete class od visual should be instantiated.
		The default is TerraLib basic visual.
	*/
	virtual bool loadView (TeView* view, const bool& loadAttrList = true, const string& visualClass="tevisual");

	//! Delete a view
	virtual bool deleteView (int viewId);

	//! Recursive inserting of a view tree.
	virtual bool insertViewTree (TeViewTree *tree) = 0;	

	//! Recursive load view tree.
	/*
		\param view			a pointer to a view that will be loaded from database
		\param id			a specific theme or viewtree id of this view that will be loaded from database
		\param loadAttrList indicates if the attribute list of each attribute table should be loaded
		\param visualClass  identifies which concrete class od visual should be instantiated.
		The default is TerraLib basic visual.
	*/
	virtual TeViewTree* loadViewTree(TeView* view, int id, const bool& loadAttrList = true, const string& visualClass = "tevisual");

	//! Recusive updating of a view tree.
	virtual bool updateViewTree (TeViewTree *tree);
	
	//! Check whether a given view already exists in the database
	virtual bool viewExist(string viewName);

	//@}

	/** @name Themes
	*  Retrieving/Inserting/Modifying/Deleting themes and group of themes in the database. 
	*/
	//@{ 
	//! Inserts a group of themes in the database
	virtual bool insertThemeGroup	(TeViewTree* tree) = 0;

	//! Inserts an abstract theme in the database.
	virtual bool insertTheme		(TeAbstractTheme *theme) = 0; 	

	//! Updates an asbtract theme in the database
	virtual bool updateTheme		(TeAbstractTheme *theme);

	//! Loads a theme from the database
	virtual bool loadTheme			(TeAbstractTheme *theme, const bool& loadAttrList = true, const string& visualClass = "tevisual");

	//! Loads all attribute tables of a theme
	virtual bool loadThemeTable		(TeTheme* theme, const bool& loadAttrList = true);

	//! Erases the theme group identified by themeId
	virtual bool deleteThemeGroup	(int themeId);

	//! Erases the theme identified by themeId
	virtual bool deleteTheme		(int themeId);

	//! Erases the legends of a theme identified by themeId
	virtual bool deleteLegend		(int themeId);
	
	//! Inserts information about a table used by a theme 
	bool insertThemeTable(TeTheme *theme, TeTable& inputTable);

	//! Inserts information about a table used by a theme 
	virtual bool insertThemeTable	(int themeId, int tableId, int relationId, int tableOrder) = 0; 

	//! Updates  information about the tables used by a theme 
	virtual bool updateThemeTable	(TeTheme *theme);
	
	//! Removes a table from the theme
	bool removeThemeTable(TeTheme *theme, int tableOrder);

	//! Inserts information about a grouping used in a theme
	virtual bool insertGrouping (int themeId, const TeGrouping& grouping);

	//! Updates information about a grouping used in a theme
	virtual bool updateGrouping (int themeId, const TeGrouping& grouping);

	//! Generates the label position (x,y) to each object of a theme or of a particular object
	virtual bool generateLabelPositions(TeTheme *theme, const std::string& objectId = "");

	//! Check whether a given theme already exists in the database
	virtual bool themeExist(string themeName);

	//! Checks whether a given theme name is valid or already exists in the database
	/*!
		\param n		theme name to be checked
		\return the modified valid theme name
	*/	
	virtual string getNewThemeName(const string& n);

	//@}

	/** @name Legend
	*  Retrieving/Inserting/Modifying/Deleting legends in the database. 
	*/
	//@{ 
	//! Inserts legend in the database
	virtual bool insertLegend	(TeLegendEntry *legend) = 0;	
	//! Updates legend entries in the database
	virtual bool updateLegend	(TeLegendEntry *legend);	

	virtual bool updateLegend   (vector<TeLegendEntry>& legVec);
	virtual bool loadLegend		(TeAbstractTheme *theme, const string& visualClass = "tevisual");
	virtual bool updateVisual	(TeLegendEntry *legend);
	virtual bool updateVisual   (vector<TeLegendEntry>& legVec);
	//@}

	/** @name Polygon
	// Accessing/Inserting/Modifying polygon geometries into the database
	*/
	//@{ 
	//! Inserts a polygon set in a geometry table
	virtual bool insertPolygonSet (const string& table, TePolygonSet &ps);	

	//! Updates a polygon set in a geometry table
	virtual bool updatePolygonSet (const string& table, TePolygonSet &ps);	

	//! Returns all polygons in a table given a criteria expressed as an SQL where statement
	virtual bool selectPolygonSet (const string& table, const string& criteria, TePolygonSet &ps);

	//! Returns all polygons that represents objects of a particular theme
	virtual bool loadPolygonSet	(TeTheme* theme, TePolygonSet &ps);

	//! Returns all polygons  that represents objects of a particular geoid
	virtual bool loadPolygonSet	(const string& table, const string& geoid, TePolygonSet &ps);

	//! Returns all polygons inside a given box
	virtual bool loadPolygonSet	(const string& table, TeBox &box, TePolygonSet &ps);

	//! Returns a database portal to iterate over the polygons that are inside a given box
	/*
		\return A database portal pointer if there is any polygons inside the box
		\return A null pointer if there isn't polygons to iterate
		\note The database portal pointer should be deleted by the application
	*/
	virtual TeDatabasePortal* loadPolygonSet(const string& table, TeBox &box);

	//! Returns the first polygon that contais a given coordinate
	virtual bool locatePolygon		(const string& table, TeCoord2D &pt, TePolygon &polygon, const double& tol = 0.0);

	//! Returns the polygons that contains a give coordinate
	virtual bool locatePolygonSet   (const string& table, TeCoord2D &pt, double tol, TePolygonSet &polygons);

	//! Inserts a polygon in a geometry table
	virtual bool insertPolygon		(const string& table, TePolygon &p) = 0;	

	//! Updates a polygon in a geometry table
	virtual bool updatePolygon		(const string& table, TePolygon &p) = 0;
	//@}

	/** @name Line
	*  Accessing/Inserting/Modifying Line geometries into the database. 
	*/
	//@{ 	
	// Accessing/Inserting line geometries into the database
	virtual bool insertLineSet	(const string& table, TeLineSet &ls);	
	virtual bool updateLineSet	(const string& table,TeLineSet &ls);
	virtual bool loadLineSet	(const string& table, const string& geoid, TeLineSet &ls);
    virtual bool loadLineSet	(TeTheme* theme, TeLineSet &ls);
	virtual bool loadLineSet	(const string& table, TeBox &box, TeLineSet &linSet);
	virtual TeDatabasePortal*    loadLineSet (const string& table, TeBox &box);
	virtual bool selectLineSet	(const string& table, const string& criteria, TeLineSet &ls);

    virtual bool insertLine		(const string& table, TeLine2D &l) = 0;		
	virtual bool updateLine		(const string& table, TeLine2D &l) = 0;
	virtual bool locateLine		(const string& table, TeCoord2D &pt, TeLine2D &line, const double& tol = 0.0);
	virtual bool locateLineSet	(const string& table, TeCoord2D &pt, TeLineSet & ls, const double& tol = 0.0);

	//@}

	/** @name Point
	*  Accessing/Inserting/Modifying Point geometries into the database. 
	*/
	//@{ 	
	// Accessing/Inserting point geometries into the database
	virtual bool insertPointSet	(const string& table, TePointSet &ps);	
    virtual bool updatePointSet (const string& table, TePointSet &ps);
	virtual bool loadPointSet	(const string& table, TeBox &box, TePointSet &ps);
	virtual TeDatabasePortal* loadPointSet	(const string& table, TeBox &box);
	virtual bool loadPointSet	(const string& table, const string& geoid, TePointSet &ps);
	virtual bool loadPointSet	(TeTheme* theme, TePointSet &ps);
	virtual bool selectPointSet (const string& table, const string& criteria, TePointSet &ps);

    virtual bool insertPoint	(const string& table, TePoint &p) = 0;	
	virtual bool updatePoint	(const string& table, TePoint &p);
	virtual bool locatePoint	(const string& table, TeCoord2D &pt, TePoint &point, const double& tol = 0.0);
	virtual bool locatePointSet (const string& table, TeCoord2D &pt, TePointSet &pointSet, const double& tol=0.0);
	//@}

	/** @name Text
	*  Accessing/Inserting/Modifying Text geometries into the database. 
	*/
	//@{ 	
	// Accessing/Inserting text geometries into the database
	virtual bool insertTextSet	(const string& table, TeTextSet &ts);	
	virtual bool updateTextSet	(const string& table, TeTextSet &ts);	
	virtual bool loadTextSet	(const string& table, const string& geoid, TeTextSet &ts);
	virtual bool selectTextSet	(const string& table, const string& criteria, TeTextSet &ts);

	virtual bool insertText		(const string& table, TeText &t) = 0;	
	virtual bool updateText		(const string& table, TeText &t);
	virtual bool locateText		(const string& table, TeCoord2D &pt, TeText &text, const double& tol = 0.0);
	virtual bool locateTextSet	(const string& table, TeCoord2D &pt, TeTextSet &textSet, const double& tol = 0.0);
	//@}

	/** @name Arc
	*  Accessing/Inserting/Modifying Arc geometries  into the database. 
	*/
	//@{ 	
	//! Inserts an arc set geometry in the database.
	virtual bool insertArcSet	(const string& table, TeArcSet &as);	

	//! Updates arc set geometry in the database.
	virtual bool updateArcSet	(const string& table, TeArcSet &as);	

	//! Loads an arc geometry from the database that has the specified object id (geoid).
	virtual bool loadArcSet		(const string& table, const string& geoid, TeArcSet &as);

	//! Inserts an arc geometry in the database.
	virtual bool insertArc		(const string& table,TeArc &arc) = 0;

	//! Updates an arc geometry in the database.
	virtual bool updateArc		(const string& table,TeArc &arc);
	//@}

	/** @name Node
	*  Accessing/Inserting/Modifying Node geometries into the database. 
	*/
	//@{ 	
	// Accessing/Inserting node geometries into the database
	virtual bool insertNodeSet	(const string& table, TeNodeSet &ns);	
	virtual bool updateNodeSet	(const string& table, TeNodeSet &ns);	
	virtual bool loadNodeSet	(const string& table, const string& geoid, TeNodeSet &ps);

	virtual bool insertNode		(const string& table, TeNode &node) = 0;	
	virtual bool updateNode		(const string& table, TeNode &node);	
	//@}

	/** @name Cell
	*  Accessing/Inserting/Modifying Cell geometries  into the database. 
	*/
	//@{ 	
	// Accessing/Inserting cell geometries into the database
	virtual bool insertCellSet	(const string& table, TeCellSet &cs);	
	virtual bool updateCellSet	(const string& table, TeCellSet &cs);	
	virtual bool loadCellSet	(const int& layerId, const string& table, const string& geoid, TeCellSet &cs);
	virtual bool selectCellSet	(const int& layerId, const string& table, const string& criteria, TeCellSet &cs);

	virtual bool insertCell		(const string& table, TeCell &c) = 0;
	virtual bool updateCell		(const string& table, TeCell &c);
	virtual bool locateCell		(const string& table, TeCoord2D &pt, TeCell &c, const double& tol = 0.0);
	//@}
	
	//! Insert a raster block into the database
	/*!
      \param table table name
	  \param blockId block unique identifier
	  \param ll  block lower left coordinate
	  \param ur  block upper right coordinate
	  \param buf block binary data
	  \param size block size
	  \param band block band
	  \param res block resolution factor
	  \param subband sub band definitiion
	*/
	virtual	bool insertRasterBlock(const string& table, const string& blockId, const TeCoord2D& ll, const TeCoord2D& ur, unsigned char *buf,unsigned long size, int band=0, unsigned int res=1, unsigned int subband=0) = 0;

	//! Inserts the entire visual of a Legend
	virtual	bool insertVisual (TeLegendEntry *legend);

	//! Insert raster visual
	virtual bool insertRasterVisual (int themeId , TeRasterVisual* rasterVisual);

	//! Returns the raster associated to a layer
	virtual TeRaster* loadLayerRaster(int layerId, const string& objectId="", const char& mode = 'r');

	//! Creates a lookup table (used for pallete raster representations)
	virtual bool createLUTTable(const string& name);

	//! Loads a look up table associated to a raster and fills its parameters
	virtual bool loadRasterLUT(TeRasterParams* par);

	//! Creates a spatial index for a spatial table. In order to get correct column names to index, see getSpatialIdxColumn method.
	virtual bool createSpatialIndex(const string& table, const string& columns, TeSpatialIndexType /*type*/ = TeRTREE,short /* level */ =0,short /* tile */ =0);

	//! Creates a spatial metadata for a spatial table. It is implemented only in spatial databases.
	virtual bool insertMetadata(const string& /* table */, const string& /* column */, double /* tolx */, double /* toly */,TeBox &/* box */,short /* srid */ =0) { return true; };

	//! Returns the name of the column that wiil be the spatially indexed, for a given type of geometry table 
	virtual string getSpatialIdxColumn(TeGeomRep rep);

	//! Update box information in a table that has a set of columns to store a box value
	/*
		This method gives to the drivers the ability to update box columns, considering precision issues
		that are particular to the driver. The box should be stored in 4 columns (lower_x, lower_y, upper_x, upper_y)
		\param tableName table name
		\param keyColumnName name of the column that is primary key
		\param idValue key value of the row that should be updated
		\param box new box value
	*/
   virtual bool updateBBox(const string& tableName, const string& keyColumnName, int keyValue, const TeBox& box);  


	/** @name Spatial Operations with vector data
	*  spatial operations over geometries into the database. 
	*/
	//@{ 	

	/** @name topologic relation query
	*  topologic relation query over geometries into the database. 
	*/
	//@{
	/*!
	  \brief Returns the geometries of a geometric table (actGeomTable) that have a specific spatial relation (relate) with a subset of geometries of this table (actGeomTable)   
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the geometric table
	  \param actIdsIn		identifiers of a geometry subset of the actGeomTable table 
	  \param portal			a pointer to a database portal that will contain the resulted geometries
	  \param relate			spatial relation
	  \param actCollTable	collection table name
	 */
	virtual bool spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
		TeDatabasePortal *portal, int relate, const string& actCollTable="");
	
	/*!
	  \brief Returns the geometries of a geometric table (visGeomTable) that have a specific spatial relation (relate) with a subset of geometries of other geometric table (actGeomTable)   
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the actGeomTable table
	  \param actIdsIn		identifiers of a geometry subset of the actGeomTable table 
	  \param visGeomTable	geometric table name
	  \param visRep			geometric representation of the visGeomTable table
	  \param portal			a pointer to a database portal that will contain the resulted geometries
	  \param relate		 	spatial relation
	  \param visCollTable	collection table name associated with the visGeomTable table
	 */
	virtual bool spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
		const string& visGeomTable, TeGeomRep visRep, TeDatabasePortal *portal, 
		int relate, const string& visCollTable=""); 

	/*!
	  \brief Returns the geometries of a geometric table (actGeomTable) that have a specific spatial relation (relate) with a geometry in memory (geom)
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the actGeomTable table
	  \param geom			a pointer to a geometry in memory
	  \param portal			a pointer to a database portal that will contain the resulted geometries
	  \param relate			spatial relation
	  \param actCollTable	collection table name associated with the actGeomTable table
	 */
	virtual bool spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, 
		TeDatabasePortal *portal, int relate, const string& actCollTable=""); 
		
	/*!
	  \brief Returns the geometries of a geometric table (actGeomTable) that have a specific spatial relation (relate) with a subset of geometries of this table (actGeomTable)   
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the geometric table
	  \param actIdsIn		identifiers of a geometry subset of the actGeomTable table 
	  \param actIdsOut		structure that will contain the identifiers of the resulted geometries
	  \param relate			spatial relation
	  \param actCollTable	collection table name
	 */
	virtual bool spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
		TeKeys& actIdsOut, int relate, const string& actCollTable="");

	/*!
	  \brief Returns the geometries of a geometric table (visGeomTable) that have a specific spatial relation (relate) with a subset of geometries of other geometric table (actGeomTable)   
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the actGeomTable table
	  \param actIdsIn		identifiers of a geometry subset of the actGeomTable table 
	  \param visGeomTable	geometric table name
	  \param visRep			geometric representation of the visGeomTable table
	  \param visIdsOut		structure that will contain the identifiers of the resulted geometries
	  \param relate			spatial relation
	  \param visCollTable	collection table name associated with the visGeomTable table
	 */
	virtual bool spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, 
		const string& visGeomTable, TeGeomRep visRep, TeKeys& visIdsOut, int relate, 
		const string& visCollTable="", TeDatabase* = 0); 

	/*!
	  \brief Returns the geometries of a geometric table (actGeomTable) that have a specific spatial relation (relate) with a geometry in memory (geom)
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the actGeomTable table
	  \param geom			a pointer to a geometry in memory
	  \param actIdsOut		structure that will contain the identifiers of the resulted geometries
	  \param relate			spatial relation
	  \param actCollTable	collection table name associated with the actGeomTable table
	*/
	virtual bool spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, 
		TeKeys& actIdsOut, int relate, const string& actCollTable=""); 
	//@}
	
	/** @name metric functions
	*  metric functions over geometries into the database. 
	*/
	//@{
	/*!
		\brief Calculates the area of a geometry set of the geometric table (actGeomTable) 
		\param actGeomTable	geometric table name
		\param actRep		geometric representation of the geometric table
		\param actIdsIn		identifiers of the geometry set of the actGeomTable table 
		\param area			the returned area value 
	 */
	virtual bool calculateArea(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, double &area);

	/*!
		\brief Calculates the length of a geometry set of the actGeomTable table 
		\param actGeomTable	geometric table name
		\param actRep		geometric representation of the geometric table
		\param actIdsIn		identifiers of the geometry set of the actGeomTable table 
		\param length		the returned length value 
	 */
	virtual bool calculateLength(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, double &length);
	
	/*!
		\brief Calculates the distance between two geometries of the actGeomTable table 
		\param actGeomTable	geometric table name
		\param actRep		geometric representation of the geometric table
		\param Ids			identifiers of the two geometries of the actGeomTable table 
		\param distance		the returned distance value 
	 */
	virtual bool calculateDistance(const string& actGeomTable, TeGeomRep actRep, TeKeys& Ids, double& distance);

	/*!
		\brief Calculates the distance between a geometry of a geometric table (actGeomTable) and a geometry of other geometric table (visGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of the geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visRep			geometric representation of the visGeomTable table
		\param objId2			identifier of the geometry of the visGeomTable table
		\param distance			the returned distance value 
	 */
	virtual bool calculateDistance(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& visGeomTable, TeGeomRep visRep, const string& objId2, double& distance);

	/*!
	  \brief Returns the geometries of a geometric table (actGeomTable) that are within a specific distance from a point in memory 
	  \param actGeomTable	geometric table name
	  \param actRep			geometric representation of the actGeomTable table
	  \param point			a point in memory
	  \param IdsDistOut		structure that will contain the identifiers of the resulted geometries and their distances
	  \param max_distance	maximum distance 
	  \param actCollTable	collection table name associated with the actGeomTable table
	*/
	virtual bool withinDistance(const string& actGeomTable, TeGeomRep actRep, const TeCoord2D& point, 
		TeKeysToDist& IdsDistOut, const double& max_distance, const string& actCollTable="");
	//@}


	/** @name functions that generate new geometries 
	*  functions that generate new geometries over geometries into the database. 
	*/
	//@{
	/*!
		\brief Returns the buffers with a specific distance of a geometry set of a geometric table (actGeomTable)
		\param actGeomTable	geometric table name
		\param actRep		geometric representation of the geometric table
		\param actIds		identifiers of the geometry set of the actGeomTable table 
		\param bufferSet	the returned buffers 
		\param dist			the distance of the buffers
	 */
	virtual bool buffer(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TePolygonSet& bufferSet, double dist);

	/*!
		\brief Returns the centroids of a geometry set of a geometric table (actGeomTable)
		\param actGeomTable	geometric table name
		\param actRep		geometric representation of the geometric table
		\param centroidSet	the returned centroids 
		\param actIds		identifiers of the geometry set of the actGeomTable table 
		\param actCollTable collection table name associated with the actGeomTable table 
	*/
	virtual bool centroid(const string& actGeomTable, TeGeomRep actRep, TePointSet& centroidSet, TeKeys actIds = vector<string>(), const string& actCollTable = "");

	/*!
		\brief Returns the convex geometries of a geometries set of a geometric table (actGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the geometric table
		\param actIds			identifiers of the geometry set of the actGeomTable table 
		\param convexHullSet	the returned convex geometries 
	*/
	virtual bool convexHull(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TePolygonSet& convexHullSet);
	//@}

	/*!
		\brief Returns the nearest neighbors of a specific geometry of a geometric table (actGeomTable)
		\param actGeomTable		geometric table name
		\param actCollTable		collection table name associated with the actGeomTable table
		\param actRep			geometric representation of the geometric table
		\param objId1			identifier of the geometry of the actGeomTable table
	    \param actIdsOut		structure that will contain the identifiers of the nearest neighbors 
		\param numRes			the number of nearest neighbors that will be returned
	*/
	virtual bool nearestNeighbors(const string& actGeomTable, const string& actCollTable,
		TeGeomRep actRep, const string& objId1, TeKeys& actIdsOut, int numRes=1);
	
	/*!
		\brief Returns the nearest neighbors of a geometric table (visGeomTable) of a specific geometry of other geometric table (actGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of the geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visCollTable		collection table name associated with the visGeomTable table
		\param visRep			geometric representation of the visGeomTable table
	    \param visIdsOut		structure that will contain the identifiers of the nearest neighbors 
		\param numRes			the number of nearest neighbors that will be returned
	*/
	virtual bool nearestNeighbors(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& visGeomTable, const string& visCollTable, 
		TeGeomRep visRep, TeKeys& visIdsOut, int numRes=1); 

	/*!
		\brief Returns the nearest neighbors of a specific geometry of a geometric table (actGeomTable)
		\param actGeomTable		geometric table name
		\param actCollTable		collection table name associated with the actGeomTable table
		\param actRep			geometric representation of the geometric table
		\param objId1			identifier of the geometry of the actGeomTable table
	    \param portal			a pointer to a database portal that will contain the identifiers of the nearest neighbors 
		\param numRes			the number of nearest neighbors that will be returned
	*/
	virtual bool nearestNeighbors(const string& actGeomTable, const string& actCollTable, 
		TeGeomRep actRep, const string& objId1, TeDatabasePortal* portal, int numRes=1);

	/*!
		\brief Returns the nearest neighbors of a geometric table (visGeomTable) of a specific geometry of other geometric table (actGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of the geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visCollTable		collection table name associated with the visGeomTable table
		\param visRep			geometric representation of the visGeomTable table
	    \param portal			a pointer to a database portal that will contain the identifiers of the nearest neighbors 
		\param numRes			the number of nearest neighbors that will be returned
	*/
	virtual bool nearestNeighbors(const string& actGeomTable, TeGeomRep actRep, 
		const string& objId1, const string& visGeomTable, const string& visCollTable, 
		TeGeomRep visRep, TeDatabasePortal* portal, int numRes=1); 

	/** @name set functions
	*  functions of set: union, intersection, difference and symmetrical difference. 
	*/
	//@{

	/*!
		\brief Returns the intersection between two geometries of a geometric table (actGeomTable) 
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param actIds		identifiers of the geometry set of the actGeomTable table 
		\param geomVect			the returned intersection 
	 */
	virtual bool geomIntersection(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeGeometryVect& geomVect);

	/*!
		\brief Returns the intersection between a geometry of a geometric table (actGeomTable) and a geometry of other geometric table (visGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of a geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visRep			geometric representation of the visGeomTable table
		\param objId2			identifier of other geometry of the visGeomTable table
		\param geomVect			the returned intersection 
	 */
	virtual bool geomIntersection(const string& actGeomTable, TeGeomRep actRep, const string& objId1,
		const string& visGeomTable, TeGeomRep visRep, const string& objId2, TeGeometryVect& geomVect);

	/*!
		\brief Returns the difference between two geometries of a geometric table (actGeomTable) 
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of a geometry of the actGeomTable table
		\param objId2			identifier of other geometry of the actGeomTable table
		\param geomVect			the returned difference 
	 */
	virtual bool geomDifference(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& objId2, TeGeometryVect& geomVect);

	/*!
		\brief Returns the difference between a geometry of a geometric table (actGeomTable) and a geometry of other geometric table (visGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of a geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visRep			geometric representation of the visGeomTable table
		\param objId2			identifier of other geometry of the visGeomTable table
		\param geomVect			the returned difference 
	 */
	virtual bool geomDifference(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& visGeomTable, TeGeomRep visRep, const string& objId2, TeGeometryVect& geomVect);

	/*!
		\brief Returns the union between geometries of a geometric table (actGeomTable) 
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param actIds			identifiers of the geometry set of the actGeomTable table 
		\param geomVect			the returned union 
	 */
	virtual bool geomUnion(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeGeometryVect& geomVect);

	/*!
		\brief Returns the union between a geometry of a geometric table (actGeomTable) and a geometry of other geometric table (visGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of a geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visRep			geometric representation of the visGeomTable table
		\param objId2			identifier of other geometry of the visGeomTable table
		\param geomVect			the returned union 
	 */
	virtual bool geomUnion(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& visGeomTable, TeGeomRep visRep, const string& objId2, TeGeometryVect& geomVect);

	/*!
		\brief Returns the symmetrical difference between two geometries of a geometric table (actGeomTable) 
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of a geometry of the actGeomTable table
		\param objId2			identifier of other geometry of the actGeomTable table
		\param geomVect			the returned symmetrical difference 
	 */
	virtual bool geomXOr(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& objId2, TeGeometryVect& geomVect);

	/*!
		\brief Returns the symmetrical difference between a geometry of a geometric table (actGeomTable) and a geometry of other geometric table (visGeomTable)
		\param actGeomTable		geometric table name
		\param actRep			geometric representation of the actGeomTable table
		\param objId1			identifier of a geometry of the actGeomTable table
		\param visGeomTable		geometric table name
		\param visRep			geometric representation of the visGeomTable table
		\param objId2			identifier of other geometry of the visGeomTable table
		\param geomVect			the returned symmetrical difference 
	 */
	virtual bool geomXOr(const string& actGeomTable, TeGeomRep actRep, const string& objId1, 
		const string& visGeomTable, TeGeomRep visRep, const string& objId2, TeGeometryVect& geomVect);	
	//@}

	/** @name Spatial Operations with raster data
	*  spatial operations over raster data into the database. 
	*/
	//@{ 	
	/*!
		\brief Returns the statistics of raster data regions inside a geometry set of a geometric table (actGeomTable)
		\param rasterTable		raster table name
		\param actGeomTable		geometric table name that contains polygons
		\param Ids				identifiers of the geometries of the actGeomTable table that define the regions in the raster data  
		\param result			structure that will contain the resulted statistics 
	*/
	virtual bool zonal(const string& rasterTable, const string& actGeomTable, TeKeys& Ids, TeObjectStatistics& result);
	
	/*!
		\brief Returns the statistics of raster data regions inside a geometry set of a geometric table (actGeomTable)
		\param rasterTable		raster table name
		\param actGeomTable		geometric table name that contains polygons
		\param actCollTable		collection table that contains the identifiers of the geometries of the actGeomTable table that define the regions in the raster data  
		\param result			structure that will contain the resulted statistics 
	*/
	virtual bool zonal(const string& rasterTable, const string& actGeomTable, const string& actCollTable, TeObjectStatistics& result);
	
	/*!
		\brief Returns the statistics of a raster data region inside a geometry in memory 
		\param rasterTable		raster table name
		\param poly				the geometry in memory that define a region in the raster data 
		\param result			structure that will contain the resulted statistics 
	*/
	virtual bool zonal(const string& rasterTable, TePolygon& poly, TeStatisticsDimensionVect& result);

	/*!
		\brief Clips a raster data from a geometry of a geometric table (actGeomTable)
		\param rasterTable		raster table name
		\param actGeomTable		geometric table name that contains polygons
		\param objId			identifier of the geometry of the actGeomTable table 
		\param nameLayerOut		the layer name that will contain the result 
		\param st				the strategy used in the clipping of the raster data
	*/
	virtual bool mask(const string& rasterTable, const string& actGeomTable, const string& objId, const string& nameLayerOut, TeStrategicIterator st);
	
	/*!
		\brief Clips a raster data from a geometry in memory (poly)
		\param rasterTable		raster table name
		\param poly				a geometry in memory 
		\param nameLayerOut		the layer name that will contain the result 
		\param st				the strategy used in the clipping of the raster data
	*/
	virtual bool mask(const string& rasterTable, TePolygon& poly, const string& nameLayerOut, TeStrategicIterator st);
	//@}
	
	/** @name specifics SQLs
	*  return SQL strings 
	*/
	//@{ 	
	//! Return a string that describes a where clause in SQL to return the geometries inside the box
	virtual string getSQLBoxWhere (TeBox &box, TeGeomRep rep);

	//! Return a string that describes a where clause in SQL to return the geometries of the table2 that are 
	//! inside the geometries box of the table1 (table1 must have lower_x, lower_y...) 
	virtual string getSQLBoxWhere (const string& table1, const string& table2, TeGeomRep rep2, TeGeomRep rep1 = TePOLYGONS);

	//! Return a string SQL to be used in the clause SELECT to select the box (lower_x, lower_y, upper_x, upper_y)
	virtual string getSQLBoxSelect (const string& tableName, TeGeomRep rep);

	//! Return a string SQL to calculate the statistics of some attributes
	virtual string getSQLStatistics (TeGroupingAttr& attrs);
	
	//! Return the database function in SQL to generate autonumber values
	virtual string getSQLAutoNumber(const string& table);

	//! Return a string SQL to temporal where
	virtual string getSQLTemporalWhere (TeTimeInterval& /* timeInterval */, TeTemporalRelation /* timeOperator */, const string& /* initialTime */, const string& /* finalTime */ );

	//! Return a string SQL to temporal where 
	virtual string getSQLTemporalWhere (const string& temporalRest);
		
	//! Return a string SQL to temporal where
	virtual string getSQLTemporalWhere(int /* time1 */, int /* time2 */, TeChronon /* chr */, TeTemporalRelation /* rel */, const string& /* initialTime */, const string& /* finalTime */);

	//! Returns a valid SQL time string
	virtual string getSQLTime(TeTime& /* time */) { return ""; };

	//! Returns a SQL temporal expression applied to a column
	virtual string getSQLTemporalFunction (TeChronon chr, const string& colName);
	//@}

	//! Return the box of a specific geometry (object_id)  
	virtual bool getMBRGeom(string tableGeom, string object_id, TeBox& box, string colGeom);

	//! Return the box of a select objects set 
	virtual bool getMBRSelectedObjects(string geomTable,string colGeom, string fromClause, 
		string whereClause, string afterWhereClause, TeGeomRep repType,TeBox &bout, const double& tol = 0.0);

	//! Gets the list of attributes of a table
	virtual bool getAttributeList(const string& tableName,TeAttributeList& attList);

	//! Escape special characters in a string to be used in a SQL statement
	virtual string  escapeSequence(const string& from) = 0;

	//! Returns theme box
	virtual TeBox getThemeBox(TeTheme* theme);

	//! Returns the container of legend title alias
	map<int, map<string, string> >& mapThemeAlias() {return metaModel_->mapThemeAlias();}

	//! Concat values in a vector using unionString as the join between each value
	virtual string concatValues(vector<string>& values, const string& unionString) = 0;

	//! Returns the SQL function for upper case
	virtual string toUpper(const string& value) = 0;

	//! Returns the SQL function for substring that starts from left to right with informed length.
	virtual string leftString(const string& /*name*/, const int& /*length*/){return "";}

	//! Write the given version as the terralib version in the database
	virtual	bool updateVersionStamp(const string& version ); 

	//! Read and returns the terralib version from the database
	virtual	bool loadVersionStamp( string& version ); 

   /** @name Transaction control methods 
	\Note should be implemented by the drivers that has it. Default implementation
	is DO NOTHING.
   */
   //@{ 
   //! Begins a transaction
   virtual bool beginTransaction(); 

   //! Commits a transaction
   virtual bool commitTransaction();

   //! Rollbacks a transaction
   virtual bool rollbackTransaction();
   //@}

   //! Drops a database view
   virtual bool dropDBView(const string& dbViewName);

protected :

	bool				isConnected_;			//!< indicates if the connection is open
	string				host_;					//!< host name of the database server
	string				user_;					//!< user name 
	string				password_;				//!< user password
	string				database_;				//!< database name
	int					portNumber_;			//!< port number
	int					errorNumber_;			//!< error number
	string				errorMessage_;			//!< error message
	string				dbmsName_;				//!< DBMS name (Ado, MySQL, Postgres, OracleSpatial)
	TeSharedPtr<TeMetaModelCache> metaModel_;	//!< Meta model: Layers, Themes, Views...

	//! Update metadata about an attribute table that had its name or columns changed
	void alterTableInfoInMemory(const string& updatedTableName, string oldTableName="");

	int               transactionCounter_;//!< counts how many nested transactions have been opened

private:

	TeDatabase(const TeDatabase& other);
};

//! An abstract access portal to a database
/*! A portal has a concept of a record set, that is generated by a selection (query) on
	the data accessible through a connection to a database server.
  \sa TeDatabase, TeGeometry, TeTable, TeTheme, TeView, TeLayer

  */
class TL_DLL TeDatabasePortal {
protected:
	

	TeDatabase*		db_;			//!< the database associated to this portal
	TeAttributeList	attList_;		//!< the list of attributes associated to this portal
	int				numRows_;		//!< the number of rows in this portal
	int				numFields_;		//!< the number of fields in this portal
	string          errorMessage_;	//!< error message
	int				errorNumber_;	//!< error number

public :

	//!Constructor 
	TeDatabasePortal ();

	//! Destructor
	virtual ~TeDatabasePortal ();
	
	//! Returns the database associated to this portal 
	TeDatabase* getDatabase()
	{ return db_; }

	//! Executes a SQL query that opens a record set
	virtual bool query ( const string &qry, TeCursorLocation l = TeSERVERSIDE, TeCursorType t = TeUNIDIRECTIONAL, TeCursorEditType e = TeREADONLY, TeCursorDataType dt = TeTEXTCURSOR ) = 0;

	//! Fetchs the next row in a record set that shouldve been previously opened
	virtual bool fetchRow () = 0;

	//! Fetchs a particular row
	virtual bool fetchRow (int i) = 0;

	//! Frees the current record set
	virtual void freeResult () = 0;

	//! Gets the last error message
	virtual string	errorMessage ()
	{ return errorMessage_; }

	//! Gets the number of the last error message
	virtual int errorNum ()
	{ return errorNumber_; }
	
//	virtual bool loadNetwork (TeLayer *layer) = 0;

// specific SQL SELECT command methods

	/*! \brief Retrieves the number of rows in a portal. Some drivers
	           (like PostgreSQL and PostGIS) brings to the client only
			   a part of the row, and then this number can be less than
			   the number of rows returned by query.
    */
	int numRows () 
	{ return numRows_;}

	//! Retrieves the number of fields in this portal
	int numFields () 
	{ return numFields_; }

	//! Retrieves attribute list in this portal
	TeAttributeList& getAttributeList()	
	{ return attList_; }

	//! Retrieves the i-th attribute in this portal
	TeAttribute getAttribute (int i);

	//! Retrieves an attribute by name
	TeAttribute getAttribute (const string& s);

	//! Gets the value of the i-th attribute as a literal
	virtual char* getData (int i) = 0;

	//! Gets the value of a named attribute as a literal
	virtual char* getData (const string& s) = 0;

	//! Gets the value of the i-th attribute as a double
	virtual double getDouble (int i);

	//! Gets the value of a named attribute as a double
	virtual double getDouble (const string& s);

	//! Gets the value of the i-th attribute as an integer
	virtual int getInt (int i);

	//! Gets the value of a named attribute as an integer
	virtual int getInt (const string& s);

	//! Gets the value of a named attribute as a boolean
	virtual bool getBool (const string& s) = 0;

	//! Gets the value of the i-th attribute as a boolean
	virtual bool getBool (int i) = 0;

	//! Gets the value of the i-th attribute as a date 
	virtual TeTime getDate (int i) = 0;

	//! Gets the value of a named attribute as a date 
	virtual TeTime getDate (const string& s) = 0;

	//! Gets the of a date/time attribute as a string formatted as accepted in further SQL statements
	virtual string getDateAsString(int i) = 0;

	//! Gets the of a date/time attribute as a string formatted as accepted in further SQL statements
	virtual string getDateAsString(const string& s) = 0;

	//! Gets the value of a named BLOB attribute 
	virtual bool getBlob(const string& s, unsigned char* &data, long& size) = 0;

	//! Gets the index of a named attribute
	int getColumnIndex (const string& s);

	//! Gets the name of the i-th attribute
	string getColumnName (int i);

	/** @name Data Model
	  The following methods decodify structures as stored 
	  in the database according to the data model proposed in TerraLib. 
	*/
	//@{ 	
	virtual TeViewTree*		getViewTree ();
	virtual TeLegendEntry	getLegend();
	virtual void			getVisual(TeVisual*);
	virtual bool			getVisual(TeVisual* vis, TeGeomRep& rep, const unsigned int& initIndex);
	virtual bool			getRasterVisual(TeRasterVisual& vis, const unsigned int& initIndex=0);
	virtual TeColor			getColor(); 
	virtual bool	getRasterBlock(unsigned long& size, unsigned char* ptData)=0;
	virtual bool	getView(TeView& view, const unsigned int& initIndex=0);
	virtual bool	getProjection(TeProjection** proj, const unsigned int& initIndex=0);
	virtual void	getViewNodeParams (TeViewNodeParams& params, const unsigned int& initIndex=0);
	virtual bool	getTheme(TeAbstractTheme& theme, const unsigned int& initIndex=0);
	virtual bool	getGrouping(TeGrouping& group, const unsigned int& initIndex=0);
	virtual bool	getLegend(TeLegendEntry& leg, const unsigned int& initIndex=0);
	virtual bool	getAttrTable(TeTable& table, const unsigned int& initIndex=0);
	virtual bool	getLayer(TeLayer& layer, const unsigned int& initIndex=0);
	virtual bool	getRepresentation(TeRepresentation& rep, const unsigned int& initIndex=0);

	//@}

	/** @name Fetch Geometry
	  The following methods decodify geometries as stored 
	  in the database according to the data model proposed in TerraLib.
	  \param geom		the geometry that will be filled from portal
	  \param initIndex  the position index in the portal where begins the geometry information 
	  \return The fetchGeometry methods advance the portal to the next record and 
	  they return TRUE if there are more records to be read and FALSE otherwise.
	*/
	//@{ 	
	virtual	bool fetchGeometry (TePolygon& geom) = 0;
	virtual	bool fetchGeometry (TePolygon& geom, const unsigned int& initIndex); 
	virtual	bool fetchGeometry (TeLine2D& geom) = 0;
	virtual	bool fetchGeometry (TeLine2D& geom, const unsigned int& initIndex); 
	virtual	bool fetchGeometry (TeNode& geom) = 0;
	virtual	bool fetchGeometry (TeNode& geom, const unsigned int& initIndex);
	virtual	bool fetchGeometry (TePoint& geom) = 0;
	virtual	bool fetchGeometry (TePoint& geom, const unsigned int& initIndex);
	virtual	bool fetchGeometry (TeCell& geom);
	virtual	bool fetchGeometry (TeCell& geom, const unsigned int& initIndex);
	virtual	bool fetchGeometry (TeArc& geom);
	virtual	bool fetchGeometry (TeArc& geom, const unsigned int& initIndex);
	virtual	bool fetchGeometry (TeText& geom);
	virtual	bool fetchGeometry (TeText& geom, const unsigned int& initIndex);
	//@}
};


/*! \example createDatabase.cpp
	Shows how to create a TerraLib database.
 */

/*! \example databaseQuery.cpp
	Shows how to retrieve an existing layer from a TerraLib database, 
	and execute some queries on its polygon geometry table.
 */

/*! \example databaseSQLQuery.cpp
	Shows how retrieve geometries (polygons) using an SQL query.
 */
 
/*! \example spatialQuery.cpp
	Shows how  to use the database interface to do some	spatial queries 
	involving objects with points, lines and polygon geometries.
 */

/*! \example spatialQueryAndBuffer.cpp
	Shows to use the database interface to do some  spatial queries involving 
	objects with points, lines and polygon geometries, and to generate a buffer operation.
 */
#endif
	
