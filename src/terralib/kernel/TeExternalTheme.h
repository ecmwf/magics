/************************************************************************************
 TerraLib - a library for developing GIS applications.
Copyright © 2001-2006 INPE and Tecgraf/PUC-Rio.

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
/** \file TeExternalTheme.h
    \brief This file contains direct implementation of TeTheme in order to 
	support themes from another TerraLib databases (remote databases).
*/

#ifndef  __TERRALIB_INTERNAL_EXTERNALTHEME_H
#define  __TERRALIB_INTERNAL_EXTERNALTHEME_H

#include <map>
#include <string>

#include "TeTheme.h"
#include "TeDBConnection.h"

/** \fn bool TeLoadConnectionsInfo(TeDatabase* sourceDB)
    \brief Loads all connections information from source database and populates Remote theme database index.
	\param sourceDB A connection to a TerraLib database that stores information about others databases that can be used by remote themes. (Input)
 */
TL_DLL bool TeLoadConnectionsInfo(TeDatabase* sourceDB);


//! A class that implements the external theme concept  
/*!
	A external theme is a theme stored in a remote TerraLib database.
	Some information about the external theme is stored in the local database, but
	its geometries and attributes are stored in the remote database. 

	\sa TeTheme
  
 */
class TL_DLL TeExternalTheme : public TeTheme
{
	public:

		//! Constructor.
		TeExternalTheme(TeDatabase* sourceDB, const string& name = "", TeViewNode* parent = 0, const int& view = 0, const int& id = 0);

		//! Constructor
		TeExternalTheme(const TeViewNodeParams& params); 

		//! Destructor.
		virtual ~TeExternalTheme();
	
		//! Copy constructor
		TeExternalTheme(const TeExternalTheme& rhs);

		//! Operator =
		TeExternalTheme& operator=(TeExternalTheme& rhs);


		/** @name Remote Theme Accessor Methods
		  * Methods related to the remote theme reference.
		  */
		//@{
		virtual void setSourceDatabase(TeDatabase* db); 

		virtual TeDatabase* getSourceDatabase(); 

		virtual TeTheme* getRemoteTheme() const;
		
		virtual void setRemoteTheme(TeTheme* theme); 

		virtual string getRemoteThemeName(); 
		//@}


		/** @name Object status containers
		  * Methods to handle object status containers
		  */
		//@{
		virtual void loadObjectLegendMap();
		
		virtual int getGridStatus(const int uniqueId, const std::string objectId);

		virtual int getObjectStatus(const std::string objectId);

		virtual bool setObjectLegendStatusMap(const std::string objId, const int status);
		
		virtual bool setObjectGridStatusMap(const std::string objId, const int uniqueId, const int status);
		//@}

		/** @name Layer accessor methods
		  * Methods related to the layer that gives origin to this theme.
		  */
		//@{

		/** \brief Returns the identification of the source layer.
		    \note In a Remote Theme this will be always -1.
		  */
		virtual int layerId();

		/** \brief Sets the identification of the source layer.
		    \param i Layer identification value that will be used to set the source layer. (Input)
			\note In a Remote Theme this method does nothing.
		  */
		virtual void layerId(int i);

		/** \brief Sets the layer that is the source of objects of the theme.
		    \param layer A pointer to a TeLayer. (Input)
          */
		virtual void layer(TeLayer* layer);

		/** \brief Returns a pointer to the layer from which the theme get its objects.
		    \note In a Remote Theme this method returns a NULL pointer.
		  */
		virtual TeLayer* layer();

		//! Returns a pointer to the projection of the remote layer
		virtual TeProjection* getThemeProjection();

		//@}

		/** @name 
		  * Methods related to the restrictions over the theme used to generate this theme
          */
		//@{

		/** \brief Verifies if this theme has any kind of restrictions.
		    \note In a Remote Theme this method always returns FALSE.
		  */
		virtual bool hasRestriction();

		/** \brief Returns if there is an attribute restriction.
		    \note In a Remote Theme this method always returns FALSE.
		  */
		virtual bool hasAttrRest();

		/** \brief Returns TRUE if there is a temporal restriction defined in the theme.
		    \note In a Remote Theme this method always returns FALSE. 
		  */
		virtual bool hasTemporalRest();

		/** \brief Returns TRUE if this theme has a spatial restriction.
		    \note In a Remote Theme this method always returns FALSE.
		  */
		virtual bool hasSpatialRest();

		/** \brief Returns the clause WHERE derived from the combination of all restricitions (spatial, attribute and temporal).
		    \note In a Remote Theme this method always returns an empty string.
		  */
		virtual string sqlWhereRestrictions(TeRepresentation* rep = 0);

		//@}

		/** @name Collection
		  * Methods related to the materialization in the database of the theme as a collection of objects 
          */
		//@{

		/** \brief Fills the sqlGridJoin_ and sqlGridFrom_ statements according to the status of the database.
		    \note In a Remote Theme this method does nothing.
		  */
		virtual void loadTablesJoin(const string& geomTable = "");

		/** \brief Returns a SQL JOIN statement to get all the attributes of the theme objects, 
		           the attributes of the collection table, and the attributes of the extended 
                   collection table.
		    \note In a Remote Theme this method returns an empty string.
	      */
		virtual string sqlGridJoin();

		/** \brief Returns a FROM clause of a SQL statement to get attributes of the theme objects,
		           the attributes of the collection table, and the attributes of the
				   extended collection table .
		    \note In a Remote Theme this method returns an empty string.
	      */
		virtual string sqlGridFrom(const string& geomTable="");

		//! Save the the theme parameters in the database.
		virtual bool save(TeDatabase* db); 
		
		//! Save the the theme parameters in the database.
		virtual bool save();

		/** \brief Create the auxiliar collection table used to represent objects with multiple versions in time.
		    \note In a Remote Theme this method creates a local auxiliary collection table.
		  */
		virtual bool createCollectionAuxTable();

		/** \brief Populate the auxiliar collection table used to represent objects with multiple versions in time.
		    \note In a Remote Theme this method populates the local auxiliary collection.
		  */
		virtual bool populateCollectionAux(std::string objectId = "");

		//@}

		//! Save the grouping parameters in memory when there is no chronon.
		virtual bool buildGrouping(const TeGrouping& g, TeSelectedObjects selectedObjects = TeAll,
			                       vector<double>* dValuesVec = 0);

		//! Save the grouping parameters in memory when there is chronon.
		virtual bool buildGrouping(const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec);
						   
		//! Build the grouping and associate each object to its group in the collection table.
		virtual bool saveGrouping(TeSelectedObjects selectedObjects = TeAll);

		//! Save the theme grouping legends in the collection table.
		virtual bool saveLegendInCollection(TeSelectedObjects selectedObjects = TeAll, std::string objectId = "");

		//! Save the theme grouping legends in the collection table  
		virtual bool saveLegendInCollection(TeDatabase* db, TeSelectedObjects selectedObjects = TeAll, std::string objectId = "");

		///! Set the legend id for each object of the theme.
		virtual void setLegendsForObjects();

		//! Generates a optimized position (x,y) in the spatial extention of each object to position label or graphs.
		virtual bool generateLabelPositions(const std::string& objectId = "");  

		//! Delete grouping.
		virtual bool deleteGrouping(); 
		
		//! Creates an appropriate visual presentation to the raster of the theme.
		virtual void createRasterVisual(TeRaster* rst = 0);

		/** @name Attribute Tables
		  * A theme can use one or more attribute tables of the layer that gives its data.
		  * These methods are related to the manipulation of these tables.
          */
		//@{	

		//! Add a new attribute table to a theme.
		virtual bool addThemeTable(TeTable& table);

		//! Add a new attribute table to a theme.
		virtual void addThemeTable(string tableName);

		//! Verify if an attribute table is part of a theme.
		virtual bool isThemeTable(int tableId);

		//! Verify if an attribute table is part of a theme.
		virtual bool isThemeTable(string tableName);

		//! Returns the list of attribute tables used by this theme.
		virtual TeAttrTableVector& attrTables();

		//! Sets the entire list of attribute tables used by this theme.
		virtual bool setAttTables(TeAttrTableVector& attrs);  

		//! Returns a vector of attribute tables, of a specific type, used by this theme.
		virtual bool getAttTables(TeAttrTableVector& attrs, TeAttrTableType attType = TeAllAttrTypes); 

		//! Returns a representation of an attribute table  given name.
		virtual bool getTable(TeTable& table, const string tableName);

		//! Clears the list of attribute tables used by this theme.
		virtual void clearAttTableVector();

		/** \brief Returns the temporal attribute table of the theme (TeEvent or TeFixedGeomDynAttr).
		    \note A theme supports only one temporal attribute table.
		  */
		virtual bool getTemporalTable(TeTable& table);

		//! Removes an attribute table from the list of tables of a theme.
		virtual bool removeThemeTable(unsigned int index);

		//! Returns the the name of an attribute table that contains a given attribute.
		virtual string getTableName(const string& attrName);

		//! Returns the name of the index-th attribute resulting of the join of all attribute tables associated to the theme tables 
		virtual string getAttribute(unsigned int index);

		//! Loads the theme tables in the database
		virtual bool loadThemeTables();
		
		//! Returns the list of attributes of theme tables.
		virtual  TeAttributeList sqlAttList();
		
		//! Clears the list of attributes associated to the theme tables.
		virtual void clearAttList();

		//! Returns the list of numerical attributes of the theme tables.
		virtual TeAttributeList sqlNumAttList();

		//! Clears the list of numerical attributes associated to the theme tables.
		virtual void clearNumAttList();

		///! Returns a SQL JOIN statement to reach to all attribute tables used by this theme.
		virtual string sqlJoin();

		//! Returns a SQL FROM CLAUSE that gives access to all attribute tables used by this theme.
		virtual string sqlFrom();

		//! Returns the alias vector of the names of the theme tables.
		virtual vector<string>&	aliasVector();

		//! Fills aliasVector_.
		virtual void loadAliasVector();

		/** \brief Refresh list of attributes of all the theme tables.
		    \note All attributes are stored into sqlAttList_ and numeric attributes are stored into sqlNumAttList_.
          */
		virtual void loadAttrLists();
		//@}

		/** @name Locate geometries
		  * Returns the geometry(ies) of the theme given coordinate
          */
		//@{ 	

		virtual bool locatePolygon(TeCoord2D &pt, TePolygon &polygon, const double& tol = 0.0);
		virtual bool locatePolygonSet(TeCoord2D &pt, double tol, TePolygonSet &polygons);
		virtual bool locateLine(TeCoord2D &pt, TeLine2D &line, const double& tol = 0.0);
		virtual bool locatePoint(TeCoord2D &pt, TePoint &point, const double& tol = 0.0);
		virtual bool locateCell(TeCoord2D &pt, TeCell &c, const double& tol = 0.0);

		//@}

		/** \brief Verifies if there are objects without geometries of a specific geometry representation.
		  */
		virtual bool hasObjectsWithoutGeometries(TeGeomRep geomRep);

		/** \brief Removes the objects without geometries of a specific geometry representation.
		  */
		virtual bool removeObjectsWithoutGeometries(TeGeomRep geomRep);

		/** \brief Creates a table to store information on how to connect to others databases and get remote theme.
		    \param sourceDB A connection to a TerraLib database that stores information about others databases that can be used by remote themes. (Input)
			\return Returns 1 if the table was created, -1 if it already exists and 0 on error.
		  */
		static int createExternalThemeTable(TeDatabase* sourceDB);

		//! Save the theme metadata in database. 
		virtual bool saveMetadata(TeDatabase* db); 

		//! Get information about the remote database connection 
		TeDBConnection& getRemoteDBConnection() { return remoteDBConn_; }

		//! Set information about the remote database connection 
		void setRemoteDBConnection(TeDBConnection& dbConn) {  remoteDBConn_ = dbConn; }
		
	protected:

		/** \brief Fill the sqlJoin_ and sqlFrom_ .
		  */
		//virtual void loadThemeTablesJoin();
		
		/** \brief Populate the collection table based in the theme restrictions.
		  */
		virtual bool populateCollection(std::string objectId = "");

		/** \brief Populate map with relationateds ids from local theme and remote theme.
		  */
		virtual void createLegendMapId(std::map<int, int>& mapIdLegend);

		/** \brief Copy the respective collection table from remoteTheme to localTheme.
		  */
		virtual bool copyRemoteCollection(std::map<int, int>& mapIdLegend);

				
		/** \brief Retrieves remote theme information from the database.
		  */
		bool getRemoteThemeInfo(int& remoteThemeId, int& databaseId);

		//! Load the external theme metadata from database. 
		virtual bool loadMetadata(TeDatabase* db);

		//! Erase the theme metadata in database. 
		virtual bool eraseMetadata(TeDatabase* db); 

	protected:
        //! A pointer to a theme in a remote database.
		TeTheme* remoteTheme_;	

		//! Information about the remote database connection 
		TeDBConnection remoteDBConn_;

		//! A pointer to the source database (where the metadata is stored)
		TeDatabase* sourceDB_;	
};


//!  This class implements a factory to create external theme objects. 
class TL_DLL TeExternalThemeFactory : public TeViewNodeFactory
{
public:
	//! Constructor 
	TeExternalThemeFactory() : TeViewNodeFactory((int)TeEXTERNALTHEME)
	{}

	//! Created theme objects 
	TeViewNode* build(TeViewNodeParams* params)
	{	
		TeViewNodeParams auxParams = *params;
		return new TeExternalTheme(auxParams);	
	}
	
	//! Created theme objects 
	TeViewNode* build()
	{	
		return new TeExternalTheme(0);	
	}
};

namespace 
{
  static TeExternalThemeFactory externalThemeFactory;
}; 

#endif	// __TERRALIB_INTERNAL_REMOTETHEME_H

