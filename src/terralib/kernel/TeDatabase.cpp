/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include "TeDatabase.h"
#include "TeDecoderDatabase.h"
#include "TeGeometryAlgorithms.h"
#include "TeSpatialOperations.h"
#include "TeImportRaster.h"
#include "TeLayer.h"
#include "TeAbstractTheme.h"
#include "TeExternalTheme.h"
#include "TeDatabaseFactoryParams.h"
#include <TeRasterTransform.h>
#include <TeTimeInterval.h>

#include <sys/stat.h>
#include <stdio.h>
#include <sstream>

typedef map<int,TeNode> TeNodeMap;

static const int scales [] =
{	150000,
	80000,
	40000,
	20000,
	10000,
	3000,
	1500,
	800,
	400,
	200,
	100,
	30,
	15,
	8,
	4,
	2,
	1
};

//-*********************

TeDatabase::TeDatabase() :
	isConnected_ (false),
	host_(""),
	user_(""),
	password_(""),
	database_(""),
	portNumber_(-1),
	errorNumber_(0),
	errorMessage_(""),
	transactionCounter_(0)
{
	metaModel_.reset(new TeMetaModelCache);
}

TeDatabase& TeDatabase::operator=(const TeDatabase& other)
{
	if(this != &other)
		metaModel_ = other.metaModel_;	

	return *this;
}

TeDatabase::~TeDatabase()
{
}

string
TeDatabase::getDatabaseDescription()
{
    std::stringstream desc;
    const char sep = ';';

    desc << dbmsName_;
    desc << sep << host_;
    desc << sep << portNumber_;
    desc << sep << database_;
    desc << sep << user_;
    desc << sep << password_;

    return desc.str();
} 

void TeDatabase::alterTableInfoInMemory(const string& updatedTableName, string oldTableName)
{
	//update table in memory
	TeTable updatedTable(updatedTableName);
	loadTableInfo(updatedTable);
	if(oldTableName.empty())
		oldTableName = updatedTableName;

	TeLayerMap::iterator itLayer = metaModel_->layerMap().begin();
	while(itLayer!=metaModel_->layerMap().end())
	{
		TeAttrTableVector::iterator itAttr = itLayer->second->attrTables().begin();
		while(itAttr!=itLayer->second->attrTables().end())
		{
			if(TeConvertToUpperCase(itAttr->name())==TeConvertToUpperCase(oldTableName))
			{
				(*itAttr)=updatedTable;
				break;
			}
			++itAttr;
		}
		++itLayer;
	}
	TeThemeMap::iterator itTheme = metaModel_->themeMap().begin();
	while(itTheme!=metaModel_->themeMap().end())
	{
		if(itTheme->second->getProductId() != TeTHEME)
		{
			++itTheme; 
			continue;
		}

		TeTheme* theme = static_cast<TeTheme*>(itTheme->second);
				
		TeAttrTableVector::iterator itAttr = theme->attrTables().begin();
		while(itAttr!=theme->attrTables().end())
		{
			if(TeConvertToUpperCase(itAttr->name())==TeConvertToUpperCase(oldTableName))
			{
				(*itAttr)=updatedTable;
				theme->loadAliasVector();
				theme->loadAttrLists();
				theme->loadTablesJoin(); 
				break;
			}
			++itAttr;
		}
		++itTheme;
	}
	return;
}

bool TeDatabase::validTable (TeTable& table)
{
	int cont=0;
	bool change = false;
	bool changeTable = false;
	bool uniqueName, linkName; 
	
	TeAttributeList::iterator it = table.attributeList().begin();
	TeAttributeList::iterator it2;  
	while(it != table.attributeList().end())
	{
		uniqueName = false;
		linkName = false;

		if((*it).rep_.name_==table.uniqueName())
			uniqueName=true;
		
		if((*it).rep_.name_==table.linkName())
			linkName=true;

		string errorMess;
		string temp = TeCheckName((*it).rep_.name_, change, errorMess);
		
		if(change)
		{
			it2 = table.attributeList().begin();
			while(it2!=table.attributeList().end())
			{
				if(temp==(*it2).rep_.name_)
				{
					temp += Te2String(cont);
					it2 = table.attributeList().begin();
					++cont;
				}
				else
					++it2;
			}

			changeTable = true;
		}

		if(change && uniqueName)
			table.setUniqueName(temp);
		if(change && linkName)
			table.setLinkName (temp);

		(*it).rep_.name_ = temp;
		++it;
		++cont;
	}
	
	return changeTable;
}

string TeDatabase::getTableName(int tableId)
{
	string tableName;

	TeDatabasePortal* pt = getPortal();
	string q = "SELECT attr_table FROM te_layer_table";
	q += " WHERE table_id = " + Te2String(tableId);
	if (pt->query(q) == true && pt->fetchRow())
		tableName = pt->getData("attr_table");
	delete pt;
	return tableName;
}

bool
TeDatabase::deleteTable (const string& table)
{
//	int f =	table.find ("te_collection", std::string::npos);
	int f =	table.find ("te_collection");

	if( table=="te_theme" ||
		table=="te_layer" ||
		table=="te_representation" ||
		table=="te_tables_relation" ||
		table=="te_layer_table" ||
		table=="te_raster_metadata" ||
		table=="te_projection" ||
		table=="te_view" ||
		table=="te_legend" ||
		table=="te_visual" ||
		table=="te_database" ||
		f == 0)
	{
		errorMessage_ = "N�o � poss�vel deletar tabelas do modelo!";
		return false;
	}

	string del = "DROP TABLE " + table;
	if(tableExist(table))
	{
		if(!execute(del))
			return false;
	}

	return true;
}


bool 
TeDatabase::deleteColumn (const string& table, const string& colName)
{
	if(!tableExist(table))
		return false;
	TeAttribute attr;
	if (!columnExist(table,colName,attr))
		return true;
	string drop = "ALTER  TABLE "+ table +" DROP COLUMN "+ colName;
	if(execute(drop) == false)
		return false;

	string tableId;
	TeDatabasePortal* portal = getPortal();
	string sql = "SELECT table_id FROM te_layer_table WHERE attr_table = '" + table + "'";
	if(portal->query(sql) && portal->fetchRow())
		tableId = portal->getData(0);

	delete portal;
	if(tableId.empty() == false)
	{
		// delete relation
		sql = "DELETE FROM te_tables_relation WHERE (related_table_id = " + tableId;
		sql += " AND related_attr = '" + colName + "')";
		sql += " OR (external_table_name = '" + table + "'";
		sql += " AND external_attr = '" + colName + "')";
		if(execute(sql) == false)
			return false;

		// delete grouping
		TeDatabasePortal* portal = getPortal();
		sql = "SELECT theme_id FROM te_grouping WHERE grouping_attr = '" + table + "." + colName + "'";
		if(portal->query(sql) && portal->fetchRow())
		{
			string themeId = portal->getData(0);

			sql = "DELETE FROM te_legend WHERE theme_id = " + themeId + " AND group_id >= 0";
			if(execute(sql) == false)
			{
				delete portal;
				return false;
			}
		}
		delete portal;

		sql = "DELETE FROM te_grouping";
		sql += " WHERE grouping_attr = '" + table + "." + colName + "'";
		if(execute(sql) == false)
			return false;
	}
	alterTableInfoInMemory(table);
	return true;
}

bool 
TeDatabase::defineIntegrity(void)
{
	if (existRelation("te_layer","fk_layer_proj_id") == TeNoRelation )
		if (!createRelation("fk_layer_proj_id", "te_layer", "projection_id", "te_projection", "projection_id", false))
			return false;
        
	if (existRelation("te_representation","fk_rep_layer_id") == TeNoRelation )
		if (!createRelation("fk_rep_layer_id", "te_representation", "layer_id", "te_layer", "layer_id", true))
			return false;

	if (existRelation("te_view","fk_view_proj_id") == TeNoRelation )
		if (!createRelation("fk_view_proj_id", "te_view", "projection_id", "te_projection", "projection_id", false))
			return false;

	if (existRelation("te_view", "fk_view_current_theme") == TeNoRelation )
		if (!createRelation("fk_view_current_theme", "te_view", "current_theme", "te_theme", "theme_id", false))
			return false;
	
	if (existRelation("te_theme","fk_theme_layer_id") == TeNoRelation )
		if (!createRelation("fk_theme_layer_id", "te_theme", "layer_id", "te_layer", "layer_id", true))
			return false;

	if (existRelation("te_theme","fk_theme_view_id") == TeNoRelation )
		if (!createRelation("fk_theme_view_id", "te_theme", "view_id", "te_view", "view_id", true))
			return false;

	if (existRelation("te_theme_table","fk_thmtable_theme_id") == TeNoRelation )
		if (!createRelation("fk_thmtable_theme_id", "te_theme_table", "theme_id", "te_theme", "theme_id", true))
			return false;

	if (existRelation("te_theme_table","fk_thmtable_lytable_id") == TeNoRelation )
		if (!createRelation("fk_thmtable_lytable_id", "te_theme_table", "table_id", "te_layer_table", "table_id", false))
			return false;

	if (existRelation("te_theme_table","fk_thmtable_relation_id") == TeNoRelation )
		if (!createRelation("fk_thmtable_relation_id", "te_theme_table", "relation_id", "te_tables_relation", "relation_id", false))
			return false;

	if (existRelation("te_grouping","fk_group_theme_id") == TeNoRelation )
		if (!createRelation("fk_group_theme_id", "te_grouping", "theme_id", "te_theme", "theme_id", true))
			return false;

	if (existRelation("te_legend","fk_legend_theme_id") == TeNoRelation )
		if (!createRelation("fk_legend_theme_id", "te_legend", "theme_id", "te_theme", "theme_id", true))
			return false;

	if (existRelation("te_visual","fk_visual_legend_id") == TeNoRelation )
		if (!createRelation("fk_visual_legend_id", "te_visual", "legend_id", "te_legend", "legend_id", true))
			return false;
        
	if (existRelation("te_layer_table","fk_laytable_layer_id") == TeNoRelation )
		if (!createRelation ("fk_laytable_layer_id", "te_layer_table", "layer_id", "te_layer", "layer_id", true))
			return false;
        
	if (existRelation("te_tables_relation","fk_tabrelation_laytable_id") == TeNoRelation )
		if (!createRelation("fk_tabrelation_laytable_id", "te_tables_relation", "related_table_id", "te_layer_table", "table_id", true))
			return false;

	if (existRelation("te_visual_raster","fk_visrast_theme_id") == TeNoRelation )
		if (!createRelation("fk_visrast_theme_id", "te_visual_raster", "theme_id", "te_theme", "theme_id", true))
			return false;

	if (existRelation("te_project_view","fk_projectview_project_id") == TeNoRelation )
		if (!createRelation("fk_projectview_project_id", "te_project_view", "project_id", "te_project", "project_id", true))
			return false;

	if (existRelation("te_project_view","fk_projectview_view_id") == TeNoRelation )
		if (!createRelation("fk_projectview_view_id", "te_project_view", "view_id", "te_view", "view_id", true))
			return false;

	return true;
}

bool 
TeDatabase::createConceptualModel(bool withIntegrity, bool newDatabase, bool /* createIndex */)
{
	bool status = true;
	bool createMainTables = false;
	
	if (!this->tableExist("te_projection"))
	{
		status = this->createProjectionTable();
		if (!status)
			return false;	
		createMainTables = true;
	}

	if (!this->tableExist("te_layer"))
	{
		status = this->createLayerTable();
		if (!status)
			return false;
		createMainTables = true;
	}

	if (!this->tableExist("te_layer_table"))
	{
		status = this->createLayerTableTable();
		if (!status)
			return false;
		createMainTables = true;
	}

	if (!this->tableExist("te_tables_relation"))
	{
		status = this->createTablesRelationTable();
		if (!status)
			return false;
		createMainTables = true;
	}
	
	if (!this->tableExist("te_representation"))
	{
		status = this->createRepresentationTable();
		if (!status)
			return false;
		createMainTables = true;
	}

	if (!this->tableExist("te_theme"))
	{
		status = this->createThemeTable();
		if (!status)
			return false;
		createMainTables = true;
	}

	if (!this->tableExist("te_view"))
	{
		status = this->createViewTable();
		if (!status)
			return false;
		createMainTables = true;
	}

	if (!this->tableExist("te_grouping"))
	{
		status = this->createGroupingTable();
		if (!status)
			return false;
	}

	if (!this->tableExist("te_theme_table"))
	{
		status = this->createThemeTablesTable();
		if (!status)
			return false;
	}

	if (!this->tableExist("te_legend"))
	{
		status = this->createLegendTable();
		if (!status)
			return false;	
	}

	if (!this->tableExist("te_visual"))
	{
		status = this->createVisualTable();
		if (!status)
			return false;	
	}

	if (!this->tableExist("te_visual_raster"))
	{
		status = this->createVisualRasterTable();
		if (!status)
			return false;	
	}

	if (!this->tableExist("te_database"))
	{
		status = this->createDatabaseTable();
		if (!status)
			return false;
	}

	if (!this->tableExist("te_project"))
	{
		status = this->createProjectTable();
		if (!status)
			return false;
	}

	if (!this->tableExist("te_project_view"))
	{
		status = this->createProjectViewTable();
		if (!status)
			return false;
	}
	if (newDatabase || createMainTables)
	{
		string ins = "INSERT INTO te_database (db_version) VALUES ('" + TeDBVERSION + "')";
		if (!execute(ins))
			return false;
	}

	if (withIntegrity)
		status = defineIntegrity();

	return status;
}

bool
TeDatabase::createIndex(const string& tableName, const string& indexName, const string& columnsName)
{
	string sql = "CREATE INDEX " + indexName + " ON " + tableName + "(" + columnsName + ")";	
	return execute (sql);
}

bool 
TeDatabase::createDatabaseTable()
{
	TeAttributeList attList;

	{TeAttribute attDBVersion;
	attDBVersion.rep_.name_ = "db_version";
	attDBVersion.rep_.type_ = TeSTRING;
	attDBVersion.rep_.numChar_ = 50;
	attDBVersion.rep_.isPrimaryKey_ = true;
	attDBVersion.rep_.null_ = false;
	attList.push_back(attDBVersion);}

	{TeAttribute attDBCreation;
	attDBCreation.rep_.name_ = "db_creation";
	attDBCreation.rep_.type_ = TeDATETIME;
	attList.push_back(attDBCreation);}

	return createTable("te_database", attList);
}

bool 
TeDatabase::createProjectTable()
{
	TeAttributeList attList;

	{TeAttribute attProjectId;
	attProjectId.rep_.name_ = "project_id";
	attProjectId.rep_.type_ = TeUNSIGNEDINT;
	attProjectId.rep_.isAutoNumber_ = true;
	attProjectId.rep_.isPrimaryKey_ = true;
	attProjectId.rep_.null_ = false;
	attList.push_back(attProjectId);}

	{TeAttribute attName;
	attName.rep_.name_ = "name";
	attName.rep_.type_ = TeSTRING;
	attName.rep_.numChar_ = 50;
	attName.rep_.null_ = false;
	attList.push_back(attName);}

	{TeAttribute attDescription;
	attDescription.rep_.name_ = "description";
	attDescription.rep_.type_ = TeSTRING;
	attDescription.rep_.numChar_ = 255;
	attDescription.rep_.null_ = true;
	attList.push_back(attDescription);}

	{TeAttribute attCurrentView;
	attCurrentView.rep_.name_ = "current_view";
	attCurrentView.rep_.type_ = TeINT;
	attList.push_back(attCurrentView);}

	return createTable("te_project", attList);
}

bool 
TeDatabase::createProjectViewTable()
{
	TeAttributeList attList;

	{TeAttribute attProjectId;
	attProjectId.rep_.name_ = "project_id";
	attProjectId.rep_.type_ = TeUNSIGNEDINT;
	attProjectId.rep_.isPrimaryKey_ = true;
	attProjectId.rep_.null_ = false;
	attList.push_back(attProjectId);}

	{TeAttribute attViewId;
	attViewId.rep_.name_ = "view_id";
	attViewId.rep_.type_ = TeUNSIGNEDINT;
	attViewId.rep_.isPrimaryKey_ = true;
	attViewId.rep_.null_ = false;
	attList.push_back(attViewId);}

	return createTable("te_project_view", attList);
}

bool 
TeDatabase::createProjectionTable ()
{
	TeAttributeList attList;

	{TeAttribute attProjectId;
	attProjectId.rep_.name_ = "projection_id";
	attProjectId.rep_.type_ = TeUNSIGNEDINT;
	attProjectId.rep_.isPrimaryKey_ = true;
	attProjectId.rep_.isAutoNumber_ = true;
	attProjectId.rep_.null_ = false;
	attList.push_back(attProjectId);}

	{TeAttribute attName;
	attName.rep_.name_ = "name";
	attName.rep_.type_ = TeSTRING;
	attName.rep_.numChar_ = 50;
	attName.rep_.null_ = false;
	attList.push_back(attName);}

	{TeAttribute attLong0;
	attLong0.rep_.name_ = "long0";
	attLong0.rep_.type_ = TeREAL;
	attLong0.rep_.decimals_ = 15;
	attLong0.rep_.defaultValue_ = "0.0";
	attList.push_back(attLong0);}

	{TeAttribute attLat0;
	attLat0.rep_.name_ = "lat0";
	attLat0.rep_.type_ = TeREAL;
	attLat0.rep_.decimals_ = 15;
	attLat0.rep_.defaultValue_ = "0.0";
	attList.push_back(attLat0);}

	{TeAttribute attOffX;
	attOffX.rep_.name_ = "offx";
	attOffX.rep_.type_ = TeREAL;
	attOffX.rep_.decimals_ = 15;
	attOffX.rep_.defaultValue_ = "0.0";
	attList.push_back(attOffX);}

	{TeAttribute attOffY;
	attOffY.rep_.name_ = "offy";
	attOffY.rep_.type_ = TeREAL;
	attOffY.rep_.decimals_ = 15;
	attOffY.rep_.defaultValue_ = "0.0";
	attList.push_back(attOffY);}

	{TeAttribute attSlat1;
	attSlat1.rep_.name_ = "stlat1";
	attSlat1.rep_.type_ = TeREAL;
	attSlat1.rep_.decimals_ = 15;
	attSlat1.rep_.defaultValue_ = "0.0";
	attList.push_back(attSlat1);}

	{TeAttribute attSlat2;
	attSlat2.rep_.name_ = "stlat2";
	attSlat2.rep_.type_ = TeREAL;
	attSlat2.rep_.decimals_ = 15;
	attSlat2.rep_.defaultValue_ = "0.0";
	attList.push_back(attSlat2);}

	{TeAttribute attUnit;
	attUnit.rep_.name_ = "unit";
	attUnit.rep_.type_ = TeSTRING;
	attUnit.rep_.numChar_ = 50;
	attUnit.rep_.null_ = false;
	attList.push_back(attUnit);}

	{TeAttribute attScale;
	attScale.rep_.name_ = "scale";
	attScale.rep_.type_ = TeREAL;
	attScale.rep_.decimals_ = 15;
	attScale.rep_.defaultValue_ = "0.0";
	attList.push_back(attScale);}

	{TeAttribute attHemis;
	attHemis.rep_.name_ = "hemis";
	attHemis.rep_.type_ = TeINT;
	attHemis.rep_.null_ = false;
	attList.push_back(attHemis);}

	{TeAttribute attDatum;
	attDatum.rep_.name_ = "datum";
	attDatum.rep_.type_ = TeSTRING;
	attDatum.rep_.numChar_ = 50;
	attDatum.rep_.null_ = false;
	attList.push_back(attDatum);}

	{TeAttribute attRadius;
	attRadius.rep_.name_ = "radius";
	attRadius.rep_.type_ = TeREAL;
	attRadius.rep_.decimals_ = 15;
	attRadius.rep_.defaultValue_ = "0.0";
	attList.push_back(attRadius);}

	{TeAttribute attFlattening;
	attFlattening.rep_.name_ = "flattening";
	attFlattening.rep_.type_ = TeREAL;
	attFlattening.rep_.decimals_ = 15;
	attFlattening.rep_.defaultValue_ = "0.0";
	attList.push_back(attFlattening);}

	{TeAttribute attDX;
	attDX.rep_.name_ = "dx";
	attDX.rep_.type_ = TeREAL;
	attDX.rep_.decimals_ = 15;
	attDX.rep_.defaultValue_ = "0.0";
	attList.push_back(attDX);}

	{TeAttribute attDY;
	attDY.rep_.name_ = "dy";
	attDY.rep_.type_ = TeREAL;
	attDY.rep_.decimals_ = 15;
	attDY.rep_.defaultValue_ = "0.0";
	attList.push_back(attDY);}

	{TeAttribute attDZ;
	attDZ.rep_.name_ = "dz";
	attDZ.rep_.type_ = TeREAL;
	attDZ.rep_.decimals_ = 15;
	attDZ.rep_.defaultValue_ = "0.0";
	attList.push_back(attDZ);}

	return createTable("te_projection", attList);
}

bool TeDatabase::createLayerTable ()
{
	TeAttributeList attList;

	{TeAttribute attLayerId;
	attLayerId.rep_.name_ = "layer_id";
	attLayerId.rep_.type_ = TeUNSIGNEDINT;
	attLayerId.rep_.isPrimaryKey_ = true;
	attLayerId.rep_.isAutoNumber_ = true;
	attLayerId.rep_.null_ = false;
	attList.push_back(attLayerId);}

	{TeAttribute attProjectionId;
	attProjectionId.rep_.name_ = "projection_id";
	attProjectionId.rep_.type_ = TeUNSIGNEDINT;
	attProjectionId.rep_.null_ = false;
	attList.push_back(attProjectionId);}

	{TeAttribute attName;
	attName.rep_.name_ = "name";
	attName.rep_.type_ = TeSTRING;
	attName.rep_.numChar_ = 255;
	attName.rep_.null_ = false;
	attList.push_back(attName);}

	{TeAttribute attLowerX;
	attLowerX.rep_.name_ = "lower_x";
	attLowerX.rep_.type_ = TeREAL;
	attLowerX.rep_.decimals_ = 15;
	attLowerX.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerX);}

	{TeAttribute attLowerY;
	attLowerY.rep_.name_ = "lower_y";
	attLowerY.rep_.type_ = TeREAL;
	attLowerY.rep_.decimals_ = 15;
	attLowerY.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerY);}

	{TeAttribute attUpperX;
	attUpperX.rep_.name_ = "upper_x";
	attUpperX.rep_.type_ = TeREAL;
	attUpperX.rep_.decimals_ = 15;
	attUpperX.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperX);}

	{TeAttribute attUpperY;
	attUpperY.rep_.name_ = "upper_y";
	attUpperY.rep_.type_ = TeREAL;
	attUpperY.rep_.decimals_ = 15;
	attUpperY.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperY);}

	{TeAttribute attInitialTime;
	attInitialTime.rep_.name_ = "initial_time";
	attInitialTime.rep_.type_ = TeDATETIME;
	attList.push_back(attInitialTime);}

	{TeAttribute attFinalTime;
	attFinalTime.rep_.name_ = "final_time";
	attFinalTime.rep_.type_ = TeDATETIME;
	attList.push_back(attFinalTime);}

	if(!createTable("te_layer", attList))
		return false;

	string idxName = "te_idx_layer_proj";

	if(!createIndex("te_layer", idxName, "projection_id"))
		return false;

	idxName = "te_idx_layer_name";

	return createIndex("te_layer", idxName, "name");
}

bool TeDatabase::createLayerTableTable()
{
	TeAttributeList attList;

	{TeAttribute attTableId;
	attTableId.rep_.name_ = "table_id";
	attTableId.rep_.type_ = TeUNSIGNEDINT;
	attTableId.rep_.isPrimaryKey_ = true;
	attTableId.rep_.isAutoNumber_ = true;
	attTableId.rep_.null_ = false;
	attList.push_back(attTableId);}

	{TeAttribute attLayerId;
	attLayerId.rep_.name_ = "layer_id";
	attLayerId.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attLayerId);}

	{TeAttribute attTable;
	attTable.rep_.name_ = "attr_table";
	attTable.rep_.type_ = TeSTRING;
	attTable.rep_.numChar_ = 255;
	attTable.rep_.null_ = false;
	attList.push_back(attTable);}

	{TeAttribute attUniqueID;
	attUniqueID.rep_.name_ = "unique_id";
	attUniqueID.rep_.type_ = TeSTRING;
	attUniqueID.rep_.numChar_ = 255;
	attList.push_back(attUniqueID);}

	{TeAttribute attLink;
	attLink.rep_.name_ = "attr_link";
	attLink.rep_.type_ = TeSTRING;
	attLink.rep_.numChar_ = 255;
	attList.push_back(attLink);}

	{TeAttribute attAttrInitialTime;
	attAttrInitialTime.rep_.name_ = "attr_initial_time";
	attAttrInitialTime.rep_.type_ = TeSTRING;
	attAttrInitialTime.rep_.numChar_ = 255;
	attList.push_back(attAttrInitialTime);}

	{TeAttribute attAttrFinalTime;
	attAttrFinalTime.rep_.name_ = "attr_final_time";
	attAttrFinalTime.rep_.type_ = TeSTRING;
	attAttrFinalTime.rep_.numChar_ = 255;
	attList.push_back(attAttrFinalTime);}

	{TeAttribute attTimeUnit;
	attTimeUnit.rep_.name_ = "attr_time_unit";
	attTimeUnit.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attTimeUnit);}

	{TeAttribute attTableType;
	attTableType.rep_.name_ = "attr_table_type";
	attTableType.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attTableType);}

	{TeAttribute attUserName;
	attUserName.rep_.name_ = "user_name";
	attUserName.rep_.type_ = TeSTRING;
	attUserName.rep_.numChar_ = 255;
	attList.push_back(attUserName);}

	{TeAttribute attInitialTime;
	attInitialTime.rep_.name_ = "initial_time";
	attInitialTime.rep_.type_ = TeDATETIME;
	attList.push_back(attInitialTime);}

	{TeAttribute attFinalTime;
	attFinalTime.rep_.name_ = "final_time";
	attFinalTime.rep_.type_ = TeDATETIME;
	attList.push_back(attFinalTime);}

	if(!createTable("te_layer_table", attList))
		return false;

	string idxName = "te_idx_layertable_layer";

	if(!createIndex("te_layer_table", idxName, "layer_id"))
		return false;

	idxName = "te_idx_layertable_att";

	return createIndex("te_layer_table", idxName, "attr_table");
}

bool 
TeDatabase::updateTableInfo(int layerId, TeTable &table, const string user)
{
	if (table.id() <= 0 )  // table information doesn�t exist in database yet
		return this->insertTableInfo(layerId,table,user);
	string sql;
	sql = "UPDATE te_layer_table SET ";
	sql += "unique_id='" + table.uniqueName() + "', ";
	sql += "attr_link='" + table.linkName() + "', ";
	sql += "attr_initial_time='" + table.attInitialTime() + "', ";
	sql += "attr_final_time='" + table.attFinalTime() + "', ";
	sql += "attr_time_unit=" + Te2String(table.attTimeUnit()) + ", ";
	sql += "attr_table_type="+ Te2String(table.tableType()) + ", ";
	sql += "user_name='" + user + "' WHERE ";
	sql += "layer_id=" + Te2String(layerId) + " AND ";
	sql += "attr_table_name='" + table.name() + "'";
	return execute (sql);
}

bool 
TeDatabase::loadTableInfo(TeTable& table)
{
	if (table.name().empty())
		return false;

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
	{
		this->errorMessage_ = "N�o foi poss�vel abrir portal para o banco";
		return false;
	}
	
	//theme tables
	string sel = " SELECT table_id, layer_id, attr_table, unique_id, ";
	sel += " attr_link, attr_initial_time, attr_final_time, attr_time_unit, ";
	sel += " attr_table_type, user_name, initial_time, final_time ";
	sel += " FROM te_layer_table ";
	sel += " WHERE  attr_table = '" + table.name() + "'";
		
	if ((!portal->query(sel)) || (!portal->fetchRow()))
	{	
		delete portal;
		return false;
	}
	
	if(!portal->getAttrTable(table))
	{
		delete portal;
		return false;
	}

	TeAttributeList attrList;
	getAttributeList(table.name(), attrList);
	table.setAttributeList(attrList);
	
	if (table.tableType() == TeAttrExternal) //information about external table 
	{
		portal->freeResult();

		sel = " SELECT te_layer_table.attr_table, te_tables_relation.related_table_id, ";
		sel += " te_tables_relation.related_attr, te_tables_relation.external_attr  ";
		sel += " FROM te_layer_table INNER JOIN te_tables_relation";
		sel += " ON te_layer_table.table_id = te_tables_relation.related_table_id ";
		sel += " WHERE  te_tables_relation.external_table_name = '" + table.name() + "'";

		if (!portal->query(sel))
		{	
			delete portal;
			return false;
		}

		if (!portal->fetchRow())
		{
			delete portal;
			return true;
		}
		
		int relatedTableId = portal->getInt(1); //related_table_id
		string relatedTable = portal->getData(0); //static table name 
		string relatedAttr = portal->getData(2); // static column name - related_attr
        		
		table.setTableType(TeAttrExternal, relatedTableId, relatedAttr);
		table.relatedTableName(relatedTable);
		table.setLinkName(portal->getData(3));  //external column name - external_attr
	}
	
	delete portal;
	return true;
}


bool TeDatabase::createLUTTable(const string& name)
{
	if(name.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attIndexId;
	attIndexId.rep_.name_ = "index_id";
	attIndexId.rep_.type_ = TeUNSIGNEDINT;
	attIndexId.rep_.isPrimaryKey_ = true;
	attIndexId.rep_.null_ = false;
	attList.push_back(attIndexId);}

	{TeAttribute attRVal;
	attRVal.rep_.name_ = "r_val";
	attRVal.rep_.type_ = TeUNSIGNEDINT;
	attRVal.rep_.null_ = false;
	attList.push_back(attRVal);}

	{TeAttribute attGVal;
	attGVal.rep_.name_ = "g_val";
	attGVal.rep_.type_ = TeUNSIGNEDINT;
	attGVal.rep_.null_ = false;
	attList.push_back(attGVal);}

	{TeAttribute attBVal;
	attBVal.rep_.name_ = "b_val";
	attBVal.rep_.type_ = TeUNSIGNEDINT;
	attBVal.rep_.null_ = false;
	attList.push_back(attBVal);}

	return createTable(name, attList);
}

bool TeDatabase::createTablesRelationTable()
{
	TeAttributeList attList;
	
	{TeAttribute attRelationId;
	attRelationId.rep_.name_ = "relation_id";
	attRelationId.rep_.type_ = TeUNSIGNEDINT;
	attRelationId.rep_.isAutoNumber_ = true;
	attRelationId.rep_.isPrimaryKey_ = true;
	attRelationId.rep_.null_ = false;
	attList.push_back(attRelationId);}

	{TeAttribute attRelatedTableId;
	attRelatedTableId.rep_.name_ = "related_table_id";
	attRelatedTableId.rep_.type_ = TeINT;
	attRelatedTableId.rep_.null_ = false;
	attList.push_back(attRelatedTableId);}

	{TeAttribute attRelatedAttr;
	attRelatedAttr.rep_.name_ = "related_attr";
	attRelatedAttr.rep_.type_ = TeSTRING;
	attRelatedAttr.rep_.numChar_ = 255;
	attRelatedAttr.rep_.null_ = false;
	attList.push_back(attRelatedAttr);}

	{TeAttribute attExternalTableName;
	attExternalTableName.rep_.name_ = "external_table_name";
	attExternalTableName.rep_.type_ = TeSTRING;
	attExternalTableName.rep_.numChar_ = 255;
	attExternalTableName.rep_.null_ = false;
	attList.push_back(attExternalTableName);}

	{TeAttribute attExternalAttr;
	attExternalAttr.rep_.name_ = "external_attr";
	attExternalAttr.rep_.type_ = TeSTRING;
	attExternalAttr.rep_.numChar_ = 255;
	attExternalAttr.rep_.null_ = false;
	attList.push_back(attExternalAttr);}

	return createTable("te_tables_relation", attList);
}

bool TeDatabase::createRepresentationTable ()
{
	TeAttributeList attList;
	
	{TeAttribute attRepresId;
	attRepresId.rep_.name_ = "repres_id";
	attRepresId.rep_.type_ = TeUNSIGNEDINT;
	attRepresId.rep_.isAutoNumber_ = true;
	attRepresId.rep_.isPrimaryKey_ = true;
	attRepresId.rep_.null_ = false;
	attList.push_back(attRepresId);}

	{TeAttribute attLayerId;
	attLayerId.rep_.name_ = "layer_id";
	attLayerId.rep_.type_ = TeUNSIGNEDINT;
	attLayerId.rep_.null_ = false;
	attList.push_back(attLayerId);}

	{TeAttribute attGeomType;
	attGeomType.rep_.name_ = "geom_type";
	attGeomType.rep_.type_ = TeINT;
	attGeomType.rep_.null_ = false;
	attList.push_back(attGeomType);}

	{TeAttribute attGeomTable;
	attGeomTable.rep_.name_ = "geom_table";
	attGeomTable.rep_.type_ = TeSTRING;
	attGeomTable.rep_.numChar_ = 255;
	attGeomTable.rep_.null_ = false;
	attList.push_back(attGeomTable);}

	{TeAttribute attDescription;
	attDescription.rep_.name_ = "description";
	attDescription.rep_.type_ = TeSTRING;
	attDescription.rep_.numChar_ = 255;
	attList.push_back(attDescription);}

	{TeAttribute attLowerX;
	attLowerX.rep_.name_ = "lower_x";
	attLowerX.rep_.type_ = TeREAL;
	attLowerX.rep_.decimals_ = 15;
	attLowerX.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerX);}

	{TeAttribute attLowerY;
	attLowerY.rep_.name_ = "lower_y";
	attLowerY.rep_.type_ = TeREAL;
	attLowerY.rep_.decimals_ = 15;
	attLowerY.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerY);}

	{TeAttribute attUpperX;
	attUpperX.rep_.name_ = "upper_x";
	attUpperX.rep_.type_ = TeREAL;
	attUpperX.rep_.decimals_ = 15;
	attUpperX.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperX);}

	{TeAttribute attUpperY;
	attUpperY.rep_.name_ = "upper_y";
	attUpperY.rep_.type_ = TeREAL;
	attUpperY.rep_.decimals_ = 15;
	attUpperY.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperY);}

	{TeAttribute attResX;
	attResX.rep_.name_ = "res_x";
	attResX.rep_.type_ = TeREAL;
	attResX.rep_.decimals_ = 15;
	attResX.rep_.defaultValue_ = "0.0";
	attList.push_back(attResX);}

	{TeAttribute attResY;
	attResY.rep_.name_ = "res_y";
	attResY.rep_.type_ = TeREAL;
	attResY.rep_.decimals_ = 15;
	attResY.rep_.defaultValue_ = "0.0";
	attList.push_back(attResY);}

	{TeAttribute attNumCols;
	attNumCols.rep_.name_ = "num_cols";
	attNumCols.rep_.type_ = TeINT;
	attList.push_back(attNumCols);}

	{TeAttribute attNumRows;
	attNumRows.rep_.name_ = "num_rows";
	attNumRows.rep_.type_ = TeINT;
	attList.push_back(attNumRows);}

	{TeAttribute attInitialTime;
	attInitialTime.rep_.name_ = "initial_time";
	attInitialTime.rep_.type_ = TeDATETIME;
	attList.push_back(attInitialTime);}

	{TeAttribute attFinalTime;
	attFinalTime.rep_.name_ = "final_time";
	attFinalTime.rep_.type_ = TeDATETIME;
	attList.push_back(attFinalTime);}

	if(!createTable("te_representation", attList))
		return false;

	string idxName = "te_idx_representation";

	return createIndex("te_representation", idxName, "layer_id");
}

bool TeDatabase::createRasterMetadataTable(const string& tableName)
{
	if(tableName.empty())
		return false;

	TeAttributeList attList;
	
	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attBandId;
	attBandId.rep_.name_ = "band_id";
	attBandId.rep_.type_ = TeUNSIGNEDINT;
	attBandId.rep_.isPrimaryKey_ = true;
	attBandId.rep_.null_ = false;
	attList.push_back(attBandId);}

	{TeAttribute attMinValue;
	attMinValue.rep_.name_ = "min_value";
	attMinValue.rep_.type_ = TeREAL;
	attMinValue.rep_.decimals_ = 15;
	attMinValue.rep_.defaultValue_ = "0.0";
	attList.push_back(attMinValue);}

	{TeAttribute attMaxValue;
	attMaxValue.rep_.name_ = "max_value";
	attMaxValue.rep_.type_ = TeREAL;
	attMaxValue.rep_.decimals_ = 15;
	attMaxValue.rep_.defaultValue_ = "0.0";
	attList.push_back(attMaxValue);}

	{TeAttribute attNumBits;
	attNumBits.rep_.name_ = "num_bits";
	attNumBits.rep_.type_ = TeINT;
	attList.push_back(attNumBits);}

	{TeAttribute attDatatype;
	attDatatype.rep_.name_ = "data_type";
	attDatatype.rep_.type_ = TeINT;
	attList.push_back(attDatatype);}

	{TeAttribute attPhotoType;
	attPhotoType.rep_.name_ = "photometric_type";
	attPhotoType.rep_.type_ = TeINT;
	attList.push_back(attPhotoType);}

	{TeAttribute attCompressType;
	attCompressType.rep_.name_ = "compression_type";
	attCompressType.rep_.type_ = TeINT;
	attList.push_back(attCompressType);}

	{TeAttribute attDummy;
	attDummy.rep_.name_ = "dummy";
	attDummy.rep_.type_ = TeREAL;
	attDummy.rep_.decimals_ = 15;
	attList.push_back(attDummy);}

	return createTable(tableName, attList);
}

bool TeDatabase::createRasterTable(const string& tableName)
{
	if(tableName.empty())
		return false;

	TeAttributeList attList;
	
	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "block_id";
	attGeomId.rep_.type_ = TeSTRING;
	attGeomId.rep_.numChar_ = 50;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attSpatialData;
	attSpatialData.rep_.name_ = "spatial_data";
	attSpatialData.rep_.type_ = TeRASTERTYPE;
	attList.push_back(attSpatialData);}

	if(!createTable(tableName, attList))
		return false;

	string idxName = "te_idx_" + tableName + "_b";

	return createIndex(tableName, idxName, "band_id");
}

bool TeDatabase::createRasterGeometry(const string& tableName)
{
	if(tableName.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attRasterTable;
	attRasterTable.rep_.name_ = "raster_table";
	attRasterTable.rep_.type_ = TeSTRING;
	attRasterTable.rep_.numChar_ = 255;
	attRasterTable.rep_.null_ = false;
	attList.push_back(attRasterTable);}

	{TeAttribute attLutTable;
	attLutTable.rep_.name_ = "lut_table";
	attLutTable.rep_.type_ = TeSTRING;
	attLutTable.rep_.numChar_ = 255;
	attList.push_back(attLutTable);}

	{TeAttribute attResX;
	attResX.rep_.name_ = "res_x";
	attResX.rep_.type_ = TeREAL;
	attResX.rep_.decimals_ = 15;
	attResX.rep_.defaultValue_ = "0.0";
	attList.push_back(attResX);}

	{TeAttribute attResY;
	attResY.rep_.name_ = "res_y";
	attResY.rep_.type_ = TeREAL;
	attResY.rep_.decimals_ = 15;
	attResY.rep_.defaultValue_ = "0.0";
	attList.push_back(attResY);}

	{TeAttribute attNumBands;
	attNumBands.rep_.name_ = "num_bands";
	attNumBands.rep_.type_ = TeINT;
	attList.push_back(attNumBands);}

	{TeAttribute attNumCols;
	attNumCols.rep_.name_ = "num_cols";
	attNumCols.rep_.type_ = TeINT;
	attList.push_back(attNumCols);}

	{TeAttribute attNumRows;
	attNumRows.rep_.name_ = "num_rows";
	attNumRows.rep_.type_ = TeINT;
	attList.push_back(attNumRows);}

	{TeAttribute attBlockHeight;
	attBlockHeight.rep_.name_ = "block_height";
	attBlockHeight.rep_.type_ = TeINT;
	attList.push_back(attBlockHeight);}

	{TeAttribute attBlockWidth;
	attBlockWidth.rep_.name_ = "block_width";
	attBlockWidth.rep_.type_ = TeINT;
	attList.push_back(attBlockWidth);}

	{TeAttribute attLowerX;
	attLowerX.rep_.name_ = "lower_x";
	attLowerX.rep_.type_ = TeREAL;
	attLowerX.rep_.decimals_ = 15;
	attLowerX.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerX);}

	{TeAttribute attLowerY;
	attLowerY.rep_.name_ = "lower_y";
	attLowerY.rep_.type_ = TeREAL;
	attLowerY.rep_.decimals_ = 15;
	attLowerY.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerY);}

	{TeAttribute attUpperX;
	attUpperX.rep_.name_ = "upper_x";
	attUpperX.rep_.type_ = TeREAL;
	attUpperX.rep_.decimals_ = 15;
	attUpperX.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperX);}

	{TeAttribute attUpperY;
	attUpperY.rep_.name_ = "upper_y";
	attUpperY.rep_.type_ = TeREAL;
	attUpperY.rep_.decimals_ = 15;
	attUpperY.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperY);}

	{TeAttribute attTilingType;
	attTilingType.rep_.name_ = "tiling_type";
	attTilingType.rep_.type_ = TeINT;
	attList.push_back(attTilingType);}

	if(!createTable(tableName, attList))
		return  false;

	string idxName = "te_idx_" + tableName;

	return createIndex(tableName,  idxName, "object_id");
}

bool TeDatabase::createViewTable ()
{
	TeAttributeList attList;

	{TeAttribute attViewId;
	attViewId.rep_.name_ = "view_id";
	attViewId.rep_.type_ = TeUNSIGNEDINT;
	attViewId.rep_.isPrimaryKey_ = true;
	attViewId.rep_.isAutoNumber_ = true;
	attViewId.rep_.null_ = false;
	attList.push_back(attViewId);}

	{TeAttribute attProjectionId;
	attProjectionId.rep_.name_ = "projection_id";
	attProjectionId.rep_.type_ = TeUNSIGNEDINT;
	attProjectionId.rep_.null_ = false;
	attList.push_back(attProjectionId);}

	{TeAttribute attName;
	attName.rep_.name_ = "name";
	attName.rep_.type_ = TeSTRING;
	attName.rep_.numChar_ = 255;
	attName.rep_.null_ = false;
	attList.push_back(attName);}

	{TeAttribute attUserName;
	attUserName.rep_.name_ = "user_name";
	attUserName.rep_.type_ = TeSTRING;
	attUserName.rep_.numChar_ = 255;
	attList.push_back(attUserName);}

	{TeAttribute attVisibility;
	attVisibility.rep_.name_ = "visibility";
	attVisibility.rep_.type_ = TeINT;
	attList.push_back(attVisibility);}

	{TeAttribute attLowerX;
	attLowerX.rep_.name_ = "lower_x";
	attLowerX.rep_.type_ = TeREAL;
	attLowerX.rep_.decimals_ = 15;
	attList.push_back(attLowerX);}

	{TeAttribute attLowerY;
	attLowerY.rep_.name_ = "lower_y";
	attLowerY.rep_.type_ = TeREAL;
	attLowerY.rep_.decimals_ = 15;
	attList.push_back(attLowerY);}

	{TeAttribute attUpperX;
	attUpperX.rep_.name_ = "upper_x";
	attUpperX.rep_.type_ = TeREAL;
	attUpperX.rep_.decimals_ = 15;
	attList.push_back(attUpperX);}

	{TeAttribute attUpperY;
	attUpperY.rep_.name_ = "upper_y";
	attUpperY.rep_.type_ = TeREAL;
	attUpperY.rep_.decimals_ = 15;
	attList.push_back(attUpperY);}

	{TeAttribute attCurrentTheme;
	attCurrentTheme.rep_.name_ = "current_theme";
	attCurrentTheme.rep_.type_ = TeINT;
	attList.push_back(attCurrentTheme);}

	if(!createTable("te_view", attList))
		return false;

	string idxName = "te_idx_view_projid";

	if(!createIndex("te_view", idxName, "projection_id"))
		return false;

	idxName = "te_idx_view_name";

	if(!createIndex("te_view", idxName, "name"))
		return false;

	idxName = "te_idx_view_user_name";

	return createIndex("te_view", idxName, "user_name");
}

bool TeDatabase::createThemeTable ()
{
	TeAttributeList attList;

	{TeAttribute attThemeId;
	attThemeId.rep_.name_ = "theme_id";
	attThemeId.rep_.type_ = TeUNSIGNEDINT;
	attThemeId.rep_.isPrimaryKey_ = true;
	attThemeId.rep_.isAutoNumber_ = true;
	attThemeId.rep_.null_ = false;
	attList.push_back(attThemeId);}

	{TeAttribute attLayerId;
	attLayerId.rep_.name_ = "layer_id";
	attLayerId.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attLayerId);}

	{TeAttribute attViewId;
	attViewId.rep_.name_ = "view_id";
	attViewId.rep_.type_ = TeUNSIGNEDINT;
	attViewId.rep_.null_ = false;
	attList.push_back(attViewId);}

	{TeAttribute attName;
	attName.rep_.name_ = "name";
	attName.rep_.type_ = TeSTRING;
	attName.rep_.numChar_ = 255;
	attName.rep_.null_ = false;
	attList.push_back(attName);}

	{TeAttribute attParentId;
	attParentId.rep_.name_ = "parent_id";
	attParentId.rep_.type_ = TeUNSIGNEDINT;
	attParentId.rep_.null_ = false;
	attList.push_back(attParentId);}

	{TeAttribute attPriority;
	attPriority.rep_.name_ = "priority";
	attPriority.rep_.type_ = TeUNSIGNEDINT;
	attPriority.rep_.null_ = false;
	attList.push_back(attPriority);}

	{TeAttribute attNodeType;
	attNodeType.rep_.name_ = "node_type";
	attNodeType.rep_.type_ = TeUNSIGNEDINT;
	attNodeType.rep_.null_ = false;
	attList.push_back(attNodeType);}

	{TeAttribute attMinScale;
	attMinScale.rep_.name_ = "min_scale";
	attMinScale.rep_.type_ = TeREAL;
	attMinScale.rep_.decimals_ = 15;
	attList.push_back(attMinScale);}

	{TeAttribute attMaxScale;
	attMaxScale.rep_.name_ = "max_scale";
	attMaxScale.rep_.type_ = TeREAL;
	attMaxScale.rep_.decimals_ = 15;
	attList.push_back(attMaxScale);}

	{TeAttribute attGenAttWhere;
	attGenAttWhere.rep_.name_ = "generate_attribute_where";
	attGenAttWhere.rep_.type_ = TeSTRING;
	attGenAttWhere.rep_.numChar_ = 255;
	attList.push_back(attGenAttWhere);}

	{TeAttribute attGenSpatWhere;
	attGenSpatWhere.rep_.name_ = "generate_spatial_where";
	attGenSpatWhere.rep_.type_ = TeSTRING;
	attGenSpatWhere.rep_.numChar_ = 255;
	attList.push_back(attGenSpatWhere);}

	{TeAttribute attGenTempWhere;
	attGenTempWhere.rep_.name_ = "generate_temporal_where";
	attGenTempWhere.rep_.type_ = TeSTRING;
	attGenTempWhere.rep_.numChar_ = 255;
	attList.push_back(attGenTempWhere);}

	{TeAttribute attCollectionT;
	attCollectionT.rep_.name_ = "collection_table";
	attCollectionT.rep_.type_ = TeSTRING;
	attCollectionT.rep_.numChar_ = 255;
	attList.push_back(attCollectionT);}

	{TeAttribute attVisiRep;
	attVisiRep.rep_.name_ = "visible_rep";
	attVisiRep.rep_.type_ = TeINT;
	attList.push_back(attVisiRep);}

	{TeAttribute attEnableVis;
	attEnableVis.rep_.name_ = "enable_visibility";
	attEnableVis.rep_.type_ = TeINT;
	attList.push_back(attEnableVis);}

	{TeAttribute attLowerX;
	attLowerX.rep_.name_ = "lower_x";
	attLowerX.rep_.type_ = TeREAL;
	attLowerX.rep_.decimals_ = 15;
	attLowerX.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerX);}

	{TeAttribute attLowerY;
	attLowerY.rep_.name_ = "lower_y";
	attLowerY.rep_.type_ = TeREAL;
	attLowerY.rep_.decimals_ = 15;
	attLowerY.rep_.defaultValue_ = "0.0";
	attList.push_back(attLowerY);}

	{TeAttribute attUpperX;
	attUpperX.rep_.name_ = "upper_x";
	attUpperX.rep_.type_ = TeREAL;
	attUpperX.rep_.decimals_ = 15;
	attUpperX.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperX);}

	{TeAttribute attUpperY;
	attUpperY.rep_.name_ = "upper_y";
	attUpperY.rep_.type_ = TeREAL;
	attUpperY.rep_.decimals_ = 15;
	attUpperY.rep_.defaultValue_ = "0.0";
	attList.push_back(attUpperY);}

	if(!createTable("te_theme", attList))
		return false;	

	string idxName = "te_idx_theme_view_id";

	if(!createIndex("te_theme", idxName, "view_id"))
		return false;

	idxName = "te_idx_theme_name";

	if(!createIndex("te_theme", idxName, "name"))
		return false;

	idxName = "te_idx_theme_layer_id";

	return createIndex("te_theme", idxName, "layer_id");
}

bool TeDatabase::createGroupingTable()
{
	TeAttributeList attList;

	{TeAttribute attThemeId;
	attThemeId.rep_.name_ = "theme_id";
	attThemeId.rep_.type_ = TeUNSIGNEDINT;
	attThemeId.rep_.isPrimaryKey_ = true;
	attThemeId.rep_.null_ = false;
	attList.push_back(attThemeId);}

	{TeAttribute attGNumber;
	attGNumber.rep_.name_ = "grouping_number";
	attGNumber.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attGNumber);}

	{TeAttribute attGAttr;
	attGAttr.rep_.name_ = "grouping_attr";
	attGAttr.rep_.type_ = TeSTRING;
	attGAttr.rep_.numChar_ = 255;
	attList.push_back(attGAttr);}

	{TeAttribute attGAttrType;
	attGAttrType.rep_.name_ = "grouping_attr_type";
	attGAttrType.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attGAttrType);}

	{TeAttribute attGMode;
	attGMode.rep_.name_ = "grouping_mode";
	attGMode.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attGMode);}

	{TeAttribute attGNormAttr;
	attGNormAttr.rep_.name_ = "grouping_norm_attr";
	attGNormAttr.rep_.type_ = TeSTRING;
	attGNormAttr.rep_.numChar_ = 255;
	attList.push_back(attGNormAttr);}

	{TeAttribute attGStdDev;
	attGStdDev.rep_.name_ = "grouping_std_dev";
	attGStdDev.rep_.type_ = TeREAL;
	attGStdDev.rep_.decimals_ = 15;
	attGStdDev.rep_.defaultValue_ = "0.0";
	attList.push_back(attGStdDev);}

	{TeAttribute attGPrec;
	attGPrec.rep_.name_ = "grouping_precision";
	attGPrec.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attGPrec);}

	{TeAttribute attGFunc;
	attGFunc.rep_.name_ = "grouping_function";
	attGFunc.rep_.type_ = TeSTRING;
	attGFunc.rep_.numChar_ = 20;
	attList.push_back(attGFunc);}

	{TeAttribute attGChronon;
	attGChronon.rep_.name_ = "grouping_chronon";
	attGChronon.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attGChronon);}

	return createTable("te_grouping", attList);
}

bool TeDatabase::createThemeTablesTable()
{
	TeAttributeList attList;

	{TeAttribute attThemeTableId;
	attThemeTableId.rep_.name_ = "theme_table_id";
	attThemeTableId.rep_.type_ = TeUNSIGNEDINT;
	attThemeTableId.rep_.isPrimaryKey_ = true;
	attThemeTableId.rep_.isAutoNumber_ = true;
	attThemeTableId.rep_.null_ = false;
	attList.push_back(attThemeTableId);}

	{TeAttribute attThemeId;
	attThemeId.rep_.name_ = "theme_id";
	attThemeId.rep_.type_ = TeUNSIGNEDINT;
	attThemeId.rep_.null_ = false;
	attList.push_back(attThemeId);}

	{TeAttribute attTableId;
	attTableId.rep_.name_ = "table_id";
	attTableId.rep_.type_ = TeUNSIGNEDINT;
	attTableId.rep_.null_ = false;
	attList.push_back(attTableId);}

	{TeAttribute attRelationId;
	attRelationId.rep_.name_ = "relation_id";
	attRelationId.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attRelationId);}

	{TeAttribute attTableOrderId;
	attTableOrderId.rep_.name_ = "table_order";
	attTableOrderId.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attTableOrderId);}

	if(!createTable("te_theme_table", attList))
		return false;

	string idxName = "te_idx_theme_table_id";

	if(!createIndex("te_theme_table", idxName, "table_id"))
		return false;

	idxName = "te_idx_theme_relat_id";

	if(!createIndex("te_theme_table", idxName, "relation_id"))
		return false;

	idxName = "te_idx_themetable_theme";

	return createIndex("te_theme_table", idxName, "theme_id");
}

bool TeDatabase::createLegendTable ()
{
	TeAttributeList attList;

	{TeAttribute attLegendId;
	attLegendId.rep_.name_ = "legend_id";
	attLegendId.rep_.type_ = TeUNSIGNEDINT;
	attLegendId.rep_.isPrimaryKey_ = true;
	attLegendId.rep_.isAutoNumber_ = true;
	attLegendId.rep_.null_ = false;
	attList.push_back(attLegendId);	}

	{TeAttribute attThemeId;
	attThemeId.rep_.name_ = "theme_id";
	attThemeId.rep_.type_ = TeUNSIGNEDINT;
	attThemeId.rep_.null_ = false;
	attList.push_back(attThemeId);}

	{TeAttribute attGId;
	attGId.rep_.name_ = "group_id";
	attGId.rep_.type_ = TeINT;
	attList.push_back(attGId);}

	{TeAttribute attNObjs;
	attNObjs.rep_.name_ = "num_objs";
	attNObjs.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attNObjs);}

	{TeAttribute attLValue;
	attLValue.rep_.name_ = "lower_value";
	attLValue.rep_.type_ = TeSTRING;
	attLValue.rep_.numChar_ = 255;
	attList.push_back(attLValue);}

	{TeAttribute attUValue;
	attUValue.rep_.name_ = "upper_value";
	attUValue.rep_.type_ = TeSTRING;
	attUValue.rep_.numChar_ = 255;
	attList.push_back(attUValue);}

	{TeAttribute attLabel;
	attLabel.rep_.name_ = "label";
	attLabel.rep_.type_ = TeSTRING;
	attLabel.rep_.numChar_ = 255;
	attList.push_back(attLabel);}
	
	if(!createTable("te_legend", attList))
		return false;

	string idxName = "te_idx_legend_theme";

	return createIndex("te_legend", idxName, "theme_id");
}

bool TeDatabase::createVisualTable()
{
	TeAttributeList attList;

	{TeAttribute attLegendId;
	attLegendId.rep_.name_ = "legend_id";
	attLegendId.rep_.type_ = TeUNSIGNEDINT;
	attLegendId.rep_.isPrimaryKey_ = true;
	attLegendId.rep_.null_ = false;
	attList.push_back(attLegendId);}

	{TeAttribute attGeomType;
	attGeomType.rep_.name_ = "geom_type";
	attGeomType.rep_.type_ = TeUNSIGNEDINT;
	attGeomType.rep_.isPrimaryKey_ = true;
	attGeomType.rep_.null_ = false;
	attList.push_back(attGeomType);}

	{TeAttribute attSymbId;
	attSymbId.rep_.name_ = "symb_id";
	attSymbId.rep_.type_ = TeINT;
	attList.push_back(attSymbId);}

	{TeAttribute attRed;
	attRed.rep_.name_ = "red";
	attRed.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attRed);}

	{TeAttribute attGreen;
	attGreen.rep_.name_ = "green";
	attGreen.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attGreen);}

	{TeAttribute attBlue;
	attBlue.rep_.name_ = "blue";
	attBlue.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attBlue);}

	{TeAttribute attTransp;
	attTransp.rep_.name_ = "transparency";
	attTransp.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attTransp);}

	{TeAttribute attWidth;
	attWidth.rep_.name_ = "width";
	attWidth.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attWidth);}

	{TeAttribute attContourSymbId;
	attContourSymbId.rep_.name_ = "contour_symb_id";
	attContourSymbId.rep_.type_ = TeINT;
	attList.push_back(attContourSymbId);}

	{TeAttribute attContourRed;
	attContourRed.rep_.name_ = "contour_red";
	attContourRed.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attContourRed);}

	{TeAttribute attContourGreen;
	attContourGreen.rep_.name_ = "contour_green";
	attContourGreen.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attContourGreen);}

	{TeAttribute attContourBlue;
	attContourBlue.rep_.name_ = "contour_blue";
	attContourBlue.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attContourBlue);}

	{TeAttribute attContourTransp;
	attContourTransp.rep_.name_ = "contour_transp";
	attContourTransp.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attContourTransp);}

	{TeAttribute attContourWidth;
	attContourWidth.rep_.name_ = "contour_width";
	attContourWidth.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attContourWidth);}

	{TeAttribute attSizeValue;
	attSizeValue.rep_.name_ = "size_value";
	attSizeValue.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attSizeValue);}

	{TeAttribute attPtAngle;
	attPtAngle.rep_.name_ = "pt_angle";
	attPtAngle.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attPtAngle);}

	{TeAttribute attFamily;
	attFamily.rep_.name_ = "family";
	attFamily.rep_.type_ = TeSTRING;
	attFamily.rep_.numChar_ = 255;
	attList.push_back(attFamily);}

	{TeAttribute attBold;
	attBold.rep_.name_ = "bold";
	attBold.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attBold);}

	{TeAttribute attItalic;
	attItalic.rep_.name_ = "italic";
	attItalic.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attItalic);}

	{TeAttribute attAlignVert;
	attAlignVert.rep_.name_ = "alignment_vert";
	attAlignVert.rep_.type_ = TeREAL;
	attAlignVert.rep_.decimals_ = 15;
	attList.push_back(attAlignVert);}

	{TeAttribute attAlignHoriz;
	attAlignHoriz.rep_.name_ = "alignment_horiz";
	attAlignHoriz.rep_.type_ = TeREAL;
	attAlignHoriz.rep_.decimals_ = 15;
	attList.push_back(attAlignHoriz);}

	{TeAttribute attTabSize;
	attTabSize.rep_.name_ = "tab_size";
	attTabSize.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attTabSize);}

	{TeAttribute attLineSpace;
	attLineSpace.rep_.name_ = "line_space";
	attLineSpace.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attLineSpace);}

	{TeAttribute attFixedSize;
	attFixedSize.rep_.name_ = "fixed_size";
	attFixedSize.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attFixedSize);}

	return createTable("te_visual", attList);
}


bool TeDatabase::createVisualRasterTable()
{	
	TeAttributeList attList;

	{TeAttribute attThemeId;
	attThemeId.rep_.name_ = "theme_id";
	attThemeId.rep_.type_ = TeUNSIGNEDINT;
	attThemeId.rep_.isPrimaryKey_ = true;
	attThemeId.rep_.null_ = false;
	attList.push_back(attThemeId);}

	{TeAttribute attBandIn;
	attBandIn.rep_.name_ = "band_in";
	attBandIn.rep_.type_ = TeUNSIGNEDINT;
	attBandIn.rep_.isPrimaryKey_ = true;
	attBandIn.rep_.null_ = false;
	attList.push_back(attBandIn);}

	{TeAttribute attBandOut;
	attBandOut.rep_.name_ = "band_out";
	attBandOut.rep_.type_ = TeUNSIGNEDINT;
	attList.push_back(attBandOut);}

	{TeAttribute attTransfType;
	attTransfType.rep_.name_ = "transf_type";
	attTransfType.rep_.type_ = TeINT;
	attList.push_back(attTransfType);}

	{TeAttribute attParam1;
	attParam1.rep_.name_ = "param1";
	attParam1.rep_.type_ = TeREAL;
	attParam1.rep_.decimals_ = 15;
	attList.push_back(attParam1);}

	{TeAttribute attParam2;
	attParam2.rep_.name_ = "param2";
	attParam2.rep_.type_ = TeREAL;
	attParam2.rep_.decimals_ = 15;
	attList.push_back(attParam2);}

	{TeAttribute attLutTable;
	attLutTable.rep_.name_ = "lut_table";
	attLutTable.rep_.type_ = TeSTRING;
	attLutTable.rep_.numChar_ = 255;
	attList.push_back(attLutTable);}

	return createTable("te_visual_raster", attList);
}

bool TeDatabase::insertVisual (TeLegendEntry *legend)
{
	TeGeomRepVisualMap::iterator it = legend->getVisualMap().begin();
	while ( it != legend->getVisualMap().end())
	{ 
		string style("0"), contourStyle("0"), sizeValue("0"), width("0");

		if(it->first == TePOLYGONS || it->first == TeCELLS)
		{
			contourStyle = Te2String(it->second->contourStyle());
			style = Te2String(it->second->style());
		}
		else if(it->first == TeLINES)
		{
			width = Te2String(it->second->width());
			style = Te2String(it->second->style());
		}
		else if(it->first == TePOINTS)
		{
			sizeValue = Te2String(it->second->size());
			style = Te2String(it->second->style());
		}
		else if(it->first == TeTEXT)
		{
			sizeValue = Te2String(it->second->size());
		}
		
		string insert = "INSERT INTO te_visual(legend_id, geom_type, ";
		insert += " symb_id, "; 
		insert += "red, green, blue, transparency, width, ";
		insert += " contour_symb_id, "; 
		insert += "contour_red, contour_green,";
		insert += "contour_blue, contour_transp, contour_width, size_value,";
		insert += "pt_angle, family, bold, italic, ";
		insert += "alignment_vert, alignment_horiz, tab_size, line_space, fixed_size) ";
		insert += " VALUES (";	
		insert += Te2String(legend->id()) + ", ";
		insert += Te2String(it->first)+ ", ";
		insert += style + ", ";
		insert += Te2String(it->second->color().red_) + ", ";
		insert += Te2String(it->second->color().green_) + ", ";
		insert += Te2String(it->second->color().blue_) + ", ";
		insert += Te2String(it->second->transparency()) + ", ";
		insert += width +",";
		insert += contourStyle + ", ";
		insert += Te2String(it->second->contourColor().red_) + ", ";
		insert += Te2String(it->second->contourColor().green_) + ", ";
		insert += Te2String(it->second->contourColor().blue_) + ", ";
		insert += Te2String(it->second->contourTransparency()) + ", ";	
		insert += Te2String(it->second->contourWidth()) + ", ";

		insert += sizeValue +",";
		insert += Te2String(it->second->ptAngle()) +", ";

		insert += "'" + it->second->family() + "', ";
		if (it->second->bold())
			insert += "1, ";
		else
			insert += "0, ";
		if (it->second->italic())
			insert += "1, ";
		else
			insert += "0, ";
		insert += Te2String(it->second->alignmentVert()) + ",";
		insert += Te2String(it->second->alignmentHoriz()) + ",";
		insert += Te2String(it->second->tabSize()) + ",";
		insert += Te2String(it->second->lineSpace()) +",";
		if (it->second->fixedSize())
			insert += "1 ";
		else
			insert += "0";			
		insert += ")";	

		if (!execute(insert))
			return false;
		++it;
	}
	return true;
}

bool TeDatabase::createCollectionTable(const string& tableName)
{
	if(tableName.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attCObjId;
	attCObjId.rep_.name_ = "c_object_id";
	attCObjId.rep_.type_ = TeSTRING;
	attCObjId.rep_.numChar_ = 255;
	attCObjId.rep_.isPrimaryKey_ = true;
	attCObjId.rep_.null_ = false;
	attList.push_back(attCObjId);}

	{TeAttribute attCLegendId;
	attCLegendId.rep_.name_ = "c_legend_id";
	attCLegendId.rep_.type_ = TeINT;
	attList.push_back(attCLegendId);}

	{TeAttribute attLabelX;
	attLabelX.rep_.name_ = "label_x";
	attLabelX.rep_.type_ = TeREAL;
	attLabelX.rep_.decimals_ = 15;
	attList.push_back(attLabelX);}

	{TeAttribute attLabelY;
	attLabelY.rep_.name_ = "label_y";
	attLabelY.rep_.type_ = TeREAL;
	attLabelY.rep_.decimals_ = 15;
	attList.push_back(attLabelY);}

	{TeAttribute attCLegendOwn;
	attCLegendOwn.rep_.name_ = "c_legend_own";
	attCLegendOwn.rep_.type_ = TeINT;
	attList.push_back(attCLegendOwn);}

	{TeAttribute attCObjStatus;
	attCObjStatus.rep_.name_ = "c_object_status";
	attCObjStatus.rep_.type_ = TeINT;
	attList.push_back(attCObjStatus);}

	if(!createTable(tableName, attList))
		return false;

	string collectionId;
	unsigned int pos = tableName.rfind("_");
	if ( (pos+1<tableName.size()) )
       collectionId = tableName.substr(pos+1); 

	string idxName = "te_idx_c" + collectionId  + "_clegid";

	if(!createIndex(tableName, idxName, "c_legend_id"))
		return false;

	idxName = "te_idx_c" + collectionId + "_clegown";

	return createIndex(tableName, idxName, "c_legend_own");
}

bool TeDatabase::createCellGeometry (const string& table)
{
	if(table.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attSpatial;
	attSpatial.rep_.name_ = "spatial_data";
	attSpatial.rep_.type_ = TeCELLTYPE;
	attList.push_back(attSpatial);}

	if(!createTable(table, attList))
		return false;

	string idxName = "te_idx_"  + table + "obj";

	if(!createIndex(table, idxName, "object_id"))
		return false;

	idxName = "te_idx_" + table + "_lc";

	return createIndex(table, idxName, "row_number, col_number");
}

bool TeDatabase::createTextGeometry(const string& table)
{
	if(table.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attX;
	attX.rep_.name_ = "x";
	attX.rep_.type_ = TeREAL;
	attX.rep_.decimals_ = 15;
	attX.rep_.defaultValue_ = "0.0";
	attList.push_back(attX);}

	{TeAttribute attY;
	attY.rep_.name_ = "y";
	attY.rep_.type_ = TeREAL;
	attY.rep_.decimals_ = 15;
	attY.rep_.defaultValue_ = "0.0";
	attList.push_back(attY);}

	{TeAttribute attTextValue;
	attTextValue.rep_.name_ = "text_value";
	attTextValue.rep_.type_ = TeSTRING;
	attTextValue.rep_.numChar_ = 255;
	attList.push_back(attTextValue);}

	{TeAttribute attAngle;
	attAngle.rep_.name_ = "angle";
	attAngle.rep_.type_ = TeREAL;
	attAngle.rep_.decimals_ = 15;
	attAngle.rep_.defaultValue_ = "0.0";
	attList.push_back(attAngle);}

	{TeAttribute attHeight;
	attHeight.rep_.name_ = "height";
	attHeight.rep_.type_ = TeREAL;
	attHeight.rep_.decimals_ = 15;
	attHeight.rep_.defaultValue_ = "0.0";
	attList.push_back(attHeight);}

	{TeAttribute attAlignVert;
	attAlignVert.rep_.name_ = "alignment_vert";
	attAlignVert.rep_.type_ = TeREAL;
	attAlignVert.rep_.decimals_ = 15;
	attList.push_back(attAlignVert);}

	{TeAttribute attAlignHoriz;
	attAlignHoriz.rep_.name_ = "alignment_horiz";
	attAlignHoriz.rep_.type_ = TeREAL;
	attAlignHoriz.rep_.decimals_ = 15;
	attList.push_back(attAlignHoriz);}

	if(!createTable(table, attList))
		return false;

	string idxName = "te_idx_"  + table + "_obj";

	if(!createIndex(table, idxName, "object_id"))
		return false;

	idxName = "te_idx_"  + table + "_pos";

	return createIndex(table, idxName, "x, y");
}

bool TeDatabase::createNodeGeometry(const string& table)
{
	if(table.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attSpatial;
	attSpatial.rep_.name_ = "spatial_data";
	attSpatial.rep_.type_ = TeNODETYPE;
	attList.push_back(attSpatial);}

	if(!createTable(table, attList))
		return false;

	string idxName = "te_idx_"  + table + "_obj";

	return createIndex(table, idxName, "object_id");
}

bool TeDatabase::createPointGeometry(const string& table)
{
	if(table.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attSpatial;
	attSpatial.rep_.name_ = "spatial_data";
	attSpatial.rep_.type_ = TePOINTTYPE;
	attList.push_back(attSpatial);}

	if(!createTable(table, attList))
		return false;

	string idxName = "te_idx_"  + table + "_obj";

	return createIndex(table, idxName, "object_id");
}

bool TeDatabase::createArcGeometry(const string& table)
{
	if(table.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attFromNode;
	attFromNode.rep_.name_ = "from_node";
	attFromNode.rep_.type_ = TeINT;
	attFromNode.rep_.null_ = false;
	attList.push_back(attFromNode);}

	{TeAttribute attToNode;
	attToNode.rep_.name_ = "to_node";
	attToNode.rep_.type_ = TeINT;
	attToNode.rep_.null_ = false;
	attList.push_back(attToNode);}

	if(!createTable(table, attList))
		return false;

	string idxName = "te_idx_"  + table + "_obj";

	if(!createIndex(table, idxName, "object_id"))
		return false;

	idxName = "te_idx_"  + table + "_from";

	if(!createIndex(table, idxName, "from_node"))
		return false;

	idxName = "te_idx_"  + table + "_to";

	return createIndex(table, idxName, "to_node");
}


bool
TeDatabase::insertTable	(TeTable &table)
{
	string tableName = table.name();
	int size = table.size();
	TeAttributeList att = table.attributeList();
	TeAttributeList::iterator it = att.begin();
	TeAttributeList::iterator itEnd = att.end();
	TeTableRow row;
	
	int blobIndex=-1;
	if (!beginTransaction())
		return false;

	int i;
	unsigned int j;
	for ( i = 0; i < size; i++ )	
	{
		row = table[i];
		it = att.begin();
		string attrs;
		string values;
		j = 1;
		int jj = 0;
		while ( it != itEnd )
		{
			if (row[jj].empty() || (*it).rep_.isAutoNumber_)
			{
				++it;
				j++;
				jj++;
				continue;
			}
			
			if (!values.empty())
			{
				attrs += ", ";
				values += ", ";
			}			
			attrs += (*it).rep_.name_;			
			switch ((*it).rep_.type_)
			{
			case TeSTRING:
				values += "'"+ escapeSequence( row[jj] ) +"'";
				break;
			case TeREAL:
				{
				std::string strValue = row[jj];
				replace(strValue.begin(), strValue.end(), ',', '.');
				values += strValue;
				}
				break;
			case TeINT:
				values += row[jj];
				break;
			case TeDATETIME:
				{
					const string temp_dt = string(row[jj].c_str());
					TeTime t(temp_dt, (*it).dateChronon_, (*it).dateTimeFormat_, (*it).dateSeparator_, (*it).timeSeparator_, (*it).indicatorPM_);
					values += this->getSQLTime(t);
				}
				break;
			case TeCHARACTER:
				values += "'" + escapeSequence(row[jj]) + "'";
				break;
			case TeBLOB:
				blobIndex = jj;
			default:
				values += "'"+ escapeSequence(row[jj]) +"'";
				break;
			}
			++it;
			j++;
			jj++;
		}

		if (values.empty()) 
			continue;

		string q = "INSERT INTO "+tableName + " ("+ attrs+") " + " VALUES ("+values+") ";
		if (!execute(q)) 
			continue;

		//If there were blob type 
		if(blobIndex>=0)
		{
			TeAttribute uniqueAttr;
			table.attrUnique(uniqueAttr);
			string uniqueValue;
			int indexUniquePos = table.attrUniquePosition();
			if(uniqueAttr.rep_.isAutoNumber_)
			{
				//we have to get the last autonumber inserted
				string sql = " SELECT MAX("+ uniqueAttr.rep_.name_ +") FROM ";
				sql += tableName;
				TeDatabasePortal* portal = getPortal();
				if(!portal)
				{
					rollbackTransaction();
					return false;
				}
				if(!portal->query(sql) || !portal->fetchRow())
				{
					delete portal;
					rollbackTransaction();
					return false;
				}
				uniqueValue = portal->getData(0);
				delete portal;
			}
			else
				//we can use the unique value
				uniqueValue = row[indexUniquePos];
			
			if(!insertBlob(tableName, att[blobIndex].rep_.name_, uniqueAttr.rep_, uniqueValue, 
				(unsigned char*) row[blobIndex].c_str(), row[blobIndex].size()))
			{
				rollbackTransaction();
				return false;
			} 
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::alterTable(const string& tableName, TeAttributeRep& rep, const string& oldColName)
{
	if(!tableExist(tableName))
		return false;

	string tab;

	if(oldColName.empty())
	{
		tab = " ALTER TABLE " + tableName + " MODIFY ";
		tab += rep.name_ +"  ";
	}
	else
	{
		tab = " ALTER TABLE " + tableName + " CHANGE ";
		tab += oldColName +" "+ rep.name_ +"  ";
	}

	switch (rep.type_)
	{
		case TeSTRING:
			tab += "VARCHAR(" + Te2String(rep.numChar_) + ") ";
			break;
			
		case TeREAL:
			tab += "DOUBLE(24, 15)";	
			break;
			
		case TeINT:
			tab += "INT";
			break;

		case TeDATETIME:
			tab += "DATETIME";
			break;

		case TeCHARACTER:
			tab += "CHAR";
			break;

		case TeBLOB:
			tab += "LONGBLOB";
			break; 
		
		default:
			tab += "VARCHAR(" + Te2String(rep.numChar_) + ") ";
			break;
	}

	tab += " NULL ";

	if(!execute(tab))
	{
		if(errorMessage_.empty())
			errorMessage_ = "Error alter table " + tableName + " !";
		return false;
	}

	string tableId;
	TeDatabasePortal* portal = getPortal();
	string sql = "SELECT table_id FROM te_layer_table WHERE attr_table = '" + tableName + "'";
	if(portal->query(sql) && portal->fetchRow())
		tableId = portal->getData(0);

	delete portal;

	if(tableId.empty() == false)
	{
		if(oldColName.empty() == false) // column name changed
		{
			 // update relation
			sql = "UPDATE te_tables_relation SET related_attr = '" + rep.name_ + "'";
			sql += " WHERE related_table_id = " + tableId;
			sql += " AND related_attr = '" + oldColName + "'";
			if(execute(sql) == false)
				return false;

			sql = "UPDATE te_tables_relation SET external_attr = '" + rep.name_ + "'";
			sql += " WHERE external_table_name = '" + tableName + "'";
			sql += " AND external_attr = '" + oldColName + "'";
			if(execute(sql) == false)
				return false;

			 // update grouping
			sql = "UPDATE te_grouping SET grouping_attr = '" + tableName + "." + rep.name_ + "'";
			sql += " WHERE grouping_attr = '" + tableName + "." + oldColName + "'";
			if(execute(sql) == false)
				return false;
		}
		else // column type changed
		{
			// delete relation
			sql = "DELETE FROM te_tables_relation WHERE (related_table_id = " + tableId;
			sql += " AND related_attr = '" + rep.name_ + "')";
			sql += " OR (external_table_name = '" + tableName + "'";
			sql += " AND external_attr = '" + rep.name_ + "')";
			if(execute(sql) == false)
				return false;

			// delete grouping
			TeDatabasePortal* portal = getPortal();
			sql = "SELECT theme_id FROM te_grouping WHERE grouping_attr = '" + tableName + "." + oldColName + "'";
			if(portal->query(sql) && portal->fetchRow())
			{
				string themeId = portal->getData(0);

				sql = "DELETE FROM te_legend WHERE theme_id = " + themeId + " AND group_id >= 0";
				if(execute(sql) == false)
				{
					delete portal;
					return false;
				}
			}
			delete portal;

			sql = "DELETE FROM te_grouping";
			sql += " WHERE grouping_attr = '" + tableName + "." + oldColName + "'";
			if(execute(sql) == false)
				return false;
		}
	}

	alterTableInfoInMemory(tableName);
	return true;
}

bool 
TeDatabase::alterTable(const string& oldTableName, const string& newTablename)
{
	string sql = " ALTER TABLE "+ oldTableName +" RENAME "+ newTablename;
	if(!execute(sql))
		return false;

	//update te_layer_table
	sql = " UPDATE te_layer_table ";
	sql += " SET attr_table = '"+ newTablename +"'";
	sql += " WHERE attr_table = '"+ oldTableName +"'";
	execute(sql);

	//update te_tables_relation
	sql = " UPDATE te_tables_relation ";
	sql += " SET external_table_name = '"+ newTablename +"'";
	sql += " WHERE external_table_name = '"+ oldTableName +"'";
	execute(sql);
	
	alterTableInfoInMemory(newTablename, oldTableName);
	return true;
}

bool 
TeDatabase::insertBlob (const string& tableName, const string& columnBlob, TeAttributeRep& columnId, const string& valueId, unsigned char* data, int size)
{
	string whereClause = columnId.name_ +" = ";
	switch (columnId.type_ )
	{
		case TeSTRING:
			whereClause += "'"+ valueId + "'";
			break;
		default:
			whereClause += valueId;
			break;
	}
	return (insertBlob (tableName, columnBlob, whereClause, data, size));
}

bool 
TeDatabase::insertBlob (const string& tableName, const string& columnBlob, TeAttributeRep& columnId, const string& valueId, const string& fileName)
{
	string whereClause = columnId.name_ +" = ";
	switch (columnId.type_ )
	{
		case TeSTRING:
			whereClause += "'"+ valueId + "'";
			break;
		default:
			whereClause += valueId;
			break;
	}
	return (insertBlob(tableName, columnBlob, whereClause, fileName));
}

bool 
TeDatabase::insertBlob (const string& tableName, const string& columnBlob, const string& whereClause, const string& fileName)
{
	unsigned char	*cdata = 0;
	int		size;
	FILE	*fp = 0;
	
	struct	stat buf;
	int		result;
	
	result = stat(fileName.c_str(), &buf);
	
	if( result != 0 )
		return false;
	
	size = buf.st_size;

	cdata = new unsigned char[size];
	fp = fopen(fileName.c_str(), "rb");
	fread(cdata, sizeof(unsigned char), size, fp); 

	bool status = insertBlob (tableName, columnBlob, whereClause, cdata, size);

	if(fp)
		fclose(fp);

	if (cdata)
		delete []cdata;

	return status;
}

bool 
TeDatabase::getAttrTables(TeAttrTableVector& atts, TeAttrTableType attType)
{
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
	{
		this->errorMessage_ = "N�o foi poss�vel abrir portal para o banco";
		return false;
	}
	
	// Get layer tables	
	string get =  " SELECT * FROM te_layer_table"; 
	if (attType != TeAllAttrTypes)
		get += " WHERE attr_table_type = " + Te2String(attType);
	if (!portal->query(get))
	{	
		delete portal;
		return false;
	}

	while (portal->fetchRow())
	{
		TeTable attTable;
		if(!portal->getAttrTable(attTable))
		{
			delete portal;
			return false;
		}
		
		TeAttributeList attrList;
		getAttributeList(attTable.name(), attrList);
		atts.push_back(attTable);
	}
	delete portal;
	return (atts.size() > 0);
}

bool
TeDatabase::updateTable	(TeTable &table)
{
	TeAttributeList& att = table.attributeList();
	unsigned int i;
	string uniqueVal;
	bool isUniqueValString = false;
	
	if (!beginTransaction())
		return false;

	string uniqueName = table.uniqueName(); // primary key explicitly defined or 
	if (table.uniqueName().empty())			// check in the attribute list
	{								
		for (i=0; i<att.size(); ++i)
			if (att[i].rep_.isPrimaryKey_)
			{
				uniqueName = att[i].rep_.name_;
				table.setUniqueName(uniqueName);
				break;
			}
	}

	int blobIndex = -1;
	TeAttributeList::iterator it;
	TeTableRow row;
	unsigned int j;
	for (i = 0; i < table.size(); i++  )
	{
		row = table[i];
		it = att.begin();
		string q;
		j = 1;
		int jj = 0;
		while ( it != att.end() )
		{
			if (uniqueName != (*it).rep_.name_)
			{
				if ((*it).rep_.isAutoNumber_)
				{
					++it;
					j++;
					jj++;
					continue;
				}			

				if ((*it).rep_.type_ != TeBLOB)
				{
					if (q.empty()==false)
						q+= ",";
					q += (*it).rep_.name_ + "=";
				}
				
  				switch ((*it).rep_.type_)
  				{
  					case TeSTRING:						
						q += "'"+escapeSequence(row[jj])+"'";
  					break;
  					case TeREAL:
					{
						std::string value = row[jj];
						replace(value.begin(), value.end(), ',', '.');
						q += value;
					}
  					break;
  					case TeINT:
						q += row[jj];
  					break;
					case TeDATETIME:
					{
						const string temp_dt = string(row[jj].c_str());
						TeTime t(temp_dt, (*it).dateChronon_, (*it).dateTimeFormat_, (*it).dateSeparator_, (*it).timeSeparator_, (*it).indicatorPM_);
						q += this->getSQLTime(t);
					}
  					break;
					case TeCHARACTER:
						q += "'" + escapeSequence(row[jj]) + "'";
  					break;
					case TeBLOB:
						blobIndex = jj;
  					break;
  					default:
						q += "'"+escapeSequence(row[jj])+"'";
  					break;
  				}
				
			}
			else
			{
				uniqueVal = row[jj];
				isUniqueValString = ((*it).rep_.type_ == TeSTRING);
			}
			++it;
			j++;
			jj++;
		}
		if (q.empty())
			continue;
		
		if (!uniqueName.empty() && !uniqueVal.empty())  
		{
			if(isUniqueValString)
				q += " WHERE " + uniqueName + " = '" + uniqueVal +"'";
			else
				q += " WHERE " + uniqueName + " = " + uniqueVal;
		}
		string sql = "UPDATE "+ table.name() + " SET " + q;
		if (!execute(sql))
		{
			rollbackTransaction();
			return false;
		}

		//verify if there was blob type
		if(blobIndex>=0)
		{
			TeAttribute uniqueAttr;
			table.attrUnique(uniqueAttr);
			if(!insertBlob (table.name(), att[blobIndex].rep_.name_, uniqueAttr.rep_, uniqueVal, 
				(unsigned char*)row[blobIndex].c_str(), row[blobIndex].size()))
			{
				rollbackTransaction();
				return false;
			}
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}


bool 
TeDatabase::loadTable(const string& tableName, TeTable &table)
{
	bool isreg = false;
	table.name(tableName);
	if (loadTableInfo(table)) // this is not a registered table...
		isreg = true;

	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	string q ="SELECT * FROM " + tableName;
	if (!portal->query(q))
	{	
		delete portal;
		return false;
	}
	if (!isreg)
		table.setAttributeList(portal->getAttributeList());
	while (portal->fetchRow())
	{
		TeTableRow row;
		for (int j = 0; j < portal->numFields(); j++)
		{
           TeAttribute& attr = table.attributeList()[j];
           if (attr.rep_.type_ == TeBLOB)
           {
               unsigned char* data = NULL;
               long size = 0;
               portal->getBlob (attr.rep_.name_, data, size);
               string blobValue;
               if (data != NULL)
               {                                      
				   blobValue.assign((char*)data, size);
				   delete [] data;
                   data = NULL;
               }
               row.push_back(blobValue);
           }
           else
               row.push_back (portal->getData (j)); 
		}
		table.add(row);
	}
	delete portal;
	return true;
}

bool 
TeDatabase::selectTable (const string& tableName, const string& criteria, TeTable &table)
{
	bool isreg = false;
	table.name(tableName);
	if (loadTableInfo(table)) // this is not a registered table...
		isreg = true;

	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	string q ="SELECT * FROM " + tableName;
	if (!criteria.empty())
		q += " WHERE " + criteria;

	if (!portal->query(q))
	{	
		delete portal;
		return false;
	}
	if (!isreg)
		table.setAttributeList(portal->getAttributeList());
	while (portal->fetchRow())
	{
		TeTableRow row;
		for(int i = 0; i < portal->numFields(); i++)
			row.push_back(portal->getData(i));
		table.add(row);
	}
	delete portal;
	return true;
}

bool 
TeDatabase::updateView (TeView *view)
{
	TeProjection* proj = view->projection();
	if (proj)
	{
		if (proj->id() <= 0)
			this->insertProjection(view->projection());
		else
			this->updateProjection(view->projection());
	}
	else
	{
		errorMessage_ = "N�o � poss�vel atualizar vista sem proje��o!";
		return false;
	}

	string sql = "UPDATE te_view SET projection_id=" + Te2String(proj->id());
	sql+= ", name='" + view->name() + "'";
	sql+= ", user_name='" + view->user() + "'";
	sql+= ", visibility= " + Te2String((int)view->isVisible());
	sql+= ", lower_x = " + Te2String(view->getCurrentBox().lowerLeft().x(),15);
	sql+= ", lower_y = " + Te2String(view->getCurrentBox().lowerLeft().y(),15);
	sql+= ", upper_x = " + Te2String(view->getCurrentBox().upperRight().x(),15);
	sql+= ", upper_y = " + Te2String(view->getCurrentBox().upperRight().y(),15);
	if(view->getCurrentTheme() > 0)
		sql+= ", current_theme = " + Te2String(view->getCurrentTheme());
	else
		sql+= ", current_theme = null";
	sql +=" WHERE view_id = " + Te2String(view->id());

	if (!this->execute (sql))
		return false;

	TeViewTree* tree = view->root();
	if (tree) 
		return	updateViewTree(tree);
	return true;

}


bool 
TeDatabase::loadViewSet (const string& user, const bool& loadAttrList, const string& visualType)
{
	//clear view map
	TeViewMap::iterator it = metaModel_->viewMap().begin();
	while (it != metaModel_->viewMap().end())
	{
		if(it->second)
			delete it->second;
		++it;
	}
	metaModel_->viewMap().clear();

	//clear theme map
	TeThemeMap::iterator itTheme = metaModel_->themeMap().begin();
	while (itTheme != metaModel_->themeMap().end())
	{
		if(itTheme->second)
			delete itTheme->second;
		++itTheme;
	}

	//clear invalid theme map
	itTheme = metaModel_->invalidThemeMap().begin();
	while (itTheme != metaModel_->invalidThemeMap().end())
	{
		if(itTheme->second)
			delete itTheme->second;
		++itTheme;
	}
	metaModel_->invalidThemeMap().clear();
	metaModel_->themeMap().clear();
	metaModel_->legendMap().clear();
	
	//load view, projection, themes and grouping
	string sql = " SELECT ";
	sql += " te_view.*, "; //0-9  (10 columns)
	sql += " te_projection.*, "; //10 - 26 (17 columns)
	sql += " te_theme.*, "; // 27 - 45 (19 columns)
	sql += " te_grouping.*, "; //46 - 55 (10 columns)
	sql += " te_legend.*,  "; // 56 - 62    (7 columns)
	sql += " te_visual.*, "; //  63 - 86 (24 columns)
	sql += " te_visual_raster.* "; //  87 (7 columns)

	sql += " FROM ((((((te_view INNER JOIN te_projection ";
	sql += " ON te_view.projection_id = te_projection.projection_id) ";
	sql += " LEFT JOIN te_theme ON te_view.view_id = te_theme.view_id ) ";
	sql += " LEFT JOIN te_grouping ON te_theme.theme_id = te_grouping.theme_id) ";
	sql += " LEFT JOIN te_legend ON te_theme.theme_id = te_legend.theme_id) ";
	sql += " LEFT JOIN te_visual ON te_visual.legend_id = te_legend.legend_id) ";
	sql += " LEFT JOIN te_visual_raster ON te_theme.theme_id = te_visual_raster.theme_id) ";
			
	sql += " WHERE ";
	if (!user.empty())
		sql += " te_view.user_name = '" + user + "'";
	else
		sql += " te_view.user_name = '" + this->user() + "'";
	sql += " ORDER BY te_view.view_id, te_theme.priority, te_theme.theme_id, te_legend.legend_id, ";
	sql += " te_legend.group_id, te_visual.geom_type, te_visual_raster.band_in  "; 

	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;
	
	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}
	
	int lastViewId = -1;
	TeView *view = 0;
	bool hasNewRow = portal->fetchRow(); //idicates that this portal was fetched 
	while(hasNewRow)
	{
		//load view and its projection 
		if(lastViewId!=atoi(portal->getData(0)))
		{
			//store the last view
			if(view)
			{
				metaModel_->viewMap()[view->id()] = view;
				view = 0;
			}
			TeProjection* proj = 0;
			if(!portal->getProjection(&proj, 10)) //load projection
			{
				delete portal;
				return false;
			}
			view = new TeView();
			if(!portal->getView(*view, 0)) //load view
			{
				delete portal;
				delete view;
				return false;
			}
			if (proj != 0)
				view->projection(proj);
			lastViewId = view->id();
		}  
		
		//make the rigth object
		string aux = portal->getData(27);
		if (aux.empty())
		{
			hasNewRow = portal->fetchRow();
			continue;
		}
		TeViewNodeType viewNodeType = (TeViewNodeType)portal->getInt(33);
		TeViewNode* viewNode = TeViewNodeFactory::make(viewNodeType);

		if(!viewNode)
		{
			int currentThemeId = portal->getInt(27);

			while((hasNewRow = portal->fetchRow()) && (portal->getInt(27) == currentThemeId))
				;

			continue;
		}

		if(viewNodeType == TeTREE)
		{
			viewNode = loadViewTree(view, portal->getInt(27), loadAttrList, visualType); 
			view->add(viewNode);

			hasNewRow = portal->fetchRow();
			continue;
		}
		else 
		{
			if(!portal->getTheme(static_cast<TeAbstractTheme&>(*viewNode), 27))
			{
				delete viewNode;
				delete portal;
				return false;
			}

			if(viewNodeType == TeTHEME)
			{
				//load layer
				int id = static_cast<TeTheme*>(viewNode)->layerId();
				TeLayerMap::iterator it = metaModel_->layerMap().find(id);
				if (it == metaModel_->layerMap().end())
					loadLayerSet(loadAttrList);
				
				static_cast<TeTheme*>(viewNode)->layer(metaModel_->layerMap()[id]);
			}
		}
		
		TeAbstractTheme* theme = static_cast<TeAbstractTheme*>(viewNode);

		//load grouping 
		TeGrouping group;
		if(portal->getGrouping(group, 46))
			theme->grouping(group);

		//load all legends of this theme
		//and its visual
		bool hasLegsToThisTheme = true;
		while(hasLegsToThisTheme)
		{
			//legend
			TeLegendEntry legend;
			if(!portal->getLegend(legend, 56))
			{
				delete theme;
				delete view;
				delete portal;
				return false;
			}

			//visual
			TeRasterVisual* rasterVisual = new TeRasterVisual();
			bool hasVisualToThisLeg = true;
			bool hasRasterVisual = false;
			while(hasVisualToThisLeg)
			{
				TeVisual* visual = TeVisualFactory::make(visualType);
				TeGeomRep geomRep;
				if(portal->getVisual(visual, geomRep, 63))
					legend.setVisual(visual, geomRep);
									
				if(portal->getRasterVisual(*rasterVisual, 87))
					hasRasterVisual=true;
				
				hasNewRow = portal->fetchRow();
				if(!hasNewRow || portal->getInt(58)!= legend.group() || portal->getInt(56)!= legend.id())
					hasVisualToThisLeg = false;
			}

			//Set raster visual to this theme
			if(hasRasterVisual)
				theme->rasterVisual(rasterVisual);
			else
				delete rasterVisual;

			//Set legend to this theme
			theme->legend(legend); 
															
			//fill legend buffer
			if(legend.group() == -6)	
				metaModel_->legendMap()[legend.id()] = &theme->queryAndPointingLegend();
			else if(legend.group() == -5)	
				metaModel_->legendMap()[legend.id()] = &theme->queryLegend(); 
			else if (legend.group() == -4)
				metaModel_->legendMap()[legend.id()] = &theme->pointingLegend(); 
			else if (legend.group() == -3)
				metaModel_->legendMap()[legend.id()] = &theme->defaultLegend(); 
			else if (legend.group() == -2)
				metaModel_->legendMap()[legend.id()] = &theme->withoutDataConnectionLegend(); 
			else if (legend.group() == -1)
				metaModel_->legendMap()[legend.id()] = &theme->outOfCollectionLegend(); 
			else if (legend.group() == -10) //own legend
			{
				TeLegendEntry* legendTemp = new TeLegendEntry(legend);
				metaModel_->legendMap()[legend.id()] = legendTemp;
			}				
			
			if(!hasNewRow || portal->getInt(27)!= theme->id())
				hasLegsToThisTheme = false;
		}
			
		for (unsigned int i = 0; i < theme->legend().size(); ++i)
			metaModel_->legendMap()[theme->legend()[i].id()] = &theme->legend()[i];

		if(viewNode->type()==(int)TeTHEME)
		{
			//load theme table
			if(!loadThemeTable(static_cast<TeTheme*>(theme), loadAttrList))
			{
				delete portal;
				return false;
			}
		}
		
		//load specific theme parameters
		if(!theme->loadMetadata(this))
		{
			metaModel_->invalidThemeMap()[viewNode->id()] = theme;
			continue;
		}

		metaModel_->themeMap()[viewNode->id()] = theme;
		view->add(viewNode);
	}

	//store the last view
	if(view)
	{
		metaModel_->viewMap()[view->id()] = view;
		view = 0;
	}

	delete portal;
	return true;
	
}

TeViewTree* 
TeDatabase::loadViewTree(TeView* view, int id, const bool& loadAttrList, const string& visualType)
{
	if( view == 0) 
		return 0;
	
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return 0;

	string q;
	TeViewTree *node = 0;

	if (id != 0)
	{
		q = "SELECT * FROM te_theme";
		q += " WHERE view_id = " + Te2String (view->id());
		q += " AND theme_id = " + Te2String(id);

		if (!portal->query(q) || !portal->fetchRow())
		{
			delete portal;
			return 0;
		}
		TeViewNodeType type = (TeViewNodeType)portal->getInt("node_type");
		if(type != TeTREE)
		{
			portal->freeResult();
			delete portal;
			return NULL;
		}
		node = portal->getViewTree();
		portal->freeResult();
	}

	q ="SELECT * FROM te_theme";
	q += " WHERE view_id = " + Te2String (view->id());
	q += " AND parent_id = " + Te2String(id);
	q += " ORDER BY priority ASC";

	if (!portal->query(q))
	{
		delete portal;
		return node;
	}

	while (portal->fetchRow())
	{
		TeViewNodeType childType = (TeViewNodeType)portal->getInt("node_type");
		TeViewNode *childNode;
		if (childType == TeTHEME)
		{
			childNode = new TeTheme();
			childNode->id(portal->getInt(0));
			this->loadTheme((TeTheme*)childNode, loadAttrList, visualType);
		} 
		else
		{
			childNode = loadViewTree(view, portal->getInt("theme_id"), loadAttrList, visualType);
		}

		if(id == 0)
		{
			view->add(childNode);
		} 
		else
		{
//			view->addTheme(childNode);
			node->add(childNode);
		}
	}
	delete portal;
	return node;
}

bool 
TeDatabase::loadView (TeView* view, const bool& loadAttrList, const string& visualType)
{
	string rest;
	if (view->id() > 0)
		rest = " te_view.view_id=" + Te2String(view->id());
	else if (!view->name().empty())
	{
		rest = " te_view.name='" + view->name() + "'";
		
		if(!view->user().empty())
			rest += " AND te_view.user_name='" + view->user() + "'"; 
	}
	else
		return false;
	
	//load view, projection, themes and grouping
	string sql = " SELECT ";
	sql += " te_view.*, "; //0-9  (10 columns)
	sql += " te_projection.*, "; //10 - 26 (17 columns)
	sql += " te_theme.*, "; // 27 - 45 (19 columns)
	sql += " te_grouping.*, "; //46 - 55 (10 columns)
	sql += " te_legend.*,  "; // 56 - 62    (7 columns)
	sql += " te_visual.*, "; //  63 - 86 (24 columns)
	sql += " te_visual_raster.* "; //  87 (7 columns)

	sql += " FROM ((((((te_view INNER JOIN te_projection ";
	sql += " ON te_view.projection_id = te_projection.projection_id) ";
	sql += " LEFT JOIN te_theme ON te_view.view_id = te_theme.view_id ) ";
	sql += " LEFT JOIN te_grouping ON te_theme.theme_id = te_grouping.theme_id) ";
	sql += " LEFT JOIN te_legend ON te_theme.theme_id = te_legend.theme_id) ";
	sql += " LEFT JOIN te_visual ON te_visual.legend_id = te_legend.legend_id) ";
	sql += " LEFT JOIN te_visual_raster ON te_theme.theme_id = te_visual_raster.theme_id) ";
			
	sql += " WHERE "+ rest;
	sql += " ORDER BY te_view.view_id, te_theme.priority, te_theme.theme_id, te_legend.legend_id, ";
	sql += " te_legend.group_id, te_visual.geom_type, te_visual_raster.band_in  "; 

	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;
	
	if (!portal->query(sql) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	//load projection
	TeProjection* proj = 0;
	if(!portal->getProjection(&proj, 10)) 
	{
		delete portal;
		return false;
	}
	//load view		
	if(!portal->getView(*view, 0)) 
	{
		delete portal;
		return false;
	}
	if (proj != 0)
		view->projection(proj);
	
	bool hasNewRow = true;

	while(hasNewRow)
	{
		string aux = portal->getData(27);
		if (aux.empty())
		{
			hasNewRow = portal->fetchRow();
			continue;
		}
		TeViewNodeType viewNodeType = (TeViewNodeType)portal->getInt(33);
		//make the rigth object
		TeViewNode* viewNode = TeViewNodeFactory::make(viewNodeType);

		if(!viewNode)
		{
			int currentThemeId = portal->getInt(27);

			while((hasNewRow = portal->fetchRow()) && (portal->getInt(27) == currentThemeId))
				;

			continue;
		}

		if(viewNodeType == TeTREE)
		{
			viewNode = loadViewTree(view, portal->getInt(27), loadAttrList, visualType); 
			view->add(viewNode);
			hasNewRow = portal->fetchRow();
			continue;
		}
		else 
		{
			if(!portal->getTheme(static_cast<TeAbstractTheme&>(*viewNode), 27))
			{
				delete viewNode;
				delete portal;
				return false;
			}

			if(viewNodeType == TeTHEME)
			{
				//load layer
				int id = static_cast<TeTheme*>(viewNode)->layerId();
				TeLayerMap::iterator it = metaModel_->layerMap().find(id);
				if (it == metaModel_->layerMap().end())
					loadLayerSet(loadAttrList);
				
				static_cast<TeTheme*>(viewNode)->layer(metaModel_->layerMap()[id]);
			}
		}
		
		TeAbstractTheme* abstractTheme = static_cast<TeAbstractTheme*>(viewNode);

		//load grouping 
		TeGrouping group;
		if(portal->getGrouping(group, 46))
			abstractTheme->grouping(group);

		//load all legends of this theme
		//and its visual
		bool hasLegsToThisTheme = true;
		while(hasLegsToThisTheme)
		{
			//legend
			TeLegendEntry legend;
			if(!portal->getLegend(legend, 56))
			{
				delete viewNode;
				delete portal;
				return false;
			}

			//visual
			TeRasterVisual* rasterVisual = new TeRasterVisual();
			bool hasVisualToThisLeg = true;
			bool hasRasterVisual = false;
			while(hasVisualToThisLeg)
			{
				TeVisual* visual = TeVisualFactory::make(visualType);
				TeGeomRep geomRep;
				if(portal->getVisual(visual, geomRep, 63))
					legend.setVisual(visual, geomRep);
									
				if(portal->getRasterVisual(*rasterVisual, 87))
					hasRasterVisual=true;
				
				hasNewRow = portal->fetchRow();
				if(!hasNewRow || portal->getInt(58)!= legend.group() || portal->getInt(56)!= legend.id())
					hasVisualToThisLeg = false;
			}

			//Set raster visual to this theme
			if(hasRasterVisual)
				abstractTheme->rasterVisual(rasterVisual);
			else
				delete rasterVisual;

			//Set legend to this theme
			abstractTheme->legend(legend); 
															
			//fill legend buffer
			if(legend.group() == -6)	
				metaModel_->legendMap()[legend.id()] = & abstractTheme->queryAndPointingLegend();
			else if(legend.group() == -5)	
				metaModel_->legendMap()[legend.id()] = & abstractTheme->queryLegend(); 
			else if (legend.group() == -4)
				metaModel_->legendMap()[legend.id()] = & abstractTheme->pointingLegend(); 
			else if (legend.group() == -3)
				metaModel_->legendMap()[legend.id()] = & abstractTheme->defaultLegend(); 
			else if (legend.group() == -2)
				metaModel_->legendMap()[legend.id()] = & abstractTheme->withoutDataConnectionLegend(); 
			else if (legend.group() == -1)
				metaModel_->legendMap()[legend.id()] = & abstractTheme->outOfCollectionLegend(); 
			else if (legend.group() == -10) //own legend
			{
				TeLegendEntry* legendTemp = new TeLegendEntry(legend);
				metaModel_->legendMap()[legend.id()] = legendTemp;
			}	
			
			if(!hasNewRow || portal->getInt(27)!= abstractTheme->id())
				hasLegsToThisTheme = false;
		}				
			
		for (unsigned int i = 0; i < abstractTheme->legend().size(); ++i)
			metaModel_->legendMap()[abstractTheme->legend()[i].id()] = & abstractTheme->legend()[i];
		
		if(viewNode->type()==(int)TeTHEME)
		{
			//load theme table
			if(!loadThemeTable(static_cast<TeTheme*>(abstractTheme), loadAttrList))
			{
				delete portal;
				return false;
			}
		}
		
		//load specific theme parameters
		if(!abstractTheme->loadMetadata(this))
		{
			metaModel_->invalidThemeMap()[viewNode->id()] = abstractTheme;
			continue;
		}
		
		metaModel_->themeMap()[viewNode->id()] = abstractTheme;
		view->add(viewNode);
	}

	metaModel_->viewMap()[view->id()] = view;
	delete portal;
	return true;
}


void
TeDatabase::clear()
{
	metaModel_->clear();
}

bool 
TeDatabase::deleteView (int viewId)
{
	TeDatabasePortal* portal = this->getPortal();

	// view projection should be deleted manually
	string sql =  "SELECT projection_id FROM te_view WHERE view_id=" + Te2String(viewId);
	portal->freeResult();
	if (!portal->query(sql))
	{
		delete portal;
		return false;
	}
	if (!portal->fetchRow())
	{
		delete portal;
		return false;
	}
	string projId = portal->getData("projection_id");
	portal->freeResult();
	
	// delete themes belonging to this view 
	sql = "SELECT theme_id FROM te_theme WHERE view_id=" + Te2String(viewId);
	if (!portal->query(sql))
	{
		delete portal;
		return false;
	}
	while (portal->fetchRow())
	{
		int id = atoi(portal->getData(0));
		if(deleteTheme(id) == false)
		{	
			delete portal;
			return false;
		}
	}

	//delete the entries in the project relation
	if (existRelation("te_project_view","fk_projectview_view_id") != TeRICascadeDeletion)
	{
		sql = "DELETE FROM te_project_view WHERE view_id =" + Te2String(viewId);
		if (!this->execute (sql))
			return false;
	}

	// delete view
	sql = "DELETE FROM te_view WHERE view_id = " + Te2String(viewId);
	if (!this->execute (sql))
	{
		delete portal;
		return false;
	}

	sql = "DELETE FROM te_projection WHERE  projection_id = " + projId;
	if (!this->execute (sql))
	{
		delete portal;
		return false;
	}

	// Delete the view and its themes
	TeView* view = metaModel_->viewMap()[viewId];
	metaModel_->viewMap().erase(viewId);
	delete view;
	delete portal;
	return true;
}

bool
TeDatabase::updateViewTree (TeViewTree *tree)
{
	if(tree ->type() == 1) //tree->id() > 0) //only for TeTREE
	{
		string sql;
		sql = "UPDATE te_theme SET ";
		sql += "name='" + tree->name()+"'";
		sql += ", parent_id=" + Te2String (tree->parentId());
		sql += ", node_type=" + Te2String (tree->type());
		sql += " ,priority=" + Te2String (tree->priority());
		sql += " WHERE theme_id = " + Te2String(tree->id());

		if(!this->execute (sql)) return false;
	}

	for (unsigned int th=0; th<tree->size(); th++)
	{
		TeViewNode* node = tree->retrieve(th);
		if (node->type() == TeTREE)
		{
			TeViewTree* tree = (TeViewTree*)node;
			if(!updateViewTree(tree)) return false;
		}
		else
		{
			TeTheme *theme = (TeTheme*)node;
			if(!updateTheme(theme)) return false;
		}
	}
	return true;
}

bool 
TeDatabase::viewExist(string viewName)
{
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	viewName = TeConvertToUpperCase(viewName);

	string sql = "SELECT name FROM te_view";
	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}
	while (portal->fetchRow())
	{
		string name = portal->getData(0);
		name = TeConvertToUpperCase(name);
		if (viewName == name)
		{
			delete portal;
			return true;
		}
	}
	delete portal;
	return false;
}


bool
TeDatabase::updateTheme (TeAbstractTheme *theme)
{
	string sql;

	if (theme->id() <= 0 )  // theme doesn�t exist in the database yet
	{
		return this->insertTheme(theme);
	}
	
	// update theme metadata
	sql = "UPDATE te_theme SET ";
	
	if(theme->type()==TeTHEME)
	{
		sql += " layer_id=" + Te2String (static_cast<TeTheme*>(theme)->layerId());
		sql += ", ";
	}
	
	sql += "  view_id=" + Te2String (theme->view());
	sql += ", name='" + escapeSequence(theme->name())+"'";
	sql += ", parent_id=" + Te2String (theme->parentId());
	sql += ", priority=" + Te2String (theme->priority());
	sql += ", node_type=" + Te2String (theme->type());
	sql += ", min_scale=" + Te2String (theme->minScale(),15);
	sql += ", max_scale=" + Te2String (theme->maxScale(),15);
	sql += ", generate_attribute_where='" + escapeSequence(theme->attributeRest())+"'";
	sql += ", generate_spatial_where='" + escapeSequence(theme->spatialRest())+"'";
	sql += ", generate_temporal_where='" + escapeSequence(theme->temporalRest())+"'";

	if(theme->type()==TeTHEME || theme->type()==TeEXTERNALTHEME )
		sql += ", collection_table='" + static_cast<TeTheme*>(theme)->collectionTable() + "'";

	sql += ", visible_rep= " + Te2String(theme->visibleRep ());
	sql += ", enable_visibility= " + Te2String(theme->visibility()); 
	sql += ", lower_x = " + Te2String(theme->box().x1(), 15); 
	sql += ", lower_y = " + Te2String(theme->box().y1(), 15); 
	sql += ", upper_x = " + Te2String(theme->box().x2(), 15); 
	sql += ", upper_y = " + Te2String(theme->box().y2(), 15); 
	sql += " WHERE theme_id=" + Te2String (theme->id(), 15);

	if (!this->execute (sql))
		return false;
         
	//delete grouping 
	sql = "DELETE FROM te_grouping WHERE theme_id= "+ Te2String(theme->id());
	this->execute (sql);
		
	if(theme->grouping().groupMode_ != TeNoGrouping)
	{
		if(!insertGrouping(theme->id(), theme->grouping()))
			return false;
	}
	
	// update each of its legends
	bool status = true;
	
	if(theme->legend().size() == 0)
	{
		if(!deleteLegend(theme->id()))
			return false;
	}
	else
	{
		status = updateLegend(theme->legend());
		if (!status)
			return status;
	}

	status = updateLegend(&(theme->withoutDataConnectionLegend()));
	if (!status)
		return status;

	status = updateLegend(&(theme->outOfCollectionLegend()));
	if (!status)
		return status;

	status = updateLegend(&(theme->defaultLegend()));
	if (!status)
		return status;

	status = updateLegend(&(theme->pointingLegend()));
	if (!status)
		return status;

	status = updateLegend(&(theme->queryLegend()));
	if (!status)
		return status;

	status = updateLegend(&(theme->queryAndPointingLegend()));
	if (!status)
		return status;
	
	//insert metadata theme
	if(!theme->saveMetadata(this))
		return false;

	// theme tables
	if(theme->type()==TeTHEME && !updateThemeTable(static_cast<TeTheme*>(theme)))
		return false;

	return true;
}

bool
TeDatabase::loadTheme (TeAbstractTheme* theme, const bool& loadAttrList, const string& visualType)
{
	if (theme == 0)
		return false;

	string rest;
	if (theme->id() > 0)
		rest = " te_theme.theme_id = "+  Te2String(theme->id());
	else if (!theme->name().empty())
		rest = " te_theme.name = '"+  theme->name() + "'";
	else
	{
		this->errorMessage_ = "Theme procurado n�o possui nem id nem nome";
		return false;
	}
	rest += " AND te_view.user_name = \'" + this->user() +"\'";
	
	//load view, projection, themes and grouping
	string sql = " SELECT ";
	sql += " te_theme.*, "; // 0 - 18 (19 columns)   
	sql += " te_grouping.*, "; //19 - 28 (10 columns)
	sql += " te_legend.*,  "; // 29 - 35    (7 columns)
	sql += " te_visual.*, "; //  36 - 59 (24 columns)
	sql += " te_visual_raster.* "; //  60 (7 columns)
	sql += " FROM (((((te_view INNER JOIN te_theme ON te_view.view_id = te_theme.view_id) ";
	sql += " LEFT JOIN te_grouping ON te_theme.theme_id = te_grouping.theme_id ) ";
	sql += " LEFT JOIN te_legend ON te_theme.theme_id = te_legend.theme_id ) ";
	sql += " LEFT JOIN te_visual ON te_legend.legend_id = te_visual.legend_id ) ";
	sql += " LEFT JOIN te_visual_raster ON te_theme.theme_id = te_visual_raster.theme_id) ";
	sql += " WHERE "+ rest;
	sql += " ORDER BY te_theme.theme_id, te_legend.legend_id, te_legend.group_id, te_visual.geom_type, te_visual_raster.band_in  "; 

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;
	
	if(!portal->query(sql) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	
	TeViewNodeType viewNodeType = (TeViewNodeType)portal->getInt(6);
	if(viewNodeType == TeTREE)
	{		
		TeViewNodeParams params;
		portal->getViewNodeParams(params, 0);
		theme->viewNodeParams(params);
		delete portal;
		return true;
	}
			
	if(!portal->getTheme(*theme, 0))
	{
		delete portal;
		return false;
	}
	
	if(viewNodeType==(int)TeTHEME)
	{
		//load layer
		int id = static_cast<TeTheme*>(theme)->layerId();
		TeLayerMap::iterator it = metaModel_->layerMap().find(id);
		if (it == metaModel_->layerMap().end())
			loadLayerSet(loadAttrList);
		
		static_cast<TeTheme*>(theme)->layer(metaModel_->layerMap()[id]);
	}
		
	//load grouping 
	TeGrouping group;
	if(portal->getGrouping(group, 19))
		theme->grouping(group);

	//load all legends of this theme
	//and its visual
	bool hasLegsToThisTheme = true;
	bool hasNewRow = true;
	while(hasLegsToThisTheme)
	{
		//legend
		TeLegendEntry legend;
		if(!portal->getLegend(legend, 29))
		{
			delete portal;
			return false;
		}

		//visual
		TeRasterVisual* rasterVisual = new TeRasterVisual();
		bool hasVisualToThisLeg = true;
		bool hasRasterVisual = false;
		while(hasVisualToThisLeg)
		{
			TeVisual* visual = TeVisualFactory::make(visualType);
			TeGeomRep geomRep;
			if(portal->getVisual(visual, geomRep, 36))
				legend.setVisual(visual, geomRep);
								
			if(portal->getRasterVisual(*rasterVisual, 60))
				hasRasterVisual=true;
			
			hasNewRow = portal->fetchRow();
			if(!hasNewRow || portal->getInt(31)!= legend.group() || portal->getInt(29)!= legend.id() )  //if legend_id and group_id
				hasVisualToThisLeg = false;
		}

		//Set raster visual to this theme
		if(hasRasterVisual)
			theme->rasterVisual(rasterVisual);
		else
			delete rasterVisual;

		//Set legend to this theme
		theme->legend(legend); 
														
		//fill legend buffer
		if(legend.group() == -6)	
			metaModel_->legendMap()[legend.id()] = &theme->queryAndPointingLegend();
		else if(legend.group() == -5)	
			metaModel_->legendMap()[legend.id()] = &theme->queryLegend(); 
		else if (legend.group() == -4)
			metaModel_->legendMap()[legend.id()] = &theme->pointingLegend(); 
		else if (legend.group() == -3)
			metaModel_->legendMap()[legend.id()] = &theme->defaultLegend(); 
		else if (legend.group() == -2)
			metaModel_->legendMap()[legend.id()] = &theme->withoutDataConnectionLegend(); 
		else if (legend.group() == -1)
			metaModel_->legendMap()[legend.id()] = &theme->outOfCollectionLegend(); 
		else if (legend.group() == -10) //own legend
		{
			TeLegendEntry* legendTemp = new TeLegendEntry(legend);
			metaModel_->legendMap()[legend.id()] = legendTemp;
		}	
		
		if(!hasNewRow || portal->getInt(0)!= theme->id())
			hasLegsToThisTheme = false;
	}

	delete portal;
			
	for (unsigned int i = 0; i < theme->legend().size(); ++i)
		metaModel_->legendMap()[theme->legend()[i].id()] = &theme->legend()[i];

	//load theme table
	if(theme->type()==(int)TeTHEME)
	{
		if(!loadThemeTable(static_cast<TeTheme*>(theme), loadAttrList))
			return false;
	}

	//load specific theme parameters
	if(!theme->loadMetadata(this))
	{
		metaModel_->invalidThemeMap()[theme->id()] = theme;
		delete portal;
		return false;
	}
			
	metaModel_->themeMap()[theme->id()] = theme;
	return true;
}


bool
TeDatabase::loadThemeTable (TeTheme* theme, const bool& loadAttrList)
{
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
	{
		this->errorMessage_ = "N�o foi poss�vel abrir portal para o banco";
		return false;
	}
	
	//theme tables
	string sel = "SELECT te_theme_table.*, ";  //0 - 4 (5 columnns)
	sel += " te_tables_relation.*, ";          //5 - 9  (5 columns)
	sel += " te_layer_table.* ";			   //10	
	sel += " FROM (te_theme_table LEFT JOIN te_tables_relation";
	sel += " ON te_theme_table.relation_id = te_tables_relation.relation_id)";
	sel += " LEFT JOIN te_layer_table ON te_theme_table.table_id = te_layer_table.table_id";
	sel += " WHERE  te_theme_table.theme_id = " + Te2String(theme->id());
	sel += " ORDER BY table_order";
	
	if (!portal->query (sel))
	{	
		delete portal;
		return false;
	}

	while(portal->fetchRow ())
	{
		string tableName = portal->getData(12);
		if(tableName.empty())
			continue;

		TeTable table(tableName);

		TeLayerMap::iterator itLayer = metaModel_->layerMap().find(theme->layerId());
		if(itLayer!=metaModel_->layerMap().end() && (!itLayer->second->getAttrTablesByName(tableName, table)))
		{
			TeAttributeList attrList;
			if(loadAttrList)
				getAttributeList(tableName, attrList);
			table.setAttributeList(attrList);
			if(!portal->getAttrTable(table, 10))
			{
				delete portal;
				return false;
			}
		}

		table.setId(portal->getInt(2)); //"te_theme_table.table_id"
		table.setOrder(portal->getInt(4)); //"table_order"
		TeAttrTableType tableType = table.tableType(); //portal->getInt("attr_table_type");
		if (tableType == TeAttrExternal)
		{
			int relatedTableId = portal->getInt(6); //"related_table_id"
			table.relatedTableName(getTableName(relatedTableId));
			int relationId = portal->getInt(3); //"te_theme_table.relation_id"
			metaModel_->relationMSet().insert(relationId);

			string relatedAttr = portal->getData(7); //"related_attr"
			table.setTableType(TeAttrExternal, relatedTableId, relatedAttr);

			table.setLinkName(portal->getData(9)); //"external_attr"
		}
		else
			table.setTableType((TeAttrTableType)tableType);

		theme->addThemeTable(table);
	}

	delete portal;
	return true;
}


bool
TeDatabase::insertThemeTable(TeTheme *theme, TeTable& inputTable)
{
	int themeId = theme->id();
	int	tableOrder = 0;
	int	relationId;
	bool status;
	string qString;

	// Get the order of the last theme table
	qString = "SELECT MAX(table_order) FROM te_theme_table";
	qString += " WHERE theme_id = " + Te2String(themeId);

	TeDatabasePortal* portal = getPortal();
	if (portal->query(qString) == false || portal->fetchRow() == false)
		return false;

	string data = portal->getData(0);
	if (data.empty())
		tableOrder = 0;
	else
	{
		tableOrder = atoi(portal->getData(0));
		++tableOrder;
	}
	inputTable.setOrder(tableOrder);
	delete portal;

	if (inputTable.tableType() == TeAttrExternal)
	{
		status = insertRelationInfo(inputTable.relatedTableId(),inputTable.relatedAttribute(),
									inputTable.name(),inputTable.linkName(),relationId);
		if (status == false)
			return false;
		metaModel_->relationMSet().insert(relationId);

		status = insertThemeTable(themeId, inputTable.id(), relationId, tableOrder);
	}
	else
	{
		// Insert the table in the te_theme_table
		status = insertThemeTable(themeId, inputTable.id(), 0, tableOrder);
	}
	return status;
}

bool
TeDatabase::removeThemeTable(TeTheme *theme, int tableOrder)
{
	if (tableOrder < 0)
		return false;

	int relationId = -1;
	string qString;

	TeAttrTableVector attrTableVector; 
	theme->getAttTables(attrTableVector);

	// If the table is external, find the relation id
	qString = "SELECT relation_id FROM te_theme_table";
	qString += " WHERE theme_id = " + Te2String(theme->id());
	qString += " AND relation_id IS NOT NULL";
	qString += " AND table_order = " + Te2String(tableOrder);

	TeDatabasePortal* portal = getPortal();
	if (portal->query(qString) && portal->fetchRow())
		relationId = portal->getInt("relation_id");
	else
	{
		delete portal;
		return false;
	}
	delete portal;

	// Remove the table from the te_theme_table
	qString = "DELETE FROM te_theme_table WHERE theme_id = " + Te2String(theme->id());
	qString += " AND table_order = " + Te2String(tableOrder);
	if (execute(qString) == false)
		return false;

	// Remove the relation from the te_tables_relation
	// table if only this theme is using it
	if (relationId > 0)
	{
		if (metaModel_->relationMSet().count(relationId) == 1)
		{
			qString = "DELETE FROM te_tables_relation WHERE relation_id = " + Te2String(relationId);
			if (execute(qString) == false)
				return false;
			metaModel_->relationMSet().erase(relationId);
		}
		else
			metaModel_->relationMSet().erase(metaModel_->relationMSet().find(relationId));
	}
	return true;
}


bool 
TeDatabase::updateThemeTable(TeTheme *theme)
{
	//Initially remove from te_theme_table all the records of this theme
	string q = "SELECT * FROM te_theme_table WHERE theme_id = " + Te2String(theme->id());
	TeDatabasePortal *portal = getPortal();

	if (portal->query(q) == false)
	{
		delete portal;
		return false;
	}

	while (portal->fetchRow())
	{
		int relationId;
		int themeTableId = portal->getInt("theme_table_id");

		string data = portal->getData("relation_id");
		if (data.empty())
			relationId = -1;
		else
			relationId = atoi(data.c_str());

		// Remove the relation from the te_tables_relation
		// table if only this theme is using it
		bool b = false;
		if (relationId > 0)
		{
			if (metaModel_->relationMSet().count(relationId) == 1)
				b = true;
			else
				metaModel_->relationMSet().erase(metaModel_->relationMSet().find(relationId));
		}

		// Remove the table from the te_theme_table
		q = "DELETE FROM te_theme_table WHERE theme_id = " + Te2String(theme->id());
		q += " AND theme_table_id = " + Te2String(themeTableId);
		if (execute(q) == false)
		{
			delete portal;
			return false;
		}
		if(b)
		{
			q = "DELETE FROM te_tables_relation WHERE relation_id = " + Te2String(relationId);
			if (execute(q) == false)
			{
				delete portal;
				return false;
			}
			metaModel_->relationMSet().erase(relationId);
		}
	}
	delete portal;

	// Update te_theme_table and te_tables_relation(if there are any external tables)
	// with the information provided from the new vector of theme tables
	TeAttrTableVector tablesVector; 
	theme->getAttTables(tablesVector);
	for (unsigned i = 0; i < tablesVector.size(); ++i)
		insertThemeTable(theme, tablesVector[i]);

	return true;
}


bool 
TeDatabase::insertGrouping (int themeId, const TeGrouping& grouping)
{
	if((themeId < 1) || (grouping.groupMode_ == TeNoGrouping) )
		return false;

	string ins = " INSERT INTO te_grouping (theme_id, grouping_number, ";
	ins += " grouping_attr, grouping_attr_type, grouping_mode, "; 
	ins += " grouping_norm_attr, grouping_std_dev, grouping_precision, "; 
	ins += " grouping_function, grouping_chronon)"; 
	ins += " VALUES ( ";
	ins += Te2String(themeId);
	ins += ", "+ Te2String(grouping.groupNumSlices_);

	string attname = grouping.groupAttribute_.name_;
	if(attname.empty() || (attname=="NONE") )
		attname = "";
	
	string norname = grouping.groupNormAttribute_;
	if(norname.empty() || (norname=="NONE"))
		norname = "";

	map<int, map<string, string> >::iterator it = metaModel_->mapThemeAlias().find(themeId);
	if(it != metaModel_->mapThemeAlias().end())
	{
		map<string, string>::iterator tit = it->second.find(attname);
		if(tit != it->second.end())
		{
			string alias = tit->second;
			attname += "(" + alias + ")";
		}
		if(norname.empty() == false)
		{
			map<string, string>::iterator tit = it->second.find(norname);
			if(tit != it->second.end())
			{
				string nalias = tit->second;
				norname += "(" + nalias + ")";
			}
		}
	}

	ins += ", '"+ attname +"'";
	ins += ", "+ Te2String(grouping.groupAttribute_.type_);
	ins += ", "+ Te2String(grouping.groupMode_);
	ins += ", '"+ norname +"'";
	ins += ", "+ Te2String(grouping.groupStdDev_); 
	ins += ", "+ Te2String(grouping.groupPrecision_);
	ins += ", '"+ grouping.groupFunction_ +"'";
	ins += ", "+ Te2String(grouping.groupChronon_);
	ins += ")";

	return (execute(ins));
}

bool 
TeDatabase::updateGrouping (int themeId, const TeGrouping& grouping)
{
	if((themeId < 1) || (grouping.groupMode_ == TeNoGrouping))
		return false;

	string up = " UPDATE te_grouping SET ";
	up += "  grouping_number = "+ Te2String(grouping.groupNumSlices_);

	string attname = grouping.groupAttribute_.name_;
	if(attname.empty() || (attname=="NONE"))
		attname = "";

	string norname = grouping.groupNormAttribute_;
	if(norname.empty()|| (norname=="NONE"))
		norname = "";

	map<int, map<string, string> >::iterator it = metaModel_->mapThemeAlias().find(themeId);
	if(it != metaModel_->mapThemeAlias().end())
	{
		map<string, string>::iterator tit = it->second.find(attname);
		if(tit != it->second.end())
		{
			string alias = tit->second;
			attname += "(" + alias + ")";
		}
		if(norname.empty() == false)
		{
			map<string, string>::iterator tit = it->second.find(norname);
			if(tit != it->second.end())
			{
				string nalias = tit->second;
				norname += "(" + nalias + ")";
			}
		}
	}

	up += ", grouping_attr = '"+ attname +"'";
	up += ", grouping_attr_type = "+ Te2String(grouping.groupAttribute_.type_);
	up += ", grouping_mode = "+ Te2String(grouping.groupMode_);
	up += ", grouping_norm_attr = '"+ norname +"'";
	up += ", grouping_std_dev = "+ Te2String(grouping.groupStdDev_); 
	up += ", grouping_precision = "+ Te2String(grouping.groupPrecision_);
	up += ", grouping_function = '"+ grouping.groupFunction_ +"'";
	up += ", grouping_chronon = "+ Te2String(grouping.groupChronon_);
	up += " WHERE theme_id = "+ Te2String(themeId);

	return (execute(up));
}

bool 
TeDatabase::generateLabelPositions(TeTheme *theme, const std::string& objectId)
{
	string	piebar, geomTable, upd;
	string	collTable = theme->collectionTable();
	
	if((collTable.empty()) || (!tableExist(collTable)))
		return false;

	if (theme->layer()->hasGeometry(TeCELLS) )
	{
		geomTable = theme->layer()->tableName(TeCELLS);

		piebar = "SELECT label_x, label_y, lower_x, lower_y, upper_x, upper_y";
		piebar += " FROM " + collTable + " LEFT JOIN " + geomTable;
		piebar += " ON " + collTable + ".c_object_id = " + geomTable + ".object_id";
		if (!objectId.empty())
			upd += " WHERE " +  collTable + ".c_object_id = '" + objectId + "'";

		upd = " UPDATE (" + piebar + ") SET";
		upd += " label_x = lower_x + (upper_x-lower_x)/2,";
		upd += " label_y = lower_y + (upper_y-lower_y)/2";
		

		if(!execute(upd))
			return false;
	}

	if( theme->layer()->hasGeometry(TePOLYGONS))
	{
		geomTable = theme->layer()->tableName(TePOLYGONS);

		piebar = "SELECT label_x, label_y, lower_x, lower_y, upper_x, upper_y";
		piebar += " FROM " + collTable + " LEFT JOIN " + geomTable;
		piebar += " ON " + collTable + ".c_object_id = " + geomTable + ".object_id";
		piebar += " WHERE label_x is null OR label_y is null";
		if (!objectId.empty())
			upd += " AND " +  collTable + ".c_object_id = '" + objectId + "'";
		piebar += " ORDER BY c_object_id ASC, ext_max ASC";
		
		upd = " UPDATE (" + piebar + ") SET";
		upd += " label_x = lower_x + (upper_x-lower_x)/2,";
		upd += " label_y = lower_y + (upper_y-lower_y)/2";

		if(!execute(upd))
			return false;
	}
	
	if (theme->layer()->hasGeometry(TePOINTS))
	{
		geomTable = theme->layer()->tableName(TePOINTS);

		piebar = "SELECT label_x, label_y, x, y";
		piebar += " FROM " + collTable + " LEFT JOIN " + geomTable;
		piebar += " ON " + collTable + ".c_object_id = " + geomTable + ".object_id";
		piebar += " WHERE label_x is null OR label_y is null";
		if (!objectId.empty())
			upd += " AND " +  collTable + ".c_object_id = '" + objectId + "'";

		upd = " UPDATE (" + piebar + ") SET";
		upd += " label_x = x,";
		upd += " label_y = y";

		if(!execute(upd))
			return false;
	}
	
	if(theme->layer()->hasGeometry(TeLINES))
	{
		geomTable = theme->layer()->tableName(TeLINES);

		piebar = "SELECT label_x, label_y, lower_x, lower_y, upper_x, upper_y";
		piebar += " FROM " + collTable + " LEFT JOIN " + geomTable;
		piebar += " ON " + collTable + ".c_object_id = " + geomTable + ".object_id";
		piebar += " WHERE label_x is null OR label_y is null";
		if (!objectId.empty())
			upd += " AND " +  collTable + ".c_object_id = '" + objectId + "'";
		piebar += " ORDER BY c_object_id ASC, ext_max ASC";
		
		upd = " UPDATE (" + piebar + ") SET";
		upd += " label_x = lower_x + (upper_x-lower_x)/2,";
		upd += " label_y = lower_y + (upper_y-lower_y)/2";

		if(!execute(upd))
			return false;
	}
	return true;
}

bool 
TeDatabase::themeExist(string themeName)
{
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	themeName = TeConvertToUpperCase(themeName);

	string sql = "SELECT name FROM te_theme";
	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}
	while (portal->fetchRow())
	{
		string name = portal->getData(0);
		name = TeConvertToUpperCase(name);
		if (themeName == name)
		{
			delete portal;
			return true;
		}
	}
	delete portal;
	return false;
}

string TeDatabase::getNewThemeName(const string& n)
{
	bool changed;
	string invalidChar;
	string name = TeCheckName(n, changed, invalidChar);
	string newName = name;

	string q = "SELECT name FROM te_theme WHERE name = '" + newName + "'";

	TeDatabasePortal* portal = this->getPortal();
	if(portal && portal->query(q) && portal->fetchRow())
	{
		// there is already a theme with this theme 
		q = "SELECT name FROM te_theme WHERE name LIKE '" + name + "_%' ORDER BY name DESC";
		portal->freeResult();
		if(portal && portal->query(q))
		{
			newName.clear();
			while(portal->fetchRow())
			{
				string s = portal->getData(0);
				size_t i, f = s.rfind("_");
				f++;
				newName = s.substr(0, f);
				string ss = s.substr(f);
				for(i=0; i < ss.size(); ++i)
				{
					if(ss[i] < 0x30 || ss[i] > 0x39)
						break;
				}
				if(i < ss.size())
					continue;

				int n = atoi(ss.c_str()) + 1;
				s = Te2String(n);
				newName += s;
				break;
			}
			if(newName.empty())
				newName = name + "_1";
		}
	}
	delete portal;
	return newName;
}

string TeDatabase::getNewTableName(const string& n)
{
	bool changed;
	string invalidChar;
	string name = TeCheckName(n, changed, invalidChar);
	string newName = name;
	int	i=1;
	while (this->tableExist(newName))
	{
		newName = name + Te2String(i);
		++i;
	}
	return newName;
}

string TeDatabase::getConcatFieldsExpression(const vector<string>& fNamesVec)
{
	string concatExp;
	for (unsigned int i = 0; i < fNamesVec.size(); ++i)
	{
		if (i != 0)
			concatExp += " & ";
		concatExp += fNamesVec[i];
	}
	return concatExp;
}


bool
TeDatabase::deleteTheme(int themeId)
{
	TeAbstractTheme* tema;
	bool themeWasFound = false;
	TeThemeMap::iterator it;

	it = invalidThemeMap().find(themeId);
	if (it != invalidThemeMap().end())
	{
		themeWasFound = true;
		tema = it->second;
		invalidThemeMap().erase(themeId);
	}

	if (themeWasFound == false)
	{
		it = themeMap().find(themeId);
		if(it != themeMap().end())
		{
			themeWasFound = true;
			tema = it->second;
			themeMap().erase(themeId);
		}
	}

	if (themeWasFound == false)
		return false;
	
	if(!tema->eraseMetadata(this))
		return false;
	
	string sql;
	// delete the collection table associated to this theme
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	sql = "SELECT collection_table FROM te_theme WHERE theme_id = " + Te2String(themeId);
	if (!portal->query(sql) ||!portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	string colTab = portal->getData("collection_table");
	//delete collection table
	if (this->tableExist(colTab))	
	{
		sql = "DROP TABLE " + colTab;
		if (!this->execute(sql) )
		{	
			delete portal;
			return false;
		}
	}
	//delete auxiliar collection table
	if (this->tableExist(colTab +"_aux"))	
	{
		sql = "DROP TABLE " +colTab +"_aux";
		if (!this->execute(sql) )
		{	
			delete portal;
			return false;
		}
	}
	portal->freeResult();

	//delete the visual associated to this theme
	if (existRelation("te_visual","fk_visual_legend_id") != TeRICascadeDeletion)
	{
		sql = "SELECT legend_id FROM te_legend WHERE theme_id = " + Te2String(themeId);
		if (!portal->query(sql))
		{	
			delete portal;
			return false;
		}
		string wherec;
		int c = 0;
		while (portal->fetchRow())
		{
			if (c)
				wherec += ",";
			c++;
			wherec += portal->getData(0);
		}
		portal->freeResult();
        if (!wherec.empty()) 
        {
			sql = "DELETE FROM te_visual WHERE legend_id IN (" + wherec + ")";
			if (!this->execute(sql))
			{	
				delete portal;
				return false;
			}
	   }
	}

	//delete all visuals of raster associated to this theme
	if (existRelation("te_visual_raster","fk_visrast_theme_id") != TeRICascadeDeletion)
	{
		sql = "DELETE FROM te_visual_raster WHERE theme_id =" + Te2String(themeId);
		if (!this->execute (sql))
		{	
			delete portal;
			return false;
		}
	}
	
	//delete all legends associated to this theme
	if (existRelation("te_legend","fk_legend_theme_id") != TeRICascadeDeletion)
	{
		sql = "DELETE FROM te_legend WHERE theme_id =" + Te2String(themeId);
		if (!this->execute (sql))
		{	
			delete portal;
			return false;
		}
	}
	
	//select the view of this theme
	sql = "SELECT view_id FROM te_theme WHERE theme_id = " + Te2String(themeId);
	portal->freeResult();
	if(!portal->query(sql) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	//delete theme of the view tree
	int viewId = portal->getInt("view_id");
	delete portal;

	//delete the tables associated to this theme
	if (existRelation("te_theme_table","fk_thmtable_theme_id") != TeRICascadeDeletion)
	{
		sql = "DELETE FROM te_theme_table WHERE theme_id =" + Te2String(themeId);
		if (!this->execute (sql))
			return false;
	}
		
	//delete the grouping
	if (existRelation("te_grouping","fk_group_theme_id")  != TeRICascadeDeletion)
	{
		sql = "DELETE FROM te_grouping WHERE theme_id =" + Te2String(themeId);
		if (!this->execute (sql))
			return false;
	}

	// delete raster visual
	if (existRelation("te_visual_raster","fk_visrast_theme_id") == TeNoRelation )
	{
		sql = "DELETE FROM te_visual_raster WHERE theme_id =" + Te2String(themeId);
		if (!this->execute (sql))
			return false;
	}

	sql = " UPDATE te_view SET current_theme = NULL WHERE current_theme = "+ Te2String(themeId);
	this->execute(sql);
	
	// delete the theme
	sql = " DELETE FROM te_theme WHERE theme_id = " + Te2String(themeId);
	if (!this->execute (sql))
		return false;

	//delete in the maps
	TeView* view = viewMap()[viewId];
	if (view) //this view exists
		view->remove(themeId); 

    unsigned int i;
	TeLegendEntryVector& legendVector = tema->legend();
	for (i = 0; i < legendVector.size(); ++i)
		legendMap().erase(legendVector[i].id());
	
	delete tema;
	return true;
}

bool
TeDatabase::deleteThemeGroup(int themeId)
{
	string sql;
	// delete the theme
	sql = "DELETE FROM te_theme WHERE theme_id = " + Te2String(themeId);
	if (!this->execute (sql))
		return false;
	return true;
}

bool
TeDatabase::deleteLegend(int themeId)
{
	// If there is a collection table update legend of the objects in it
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;
	string sel = "SELECT collection_table FROM te_theme WHERE theme_id = " + Te2String(themeId);
	string TC;
	if (portal->query(sel) && portal->fetchRow())
		TC = portal->getData(0);
	delete portal;

	if (!TC.empty() && this->tableExist(TC))
	{
		string up = "UPDATE " + TC + " SET c_legend_id = 0";
		if (!execute(up))
			return false;
	}

	// Delete visual of the legends
	string del = "DELETE FROM te_visual WHERE legend_id IN ";
	del += "(SELECT legend_id FROM te_legend WHERE theme_id = " + Te2String(themeId);
	del += " AND group_id > -1)";
	if (!execute(del))
		return false;

	del = "DELETE FROM te_legend WHERE theme_id = " + Te2String(themeId);
	del += " AND group_id > -1";
	if (!execute(del))
		return false;

	// Delete from memory the legends of the theme
	unsigned int i;
	TeAbstractTheme *theme = metaModel_->themeMap()[themeId];
	TeLegendEntryVector& legendVector = theme->legend();
	for (i = 0; i < legendVector.size(); ++i)
		metaModel_->legendMap().erase(legendVector[i].id());
	legendVector.clear();

	//delete grouping
	del = "DELETE FROM te_grouping WHERE theme_id =" + Te2String(themeId);
	if (!execute (del))
		return false;
		
	return true;
}

bool
TeDatabase::updateLayer(TeLayer *layer)
{
	if (!layer)
		return false;

	if (layer->projection())
		updateProjection(layer->projection());

	string sql;
	sql = "UPDATE te_layer SET ";
	sql += "name = '" + layer->name() + "' ";
	if (layer->box().isValid())
	{
		sql += ", lower_x = " + Te2String(layer->box().x1(),15) + " ";
		sql += ", lower_y = " + Te2String(layer->box().y1(),15) + " ";
		sql += ", upper_x = " + Te2String(layer->box().x2(),15) + " ";
		sql += ", upper_y = " + Te2String(layer->box().y2(),15) + " ";
	}else
	{
		sql += ", lower_x = " + Te2String(layer->box().x1()) + " ";
		sql += ", lower_y = " + Te2String(layer->box().y1()) + " ";
		sql += ", upper_x = " + Te2String(layer->box().x2()) + " ";
		sql += ", upper_y = " + Te2String(layer->box().y2()) + " ";
	}
	sql +=  " WHERE layer_id = " + Te2String(layer->id());

	return (this->execute (sql));
}

bool
TeDatabase::loadLayerSet(const bool& loadAttrList)
{
	//clear layer map
	TeLayerMap::iterator it = metaModel_->layerMap().begin();
	while (it != metaModel_->layerMap().end())
	{
		if(it->second)
			delete it->second;
		++it;
	}
	metaModel_->layerMap().clear();
	
	string sql = " SELECT te_layer.*, "; // 0 - 8 (9 columns)
	sql += " te_projection.*, "; // 9 - 25  (17 columns)
	sql += " te_representation.*, "; // 26 - 40 (15 columns)
	sql += " te_layer_table.* "; // 41 - 52 (12 columns)
	
	sql += " FROM (((te_layer INNER JOIN te_projection ";
	sql += " ON te_layer.projection_id = te_projection.projection_id) ";
	sql += " LEFT JOIN te_representation ON ";
	sql += " te_layer.layer_id = te_representation.layer_id) ";
	sql += " LEFT JOIN te_layer_table ON ";
	sql += " te_layer.layer_id = te_layer_table.layer_id) ";

	sql += " ORDER BY te_layer.layer_id, te_representation.geom_type, te_layer_table.table_id ";

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}
	
	int lastLayerId = -1;
	TeLayer *layer = 0;
	bool hasNewRow = portal->fetchRow(); //idicates that this portal was fetched 
	while(hasNewRow)
	{
		//load view and its projection 
		if(lastLayerId!=atoi(portal->getData(0)))
		{
			TeProjection* proj = 0;
			if(!portal->getProjection(&proj, 9)) //load projection
			{
				delete portal;
				return false;
			}
			layer = new TeLayer();
			if(!portal->getLayer(*layer, 0)) //load layer
			{
				delete portal;
				delete layer;
				return false;
			}
			if (proj != 0)
				layer->setProjection(proj);
			lastLayerId = layer->id();
		}

		//load its representation and its tables
		bool hasRepsTablesToThisLayer = true;
		vector<int> loadedTableId;
		while(hasRepsTablesToThisLayer)
		{
			//load representation
			string repId = portal->getData(26);
			if(!repId.empty() && !layer->hasGeometry(TeGeomRep(portal->getInt(28))))
			{
				TeRepresentation* rep = new TeRepresentation();
				if(!portal->getRepresentation(*rep, 26))
				{
					delete rep;
					delete layer;
					delete portal;
				}
				layer->addVectRepres(rep);
			}
			
			//load tables
			if( find(loadedTableId.begin(), loadedTableId.end(), portal->getInt(41)) == loadedTableId.end())
			{
				TeTable attrTable;
				if(portal->getAttrTable(attrTable, 41))
				{
					TeAttributeList attrList;
					if(loadAttrList)
						getAttributeList(attrTable.name(), attrList);
					attrTable.setAttributeList(attrList);
					layer->addAttributeTable(attrTable);
					loadedTableId.push_back(attrTable.id());
				}
			}
			hasNewRow = portal->fetchRow();
			if(!hasNewRow || portal->getInt(0)!= layer->id())
				hasRepsTablesToThisLayer = false;
		}

		layer->setDatabase(this);
		metaModel_->layerMap()[layer->id()] = layer;
	}

	delete portal;
	return true;

}

string
TeDatabase::getRasterTable(int layerId, const string& objectId)
{
	if (layerId <=0 )
		return "";

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return "";

	string get;
	// get the raster representation
	get = "SELECT geom_table FROM te_representation WHERE layer_id = "+Te2String(layerId);
	get += " AND geom_type= " + Te2String((int)TeRASTER) + " OR geom_type= " + Te2String((int)TeRASTERFILE);
	
	// error executing query or no there is no raster representation 
	if (!portal->query(get) || !portal->fetchRow())
	{
		delete portal;
		return "";
	}

	string tableName = portal->getData(0);
	portal->freeResult();
	if (tableName.empty())
	{
		delete portal;
		return "";
	}

	// check if a specific object is being looked for
	get = "SELECT raster_table FROM " + tableName + " WHERE object_id='" + objectId + "'";
	if (!portal->query(get) || !portal->fetchRow())
	{
		delete portal;
		return "";
	}
	tableName = portal->getData(0);
	delete portal;
	return tableName;
}


TeRaster*
TeDatabase::loadLayerRaster(int layerId, const string& objectId, const char& mode)
{
	if (layerId <=0 )
		return 0;

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return 0;

	TeRaster* raster = 0;
	string get;

	// get the raster representation
	get = "SELECT geom_table, geom_type FROM te_representation WHERE layer_id = "+Te2String(layerId);
	get += " AND (geom_type= " + Te2String((int)TeRASTER) + " OR geom_type= " + Te2String((int)TeRASTERFILE) + ")";
	
	// error executing query or no there is no raster representation 
	if (!portal->query(get) || !portal->fetchRow())
	{
		delete portal;
		return 0;
	}
	string tableName = portal->getData(0);
	TeGeomRep rep = static_cast<TeGeomRep>(portal->getInt(1));
	portal->freeResult();
	if (tableName.empty())
	{
		delete portal;
		return 0;
	}

	//--- this check is made for compatibility reasons with older terralib databases
	TeAttributeRep	attrRep;
	attrRep.name_ = "tiling_type";
	attrRep.type_ = TeINT;
		
	TeAttribute att;
	if(!columnExist(tableName, attrRep.name_,att))
	{
		addColumn (tableName, attrRep);
		string sql = "UPDATE " + tableName + " SET tiling_type = " + Te2String(static_cast<int>(TeRasterParams::TeExpansible));
		this->execute(sql);
	}

	// ---

	// check if a specific object is being looked for
	get = "SELECT * FROM " + tableName;
	if (!objectId.empty())
		get += " WHERE object_id='" + objectId + "'"; 
	if (!portal->query(get) || !portal->fetchRow())
	{
		delete portal;
		return 0;
	}
		
	string oid = portal->getData("object_id");
	int geomId = portal->getInt("geom_id"); // get raster representation id

	// get raster parameters from te_raster_table table
	TeRasterParams params;
	params.fileName_ = portal->getData("raster_table");
	params.lutName_ = portal->getData("lut_table");
	params.nBands(portal->getInt("num_bands"));
	params.boundingBoxResolution(portal->getDouble("lower_x"),portal->getDouble("lower_y"),
								 portal->getDouble("upper_x"),portal->getDouble("upper_y"),
							     portal->getDouble("res_x"),portal->getDouble("res_y"));
	params.blockHeight_ = portal->getInt("block_height");
	params.blockWidth_ = portal->getInt("block_width");
	params.tiling_type_ = static_cast<TeRasterParams::TeRasterTilingType>(portal->getInt("tiling_type"));

	portal->freeResult();

	// get extra information from te_raster_metadata table
	string metadatatable = tableName + "_metadata"; 
	unsigned int nb = params.nBands();
	unsigned int i;
	for (i=0; i<nb; i++)
	{
		get = "SELECT * FROM " + metadatatable + " WHERE geom_id=" + Te2String(geomId);
		get += " AND band_id=" + Te2String(i);
		if (portal->query(get) && portal->fetchRow()) 
		{
			params.vmax_[i] = portal->getDouble("max_value");
			params.vmin_[i] = portal->getDouble("min_value");
			params.nbitsperPixel_[i] = portal->getInt("num_bits");
			params.dataType_[i] =  static_cast<TeDataType>(portal->getInt("data_type"));
			params.compression_[i] = static_cast<TeRasterParams::TeRasterCompressionMode>(portal->getInt("compression_type"));
			params.photometric_[i] = static_cast<TeRasterParams::TeRasterPhotometricInterpretation>(portal->getInt("photometric_type"));
		}
		portal->freeResult();
	}

	// if raster is pallete get the associated LUT
	if (params.photometric_[0] == TeRasterParams::TePallete && 	rep != TeRASTERFILE)
		this->loadRasterLUT(&params);

	// raster has the same projection as its layer
	get = " SELECT te_projection.* FROM ";
	get += " te_projection INNER JOIN te_layer ON ";
	get += " te_projection.projection_id = te_layer.projection_id ";
	get += " WHERE layer_id = " + Te2String(layerId);
	
	TeProjection* proj=0;
	if (portal->query(get) && portal->fetchRow())
		portal->getProjection(&proj);

	portal->freeResult();
	params.projection(proj);
	if (proj)
		delete proj;

	bool hasDummy = false;
	get = "SELECT band_id, dummy FROM " + metadatatable + " WHERE geom_id=" + Te2String(geomId);
	get += " AND NOT (dummy IS NULL)";
	if (portal->query(get))
	{
		while (portal->fetchRow())
		{
			int b = portal->getInt(0);
			double d = portal->getDouble("dummy");
			params.setDummy(d,b);
			hasDummy = true;
		}
	}
	params.useDummy_ = hasDummy;
	params.mode_ = mode;
	params.objectId_ = oid;
	params.layerId_ = layerId;
	delete portal;

	if ( rep == TeRASTER)
	{
		params.nTilesInMemory_ = 0;
		params.database_ = this;
		TeDecoderDatabase* dec = new TeDecoderDatabase(params);
		dec->init();
		raster = new TeRaster();
		raster->setDecoder(dec);
		raster->objectId(oid);
		return raster;
	}
	try
	{
		raster = new TeRaster(params);
	}
	catch(...)
	{
		if (params.fileName_.empty() == false)
		{
			errorMessage_ = "File doesn't exist: ";
			errorMessage_ += params.fileName_;
		}
		else
		{
			errorMessage_ = "Raster file is not accessible.";
		}
		return 0;
	} 
	raster->init();
	return raster;
}

bool 
TeDatabase::loadRasterLUT(TeRasterParams* par)
{
	if (par->lutName_.empty())		
		return false;
	
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	string get = "SELECT COUNT(index_id) FROM " + par->lutName_;
	if (!portal->query(get) || !portal->fetchRow())	 // if there is no table, or table is empty
	{
		delete portal;
		return false;
	}
	int nentries = atoi(portal->getData(0));
	if (nentries <= 0)
	{
		delete portal;
		return false;
	}
	portal->freeResult();

	par->lutr_.clear();
	par->lutg_.clear();
	par->lutb_.clear();

	par->lutr_.resize(nentries);
	par->lutg_.resize(nentries);
	par->lutb_.resize(nentries);

	par->lutr_.assign(nentries,0);
	par->lutg_.assign(nentries,0);
	par->lutb_.assign(nentries,0);

	get = "SELECT * FROM " + par->lutName_ + " ORDER BY index_id ASC ";
	if (!portal->query(get) || !portal->fetchRow())	 // if there is no table, or table is empty
	{
		delete portal;
		return false;
	}

	do
	{
		int index = atoi(portal->getData(0));
		par->lutr_[index] = atoi(portal->getData(1));
		par->lutg_[index] = atoi(portal->getData(2));
		par->lutb_[index] = atoi(portal->getData(3));
	}while (portal->fetchRow());

	delete portal;
	return true;
}

bool
TeDatabase::createSpatialIndex(const string& table, const string& columns, TeSpatialIndexType /*type*/, short /*level*/, short /*tile*/)
{
	string idxName = "sp_idx_" + table;
	return createIndex(table, idxName, columns);
}

string TeDatabase::getSpatialIdxColumn(TeGeomRep rep)
{
	string columns = "";
	switch(rep)
	{
		case TePOINTS:
		case TeNODES:	columns = "x, y";
						break;
		case TeLINES:
		case TePOLYGONS:
		case TeCELLS:
						columns = "lower_x, lower_y, upper_x, upper_y";
						break;
		case TeRASTER:
						columns = " lower_x, lower_y, upper_x, upper_y, resolution_factor, subband ";
						break;
		default:		columns = "";
	}
	
    return columns;
}

bool
TeDatabase::loadLayer(TeLayer* layer, const bool& loadAttrList)
{
	if (layer == 0)
		return false;

	string rest;
	if (layer->id() > 0)
		rest = " te_layer.layer_id = "+  Te2String(layer->id());
	else if (!layer->name().empty())
		rest = " te_layer.name = '"+  layer->name() + "'";
	else
	{
		this->errorMessage_ = "Layer procurado n�o possui nem id nem nome";
		return false;
	}
	
	string sql = " SELECT te_layer.*, "; // 0 - 8 (9 columns)
	sql += " te_projection.*, "; // 9 - 25  (17 columns)
	sql += " te_representation.*, "; // 26 - 40 (15 columns)
	sql += " te_layer_table.* "; // 41 - 52 (12 columns)
	
	sql += " FROM (((te_layer INNER JOIN te_projection ";
	sql += " ON te_layer.projection_id = te_projection.projection_id) ";
	sql += " LEFT JOIN te_representation ON ";
	sql += " te_layer.layer_id = te_representation.layer_id) ";
	sql += " LEFT JOIN te_layer_table ON ";
	sql += " te_layer.layer_id = te_layer_table.layer_id) ";

	sql += " WHERE "+ rest;

	sql += " ORDER BY te_representation.geom_type, te_layer_table.table_id ";

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	if(!portal->query(sql) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	bool hasNewRow = true;
	TeProjection* proj = 0;
	if(!portal->getProjection(&proj, 9)) //load projection
	{
		delete portal;
		return false;
	}
	
	if(!portal->getLayer(*layer, 0)) //load layer
	{
		delete portal;
		delete layer;
		return false;
	}
	if (proj != 0)
		layer->setProjection(proj);
		
	//load its representation and its tables
	bool hasRepsTablesToThisLayer = true;
	vector<int> loadedTableId;
	while(hasRepsTablesToThisLayer)
	{
		string repId = portal->getData(26);
		if(!repId.empty() && !layer->hasGeometry(TeGeomRep(portal->getInt(28))))
		{
			//load the geometry representation
			TeRepresentation* rep = new TeRepresentation();
			if(!portal->getRepresentation(*rep, 26))
			{
				delete rep;
				delete layer;
				delete portal;
			}
			layer->addVectRepres(rep);
		}
		
		//load tables
		if(find(loadedTableId.begin(), loadedTableId.end(), portal->getInt(41)) == loadedTableId.end())
		{
			TeTable attrTable;
			if(portal->getAttrTable(attrTable, 41))
			{
				TeAttributeList attrList;
				if(loadAttrList)
					getAttributeList(attrTable.name(), attrList);
				attrTable.setAttributeList(attrList);
				layer->addAttributeTable(attrTable);
				loadedTableId.push_back(attrTable.id());
			}
		}
		
		hasNewRow = portal->fetchRow();
		if(!hasNewRow || portal->getInt(0)!= layer->id())
			hasRepsTablesToThisLayer = false;
	}
	layer->setDatabase(this);
	metaModel_->layerMap()[layer->id()] = layer;
	
	delete portal;
	return true;
}

bool
TeDatabase::loadLayerTable(TeLayer* layer, const bool& loadAttrList)
{
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
	{
		this->errorMessage_ = "N�o foi poss�vel abrir portal para o banco";
		return false;
	}
	
	// Get layer tables	
	string get =  " SELECT * FROM te_layer_table"; 
	get += " WHERE layer_id = " + Te2String(layer->id());
	get += " ORDER BY attr_table_type, table_id";

	if (!portal->query (get))
	{	
		delete portal;
		return false;
	}

	while (portal->fetchRow())
	{
		TeTable attTable;
		if(!portal->getAttrTable (attTable))
		{
			delete portal;
			return false;
		}
		TeAttributeList attrList;
		if(loadAttrList)
			getAttributeList(attTable.name(), attrList);
		attTable.setAttributeList(attrList);
		layer->addAttributeTable(attTable);
	}

	delete portal;
	return true;
}

bool 
TeDatabase::layerExist(int layerId)
{
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	string sql = "SELECT layer_id FROM te_layer WHERE layer_id = " + Te2String(layerId);
	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}
	if (!portal->fetchRow())
	{
		delete portal;
		return false;
	}
	delete portal;
	return true;
}

bool 
TeDatabase::layerExist(string layerName)
{
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	layerName = TeConvertToUpperCase(layerName);

	string sql = "SELECT name FROM te_layer";
	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}
	while (portal->fetchRow())
	{
		string name = portal->getData(0);
		name = TeConvertToUpperCase(name);
		if (layerName == name)
		{
			delete portal;
			return true;
		}
	}
	delete portal;
	return false;
}

string TeDatabase::getNewLayerName(const string& n)
{
	bool changed;
	string invalidChar;
	string name = TeCheckName(n, changed, invalidChar);
	string newName = name;
	
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return "";
	
	bool flag = true;
	int count = 0;
	while(flag)
	{
		portal->freeResult();
		string q = "SELECT name FROM te_layer WHERE name = '" + newName + "'";
		if(portal->query(q) && portal->fetchRow())
		{
			// there is already this theme name 
			newName = newName+"_"+Te2String(count);
			++count;
			flag = true;
         }
		else
			flag = false; //found the new name
	}
	delete portal;
	return newName;
}

bool
TeDatabase::deleteLayer(int layerId)
{
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;
	
	string sql = "SELECT projection_id FROM te_layer WHERE layer_id = ";
	sql += Te2String(layerId);

	if (!portal->query(sql))
	{	
		delete portal;
		return false;
	}

	if (!portal->fetchRow())
	{
		delete portal;
		return false;
	}
	string projId = portal->getData("projection_id");
	portal->freeResult();

	// Get all representations that are associated to this layer
	sql = "SELECT * FROM te_representation WHERE layer_id = "+ Te2String(layerId);
	if (!portal->query (sql))
	{
		delete portal;
		return false;
	}

	while (portal->fetchRow())
	{	
		// Save the name of the geometry table
		string geomTable = portal->getData("geom_table");

		// Delete lut table
		TeGeomRep rep = TeGeomRep(portal->getInt("geom_type"));
		if (rep == TeRASTER || rep == TeRASTERFILE)
		{
			TeDatabasePortal* portal2 = this->getPortal();
			sql = "SELECT lut_table, raster_table FROM " + geomTable;
			string tabName;
			if (!portal2->query (sql))
			{
				delete portal2;
				continue;
			}

			while (portal2->fetchRow())
			{
				// remove lut table
				tabName = portal2->getData(0);
				if (!tabName.empty() && this->tableExist(tabName))
				{
					sql = "DROP TABLE " + tabName;
					this->execute(sql);
				}
				// remove raster table
				tabName = portal2->getData(1);
				if (!tabName.empty() && this->tableExist(tabName))
				{
					sql = "DROP TABLE " + tabName;
					this->execute(sql);
				}
			}
			delete portal2;
			// remove raster metadata table
			tabName = geomTable + "_metadata";
			if (!tabName.empty() && this->tableExist(tabName))
			{
				sql = "DROP TABLE " + tabName;
				this->execute(sql);
			}
		}
		if (this->tableExist(geomTable))
		{
			sql = "DROP TABLE " + geomTable;
			if (!this->execute(sql) )
			{
				delete portal;
				return false;
			}
		}
	}
	portal->freeResult();

	if (existRelation("te_representation","fk_rep_layer_id") != TeRICascadeDeletion)
	{
		// Delete entries into representations table
		sql = "DELETE FROM te_representation WHERE layer_id = " +Te2String(layerId);
		if (!this->execute(sql) )
		{
			delete portal;
			return false;
		}
	}

	// delete layer themes
	sql = "SELECT theme_id FROM te_theme WHERE layer_id=" + Te2String(layerId);
	if (!portal->query (sql))
	{
		delete portal;
		return false;
	}
	
	int themeId;
	while (portal->fetchRow())
	{	
		themeId = portal->getInt("theme_id");
		this->deleteTheme(themeId);
	}

	//Delete attributes tables
	if(!deleteLayerTable(layerId))
		return false;
	
	sql = "DELETE FROM te_layer WHERE layer_id=" + Te2String(layerId);
	if (!this->execute(sql))
	{
		delete portal;
		return false;
	}

	// delete layer projection
	sql = "DELETE FROM te_projection WHERE projection_id = "+ projId;
	if (!this->execute(sql))
	{	
		delete portal;
		return false;
	}

	// remove all the items� themes associated to the layer to be removed
	TeThemeMap::iterator it;
	for (it = metaModel_->themeMap().begin(); it != metaModel_->themeMap().end();)
	{
		if(it->second->getProductId() != TeTHEME)
		{
			++it;
			continue;
		}
		TeTheme *theme = static_cast<TeTheme*> (it->second);
		++it;
		if (theme && theme->layer() && (theme->layer()->id() == layerId))
		{
			metaModel_->themeMap().erase(theme->id());
			delete theme;
		}
	}
	// delete layer and its entry in the layer map
	TeLayer* layer = metaModel_->layerMap()[layerId];
	metaModel_->layerMap().erase(layerId);
	delete layer;

	delete portal;
	return true;
}


bool
TeDatabase::deleteLayerTable (int layerId, TeAttrTableType ttype)
{
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	//tables of the type 1 can be relationed with other layer ??? Conferir
	string query = "SELECT attr_table, table_id FROM te_layer_table WHERE layer_id = " + Te2String(layerId);
	query += " AND attr_table_type = " + Te2String(static_cast<int>(ttype));
	if(!portal->query(query))
	{
		delete portal;
		return false;
	}
		
	vector<int> tableIds;
	string attrTable;
	string tableId;
	string drop;
	while (portal->fetchRow())
	{
		attrTable = portal->getData(0);
		tableId = portal->getData(1);
		drop = "DROP TABLE " + attrTable;
		if(tableExist(attrTable))
		{
			if(!execute(drop))
			{
				delete portal;
				return false;
			}
		}
		tableIds.push_back(atoi(tableId.c_str()));

		string del  = "DELETE FROM te_address_locator WHERE table_id = "+ tableId;
		execute(del);
	}

	delete portal;
	string del;
	if (existRelation("te_tables_relation","fk_tabrelation_laytable_id") != TeRICascadeDeletion)
	{
		for (unsigned int i=0; i<tableIds.size();i++)
		{
			del = "DELETE FROM te_tables_relation WHERE relation_id = " + Te2String(tableIds[i]);
			if (!execute (del))
				return false;
		}
	}
	del = "DELETE FROM te_layer_table WHERE layer_id = " + Te2String(layerId);
	if (!execute (del))
		return false;
	return true;
}

bool 
TeDatabase::updateRepresentation (int layerId, TeRepresentation& rep)
{
	if (layerId <= 0)
		return false;

	string sql;
	sql  = "UPDATE te_representation SET ";
	sql += " lower_x= " + Te2String(rep.box_.x1(),15);
	sql += ", lower_y= " + Te2String(rep.box_.y1(),15);
	sql += ", upper_x= " + Te2String(rep.box_.x2(),15);
	sql += ", upper_y= " + Te2String(rep.box_.y2(),15);
	sql += ", description= '" + rep.description_ + "'";
	sql += ", res_x= " + Te2String(rep.resX_,15);
	sql += ", res_y= " + Te2String(rep.resY_,15);
	sql += ", num_cols=" + Te2String(rep.nCols_);
	sql += ", num_rows=" + Te2String(rep.nLins_);

	if (rep.geomRep_ != TeTEXT)
		sql += ", geom_table='" + rep.tableName_ + "'";

	sql += " WHERE layer_id=" + Te2String(layerId);
	sql += " AND geom_type= " + Te2String(rep.geomRep_);

	if (rep.geomRep_ == TeTEXT)
		sql += " AND geom_table='" + rep.tableName_ + "'";

	return this->execute(sql);
}

bool 
TeDatabase::insertRasterGeometry(const string& tableName, TeRasterParams& par, const string& objectId)
{
	if (tableName.empty())
		return false;
	
	string objId; 
	if (objectId.empty())
		objId = "O1";
	else
		objId = objectId;

	//------ this check is made for compatibility reasons with old versions of TerraLib databases
	TeAttributeRep	attrRep;
	attrRep.name_ = "tiling_type";
	attrRep.type_ = TeINT;
		
	TeAttribute att;
	if(!columnExist(tableName, attrRep.name_,att))
	{
		addColumn (tableName, attrRep);
		string sql = "UPDATE " + tableName + " SET tiling_type = " + Te2String(static_cast<int>(TeRasterParams::TeExpansible));
		this->execute(sql);
	}
	//------

	// finds the name of the raster geometry table
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	TeBox box = par.boundingBox();

	string ins = "INSERT INTO " + tableName + " (object_id, raster_table, lut_table, ";
	ins += "res_x, res_y, num_bands, num_cols, num_rows, block_height, block_width, ";
	ins += "lower_x, lower_y, upper_x, upper_y, tiling_type) ";
	ins += " VALUES ('" + objId + "', '" + par.fileName_+ "', '" +  par.lutName_ + "', ";
	ins += Te2String(par.resx_) + ", " + Te2String(par.resy_) + ", ";
	ins += Te2String(par.nBands()) + ", " + Te2String(par.ncols_) + ", " + Te2String(par.nlines_) + ", ";
	ins += Te2String(par.blockHeight_) + ", " + Te2String(par.blockWidth_) + ", ";
	ins += Te2String(box.x1_,15) +", " + Te2String(box.y1_,15) + ", ";
	ins += Te2String(box.x2_,15) +", " + Te2String(box.y2_,15) + ", ";
	ins	+= Te2String(par.tiling_type_) + ")";
	if (!this->execute(ins))
	{
		delete portal;
		return false;
	}

	// save the pallete associated to the raster
	// if it doesn�t exist yet
	if (par.photometric_[0] == TeRasterParams::TePallete && !par.lutName_.empty()) 
	{
		 if (!this->tableExist(par.lutName_))
		 {
			 if (this->createLUTTable(par.lutName_))
			{
				for (unsigned int i=0; i<par.lutb_.size(); i++)
				{
					string sql = "INSERT INTO " + par.lutName_ + " VALUES(";
					sql += Te2String(i) + ", ";
					sql += Te2String(par.lutr_[i]) + ", ";
					sql += Te2String(par.lutg_[i]) + ", ";
					sql += Te2String(par.lutb_[i]) + ")";
					this->execute(sql);
				}
			 }
		 }
	}
	
	ins = "SELECT geom_id FROM " + tableName + " WHERE object_id='" + objId + "'";
	ins += " AND raster_table='" + par.fileName_+ "'";
	if(!portal->query(ins) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	int geomId = atoi(portal->getData(0));
	delete portal;
	string 	metadataTableName = tableName+"_metadata";
	insertRasterMetadata(metadataTableName, geomId,par);
	 return true;
}

bool 
TeDatabase::updateRasterRepresentation(int layerId, TeRasterParams& par, const string& objectId)
{
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	string sql = "SELECT repres_id, lower_x, lower_y, upper_x, upper_y, geom_table ";
	sql += " FROM te_representation WHERE layer_id= " + Te2String(layerId);
	sql += " AND geom_type= " + Te2String(TeRASTER);

	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	TeBox box (portal->getDouble(1),portal->getDouble(2),
		       portal->getDouble(3),portal->getDouble(4));
	int represId = atoi(portal->getData(0));
	string rasterrep = portal->getData(5);
	portal->freeResult();

	updateBox(box,par.boundingBox());
	sql = "UPDATE te_representation SET lower_x = " + Te2String(box.x1_,15);
	sql += ", lower_y = " + Te2String(box.y1_,15) + ", upper_x = " + Te2String(box.x2_,15);
	sql += ", upper_y = " + Te2String(box.y2_,15) + "  WHERE repres_id=" + Te2String(represId);

	if(!execute(sql))
	{
		delete portal;
		return false;
	}	 

	string objId; 
	if (objectId.empty())
		objId = "O1";
	else
		objId = objectId;

	box = par.boundingBox();

	sql = "UPDATE " + rasterrep + " SET lut_table ='" + par.lutName_ + "'";
	sql += ", res_x= " + Te2String(par.resx_) + ", res_y=" + Te2String(par.resy_);
	sql += ", num_bands=" + Te2String(par.nBands()) + ", num_cols=" + Te2String(par.ncols_);
	sql += ", num_rows=" + Te2String(par.nlines_) + ", block_height=" +  Te2String(par.blockHeight_);
	sql += ", block_width= " + Te2String(par.blockWidth_) + ", lower_x = " + Te2String(box.x1_,15);
	sql += ", lower_y = " + Te2String(box.y1_,15) +  ", upper_x = " + Te2String(box.x2_,15);
	sql += ", upper_y = " + Te2String(box.y2_,15);
	sql += "  WHERE object_id='" + objId + "' AND raster_table='" + par.fileName_ + "'";
	if (!this->execute(sql))
	{
		delete portal;
		return false;
	}
	
	sql = "SELECT geom_id FROM " + rasterrep + " WHERE object_id='" + objId + "'";
	sql+= " AND raster_table='" + par.fileName_+ "'";
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	// save the pallete associated to the raster
	// if it doesn�t exist yet
	if (par.photometric_[0] == TeRasterParams::TePallete && !par.lutName_.empty()) 
	{

		 if (!this->tableExist(par.lutName_))
		 {
			 if (this->createLUTTable(par.lutName_))
			{
				for (unsigned int i=0; i<par.lutb_.size(); i++)
				{
					string sql = "INSERT INTO " + par.lutName_ + " VALUES(";
					sql += Te2String(i) + ", ";
					sql += Te2String(par.lutr_[i]) + ", ";
					sql += Te2String(par.lutg_[i]) + ", ";
					sql += Te2String(par.lutb_[i]) + ")";
					if (!this->execute(sql) )	
					{
						delete portal;
						return false;						
					}
				}
			 }
		 }
	}

	int geomId = atoi(portal->getData(0));
	delete portal;
	string metadatatabel = rasterrep + "_metadata";
	return updateRasterMetadata(metadatatabel,geomId,par);
}

bool 
TeDatabase::insertRasterMetadata (const string& tableName, int geomId, TeRasterParams& par)
{
	if (geomId <= 0)
		return false;
	
	string ins;
	unsigned int i;

	unsigned int nb = par.nBands();
	ins = "INSERT INTO " + tableName + " (geom_id, band_id, min_value, max_value, ";
	ins += " num_bits, data_type, photometric_type, compression_type ) VALUES (";
	string vals;
	for (i=0; i<nb; i++)
	{
		vals = Te2String(geomId) + ", " + Te2String(i) + ", ";
		vals += Te2String(par.vmin_[i]) + ", " +  Te2String(par.vmax_[i]) + ", ";
		vals += Te2String(par.nbitsperPixel_[i]) + ", " +  Te2String(par.dataType_[i]) + ", " ;
		vals += Te2String(par.photometric_[i]) + ", " +  Te2String(par.compression_[i]) + " )" ;
		string sql = ins + vals;
		if (!this->execute(sql))
			return false;
	}

	// update dummy value
	if (par.useDummy_)
	{
		ins = "UPDATE " + tableName + " SET dummy = ";
		for (i=0; i<nb; i++)
		{
			vals = Te2String(par.dummy_[i]) + " WHERE geom_id = " +  Te2String(geomId);
			vals += " AND band_id=" + Te2String(i);
			string sql = ins + vals;
			if (!this->execute(sql))
				return false;
		}
	}
	return true;
}

bool 
TeDatabase::updateRasterMetadata (const string& tableName, int geomId, TeRasterParams& par)
{
	if (geomId <= 0)
		return false;

	string sql = "DELETE FROM " + tableName + " WHERE geom_id = " + Te2String(geomId);
	if (!this->execute (sql))
		return false;
	return insertRasterMetadata(tableName,geomId,par);
}

bool 
TeDatabase::updateLegend (TeLegendEntry *legend)
{
	if (!legend)
		return false;

	string sql;
	if (legend->id() > 0 )
	{
		sql = "UPDATE te_legend SET ";
		sql += " theme_id=" + Te2String (legend->theme());
		sql += ",group_id=" + Te2String (legend->group());
		sql += ",num_objs=" + Te2String (legend->count());
		sql += ",lower_value='" + escapeSequence(legend->from())+"'";
		sql += ",upper_value='" + escapeSequence(legend->to())+"'";
		sql += ",label='" + escapeSequence(legend->label())+"'";
		sql += " WHERE legend_id=" + Te2String (legend->id());

		if (execute(sql) == false)
			return false;
	}
	else
	{
		if (!insertLegend(legend))
			return false;
	}
	metaModel_->legendMap()[legend->id()] = legend;

	return updateVisual(legend);
}

bool 
TeDatabase::updateLegend (vector<TeLegendEntry>& legVec)
{
	unsigned int i;
	for (i = 0; i < legVec.size(); ++i)
	{
		if(!updateLegend(&legVec[i]))
			return false;
	}
	return true;
}

bool
TeDatabase::updateVisual(TeLegendEntry *legend)
{
	if (!legend)
		return false;

	TeGeomRepVisualMap& mapVis = legend->getVisualMap();
	TeGeomRepVisualMap::iterator it =  mapVis.begin();
	while ( it != mapVis.end())
	{ 
			
		TeGeomRep rep = it->first;
		TeVisual* vis = it->second;

		TeColor cor = vis->color();				// filling color
		TeColor contourCor = vis->contourColor();// contour color

		string update = "UPDATE te_visual SET ";
		update += "red = "+ Te2String(cor.red_) + ", ";
		update += "green =" + Te2String(cor.green_) + ", ";
		update += "blue =" + Te2String(cor.blue_) + ", ";
		update += "transparency =" + Te2String(vis->transparency()) + ", ";
		update += "contour_red=" + Te2String(contourCor.red_) + ", ";
		update += "contour_green=" + Te2String(contourCor.green_) + ", ";
		update += "contour_blue=" + Te2String(contourCor.blue_) + ", "; 
		update += "contour_transp=" + Te2String(vis->contourTransparency()) + ", ";
		update += "contour_width=" + Te2String(vis->contourWidth()) + ", ";

		if(rep == TePOLYGONS)
		{
			update += "width=" + Te2String(vis->contourWidth()) + ", ";
			update += "contour_symb_id=" + Te2String(vis->contourStyle()) + ", ";
			update += "symb_id=" + Te2String(vis->style()) + ", ";
		}
		else if(rep == TeLINES)
		{
			update += "width=" + Te2String(vis->width()) + ", ";
			update += "symb_id=" + Te2String(vis->style()) + ", ";
		}
		else if(rep == TePOINTS)
		{
			update += "size_value=" + Te2String(vis->size()) + ", ";
			update += "symb_id=" + Te2String(vis->style ()) + ", ";
		}
		else if(rep == TeTEXT)
		{
			update += "size_value=" + Te2String(vis->size()) + ", ";
			update += "pt_angle=" + Te2String(vis->ptAngle()) + ", ";
		}

		update += "family='" + vis->family() + "', ";
		if (vis->bold())
			update += "bold=1, ";
		else
			update += "bold=0, ";

		if (vis->italic())
			update += "italic=1, ";
		else
			update += "italic=0, ";

		update += "fixed_size=" + Te2String(vis->fixedSize())+ ", ";
		update += "alignment_vert=" + Te2String(vis->alignmentVert())+ ", ";
		update += "alignment_horiz=" + Te2String(vis->alignmentHoriz())+ ", ";
		update += "tab_size=" + Te2String(vis->tabSize())+ ", ";
		update += "line_space=" + Te2String(vis->lineSpace());

		update += " WHERE legend_id= " + Te2String(legend->id()) ;
		update += " AND geom_type= " + Te2String(rep);

		if (!execute(update))
			return false;
		++it;
	}
	return true;
}

bool
TeDatabase::updateVisual(vector<TeLegendEntry>& legVec)
{
	unsigned int i;

	// Update the te_visual table
	TeTable visualTable;
	selectTable("te_visual", "1=0", visualTable);
	TeAttributeList& visualAttrList = visualTable.attributeList();
	TeAttribute& visualAttr0 = visualAttrList[0];		// legend_id
	visualAttr0.rep_.isPrimaryKey_ = true;
	visualAttr0.rep_.isAutoNumber_ = false;
	TeAttribute& visualAttr1 = visualAttrList[1];		// geom_type
	visualAttr1.rep_.isPrimaryKey_ = true;
	visualAttr1.rep_.isAutoNumber_ = false;

	TeGeomRepVisualMap::iterator it;
	for (i = 0; i < legVec.size(); ++i)
	{
		TeLegendEntry& leg = legVec[i];
		for (it = leg.getVisualMap().begin(); it != leg.getVisualMap().end(); ++it)
		{
			string style, contourStyle, sizeValue, width;
			int geomRep = it->first;
			int legId = leg.id();
			TeVisual* visual = it->second;
			TeTableRow row;

			if(geomRep == TePOLYGONS || geomRep == TeCELLS)
			{
				sizeValue = Te2String(0);
				contourStyle = Te2String(visual->contourStyle());
				width =  Te2String(0);
				style = Te2String(it->second->style());
			}
			else if(geomRep == TeLINES)
			{
				sizeValue = Te2String(0);
				contourStyle = Te2String(0);
				width = Te2String(visual->width());
				style = Te2String(visual->style());
			}
			else if(geomRep == TePOINTS)
			{
				sizeValue = Te2String(visual->size());
				contourStyle = Te2String(0);
				width = Te2String(0);
				style = Te2String(visual->style());
			}
			else if(geomRep == TeTEXT)
			{
				sizeValue = Te2String(visual->size());
				contourStyle = Te2String(0);
				width = Te2String(0);
				style = Te2String(0);
			}
		
			row.push_back(Te2String(legId));						// legend_id
			row.push_back(Te2String(geomRep));						// geom_type
			row.push_back(style);									// symb_id
			row.push_back(Te2String(visual->color().red_));			// red
			row.push_back(Te2String(visual->color().green_));		// green
			row.push_back(Te2String(visual->color().blue_));			// blue
			row.push_back(Te2String(visual->transparency()));		// transparency
			row.push_back(width);									// width
			row.push_back(contourStyle);							// contour_symb_id
			row.push_back(Te2String(visual->contourColor().red_));	// contour_red
			row.push_back(Te2String(visual->contourColor().green_));	// contour_green
			row.push_back(Te2String(visual->contourColor().blue_));	// contour_blue
			row.push_back(Te2String(visual->contourTransparency()));	// contour_transp
			row.push_back(Te2String(visual->contourWidth()));		// contour_width
			row.push_back(sizeValue);								// size_value
			row.push_back(Te2String(visual->ptAngle()));				// pt_angle
			row.push_back(visual->family());							// family
			if (visual->bold())										// bold
				row.push_back("1");
			else
				row.push_back("0");
			if (visual->italic())									// italic
				row.push_back("1");
			else
				row.push_back("0");

			row.push_back(Te2String(visual->alignmentVert()));		// alignment_vert
			row.push_back(Te2String(visual->alignmentHoriz()));		// alignment_horiz
			row.push_back(Te2String(visual->tabSize()));				// tab_size
			row.push_back(Te2String(visual->lineSpace()));			// line_space

			if (visual->fixedSize())									// fixed_size
				row.push_back("1");
			else
				row.push_back("0");

			visualTable.add(row);
		}
	}
	return insertTable(visualTable);
}

bool 
TeDatabase::loadLegend (TeAbstractTheme *theme, const string& visualType)
{
	if (!theme)
		return false;
	
	theme->cleanLegend();

	string rest;
	if (theme->id() > 0)
		rest = " te_theme.theme_id = "+  Te2String(theme->id());
	else if (!theme->name().empty())
		rest = " te_theme.name = '"+  theme->name() + "'";
	else
		return false;
	
	//load legend and visual
	string sql = " SELECT ";
	sql += " te_legend.*,  "; // 0 - 6  (7 columns)
	sql += " te_visual.*  "; //  7		(24 columns)
	sql += " FROM ((te_theme LEFT JOIN te_legend ON te_theme.theme_id = te_legend.theme_id ) ";
	sql += " LEFT JOIN te_visual ON te_legend.legend_id = te_visual.legend_id ) ";
	sql += " WHERE "+ rest;
	sql += " ORDER BY te_legend.legend_id, te_legend.group_id, te_visual.geom_type  "; 

	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;
	
	if (!portal->query(sql) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	//load all legends of this theme
	//and its visual
	bool hasLegsToThisTheme = true;
	bool hasNewRow = true;
	while(hasLegsToThisTheme)
	{
		//legend
		TeLegendEntry legend;
		if(!portal->getLegend(legend, 0))
		{
			delete portal;
			return false;
		}

		//visual
		bool hasVisualToThisLeg = true;
		while(hasVisualToThisLeg)
		{
			TeVisual* visual = TeVisualFactory::make(visualType);
			TeGeomRep geomRep;
			if(portal->getVisual(visual, geomRep, 7))
				legend.setVisual(visual, geomRep);
								
			hasNewRow = portal->fetchRow();
			if(!hasNewRow || portal->getInt(2)!= legend.group() || portal->getInt(0)!= legend.id())
				hasVisualToThisLeg = false;
		}

		//Set legend to this theme
		theme->legend(legend); 
														
		//fill legend buffer
		if(legend.group() == -6)	
			metaModel_->legendMap()[legend.id()] = &theme->queryAndPointingLegend();
		else if(legend.group() == -5)	
			metaModel_->legendMap()[legend.id()] = &theme->queryLegend(); 
		else if (legend.group() == -4)
			metaModel_->legendMap()[legend.id()] = &theme->pointingLegend(); 
		else if (legend.group() == -3)
			metaModel_->legendMap()[legend.id()] = &theme->defaultLegend(); 
		else if (legend.group() == -2)
			metaModel_->legendMap()[legend.id()] = &theme->withoutDataConnectionLegend(); 
		else if (legend.group() == -1)
			metaModel_->legendMap()[legend.id()] = &theme->outOfCollectionLegend(); 
		else if (legend.group() == -10) //own legend
		{
			TeLegendEntry* legendTemp = new TeLegendEntry(legend);
			metaModel_->legendMap()[legend.id()] = legendTemp;
		}
		
		if(!hasNewRow || portal->getInt(0)!= theme->id())
			hasLegsToThisTheme = false;
	}
			
	for (unsigned int i = 0; i < theme->legend().size(); ++i)
		metaModel_->legendMap()[theme->legend()[i].id()] = &theme->legend()[i];

	delete portal;
	return true;
}


bool 
TeDatabase::updateProjection (TeProjection *proj)
{
	if (proj->id() <= 0)
		return false;
	string sql;
	sql = "UPDATE te_projection SET ";
	sql += "name='" + proj->name() + "',";
	sql += " long0=" + Te2String(proj->params().lon0*TeCRD,15)+ ",";
	sql += " lat0=" + Te2String(proj->params().lat0*TeCRD,15) + ",";
	sql += " offx=" +Te2String(proj->params().offx,15) + ",";
	sql += " offy=" +Te2String(proj->params().offy,15) + ",";
	sql += " stlat1="+ Te2String(proj->params().stlat1*TeCRD,15) + ",";
	sql += " stlat2=" +Te2String(proj->params().stlat2*TeCRD,15) + ",";
	sql += " unit='" + proj->params().units + "',";
	sql += " scale=" + Te2String(proj->params().scale) + ",";
	sql += " hemis=" + Te2String(proj->params().hemisphere) + ",";
	sql += " datum='" + proj->datum().name() + "',";
	sql += " radius=" + Te2String(proj->datum().radius(),15) + ",";
	sql += " flattening=" + Te2String(proj->datum().flattening(),15) + ",";
	sql += " dx=" + Te2String(proj->datum().xShift(),15) + ",";
	sql += " dy=" + Te2String(proj->datum().yShift(),15) + ",";
	sql += " dz=" + Te2String(proj->datum().zShift(),15) ;
	sql += " WHERE projection_id = " + Te2String(proj->id());
	return this->execute(sql);
}

TeProjection* 
TeDatabase::loadProjection (int projId)
{
	TeDatabasePortal* portal = this->getPortal();

	string sql ="SELECT * FROM te_projection WHERE projection_id = " + Te2String (projId);

	if (!portal->query(sql))
	{
		delete portal;
		return 0;
	}

	// Look for the projection
	if (!portal->fetchRow())
	{
		delete portal;
		return 0;
	}

	TeProjection* proj = 0;
	if(!portal->getProjection(&proj))
	{
		if(proj)
			delete proj;
		delete portal;
		return 0;
	}

	delete portal;
	return proj;
}

bool
TeDatabase::insertPolygonSet(const string& table, TePolygonSet &ps)
{
	if (!beginTransaction())
		return false;
	for (unsigned int i = 0; i < ps.size(); i++ )
	{
		TePolygon& poly = ps [i];
		if (!insertPolygon (table,poly))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::updatePolygonSet (const string& table, TePolygonSet &ps)
{
 	if (!beginTransaction())
		return false;
	for (unsigned int i = 0; i < ps.size(); i++ )
	{
		TePolygon& poly = ps [i];
		if (!updatePolygon (table,poly))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::selectPolygonSet (const string& table, const string& criteria, TePolygonSet &ps)
{
	TeDatabasePortal *portal = this->getPortal();
	string sql ="SELECT * FROM " + table;
	if (!criteria.empty())
		sql += " WHERE " + criteria;
	sql += " ORDER BY object_id ASC, parent_id, num_holes DESC, ext_max ASC";
	 
	if (!portal->query(sql))
	{
		delete portal;
		return false;
	}
	if (!portal->fetchRow())
	{
		delete portal;
		return false;
	}
	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);
		ps.add(poly);
	}
	while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::loadPointSet(TeTheme* theme, TePointSet &ps)
{
	string collTable = theme->collectionTable();
	if (collTable.empty())
		return false;

	TeLayer* themeLayer = theme->layer();
	if (!themeLayer->hasGeometry(TePOINTS))
		return false;
	
	string pointTable = themeLayer->tableName(TePOINTS);
	if (pointTable.empty())
		return false;

	string sql = "SELECT * FROM (" + pointTable + " RIGHT JOIN " + collTable;
	sql = sql + " ON " + pointTable + ".object_id = " + collTable + ".object_id)";

	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	if (!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	bool flag = true;
	do {
		TePoint pt;
		flag =  portal->fetchGeometry(pt);
		ps.add(pt);
	}
	while (flag);
	delete portal;
	return true;
}

bool 
TeDatabase::loadLineSet(TeTheme* theme, TeLineSet &ls)
{
	string collTable = theme->collectionTable();
	if (collTable.empty())
		return false;

	TeLayer* themeLayer = theme->layer();
	if (!themeLayer->hasGeometry(TeLINES))
		return false;
	
	string lineTable = themeLayer->tableName(TeLINES);
	if (lineTable.empty())
		return false;

	string sql = "SELECT * FROM (" + lineTable + " RIGHT JOIN " + collTable;
	sql = sql + " ON " + lineTable + ".object_id = " + collTable + ".object_id)";

	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	if (!portal->query(sql)  || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	bool flag;
	do 
	{
		TeLine2D lin;
		flag = portal->fetchGeometry(lin);
		ls.add(lin);
	}while(flag);
	delete portal;
	return true;
}

bool 
TeDatabase::loadPolygonSet (const string& table, const string& geoid, TePolygonSet &ps)
{
	TeDatabasePortal *portal = this->getPortal();
	string q ="SELECT * FROM " + table;

	if (!geoid.empty())
		q += " WHERE object_id = '" + geoid +"'";
	q += " ORDER BY parent_id, num_holes DESC, ext_max ASC";

	if (!portal->query(q))
	{	
		delete portal;
		return false;
	}

	if (!portal->fetchRow())
	{
		delete portal;
		return false;
	}
	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);
		ps.add(poly);
	}
	while (flag);
	delete portal;
	return true;
}

bool 
TeDatabase::loadPolygonSet (const string& table, TeBox &bb, TePolygonSet &polSet)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	string q;
	q = "SELECT * FROM " + table + " WHERE ";
	q += this->getSQLBoxWhere (bb, TePOLYGONS);
	q += " ORDER BY parent_id, num_holes DESC, ext_max ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);
		polSet.add(poly);
	}
	while (flag);
	delete portal;
	return true;
}

TeDatabasePortal* 
TeDatabase::loadPolygonSet(const string& table, TeBox &box)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return 0;

	string q;
	q = "SELECT * FROM " + table + " WHERE ";
	q += this->getSQLBoxWhere (box, TePOLYGONS);
	q += " ORDER BY parent_id, num_holes DESC, ext_max ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return 0;
	}
	else 
		return portal;
}


bool 
TeDatabase::locatePolygon (const string& table, TeCoord2D &pt, TePolygon &polygon, const double& tol)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);

	string q;
	q = "SELECT * FROM " + table + " WHERE lower_x < " + Te2String(box.x2(),15);
	q += " AND upper_x > " + Te2String(box.x1(),15);
	q += " AND lower_y < " + Te2String(box.y2(),15);
	q += " AND upper_y > " + Te2String(box.y1(),15);
	q += " ORDER BY parent_id, num_holes DESC, ext_max ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);
		if (TeWithin (TePoint(pt), poly))
		{
			polygon = poly;
			delete portal;
			return true;
		}
	}
	while (flag);
	delete portal;
	return false;
}


bool 
TeDatabase::locatePolygonSet (const string& table, TeCoord2D &pt, double tol, TePolygonSet &polygons)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);

	string q;
	q = "SELECT * FROM " + table + " WHERE lower_x < " + Te2String(box.x2(),6);
	q += " AND upper_x > " + Te2String(box.x1(),6);
	q += " AND lower_y < " + Te2String(box.y2(),6);
	q += " AND upper_y > " + Te2String(box.y1(),6);
	q += " ORDER BY parent_id, num_holes DESC, ext_max ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	bool flag = true;
	polygons.clear();
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);
		if (TeWithin (TePoint(pt), poly))
			polygons.add(poly);
	}
	while (flag);
	delete portal;

	if(polygons.size())
		return (true);
	return false;
}

bool 
TeDatabase::loadPolygonSet(TeTheme* theme, TePolygonSet &ps)
{
	string collTable = theme->collectionTable();
	if (collTable.empty())
		return false;

	TeLayer* themeLayer = theme->layer();
	if (!themeLayer->hasGeometry(TePOLYGONS))
		return false;
	
	string polygonTable = themeLayer->tableName(TePOLYGONS);
	if (polygonTable.empty())
		return false;

	string sql = "SELECT * FROM (" + polygonTable + " RIGHT JOIN " + collTable;
	sql = sql + " ON " + polygonTable + ".object_id = " + collTable + ".object_id)";
	sql += " ORDER BY " + polygonTable + ".parent_id, ";
	sql += polygonTable + ".num_holes DESC, " + polygonTable + ".ext_max ASC";

	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	if (!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);
		ps.add ( poly );
	}
	while (flag);		
	delete portal;
	return true;
}

bool 
TeDatabase::insertLineSet	(const string& table, TeLineSet &ls)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < ls.size(); i++ )
	{
		TeLine2D& line = ls [i];
		if (!insertLine (table,line))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::updateLineSet	(const string& table, TeLineSet &ls)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < ls.size(); i++ )
	{
		TeLine2D line = ls [i];
		if (!updateLine (table,line))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::loadLineSet (const string& table, const string& geoid, TeLineSet &ls)
{
	TeDatabasePortal *portal = this->getPortal();

	string q ="SELECT * FROM " + table;

	if (!geoid.empty())
		q += " WHERE object_id = '" + geoid +"'";

	q += " ORDER BY ext_max DESC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	bool flag = true;
	do 
	{
		TeLine2D line;
		flag = portal->fetchGeometry(line);
		ls.add ( line );
	}while(flag);

	delete portal;
	return true;
}

bool 
TeDatabase::loadLineSet (const string& table, TeBox &bb, TeLineSet &linSet)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	string q;
	q = "SELECT * FROM " + table + " WHERE ";
	q += this->getSQLBoxWhere (bb, TeLINES);
	q += " ORDER BY ext_max DESC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	bool flag = true;
	do
	{
		TeLine2D lin;
		flag = portal->fetchGeometry(lin);
		linSet.add(lin);
	}
	while (flag);
	delete portal;
	return true;
}

TeDatabasePortal* 
TeDatabase::loadLineSet (const string& table, TeBox &box)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return 0;

	string q;
	q = "SELECT * FROM " + table + " WHERE ";
	q += this->getSQLBoxWhere (box, TeLINES);
	q += " ORDER BY ext_max DESC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return 0;
	}
	return portal;
}

bool 
TeDatabase::selectLineSet (const string& table, const string& criteria, TeLineSet &ls)
{
	TeDatabasePortal *portal = this->getPortal();
	string q ="SELECT * FROM " + table;
	if (!criteria.empty())
		q += " WHERE " + criteria;
	q += " ORDER BY object_id ASC, geom_id ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	bool flag = true;
	do 
	{
		TeLine2D line;
		flag = portal->fetchGeometry(line);
		ls.add ( line );
	}while(flag);

	delete portal;
	return true;
}

bool 
TeDatabase::locateLine (const string& table, TeCoord2D &pt, TeLine2D &line, const double& tol)
{
	TeDatabasePortal* portal = this->getPortal();

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE lower_x < %f AND upper_x > %f AND lower_y < %f AND upper_y > %f",
		box.x2(),box.x1(),box.y2(),box.y1());
	q += buf;
	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	// Get all lines
	TeLineSet ls;
	int k;
	bool flag = true;
	do 
	{
		TeLine2D l;
		flag = portal->fetchGeometry( l );
		ls.add ( l );
	} while (flag);

	delete portal;

	TeCoord2D paux;

	if (TeNearest (pt, ls, k, paux, tol))
	{
		line = ls[k];
		return true;
	}
	return false;
}

bool 
TeDatabase::insertPointSet	(const string& table, TePointSet &ps)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < ps.size(); i++ )
	{
		TePoint& point = ps [i];
		if (!insertPoint (table,point))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::updatePointSet (const string& table, TePointSet &ps)
{
	if (!beginTransaction())
		return false;
	
	for (unsigned int i = 0; i < ps.size(); i++ )
	{
		TePoint point = ps [i];
		if (!updatePoint (table,point))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::loadPointSet (const string& table, const string& geoid, TePointSet &ps)
{
	TeDatabasePortal* portal = this->getPortal();
	string q ="SELECT * FROM " + table;

	if (!geoid.empty())
		q += " WHERE object_id = '" + geoid +"'";

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	do 
	{
		TePoint point;
		flag = portal->fetchGeometry (point);
		ps.add ( point );
	}while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::loadPointSet (const string& table, TeBox &bb, TePointSet &ps)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return false;

	string q;
	q = "SELECT * FROM " + table + " WHERE ";
	q += this->getSQLBoxWhere (bb, TePOINTS);

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}
	bool flag = true;
	do
	{
		TePoint pt;
		flag = portal->fetchGeometry(pt);
		ps.add(pt);
	}
	while (flag);
	delete portal;
	return true;
}

TeDatabasePortal* 
TeDatabase::loadPointSet(const string& table, TeBox &box)
{
	TeDatabasePortal *portal = this->getPortal();
	if (!portal)
		return 0;

	string q;
	q = "SELECT * FROM " + table + " WHERE ";
	q += this->getSQLBoxWhere (box, TePOINTS);

	if (!portal->query(q) || !portal->fetchRow())
	{	
		delete portal;
		return 0;
	}
	return portal;
}

bool 
TeDatabase::selectPointSet (const string& table, const string& criteria, TePointSet &ps)
{
	TeDatabasePortal* portal = this->getPortal();
	string q ="SELECT * FROM " + table;
	if (!criteria.empty())
		q += " WHERE " + criteria;
	q += " ORDER BY object_id ASC, geom_id ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	do 
	{
		TePoint point;
		flag = portal->fetchGeometry (point);
		ps.add ( point );
	}while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::locatePoint (const string& table, TeCoord2D &pt, TePoint &point, const double& tol)
{
	TeDatabasePortal* portal = this->getPortal();

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE x < %f AND x > %f AND y < %f AND y > %f",
		box.x2(),box.x1(),box.y2(),box.y1());
	q += buf;

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	TePointSet ps;
	bool flag = true;
	do 
	{
		TePoint point;
		flag = portal->fetchGeometry (point);
		ps.add ( point );
	}while (flag);

	delete portal;
	int k;
	if (TeNearest (pt, ps, k, tol))
	{
		point = ps[k];
		return true;
	}
	return false;
}

bool 
TeDatabase::insertTextSet	(const string& table, TeTextSet &ts)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < ts.size(); i++ )
	{
		TeText& text = ts [i];
		if (!insertText (table,text))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::updateTextSet	(const string& table, TeTextSet &ts)
{
	if (!beginTransaction())
		return false;
	for ( unsigned int i = 0; i < ts.size(); i++ )
	{
		TeText text = ts [i];
		if (!updateText (table,text))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::loadTextSet (const string& table, const string& geoid, TeTextSet &ts)
{
	TeDatabasePortal *portal = this->getPortal();
	
	string q ="SELECT * FROM " + table;
	if (!geoid.empty())
		q += " WHERE object_id = '" + geoid +"'";

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	// Look for all texts
	bool flag  = true;
	do
	{
		TeText p;
		flag = portal->fetchGeometry(p);
		ts.add ( p );
	} while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::selectTextSet (const string& table, const string& criteria, TeTextSet &ts)
{
	TeDatabasePortal* portal = this->getPortal();
	string q ="SELECT * FROM " + table;
	if (!criteria.empty())
		q += " WHERE " + criteria;
	q += " ORDER BY object_id ASC, geom_id ASC";

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	// Look for all texts
	bool flag = true;
	do
	{
		TeText p;
		flag = portal->fetchGeometry(p);
		ts.add ( p );
	} while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::updateText(const string& table, TeText &t)
{
	string sql;
	sql = "UPDATE " + table + " SET ";
    sql += "x=" + Te2String(t.location().x(),15) + ", ";
	sql += "y=" + Te2String(t.location().y(),15) + ", ";
	sql += "text_value='" + t.textValue() + "', ";
	sql += "angle=" + Te2String(t.angle(),15) + ", ";
	sql += "height=" + Te2String(t.height(),15) + ",";
	sql += "alignment_vert=" + Te2String(t.alignmentVert(),15) + ",";
	sql += "alignment_horiz=" + Te2String(t.alignmentHoriz(),15);
    sql += " WHERE geom_id=" + Te2String(t.geomId());
	return (this->execute(sql));
}

bool 
TeDatabase::updateNode(const string& table, TeNode &node)
{
	string sql;
	sql = "UPDATE " + table + " SET ";
    sql += "x=" + Te2String(node.location().x(),15) + ", ";
	sql += "y=" + Te2String(node.location().y(),15);
    sql += " WHERE geom_id = " + Te2String(node.geomId());
	return (this->execute(sql));
}

bool 
TeDatabase::updatePoint(const string& table, TePoint &p)
{
	string sql;
	sql = "UPDATE " + table + " SET ";
    sql += "x=" + Te2String(p.location().x(),15) + ", ";
	sql += "y=" + Te2String(p.location().y(),15);
    sql += " WHERE geom_id = " + Te2String(p.geomId());
	return (this->execute(sql));
}

bool TeDatabase::locateText (const string& table, TeCoord2D &pt, TeText &text, const double& tol)
{
	TeDatabasePortal* portal = this->getPortal();

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE x < %f AND x > %f AND y < %f AND y > %f",
		box.x2(),box.x1(),box.y2(),box.y1());
	q += buf;
	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	// Look for all texts
	bool flag = true;
	TeTextSet ts;
	do
	{
		TeText p;
		flag = portal->fetchGeometry(p);
		ts.add ( p );
	} while (flag);

	delete portal;

	int k;
	if (TeNearest (pt, ts, k, tol))
	{
		text = ts[k];
		return true;
	}
	return false;
}

bool 
TeDatabase::insertArcSet	(const string& table, TeArcSet &as)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < as.size(); i++ )
	{
		TeArc& arc = as [i];
		if (!insertArc (table,arc))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::updateArcSet	(const string& table, TeArcSet &as)
{
	if (!beginTransaction())
		return false;

	for ( unsigned int i = 0; i < as.size(); i++ )
	{
		TeArc arc = as [i];
		if (!updateArc (table,arc))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::loadArcSet (const string& table, const string& geoid, TeArcSet &as)
{

	TeDatabasePortal* portal = this->getPortal();
	string q ="SELECT * FROM " + table;
	if (!geoid.empty())
		q += " WHERE object_id = " + geoid;

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	// Look for all nodes
	bool flag;
	do 
	{
		TeArc arc;
		flag = portal->fetchGeometry(arc);
		as.add (arc);
	} while (flag);
	delete portal;
	return true;
}

bool 
TeDatabase::updateArc(const string& table, TeArc &arc)
{
	string sql;
	sql = "UPDATE"+ table +" SET ";
    sql += "from_node=" + Te2String(arc.fromNode().geomId()) + ", ";
	sql += "to_node=" + Te2String(arc.toNode().geomId());
    sql += " WHERE geom_id = " + Te2String(arc.geomId());
	return (this->execute(sql));
}

bool 
TeDatabase::insertNodeSet	(const string& table, TeNodeSet &ns)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < ns.size(); i++ )
	{
	     TeNode& no = ns [i];
	     if (!insertNode (table,no))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::updateNodeSet	(const string& table, TeNodeSet &ns)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < ns.size(); i++ )
	{
		TeNode no = ns [i];
		if (!updateNode (table,no))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::loadNodeSet (const string& table, const string& geoid, TeNodeSet &ns)
{
	TeDatabasePortal* portal = this->getPortal();
	string q ="SELECT * FROM " + table;
	if (!geoid.empty())
		q += " WHERE object_id = " + geoid;

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	bool flag = true;
	do 
	{
		TeNode n;
		flag = portal->fetchGeometry(n);
		ns.add ( n );
	}while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::insertCellSet	(const string& table, TeCellSet &cs)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < cs.size(); i++ )
	{
		TeCell& cell = cs [i];
		if (!insertCell (table,cell))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool
TeDatabase::updateCellSet	(const string& table, TeCellSet &cs)
{
	if (!beginTransaction())
		return false;

	for (unsigned int i = 0; i < cs.size(); i++ )
	{
		TeCell cell = cs [i];
		if (!updateCell (table,cell))
		{
			rollbackTransaction();
			return false;
		}
	}
	if (!commitTransaction())
	{
		rollbackTransaction();
		return false;
	}
	return true;
}

bool 
TeDatabase::loadCellSet (const int& layerId, const string& table, const string& geoid, TeCellSet &cs)
{

	TeDatabasePortal *portal = this->getPortal();

	// Get the cell set resolution
	string q  = "SELECT * FROM te_representation WHERE layer_id = " + Te2String(layerId);
	q += " AND geom_type = " + Te2String(TeCELLS);

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	cs.resX(portal->getDouble("res_x"));
	cs.resY(portal->getDouble("res_y"));
	
	portal->freeResult();
		
	q = "SELECT * FROM " + table;
	if (!geoid.empty())
		q += " WHERE object_id = '" + geoid +"'";
	q += " ";

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	bool flag;
	do 
	{
		TeCell cell;
		flag = portal->fetchGeometry(cell);
		cs.add ( cell );
	} while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::selectCellSet (const int& layerId, const string& table, const string& criteria, TeCellSet &cs)
{
	TeDatabasePortal* portal = this->getPortal();
	string q = "SELECT * FROM te_representation WHERE layer_id = " ;
	q += Te2String(layerId) + " AND geom_type = " + Te2String(TeCELLS);

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	cs.resX(portal->getDouble("res_x"));
	cs.resY(portal->getDouble("res_y"));
	portal->freeResult();
	
	q ="SELECT * FROM " + table;
	if (!criteria.empty())
		q += " WHERE " + criteria;

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	bool flag;
	do 
	{
		TeCell cell;
		flag = portal->fetchGeometry(cell);
		cs.add ( cell );
	} while (flag);

	delete portal;
	return true;
}

bool 
TeDatabase::updateCell(const string& table, TeCell &c)
{
	TeBox b = c.box();

	string sql;
	sql = "UPDATE "+ table +"  SET ";
	sql += "lower_x=" + Te2String(b.lowerLeft().x(),15) + ", ";
	sql += "lower_y=" + Te2String(b.lowerLeft().y(),15) + ", ";
	sql += "upper_x=" + Te2String(b.upperRight().x(),15) + ", ";
	sql += "upper_y=" + Te2String(b.upperRight().y(),15) + ", ";
	sql += "col_number=" + Te2String(c.column()) + ", ";
	sql += "row_number=" + Te2String(c.line());
	sql += " WHERE geom_id = " + Te2String(c.geomId());
	return (this->execute(sql));
}

bool 
TeDatabase::locateCell (const string& table, TeCoord2D &pt, TeCell &cell, const double& /* tol */)
{
	TeDatabasePortal* portal = this->getPortal();
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE lower_x < %f AND upper_x > %f AND lower_y < %f AND upper_y > %f",
		pt.x(),pt.x(),pt.y(),pt.y());
	q += buf;
	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	portal->fetchGeometry(cell);
	delete portal;
	return true;
}

bool
TeDatabase::inClauseValues(const string& query, const string& /* attribute */, vector<string>& inClauseVector)
{
	inClauseVector.push_back( "(" + query + ")" );
	return true;
}

//Spatial query
//retornam um portal

bool 
TeDatabase::spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, TeDatabasePortal *portal, int relate, const string& actCollTable)
{
	return (TeTopologicalRelation(actGeomTable, actRep, actIdsIn, portal, relate, actCollTable));
}

bool 
TeDatabase::spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, const string& visGeomTable, TeGeomRep visRep, TeDatabasePortal *portal, int relate, const string& visCollTable)
{
	return (TeTopologicalRelation(actGeomTable, actRep, actIdsIn, visGeomTable, visRep, portal, relate, visCollTable));
}

bool 
TeDatabase::spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, TeDatabasePortal *portal, int relate, const string& actCollTable)
{
	return (TeTopologicalRelation(actGeomTable, actRep, geom, portal, relate, actCollTable));
}

//retornam um vetor de object_ids resultantes da consulta
bool 
TeDatabase::spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, TeKeys& actIdsOut, int relate, const string& actCollTable)
{
	return (TeTopologicalRelation(actGeomTable, actRep, actIdsIn, actIdsOut, this, relate, actCollTable));
}

bool 
TeDatabase::spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, const string& visGeomTable, TeGeomRep visRep, TeKeys& visIdsOut, int relate, const string& visCollTable, TeDatabase* dbVis)
{
	return (TeTopologicalRelation(actGeomTable, actRep, actIdsIn, visGeomTable, visRep, visIdsOut, this, relate, visCollTable, dbVis));
}

bool 
TeDatabase::spatialRelation(const string& actGeomTable, TeGeomRep actRep, TeGeometry* geom, TeKeys& actIdsOut, int relate, const string& actCollTable)
{
	return (TeTopologicalRelation(actGeomTable, actRep, geom, actIdsOut, this, relate, actCollTable));
}

//metric functions
bool
TeDatabase::calculateArea(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn, double &area)
{
	return (TeGetArea(actGeomTable, actRep, actIdsIn, this, area));
}

bool 
TeDatabase::calculateLength(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIdsIn , double& length )
{
	return (TeGetLength(actGeomTable, actRep, actIdsIn, this, length));
}

bool 
TeDatabase::calculateDistance(const string& actGeomTable, TeGeomRep actRep, TeKeys& Ids, double& distance)
{
	return (TeGetDistance(actGeomTable, actRep, Ids, this, distance));
}

bool 
TeDatabase::calculateDistance(const string& /* actGeomTable */, TeGeomRep /*actRep */, const string& /* objId1 */, const string& /* visGeomTable */, TeGeomRep /* visRep */, const string& /* objId2 */, double& /* distance */)
{
	return false;
}

bool 
TeDatabase::withinDistance(const string& actGeomTable, TeGeomRep actRep, const TeCoord2D& coord, TeKeysToDist& IdsDistOut, const double& max_distance, const string& actCollTable)
{
	return (TeGetWithinDistance(actGeomTable, actRep, coord, IdsDistOut, this, max_distance, actCollTable));
}

// functions that generate new geometry
bool 
TeDatabase::buffer(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TePolygonSet& bufferSet, double dist)
{
	return (TeGetBuffer(actGeomTable, actRep, actIds, this, bufferSet, dist));
}

bool 
TeDatabase::centroid(const string&  actGeomTable , TeGeomRep actRep, TePointSet& centroidSet, TeKeys actIds, const string& actCollTable)
{
	return (TeGetCentroid(actGeomTable, actRep, this, centroidSet, actIds, actCollTable));
}

bool 
TeDatabase::convexHull(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TePolygonSet& convexHullSet)
{
	return (TeGetConvexHull(actGeomTable, actRep, actIds, this, convexHullSet));
}

bool 
TeDatabase::nearestNeighbors(const string& /* actGeomTable */, const string& /* actCollTable */, TeGeomRep /* actRep */, const string& /* objId1 */, TeKeys& /* actIdsOut */, int /* numRes */)
{
	return false;
}

bool 
TeDatabase::nearestNeighbors(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* visGeomTable */, const string& /* visCollTable */, TeGeomRep /* visRep */, TeKeys& /* visIdsOut */, int /* numRes */)
{
	return false;
}

bool 
TeDatabase::nearestNeighbors(const string& /* actGeomTable */, const string& /* actCollTable */, TeGeomRep /* actRep */, const string& /* objId1 */, TeDatabasePortal* /* portal */, int /* numRes */)
{
	return false;
}
	
bool 
TeDatabase::nearestNeighbors(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* visGeomTable */, const string& /* visCollTable */, TeGeomRep /* visRep */, TeDatabasePortal* /* portal */, int /* numRes */)
{
	return false;
}

bool 
TeDatabase::geomIntersection(const string& actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeGeometryVect& geomVect)
{
	return (TeGetOverlay(actGeomTable, actRep, actIds, this, geomVect, TeINTERSECTION));
}
	
bool 
TeDatabase::geomIntersection(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* visGeomTable */, TeGeomRep /* visRep */, const string& /* objId2 */, TeGeometryVect& /* geomVect */)
{
	return false;
}

bool 
TeDatabase::geomDifference(const string& actGeomTable, TeGeomRep actRep, const string& objId1, const string& objId2, TeGeometryVect& geomVect)
{
	TeKeys actIds;
	actIds.push_back(objId1);
	actIds.push_back(objId2);

	return (TeGetOverlay(actGeomTable, actRep, actIds, this, geomVect, TeDIFFERENCE));

}

bool 
TeDatabase::geomDifference(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* visGeomTable */, TeGeomRep /* visRep */, const string& /* objId2 */, TeGeometryVect& /* geomVect */)
{
	return false;
}

bool 
TeDatabase::geomUnion(const string&  actGeomTable, TeGeomRep actRep, TeKeys& actIds, TeGeometryVect& geomVect)
{
    return (TeGetOverlay(actGeomTable, actRep, actIds, this, geomVect, TeUNION));
}

bool 
TeDatabase::geomUnion(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* visGeomTable */, TeGeomRep /* visRep */, const string& /* objId2 */, TeGeometryVect& /* geomVect */)
{
	return false;
}

bool 
TeDatabase::geomXOr(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* objId2 */, TeGeometryVect& /* geomVect */)
{
	return false;
}

bool 
TeDatabase::geomXOr(const string& /* actGeomTable */, TeGeomRep /* actRep */, const string& /* objId1 */, const string& /* visGeomTable */, TeGeomRep /* visRep */, const string& /* objId2 */, TeGeometryVect& /* geomVect */)
{
	return false;
}

// Operation with Image

bool 
TeDatabase::zonal(const string& rasterTable, const string& actGeomTable, TeKeys& Ids, TeObjectStatistics& result)
{
	TeDatabasePortal* portal=getPortal();
	if (!portal)
		return false;
	
	//recuperar o raster!!!
	string sql = "SELECT layer_id FROM te_representation";
	sql += " WHERE geom_table = '" + rasterTable +"'";
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	int layerId = atoi(portal->getData(0));
	TeRaster* raster = loadLayerRaster(layerId);

	//recuperar as geometrias
	portal->freeResult();
	
	string objIds = getStringIds(Ids);
	sql = "SELECT * FROM "+ actGeomTable;
	sql+= " WHERE object_id IN ("+ objIds +")";

	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);

		TeStatisticsDimensionVect st;

		TeRaster::iteratorPoly itBegin = raster->begin(poly, TeBoxPixelIn);
		TeRaster::iteratorPoly itEnd = raster->end(poly, TeBoxPixelIn);
	
		if(!TeCalculateStatistics (itBegin, itEnd, st))
		{
			delete portal;
			return false;
		}
	
		result[poly.objectId()] = st;

	}while (flag);

	
	delete raster;
	delete portal;
	return true;
}
	
bool 
TeDatabase::zonal(const string& rasterTable, const string& actGeomTable, const string& actCollTable, TeObjectStatistics& result)
{
	TeDatabasePortal* portal=getPortal();
	if (!portal)
		return false;
	
	//recuperar o raster
	string sql = "SELECT layer_id FROM te_representation";
	sql += " WHERE geom_table = '" + rasterTable +"'";
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	int layerId = atoi(portal->getData(0));
	TeRaster* raster = loadLayerRaster(layerId);

	//recuperar as geometrias
	portal->freeResult();
	
	sql = "SELECT * FROM "+ actGeomTable;

	if(!actCollTable.empty())
	{
		sql += " ,"+ actCollTable;
		sql += " WHERE object_id = c_object_id ";
	}
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	do
	{
		TePolygon poly;
		flag = portal->fetchGeometry(poly);

		TeStatisticsDimensionVect st;
		
		TeRaster::iteratorPoly itBegin = raster->begin(poly, TeBoxPixelIn);
		TeRaster::iteratorPoly itEnd = raster->end(poly, TeBoxPixelIn);
	
		if(!TeCalculateStatistics (itBegin, itEnd, st))
		{
			delete portal;
			return false;
		}
		result[poly.objectId()] = st;

	}while (flag);

	
	delete raster;
	delete portal;
	return true;
}

bool 
TeDatabase::zonal(const string& rasterTable, TePolygon& poly, TeStatisticsDimensionVect& result)
{
	TeDatabasePortal* portal=getPortal();
	if (!portal)
		return false;
	
	//recuperar o raster!!!
	string sql = "SELECT layer_id FROM te_representation";
	sql += " WHERE geom_table = '" + rasterTable +"'";
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	int layerId = atoi(portal->getData(0));
	TeRaster* raster = loadLayerRaster(layerId);

	delete portal;

	TeRaster::iteratorPoly itBegin = raster->begin(poly, TeBoxPixelIn);
	TeRaster::iteratorPoly itEnd = raster->end(poly, TeBoxPixelIn);
	
	if(!TeCalculateStatistics (itBegin, itEnd, result))
		return false;

	delete raster;
	return true;
}


bool 
TeDatabase::mask(const string& rasterTable, const string& actGeomTable, const string& objId, const string& nameLayerOut, TeStrategicIterator st)
{
	TeDatabasePortal* portal=getPortal();
	if (!portal)
		return false;
	
	//recuperar o raster!!!
	string sql = "SELECT layer_id FROM te_representation";
	sql += " WHERE geom_table = '" + rasterTable +"'";
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	int layerId = atoi(portal->getData(0));
	TeRaster* raster = loadLayerRaster(layerId);

	//recuperar a geometria
	portal->freeResult();
	
	sql = "SELECT * FROM "+ actGeomTable;
	sql+= " WHERE object_id = '" + objId + "'"; 
	
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	TePolygon poly;
	portal->fetchGeometry(poly);
	delete portal;

	TeRaster* rasterOut = TeMask (raster, poly, st);
	if(!rasterOut)
		return false;

	TeLayer* res = TeImportRaster(nameLayerOut, rasterOut, this);
	delete raster;
	delete rasterOut;
	return (res != 0);
}



bool
TeDatabase::mask(const string& rasterTable, TePolygon& poly, const string& nameLayerOut, TeStrategicIterator st)
{
	TeDatabasePortal* portal=getPortal();
	if (!portal)
		return false;
	
	string sql = "SELECT layer_id FROM te_representation";
	sql += " WHERE geom_table = '" + rasterTable +"'";
	if(!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	TeRaster* raster = loadLayerRaster(atoi(portal->getData(0)));
	delete portal;

	TeRaster* rasterOut = TeMask (raster, poly, st);
	if(!rasterOut)
		return false;

	TeLayer* res = TeImportRaster(nameLayerOut, rasterOut, this);
	delete raster;
	delete rasterOut;
	return (res != 0);
}


string
TeDatabase::getSQLBoxWhere (TeBox &box, TeGeomRep rep)
{
	string wherebox;
	string lowerX, lowerY, upperX, upperY;
	if(rep == TePOLYGONS || rep == TeLINES || rep == TeCELLS || rep == TeRASTER || rep == TeRASTERFILE)
	{
		lowerX = "lower_x";
		lowerY = "lower_y";
		upperX = "upper_x";
		upperY = "upper_y";
	}
	else if(rep == TePOINTS || rep == TeTEXT)
	{
		lowerX = "x";
		lowerY = "y";
		upperX = "x";
		upperY = "y";
	}
	wherebox = "NOT("+ lowerX +" > " + Te2String(box.x2_, 12) + " OR ";
	wherebox += upperX +" < " + Te2String(box.x1_, 12) + " OR ";
	wherebox += lowerY +" > " + Te2String(box.y2_, 12) + " OR ";
	wherebox += upperY +" < " + Te2String(box.y1_, 12) + ")";
	return wherebox;
}


string 
TeDatabase::getSQLBoxWhere (const string& table1, const string& table2, TeGeomRep rep2, TeGeomRep /* rep1 */)
{
	string wherebox;

	if(rep2 == TePOLYGONS || rep2 == TeLINES || rep2 == TeCELLS || rep2 == TeRASTER || rep2 == TeRASTERFILE)
	{
		wherebox = " NOT( ";
		wherebox += table2 +".lower_x > "+ table1 +".upper_x  OR ";
		wherebox += table2 +".upper_x < "+ table1 +".lower_x  OR ";
		wherebox += table2 +".lower_y > "+ table1 +".upper_y  OR ";
		wherebox += table2 +".upper_y < "+ table1 +".lower_y )";
	}
	else if(rep2 == TePOINTS || rep2 == TeTEXT)
	{
		wherebox = " NOT( ";
		wherebox += table2 +".x > "+ table1 +".upper_x  OR ";
		wherebox += table2 +".x < "+ table1 +".lower_x  OR ";
		wherebox += table2 +".y > "+ table1 +".upper_y  OR ";
		wherebox += table2 +".y < "+ table1 +".lower_y )";
	}
	
	return wherebox;
}


string 
TeDatabase::getSQLBoxSelect (const string& tableName, TeGeomRep /* rep */)
{
	string sql = tableName +".* ";
	return sql;
}


string
TeDatabase::getSQLStatistics (TeGroupingAttr& attrs)
{
	string sql = "";
	string virg = "";

	TeGroupingAttr::iterator it = attrs.begin();
	int count = 0;
	while(it != attrs.end())
	{
		if(count>0)
			virg = ",";

		switch ((*it).second)
		{
			case TeSUM:
				sql += virg +" SUM( "+ (*it).first.name_ +") AS SUM_"+ Te2String(count);
				(*it).second = TeNOSTATISTIC;
				++count;
				break;
			case TeMAXVALUE:
				sql += virg +" MAX( "+ (*it).first.name_ +") AS MAX_"+ Te2String(count);
				(*it).second = TeNOSTATISTIC; 
				++count;
				break;
			case TeMINVALUE:
				sql += virg +" MIN( "+ (*it).first.name_ +") AS MIN_"+ Te2String(count);
				(*it).second = TeNOSTATISTIC;
				++count;
				break;
			case TeCOUNT:
				sql += virg +" COUNT( "+ (*it).first.name_ +") AS COUNT_"+ Te2String(count);
				(*it).second = TeNOSTATISTIC;
				++count;
				break;
			case TeMEAN:
				sql += virg +" AVG( "+ (*it).first.name_ +") AS AVG_"+ Te2String(count);
				(*it).second = TeNOSTATISTIC;
				++count;
				break;
			default:
				break;
		}
		++it;
	}
	return sql;
}

string 
TeDatabase::getSQLAutoNumber(const string& /* table */)
{
	return "";
}

string 
TeDatabase::getSQLTemporalWhere (TeTimeInterval& timeInterval, TeTemporalRelation timeOperator, const string& initialTime, const string& finalTime)
{
	string sql;
	string t1 = getSQLTime(timeInterval.getT1()); 
	string t2 = getSQLTime(timeInterval.getT2());
	
	switch(timeOperator)
	{
		case TeTIMEBEFORE:  
			sql = finalTime +" < "+ t1;
			break;
		case TeTIMEAFTER:  
			sql = initialTime +" > "+ t2;
			break;
		case TeTIMEEQUAL:             
			sql = "( "+ initialTime +" >= "+ t1;
			sql += " AND "+ initialTime +" <= "+ t2 +" )";
			if (initialTime != finalTime)
			{
				sql += " AND ";
				sql += "( "+ finalTime +" >= "+ t1;
				sql += " AND "+ finalTime +" <= "+ t2 +" )";
			}
			break;

		case TeTIMEMEETS:             
			sql = finalTime +" = "+ t1;
			sql += " OR "+ initialTime +" = "+ t2;
			break;

		case TeTIMEDURING: 
			sql = initialTime +" >= "+ t1;
			sql += " AND "+ initialTime +" <= "+ t2;
			if (initialTime != finalTime)
			{
				sql += " AND "+ finalTime +" >= "+ t1;
				sql += " AND "+ finalTime +" <= "+ t2;
			}
			break;

		case TeTIMEOVERLAPS:             
			sql = "( "+ initialTime +" < "+ t1;
			sql += " AND "+ finalTime +" > "+ t1;
			sql += " AND "+ finalTime +" < "+ t2 +" )";
			sql += " OR ";
			sql += "( "+ initialTime +" > "+ t1;
			sql += " AND "+ initialTime +" < "+ t2;
			sql += " AND "+ finalTime +" > "+ t2 +" )";
			break;

		case TeTIMEENDS:
			sql = finalTime +" = "+ t2;
			break;

		case TeTIMESTARTS:
			sql = initialTime +" = "+ t1;
			break;

        default:
            break;
	}

	return sql; 
}

string
TeDatabase::getSQLTemporalWhere(int time1, int time2, TeChronon chr, TeTemporalRelation rel, 
						   const string& initialTime, const string& finalTime)
{
	//rever os chronons definidos - alterar o parser para restri��o temporal
	string func, sql;
	switch(chr)
	{
		case TeSECONDOFMINUTE:  
		   func = " second"; 
		break;
		
		case TeMINUTEOFHOUR:  
		   func = " minute";
		break;
		
		case TeHOUROFDAY:             
			func = " hour";
		break;
		
		case TeDAYOFMONTH:             
			func = " day";
		break;

		case TeDAYOFWEEK:
			func = " weekday";
		break;
		
		case TeMONTHOFYEAR:
		   func = " month";
		break;

		case TeYEAR:             
		   func = " year";
		break;

		default:
			return "";
	}

	switch(rel)
	{
		case TeTIMEBEFORE:  
			sql = func +"("+ finalTime +") < "+ Te2String(time1);
		break;
		
		case TeTIMEAFTER:  
			sql = func +"("+ initialTime +") > "+ Te2String(time2);
		break;
		
		case TeTIMEEQUAL:  
			sql = func +"("+ initialTime +") = "+ Te2String(time1);
			sql += " AND "+ func +"(" + finalTime +") = "+ Te2String(time2);
		break;

		case TeTIMEMEETS:     
			sql = func +"("+ finalTime +") = "+ Te2String(time1);
			sql += " OR "+ func +"(" + initialTime +") = "+ Te2String(time2);
		break;

		case TeTIMEDURING: 
			sql = func +"("+ initialTime +") >= "+ Te2String(time1);
			sql += " AND "+ func +"("+ initialTime +") <= "+ Te2String(time2);
			sql += " AND "+ func +"("+ finalTime +") >= "+ Te2String(time1);
			sql += " AND "+ func +"("+ finalTime +") <= "+ Te2String(time2);
		break;

		case TeTIMEOVERLAPS:         
			sql =  "("+ func +"("+ initialTime +") <= "+ Te2String(time1);
			sql += " AND "+ func +"("+ finalTime +") >= "+ Te2String(time1);
			sql += " AND "+ func +"("+ finalTime +") <= "+ Te2String(time2) +")";
			sql += " OR ";
			sql += "("+ func +"("+ initialTime +") >= "+ Te2String(time1);
			sql += " AND "+ func +"("+ initialTime +") <= "+ Te2String(time2);
			sql += " AND "+ func +"("+ finalTime +") >= "+ Te2String(time2) +")";
		break;

		case TeTIMEENDS:
			sql = func +"("+ finalTime +") = "+ Te2String(time2);
		break;

		case TeTIMESTARTS:
			sql = func +"("+ initialTime +") = "+ Te2String(time1);

		default:
			break;
	}
	
	return sql;
}

string 
TeDatabase::getSQLTemporalWhere (const string& temporalRest)
{
	string result, tableName, initialCol, finalCol, time1, time2, mask; 
	TeTemporalRelation rel = TeTIMEUNDEFINED;
	TeChronon chronon = TeNOCHRONON; 
	
	string temp = temporalRest;
	bool flag = true;
	int cont = 0;
		
	while(flag)
	{
		string element;
		int pos = temp.find (";");
		if(pos<0)
		{
			flag = false;
			element = temp;
		}
		else
		{
			element = temp.substr(0, pos);
			temp = temp.substr (pos+1);
		}

		if(cont==0) //table name
			tableName = element;
		else if(cont==1) //column name (initial time) 
			initialCol = element; 
		else if(cont==2) //column name (final time)
			finalCol = element;
		else if(cont==3) //TeTemporalRelation 
		{
			if(element=="TeTIMEEQUAL")
				rel = TeTIMEEQUAL;
			else if (element=="TeTIMEBEFORE")
				rel = TeTIMEBEFORE;
			else if (element=="TeTIMEAFTER")
				rel = TeTIMEAFTER;
			else if (element=="TeTIMEMEETS")
				rel = TeTIMEMEETS;
			else if (element=="TeTIMEDURING")
				rel = TeTIMEDURING;
			else if (element=="TeTIMEOVERLAPS")
				rel = TeTIMEOVERLAPS;
			else if (element=="TeTIMEENDS")
				rel = TeTIMEENDS;
			else if (element=="TeTIMESTARTS")
				rel = TeTIMESTARTS;
		}
		else if(cont==4) //time 1
			time1 = element;
		else if(cont==5) //time 2
		{
			if(element.empty())
				time1 = time2;
			else
				time2 = element;
		}
		else if(cont==6)
			mask = element;
		else if(cont==7) //TeChronon  
		{
			if (element=="TeSECOND")
				chronon = TeSECOND;
			else if (element=="TeMINUTE")
				chronon = TeMINUTE;
			else if (element=="TeHOUR")
				chronon = TeHOUR;
			else if (element=="TeDAY")
				chronon = TeDAY;
			else if (element=="TeMONTH")
				chronon = TeMONTH;
			else if (element=="TeYEAR")
				chronon = TeYEAR;
			else if (element=="TeDAYOFWEEK")
				chronon = TeDAYOFWEEK;
			else if (element=="TeMONTHOFYEAR")
				chronon = TeMONTHOFYEAR;
			else if (element=="TeSEASON")
				chronon = TeSEASON;
			else if (element=="TeWEEKOFYEAR")
				chronon = TeWEEKOFYEAR;
		}

		++cont;
	}

	int posMask = mask.find ("s");
	if(posMask<0) 
	{
		result = " "+ getSQLTemporalWhere(atoi(time1.c_str()), atoi(time2.c_str()), chronon, rel, (tableName+"."+initialCol), (tableName+"."+finalCol)) +" ";
	}
	else
	{
		TeTimeInterval interval(time1, time2, chronon, mask);
		result = " "+ getSQLTemporalWhere(interval, rel, (tableName+"."+initialCol), (tableName+"."+finalCol)) + " "; 
	}

	return result;
}


string 
TeDatabase::getSQLTemporalFunction (TeChronon chr, const string& colName)
{
	string func;
	switch(chr)
	{
		case TeYEAR:             
			func = " year ("+ colName +")";
		break;

		case TeMONTH:			// Fev/2003 != Fev/2004
			func = " year ("+ colName +"), month ("+ colName +") ";
		break;

		case TeMONTHOFYEAR:		// Fev/2003 == Fev/2004
			func = " month ("+ colName +")";
		break;

		case TeDAY:				// 01/01/2003 != 01/01/2004 != 01/02/2004             
			func = " year ("+ colName +"), month ("+ colName +"), day("+ colName +") ";
		break;

		case TeDAYOFMONTH:		// 01/01/2003 == 01/01/2004 == 01/02/2004             
			func = " day("+ colName +") ";
		break;

		case TeDAYOFYEAR:		// 01/01/2003 == 01/01/2004 != 01/02/2004             
			func = " month ("+ colName +"), day("+ colName +") ";
		break;

		case TeDAYOFWEEK:		// 01/01/2003 != 01/01/2004 != 01/02/2004             
			func = " weekday ("+ colName +") ";
		break;

		case TeHOUR:			// 01/01/2003 10:00 != 01/01/2004 10:00 != 01/02/2004 10:00             
			func = " year ("+ colName +"), month ("+ colName +"), day("+ colName +"), hour("+ colName +") ";
		break;

		case TeHOUROFDAY:       // 01/01/2003 10:00 == 01/01/2004 10:00 == 01/02/2004 10:00    
			func = " hour ("+ colName +")";
		break;

		case TeMINUTE:			// 01/01/2003 10:30 != 01/01/2004 10:30 != 01/02/2004 10:30 
		   func = " year ("+ colName +"), month ("+ colName +"), day("+ colName +"), hour("+ colName +"), minute("+ colName +") ";
		break;

		case TeMINUTEOFHOUR:   // 01/01/2003 10:30 == 01/01/2004 10:30 == 01/02/2004 10:30 != 01/02/2004 10:31
			func = " minute("+ colName +") ";
		break;

		case TeSECOND:  // 01/01/2003 10:30:04 != 01/01/2004 10:30:04 != 01/02/2004 10:30:04 
		   func = " year ("+ colName +"), month ("+ colName +"), day("+ colName +"), hour("+ colName +"), minute("+ colName +"), second("+ colName +") ";
		break;

		case TeSECONDOFMINUTE:  // 01/01/2003 10:30:04 == 01/01/2004 10:30:04 == 01/02/2004 10:30:04 != 01/02/2004 10:30:07
		   func = " second("+ colName +") "; 
		break;
		
		default:
			return "";
	}
	
	return func;
}
 

bool
TeDatabase::getMBRGeom(string tableGeom, string object_id, TeBox& box, string /* colGeom */)
{
	double xmin = TeMAXFLOAT;
	double xmax = -TeMAXFLOAT;
	double ymin = TeMAXFLOAT;
	double ymax = -TeMAXFLOAT;
			
	TeDatabasePortal* portal = getPortal();
	if(!portal)
		return false;

	string sel = "SELECT lower_x, upper_x, lower_y, upper_y FROM " + tableGeom;
	sel += " WHERE object_id = '" + object_id + "'";

	if(!portal->query(sel))
	{
		delete portal;
		return false;
	}
		
	bool b = portal->fetchRow();
	if(!b)
	{
		delete portal;
		return false;
	}
	
	while(b)
	{
		xmin = MIN(xmin, portal->getDouble(0));
		xmax = MAX(xmax, portal->getDouble(1));
		ymin = MIN(ymin, portal->getDouble(2));
		ymax = MAX(ymax, portal->getDouble(3));
		b = portal->fetchRow();	
	}
			
	TeBox bb(xmin, ymin, xmax, ymax);
	box = bb;
	delete portal;
	return true;
}

bool 
TeDatabase::getMBRSelectedObjects(string /* geomTable */,string /* colGeom */, string fromClause, string whereClause, string afterWhereClause, TeGeomRep repType,TeBox &bout, const double& tol)
{
	string	fields;
	string	query;
	bool	status = false;

	TeBox	box;
	bout = box;

	TeDatabasePortal* portal = this->getPortal();

	switch(repType)
	{
		case TePOLYGONS:
		case TeLINES:
		case TeCELLS:
			fields = "MIN(lower_x), MIN(lower_y), MAX(upper_x), MAX(upper_y)";
			query =  " SELECT " + fields;
			query += " FROM " + fromClause; 
			if (!whereClause.empty())
				query += " WHERE " + whereClause;
			if (!afterWhereClause.empty())
				query += afterWhereClause;

			if (portal->query (query))
			{
				bool b = portal->fetchRow();
				while(b)
				{
					string vxmin = portal->getData(0);
					string vymin = portal->getData(1);
					string vxmax = portal->getData(2);
					string vymax = portal->getData(3);
					if(vxmin.empty() || vymin.empty() || vxmax.empty() || vymax.empty())
					{
						b = portal->fetchRow();
						continue;
					}
					double xmin = atof(vxmin.c_str());
					double ymin = atof(vymin.c_str());
					double xmax = atof(vxmax.c_str());
					double ymax = atof(vymax.c_str());
					TeBox	ibox(xmin, ymin, xmax, ymax);
					updateBox (bout, ibox);
					b = portal->fetchRow();
					status = true;
				}
			}
			break;

		case TePOINTS:
		case TeTEXT:
			fields = "MIN(x), MIN(y), MAX(x), MAX(y)";
			query =  " SELECT " + fields;
			query += " FROM " + fromClause; 
			if (!whereClause.empty())
				query += " WHERE " + whereClause;
			if (!afterWhereClause.empty())
				query += afterWhereClause;
			
			if (portal->query (query))
			{
				bool b = portal->fetchRow();
				while(b)
				{
          string vxmin = portal->getData(0);
          string vymin = portal->getData(1);
          string vxmax = portal->getData(2);
          string vymax = portal->getData(3);
          if(vxmin.empty() || vymin.empty() || vxmax.empty() || vymax.empty())
          {
            b = portal->fetchRow();
            continue;
          }
          double xmin = atof(vxmin.c_str());
          double ymin = atof(vymin.c_str());
          double xmax = atof(vxmax.c_str());
          double ymax = atof(vymax.c_str());
          
          TeBox ibox;
          if (xmin == xmax) {
            ibox.x1_ = xmin - tol;
            ibox.x2_ = xmax + tol;
          }
          else{
            ibox.x1_ = xmin;
            ibox.x2_ = xmax;
          }
          if (ymin == ymax) {
            ibox.y1_ = ymin - tol;
            ibox.y2_ = ymax + tol;
          }
          else {
            ibox.y1_ = ymin;
            ibox.y2_ = ymax;
          }
					updateBox (bout, ibox);
					b = portal->fetchRow();
					status = true;
				}
			}
			break;


		default:
			status = false;
			break;
	}
	delete portal;
	return status;
}

bool 
TeDatabase::getAttributeList(const string& tableName,TeAttributeList& attList)
{
	TeDatabasePortal* portal = this->getPortal();
	if (!portal)
		return false;

	string sql = "SELECT * FROM " + tableName + " WHERE 1=2";
	if (!portal->query(sql))
	{
		delete portal;
		return false;
	}
	else
		attList = portal->getAttributeList();
	delete portal;
	return true;
}


bool TeDatabase::insertRasterVisual (int themeId , TeRasterVisual* vis)
{
	TeRasterTransform::TeRasterTransfFunctions tf = vis->getTransfFunction();
		
	if (tf == TeRasterTransform::TeNoTransf)
		return true;

	string sql = "DELETE FROM te_visual_raster WHERE theme_id = " + Te2String(themeId);
	if (!this->execute(sql))
		return false;

	if (tf == TeRasterTransform::TeMono2Three ||
		tf == TeRasterTransform::TePall2Three || 
		tf == TeRasterTransform::TeLUT2Three)
	{
		sql = "INSERT INTO te_visual_raster (theme_id, band_in, transf_type) VALUES (";
		sql += Te2String(themeId) + "," + Te2String(vis->getSrcBand()) + ",";
		sql += Te2String(static_cast<short>(tf)) + ")";
		if (!this->execute(sql))
			return false;
		return true;
	}

	if (tf == TeRasterTransform::TeExtractBand)
	{
		sql = "INSERT INTO te_visual_raster (theme_id, band_in, band_out, transf_type) VALUES (";
		sql += Te2String(themeId) + "," + Te2String(vis->getSrcBand()) + ",";
		sql += Te2String(vis->getDestBand()) + "," + Te2String(static_cast<short>(tf))+ ")";
		if (!this->execute(sql))
			return false;
		return true;
	}

	if (tf == TeRasterTransform::TeExtractBands || tf == TeRasterTransform::TeExtractRGB)
	{
		map<TeRasterTransform::TeRGBChannels,short>& RGBmap = vis->getRGBMap();
		map<TeRasterTransform::TeRGBChannels,short>::iterator it = RGBmap.begin();
		while (it != RGBmap.end())
		{
			sql = "INSERT INTO te_visual_raster (theme_id, band_in, band_out, transf_type) VALUES (";
			sql += Te2String(themeId) + "," + Te2String(static_cast<short>(it->second)) + ",";
			sql += Te2String(static_cast<short>(it->first)) + "," + Te2String(static_cast<short>(tf))+ ")";
			if (!this->execute(sql))
				return false;
			++it;
		}
		return true;
	}
	return false;
}


bool 
TeDatabase::locateLineSet (const string& table, TeCoord2D &pt, TeLineSet &ls, const double& tol)
{
	bool located=false;
	TeDatabasePortal* portal = this->getPortal();

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE lower_x < %f AND upper_x > %f AND lower_y < %f AND upper_y > %f",
		box.x2(),box.x1(),box.y2(),box.y1());
	q += buf;
	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}

	// Get all lines
	bool flag = true;
	do 
	{
		TeLine2D l;
		TeLineSet tmp;
		int index;
		double dist;
		TeCoord2D pout;

		flag = portal->fetchGeometry( l );
		tmp.add(l);
		if(TeNearest(pt,tmp,index,pout,dist,tol))
			{
				ls.add ( l );
				located=true;
			}
	} while (flag);

	delete portal;
	return located;
}


bool 
TeDatabase::locatePointSet (const string& table, TeCoord2D &pt, TePointSet &pointSet, const double& tol)
{
	bool located=false;

	TeDatabasePortal* portal = this->getPortal();

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE x < %f AND x > %f AND y < %f AND y > %f",
		box.x2(),box.x1(),box.y2(),box.y1());
	q += buf;

	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	
	bool flag = true;
	do 
	{
		TePoint point;
		flag = portal->fetchGeometry (point);
		pointSet.add ( point );
		located=true;
	}while (flag);

	delete portal;
	return located;
}


bool 
TeDatabase::locateTextSet (const string& table, TeCoord2D &pt, TeTextSet& textSet, const double& tol)
{
	bool located=false;

	TeDatabasePortal* portal = this->getPortal();

	TeBox box (pt.x()-tol,pt.y()-tol,pt.x()+tol,pt.y()+tol);
	string q ="SELECT * FROM " + table;
	char buf [1024];
	sprintf (buf," WHERE x < %f AND x > %f AND y < %f AND y > %f",
		box.x2(),box.x1(),box.y2(),box.y1());
	q += buf;
	if (!portal->query(q) || !portal->fetchRow())
	{
		delete portal;
		return false;
	}
	// Look for all texts
	bool flag = true;
	
	do
	{
		TeText p;
		flag = portal->fetchGeometry(p);
		textSet.add ( p );
		located=true;
	} while (flag);

	delete portal;
	return located;
}

bool 
TeDatabase::updateVersionStamp(const string& DBversion )
{
	TeTable versionTable;
	if( !loadTable( "te_database", versionTable ) )
		return false;
	if ( versionTable.size() ) //no empty
	{
		string deleteVersionTb = "DELETE FROM te_database";
		if( !execute( deleteVersionTb ) )
			return false;
	}
	//insert
	string insVersionTb = "INSERT INTO te_database (db_version) VALUES('" + DBversion + "')";
	if( !execute(insVersionTb) )
		return false;
	return true;
}

bool 
TeDatabase::loadVersionStamp(string& DBversion)
{
	DBversion.clear();
	TeTable versionTable;	
	if( !loadTable("te_database",versionTable) )
		return false;
	if ( versionTable.size() == 0 )
		return false;
	std::vector<std::string> attrs;
	versionTable.attributeNames( attrs );
	for (unsigned int i = 0; i < attrs.size(); i++ )
	{
		if ( TeStringCompare(attrs[i], "db_version", false ) )
		{
			DBversion = versionTable(0, i);
			break;
		}
	}
	return false;
}

bool 
TeDatabase::updateLayerBox(TeLayer* layer)
{
	if (!layer)
		return false;
	TeBox box = layer->box();
	string sql = "UPDATE te_layer SET lower_x = " + Te2String(box.x1(),15);
		   sql += ", lower_y = " + Te2String(box.y1(),15);
		   sql += ", upper_x = " + Te2String(box.x2(),15);
		   sql += ", upper_y = " + Te2String(box.y2(),15);
		   sql += " WHERE layer_id=" + Te2String(layer->id());
	return execute(sql);
}

//! Load information about all projects stored in the database
bool 
TeDatabase::loadProjectSet()
{
	//clear map
	TeProjectMap::iterator projectIt;
	for (projectIt = metaModel_->projectMap().begin(); projectIt != metaModel_->projectMap().end(); ++projectIt)
	{
		if (projectIt->second)
			delete projectIt->second;
	}
	metaModel_->projectMap().clear();
	
	//sql
	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;

	string get = " SELECT te_project.*, te_project_view.* ";
	get += " FROM te_project LEFT JOIN te_project_view ";
	get += " ON te_project.project_id = te_project_view.project_id ";
	get += " ORDER BY te_project.project_id, te_project_view.view_id";

	if (!portal->query(get))
	{	
		delete portal;
		return false;
	}

	bool flag = portal->fetchRow();
	while (flag)
	{
		TeProject* project = new TeProject(portal->getInt(0), this);
		project->setName(string(portal->getData(1)));
		project->setDescription(string(portal->getData(2)));
//		project->setCurrentViewId(portal->getInt(3));
		int pId = portal->getInt(3);
		
		//load all views
		bool hasViews = true;
		while(hasViews)
		{
			project->addView(portal->getInt(5));
			flag = portal->fetchRow();
			if(!flag || (portal->getInt(0)!=project->id()))
				hasViews = false;
		}
		
		project->setCurrentViewId(pId);
		metaModel_->projectMap()[project->id()] = project;
	}

	delete portal;
	return true;

}

//! Load information about a particular project
bool 
TeDatabase::loadProject(TeProject* project)
{
	if(!project)
		return false;
	
	//sql
	string get = " SELECT te_project.*, te_project_view.* ";
	get += " FROM te_project LEFT JOIN te_project_view ";
	get += " ON te_project.project_id = te_project_view.project_id ";

	if(project->id()>0)
		get += " WHERE  te_project.project_id = "+ Te2String(project->id());
	else if (!project->name().empty())
		get += " WHERE  te_project.name = '"+ project->name() +"'";
	else 
		return false;

	get += " ORDER BY te_project.project_id, te_project_view.view_id";

	TeDatabasePortal* portal = this->getPortal();
	if(!portal)
		return false;
	if (!portal->query(get) || !portal->fetchRow())
	{	
		delete portal;
		return false;
	}

	project->setId (portal->getInt(0));
	project->setDatabase(this);
	project->setName(string(portal->getData(1)));
	project->setDescription(string(portal->getData(2)));	
	int currentViewId = portal->getInt(3);
	
	//load all views
	bool hasViews = true;
	while(hasViews)
	{
		project->addView(portal->getInt(5));
		bool flag = portal->fetchRow();
		if(!flag || (portal->getInt(0)!=project->id()))
			hasViews = false;
	}
	project->setCurrentViewId(currentViewId); //the views vector need be filled
		
	metaModel_->projectMap()[project->id()] = project;
	delete portal;
	return true;
}
	
//! Update information about a layer
bool 
TeDatabase::updateProject(TeProject *project)
{
	if (!project)
		return false;
	int projectId = project->id();
	if (projectId <= 0)
		return false;

	//update project information
	string sql = " UPDATE te_project SET ";
	sql += " name = '" + project->name() +"'";
	sql += ", description = '" + project->description() +"'";
	sql += ", current_view = "+ Te2String(project->getCurrentViewId());
	sql += " WHERE project_id = "+ Te2String(project->id());
	if (!this->execute (sql))
	{
		this->errorMessage_ = "\nError updating project information";
		return false;
	}	
	
	//delete relation
	sql = "DELETE FROM te_project_view WHERE project_id =" + Te2String(project->id());
	if (!this->execute (sql))
	{
		this->errorMessage_ = "Error deleting project/view relation";
		return false;
	}
	const TeViewVector vv = project->getViewVector();
	for (unsigned int i=0; i<vv.size(); ++i)
	{
		sql = "INSERT INTO te_project_view VALUES(" + Te2String(projectId) + "," + Te2String(vv[i]) + ")";
		if (!this->execute (sql))
		{
			this->errorMessage_ = "Error inserting project/view relation";
			return false;
		}
	}
	return true;
}

//! Delete a project from the database
bool 
TeDatabase::deleteProject(int projectId)
{
	string sql = "DELETE FROM te_project_view WHERE project_id =" + Te2String(projectId);
	if (!this->execute (sql))
	{
		this->errorMessage_ = "Error deleting project/view relation";
		return false;
	}
	sql = "DELETE FROM te_project WHERE project_id =" + Te2String(projectId);
	if (!this->execute (sql))
	{
		this->errorMessage_ = "Error deleting te_project entry";
		return false;
	}
	
	// delete project and its entry in the project map
	TeProject* proj = metaModel_->projectMap()[projectId];
	metaModel_->projectMap().erase(projectId);
	if(proj)
		delete proj;
	return true;
}

//! Insert a project/view relation
bool 
TeDatabase::insertProjectViewRel(int projectId, int viewId)
{
	string sql = "INSERT INTO te_project_view VALUES(" + Te2String(projectId) + "," + Te2String(viewId) + ")";
	if (!this->execute (sql))
	{
		this->errorMessage_ = "Error inserting project/view relation";
		return false;
	}
	return true;
}

bool 
TeDatabase::deleteProjectViewRel(int projectId, int viewId)
{
	string sql ="DELETE FROM te_project_view WHERE project_id = ";
	       sql += Te2String(projectId);
		   sql += " AND view_id = ";
		   sql += Te2String(viewId);
	if (!this->execute (sql))
	{
		this->errorMessage_ = "Error removing project/view relation";
		return false;
	}
	return true;
}

bool TeDatabase::projectExist(const string& projectName)
{
	TeDatabasePortal* portal = this->getPortal();

	if(!portal)
		return false;

	string sql = "SELECT name FROM te_project WHERE " +  this->toUpper("name") + " = '" + TeConvertToUpperCase(projectName) + "'";

	if(!portal->query(sql))
	{	
		portal->freeResult();
		delete portal;
		return false;
	}

	if(portal->fetchRow())
	{
		portal->freeResult();
		delete portal;
		return true;		
	}

	portal->freeResult();
	delete portal;
	return false;
}

bool TeDatabase::createPolygonGeometry(const string& tableName)
{
	if(tableName.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attSpatial;
	attSpatial.rep_.name_ = "spatial_data";
	attSpatial.rep_.type_ = TePOLYGONTYPE;
	attList.push_back(attSpatial);}

	if(!createTable(tableName, attList))
		return false;

	string idxName = "te_idx_"  + tableName + "_obj";

	return createIndex(tableName, idxName, "object_id");
}

bool TeDatabase::createLineGeometry(const string& tableName) 
{
	if(tableName.empty())
		return false;

	TeAttributeList attList;

	{TeAttribute attGeomId;
	attGeomId.rep_.name_ = "geom_id";
	attGeomId.rep_.type_ = TeUNSIGNEDINT;
	attGeomId.rep_.isAutoNumber_ = true;
	attGeomId.rep_.isPrimaryKey_ = true;
	attGeomId.rep_.null_ = false;
	attList.push_back(attGeomId);}

	{TeAttribute attObjId;
	attObjId.rep_.name_ = "object_id";
	attObjId.rep_.type_ = TeSTRING;
	attObjId.rep_.numChar_ = 255;
	attObjId.rep_.null_ = false;
	attList.push_back(attObjId);}

	{TeAttribute attSpatial;
	attSpatial.rep_.name_ = "spatial_data";
	attSpatial.rep_.type_ = TeLINE2DTYPE;
	attList.push_back(attSpatial);}

	if(!createTable(tableName, attList))
		return false;

	string idxName = "te_idx_"  + tableName + "_obj";

	return createIndex(tableName, idxName, "object_id");
}

bool 
TeDatabase::deleteRelation(const string& name, const string& table)
{
	string relation = "ALTER TABLE " + table + " DROP ";
	relation += " CONSTRAINT " + name;
	return execute(relation);
}

bool 
TeDatabase::dropDBView(const string& dbViewName)
{
   string del = "DROP VIEW " + dbViewName;
   return (execute(del));
} 

bool 
TeDatabase::updateBBox(const string& tableName, const string& keyColumnName, int keyValue, const TeBox& box)
{
	string upd = "UPDATE "+tableName+" SET lower_x=" + Te2String(box.x1_,15);
	upd += ", lower_y=" + Te2String(box.y1_, 15);
	upd += ", upper_x=" + Te2String(box.x2_, 15);
	upd += ", upper_y=" + Te2String(box.y2_, 15);
	upd += " WHERE " + keyColumnName + "=" + Te2String(keyValue);
	return execute(upd);
}
bool 
TeDatabase::beginTransaction()
{
	transactionCounter_++; 
	return true;
} 

bool 
TeDatabase::commitTransaction()
{
	transactionCounter_ = max(transactionCounter_-1, 0);
	return true;
} 

bool 
TeDatabase::rollbackTransaction()
{
	transactionCounter_ = max(transactionCounter_-1, 0);
	return true;
} 

TeBox 
TeDatabase::getThemeBox(TeTheme* theme)
{
	TeBox bb;
	if (!theme)
		return bb;

	TeLayer* layer = theme->layer();
	if (layer->hasGeometry(TeRASTER))
		bb = layer->box();

	if (layer->hasGeometry(TeRASTERFILE))
		updateBox(bb,layer->getRepresentation(TeRASTERFILE)->box_);

	string colTabName = theme->collectionTable();
	if (colTabName.empty())
		return bb;

	string sqlfrom;
	string geomTable;
	if (layer->hasGeometry(TePOINTS))
	{
		geomTable = layer->tableName(TePOINTS);
		sqlfrom = colTabName + " LEFT JOIN " + geomTable;
		sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
		TeBox bpt;
		if (getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TePOINTS,bpt))
			updateBox(bb,bpt);
	}
	if (layer->hasGeometry(TeLINES))
	{
		geomTable = layer->tableName(TeLINES);
		sqlfrom = colTabName + " LEFT JOIN " + geomTable;
		sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
		TeBox bln;
		if (getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TeLINES,bln))
			updateBox(bb,bln);
	}
	if (layer->hasGeometry(TePOLYGONS))
	{
		geomTable = layer->tableName(TePOLYGONS);
		sqlfrom = colTabName + " LEFT JOIN " + geomTable;
		sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
		TeBox bpol;
		if (getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TePOLYGONS,bpol))
			updateBox(bb,bpol);
	}

	if (layer->hasGeometry(TeCELLS))
	{
		geomTable = layer->tableName(TeCELLS);
		sqlfrom = colTabName + " LEFT JOIN " + geomTable;
		sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
		TeBox bpol;
		if (getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TeCELLS,bpol))
			updateBox(bb,bpol);
	}

	if (layer->hasGeometry(TeTEXT))
	{
		geomTable = layer->tableName(TeTEXT);
		sqlfrom = colTabName + " LEFT JOIN " + geomTable;
		sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
		TeBox bpol;
		if (getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TeTEXT,bpol))
			updateBox(bb,bpol);
	}	

	return bb;
}

// End TeDatabase Methods

// Begin TeDatabasePortal Methods

TeDatabasePortal::TeDatabasePortal():
	db_(0),			
	numRows_(0),
	numFields_ (0),	
	errorMessage_(""),
	errorNumber_(0)
	{}

TeDatabasePortal::~TeDatabasePortal ()
{
}

double 
TeDatabasePortal::getDouble (int i)
{  
	char* val = getData(i); 
	return atof(val); 
}

double 
TeDatabasePortal::getDouble (const string& s)
{  
	char* val = getData(s); 
	return atof(val); 
}

int 
TeDatabasePortal::getInt (int i)
{  
	char* val = getData(i); 
	return atoi(val); 
}

int 
TeDatabasePortal::getInt (const string& s)
{  
	char* val = getData(s); 
	return atoi(val); 
}

bool 
TeDatabasePortal::fetchGeometry (TePolygon& geom, const unsigned int&)
{
	return fetchGeometry(geom);
}

bool 
TeDatabasePortal::fetchGeometry (TeLine2D& geom, const unsigned int&)
{
	return fetchGeometry(geom);
}
	
bool 
TeDatabasePortal::fetchGeometry (TeNode& geom, const unsigned int&)
{
	return fetchGeometry(geom);
}

bool 
TeDatabasePortal::fetchGeometry (TePoint& geom, const unsigned int&)
{
	return fetchGeometry(geom);
}

bool
TeDatabasePortal::fetchGeometry (TeCell& cell)
{
	int index = atoi(getData("geom_id"));
	string object_id = getData("object_id");
	TeBox b (atof(getData("lower_x")),atof(getData("lower_y")),atof(getData("upper_x")),atof(getData("upper_y")));
	cell.geomId(index);
	cell.objectId(object_id);
	cell.setBox (b);
	cell.column(atoi(getData("col_number")));
	cell.line(atoi(getData("row_number")));
	return (fetchRow());
}

bool
TeDatabasePortal::fetchGeometry (TeCell& cell, const unsigned int& initIndex)
{
	int index = atoi(getData(initIndex));
	string object_id = getData(initIndex+1);
	TeBox b (atof(getData(initIndex+2)),atof(getData(initIndex+3)),atof(getData(initIndex+4)),atof(getData(initIndex+5)));
	cell.geomId(index);
	cell.objectId(object_id);
	cell.setBox (b);
	cell.column(atoi(getData(initIndex+6)));
	cell.line(atoi(getData(initIndex+7)));
	return (fetchRow());
}

TeAttribute TeDatabasePortal::getAttribute (int i)
{
	TeAttributeList::iterator it = attList_.begin();
	int j = 0;
	while ( it != attList_.end() )
	{
		if (i == j)
			return (*it);
		++it;
		j++;
	}
	return TeAttribute();
}

TeAttribute TeDatabasePortal::getAttribute (const string& s)
{
	TeAttributeList::iterator it = attList_.begin();
	while ( it != attList_.end() )
	{
		if (TeConvertToUpperCase(s) == TeConvertToUpperCase((*it).rep_.name_))
			return (*it);
		++it;
	}
	return TeAttribute();
}

int 
TeDatabasePortal::getColumnIndex (const string& s)
{
	TeAttributeList::iterator it = attList_.begin();
	int j = 0;
	while ( it != attList_.end() )
	{
		if (TeConvertToUpperCase(s) == TeConvertToUpperCase((*it).rep_.name_))
			return j;
		++it;
		j++;
	}
	return -1;
}

string 
TeDatabasePortal::getColumnName (int i)
{
	TeAttributeList::iterator it = attList_.begin();
	int j=0;
	while ( it != attList_.end() )
	{
		if (j==i)
			return (*it).rep_.name_;
		++it;
		j++;
	}
	return "";
}


TeViewTree*
TeDatabasePortal::getViewTree ()
{
	TeViewTree *tree = new TeViewTree();
	tree->id (atoi(getData("theme_id")));
	tree->name (getData("name"));
	tree->priority(atoi(getData("priority")));
	//Update the tree also with parent_id and node_type from te_theme table
	tree->parentId(atoi(getData("parent_id")));
	tree->type(atoi(getData("node_type")));	
	//Is it necessary to set parent??
	return tree;
}

TeLegendEntry 
TeDatabasePortal::getLegend ()
{
	TeLegendEntry leg;
	leg.id (atoi(getData("legend_id")));
	leg.theme (atoi(getData("theme_id")));
	leg.group (atoi(getData("group_id")));

	string data;
	data = getData("num_objs");
	leg.count(atoi(data.c_str()));
	data = getData("lower_value");
	leg.from(data);
	data = getData("upper_value");
	leg.to(data);
	data = getData("label");
	leg.label(data);
	return leg;
}

void
TeDatabasePortal::getVisual(TeVisual* vis)
{
	TeGeomRep rep = (TeGeomRep)atoi (getData("geom_type"));

	TeColor cor(atoi(getData("red")),atoi(getData("green")),atoi(getData("blue")));
	vis->color(cor);
	vis->transparency(atoi(getData("transparency")));

	TeColor ccor(atoi(getData("contour_red")),atoi(getData("contour_green")),atoi(getData("contour_blue")));
	vis->contourColor(ccor);
	vis->contourWidth(atoi(getData("contour_width")));
	vis->contourTransparency(atoi(getData("contour_transp")));

	if(rep == TePOLYGONS || rep == TeCELLS)
	{
		vis->contourWidth(atoi(getData("width")));
		vis->contourStyle(atoi(getData("contour_symb_id")));
		vis->style(atoi(getData("symb_id")));
	}
	else if(rep == TeLINES)
	{
		vis->width(atoi(getData("width")));
		vis->style(atoi(getData("symb_id")));
	}
	else if(rep == TePOINTS)
	{
		vis->size(atoi(getData("size_value")));
		vis->style(atoi(getData("symb_id")));
	}
	else if(rep == TeTEXT)
		vis->size(atoi(getData("size_value")));

	vis->family(getData("family"));
	vis->bold (getBool("bold"));
	vis->italic (getBool("italic"));
	vis->fixedSize (getBool("fixed_size"));

	vis->alignmentVert(getDouble("alignment_vert"));
	vis->alignmentHoriz(getDouble("alignment_horiz"));

	vis->tabSize(atoi(getData("tab_size")));
	vis->lineSpace(atoi(getData("line_space")));
}

bool 
TeDatabasePortal::fetchGeometry (TeText& t)
{
	TeCoord2D c(getDouble("x"), getDouble("y"));
	string txt = getData ("text_value");
	TeText t2(c,txt);
	t2.geomId(atoi(getData("geom_id")));
	t2.objectId(string(getData("object_id")));
	t2.setAngle (getDouble("angle"));
	t2.setHeight (getDouble("height"));
	t2.setAlignmentVert(getDouble("alignment_vert"));
	t2.setAlignmentHoriz(getDouble("alignment_horiz"));
	t = t2;
	return (fetchRow());
}

bool 
TeDatabasePortal::fetchGeometry (TeText& t, const unsigned int& initIndex)
{
	TeCoord2D c(getDouble(initIndex+2), getDouble(initIndex+3));
	string txt = getData(initIndex+4);
	TeText t2(c,txt);
	t2.geomId(atoi(getData(initIndex)));
	t2.objectId(string(getData(initIndex+1)));
	t2.setAngle (getDouble(initIndex+5));
	t2.setHeight (getDouble(initIndex+6));
	t2.setAlignmentVert(getDouble(initIndex+7));
	t2.setAlignmentHoriz(getDouble(initIndex+8));
	t = t2;
	return (fetchRow());
}

bool 
TeDatabasePortal::fetchGeometry (TeArc& arc)
{
	arc.fromId(atoi(getData(2)));
	arc.toId(atoi(getData(3)));
	arc.geomId(atol(getData(0)));
	arc.objectId(string(getData(1)));
	return fetchRow();
}

bool 
TeDatabasePortal::fetchGeometry (TeArc& arc, const unsigned int& initIndex)
{
	arc.fromId(atoi(getData(initIndex+2)));
	arc.toId(atoi(getData(initIndex+3)));
	arc.geomId(atol(getData(initIndex+0)));
	arc.objectId(string(getData(initIndex+1)));
	return fetchRow();
}

bool	
TeDatabasePortal::getVisual(TeVisual* vis, TeGeomRep& rep, const unsigned int& initIndex)
{
	string legendId = this->getData(initIndex);
	if(legendId.empty())
		return false;

	rep = (TeGeomRep)atoi (getData(initIndex+1)); //geom_type = 1

	TeColor cor(atoi(getData(initIndex+3)),atoi(getData(initIndex+4)),atoi(getData(initIndex+5)));
	vis->color(cor);
	vis->transparency(atoi(getData(initIndex+6)));

	TeColor ccor(atoi(getData(initIndex+9)),atoi(getData(initIndex+10)),atoi(getData(initIndex+11)));
	vis->contourColor(ccor);
	vis->contourWidth(atoi(getData(initIndex+13)));
	vis->contourTransparency(atoi(getData(initIndex+12)));

	if(rep == TePOLYGONS || rep == TeCELLS)
	{
		vis->contourWidth(atoi(getData(initIndex+13)));
		vis->contourStyle(atoi(getData(initIndex+8)));
		vis->style(atoi(getData(initIndex+2)));
	}
	else if(rep == TeLINES)
	{
		vis->width(atoi(getData(initIndex+7)));
		vis->style(atoi(getData(initIndex+2)));
	}
	else if(rep == TePOINTS)
	{
		vis->size(atoi(getData(initIndex+14)));
		vis->style(atoi(getData(initIndex+2)));
	}
	else if(rep == TeTEXT)
		vis->size(atoi(getData(initIndex+14)));

	vis->family(getData(initIndex+16));
	vis->bold (getBool(initIndex+17));
	vis->italic (getBool(initIndex+18));
	vis->fixedSize (getBool(initIndex+23));

	vis->alignmentVert(getDouble(initIndex+19));
	vis->alignmentHoriz(getDouble(initIndex+20));

	vis->tabSize(atoi(getData(initIndex+21)));
	vis->lineSpace(atoi(getData(initIndex+22)));
	return true;
}

bool	
TeDatabasePortal::getRasterVisual(TeRasterVisual& vis, const unsigned int& initIndex)
{
	string themeId = getData(initIndex);
	if(themeId.empty())
		return false;

	vis.setSrcBand(getInt(initIndex+1));
	vis.setDestBand(getInt(initIndex+2));
	vis.setTransfFunction(static_cast<TeRasterTransform::TeRasterTransfFunctions>(getInt(initIndex+3)));
	if (vis.getTransfFunction() == TeRasterTransform::TeExtractRGB || 
		vis.getTransfFunction() == TeRasterTransform::TeExtractBands)
	{
		vis.setBChannelMapping(getInt(initIndex+1),static_cast<TeRasterTransform::TeRGBChannels>(getInt(initIndex+2)));
	}
	
	return true;
}


TeColor 
TeDatabasePortal::getColor ()
{
	TeColor c(atoi(getData("red")), atoi(getData("green")), atoi(getData("blue")));
	return c;
}

bool	
TeDatabasePortal::getView(TeView& view, const unsigned int& initIndex)
{
	string viewId = getData(initIndex);
	if(viewId.empty())
		return false;
	view.id(atoi(viewId.c_str()));
	view.name(getData(initIndex+2));
	view.user(getData(initIndex+3));
	view.isVisible(getBool(initIndex+4));
	view.setCurrentBox(TeBox(getDouble(initIndex+5), getDouble(initIndex+6), getDouble(initIndex+7), getDouble(initIndex+8)));
	if(strncmp(getData(initIndex+9),"",1))
		view.setCurrentTheme(-1);
	else
		view.setCurrentTheme(getInt(initIndex+9));
	return true;
}

bool	
TeDatabasePortal::getProjection(TeProjection** proj, const unsigned int& initIndex)
{
	string projId = getData(initIndex);
	if(projId.empty())
		return false;

	TeDatum datum (	getData(initIndex+11),
					getDouble(initIndex+12),
					getDouble(initIndex+13),
					getDouble(initIndex+14),
					getDouble(initIndex+15),
					getDouble(initIndex+16));

	TeProjectionParams mProjPars;
	mProjPars.datum = datum;
	mProjPars.name = getData(initIndex+1);
	mProjPars.lat0 = getDouble(initIndex+3)*TeCDR;
	mProjPars.lon0 = getDouble(initIndex+2)*TeCDR;
	mProjPars.offx = getDouble(initIndex+4);
	mProjPars.offy = getDouble(initIndex+5);
	mProjPars.stlat1 = getDouble(initIndex+6)*TeCDR;
	mProjPars.stlat2 = getDouble(initIndex+7)*TeCDR;
	mProjPars.units = getData(initIndex+8);
	mProjPars.scale = getDouble(initIndex+9);
	mProjPars.hemisphere = (TeHemisphere)getInt(initIndex+10);

	*proj = TeProjectionFactory::make(mProjPars);
	if(!*proj)
		return false;

	(*proj)->id(atoi(projId.c_str()));
	return true;
}

void
TeDatabasePortal::getViewNodeParams (TeViewNodeParams& params, const unsigned int& initIndex)
{
	params.name_ = string (this->getData(initIndex+3));	//name
	params.id_ = this->getInt(initIndex);				// id
	params.viewId_ = this->getInt(initIndex+2);			//view id
	params.nodeType_ = 	this->getInt(initIndex+6);		//node type
	params.priority_ = 	this->getInt(initIndex+5);		//priority  
	params.myParentId_ = this->getInt(initIndex+4);	//parent id
}

bool
TeDatabasePortal::getTheme(TeAbstractTheme& theme, const unsigned int& initIndex)
{
	string themeId = getData(initIndex);
	if(themeId.empty())
		return false;
		
	theme.id(atoi(themeId.c_str()));
	theme.view (atoi(this->getData (initIndex+2)));
	theme.name(string (this->getData(initIndex+3)));
	theme.parentId( atoi(this->getData (initIndex+4)));
	theme.type ((TeViewNodeType)this->getInt(initIndex+6));
	theme.priority(this->getInt(initIndex+5));
	theme.minScale (this->getDouble (initIndex+7));
	theme.maxScale (this->getDouble (initIndex+8));
	theme.attributeRest(string(this->getData (initIndex+9)));
	theme.spatialRest(string(this->getData (initIndex+10)));
	theme.temporalRest(string(this->getData (initIndex+11)));
	theme.visibleRep(atoi(this->getData (initIndex+13)));
	theme.visibility(atoi(this->getData (initIndex+14)));
	theme.setThemeBox(TeBox(getDouble(initIndex+15), getDouble(initIndex+16), getDouble(initIndex+17), getDouble(initIndex+18)));

	if(theme.type()==TeTHEME)
	{
		static_cast<TeTheme&>(theme).collectionTable(string(this->getData (initIndex+12)));
		static_cast<TeTheme&>(theme).collectionAuxTable(static_cast<TeTheme&>(theme).collectionTable() + "_aux");
		static_cast<TeTheme&>(theme).layerId (atoi(this->getData(initIndex+1)));
	}
	else if(theme.type()==TeEXTERNALTHEME)
	{
		static_cast<TeExternalTheme&>(theme).collectionTable(string(this->getData (initIndex+12)));
		static_cast<TeExternalTheme&>(theme).collectionAuxTable(static_cast<TeExternalTheme&>(theme).collectionTable() + "_aux");
	}
	else if(theme.type()==TeEXTERNALTHEME)
	{
		static_cast<TeTheme&>(theme).collectionTable(string(this->getData (initIndex+12)));
		static_cast<TeTheme&>(theme).collectionAuxTable(static_cast<TeTheme&>(theme).collectionTable() + "_aux");
	}
	return true;
}

bool	
TeDatabasePortal::getGrouping(TeGrouping& group, const unsigned int& initIndex)
{
	string groupThemeId = getData(initIndex);
	if(groupThemeId.empty())
		return false;

	TeAttributeRep atRep;
	string attname = getData (initIndex+2);
	string norname = getData (initIndex+5);
	int f = attname.find("(");
	if(f >= 0)
	{
		string alias = attname;
		attname = attname.substr(0, f);
		alias = alias.substr(f+1);
		alias = alias.substr(0, alias.size()-1);
		map<string, string>& m = this->getDatabase()->mapThemeAlias()[atoi(groupThemeId.c_str())];
		m[attname] = alias;
	}
	f = norname.find("(");
	if(f >= 0)
	{
		string alias = norname;
		norname = norname.substr(0, f);
		alias = alias.substr(f+1);
		alias = alias.substr(0, alias.size()-1);
		map<string, string>& m = this->getDatabase()->mapThemeAlias()[atoi(groupThemeId.c_str())];
		m[norname] = alias;
	}

	if(attname=="NONE")
		attname = "";
	
	if(norname=="NONE")
		norname = "";
		
	atRep.name_ = attname;
	atRep.type_ = TeAttrDataType(atoi(getData(initIndex+3)));
	group.groupAttribute_ = atRep;
	group.groupNormAttribute_ = norname;
	group.groupMode_ = TeGroupingMode(getInt(initIndex+4));
	group.groupNumSlices_ = getInt(initIndex+1);
	group.groupPrecision_ = getInt(initIndex+7);
	group.groupStdDev_ = getDouble(initIndex+6);
	group.groupFunction_ = getData(initIndex+8);
	group.groupChronon_ = TeChronon(getInt(initIndex+9));
	return true;
}

bool 
TeDatabasePortal::getLegend (TeLegendEntry& leg, const unsigned int& initIndex)
{
	string legId = getData(initIndex);
	if(legId.empty())
		return false;

	leg.id(atoi(getData(initIndex)));
	leg.theme (atoi(getData(initIndex+1)));
	leg.group (atoi(getData(initIndex+2)));
	leg.count (atoi(getData(initIndex+3)));

  std::string lower_value_str( getData(initIndex+4) );
  leg.from( lower_value_str );
  
  std::string upper_value_str( getData(initIndex+5) );
  leg.to( upper_value_str );
  
  std::string label_str( getData(initIndex+6) );
  leg.label( label_str );
    
	return true;
}

bool	
TeDatabasePortal::getAttrTable(TeTable& table, const unsigned int& initIndex)
{
	string tableName = this->getData(initIndex+2);
	if(tableName.empty())
		return false;

	table.name (tableName);
	table.setId(this->getInt(initIndex+0));
	table.setLinkName(this->getData(initIndex+4));
	table.setUniqueName(this->getData(initIndex+3));
	table.attInitialTime(this->getData(initIndex+5));
	table.attFinalTime(this->getData(initIndex+6));
	table.attTimeUnit(TeChronon(this->getInt(initIndex+7)));
	table.setTableType((TeAttrTableType)this->getInt(initIndex+8));
	return true;
}

bool	
TeDatabasePortal::getLayer(TeLayer& layer, const unsigned int& initIndex)
{
	string layerId = this->getData(initIndex);
	if(layerId.empty())
		return false;

    layer.id(atoi(layerId.c_str()));
	layer.name(string (this->getData(initIndex+2)));

	std::string x1 = this->getData("te_layer.lower_x");
	std::string y1 = this->getData("te_layer.lower_y");
	std::string x2 = this->getData("te_layer.upper_x");
	std::string y2 = this->getData("te_layer.upper_y");

	layer.setLayerBox(TeBox(this->getDouble(initIndex+3),
						this->getDouble(initIndex+4),
						this->getDouble(initIndex+5),
						this->getDouble(initIndex+6)));
	return true;
}

bool	
TeDatabasePortal::getRepresentation(TeRepresentation& rep, const unsigned int& initIndex)
{
	string repId = this->getData(initIndex);
	if(repId.empty())
		return false;

	rep.id_ = atoi(repId.c_str());
	TeGeomRep g = (TeGeomRep)atoi(this->getData(initIndex+2));
	rep.geomRep_ = g;
	rep.tableName_ = this->getData(initIndex+3);
	rep.description_ = this->getData(initIndex+4);
	rep.box_ = TeBox(this->getDouble(initIndex+5),
				this->getDouble(initIndex+6),
				this->getDouble(initIndex+7),
				this->getDouble(initIndex+8));
	rep.resX_ = this->getDouble(initIndex+9);
	rep.resY_ = this->getDouble(initIndex+10);
	rep.nCols_ = this->getInt(initIndex+11);
	rep.nLins_ = this->getInt(initIndex+12);
	return true;
}



// End TeDatabasePortal Methods


