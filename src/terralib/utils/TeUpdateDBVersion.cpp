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

#include "TeUpdateDBVersion.h"

bool needUpdateDB(TeDatabase* db, string& DBversion)
{
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	string sql = " SELECT db_version FROM te_database ";
	//The database does not have the te_database connection
	if(!portal->query(sql))
	{
		DBversion = "";
		delete portal;
		return false;
	}

	if(!portal->fetchRow())
	{
		DBversion = "";
		delete portal;
		return true;
	}

	DBversion = portal->getData(0);
	if(DBversion==TeDBVERSION)
	{
		delete portal;
		return false;
	}
	
	delete portal;
	return true;
}


bool updateDBVersion(TeDatabase* db, string& DBversion, string& errorMessage)
{
	//from old version to 3.0
	db->beginTransaction();
	if(DBversion=="")
	{
		if(!updateDB30To301(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}

		if(!updateDB20To30(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}
		
		DBversion = "3.0.1";
	}

	//from 3.0 to 3.0.1
	if(DBversion=="3.0") 
	{
		if(!updateDB30To301(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}

		DBversion = "3.0.1";
	}

	//from 3.0.1 to 3.0.2
	if(DBversion=="3.0.1") 
	{
		if(!updateDB301To302(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}

		DBversion = "3.0.2";
	}

	//from 3.0.2 to 3.1.0
	if(DBversion=="3.0.2") 
	{
		if(!updateDB302To310(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}

		DBversion = "3.1.0";
	}

	if(DBversion=="3.1.0")
	{
		if(!updateDB310To311(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}

		DBversion = "3.1.1";
	}

	if(DBversion=="3.1.1")
	{
		if(!updateDB311To320(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}
		DBversion = "3.2.0";
	}

	if(DBversion=="3.2.0")
	{
		if(!updateDB320To3201(db, errorMessage))
		{
			db->rollbackTransaction();
			return false;
		}
		DBversion = "3.2.0.1";
	}
	
	if(DBversion!=TeDBVERSION)
	{
		db->rollbackTransaction();
		return false;
	}

	string del= "DELETE FROM te_database"; 
	if(!db->execute(del))
	{
		db->rollbackTransaction();
		return false;
	}

	string ins = "INSERT INTO te_database (db_version) VALUES ('"+ TeDBVERSION +"')";
	if(!db->execute(ins))
	{
		db->rollbackTransaction();
		return false;
	}

	db->commitTransaction();
	return true;
}


bool updateDB20To30(TeDatabase* db, string& errorMessage)
{
	TeAttribute fattr;

	// ----- te_grouping 

	if(db->columnExist("te_grouping", "grouping_function", fattr) == false)
	{
		TeAttributeRep atRep;
		atRep.type_ = TeSTRING;
		atRep.name_ = "grouping_function";
		atRep.numChar_ = 10;
		if(db->addColumn("te_grouping", atRep) == false)
		{
			errorMessage = "The column grouping_function could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}

		string upd;
		if(db->columnExist("te_theme_application", "group_function", fattr))
		{
			upd =	" UPDATE te_grouping g, te_theme_application ta ";
			upd +=	" SET g.grouping_function = ta.group_function ";
			upd +=	" WHERE ta.theme_id = g.theme_id ";

			if(!db->execute (upd))
			{
				errorMessage = db->errorMessage();
				return false;
			}

			db->deleteColumn("te_theme_application", "group_function");
		}
		else
		{
			upd =	" UPDATE te_grouping g ";
			upd +=	" SET g.grouping_function = 'AVG' ";
			
			if(!db->execute (upd))
			{
				errorMessage = db->errorMessage();
				return false;
			}
		}
	}

	// ----- te_theme_application

	if(db->columnExist("te_theme_application", "chart_function", fattr) == false)
	{
		TeAttributeRep atRep;
		atRep.type_ = TeSTRING;
		atRep.name_ = "chart_function";
		atRep.numChar_ = 10;
		if(db->addColumn("te_theme_application", atRep) == false)
		{
			errorMessage = "The column chart_function could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
		db->execute("UPDATE te_theme_application SET chart_function = 'AVG'");
	}

	// ----- te_theme 
	if(db->columnExist("te_theme", "visible_rep", fattr) == false)
	{
		TeAttributeRep atRep;
		atRep.type_ = TeINT;
		atRep.name_ = "visible_rep";
		if(db->addColumn("te_theme", atRep) == false)
		{
			errorMessage = "The column visible_rep could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}

		atRep.name_ = "enable_visibility";
		if(db->addColumn("te_theme", atRep) == false)
		{
			errorMessage = "The column enable_visibility could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}

		string upd = " UPDATE te_theme t, te_theme_application ta ";
		upd +=       " SET t.visible_rep = ta.visible_rep ";
		upd +=       " WHERE ta.theme_id = t.theme_id ";
		if(!db->execute (upd))
		{
			errorMessage = db->errorMessage();
			return false;
		}

		upd =		 " UPDATE te_theme t, te_theme_application ta ";
		upd +=       " SET t.enable_visibility = ta.enable_visibility ";
		upd +=       " WHERE ta.theme_id = t.theme_id ";
		if(!db->execute (upd))
		{
			errorMessage = db->errorMessage();
			return false;
		}
			
		db->deleteColumn("te_theme_application", "visible_rep");
		db->deleteColumn("te_theme_application", "enable_visibility");
	}

	//if for ADO passar todas as tabelas para aceitar comprimento iguel a zero!!!
	if(db->dbmsName() == "Ado")
	{
		//te_layer 
		//te_layer_table
		db->allowEmptyString ("te_layer_table", "attr_initial_time");
		db->allowEmptyString ("te_layer_table", "attr_final_time");
		db->allowEmptyString ("te_layer_table", "user_name");

		//te_representation
		db->allowEmptyString ("te_representation", "description");
		
		//te_view
		db->allowEmptyString ("te_view", "user_name");

		//te_visual
		db->allowEmptyString ("te_visual", "lib_name");
		db->allowEmptyString ("te_visual", "contour_lib_name");
		db->allowEmptyString ("te_visual", "family");

		//te_legend
		db->allowEmptyString ("te_legend", "lower_value");
		db->allowEmptyString ("te_legend", "upper_value");
		db->allowEmptyString ("te_legend", "label");
		
		//te_theme
		db->allowEmptyString ("te_theme", "generate_attribute_where");
		db->allowEmptyString ("te_theme", "generate_spatial_where");
		db->allowEmptyString ("te_theme", "generate_temporal_where");
		db->allowEmptyString ("te_theme", "collection_table");

		//te_grouping
		db->allowEmptyString ("te_grouping", "grouping_attr");
		db->allowEmptyString ("te_grouping", "grouping_norm_attr");
		db->allowEmptyString ("te_grouping", "grouping_function");

		//te_theme_application
		db->allowEmptyString ("te_theme_application", "refine_attribute_where");
		db->allowEmptyString ("te_theme_application", "refine_spatial_where");
		db->allowEmptyString ("te_theme_application", "refine_temporal_where");
		db->allowEmptyString ("te_theme_application", "grouping_color");
		db->allowEmptyString ("te_theme_application", "pie_dimension_attr");
		db->allowEmptyString ("te_theme_application", "text_table");
		db->allowEmptyString ("te_theme_application", "chart_function");
	}

	//------------ auxiliary collection  
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	string sql = " SELECT theme_id, visible_rep FROM te_theme ";
	if(!portal->query(sql))
	{
		delete portal;
		return true;
	}

	while(portal->fetchRow ())
	{
		string	themeId = portal->getData(0);
		int		visRep  = atoi(portal->getData(1));

		string collExtTable = "te_collection_"+ themeId +"_aux";
		
		if	((db->tableExist("te_collection_"+ themeId)) && 
			((visRep & TeRASTER) != TeRASTER)			 && 
			(!db->tableExist(collExtTable)))
		{
			TeTheme* theme = new TeTheme();
			theme->id(atoi(themeId.c_str()));
			if(!db->loadTheme (theme))
			{
				delete portal;
				return false;
			}

			string up = "UPDATE " + theme->collectionTable() + " SET c_object_status = 0";
			up += " WHERE c_object_status <> " + Te2String(theme->pointingLegend().id());
			up += " AND c_object_status <> " + Te2String(theme->queryLegend().id());
			up += " AND c_object_status <> " + Te2String(theme->queryAndPointingLegend().id());
			db->execute(up);

			up = "UPDATE " + theme->collectionTable() + " SET c_object_status = 1";
			up += " WHERE c_object_status = " + Te2String(theme->pointingLegend().id());
			db->execute(up);

			up = "UPDATE " + theme->collectionTable() + " SET c_object_status = 2";
			up += " WHERE c_object_status = " + Te2String(theme->queryLegend().id());
			db->execute(up);

			up = "UPDATE " + theme->collectionTable() + " SET c_object_status = 3";
			up += " WHERE c_object_status = " + Te2String(theme->queryAndPointingLegend().id());
			db->execute(up);

			if ((!theme->createCollectionAuxTable()) || (!theme->populateCollectionAux())) 
			{
				errorMessage = "Fail to mount the auxiliary table of the collection!\n";
				errorMessage += db->errorMessage();
				delete portal;
				return false;
			}

			theme->collectionAuxTable(collExtTable);
			theme->addThemeTable(collExtTable);
					
			string oldTCE = theme->collectionTable() + "_ext";
			if (db->tableExist(oldTCE))
			{
				string delTable = "DROP TABLE " + oldTCE;
				db->execute(delTable);
			}
		}
		
	}
	//------------ end auxiliary collection  

	portal->freeResult();

	//------------ text table  
	// if text table have not text visual table associated, create it
	string sel = "SELECT geom_table FROM te_representation WHERE";
	sel += " geom_type = " + Te2String(TeTEXT);
	if(portal->query(sel))
	{
		while(portal->fetchRow())
		{
			string table = portal->getData(0);
			string tvis = table + "_txvisual";
			if(!db->tableExist(tvis))
			{
				TeAttributeList atl;
				TeAttribute		at;

				at.rep_.name_ = "geom_id";
				at.rep_.type_ = TeINT;
				at.rep_.numChar_ = 0;
				at.rep_.isPrimaryKey_ = true;
				atl.push_back(at);

				at.rep_.isPrimaryKey_ = false;
				at.rep_.name_ = "dot_height";
				at.rep_.type_ = TeINT;
				atl.push_back(at);

				at.rep_.name_ = "fix_size";
				at.rep_.type_ = TeINT;
				atl.push_back(at);

				at.rep_.name_ = "color";
				at.rep_.type_ = TeINT;
				atl.push_back(at);

				at.rep_.name_ = "family";
				at.rep_.type_ = TeSTRING;
				at.rep_.numChar_ = 128;
				atl.push_back(at);

				at.rep_.name_ = "bold";
				at.rep_.type_ = TeINT;
				at.rep_.numChar_ = 0;
				atl.push_back(at);

				at.rep_.name_ = "italic";
				at.rep_.type_ = TeINT;
				atl.push_back(at);

				db->createTable(tvis, atl);
				string fk = "fk_" + tvis;
				db->createRelation(fk, tvis, "geom_id", table, "geom_id", true);
				string ins = "INSERT INTO " + tvis + " (geom_id) SELECT geom_id FROM " + table;
				db->execute(ins);

				string popule = "UPDATE " + tvis;
				popule += " SET dot_height = 12";
				popule += ", fix_size = 0";
				popule += ", color = 16711680";
				popule += ", family = 'verdana'";
				popule += ", bold = 1";
				popule += ", italic = 0";
				if(!db->execute(popule))
				{
					errorMessage = "Fail to generate the text visual table!\n";
					errorMessage += db->errorMessage();
					delete portal;
					return false;;
				}
			}
		}
	}
	//------------ end text table  
	delete portal;
	return true;
}


bool updateDB30To301(TeDatabase* db, string& errorMessage)
{
	TeAttribute fattr;

	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	// ----- collection_table

	string sql = " SELECT collection_table FROM te_theme ";   
	if(!portal->query(sql))
	{
		delete portal;
		return false;
	}

	while(portal->fetchRow())
	{
		string collName = portal->getData(0);
		if(	(db->tableExist(collName)) && 
			(!db->columnExist(collName, "c_object_status", fattr)))
		{
			TeAttributeRep atRep;
			atRep.type_ = TeINT;
			atRep.name_ = "c_object_status";
			if(db->addColumn(collName, atRep) == false)
			{
				errorMessage = "The column c_object_status could not be appended!\n";
				errorMessage += db->errorMessage();
				delete portal;
				return false;
			}

			string upd = " UPDATE "+collName+" t1, "+collName+" t2 ";
			upd +=	" SET t1.c_object_status = t2.c_legend_result ";
			upd +=	" WHERE t1.c_object_id = t2.c_object_id ";

			if(!db->execute (upd))
			{
				errorMessage = db->errorMessage();
				delete portal;
				return false;
			}

			db->deleteColumn(collName, "c_legend_result");
		}
	}

	delete portal;
	return true; 
}


bool updateDB301To302(TeDatabase* db, string& errorMessage)
{
		
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	// ----- valid attribute table 
	
	TeAttrTableVector attrTableVec;
	if(db->getAttrTables(attrTableVec))
	{
		for(unsigned int i=0; i<attrTableVec.size(); ++i)
		{
			TeTable fromTable = attrTableVec[i];
			bool	flag = false;
			
			if(fromTable.tableType()==TeAttrMedia)
				continue;

			//verify if there is another table with the same name
			for(unsigned int j=0; j<i; ++j)
			{
				if(TeConvertToUpperCase(attrTableVec[j].name())==TeConvertToUpperCase(fromTable.name())) 
				{
					flag = true;
					break;
				}
			}

			if(flag)
				continue;
			
			if(db->validTable(fromTable)) //fromTable was modified
			{
				TeAttributeList newAttrList = fromTable.attributeList();
				TeAttributeList oldAttrList = attrTableVec[i].attributeList();
				TeAttributeList::iterator newAttIt = newAttrList.begin();
				TeAttributeList::iterator oldAttIt = oldAttrList.begin();

				bool change = false;
				while(newAttIt!=newAttrList.end())
				{
					if(((*oldAttIt).rep_.name_) != ((*newAttIt).rep_.name_))
					{
						TeAttributeRep rep = (*newAttIt).rep_;
						if(db->alterTable(fromTable.name(), rep, (*oldAttIt).rep_.name_))
							change = true;
					}

					++newAttIt;
					++oldAttIt;
				}

				if(change)
				{
					// update te_layer_table
					string upd = " UPDATE te_layer_table ";
					upd +=	" SET unique_id = '"+ fromTable.uniqueName() +"'";
					upd +=  ", attr_link = '"+ fromTable.linkName() +"'";
					upd +=	" WHERE attr_table = '"+ fromTable.name() +"'";

					if(!db->execute (upd))
					{
						delete portal;
						errorMessage = "Error updating te_layer_table!\n";
						errorMessage += db->errorMessage();
						return false;
					}
				}
			}//if
		}//for
		portal->freeResult();
	}

	// ----- te_grouping table

	string sel = " SELECT theme_id, grouping_attr, grouping_attr_type FROM te_grouping ";
	if(!portal->query(sel))
	{
		delete portal;
		return false;
	}

	while(portal->fetchRow())
	{
		string themeId, gAttr;
		themeId = portal->getData(0);
		gAttr = portal->getData(1);
		int gTypr = portal->getInt(2);
		
		if(((gAttr.empty()) || (gAttr=="NONE")) && (gTypr==0))
		{
			string del = " DELETE FROM te_grouping WHERE theme_id = "+ themeId; 
			db->execute(del);
		}
	}

	// ----- te_grouping table

	delete portal;
	return true; 
}

bool PostgreSQLUpdateDB302To310(TeDatabase* db, TeGeomRep rep, const string& geomTableName)
{
	string geomColumnName = "spatial_box";

	if(rep & TeRASTER)
		geomColumnName = "block_box";

	TeAttribute attr;

// verifies if table already has a box column
	if(db->columnExist(geomTableName, geomColumnName, attr))
		return true;

// add box column
	string sql  = "ALTER TABLE " + geomTableName;
			sql += " ADD COLUMN " + geomColumnName + " BOX";

	if(!db->execute(sql))
		return false;

// make sure box column exist	
	if(!db->columnExist(geomTableName, geomColumnName, attr))
		return false;

// populate column with values from older one
	sql  = "UPDATE " + geomTableName;

	if(rep & TePOINTS)
		sql += " SET " + geomColumnName + " = box(point(x, y), point(x, y))";
	else
		sql += " SET " + geomColumnName + " = box(point(upper_x, upper_y), point(lower_x, lower_y))";

	if(!db->execute(sql))
		return false;

// check if there is no null values
	sql  = "SELECT * FROM " + geomTableName;
	sql += " WHERE " + geomColumnName + " IS NULL";

	TeDatabasePortal* p = db->getPortal();

	if(!p)
		return false;

	 if(!p->query(sql))
	 {
		 delete p;

		 return false;
	 }

     if(p->fetchRow())
     {
         delete p;

         return false;
	 }

     delete p;

// add not null for box column
	sql  = "ALTER TABLE " + geomTableName;
	sql += " ALTER COLUMN " + geomColumnName + " SET NOT NULL";

	if(!db->execute(sql))
		return false;
	
// create GiST index
	if(!db->createSpatialIndex(geomTableName, geomColumnName))
		return false;

// for points is enough
	if(rep & TePOINTS)
		return true;

// drop older B-Tree index
	sql = "DROP INDEX " +  geomTableName + "_idx_box";

	if(!db->execute(sql))
		return false;

// drop older box column
	sql  = "ALTER TABLE " + geomTableName;
	sql += " DROP COLUMN lower_x";

	if(!db->execute(sql))
		return false;

	sql  = "ALTER TABLE " + geomTableName;
	sql += " DROP COLUMN lower_y";

	if(!db->execute(sql))
		return false;

	sql  = "ALTER TABLE " + geomTableName;
	sql += " DROP COLUMN upper_x";

	if(!db->execute(sql))
		return false;

	sql  = "ALTER TABLE " + geomTableName;
	sql += " DROP COLUMN upper_y";

	if(!db->execute(sql))
		return false;

	return true;
}

bool updateDB302To310(TeDatabase* db, string& errorMessage)
{
	// Append the column "grouping_chronon" in the "te_grouping"
	// table if he column doesn´t exist
	TeAttribute attr;
	if(db->columnExist("te_grouping", "grouping_chronon", attr) == false)
	{
		TeAttributeRep atRep;
		atRep.type_ = TeINT;
		atRep.name_ = "grouping_chronon";
		if(db->addColumn("te_grouping", atRep) == false)
		{
			errorMessage = "The column grouping_chronon could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}

		// Set the new column "grouping_chronon" to no chronon
		string upd = " UPDATE te_grouping ";
		upd +=	" SET grouping_chronon = 0 ";	
		if(!db->execute (upd))
		{
			errorMessage = db->errorMessage();
			return false;
		}

	}

	if(db->dbmsName() == "MySQL")
	{
		TeAttributeRep rep;
		rep.name_ = "visible_rep";
		rep.type_ = TeINT;
		
		if (db->alterTable("te_theme", rep) == false)
		{
			errorMessage = db->errorMessage();
			return false;
		}
	}
	else if(db->dbmsName() == "PostgreSQL")
	{
		if(!db->execute("BEGIN TRANSACTION"))
		{
			errorMessage = db->errorMessage();
			return false;
		}

		if(!db->loadLayerSet())
		{
			db->execute("ROLLBACK TRANSACTION");
			return false;
		}

		TeLayerMap& layerMap = db->layerMap();

		TeLayerMap::iterator itLayer = layerMap.begin();

//		for each layer, find representations to update box columns
		while(itLayer != layerMap.end())
		{
// update point geometry table
			TeRepresentation* rep = itLayer->second->getRepresentation(TePOINTS);

			if(rep)
			{
				string geomTableName = rep->tableName_;

				if(!PostgreSQLUpdateDB302To310(db, TePOINTS, geomTableName))
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}
			}

// update line geometry table
			rep = itLayer->second->getRepresentation(TeLINES);

			if(rep)
			{
				string geomTableName = rep->tableName_;

				if(!PostgreSQLUpdateDB302To310(db, TeLINES, geomTableName))
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}
			}

// update polygon geometry table
			rep = itLayer->second->getRepresentation(TePOLYGONS);

			if(rep)
			{
				string geomTableName = rep->tableName_;

				if(!PostgreSQLUpdateDB302To310(db, TePOLYGONS, geomTableName))
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}
			}

// update raster geometry table
			rep = itLayer->second->getRepresentation(TeRASTER);

			if(rep)
			{
				string rasterGeomTableName = rep->tableName_;

				string sql = "SELECT raster_table FROM " + rasterGeomTableName;

				TeDatabasePortal* p = db->getPortal();

				if(!p)
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}

				if(!p->query(sql))
				{
					delete p;

					db->execute("ROLLBACK TRANSACTION");

					return false;
				}

				while(p->fetchRow())
				{
					string geomTableName = p->getData(0);

					if(!PostgreSQLUpdateDB302To310(db, TeRASTER, geomTableName))
					{
						delete p;

						db->execute("ROLLBACK TRANSACTION");

						return false;
					}
				}

				delete p;
			}

// update cell geometry table
			rep = itLayer->second->getRepresentation(TeCELLS);

			if(rep)
			{
				string geomTableName = rep->tableName_;

				if(!PostgreSQLUpdateDB302To310(db, TeCELLS, geomTableName))
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}
			}

// update node geometry table
			rep = itLayer->second->getRepresentation(TeNODES);

			if(rep)
			{
				string geomTableName = rep->tableName_;

				if(!PostgreSQLUpdateDB302To310(db, TeNODES, geomTableName))
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}
			}

			++itLayer;
		}

		if(!db->execute("COMMIT TRANSACTION"))
		{
			errorMessage = db->errorMessage();
			return false;
		}
	}
	else if(db->dbmsName() == "PostGIS")
	{
		if(!db->execute("BEGIN TRANSACTION"))
		{
			errorMessage = db->errorMessage();
			return false;
		}

		if(!db->loadLayerSet())
		{
			db->execute("ROLLBACK TRANSACTION");

			return false;
		}

		TeLayerMap& layerMap = db->layerMap();

		TeLayerMap::iterator itLayer = layerMap.begin();

//		for each layer, find representations to update box columns
		while(itLayer != layerMap.end())
		{
// update raster geometry table
			TeRepresentation* rep = itLayer->second->getRepresentation(TeRASTER);

			if(rep)
			{
				string rasterGeomTableName = rep->tableName_;

				string sql = "SELECT raster_table FROM " + rasterGeomTableName;

				TeDatabasePortal* p = db->getPortal();

				if(!p)
				{
					db->execute("ROLLBACK TRANSACTION");

					return false;
				}

				if(!p->query(sql))
				{
					delete p;

					db->execute("ROLLBACK TRANSACTION");

					return false;
				}

				while(p->fetchRow())
				{
					string geomTableName = p->getData(0);

					if(!PostgreSQLUpdateDB302To310(db, TeRASTER, geomTableName))
					{
						delete p;

						db->execute("ROLLBACK TRANSACTION");

						return false;
					}
				}

				delete p;
			}

			++itLayer;
		}

		if(!db->execute("COMMIT TRANSACTION"))
		{
			errorMessage = db->errorMessage();
			return false;
		}
	}

	return true;
}

bool updateDB310To311(TeDatabase* db, string& errorMessage)
{
	string sql = " SELECT collection_table FROM te_theme ";
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;

	if(!portal->query(sql))
	{
		delete portal;
		return false;
	}

	while(portal->fetchRow())
	{
		string tableName = portal->getData(0);
		if(!db->tableExist(tableName+"_aux"))
			continue;

		TeDatabasePortal* portal2 = db->getPortal();
		if(!portal2)
		{
			delete portal;
			return false;
		}

		sql = " SELECT * FROM "+ tableName+"_aux WHERE 1=2";
		if(!portal2->query(sql))
		{
			delete portal;
			delete portal2;
			return false;
		}

		TeAttributeList attrList;
		for(int i=0; i<portal2->numFields(); ++i)
		{
			if(TeConvertToUpperCase(portal2->getAttribute(i).rep_.name_) != "UNIQUE_ID" )
				attrList.push_back(portal2->getAttribute(i));
		}

        delete portal2;

		//add new autonumber column
		TeAttribute attr;
		attr.rep_.name_= "unique_id" ;
		attr.rep_.type_= TeINT;
		attr.rep_.isAutoNumber_ = true;
		attr.rep_.isPrimaryKey_ = true;
		attrList.push_back(attr);
		
		//create new table 
		if(!db->createTable(tableName+"_aux2", attrList))
		{
			errorMessage = "Error creating table "+ tableName+"_aux2 !\n";
			errorMessage += db->errorMessage();
			delete portal;
			return false;
		}

		//insert records
		string ins = " INSERT INTO "+ tableName +"_aux2 ( ";
		string ins2 ="";
		for(unsigned int j=0; j<(attrList.size()-1); ++j)
		{
			if(j>0)
				ins2 += ",";
			ins2 += attrList[j].rep_.name_;
		}
		ins += ins2 +" ) SELECT "+ ins2;
		ins += " FROM "+ tableName+"_aux";
		if(!db->execute(ins))
		{
			errorMessage = "Error inserting table "+ tableName+"_aux2 !\n";
			errorMessage += db->errorMessage();
			delete portal;
			return false;
		}

		sql = " DROP TABLE "+ tableName+"_aux";
		if(db->tableExist(tableName+"_aux"))
		{
			if(!db->execute(sql))
			{
				errorMessage = "Error dropping table "+ tableName+"_aux2 !\n";
				errorMessage += db->errorMessage();
				delete portal;
				return false;
			}
		}
		
		if(!db->alterTable(tableName+"_aux2", tableName+"_aux"))
		{
			errorMessage = "Error renaming table "+ tableName+"_aux2 !\n";
			errorMessage += db->errorMessage();
			delete portal;
			return false;
		}
	}

	delete portal;
	return true;
}

bool updateDB311To320(TeDatabase* db, string& errorMessage)
{
	if (!db->tableExist("te_project"))
	{
		if (!db->createProjectTable())
		{
			errorMessage = "Error creating te_project table\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}

	if (!db->tableExist("te_project_view"))
	{
		if (!db->createProjectViewTable())
 		{
			errorMessage = "Error creating te_project_view table\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	
	//remove columns of the table te_visual
	TeAttribute	attr;
	attr.rep_.name_ = "lib_name";
	attr.rep_.type_ = TeSTRING;
		
	if(db->columnExist("te_visual", attr.rep_.name_,attr))
	{
        //remove column lib_name
		if(!db->deleteColumn("te_visual", attr.rep_.name_))
		{
			errorMessage = "Error removing a column of the te_visual table\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}

	attr.rep_.name_ = "contour_lib_name";
	if(db->columnExist("te_visual", attr.rep_.name_,attr))
	{
        //remove column lib_name
		if(!db->deleteColumn("te_visual", attr.rep_.name_))
		{
			errorMessage = "Error removing a column of the te_visual table\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}

	return true;
}

bool updateDB320To3201(TeDatabase* db, string& errorMessage)
{
	//--- remove the table te_color_scheme
	db->deleteTable("te_color_scheme");	
	
	//--- store the theme box 
	//verify if the theme box is stored 
	TeAttribute	attr;
	attr.rep_.name_ = "lower_x";
	attr.rep_.type_ = TeREAL;

	TeAttributeRep atRep;
	if(!db->columnExist("te_theme", attr.rep_.name_,attr))
	{
		//create the columns
		TeAttributeRep atRep;
		atRep.type_ = TeREAL;
		atRep.name_ = "lower_x";
		atRep.decimals_ = 15;
		atRep.defaultValue_ = "0.0";
		if(db->addColumn("te_theme", atRep) == false)
		{
			errorMessage = "The theme box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	attr.rep_.name_ = "lower_y";
	if(!db->columnExist("te_theme", attr.rep_.name_,attr))
	{
		atRep.name_ = "lower_y";
		if(db->addColumn("te_theme", atRep) == false)
		{
			errorMessage = "The theme box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	attr.rep_.name_ = "upper_x";
	if(!db->columnExist("te_theme", attr.rep_.name_,attr))
	{
		atRep.name_ = "upper_x";
		if(db->addColumn("te_theme", atRep) == false)
		{
			errorMessage = "The theme box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	attr.rep_.name_ = "upper_y";
	if(!db->columnExist("te_theme", attr.rep_.name_,attr))
	{
		atRep.name_ = "upper_y";
		if(db->addColumn("te_theme", atRep) == false)
		{
			errorMessage = "The theme box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	
	//fill the box theme
	string sql = "SELECT te_theme.name, te_view.user_name ";
	sql += " FROM te_theme INNER JOIN te_view ON te_theme.view_id = te_view.view_id ";
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;
	
	if(!portal->query(sql))
	{
		delete portal;
		return false;
	}

	string originalUser = db->user();
	while(portal->fetchRow())
	{
		string themeName = string(portal->getData(0));
		string userName = string(portal->getData(1));
		db->user(userName);

		TeTheme theme(themeName);
		if(!db->loadTheme(&theme))
		{
			delete portal;
			errorMessage = "Error updating theme box!";
			return false;
		}

		if(theme.type()==TeTREE)
			continue;

		//select the theme box
		TeBox bb;
		if (theme.layer()->hasGeometry(TeRASTER))
			bb = theme.layer()->box();

		if (theme.layer()->hasGeometry(TeRASTERFILE))
			updateBox(bb,theme.layer()->getRepresentation(TeRASTERFILE)->box_);

		string colTabName = theme.collectionTable();
		if (!colTabName.empty())
		{
			string sqlfrom;
			string geomTable;
			if (theme.layer()->hasGeometry(TePOINTS))
			{
				geomTable = theme.layer()->tableName(TePOINTS);
				sqlfrom = colTabName + " LEFT JOIN " + geomTable;
				sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
				TeBox bpt;
				if(db->getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TePOINTS,bpt))
					updateBox(bb,bpt);
			}
			if (theme.layer()->hasGeometry(TeLINES))
			{
				geomTable = theme.layer()->tableName(TeLINES);
				sqlfrom = colTabName + " LEFT JOIN " + geomTable;
				sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
				TeBox bln;
				if (db->getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TeLINES,bln))
					updateBox(bb,bln);
			}
			if (theme.layer()->hasGeometry(TePOLYGONS))
			{
				geomTable = theme.layer()->tableName(TePOLYGONS);
				sqlfrom = colTabName + " LEFT JOIN " + geomTable;
				sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
				TeBox bpol;
				if (db->getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TePOLYGONS,bpol))
					updateBox(bb,bpol);
			}

			if (theme.layer()->hasGeometry(TeCELLS))
			{
				geomTable = theme.layer()->tableName(TeCELLS);
				sqlfrom = colTabName + " LEFT JOIN " + geomTable;
				sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
				TeBox bpol;
				if (db->getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TeCELLS,bpol))
					updateBox(bb,bpol);
			}

			if (theme.layer()->hasGeometry(TeTEXT))
			{
				geomTable = theme.layer()->tableName(TeTEXT);
				sqlfrom = colTabName + " LEFT JOIN " + geomTable;
				sqlfrom += " ON " + colTabName + ".c_object_id = " + geomTable + ".object_id";
				TeBox bpol;
				if (db->getMBRSelectedObjects(geomTable,"spatial_data", sqlfrom, "","",TeTEXT,bpol))
					updateBox(bb,bpol);
			}	
		}
					
		//update theme box
		string update = "UPDATE te_theme SET ";
		update += "  lower_x = " + Te2String(bb.x1(), 15); 
		update += ", lower_y = " + Te2String(bb.y1(), 15); 
		update += ", upper_x = " + Te2String(bb.x2(), 15); 
		update += ", upper_y = " + Te2String(bb.y2(), 15); 
		update += " WHERE theme_id=" + Te2String (theme.id());
		if(!db->execute(update))
		{
			delete portal;
			errorMessage = "Error updating theme box!\n";
			errorMessage += db->errorMessage();
			return false;
		}
    }

	db->clear();
	portal->freeResult();
	db->user(originalUser);
	
	//--- store the view box and current theme 
	//verify if the view box is stored 
	attr.rep_.name_ = "lower_x";
	attr.rep_.type_ = TeREAL;

	//create the columns
	if(!db->columnExist("te_view", attr.rep_.name_,attr))
	{
		atRep.type_ = TeREAL;
		atRep.name_ = "lower_x";
		atRep.decimals_ = 15;
		atRep.defaultValue_ = "0.0";
		if(db->addColumn("te_view", atRep) == false)
		{
			delete portal;
			errorMessage = "The view box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	attr.rep_.name_ = "lower_y";
	if(!db->columnExist("te_view", attr.rep_.name_,attr))
	{
		atRep.name_ = "lower_y";
		if(db->addColumn("te_view", atRep) == false)
		{
			delete portal;
			errorMessage = "The view box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	attr.rep_.name_ = "upper_x";
	if(!db->columnExist("te_view", attr.rep_.name_,attr))
	{
		atRep.name_ = "upper_x";
		if(db->addColumn("te_view", atRep) == false)
		{
			delete portal;
			errorMessage = "The view box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	attr.rep_.name_ = "upper_y";
	if(!db->columnExist("te_view", attr.rep_.name_,attr))
	{
		atRep.name_ = "upper_y";
		if(db->addColumn("te_view", atRep) == false)
		{
			delete portal;
			errorMessage = "The view box could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}
	atRep.type_ = TeINT;
	atRep.name_ = "current_theme";
	atRep.decimals_ = 0;
	attr.rep_.name_ = "current_theme";
	if(!db->columnExist("te_view", attr.rep_.name_,attr))
	{
		if(db->addColumn("te_view", atRep) == false)
		{
			delete portal;
			errorMessage = "The view current theme could not be appended!\n";
			errorMessage += db->errorMessage();
			return false;
		}
		//create foreign key
		if (!db->createRelation("fk_view_current_theme", "te_view", "current_theme", "te_theme", "theme_id", true))
		{
			delete portal;
			errorMessage = "Error creating foreign key in the view table!\n";
			errorMessage += db->errorMessage();
			return false;
		}
	}

	//fill the box view
	sql = "SELECT view_id, MIN(lower_x), MIN(lower_y), MAX(upper_x), MAX(upper_y) ";
	sql += " FROM te_theme GROUP BY view_id ";
	
	if(!portal->query(sql))
	{
		delete portal;
		return false;
	}

	while(portal->fetchRow())
	{
		string viewId = string(portal->getData(0));
		string update = " UPDATE te_view SET ";
		update += " lower_x = "+ string(portal->getData(1));
		update += ", lower_y = "+ string(portal->getData(2));
		update += ", upper_x = "+ string(portal->getData(3));
		update += ", upper_y = "+ string(portal->getData(4));
		update += ", current_theme = NULL ";
		update += " WHERE view_id = "+ viewId;

		if(!db->execute(update))
		{
			delete portal;
			errorMessage = "Error updating view box!\n";
			errorMessage += db->errorMessage();
			return false;
		}
    }

	//------ Project information
	attr.rep_.name_ = "current_view";
	attr.rep_.type_ = TeINT;

	//if there is the column "current_view", delete it and create it again 
	if(db->columnExist("te_project", attr.rep_.name_, attr))
		db->deleteColumn("te_project", attr.rep_.name_);
	
	attr.rep_.defaultValue_ = "0";
	if(!db->addColumn("te_project", attr.rep_))
	{
		delete portal;
		errorMessage = "The project view could not be appended!\n";
		errorMessage += db->errorMessage();
		return false;
	}
	
	TeProject project;
	project.setName("TV_Project");
	project.setDescription("TerraView_Default_Project");
	project.setCurrentViewId(-1); // not have project
	if(!db->insertProject(&project))
	{
		delete portal;
		errorMessage = "Error inserting terraView default project!\n";
		errorMessage += db->errorMessage();
		return false;
	}

	portal->freeResult();
	if(!portal->query("SELECT view_id FROM te_view"))
	{
		errorMessage = db->errorMessage();
		delete portal;
		return false;
	}
	while(portal->fetchRow())
	{
		string viewId = string(portal->getData(0));
		string ins = "INSERT INTO te_project_view (project_id, view_id) VALUES (";
		ins += Te2String(project.id()) +  "," + viewId + ")";
		if(!db->execute(ins))
		{
			errorMessage = db->errorMessage();
			delete portal;
			return false;
		}
	}

	delete portal;
	return true;
}


