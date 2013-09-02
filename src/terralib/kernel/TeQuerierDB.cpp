
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

#include "TeQuerierDB.h"
#include "TeDatabase.h"
#include "TeSTInstance.h"
#include "TeTemporalSeries.h"

// Add geometries from portal to STO 
bool 
addGeometry(TeDatabasePortal* portal, TeGeomRep geomRep, TeSTInstance& sto, const int& linkIndex, const int& geomIdIndex) 
{
	bool flag=true;
	map<int, int> geomIds;
		
	//verify if the portal has geometry (multi geometries)
	string geomId = string(portal->getData(geomIdIndex));
	string objId =  string(portal->getData(linkIndex)); 
	int	gId = atoi(geomId.c_str());
	
	//There are no geometries in the portal. This happens when
	//the objects have multi geometries (ex.: obj 1 is line and obj 2 is ponit).
	//The portal must point to the next object.
	if(geomId.empty())
	{
		do
		{
			flag = portal->fetchRow();
		} while(flag && (string(portal->getData(linkIndex)) == sto.objectId()));
		
		return flag; 
	}

	//The portal points to other object. This happens when
	//the objects have more than one geometrical representation and, at the same time, 
	//the theme has an external table.
	if(objId!=sto.objectId())
	{
		do
		{
			flag = portal->fetchRow();
			objId = string(portal->getData(linkIndex));
		} while(flag && ( objId != sto.objectId()));
		
		gId = atoi(portal->getData(geomIdIndex));
	}

	//There are geometries
	while(	flag && (objId == sto.objectId()) &&
			(geomIds.find(gId) == geomIds.end()))
	{
		geomIds[gId] = gId;
		if(geomRep == TePOLYGONS)
		{
			TePolygon pol;
			flag = portal->fetchGeometry(pol, geomIdIndex);
			sto.addGeometry(pol);
		}
		else if (geomRep==TeLINES)
		{
			TeLine2D lin;
			flag = portal->fetchGeometry(lin, geomIdIndex);
			sto.addGeometry(lin);
		}
		else if (geomRep == TePOINTS)
		{
			TePoint point;
			flag = portal->fetchGeometry(point, geomIdIndex);
			sto.addGeometry(point);
		}
		else if (geomRep == TeCELLS)
		{
			TeCell cell;
			flag = portal->fetchGeometry(cell, geomIdIndex);
			sto.addGeometry(cell);
		}
		else if (geomRep == TeTEXT)
		{
			TeText text;
			flag = portal->fetchGeometry(text, geomIdIndex);
			sto.addGeometry(text); 
		}
		else
			flag = portal->fetchRow();

		if(flag)
		{
			gId = atoi(portal->getData(geomIdIndex));
			objId =  string(portal->getData(linkIndex)); 
		}
	}

	return flag;
}

// Add geometries from portal to STO considering the time value
bool
addGeometry(TeDatabasePortal* portal, TeGeomRep geomRep, TeSTInstance& sto, const int& linkIndex, 
			const int& geomIdIndex, TeTimeInterval time, const int& initTimeIndex, const int& finalTimeIndex) 
{
	bool flag=true;
	map<int, int> geomIds;
	TeTime t1 = time.getT1();
	TeTime t2 = time.getT2();
			
	//verify if the portal has geometry (multi geometries)
	
	string geomId = string(portal->getData(geomIdIndex));
	string objId =  string(portal->getData(linkIndex)); 
	int	gId = atoi(geomId.c_str());
	
	//There are no geometries in the portal. This happens when
	//the objects have multi geometries (ex.: obj 1 is line and obj 2 is ponit).
	//The portal must point to the next object.
	if(geomId.empty())
	{
		do
		{
			flag = portal->fetchRow();
			objId = string(portal->getData(linkIndex));
		} while ( flag && 
			 (objId == sto.objectId()) &&
			 (portal->getDate(initTimeIndex) == t1) && 
			 (portal->getDate(finalTimeIndex) == t2) );
		
		return flag; 
	}

	//The portal points to other object. This happens when
	//the objects have more than one geometrical representation and, at the same time, 
	//the theme has an external table.
	if(objId!=sto.objectId())
	{
		do
		{
			flag = portal->fetchRow();
			objId = string(portal->getData(linkIndex));
		} while(flag && (objId != sto.objectId()));
		
		gId = atoi(portal->getData(geomIdIndex));
	}


	//There are geometries
	while(	flag && (objId == sto.objectId()) &&
			(geomIds.find(gId) == geomIds.end()) && 
			(portal->getDate(initTimeIndex) == t1) && 
			(portal->getDate(finalTimeIndex) == t2) )
	{
		geomIds[gId] = gId;
		if(geomRep == TePOLYGONS)
		{
			TePolygon pol;
			flag = portal->fetchGeometry(pol, geomIdIndex);
			sto.addGeometry(pol);
		}
		else if (geomRep==TeLINES)
		{
			TeLine2D lin;
			flag = portal->fetchGeometry(lin, geomIdIndex);
			sto.addGeometry(lin);
		}
		else if (geomRep == TePOINTS)
		{
			TePoint point;
			flag = portal->fetchGeometry(point, geomIdIndex);
			sto.addGeometry(point);
		}
		else if (geomRep == TeCELLS)
		{
			TeCell cell;
			flag = portal->fetchGeometry(cell, geomIdIndex);
			sto.addGeometry(cell);
		}
		else if (geomRep == TeTEXT)
		{
			TeText text;
			flag = portal->fetchGeometry(text, geomIdIndex);
			sto.addGeometry(text); 
		}
		else
			flag = portal->fetchRow();

		if(flag)
		{
			gId = atoi(portal->getData(geomIdIndex));
			objId =  string(portal->getData(linkIndex)); 
		}
	}

	return flag;
}

bool 
addGeometry(TeDatabasePortal* portal, TeGeomRep geomRep, TeMultiGeometry& geometries) 
{
	bool flag = true;
	//There are geometries
	do
	{
		if(geomRep == TePOLYGONS)
		{
			TePolygon pol;
			flag = portal->fetchGeometry(pol);
			geometries.getPolygons().add(pol);
		}
		else if (geomRep==TeLINES)
		{
			TeLine2D lin;
			flag = portal->fetchGeometry(lin);
			geometries.getLines().add(lin);
		}
		else if (geomRep == TePOINTS)
		{
			TePoint point;
			flag = portal->fetchGeometry(point);
			geometries.getPoints().add(point);
		}
		else if (geomRep == TeCELLS)
		{
			TeCell cell;
			flag = portal->fetchGeometry(cell);
			geometries.getCells().add(cell);
		}
		else if (geomRep == TeTEXT)
		{
			TeText text;
			flag = portal->fetchGeometry(text);
			geometries.getTexts().add(text);
		}
		else
			flag = portal->fetchRow();
	}while(flag);
	return flag;
}

TeQuerierDB::~TeQuerierDB() 
{
	for(unsigned int i=0; i<portals_.size(); ++i)
		delete (portals_[i]);

	portals_.clear();
	geomRepr_.clear();
}


void
TeQuerierDB::clearVectors()
{
	vector<TeDatabasePortal*>::iterator itPortal = portals_.begin();
	while(itPortal!=portals_.end())
	{		
		TeDatabasePortal* portal = *itPortal;
		delete portal; 
		++itPortal;
	}
	
	portals_.clear();
	geomRepr_.clear();
	flagPortal_ = false;
}

void 
TeQuerierDB::clear()
{
	clearVectors();
}

TeGeomRep 
TeQuerierDB::geometryRep() 
{  return TeGeomRep(params_->theme()->layer()->geomRep()); }

string 
TeQuerierDB::sqlWhereRestrictions(TeRepresentation* rep)
{
	TeKeys objs;
	string whereClause= " 1 = 1 ";
	TeDatabase* db = params_->theme()->layer()->database();
	if(!db)
		return "";
	
	// load the first representation 
	if(!rep)
		rep = (params_->theme()->layer()->vectRepres())[0];

	// spatial restriction with other geometry representation
	if (params_->hasSpatialRes() && rep)
	{
		if(params_->boxRest().isValid())
		{
            TeBox b =  params_->boxRest();
            TeGeomRep gRep = rep->geomRep_;
			whereClause += " AND "+ db->getSQLBoxWhere(b, gRep);
		}
		else if(params_->geomRest())
		{
			string geomTableRest = params_->theme()->layer()->tableName(params_->geomRepRest());
			TePrecision::instance().setPrecision(TeGetPrecision(params_->theme()->layer()->projection()));

			if((db->spatialRelation(geomTableRest, params_->geomRepRest(), params_->geomRest(),  
							   objs, params_->spatialRelation())) && (!objs.empty()))
			{
				string obs;
				for(unsigned int i=0; i<objs.size(); i++)
				{
					if(i!=0)
						obs += ",";
					obs += "'"+ objs[i] +"'";
				}
				
				whereClause += " AND "+ rep->tableName_ +".object_id IN ("+ obs +")";
			}
			else
				whereClause += " AND 1 <> 1 "; // no geometry was found 

		}
	}

	//selected objects
	switch (params_->selectedObjs())
	{
		case TeAll:
			break;
		
		case TeSelectedByPointing:
			whereClause += " AND (grid_status = 1 OR grid_status = 3";
			whereClause += " OR (grid_status IS NULL AND (c_object_status = 1 OR c_object_status = 3)))";
			break;
		
		case TeNotSelectedByPointing:
			whereClause += " AND (grid_status = 0 OR grid_status = 2";
			whereClause += " OR (grid_status is null AND (c_object_status = 0 OR c_object_status = 2)))";
			break;
		
		case TeSelectedByQuery:
			whereClause += " AND (grid_status = 2 OR grid_status = 3";
			whereClause += " OR (grid_status is null AND (c_object_status = 2 OR c_object_status = 3)))";
			break;
		
		case TeNotSelectedByQuery:
			whereClause += " AND (grid_status = 0 OR grid_status = 1";
			whereClause += " OR (grid_status is null AND (c_object_status = 0 OR c_object_status = 1)))";
			break;
	
		case TeGrouped:
			whereClause += " AND c_legend_id <> 0";
			break;

		case TeNotGrouped:
			whereClause += " AND c_legend_id = 0";
			break;

		case TeSelectedByPointingAndQuery:
			whereClause += " AND grid_status = 3";
			whereClause += " OR (grid_status is null AND c_object_status = 3)";
			break;

		case TeSelectedByPointingOrQuery:
			whereClause += " AND (grid_status = 1 OR grid_status = 2 OR grid_status = 3)";
			whereClause += " OR  (grid_status is null AND (c_object_status = 3 OR c_object_status = 1 OR c_object_status = 2))";
			break;	
	}

	return whereClause;
}


string 
TeQuerierDB::sqlFrom(string geomTable)
{
	string fromPar = "";
	string fromClause = "";
	
	//get collection tables 
	string collAuxTable = params_->theme()->collectionAuxTable();
	string collTable = params_->theme()->collectionTable();
	
	if(collAuxTable.empty() || collTable.empty())
		return attrTable_.name();

	if(attrTable_.name().empty())
		return "";
	
	string uniqueIdName = attrTable_.name() +"."+ attrTable_.uniqueName(); 
	string objectIdName = attrTable_.name() +"."+ attrTable_.linkName(); 
	string linkName; 
	if(attrTable_.tableType() != TeAttrExternal)
		linkName = attrTable_.name() +"."+ attrTable_.linkName(); 
	else
		linkName = collTable +".c_object_id "; 

	//load geometry table if there is spatial restriction
	if(geomTable.empty() && params_->hasSpatialRes())
	{
		TeRepresentation* rep = (theme()->layer()->vectRepres())[0];
		geomTable = theme()->layer()->tableName(rep->geomRep_);
	}

	//get the extern table position 
	int posExtern = -1;
	TeAttrTableVector attr = params_->theme()->attrTables();
	for(unsigned int i=0; i<attr.size(); ++i)
	{
		if(attr[i].tableType() == TeAttrExternal)
			++posExtern;
	}
	
	//if the table is temporal   
	if((attrTable_.tableType()==TeAttrEvent) || (attrTable_.tableType()==TeFixedGeomDynAttr))
	{
		fromPar += "((";
		fromClause = attrTable_.name()+" RIGHT JOIN "+ collAuxTable; 
				
		if(attrTable_.tableType()==TeFixedGeomDynAttr)
		{
			fromClause += " ON "+ uniqueIdName +" = ";
			fromClause += collAuxTable +".aux0";
			fromClause += ")";
		}
		else
		{
			fromClause += " ON "+ objectIdName +" = ";
			fromClause += collAuxTable +".object_id"+ ")";
		}

		fromClause += " LEFT JOIN "+ collTable; 
		fromClause += " ON "+ collAuxTable +".object_id = "+ collTable +".c_object_id )";
		
		if(!geomTable.empty())
		{
			fromPar += "(";
			fromClause += " LEFT JOIN "+ geomTable +" ON ";
			fromClause += collAuxTable +".object_id = "+ geomTable +".object_id )";
		}

		if(params_->selectedObjs() != TeAll) //! join with collection table
		{
			fromPar += "(";
			fromClause += " LEFT JOIN "+ collTable +" ON ";
			fromClause += collAuxTable +".object_id = "+ collTable +".c_object_id )";
		}
	} 

	else if (attrTable_.tableType()==TeAttrStatic)
	{
		fromPar += "(";
		fromClause =  attrTable_.name()+" RIGHT JOIN "+ collTable; 
		fromClause += " ON "+ linkName +" = "+ collTable +".c_object_id)";

		if(!geomTable.empty()) //! join with geometry table
		{
			fromPar += "(";
			fromClause += " LEFT JOIN "+ geomTable +" ON ";
			fromClause += collTable +".c_object_id = "+ geomTable +".object_id )";
		}

		if(params_->selectedObjs() != TeAll) //! join with collection table
		{
			fromPar += "(";
			fromClause += " LEFT JOIN "+ collAuxTable +" ON ";
			fromClause += collTable +".c_object_id = "+ collAuxTable +".object_id )";
		}
	}
	else if (attrTable_.tableType()==TeAttrExternal)
	{
		fromPar += "((";
		fromClause = collAuxTable +" RIGHT JOIN "+ collTable; 
		fromClause += " ON "+ collAuxTable +".object_id = "+ collTable +".c_object_id )";
		fromClause += " LEFT JOIN "+ attrTable_.name() +" ON ";
		fromClause +=  collAuxTable +".aux"+ Te2String(posExtern) +" = ";
		fromClause +=  uniqueIdName +" )";

		if(!geomTable.empty()) //! join with geometry table
		{
			fromPar += "(";
			fromClause += " LEFT JOIN "+ geomTable +" ON ";
			fromClause += collTable +".c_object_id = "+ geomTable +".object_id )";
		} 
	}
		
	return (fromPar+fromClause);
}

bool 
TeQuerierDB::loadGeometries(TeMultiGeometry& geometries, unsigned int& index)
{
	if((portals_.size()<(index+1)) || (geomRepr_.size()<(index+1)))
		return false;

	TeDatabasePortal* portal = portals_[index]; 
	TeRepresentation rep = geomRepr_[index];
	if(!portal)
		return false; 
	
	bool flag = addGeometry(portal, rep.geomRep_, geometries);

	return flag;
}

bool 
TeQuerierDB::loadGeometries(TeMultiGeometry& geometries) 
{
	bool flag = false;
	
	if(!params_->loadGeom())
		return flag;
	
	for(unsigned int i=0; i<portals_.size(); ++i)
		flag = loadGeometries(geometries, i);
	
	return flag;
}



