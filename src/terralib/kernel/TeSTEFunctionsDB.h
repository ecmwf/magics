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
The library p rovided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeSTEFunctionsDB.h
    \brief This file contains a set of functions to build spatio-temporal sets from a TerraLib databse  
*/
#ifndef  __TERRALIB_INTERNAL_STOFUNCTIONS_H
#define  __TERRALIB_INTERNAL_STOFUNCTIONS_H

#include "TeDatabase.h"
#include "TeProgress.h"
#include <vector>
#include <string>

//! Builds the spatial object set from database according to the restrictions previously defined
/*! 
	\param stoset				the STOSet that will be filled 
	\param loadGeometries		if the STOSet will be filled with the geometries 
	\param loadAllAttributes	if the STOSet will be filled with all attributes of the tables of the STOSet 
	\param attrNames			a subset of the attribute names that will be loaded, if the param loadAllAttributes is false 
*/
template<typename elementSet>
bool TeSTOSetBuildDB(elementSet* stoset, bool loadGeometries = false, bool loadAllAttributes = false, vector<string> attrNames = vector<string>());

//! Builds the spatial object set from database according to the restrictions previously defined
/*! 
	\param stoset				the STOSet that will be filled 
	\param groupAttr			a map from attribute name to statistic type  
	\param loadGeometries		if the STOSet will be filled with the geometries 
*/
template<typename elementSet>
bool TeSTOSetBuildDB(elementSet* stoset, TeGroupingAttr& groupAttr, bool loadGeometries = false);


//! Updates or inserts a database table from a spatial object set
/*! 
	\param elemSet		the set of spatial objects that will be inserted or updated into database
	\param tableName	database table name that will be updated  
	\param indexes		the attribute indexes of the spatial objects that must be updated
*/
template<typename elementSet> 
bool TeUpdateDBFromSet (elementSet* elemSet, const string& tableName, vector<int>* indexes = 0);


//! Auxiliary function to insert a row
template<typename Element> 
bool insertRow (Element* elem, TeTable& table, const string& uniqueValue, TeDatabase* db, vector<int>* indexes=0);

//! Auxiliary function to update a row
template<typename Element>
bool updateRow (Element* elem, TeTable table, const string& uniqueId, TeDatabase* db, vector<int>* indexes=0);


//--------------- Implementation

template<typename elementSet> 
bool TeSTOSetBuildDB(elementSet* stoset, bool loadGeometries, bool loadAllAttributes, vector<string> attrNames)
{
	if(!stoset->build(loadGeometries, loadAllAttributes, attrNames))
		return false;
	return true;
}

template<typename elementSet>
bool TeSTOSetBuildDB(elementSet* stoset, TeGroupingAttr& groupAttr, bool loadGeometries)
{
	if(!stoset->build(groupAttr, loadGeometries))
		return false;
	return true;
}

template<typename elementSet>  
bool TeUpdateDBFromSet (elementSet* elemSet, const string& tableName, vector<int>* indexes)
{
	TeDatabase* db = 0;
	if((elemSet->getTheme()) && (elemSet->theme()->layer()))
		db = elemSet->getTheme()->layer()->database();
	else if(elemSet->getLayer())
		db = elemSet->getLayer()->database();
	
	if(!db)
		return false;
	 
	//progress bar
	int step = 0;
	int numSteps = elemSet->numSTInstance();
	if(TeProgress::instance())
		TeProgress::instance()->setTotalSteps(numSteps);
	
	try
	{
		TeAttrTableVector attrTables;
		if(elemSet->getTheme())
			elemSet->getTheme()->getAttTables(attrTables); 
		else if(elemSet->getLayer())
			elemSet->getLayer()->getAttrTables(attrTables); 

		if(attrTables.empty())
			return false;	

		TeTable table;
		int	uniqueIndex = -1;
	
		//! verify if the table is in the stoset
		for(unsigned int i=0; i<attrTables.size(); i++)
		{
			if(attrTables[i].name()==tableName)
			{
				uniqueIndex = i;
				table = attrTables[i];
				break;
			}
		}
		
		if((uniqueIndex==-1) || ((table.tableType()!=TeAttrEvent) && 
								 (table.tableType()!=TeAttrStatic) && 
								 (table.tableType()!=TeFixedGeomDynAttr) &&
								 (table.tableType()!=TeAttrExternal)))
			return false;

		// get some information about the attribute table required
		string uniqueIdName = table.uniqueName();
	
		TeDatabasePortal* portal = db->getPortal();
		if(!portal)
			return false;

		map<string, string> uniqueIds;
		string sql = "SELECT "+ uniqueIdName +" FROM "+ table.name();
		
		if(!portal->query (sql))
		{
			delete portal;
			return false;
		}

		while(portal->fetchRow())
			uniqueIds[string(portal->getData(0))] = string(portal->getData(0));
		
		delete portal;

		//verifies if the attribute column exists in the table
		TeAttributeList attList = elemSet->getAttributeList();
		for(unsigned int i=0; i<attList.size(); ++i)
		{
			if(indexes && (find(indexes->begin(), indexes->end(),(int)i) == indexes->end()))
				continue;

			//verify if the table has this column 
			string attrName = attList[i].rep_.name_;
			size_t pos = attrName.find(".", 0, 1);
			if (pos != string::npos)
				attList[i].rep_.name_ = attrName.substr(pos+1);

			if (!db->columnExist(tableName, attList[i].rep_.name_, attList[i]))
			{
				if(!db->addColumn (tableName, attList[i].rep_))
					return false; 

				TeAttributeList attrListTable = table.attributeList();
				attrListTable.push_back (attList[i]);
				table.setAttributeList(attrListTable);
			}
		}
		
		// Update all the objects 
		typename elementSet::iterator itObj = elemSet->begin();
		while (itObj != elemSet->end())
		{
			string uniqueId =  (*itObj).getUniqueId(uniqueIndex); 

			if(uniqueIds.find(uniqueId) ==  uniqueIds.end())
			{	
				if (!insertRow (&(*itObj), table, uniqueId, db, indexes))
					return false;

				uniqueIds[uniqueId] = uniqueId;
			}
			else
			{	
				if (!updateRow (&(*itObj), table, uniqueId, db, indexes))
					return false;
			}
					
			++itObj;

			if(TeProgress::instance())
			{
				if (TeProgress::instance()->wasCancelled())
				{
					TeProgress::instance()->reset();
					return false;
				}
				else
					TeProgress::instance()->setProgress(step);
			}	
			++step;
		}
	}
	catch(...)
	{
		if (TeProgress::instance())
			TeProgress::instance()->reset();
		return false;
	}
	
	if (TeProgress::instance())
		TeProgress::instance()->reset();
	
	return true;
}

template<typename Element> 
bool insertRow (Element* elem, TeTable& table, const string& uniqueValue, TeDatabase* db, vector<int>* indexes)
{
	vector<string> attrs;
	table.attributeNames(attrs);
	
	string ins = " INSERT INTO "+ table.name() +" (";
	string values = " VALUES ( ";
	
	TePropertyVector prop = elem->getPropertyVector();
	int count=0;

	for(unsigned int i=0; i<prop.size(); ++i)
	{
		if(indexes && (find(indexes->begin(), indexes->end(),(int)i) == indexes->end()))
			continue;
		
		string attrName = prop[i].attr_.rep_.name_;
		size_t pos = attrName.find(".", 0, 1);
		if (pos != string::npos)
			attrName = attrName.substr(pos+1);
				
		if( (find(attrs.begin(), attrs.end(), attrName)!=attrs.end()) &&
			(TeStringCompare(attrName, table.uniqueName()) == false) && 
			(TeStringCompare(attrName,table.linkName()) == false)&&
			(TeStringCompare(attrName,table.attInitialTime()) == false)&&
			(TeStringCompare(attrName,table.attFinalTime()) == false)  )
			
		{
			if((prop[i].attr_.rep_.type_!=TeSTRING) && prop[i].value_.empty())
				continue; 

			if(count>0)
			{
				ins += ",";
				values += ",";
			}
			++count;
			ins += attrName;
			if(prop[i].attr_.rep_.type_==TeSTRING)
				values += "'"+ prop[i].value_ +"'";
			else if(prop[i].attr_.rep_.type_==TeREAL)
			{
				std::string strValue = prop[i].value_;
				replace (strValue.begin(), strValue.end(), ',', '.');
				values += strValue;
			}
			else if(prop[i].attr_.rep_.type_==TeDATETIME)
			{
				TeTime time(prop[i].value_, prop[i].attr_.dateChronon_, prop[i].attr_.dateTimeFormat_, prop[i].attr_.dateSeparator_, prop[i].attr_.timeSeparator_);  
				values += db->getSQLTime(time);
			}
			else
				values += prop[i].value_;
		}
	}

	// -------- object_id, unique_id and timeInterval
		
	if(count>0)
	{
		ins +=		" ,";
		values +=	" ,";
	}

	ins +=		table.linkName();
	values +=	"'"+ elem->objectId() +"'";

	if(table.linkName() != table.uniqueName())
	{
		ins +=		", "+ table.uniqueName();
		values +=	", '"+ uniqueValue +"'";
	}

	if(!table.attInitialTime().empty())
	{
		TeTime time (elem->timeInterval().getT1());
		ins +=		", "+ table.attInitialTime();
		values +=	", "+ db->getSQLTime(time); 
	}

	if((!table.attFinalTime().empty()) && (table.attInitialTime()!=table.attFinalTime()))
	{
		TeTime time (elem->timeInterval().getT2());
		ins +=		", "+ table.attFinalTime();
		values +=	", "+ db->getSQLTime(time); 
	}
	// ----------

	ins += ") "+ values +" )";
	
	if(!db->execute (ins))
		return false;

	return true;
}

template<typename Element>
bool updateRow (Element* elem, TeTable table, const string& uniqueId, TeDatabase* db, vector<int>* indexes)
{
	
	vector<string> attrs;
	table.attributeNames(attrs);

	string ins = " UPDATE "+ table.name() +" SET ";
	
	TePropertyVector prop = elem->getPropertyVector();
	int count = 0;
	for(unsigned int i=0; i<prop.size(); ++i)
	{
		if(indexes && (find(indexes->begin(), indexes->end(),(int)i) == indexes->end()))
			continue;

		string attrName = prop[i].attr_.rep_.name_;
		size_t pos = attrName.find(".", 0, 1);

		std::string tableName; 
		if (pos != string::npos)
		{
			attrName = attrName.substr(pos+1);
			tableName = attrName.substr(0, pos-1);
			if (tableName != table.name())
               continue;
		} 

		if( (find(attrs.begin(), attrs.end(), attrName)!=attrs.end()) &&
			(TeStringCompare(attrName,table.uniqueName()) == false) && 
			(TeStringCompare(attrName,table.linkName()) == false) && 
			(TeStringCompare(attrName,table.attInitialTime()) == false) && 
			(TeStringCompare(attrName,table.attFinalTime()) == false) )
		{
		
			if((prop[i].attr_.rep_.type_!=TeSTRING) && (prop[i].value_.empty()))
				continue; 

			if(count>0)
				ins += ",";
			
			++count;
			ins += attrName +" = ";
		
			if(prop[i].attr_.rep_.type_==TeSTRING)
				ins += "'"+ prop[i].value_ +"'";
			else if(prop[i].attr_.rep_.type_==TeREAL)
			{
				std::string strValue = prop[i].value_;
				replace (strValue.begin(), strValue.end(), ',', '.');
				ins += strValue;
			}
			else if(prop[i].attr_.rep_.type_==TeDATETIME)
			{
				TeTime time(prop[i].value_, prop[i].attr_.dateChronon_, prop[i].attr_.dateTimeFormat_, prop[i].attr_.dateSeparator_, prop[i].attr_.timeSeparator_);  
				ins += db->getSQLTime(time);
			}
			else
				ins += prop[i].value_;
		}
	}

	// -------- timeInterval
	if(!table.attInitialTime().empty())
	{
		TeTime time (elem->timeInterval().getT1());
		if(count>0)
			ins +=	", "; 
		ins += table.attInitialTime() +" = ";
		ins +=	db->getSQLTime(time); 
		++count;
	}

	if((!table.attFinalTime().empty()) && (table.attInitialTime()!=table.attFinalTime()))
	{
		TeTime time (elem->timeInterval().getT2());
		if(count>0)
			ins +=	", "; 
		ins += table.attFinalTime() +" = ";
		ins +=	db->getSQLTime(time);
		++count;
	}
	// ----------

	if ( count == 0 ) 
		return true; 

	ins += " WHERE "+ table.uniqueName() +" = '"+ uniqueId +"'";
	
	if(!db->execute (ins))
		return false;

	return true;
}
 
/** \example createSTElementSetFromLayer.cpp
 	Shows how to create a new Spatial Temporal Element Set (STElementSet) from a layer
 */

/** \example createSTElementSetFromTheme.cpp
 	Shows how to create a new Spatial Temporal Element Set (STElementSet) from a theme
 */
#endif

