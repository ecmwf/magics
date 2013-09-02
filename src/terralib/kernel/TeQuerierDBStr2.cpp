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

#include "TeQuerierDBStr2.h"
#include "TeDatabase.h"
#include "TeSTInstance.h"

//It must group the attributes of ONE attribute table using DBMS functions
// and do not load geometries. It must be used with theme (with collection table).
bool
TeQuerierDBStr2::initPortal(TeTSEntry* ent)  
{
	string selectClause, fromClause, whereClause, groupByClause;
	string initialTime, finalTime;
	linkIndex_.clear();
	linkIndex_.push_back(-1); 
	attrIndex1_=-1;
	attrIndex2_=-1;
	timeIndex1_.clear();
	timeIndex1_.push_back(-1);
	timeIndex2_.clear();
	timeIndex2_.push_back(-1);
	groupIndex_=-1;

	TeDatabase* db = params_->theme()->layer()->database();

	if(params_->groupAttr().empty())
		return false;

	//get the table name (there is only one attribute table)
	string tableName;
	TeGroupingAttr groupAttr = params_->groupAttr();
	TeGroupingAttr::iterator it = groupAttr.begin();
	size_t pos = (it->first.name_).find(".", 0, 1);
	if (pos != string::npos)
		tableName = (it->first.name_).substr(0,pos);

	//group information
	legendIdGroup_.clear();
	legendIdGroup_[0]=0;
	TeLegendEntryVector& legVec = params_->theme()->legend();
	for(unsigned int l=0; l<legVec.size(); ++l)
		legendIdGroup_[legVec[l].id()]=legVec[l].group();
		
	clearVectors();

	// ---------------------------------------------  Mount SQL
	
	//------- Select and Group By clause
	//load the attribute table 
	int index = -1;
	TeAttrTableVector attr = params_->theme()->attrTables();
	for(unsigned int i=0; i<attr.size(); ++i)
	{
		if(TeConvertToUpperCase(attr[i].name()) == TeConvertToUpperCase(tableName))
		{
			index = i;
			attrTable_ = attr[i];
			break;
		}
	}
	
	//not find the table
	if(index<0)
		return false;

	//verify the attribute table
	if((ent) && (attrTable_.tableType()!=TeAttrEvent) && (attrTable_.tableType()!=TeFixedGeomDynAttr))
		return false;
 	
	//object_id
	string linkName; 
	if(attrTable_.tableType() != TeAttrExternal)
		linkName = attrTable_.name() +"."+ attrTable_.linkName(); 
	else
		linkName = params_->theme()->collectionTable() +".c_object_id "; 

	if(params_->hasSpatialRes())
		selectClause = " MIN("+ linkName +")";		
	else
		selectClause = linkName;	
	linkIndex_[0]=0;

	if(ent)
	{
		//date
		initialTime = attrTable_.name() +"."+ attrTable_.attInitialTime();
		
		groupByClause = db->getSQLTemporalFunction(params_->chronon(), initialTime);
		if(!params_->hasSpatialRes())
			groupByClause += ","+ linkName;
				
		selectClause += ", MAX("+ initialTime +")";
		timeIndex1_[0]=1;
		timeIndex2_[0]=1;
	}
	else 
		groupByClause = linkName;

	
	//statistic
	selectClause += ","+ db->getSQLStatistics(groupAttr);  
	if(timeIndex1_[0]<0)
		attrIndex1_=1;
	else
		attrIndex1_=2;
	attrIndex2_ = attrIndex1_+(params_->groupAttr().size()-1);

	//group index
	selectClause += ", MIN("+ params_->theme()->collectionTable()+".c_legend_id )";
	groupIndex_ = attrIndex2_+1;		

	//---- from clause 
	if(params_->hasSpatialRes())
	{
		groupByClause = ""; //It groups all attributes from all objects 
		string gTable = params_->theme()->layer()->tableName(params_->geomRepRest());
		fromClause = this->sqlFrom(gTable);
	}
	else
		fromClause = this->sqlFrom();
	
	if(fromClause.empty())
		return false;
	
	//where clause
	if(!params_->objId().empty())
		whereClause = linkName +" = '"+ params_->objId() +"'";  

	//---------- querier restriction
	string sqlQuerierRest = sqlWhereRestrictions();
	if(!whereClause.empty())
		whereClause += " AND ";

	whereClause += sqlQuerierRest;

	//----------
	
	string sql = " SELECT "+ selectClause; 
	sql += " FROM "+ fromClause;
	if(!whereClause.empty())
		sql += " WHERE "+ whereClause; 
	if(!groupByClause.empty())
		sql += " GROUP BY "+ groupByClause;
	
	// ---------------------------------------------  Submit the query
	
	portals_.clear();
	TeDatabasePortal* portal = db->getPortal();
	
	if(!portal)
		return false;
	
	if(!portal->query(sql))
	{
		delete (portal);
		flagPortal_ = false;
		return false;
	}

	if(!portal->fetchRow())
	{
		delete (portal);
		flagPortal_ = false;
		return false;
	}

	string id = portal->getData(linkIndex_[0]); //object_Id
	bool flag = true;
	while(id.empty() && flag) 
	{
		flag = portal->fetchRow();
		if(flag)
			id = portal->getData(linkIndex_[0]); //object_Id
	}
	
	if(id.empty() || (!flag)) //se o id �vazio ou chegou ao final do portal 
	{
		delete (portal);
		flagPortal_ = false;
		return false;
	}

	portals_.push_back (portal);
	flagPortal_ = true;
	return true;
}


bool
TeQuerierDBStr2::fillSTO(TeSTInstance& sto)  
{
	if(portals_.empty())
		return false;

	TeDatabasePortal* portal = portals_[0]; 
	if(!portal) 
		return false; 

	if(!flagPortal_) // end of portal
	{
		clearVectors();
		return false; 
	}

	if(TSEntry_.timeFrame_ > -1)
	{
		// first : verify the time
		TeTime t1 = portal->getDate(timeIndex1_[0]);		
		t1.chronon ((TSEntry_.time_).intervalChronon());
			
		//if the time is before the requested interval 
		while ( (TSEntry_.time_).before(t1) && flagPortal_)
		{
			flagPortal_ = portal->fetchRow();
			t1 = portal->getDate(timeIndex1_[0]);
			t1.chronon ((TSEntry_.time_).intervalChronon());
		}
	
		if(!(TSEntry_.time_).during(t1))
			return false;
		
		TeTimeInterval interval(t1, t1);
		sto.timeInterval(interval);

		timeFramePortal_ = (TSEntry_.timeFrame_)+1; //next time frame 
	}

	//if there is spatial restriction, it groups all instances of all objects
	if(!params_->hasSpatialRes())
	{
		// get object_id
		string objId = string(portal->getData(linkIndex_[0]));  // 0: object_id
		sto.objectId (objId);
		sto.addUniqueId (objId);

		//get the group index
		if(groupIndex_!=-1)
		{
			int gIndex = portal->getInt(groupIndex_);
			sto.setSlice(legendIdGroup_[gIndex]);
		}
	}

	//get property
	for(int s=attrIndex1_; s<=attrIndex2_; ++s)  
		sto.addPropertyValue(string(portal->getData(s)));

	sto.setAttrList(attrList_);
	flagPortal_ = portal->fetchRow();
	return true;
}


bool
TeQuerierDBStr2::loadInstances(TeTSEntry* ent) 
{
	if(!params_->theme())
		return false;

	//set a invalid box
	TeBox box;
	params_->box(box);

	if(ent)
	{
		//if the portal was not initialised (timeFramePortal_==-1) or
		//if the time frame requested is lesser than the time frame appointed by the portal
		if((timeFramePortal_<0) || (ent->timeFrame_<timeFramePortal_))
		{
			if(!initPortal(ent))
				return false;

			timeFramePortal_ = 0;
		}
		TSEntry_ = (*ent);
	}
	else // no chronon
	{
		if(!initPortal())
			return false;
 
		timeFramePortal_ = -1;
	}

	attrList_->clear();
	TeAttribute at;
	TeGroupingAttr::iterator it = params()->groupAttr().begin();
	while(it!=params()->groupAttr().end())
	{
		TeAttribute at;
		at.rep_ = it->first;
		attrList_->push_back (at);
		++it;
	}

	return true;
}

bool
TeQuerierDBStr2::fetchInstance(TeSTInstance&  sto) 
{
	sto.clear();

	if(!fillSTO(sto))
		return false; 
	
	//build a temporal serie to one object
	string val;
	sto.getPropertyValue(val, 0);
	if((ts_) && (ts_->setTSEntryValue(TSEntry_.timeFrame_, atof(val.c_str()), TeDATA)))
		filledSerie_ = true;
	
	return true;
}

int 
TeQuerierDBStr2::numElemInstances()
{
	if((portals_.empty()))
		return 0;

	//get the first portal
	if(!portals_[0])
		return 0;

	return (portals_[0]->numRows());
}


