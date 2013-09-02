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

#include "TeQuerierDBStr1.h"
#include "TeDatabase.h"
#include "TeTemporalSeries.h"
#include "TeSTInstance.h"

bool 
TeQuerierDBStr1::initPortal(TeRepresentation& rep, TeTSEntry* ent)
{
	string selectClause, fromClause, whereClause, orderByClause, groupByClause, parClause;
	
	//Init portal indexes
	isGroup_ = false;
	uniqueIndex_.clear();
	vector<int> indexesAux;
	linkIndex_.clear();
	linkIndex_.push_back(-1); 
	attrIndex1_=-1;
	attrIndex2_=-1;
	groupIndex_=-1;
	
	geomIndex1_.clear();
	geomIndex1_.push_back(-1);
	geomIndex2_.clear();
	geomIndex2_.push_back(-1);
	timeIndex1_.clear();
	timeIndex1_.push_back(-1);
	timeIndex2_.clear();
	timeIndex2_.push_back(-1);

	//all vectors contains attributes with the table name and in upper case
	vector<string>	attrs_select;	//contains the attributes that will be used to fill the TeSTInstance
	vector<string>	geoms_select;	//contains the geometry attributes 
	vector<string>	times_select;	//contains the date time attributes, when they are not in the attrs_select
	vector<string>	unique_select;	//contains the unique attributes, when they are not in the attrs_select
	
	//------------------------------------------- begin get information
	//get database
	TeDatabase* db = params_->theme()->layer()->database();

	//get attribute tables
	TeAttrTableVector atts; 
	params_->theme()->getAttTables(atts); 
	
	//verify if will be group the objects	
	TeGroupingAttr groups = params_->groupAttr();  
	bool groupAttr = !(groups.empty());
		
	//verify if the theme has collection table
	bool hasCollTable = false;
	legendIdGroup_.clear();
	legendIdGroup_[0]=0;
	if(db->tableExist(params_->theme()->collectionAuxTable()))
	{
		TeLegendEntryVector& legVec = params_->theme()->legend();
		for(unsigned int l=0; l<legVec.size(); ++l)
			legendIdGroup_[legVec[l].id()]=legVec[l].group();
		hasCollTable = true;
	}
		
	clearVectors();
	params_->setLoadAttrs(params_->loadSetedAttrs()); //the original data
	
	//get the link name - the first attribute table or geometry table
	linkName_ = rep.tableName_ +".object_id"; 
	if(atts.size() > 0)
		linkName_ = atts[0].name() +"."+ atts[0].linkName(); 
			
	if(params_->loadGeom())
		geomRepr_.push_back(rep);
	
	//------------------------------------------- end get information
	
	//------------------------------------------- begin information to mount SQL
	// -------- from
	bool fromNeedGeomTable = false;
	bool fromNeedCollTable = false;
	
	if(params_->loadGeom() || params_->hasSpatialRes() || atts.empty())
		fromNeedGeomTable = true;
	if(hasCollTable)
		fromNeedCollTable = true;

	// -------- group and order by 
	bool useGroupByClause = false;	//group by object_id
	bool useOrderByClause = false;  //order by object_id, geom_id
	//Use group by clause when 
	//1)it must group the attributes and 
	//2)it must not load geometry and 
	//3)all statistic functions exist in the SGBD 
	string sGroup="";
	if(groupAttr)
	{
		if(params_->loadGeom())
			useGroupByClause = false; 
		else
		{
			sGroup = db->getSQLStatistics(groups);
			isGroup_ = true;
			TeGroupingAttr::iterator it = groups.begin();
			while(it!= groups.end())
			{
				if(it->second != TeNOSTATISTIC)
					isGroup_ = false;
				++it;
			}
			if(isGroup_)
				useGroupByClause = true; 
		}
		groupInMemory_ = !useGroupByClause;
	}
	else
		useOrderByClause = true;

	// -------- select
	bool selectNeedGeom = false;
	if(params_->loadGeom())
		selectNeedGeom = true;
	
	//------------------------------------------- end information to mount SQL
	
	//------------------------------------------- begin mount SQL
	// order and group by clause
	orderByClause = " ORDER BY "+ linkName_; 
	if(selectNeedGeom)
		orderByClause += ", "+ rep.tableName_ +".geom_id";
	groupByClause = " GROUP BY "+ linkName_;

	// ------------- select clause
	if(groupAttr)
	{
		if(!groupInMemory_) //use statistic function database
		{
			bool flag = true;
			string auxS = sGroup;
			while(flag)
			{
				size_t pos = auxS.find(",", 0, 1);
				if (pos == string::npos)
				{
					attrs_select.push_back(auxS);
					flag = false;
				}
				else
				{
					attrs_select.push_back(auxS.substr(0, pos));
					auxS = auxS.substr(pos+1);
				}
			}
		}
		else //load the attributes from TeGroupingAttr
		{
			//fill select clause from set of attributes
			string lastAttr = "";
			TeGroupingAttr::iterator it = groups.begin(); 
			while(it!= groups.end())
			{
				if(lastAttr != it->first.name_) 
					attrs_select.push_back(it->first.name_);
				lastAttr = it->first.name_;
				++it;
			}

			// select datatime information
			if(!attrTable_.name().empty())
			{
				times_select.push_back(attrTable_.name()+"."+attrTable_.attInitialTime());

				if(attrTable_.attInitialTime()!=attrTable_.attFinalTime())
					times_select.push_back(attrTable_.name()+"."+attrTable_.attFinalTime());
			}
		}
	}
	else // load all attributes or the attributes that are in the vector
	{
		// get some information about the attribute tables required
		for(unsigned int i=0; i<atts.size(); i++)
		{
			//date time information
			if((atts[i].tableType()==TeAttrEvent) || (atts[i].tableType()==TeFixedGeomDynAttr))
			{
				attrTable_ = atts[i]; 
				times_select.push_back(atts[i].name()+"."+attrTable_.attInitialTime());
				times_select.push_back(atts[i].name()+"."+attrTable_.attFinalTime());
			}
		
			//unique information
			unique_select.push_back(atts[i].name()+"."+atts[i].uniqueName());
			indexesAux.push_back(-1);
						
			//fill vector of attributes and sql string with all attributes
			if(params_->loadAllAttr())
			{
				TeAttributeList::iterator itAttr = atts[i].attributeList().begin();
				while(itAttr!= atts[i].attributeList().end())
				{
					string attribute = atts[i].name() +"."+ (*itAttr).rep_.name_;
					attrs_select.push_back(attribute);
                    ++itAttr;
				}
			}
		}

		if(!params_->loadAllAttr())
		{
			vector<string>::iterator itVec = params_->loadAttrs().begin();
			while(itVec!=params_->loadAttrs().end())
			{
				//insert in select clause itVec: deve estar no formato tableName.attrName
				attrs_select.push_back (*itVec);
				++itVec;
			}
		}
	}
	
	if(selectNeedGeom)
	{
		TeAttributeList attrs;
		if (!db->getAttributeList(rep.tableName_, attrs))
			return false; 

		for(unsigned int i=0; i<attrs.size(); ++i)
			geoms_select.push_back(rep.tableName_+"."+ attrs[i].rep_.name_);
		
		// order by clause
		if((rep.geomRep_ == TePOLYGONS) && (db->dbmsName() != "OracleSpatial") && (db->dbmsName() != "PostGIS") )
			orderByClause += ", parent_id ASC, num_holes DESC";
	}
	
	// ------------- from and where clause
	if(!fromNeedCollTable)
	{
		if(fromNeedGeomTable)
			fromClause = " FROM " + tableJoin(atts, rep.tableName_, "object_id");
		else
			fromClause = " FROM " + tableJoin(atts);
		
		//if the theme does not have collection, it must apply its restrictions (attribute, temporal and spatial)
		whereClause = params_->theme()->sqlWhereRestrictions(&rep); 
	}
	else
	{
		if(fromNeedGeomTable)
			fromClause = params_->theme()->sqlGridFrom(rep.tableName_);
		else
			fromClause = params_->theme()->sqlGridFrom();
	}

	if(ent)
	{
		if(attrTable_.name().empty())
			params_->theme()->getTemporalTable(attrTable_);

		string iniTime = attrTable_.name()+"."+attrTable_.attInitialTime(); 
		string finTime = attrTable_.name()+"."+attrTable_.attFinalTime(); 
		if(!whereClause.empty())
			whereClause += " AND ";

		if ((params_->chronon()==TeMONTHOFYEAR) || (params_->chronon()==TeDAYOFWEEK))
			whereClause += db->getSQLTemporalWhere(ent->timeInt_, ent->timeInt_, params_->chronon(), TeTIMEDURING, iniTime, finTime);    
		else
		{
			TeTimeInterval interval = ent->time_;
			interval.intervalChronon(params_->chronon());

			whereClause += db->getSQLTemporalWhere(interval, TeTIMEDURING, iniTime, finTime);
		}
	}

	if(!objectId().empty())
	{
		if(!whereClause.empty())
			whereClause += " AND ";
		
		whereClause += linkName_ +" = '"+ objectId() +"'";  
	}

	//---------- mount select clause and get the indexes
	unsigned int index=0;
	unsigned int count=0;
	selectClause = "";
	uniqueIndex_[0] = indexesAux;
	
	//this attribute list can contain datetime, link and unique information
	attrIndex1_ = count;
	for(index=0; index<attrs_select.size(); ++index)
	{
		if(!selectClause.empty())
			selectClause += ", ";
		selectClause += attrs_select[index];
		//link index
		if(TeConvertToUpperCase(linkName_)==TeConvertToUpperCase(attrs_select[index]))
			linkIndex_[0] = count;
		//unique index
		for(unsigned int i=0; i<unique_select.size(); ++i)
		{
			if(TeConvertToUpperCase(unique_select[i])==TeConvertToUpperCase(attrs_select[index]))
				uniqueIndex_[0][i] = count;
		}
		
		//time index
		for(unsigned int i=0; i<times_select.size(); ++i)
		{
			if(TeConvertToUpperCase(times_select[i])==TeConvertToUpperCase(attrs_select[index]))
			{
				if(i==0)
					timeIndex1_[0] = count;
				else
					timeIndex2_[0] = count;
			}
		}
		if(timeIndex1_[0]>=0 && timeIndex2_[0]<0)
			timeIndex2_[0] = timeIndex1_[0];
		
		attrIndex2_ = count;
		++count;
	}

	//link attribute
	if(linkIndex_[0]<0)
	{
		if(!selectClause.empty())
			selectClause += ", ";
		selectClause += linkName_;
		linkIndex_[0] = count;
		++count;
	}
	
	//unique attributes
	for(index=0; index<unique_select.size(); ++index)
	{
		if(uniqueIndex_[0][index]<0)
		{
			//verify if the unique name is equal to link name
			if(TeConvertToUpperCase(unique_select[index]) == TeConvertToUpperCase(linkName_))
				uniqueIndex_[0][index] = linkIndex_[0];
			else
			{
				//adds the unique attributes that were not added
				if(!selectClause.empty())
					selectClause += ", ";
				selectClause += unique_select[index];
				uniqueIndex_[0][index] = count; 
				++count;
			}
		}
	}

	//group index
	if(hasCollTable)
	{
		if(!selectClause.empty())
			selectClause += ", ";
		if(useGroupByClause)
			selectClause += " MIN("+ params_->theme()->collectionTable()+".c_legend_id )";
		else
			selectClause += params_->theme()->collectionTable()+".c_legend_id";
			
		groupIndex_ = count;
		++count;
	}

	//time attributes
	if(timeIndex1_[0]<0 && (!times_select.empty()))
	{
		timeIndex1_[0] = count;
		for(index=0; index<times_select.size(); ++index)
		{
			if(!selectClause.empty())
				selectClause += ", ";
			selectClause += times_select[index];
			timeIndex2_[0] = count;
			++count;
		}
	}

	//geometries
	if(!geoms_select.empty())
		geomIndex1_[0] = count;
	for(index=0; index<geoms_select.size(); ++index)
	{
		if(!selectClause.empty())
			selectClause += ", ";
		selectClause += geoms_select[index];
		geomIndex2_[0] = count;
		++count;
	}
	
	//---------- querier restriction
	string sqlQuerierRest = sqlWhereRestrictions(&rep);
	if(!whereClause.empty())
		whereClause += " AND ";

	whereClause += sqlQuerierRest;

	fromClause_ = fromClause;
	whereClause_ = whereClause;
    string sql = "SELECT "+ selectClause + fromClause;
	if(!whereClause.empty())
		sql += " WHERE "+ whereClause;
	
	if(useGroupByClause)
		sql += " "+ groupByClause;
	else
		sql += " "+ orderByClause;
	
	//------------------------------------------- end mount SQL

	// --------- Submit the query
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

	string id = portal->getData(linkIndex_[0]);
	bool flag = true;
	while(id.empty() && flag) 
	{
		flag = portal->fetchRow();
		if(flag)
			id = portal->getData(0); //object_Id
	}
	
	if(id.empty() || (!flag))
	{
		delete (portal);
		flagPortal_ = false;
		return false;
	}

	portals_.push_back (portal);
	flagPortal_ = true;
	lastObjId_ ="";
	return true;
}

bool 
TeQuerierDBStr1::initGeomPortal(TeRepresentation& rep, TeTSEntry* ent)
{
	string selectClause, fromClause, whereClause, orderByClause, parClause;
	string initialTime, finalTime;
	string uniqueName;
	vector<int> indexesAux;
	linkIndex_.push_back(-1); 
	geomIndex1_.push_back(-1);
	geomIndex2_.push_back(-1);
	timeIndex1_.push_back(-1);
	timeIndex2_.push_back(-1);
	
	TeDatabase* db = params_->theme()->layer()->database();

	//------- Get geometry table
	geomRepr_.push_back(rep);
	if(rep.tableName_.empty())
		return false;

	int ind = linkIndex_.size()-1; //the last position in the vectors
	
	selectClause = linkName_; 
	orderByClause = " ORDER BY "+ linkName_;  
	linkIndex_[ind] = 0;
	
	if((rep.geomRep_ == TePOLYGONS) && (db->dbmsName() != "OracleSpatial") && (db->dbmsName() != "PostGIS") )
		orderByClause += " , parent_id ASC, num_holes DESC";
	
	//------- Get temporal attribute 
	if(!attrTable_.name().empty())
	{
		initialTime = attrTable_.name() +"."+ attrTable_.attInitialTime ();
		finalTime = attrTable_.name() +"."+ attrTable_.attFinalTime ();
			
		// fill vector of unique name 
		uniqueName = attrTable_.name()+"."+attrTable_.uniqueName();
								
		selectClause += ", "+ uniqueName;
		indexesAux.push_back(1);
		selectClause += ", "+ initialTime;
		timeIndex1_[ind] = 2;
		timeIndex2_[ind] = 2;

		if (initialTime != finalTime)
		{
			selectClause += ", "+ finalTime;
			timeIndex2_[ind] = 3;
		}
	}

	//adds geometry attributes
	selectClause += ","+ rep.tableName_ +".* ";
	uniqueIndex_[ind] = indexesAux;
	if(indexesAux.empty())
		geomIndex1_[ind] = 1;
	else if(timeIndex1_[ind]==timeIndex2_[ind])
		geomIndex1_[ind] = 3;
	else
		geomIndex1_[ind] = 4;
	
	if(!db->tableExist(params_->theme()->collectionAuxTable()))
	{
		TeAttrTableVector atts; 
		params_->theme()->getAttTables(atts); 

		if(atts.empty())
			return false;

		fromClause = " FROM " + tableJoin(atts, rep.tableName_, "object_id");
		whereClause = params_->theme()->sqlWhereRestrictions(&rep);
	}
	else
		fromClause = params_->theme()->sqlGridFrom(rep.tableName_);
	
	if(ent)
	{
		string iniTime = attrTable_.name()+"."+initialTime;
		string finTime = attrTable_.name()+"."+finalTime;
		if(!whereClause.empty())
			whereClause += " AND ";

		if ((params_->chronon()==TeMONTHOFYEAR) || (params_->chronon()==TeDAYOFWEEK))
			whereClause += db->getSQLTemporalWhere(ent->timeInt_, ent->timeInt_, params_->chronon(), TeTIMEDURING, iniTime, finTime);    
		else
		{
			TeTimeInterval interval = ent->time_;
			interval.intervalChronon(params_->chronon());

			whereClause += db->getSQLTemporalWhere(interval, TeTIMEDURING, iniTime, finTime);
		}
	}

	//where clause
	if(!objectId().empty())
	{
		if(!whereClause.empty())
			whereClause += " AND ";
		
		whereClause += rep.tableName_+".object_id = '"+ objectId() +"'";  
	}

	//---------- querier restriction
	string sqlQuerierRest = sqlWhereRestrictions(&rep);
	if(!whereClause.empty())
		whereClause += " AND ";

	whereClause += sqlQuerierRest;

	//----------

	string sql = "SELECT "+ selectClause + fromClause;
	if(!whereClause.empty())
		sql += " WHERE "+ whereClause;
	sql += orderByClause;

	//------ Submit the query
	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;
	
	if(!portal->query(sql))
	{
		delete portal;
		return false;
	}

	if(portal->fetchRow())
		portals_.push_back (portal); 
	else
		delete portal;

	return true;
}


// ---------- final - initPortal
// It is used in two cases:
// 1) when it must return all instances of an object 
// 2) when the attributes was grouped by sql functions - without geometry 

bool
TeQuerierDBStr1::fillSTOGrouped(TeSTInstance& sto, bool fetchInstance)
{
	if(portals_.empty()) 
		return false;

	TeDatabasePortal* portal = portals_[0]; 
	if(!portal) 
		return false; 

	TeRepresentation* rep = 0;
	if(!geomRepr_.empty())
		rep = &(geomRepr_[0]);

	TeTime  minT1, maxT2;
	
	// builds a ST object instance
	TeTime t1, t2;
	vector<string> attrValues;

	// verify if exist several geometries for an object
	bool flagGeom = true;
	while(flagGeom && fetchInstance)
	{
		sto.objectId (portal->getData(linkIndex_[0]));
						
		if(rep && (!params_->loadGeom()))
		{
			int geomId = portal->getInt(rep->tableName_ +".geom_id");
			
			if(sto.objectId()==lastObjId_) 
			{
				if (geomId_!=geomId)
				{
					flagGeom = portal->fetchRow();
					continue;
				}
			}
			else
				geomId_ = geomId;
		}
		flagGeom = false;
	}	
	
	// ------------- begin fill attribute
	// process the records filling the parameters of each instance
	TeAttributeList& attrsPortal = portal->getAttributeList();
	
	//get the attributes
	for(int s=attrIndex1_; s<=attrIndex2_; ++s)
	{
		if(s<0)
			break;
		attrValues.push_back(portal->getData(s));
		if(!fetchInstance) //fill the attribute list
			attrList_->push_back(attrsPortal[s]);
	}

	//get the group index
	if(groupIndex_!=-1)
	{
		int gIndex = portal->getInt(groupIndex_);
		sto.setSlice(legendIdGroup_[gIndex]);
	}
	
	if(isGroup_)  // it was grouped by sql functions and without geometry 
		sto.addUniqueId(string(portal->getData(linkIndex_[0])));
	else
	{
		//unique id
		for(unsigned int s=0; s<uniqueIndex_[0].size(); ++s)
		{
			string uniqueValue = portal->getData(uniqueIndex_[0][s]);
			sto.addUniqueId(uniqueValue);
		}

		//date time information
		if(timeIndex1_[0]>=0 && timeIndex2_[0]>=0)
		{
			t1 = portal->getDate(timeIndex1_[0]);
			t2 = portal->getDate(timeIndex2_[0]);
			sto.timeInterval(TeTimeInterval(t1,t2));
		}
	}

	//! Set the property in the stoInstance
	sto.setProperties(attrValues); 
	sto.setAttrList(attrList_);

	if(!fetchInstance) //only to get the attributes
		return true;

	// ------------- end fill attribute
	//total time
	if(t1.isValid() && t1<minT1)
		minT1 = t1;
	if(t2.isValid() && maxT2<t2)
		maxT2 = t2;
		
	// ------------- begin fill geometries  
	if(params_->loadGeom())
	{
		if(t1.isValid() && t2.isValid())
			flagPortal_ = addGeometry(portal, rep->geomRep_, sto, linkIndex_[0], geomIndex1_[0], TeTimeInterval(t1,t2), timeIndex1_[0], timeIndex2_[0]);
		else
			flagPortal_ = addGeometry(portal, rep->geomRep_, sto, linkIndex_[0], geomIndex1_[0]);
	}
	else
		flagPortal_ = portal->fetchRow();

	// ------------- end fill geometries
	
	lastObjId_ = sto.objectId(); 
	return true;
}

bool 
TeQuerierDBStr1::fillGeomSTO(TeSTInstance& sto, unsigned int index)
{
	if((portals_.size()<(index+1)) || (geomRepr_.size()<(index+1)))
		return false;

	TeDatabasePortal* portal = portals_[index]; 
	TeRepresentation rep = geomRepr_[index];
	if(!portal)
		return false; 

	TeTimeInterval time = sto.timeInterval();
	bool flag = false;
	if(time.isValid())
		flag = addGeometry(portal, rep.geomRep_, sto, linkIndex_[index], geomIndex1_[index], time, timeIndex1_[index], timeIndex2_[index]);
	else
		flag = addGeometry(portal, rep.geomRep_, sto, linkIndex_[index], geomIndex1_[index] );

	if(flag==false)
		flagPortal_ = false; 
	
	return true;
}

// It is used in a case:
// 1) when the attributes of the object instances must be grouping in memory - with or without geometry 
bool
TeQuerierDBStr1::fillSTONoGrouped(TeSTInstance& sto)
{
	int s;
	if(portals_.empty())
		return false;

	TeDatabasePortal* portal = portals_[0]; 
	if(!portal) 
		return false; 

	TeRepresentation* rep = 0; 
	if(!geomRepr_.empty())
		rep = &(geomRepr_[0]);
	
	map<int, vector<double> >	valuesDouble;
	map<int, vector<string> >	valuesString;
	string lastObj = "";

	TeAttributeList& attrsPortal = portal->getAttributeList();
	
	//get the group index
	if(groupIndex_!=-1)
	{
		int gIndex = portal->getInt(groupIndex_);
		sto.setSlice(legendIdGroup_[gIndex]);
	}

	//when there is spatial restriction, the querier can not group the attributes and
	//get the geometry at the same time. In this case, it do not get the geometry.
	if(!params_->loadGeom() || params_->hasSpatialRes())
	{
		int cont = 0;
		do
		{
			string objId = portal->getData(linkIndex_[0]);
			//if there is a spatial restriction, it groups all instances of all objects of the portal 
			if(!params_->hasSpatialRes() && (!lastObj.empty() && objId != lastObj))
				break;  
					
			string val = ""; 
			//portal has the attributes that will be grouped 
			for(s=attrIndex1_; s<=attrIndex2_; ++s)
			{
				if(s<0)
					break;
				val = portal->getData(s);
				if((attrsPortal[s].rep_.type_==TeREAL) || (attrsPortal[s].rep_.type_==TeINT))
				{
					if(val.empty())
						valuesDouble[s].push_back (TeMAXFLOAT);  //invalid value
					else
						valuesDouble[s].push_back (atof(val.c_str()));
				}
				else
					valuesString[s].push_back(val);
			}

			flagPortal_ = portal->fetchRow();
			lastObj = objId;
			++cont;

		} while(flagPortal_);
	}
	else // -- when it must fill the geometry
	{		
		//------- first: load the geometry, attributes and time of the first object 
		//object id
		lastObj = portal->getData(linkIndex_[0]);
		
		//attribute val
		string val = ""; 
		
		for(s=attrIndex1_; s<=attrIndex2_; ++s)
		{
			val = portal->getData(s);
			if((attrsPortal[s].rep_.type_==TeREAL) || (attrsPortal[s].rep_.type_==TeINT))
			{
				if(val.empty())
					valuesDouble[s].push_back (TeMAXFLOAT);  //invalid value
				else
					valuesDouble[s].push_back (atof(val.c_str()));
			}
			else
				valuesString[s].push_back (val);
		}

		//time
		TeTime t1, t2;
		if(!attrTable_.name().empty())
		{
			t1 = portal->getDate(timeIndex1_[0]);
			t2 = portal->getDate(timeIndex2_[0]);
		}
		
		sto.objectId(lastObj);
		if(t1.isValid() && t2.isValid())
			flagPortal_ = addGeometry(portal, rep->geomRep_, sto, linkIndex_[0], geomIndex1_[0], TeTimeInterval(t1,t2), timeIndex1_[0], timeIndex2_[0]);
		else
			flagPortal_ = addGeometry(portal, rep->geomRep_, sto, linkIndex_[0], geomIndex1_[0]);
				
		//------- second: load the attributes of the other objects
		int cont = 0;   
		while (flagPortal_)
		{
			string objId = portal->getData(linkIndex_[0]);
			//if there is a spatial restriction, it groups all instances of all objects of the portal 
			if(objId != lastObj)
				break;  
			
			//same object id
			string gId = portal->getData (geomIndex1_[0]);
			int geomId = atoi(gId.c_str());
			if(gId.empty())
				geomId = cont;   
			
			if(!cont)
				geomId_ = geomId;  //first geom_id 
			
			//verify the geomId
			if(geomId==geomId_)   
			{
				flagPortal_ = portal->fetchRow();
				lastObj = objId;
				continue;
			}
							
			val = ""; 
			for(s=attrIndex1_; s<=attrIndex2_; ++s)
			{
				val = portal->getData(s);
				if((attrsPortal[s].rep_.type_==TeREAL) || (attrsPortal[s].rep_.type_==TeINT))
				{
					if(val.empty())
						valuesDouble[s].push_back (TeMAXFLOAT);  //invalid value
					else
						valuesDouble[s].push_back (atof(val.c_str()));
				}
				else
					valuesString[s].push_back (val);
			}

			flagPortal_ = portal->fetchRow();
			lastObj = objId; 
			++cont;
		}
	}

	//fill the property from portal 
	//calculate the statistics
	string lastAttr = "";
	unsigned int j=0;
	string colName = "";
	TeGroupingAttr::iterator it = params()->groupAttr().begin(); 

	TeStatisticValMap		statDouble;
	TeStatisticStringValMap statString;
	
	TeAttrDataType attrType = TeSTRING;
	while(it!=params()->groupAttr().end())
	{
		if(lastAttr != it->first.name_)
		{
			statDouble.clear();
			statString.clear();
			//if((it->first.type_==TeREAL) || (it->first.type_==TeINT))
			if((attrsPortal[j].rep_.type_==TeREAL) || (attrsPortal[j].rep_.type_==TeINT))
			{
				TeCalculateStatistics(valuesDouble[j].begin(), valuesDouble[j].end(), statDouble);
				attrType = TeREAL;
			}
			else
			{
				TeCalculateStatistics(valuesString[j].begin(), valuesString[j].end(), statString);
				attrType = TeSTRING;
			}
			
			colName = attrsPortal[j].rep_.name_;
			++j;
		}

		double resultD=0;
		string resultS="";
		string prefix;
		switch(it->second)
		{
			case TeCOUNT:
				resultD = statDouble[TeCOUNT]; 
				resultS = statString[TeCOUNT];
				prefix = "count";
				break;
			case TeVALIDCOUNT:
				resultD = statDouble[TeVALIDCOUNT]; 
				resultS = statString[TeVALIDCOUNT];
				prefix = "valCount";
				break;
			case TeMINVALUE:
				resultD = statDouble[TeMINVALUE];
				resultS = statString[TeMINVALUE];
				prefix = "min";
				break;
			case TeMAXVALUE:
				resultD = statDouble[TeMAXVALUE];
				resultS = statString[TeMAXVALUE];
				prefix = "max";
				break;
			case TeSUM:
				resultD = statDouble[TeSUM]; 
				prefix = "sum";
				break;
			case TeMEAN:
				resultD = statDouble[TeMEAN]; 
				prefix = "mean";
				break;
			case TeSTANDARDDEVIATION:
				resultD = statDouble[TeSTANDARDDEVIATION];
				prefix = "stDeviation";
				break;
			case TeVARIANCE:
				resultD = statDouble[TeVARIANCE];
				prefix = "variance";
				break;
			case TeSKEWNESS:
				resultD = statDouble[TeSKEWNESS];
				prefix = "skwness";
				break;
			case TeKURTOSIS:
				resultD = statDouble[TeKURTOSIS]; 
				prefix = "kurtosis";
				break;
			case TeAMPLITUDE:
				resultD = statDouble[TeAMPLITUDE]; 
				prefix = "amplitude";
				break;
			case TeMEDIAN:
				resultD = statDouble[TeMEDIAN]; 
				prefix = "median";
				break;
			case TeVARCOEFF:
				resultD = statDouble[TeVARCOEFF]; 
				prefix = "varcoeff";
				break;
			case TeMODE: 
				resultD = statDouble[TeMODE]; 
				prefix = "mode";
				break;
			default:
				break;
		}
		
		lastAttr = it->first.name_; 
		++it;

		//fill the property 
		if(attrType == TeREAL)
			sto.addPropertyValue(Te2String(resultD));
		else
			sto.addPropertyValue(resultS);
	}
	
	//adds the property in the stoInstance
	if(!params_->hasSpatialRes())
	{
		sto.addUniqueId(lastObj);
		sto.objectId(lastObj);
	}
	sto.setAttrList(attrList_);

	lastObjId_ = sto.objectId(); 
	return true;
}


bool
TeQuerierDBStr1::loadInstances(TeTSEntry* ent)
{
	if(!params_->theme())
		return false;

	TeRepresPointerVector repres = params_->theme()->layer()->vectRepres();
	TeRepresentation rep;
	
	//fill instances - first fill the geometry representation of the spatial restriction 
	if(params_->theme()->hasSpatialRest())
		rep = **(TeFindRepresentation(repres.begin(), repres.end(), params_->theme()->geomRepRestriction()));
	else
	{
		for(int i=0; i<(int)repres.size(); ++i)
		{
			rep = *repres[i];
			break;
		}
	}
	
	if(!initPortal(rep, ent))
		return false;

	//get the attribute list
	attrList_->clear();
	if(!groupInMemory_)
	{
		TeSTInstance i;
		fillSTOGrouped(i, false);
	}
	else
	{
		TeGroupingAttr::iterator it = params()->groupAttr().begin();
		while(it!=params()->groupAttr().end())
		{
			TeAttribute at;
			at.rep_ = it->first;
			attrList_->push_back (at);
			++it;
		}
	}
			
	if(!params_->loadGeom())
		return true;

	//set a invalid box
	TeBox box;
	params_->box(box);
	
	for(unsigned int i=0; i<repres.size(); ++i)
	{
		if(repres[i]->geomRep_ == rep.geomRep_)
			continue;

		if(!initGeomPortal(*(repres[i]), ent))
		{
			clearVectors();
			return false;
		}
	}
	return true;
}

bool 
TeQuerierDBStr1::fetchInstance(TeSTInstance&  sto)  
{
	sto.clear();
	bool flag = false;

	if(!flagPortal_)
	{
		clearVectors();
		return false;
	}

	if(groupInMemory_)
		flag = fillSTONoGrouped(sto); 
	else
		flag = fillSTOGrouped(sto); 
	
	if((flag==false) || (!params_->loadGeom()))
		return flag;
    	
	for(unsigned int i=1; i<portals_.size(); ++i)
		flag = fillGeomSTO(sto, i);

	updateBox(params_->box(), sto.getGeometries().getBox());
	return flag;
}

int 
TeQuerierDBStr1::numElemInstances()
{
	//from clause has the geom table
	if(params_->loadGeom() || params_->hasSpatialRes())
	{
		//load the attribute tables
		TeAttrTableVector atts; 
		params_->theme()->getAttTables(atts); 
		if(atts.empty())
			return 0;

		string selectClause = " DISTINCT ";
		for(unsigned int i=0; i<atts.size(); ++i)
		{
			if(i>0)
				selectClause+= ",";
			selectClause+= atts[i].name() +"."+ atts[i].linkName();

			if(atts[i].linkName()!=atts[i].uniqueName())
				selectClause+= ","+ atts[i].name() +"."+ atts[i].uniqueName();
		}
		
        string sql = "SELECT "+ selectClause + fromClause_;
		if(!whereClause_.empty())
			sql += " WHERE "+ whereClause_;

		TeDatabasePortal* portal = params_->theme()->layer()->database()->getPortal();
		if(!portal)
			return 0;
		
		if(!portal->query(sql) || !portal->fetchRow())
		{
			delete (portal);
			return 0;
		}
		int result = portal->numRows();
		delete portal;
		return result;
	}
		
	if((portals_.empty()))
		return 0;

	//get the first portal
	if(!portals_[0])
		return 0;

	return (portals_[0]->numRows());
}




