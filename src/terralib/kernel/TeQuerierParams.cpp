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

#include "TeQuerierParams.h" 
#include "TeDatabase.h"

TeQuerierParams::~TeQuerierParams()
{
	if(themeFlag_)
		delete theme_;
}

TeQuerierParams::TeQuerierParams(const TeQuerierParams& rhs)
{
	loadGeometries_ = rhs.loadGeometries_;  
	loadAllAttributes_ = rhs.loadAllAttributes_;
	loadAttrNames_ = rhs.loadAttrNames_;
	loadAttrNamesSeted_ = rhs.loadAttrNamesSeted_;
	themeFlag_ = false;
	layer_ = rhs.layer_;

	if ((rhs.themeFlag_) && layer_)
	{
		theme_ = new TeTheme("theme_", layer_); 
		TeAttrTableVector attrTables;
		layer_->getAttrTables(attrTables);
		theme_->setAttTables(attrTables);
		themeFlag_ = true;
	}
	else
		theme_ = rhs.theme_;

	fileName_ = rhs.fileName_;
	chr_ = rhs.chr_;
	objId_ = rhs.objId_;
	groupingAttr_ = rhs.groupingAttr_;
	strategy_ = rhs.strategy_;

	//restrictions
	selectedObjs_ = rhs.selectedObjs_;  
	spatialRelation_ = rhs.spatialRelation_; 
	hasSpatialRes_ = rhs.hasSpatialRes_; 
	boxRest_ = rhs.boxRest_;	
	geomRest_ = rhs.geomRest_;
	geomRepRest_ = rhs.geomRepRest_;
}


TeQuerierParams&
TeQuerierParams::operator=(const TeQuerierParams& rhs)
{
	if (&rhs != this)
	{
		loadGeometries_ = rhs.loadGeometries_;  
		loadAllAttributes_ = rhs.loadAllAttributes_;
		loadAttrNames_ = rhs.loadAttrNames_;
		loadAttrNamesSeted_ = rhs.loadAttrNamesSeted_;
		themeFlag_ = false;
		layer_ = rhs.layer_;

		if ((rhs.themeFlag_) && layer_)
		{
			theme_ = new TeTheme("theme_", layer_); 
			TeAttrTableVector attrTables;
			layer_->getAttrTables(attrTables);
			theme_->setAttTables(attrTables);
			themeFlag_ = true;
		}
		else
			theme_ = rhs.theme_;

		fileName_ = rhs.fileName_;
		chr_ = rhs.chr_;
		objId_ = rhs.objId_;
		groupingAttr_ = rhs.groupingAttr_;
		strategy_ = rhs.strategy_;

		//restrictions
		selectedObjs_ = rhs.selectedObjs_;  
		spatialRelation_ = rhs.spatialRelation_; 
		hasSpatialRes_ = rhs.hasSpatialRes_; 
		boxRest_ = rhs.boxRest_;	
		geomRest_ = rhs.geomRest_;
		geomRepRest_ = rhs.geomRepRest_;
	}
	return *this;
}

void 
TeQuerierParams::setParams(TeLayer* layer)
{
	layer_ = layer;
	if(theme_ == 0)
	{
		theme_ = new TeTheme("theme_", layer_); 
		TeAttrTableVector attrTables;
		theme_->layer(layer_);
		layer_->getAttrTables(attrTables);
		theme_->setAttTables(attrTables);
		themeFlag_ = true;
	}
	strategy_ = "querierDBStr1";
}	

void 
TeQuerierParams::setParams(TeTheme* theme, TeChronon chr)
{
	theme_ = theme;
	chr_ = chr;
	strategy_ = "querierDBStr1"; 

	bool geomRep = (theme_->layer()->hasGeometry (TeCELLS) || theme_->layer()->hasGeometry (TePOINTS));
	int numRepres = theme_->layer()->vectRepres().size();
	
	bool groupAttr = !(groupingAttr_.empty());
	bool hasOneTable = true;

	if(groupAttr)
	{
		string lastTable = "";
		TeGroupingAttr::iterator it = groupingAttr_.begin();
		while(it!=groupingAttr_.end())
		{
			string tableName;
			size_t pos = (it->first.name_).find(".", 0, 1);
			if (pos != string::npos)
				tableName = (it->first.name_).substr(0,pos);

			if((!lastTable.empty()) && (lastTable!=tableName))
			{
				hasOneTable = false;
				break;
			}
			lastTable = tableName;
			++it;
		}
		
		//verify if the statistis can be calculated by SGBD
		TeGroupingAttr groups = groupingAttr_; 
		string sGroup = theme->layer()->database()->getSQLStatistics(groups);
		TeGroupingAttr::iterator itgroup = groups.begin();
		while(itgroup!= groups.end())
		{
			if(itgroup->second != TeNOSTATISTIC)
			{
				strategy_ = "querierDBStr1";
				return;
			}
			++itgroup;
		}
	}

	if( groupAttr && hasOneTable && (!loadGeom()))
		strategy_ = "querierDBStr2";
	else if( groupAttr && hasOneTable && loadGeom() && geomRep && (numRepres==1))
		strategy_ = "querierDBStr3";

	return;
}

void 
TeQuerierParams::setParams(TeTheme* theme, const string& objId, TeChronon chr)
{
	objId_ = objId;
	setParams(theme, chr);
}

void 
TeQuerierParams::setParams(const string& fileName, TeChronon chr)
{
	fileName_ = fileName;
	chr_ = chr;
	strategy_ = "querierSHP";
}


void 
TeQuerierParams::setFillParams(bool loadGeom, bool loadAllAttr, vector<string> loadAttr)
{
	loadGeometries_ = loadGeom;
	loadAllAttributes_ = loadAllAttr;
	
	if(!loadAttr.empty()) 
		loadAllAttributes_ = false;
	
	loadAttrNames_ = loadAttr;
	loadAttrNamesSeted_ = loadAttr;
	groupingAttr_.clear(); 

	if(theme_)
		setParams(theme_, objId_, chr_);
}

void 
TeQuerierParams::setFillParams(bool loadGeom, TeGroupingAttr attrG)
{
	loadGeometries_ = loadGeom;
	loadAttrNames_.clear(); 
	loadAttrNamesSeted_.clear(); 
	groupingAttr_ = attrG;

	if(theme_)
		setParams(theme_, objId_, chr_);
}

void 
TeQuerierParams::setSpatialRest(TeBox& box, int relation, TeGeomRep rep)
{
	hasSpatialRes_ = true;
	boxRest_ = box;
	spatialRelation_ = relation;
	
	if(rep==TeGEOMETRYNONE)
		geomRepRest_ = theme_->layer()->vectRepres()[0]->geomRep_;
	else
		geomRepRest_ = rep;
	
	geomRest_ = 0;
}

void 
TeQuerierParams::setSpatialRest(TeGeometry* geom, int relation, TeGeomRep rep)
{
	hasSpatialRes_ = true;
	geomRest_ = geom;
	spatialRelation_ = relation;
	
	if(rep==TeGEOMETRYNONE)
		geomRepRest_ = theme_->layer()->vectRepres()[0]->geomRep_;
	else
		geomRepRest_ = rep;
	
	boxRest_ = TeBox();
}

TeBox& 
TeQuerierParams::box()
{
	return box_; 
}

void
TeQuerierParams::clear()
{
	loadAttrNames_.clear();			
	loadAttrNamesSeted_.clear();	
	groupingAttr_.clear();
	if(themeFlag_)
		delete theme_;
	themeFlag_=false;
	theme_ = 0;
	layer_ = 0;				
	fileName_ = "";
	box_ = TeBox();
	chr_ = TeNOCHRONON; 
	objId_ = "";
	strategy_="";
	selectedObjs_=TeAll;  
	spatialRelation_=TeUNDEFINEDREL;	
	hasSpatialRes_ = false;		
	boxRest_=TeBox();
	geomRest_=0;	
	geomRepRest_=TeGEOMETRYNONE;	
}


