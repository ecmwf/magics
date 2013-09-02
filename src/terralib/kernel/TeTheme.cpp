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

#include "TeTheme.h"
#include "TeDatabase.h"
#include "TeGroupingAlgorithms.h"
#include "TeDatabaseUtils.h"
#include "TeRasterTransform.h"
#include "TeQuerier.h"
#include "TeQuerierParams.h"

extern int  yyparse(string& sqlOut);
extern int  initParse(const string& strIn, TeDatabase* db);

static TeThemeFactory themeFactory;  

TeTheme::TeTheme( const string& name, TeLayer* layer, TeViewNode* parent, const int& view, const int& id)
		: TeAbstractTheme(name, parent, view, id, TeTHEME),
		layer_(layer)
{
	//layer id
	if(layer)
	{
		layerId_ = layer->id();
		if (layer_->database())
			themeBox_ = layer_->box();
	}
	else
		layerId_ = -1;
}

TeTheme::TeTheme(const TeViewNodeParams& params) : TeAbstractTheme(params),	layer_(0), layerId_(-1)
{ }


// Copy constructor
TeTheme::TeTheme (const TeTheme& other) : 
	TeAbstractTheme(other.viewNodeParams_) 
{
	layerId_ = other.layerId_;
	layer_ = other.layer_;      //the same layer pointer
	collectionTable_ = other.collectionTable_;
	collectionAuxTable_ = other.collectionAuxTable_;
	
	attTableVector_ = other.attTableVector_;
	sqlFrom_ =  other.sqlFrom_;		
	sqlJoin_ = other.sqlGridJoin_;
	sqlGridFrom_ = other.sqlGridFrom_;
	sqlGridJoin_ = other.sqlGridJoin_;
	aliasVector_ = other.aliasVector_;
	sqlAttList_ = other.sqlAttList_;
	sqlNumAttList_ = other.sqlNumAttList_;		
}


// Destructor
TeTheme::~TeTheme () 
{ 
	clearAttTableVector();
	clearAttList();
	collectionTable_.clear();
	collectionAuxTable_.clear();
	sqlFrom_.clear();
	sqlJoin_.clear();
	sqlGridFrom_.clear();
	sqlGridJoin_.clear();
	aliasVector_.clear();
	sqlAttList_.clear();
	sqlNumAttList_.clear();
}

TeTheme& 
TeTheme::operator= (const TeTheme& other)
{
	if ( this != &other )
	{
		TeAbstractTheme* absTheme = (TeAbstractTheme*)this;
		absTheme->operator=(other);
		layerId_ = other.layerId_;
		layer_ = other.layer_;      //the same layer pointer
		
		collectionTable_ = other.collectionTable_;
		collectionAuxTable_ = other.collectionAuxTable_;
		
		attTableVector_ = other.attTableVector_;
		sqlFrom_ =  other.sqlFrom_;		
		sqlJoin_ = other.sqlGridJoin_;
		sqlGridFrom_ = other.sqlGridFrom_;
		sqlGridJoin_ = other.sqlGridJoin_;
		aliasVector_ = other.aliasVector_;
		sqlAttList_ = other.sqlAttList_;
		sqlNumAttList_ = other.sqlNumAttList_;
	}
	return *this;
}


void TeTheme::layer(TeLayer* layer)
{
	layer_ = layer;
	if (layer)
	{
		layerId_ = layer->id();
//		themeBox_ = layer_->box();
	}
}

void 
TeTheme::setSpatialRest(TeBox& box, TeGeomRep rep, TeSpatialRelation relation)
{
	hasSpatialRes_ = true;
	boxRest_ = box;
	spatialRelation_ = relation;
	
	if(rep==TeGEOMETRYNONE)
		geomRepRest_ = layer()->vectRepres()[0]->geomRep_;
	else
		geomRepRest_ = rep;
	
	geomRest_ = 0;
}

void 
TeTheme::setSpatialRest(TeGeometry* geom, TeGeomRep rep, TeSpatialRelation relation)
{
	hasSpatialRes_ = true;
	geomRest_ = geom;
	spatialRelation_ = relation;
	
	if(rep==TeGEOMETRYNONE)
		geomRepRest_ = layer()->vectRepres()[0]->geomRep_;
	else
		geomRepRest_ = rep;
	
	boxRest_ = TeBox();
}

string 
TeTheme::sqlWhereRestrictions(TeRepresentation* rep)
{
	TeKeys objs;
	string whereClause= " 1 = 1 ";
	TeDatabase* db = layer()->database();
	
	// load the first representation 
	if(!rep)
		rep = (layer()->vectRepres())[0];

	// temporal restrictions are applied only to temporal tables
	if(hasTemporalRest())
	{
		string result = "";
		
		initParse(temporalRest(), db); 
				
		if(!yyparse(result))  //0: accept  1: reject
			whereClause += " AND "+ result;
		else 
			return "";
	}
			
	// we should test if the attribute restriction is valid
	if (hasAttrRest())
		whereClause += " AND "+ attributeRest();

	// spatial restriction with other geometry representation
	if (hasSpatialRest() && rep)
	{
		if(boxRestriction().isValid())
		{
            TeBox b =  boxRestriction();
            TeGeomRep gRep = rep->geomRep_;
//			whereClause += " AND "+ db->getSQLBoxWhere(boxRestriction(), rep->geomRep_);
			whereClause += " AND "+ db->getSQLBoxWhere(b, gRep);
		}
		else if(geomRestriction())
		{
			string geomTableRest = layer()->tableName(geomRepRestriction());
			TePrecision::instance().setPrecision(TeGetPrecision(layer()->projection()));

			if(db->spatialRelation(geomTableRest, geomRepRestriction(), geomRestriction(),  
							   objs, spatialRelation()))
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
		}
	}
	return whereClause;
}

void TeTheme::createRasterVisual(TeRaster* rst)
{
	if (rasterVisual_)
		delete rasterVisual_;

	if (!rst)
		rst = layer_->raster();

	if (!rst)
		return;

	rasterVisual_ = new TeRasterTransform();
										
	if (rst->params().photometric_[0] == TeRasterParams::TePallete)  // raster palette -> uses its palette
	{
		rasterVisual_->setTransfFunction(&TeRasterTransform::Pallete2ThreeBand);
		rasterVisual_->setLutSize(rst->params().lutr_.size());
		return;
	}

	if (visibleRep_ & 0x40000000  &&		// sliced raster -> generate the
		grouping_.groupMode_ == TeRasterSlicing)	// appropriate palette
	{
		int band = atoi(grouping_.groupAttribute_.name_.c_str());
		rasterVisual_->setSrcBand(band);
		if (rst->params().dataType_[band] != TeUNSIGNEDCHAR)
			rasterVisual_->generateLUT(legend_, 1024, defaultLegend_.visual(TePOLYGONS)->color());
		else
			rasterVisual_->generateLUT(legend_, 256, defaultLegend_.visual(TePOLYGONS)->color());
		rasterVisual_->setTransfFunction(&TeRasterTransform::LUT2ThreeBand);
		return;
	}

	if (rst->params().dataType_[0] != TeUNSIGNEDCHAR)	// non unsigned char -> generate linear transformation
		rasterVisual_->setLinearTransfParameters(rst->params().vmin_[0],rst->params().vmax_[0], 0, 255);

	if (rst->params().nBands() == 1)				
		rasterVisual_->setTransfFunction(&TeRasterTransform::Mono2ThreeBand);
	else if (rst->params().nBands() == 3)
		rasterVisual_->setTransfFunction(&TeRasterTransform::ThreeBand2RGB);
	else
		rasterVisual_->setTransfFunction(&TeRasterTransform::Band2Band);
}



bool
TeTheme::buildCollection(std::string objectId)
{
	if(id()==0)
		return false;
	
	if(!populateCollection(objectId))
		return false;

	if(!populateCollectionAux(objectId))
		return false;

	return true;
}

bool  
TeTheme::buildGrouping(const TeGrouping& g, TeSelectedObjects selectedObjects, 
						 vector<double>* dValuesVec)   
{
	return buildGrouping(layer_->database(), g, selectedObjects, dValuesVec);
}

bool  
TeTheme::buildGrouping(TeDatabase* db, const TeGrouping& g, TeSelectedObjects selectedObjects, vector<double>* dValuesVec)   
{ 
	if(!db)
		return false;
	grouping_ = g;
	unsigned int i;
	vector<TeSlice> slices;
	int	nullValues = 0;
	if (grouping_.groupMode_ == TeRasterSlicing)
	{
		int b = atoi(grouping_.groupAttribute_.name_.c_str());
		if (!layer_->raster() ||  
			b < 0 ||
			b > layer_->raster()->params().nBands() ||
			grouping_.groupNumSlices_ <= 0)
			return false;

		if (grouping_.groupMaxVal_ == TeMINFLOAT)
			grouping_.groupMaxVal_ = layer_->raster()->params().vmax_[b];

		if (grouping_.groupMinVal_ == TeMAXFLOAT)
			grouping_.groupMinVal_ = layer_->raster()->params().vmin_[b];

		TeGroupByEqualStep(grouping_.groupMinVal_, grouping_.groupMaxVal_,
			grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
	}
	else
	{
        if(grouping_.groupAttribute_.name_.empty())
			return false;

		//verify what the objects will be considered
		string	input;
		if(selectedObjects == TeSelectedByPointing)
		{
			input = " WHERE (grid_status = 1 OR grid_status = 3";
			input += " OR (grid_status is null AND (c_object_status = 1 OR c_object_status = 3)))";
		}
		else if(selectedObjects == TeNotSelectedByPointing)
		{
			input = " WHERE (grid_status = 0 OR grid_status = 2";
			input += " OR (grid_status is null AND (c_object_status = 0 OR c_object_status = 2)))";
		}
		else if(selectedObjects == TeSelectedByQuery)
		{
			input = " WHERE (grid_status = 2 OR grid_status = 3";
			input += " OR (grid_status is null AND (c_object_status = 2 OR c_object_status = 3)))";
		}
		else if(selectedObjects == TeNotSelectedByQuery)
		{
			input = " WHERE (grid_status = 0 OR grid_status = 1";
			input += " OR (grid_status is null AND (c_object_status = 0 OR c_object_status = 1)))";
		}
		else if(selectedObjects == TeGrouped)
		{
			input = " WHERE c_legend_id <> 0";
		}
		else if(selectedObjects == TeNotGrouped)
		{
			input = " WHERE c_legend_id = 0";
		}

		TeDatabasePortal* portal = db->getPortal();
		string query;
		bool normal = false;
		string aggrFunc = "";
		if(grouping_.groupFunction_.empty())
			aggrFunc = " MIN";
		else
			aggrFunc = grouping_.groupFunction_;

		if(grouping_.groupNormAttribute_.empty())
		{
			query = " SELECT "+ aggrFunc +"("+ grouping_.groupAttribute_.name_ +")";  
		}
		else
		{
			query = " SELECT "+ aggrFunc +"("+ grouping_.groupAttribute_.name_ +") / "+ aggrFunc +"("+ grouping_.groupNormAttribute_ + ")";
			normal = true;
		}
		query += sqlGridFrom(); 
		
		if(selectedObjects != TeAll)
			query += input;
		query += " GROUP BY " + collectionTable() + ".c_object_id";
		if(!portal->query(query) || !portal->fetchRow())
		{
			delete portal;
			return false;
		}
		vector<double> dValues;  //inputvect
		vector<string> sValues;	//svec
		double mean, sum; 
		mean = sum = 0.;
		do  
		{
			string val = portal->getData(0);
			string valNorm = Te2String(atof(val.c_str()), grouping_.groupPrecision_);

			if (!val.empty())
			{
				if(grouping_.groupMode_== TeUniqueValue)
				{
					if(normal)
						sValues.push_back(valNorm);
					else
						sValues.push_back(val);
				}
				else
				{
					dValues.push_back(atof(valNorm.c_str()));
					sum += atof(valNorm.c_str());
				}

			}
			else
				nullValues++;
		}while(portal->fetchRow());
		
		delete portal;
		
		if(dValues.empty() && sValues.empty())
			return false;

		if(grouping_.groupMode_== TeEqualSteps)
			TeGroupByEqualStep(dValues.begin(), dValues.end(), grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
		else if(grouping_.groupMode_== TeQuantil)
			TeGroupByQuantil(dValues.begin(), dValues.end(), grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
		else if(grouping_.groupMode_== TeStdDeviation)
		{
			string m = Te2String(mean);
			TeGroupByStdDev(dValues.begin(), dValues.end(), grouping_.groupStdDev_, slices, m, grouping_.groupPrecision_);
		}
		else if(grouping_.groupMode_== TeUniqueValue)
		{
			if(grouping_.groupFunction_ == "COUNT")
				TeGroupByUniqueValue(sValues, TeINT, slices, grouping_.groupPrecision_);
			else
				TeGroupByUniqueValue(sValues, grouping_.groupAttribute_.type_, slices, grouping_.groupPrecision_);
		}

		if (dValuesVec)
		{
			for (i = 0; i < dValues.size(); ++i)
				dValuesVec->push_back(dValues[i]);
		}
	}
	if(grouping_.groupNullAttr_ && nullValues > 0)
	{
		TeSlice ps;
		ps.count_ = nullValues;
		ps.from_ = "Missing Data";
		slices.push_back(ps);
		grouping_.groupNumSlices_ = slices.size() - 1;
	}
	else
		grouping_.groupNumSlices_ = slices.size();

	legend_.clear(); 
	for(i=0; i<slices.size(); ++i)
	{
		TeLegendEntry legend(slices[i]);
		legend.group(i);
		legend.theme(id());
		legend_.push_back(legend);
	}
	return true;
}

bool  
TeTheme::buildGrouping(const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec)   
{ 
	return buildGrouping(layer_->database(), g, chr, mapObjValVec);   
}

bool  
TeTheme::buildGrouping(TeDatabase* db, const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec)   
{ 
	if(!db || chr == TeNOCHRONON)
		return false;

	grouping_ = g;

	unsigned int i;
	vector<TeSlice> slices;
	vector<double> dValues;  //inputvect
	vector<string> sValues;	//svec
	double mean, sum; 
	mean = sum = 0.;
	int	nullValues = 0;
	string val;

	if (grouping_.groupMode_ == TeRasterSlicing)
	{
		int b = atoi(grouping_.groupAttribute_.name_.c_str());
		if (!layer_->raster() ||  
			b < 0 ||
			b > layer_->raster()->params().nBands() ||
			grouping_.groupNumSlices_ <= 0)
			return false;

		if (grouping_.groupMaxVal_ == TeMINFLOAT)
			grouping_.groupMaxVal_ = layer_->raster()->params().vmax_[b];

		if (grouping_.groupMinVal_ == TeMAXFLOAT)
			grouping_.groupMinVal_ = layer_->raster()->params().vmin_[b];

		TeGroupByEqualStep(grouping_.groupMinVal_, grouping_.groupMaxVal_,
			grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
	}
	else
	{
		string func;
		TeStatisticType statType = TeNOSTATISTIC;
		if (grouping_.groupMode_ == TeUniqueValue && grouping_.groupAttribute_.type_ == TeSTRING)
			func = "MIN";
		else
			func = grouping_.groupFunction_;

		if (func == "MIN")
			statType = TeMINVALUE;
		else if (func == "MAX")
			statType = TeMAXVALUE;
		else if (func == "MEAN")
			statType = TeMEAN;
		else if (func == "SUM")
			statType = TeSUM;
		else if (func == "COUNT")
			statType = TeCOUNT;

		// Set the flag that indicates the geometries must not be loaded
		bool loadGeometries = false;

		// Insert the attributes in a multimap that relates the attribute 
		// representation and its statistic type
		TeGroupingAttr attrMMap;
		pair<TeAttributeRep, TeStatisticType> attr1 (
			TeAttributeRep(grouping_.groupAttribute_), statType);
		attrMMap.push_back(attr1);

		// Set querier parameters
		TeQuerierParams querierParams(loadGeometries, attrMMap);
		querierParams.setParams(this, chr);

		TeQuerier querier(querierParams);

		// Load instances based on the querier parameters given
		int numFrames = querier.getNumTimeFrames();
		TeSTInstance sti;
		string objId;
		TePropertyVector vec;
		mapObjValVec.resize(numFrames);

		for (int frame = 0; frame < numFrames; ++frame)
		{
			if (querier.loadInstances(frame) == false)
				continue;

			// Traverse all the instances
			while(querier.fetchInstance(sti))
			{
				objId = sti.objectId();
				sti.getPropertyValue(val, 0);

				dValues.push_back(atof(val.c_str()));
				sValues.push_back(val);
				map<string, string>& objValMap = mapObjValVec[frame];
				objValMap.insert(make_pair(objId, val));
			}
		}

		if(grouping_.groupMode_== TeEqualSteps)
			TeGroupByEqualStep(dValues.begin(), dValues.end(), grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
		else if(grouping_.groupMode_== TeQuantil)
			TeGroupByQuantil(dValues.begin(), dValues.end(), grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
		else if(grouping_.groupMode_== TeStdDeviation)
		{
			string m = Te2String(mean);
			TeGroupByStdDev(dValues.begin(), dValues.end(), grouping_.groupStdDev_, slices, m, grouping_.groupPrecision_);
		}
		else if(grouping_.groupMode_== TeUniqueValue)
		{
			if(grouping_.groupFunction_ == "COUNT")
				TeGroupByUniqueValue(sValues, TeINT, slices, grouping_.groupPrecision_);
			else
				TeGroupByUniqueValue(sValues, grouping_.groupAttribute_.type_, slices, grouping_.groupPrecision_);
		}
	}

	if(grouping_.groupNullAttr_ && nullValues > 0)
	{
		TeSlice ps;
		ps.count_ = nullValues;
		ps.from_ = "Missing Data";
		slices.push_back(ps);
		grouping_.groupNumSlices_ = slices.size() - 1;
	}
	else
		grouping_.groupNumSlices_ = slices.size();

	legend_.clear(); 
	for(i=0; i<slices.size(); ++i)
	{
		TeLegendEntry legend(slices[i]);
		legend.group(i);
		legend.theme(id());
		legend_.push_back(legend);
	}
	
	return true;
}

bool 
TeTheme::getAttTables(TeAttrTableVector& attrs, TeAttrTableType attType)
{
	TeAttrTableVector::iterator it = attTableVector_.begin();
	while (it != attTableVector_.end())
	{
		if ((attType == TeAllAttrTypes) || ((*it).tableType() == attType))
			attrs.push_back((*it));
		++it;
	}
	return (!attrs.empty());
}

bool 
TeTheme::getTable(TeTable& table, const string tableName)
{
	unsigned int i;
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].name() == tableName)
		{
			table = attTableVector_[i];
			return true;
		}
	}
	return false;
}

bool 
TeTheme::getTemporalTable(TeTable& table)
{
	TeAttrTableVector::iterator it = attTableVector_.begin();
	while (it != attTableVector_.end())
	{
		if (((*it).tableType() == TeAttrEvent) || ((*it).tableType() == TeFixedGeomDynAttr))
		{
			table = (*it); 
			return true;
		}
		++it;
	}
	return false;
}

bool
TeTheme::saveGrouping(TeSelectedObjects selectedObjects)
{ 
	return saveGrouping(layer_->database(), selectedObjects);
}

bool
TeTheme::saveGrouping(TeDatabase* db, TeSelectedObjects)
{ 
	if(!db)
		return false;

	vector<TeLegendEntry> legVec = legend_;

	// Delete the theme grouping(if any) from the database
	if(db->deleteLegend(id()) == false)
		return false;

	if(grouping_.groupMode_ == TeNoGrouping)
		return true;

	// Insert the new grouping
	if(db->insertGrouping(id(), grouping_) == false)
		return false;
	
	legend_ = legVec;
	// Update (insert) the new legends
	if (db->updateLegend(legend_) == false)
		return false;

	setLegendsForObjects();

	if(legend_.size())
		visibleRep_ = visibleRep_ | 0x40000000;
	else
		visibleRep_ = visibleRep_ | 0xbfffffff;
	string upVis = "UPDATE te_theme SET visible_rep=" + Te2String(visibleRep_) + " WHERE theme_id=" + Te2String(id());
	return (db->execute(upVis));

	return true;
}

bool 
TeTheme::loadThemeTables() 
{
	clearAttTableVector();
	TeDatabase* db = this->layer()->database(); 
	if(!db)
		return false;
	return (db->loadThemeTable(this));
}

bool 
TeTheme::addThemeTable(TeTable& inputTable)  
{
	bool result = true;
	TeAttrTableType type = inputTable.tableType();

	if(inputTable.name() == collectionAuxTable_)
	{
		loadTablesJoin();
		return false;
	}
	
	if (type == TeAttrMedia)
		return false; 

	if(type != TeAttrStatic)
	{
		bool hasTemporal = false;
		bool hasExtern = false; 

		TeAttrTableVector::iterator it = attTableVector_.begin();
		while(it!=attTableVector_.end())
		{
			//temporal
			if( (it->tableType()==TeAttrEvent) || 
				(it->tableType()==TeFixedGeomDynAttr) ||
				(it->tableType()==TeDynGeomDynAttr))
				hasTemporal = true;

			//extern
			if((it->tableType())==TeAttrExternal)
				hasExtern = true;

			++it;
		}

		if( ((type==TeAttrEvent) || 
			(type==TeFixedGeomDynAttr) ||
			(type==TeDynGeomDynAttr)) && (hasTemporal || hasExtern))
			result = false;
		else if ((type==TeAttrExternal) && hasTemporal)
			result = false;
	}

	if(!result)
		return false;

	attTableVector_.push_back(inputTable);
	loadAliasVector();
	loadAttrLists();
	loadTablesJoin(); 
	return true;
}

void 
TeTheme::addThemeTable(string tableName)
{
	TeTable table(tableName);
	addThemeTable(table);
}

bool
TeTheme::setAttTables(TeAttrTableVector& attrs)
{	
	attTableVector_.clear();	
	int countTemporal = 0;
	int countExtern = 0;
	bool result = true;

	TeAttrTableVector::iterator it = attrs.begin();
	while(it!=attrs.end())
	{
		//temporal
		if( (it->tableType()==TeAttrEvent) || 
			(it->tableType()==TeFixedGeomDynAttr) ||
			(it->tableType()==TeDynGeomDynAttr))
			++countTemporal;

		//extern
		if(it->tableType()==TeAttrExternal)
			++countExtern;

		if( (it->tableType()==TeAttrMedia)     ||
			(countTemporal>0 && countExtern>0) ||
			(countTemporal>1) ||
			(it->name() == collectionAuxTable_) )
		{
			result = false;
		}
		else
		{
			attTableVector_.push_back(*it);
			loadAliasVector();
			loadAttrLists();
			loadTablesJoin(); 
		}
		++it;
	}
	return result;
}

bool 
TeTheme::removeThemeTable(unsigned int index)  
{
	if (index > (attTableVector_.size() - 1))
		return false;

	TeAttrTableVector::iterator it;
	it = attTableVector_.begin() + index;

	attTableVector_.erase(it);
	loadAliasVector();
	loadAttrLists();
	loadTablesJoin();
	return true; 
}

string
TeTheme::getAttribute(unsigned int pos)
{
	unsigned int i;
	unsigned int sumCols = 0, tableCol;
	bool found = false;
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		TeAttributeList& attrList = attTableVector_[i].attributeList();
		if (pos < (sumCols + attrList.size()))
		{
			if (i == 0)
				tableCol = pos;
			else
				tableCol = pos - sumCols;
			found = true;
			break;
		}
		sumCols += attrList.size();
	}

	if (found == false)
		return "";

	TeAttributeList& attrList = attTableVector_[i].attributeList();
	string attrName = attTableVector_[i].name() + "." + attrList[tableCol].rep_.name_;
	return attrName;
}

bool
TeTheme::isIndex(unsigned int pos)
{
	string fullAttrName = getAttribute(pos);
	if (fullAttrName.empty() == true)
		return false;

	size_t idx = fullAttrName.find(".");
	string tableName = fullAttrName.substr(0, idx);
	string attrName = fullAttrName.substr(idx+1);

	// Get the representation of a table given an attribute name
	TeTable table;
	if (getTable(table, tableName) == false)
		return false;

	// Check if the attribute is index or not
	if(table.uniqueName() == attrName)
		return true;

	return false;
}

bool 
TeTheme::isThemeTable(int tableId)  
{
	bool isThemeTable = false;
	unsigned int i;
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].id() == tableId)
		{
			isThemeTable = true;
			break;
		}
	}
	return isThemeTable;
}

bool 
TeTheme::isThemeTable(string tableName)  
{
	bool isThemeTable = false;
	for (unsigned int i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].name() == tableName)
		{
			isThemeTable = true;
			break;
		}
	}
	return isThemeTable;
}

string
TeTheme::getTableName(const string& attrName)
{
	string tableName;
	size_t pos = attrName.find(".");

	if (pos != string::npos)
		return tableName = attrName.substr(0, pos);

	for (unsigned int i = 0; i < attTableVector_.size(); ++i)
	{
		TeAttributeList& attrList = attTableVector_[i].attributeList();
		for (unsigned j = 0; j < attrList.size(); ++j)
		{
			if (attrList[j].rep_.name_ == attrName)
			{
				tableName = attTableVector_[i].name();
				return tableName;
			}
		}	
	}

	return tableName;
}


bool 
TeTheme::deleteGrouping()  
{
	TeDatabase* db = layer_->database();
	if(!db)
		return false;

	resetGrouping();
	
	//delete te_legend table
	if(!db->deleteLegend (this->id()))
		return false;
	
	//delete te_grouping table
	string sql = "DELETE FROM te_grouping WHERE theme_id= "+ Te2String(this->id());
	db->execute (sql);

	return true;
}

bool 
TeTheme::createCollectionAuxTable()
{
	return createCollectionAuxTable(layer_->database());
}

bool 
TeTheme::createCollectionAuxTable(TeDatabase* db)   
{
	unsigned int i, j;
	bool status;
	vector<string> indexes;
	
	if(!db)
		return false;

	if(!db->tableExist(collectionTable_))
		return false;

	if(db->tableExist(collectionAuxTable_))
	{
		status = db->execute("DROP TABLE " + collectionAuxTable_);
		if(db->tableExist(collectionAuxTable_))
		{
			if(!status)
				return false; 
		}
	}

	TeAttributeList attList;

	TeAttribute at;
	at.rep_.type_ = TeSTRING;
	at.rep_.numChar_ = 50;
	at.rep_.name_ = "object_id";
	attList.push_back(at);

	j = 0;
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].tableType() == TeAttrExternal ||
			attTableVector_[i].tableType() == TeFixedGeomDynAttr)
		{
			at.rep_.name_ = "aux" + Te2String(j++);
			attList.push_back(at);
			indexes.push_back(at.rep_.name_);
		}
	}

	at.rep_.isPrimaryKey_ = true;
	at.rep_.numChar_ = 0;
	at.rep_.name_ = "unique_id";
	at.rep_.isAutoNumber_ = true;
	at.rep_.type_ = TeINT;
	attList.push_back(at);
	
	at.rep_.name_ = "grid_status";
	at.rep_.isPrimaryKey_ = false;
	at.rep_.isAutoNumber_ = false;
	at.rep_.numChar_ = 0;
	attList.push_back(at);	
	
	status = db->createTable(collectionAuxTable_,attList);
	if(!status)
		return false;

	string idxName = "te_idx_caux"+ Te2String(this->id());

	//create index to object_id
	db->createIndex(collectionAuxTable_, idxName+"_objId", "object_id"); 

	for(unsigned int i=0; i<indexes.size(); ++i)
		db->createIndex(collectionAuxTable_, idxName+"_"+indexes[i], indexes[i]); 

	return true;
} 

bool 
TeTheme::populateCollectionAux(std::string objectId)  
{	
	TeDatabase* db = layer_->database();
	if((!db) || (collectionTable_.empty()) || (collectionAuxTable_.empty()))
		return false;
	
	string whereClause; 

	whereClause = " WHERE 1=1 ";
	
	// Populate the collection auxiliary table
	string ins = "INSERT INTO " + collectionAuxTable_ + " (object_id";
    unsigned int i, j;
	j = 0;
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].tableType() == TeAttrExternal ||
			attTableVector_[i].tableType() == TeFixedGeomDynAttr)
			ins += ", aux" + Te2String(j++);
	}

	ins += ", grid_status) SELECT c_object_id";
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].tableType() == TeAttrExternal ||
			attTableVector_[i].tableType() == TeFixedGeomDynAttr)
			ins += "," + aliasVector()[i] + "." + attTableVector_[i].uniqueName();
	}

	ins += ",c_object_status";

	//attribute restriction
	if(!generateAttributeRest_.empty())
		whereClause += " AND "+ generateAttributeRest_; 

	//temporal restriction
	if(!generateTemporalRest_.empty())
	{
		string sqlTemp;
		initParse(generateTemporalRest_, db); 
				
		if(!yyparse(sqlTemp))  //0: accept  1: reject
			whereClause += " AND "+ sqlTemp;
		else
			return false;
	}

	//spatial restriction is already stored in the collection table
	
	bool usaTemporal = false;
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		if (attTableVector_[i].tableType() == TeFixedGeomDynAttr ||
			attTableVector_[i].tableType() == TeDynGeomDynAttr)
			usaTemporal = true;
	}
	loadThemeTablesJoin();
	string result = ins + sqlFrom() + whereClause + " ";
	if (!objectId.empty() && !whereClause.empty())
		result += " AND c_object_id = '"+objectId+"'";
	if (!db->execute(result))
		return false;

	if(usaTemporal) // filter collection table
	{
		string s = "DELETE FROM " + collectionTable_ + " WHERE c_object_id NOT IN";
		s += " (SELECT DISTINCT object_id FROM " + collectionAuxTable_ + ")";
		return(db->execute(s));
	}
	return true;
}


bool 
TeTheme::locatePolygon (TeCoord2D &pt, TePolygon &polygon, const double& tol)  
{
	if (!layer()->database() || collectionTable().empty())
		return false;

	string geomTable = layer()->tableName(TePOLYGONS);
	string sqlFrom = " "+ geomTable +" INNER JOIN " + collectionTable();
	sqlFrom += " ON "+ collectionTable() +".c_object_id = "+ geomTable +".object_id "; 

	if(!layer()->database()->locatePolygon(sqlFrom, pt, polygon, tol))
		return false;

	return true;
}


bool 
TeTheme::locatePolygonSet (TeCoord2D &pt, double tol, TePolygonSet &polygons)  
{
	if (!layer()->database() || collectionTable().empty())
		return false;

	string geomTable = layer()->tableName(TePOLYGONS);
	string sqlFrom = " "+ geomTable +" INNER JOIN " + collectionTable();
	sqlFrom += " ON "+ collectionTable() +".c_object_id = "+ geomTable +".object_id "; 

	if(!layer()->database()->locatePolygonSet(sqlFrom, pt, tol, polygons))
		return false;

	return true;
}

bool 
TeTheme::locateLine (TeCoord2D &pt, TeLine2D &line, const double& tol)  
{
	if (!layer()->database() || collectionTable().empty())
		return false;

	string geomTable = layer()->tableName(TeLINES);
	string sqlFrom = " "+ geomTable +" INNER JOIN " + collectionTable();
	sqlFrom += " ON "+ collectionTable() +".c_object_id = "+ geomTable +".object_id "; 

	if(!layer()->database()->locateLine(sqlFrom, pt, line, tol))
		return false;

	return true;
}

bool 
TeTheme::locatePoint (TeCoord2D &pt, TePoint &point, const double& tol)  
{
	if (!layer()->database() || collectionTable().empty())
		return false;

	string geomTable = layer()->tableName(TePOINTS);
	string sqlFrom = " "+ geomTable +" INNER JOIN " + collectionTable();
	sqlFrom += " ON "+ collectionTable() +".c_object_id = "+ geomTable +".object_id "; 

	if(!layer()->database()->locatePoint(sqlFrom, pt, point, tol))
		return false;

	return true;
}

bool 
TeTheme::locateCell (TeCoord2D &pt, TeCell &cell, const double&  tol)  
{
	if (!layer()->database() || collectionTable().empty())
		return false;

	string geomTable = layer()->tableName(TeCELLS);
	string sqlFrom = " "+ geomTable +" INNER JOIN " + collectionTable();
	sqlFrom += " ON "+ collectionTable() +".c_object_id = "+ geomTable +".object_id "; 

	if(!layer()->database()->locateCell(sqlFrom, pt, cell, tol))
		return false;

	return true;
}

//------------------------------ protected methods
void 
TeTheme::loadAliasVector()  
{
	unsigned int i, count;
	TeTable table;
	multimap<string, int> tableMMap;

	aliasVector_.clear();

	for (i = 0; i < attTableVector_.size(); ++i)
	{
		table = attTableVector_[i];

		if (table.tableType() != TeAttrExternal)
			aliasVector_.push_back(table.name());
		else
		{
			count = tableMMap.count(table.name());
			if (count == 0)
				aliasVector_.push_back(table.name());
			else
				aliasVector_.push_back(table.name() + "_" + Te2String(count));

			tableMMap.insert(multimap<string,int>::value_type(table.name(), ++count));
		}
	}
}

string 
TeTheme::sqlGridFrom(const string& geomTable) 
{ 
	if(geomTable.empty())
		return sqlGridFrom_; 
	
	string result;
	loadTablesJoin(geomTable);
	result = sqlGridFrom_;
	loadTablesJoin();
	return result;
}

bool 
TeTheme::generateLabelPositions(const std::string& objectId)
{ 
	TeDatabase* db = layer()->database();
	if(!db)
		return false;

	return (db->generateLabelPositions(this, objectId)); 
}

void 
TeTheme::loadTablesJoin(const string& geomTable)  //sqlGridFrom and sqlGridJoin
{
	unsigned int i, count;
	multimap<string, int> tableMMap;
    TeTable table;
    bool hasExternalTable = false;

    // Set the new sqlGridFrom_ clause and the new sqlGridJoin_ string
    sqlGridFrom_.clear();
    sqlGridJoin_.clear();

    TeAttrTableVector tableVec;
    vector<string>    aliasVec;

    if(!geomTable.empty())
    {
        TeTable table;
        table.name(geomTable);
        table.setLinkName("object_id");
        table.setUniqueName("object_id");
        table.setTableType(TeAttrStatic);
        tableVec.push_back(table);

        for(i=0; i<attTableVector_.size(); ++i)
            tableVec.push_back(attTableVector_[i]);

        aliasVec.push_back(geomTable);
        for(i=0; i<aliasVector_.size(); ++i)
            aliasVec.push_back(aliasVector_[i]);
    }
    else
    {
        tableVec = attTableVector_;
        aliasVec = aliasVector_;
    }

    //verify if there is external table
    for (i = 0; i < tableVec.size(); ++i)
    {
        if (tableVec[i].tableType() == TeAttrExternal)
        {
            hasExternalTable = true;
            break;
        }
    }
   
    if(!collectionAuxTable_.empty())
    {
        sqlGridFrom_ = " FROM ";

        for (i = 0; i <= tableVec.size(); ++i)
            sqlGridFrom_ += "(";

        sqlGridFrom_ += collectionAuxTable_;
        sqlGridJoin_ = "SELECT ";

        if(hasExternalTable)
            sqlGridFrom_ += " RIGHT JOIN "+ collectionTable_;
        else
            sqlGridFrom_ += " LEFT JOIN "+ collectionTable_;

        sqlGridFrom_ += " ON "+ collectionAuxTable_ +".object_id = "+ collectionTable_ +".c_object_id )";

        int numAux = 0;
        for (i = 0; i < tableVec.size(); ++i)
        {
            table = tableVec[i];
            if ((table.tableType()==TeAttrStatic) || (table.tableType()==TeAttrMedia) || (table.tableType()==TeAttrEvent))
            {
                sqlGridFrom_ += " LEFT JOIN " + aliasVec[i];
                sqlGridFrom_ += " ON " + collectionTable_ +".c_object_id = " + aliasVec[i] + "." + table.linkName() +")";
            }
            else if (table.tableType() == TeAttrExternal)
            {
				count = tableMMap.count(table.name());
				if (count == 0)
					sqlGridFrom_ += " LEFT JOIN " + aliasVec[i];		
				else
					sqlGridFrom_ += " LEFT JOIN " + table.name() + " AS " + aliasVec[i];

				tableMMap.insert(multimap<string,int>::value_type(table.name(), ++count));
                sqlGridFrom_ += " ON " + collectionAuxTable_ + ".aux" + Te2String(numAux++) +" = "+ aliasVec[i] +"."+ table.uniqueName() +")";
            }
            else
            {
                sqlGridFrom_ += " LEFT JOIN " + aliasVec[i];
                sqlGridFrom_ += " ON " + collectionAuxTable_ + ".aux" + Te2String(numAux++) +" = "+ aliasVec[i] +"."+ table.uniqueName() +")";
            }
           
            sqlGridJoin_ += aliasVec[i] + ".*,";
        }

        sqlGridJoin_ += collectionTable_ + ".*, " +  collectionAuxTable_ + ".* " + sqlGridFrom_;
    }

	loadThemeTablesJoin();
 }

void TeTheme::loadThemeTablesJoin()  ////sqlJoin and sqlFrom
{
	unsigned int i, count;
	multimap<string, int> tableMMap;
	TeTable table;

	// Set the new from clause and the new join string
	sqlFrom_.clear();
	sqlJoin_.clear();

	sqlFrom_ = " FROM ";
	sqlJoin_ = "SELECT ";

	if (collectionTable_.empty() == false)
	{
		for (i = 0; i < attTableVector_.size(); ++i)
			sqlFrom_ += "(";
		sqlFrom_ += collectionTable_;

		for (i = 0; i < attTableVector_.size(); ++i)
		{
			table = attTableVector_[i];
			if (table.tableType() != TeAttrExternal)
			{
				sqlFrom_ += " LEFT JOIN " + aliasVector_[i];
				sqlFrom_ += " ON " + collectionTable_ + ".c_object_id = " + aliasVector_[i] + "." + table.linkName() + ")";
			}
			else
			{
				count = tableMMap.count(table.name());
				if (count == 0)
					sqlFrom_ += " LEFT JOIN " + aliasVector_[i];		
				else
					sqlFrom_ += " LEFT JOIN " + table.name() + " AS " + aliasVector_[i];

				tableMMap.insert(multimap<string,int>::value_type(table.name(), ++count));
				
				sqlFrom_ += " ON " + table.relatedTableName() + "." + table.relatedAttribute() + " = ";
				sqlFrom_ +=  aliasVector_[i] + "." + table.linkName() + ")";
			}

			sqlJoin_ += aliasVector_[i] + ".*,";
		}

		sqlJoin_ += collectionTable_ + ".*" + sqlFrom_;
	}
	else
	{
		if (attTableVector_.size() == 1)
		{
			table = attTableVector_[0];
			sqlFrom_ += table.name();
			sqlJoin_ = "SELECT " + table.name() + ".*" + sqlFrom_;
		}
		else
		{
			for (i = 0; i < attTableVector_.size() - 1; ++i)
				sqlFrom_ += "(";

			TeTable firstTable = attTableVector_[0];
			sqlFrom_ += firstTable.name();
			sqlJoin_ += firstTable.name() + ".*,";
			for (i = 1; i < attTableVector_.size(); ++i)
			{
				table = attTableVector_[i];
				sqlFrom_ += " LEFT JOIN " + aliasVector_[i];
				sqlFrom_ += " ON " + firstTable.name() + "." + firstTable.linkName() + " = " + aliasVector_[i] + "." + table.linkName() + ")";

				if (i == attTableVector_.size() - 1)
					sqlJoin_ += aliasVector_[i] + ".*";
				else
					sqlJoin_ += aliasVector_[i] + ".*,";
			}

			sqlJoin_ += sqlFrom_;
		}
	}
	return;
}

void 
TeTheme::loadAttrLists()  
{
	// Set the new list of attributes of all the theme tables and its new numerical list,
	unsigned int i, j, count;
	TeTable table;
	multimap<string, int> attrMMap;
	string attrName;

	sqlAttList_.clear();
	sqlNumAttList_.clear();

	if (layer() == 0)
		return;
	TeDatabase *db = layer()->database();

	for (i = 0; i < attTableVector_.size(); ++i)
	{
		table = attTableVector_[i];
		// Set the map of attribute names
		if(table.attributeList().empty())
			db->getAttributeList(table.name(), table.attributeList());

		for (j = 0; j < table.attributeList().size(); ++j)
		{
			attrName = table.attributeList()[j].rep_.name_; 
			count = attrMMap.count(attrName);
			attrMMap.insert(multimap<string,int>::value_type(attrName, ++count));
		}
	}

	// Set the list of attribute names that contains all the attribute names
	// of the theme tables
	for (i = 0; i < attTableVector_.size(); ++i)
	{
		table = attTableVector_[i];
		TeAttributeList attrList=table.attributeList();
		if(attrList.empty())
			db->getAttributeList(table.name(), attrList);
		for (j = 0; j < attrList.size(); ++j)
		{
			attrName = attrList[j].rep_.name_; 
			count = attrMMap.count(attrName);
			if (count == 1)
				attrList[j].rep_.name_ = attrName;
			else
				attrList[j].rep_.name_ = aliasVector_[i] + "." + attrName;

			sqlAttList_.push_back(attrList[j]);
		}		
	}

	// Set the list of attribute names that contains all the numeric attribute names
	// of the theme tables
	for(i = 0; i < sqlAttList_.size(); ++i)
	{
		if(sqlAttList_[i].rep_.type_ == TeREAL || sqlAttList_[i].rep_.type_ == TeINT)
			sqlNumAttList_.push_back(sqlAttList_[i]);
	}
}

void
TeTheme::setLegendsForObjects()
{
	if(grouping_.groupMode_ == TeNoGrouping || attTableVector_.empty())
		return;

	unsigned int i, j;
	string func, query, oid, val;

	TeDatabase *db = layer_->database();
	TeDatabasePortal* portal = db->getPortal();
 
	TeAttrDataType type = grouping_.groupAttribute_.type_;
	string	groupingAttr = grouping_.groupAttribute_.name_;
	
	vector<string> oidVec;
	vector<string> oidWithNullValVec;
	vector<string> valVec;
	map<int, set<string> > legObjSetMap;

	objLegendMap_.clear();

	if(grouping_.groupFunction_.empty())
		func = "MIN";
	else
		func = grouping_.groupFunction_;

	if(grouping_.groupFunction_ == "COUNT")
		type = TeINT;

	query = "SELECT MIN(" + attTableVector_[0].uniqueName() + ")";
	query += ", " + func + "(" + groupingAttr + ")" + sqlFrom_;
	query += " GROUP BY " + attTableVector_[0].uniqueName();

	if(portal->query(query) == false)
	{
		delete portal;
		return;
	}

	while(portal->fetchRow())
	{
		oid = portal->getData(0);
		val = portal->getData(1);
		if (val.empty() == false)
		{
			oidVec.push_back(oid);
			valVec.push_back(val);
		}
		else
			oidWithNullValVec.push_back(oid);
	}

	unsigned int legSize = legend_.size();
	if(grouping_.groupNullAttr_ && oidWithNullValVec.size() > 0)
		--legSize;

	if (grouping_.groupMode_ == TeUniqueValue)
	{
		for (i = 0; i < oidVec.size(); ++i)
		{
			oid = oidVec[i];
			val = valVec[i];

			if(type == TeREAL)
				val = Te2String(atof(val.c_str()), grouping_.groupPrecision_);
			else if(type == TeINT)
				val = Te2String(atoi(val.c_str()));

			for(j = 0; j < legSize; ++j)
			{
				TeLegendEntry& leg = legend_[j];
				if(val == leg.from())
				{
					objLegendMap_[oid] = leg.id();
					set<string>& oidSet = legObjSetMap[leg.id()];
					oidSet.insert(oid);
					break;
				}
			}
		}		
	}
	else
	{
		for (i = 0; i < oidVec.size(); ++i)
		{
			double dVal, dFrom, dTo;
			oid = oidVec[i];
			val = valVec[i];

			if (type == TeREAL)
				val = Te2String(atof(val.c_str()), grouping_.groupPrecision_);
			
			for (j = 0; j < legSize; ++j)
			{
				TeLegendEntry& leg = legend_[j];
				if (leg.from().find("mean") != string::npos)
					continue;

				dVal  = atof(val.c_str());
				dFrom = atof(leg.from().c_str());
				dTo = atof(leg.to().c_str());

				if (j < (legSize - 1))
				{
					if(dVal >= dFrom && dVal < dTo)
					{
						objLegendMap_[oid] = leg.id();
						set<string>& oidSet = legObjSetMap[leg.id()];
						oidSet.insert(oid);
						break;
					}
				}
				else
				{
					if(dVal >= dFrom && dVal <= dTo)
					{
						objLegendMap_[oid] = leg.id();
						set<string>& oidSet = legObjSetMap[leg.id()];
						oidSet.insert(oid);
						break;
					}
				}
			}
		}
	}
	delete portal;

	// Set the leg id for the objects with null values
	int legId = defaultLegend_.id();
	if (grouping_.groupNullAttr_)
		legId = legend_[legend_.size() - 1].id();	

	for (i = 0; i < oidWithNullValVec.size(); ++i)
	{
		oid = oidWithNullValVec[i];
		objLegendMap_[oid] = legId;
		set<string>& oidSet = legObjSetMap[legId];
		oidSet.insert(oid);
	}

/*
	// Insert the legends in the te_legend table
	map<int, set<string> >::iterator mapIt;
	set<string>::iterator setIt;
	unsigned char *data;
	string where = " theme_id = " + Te2String(id());
	for (mapIt = legObjSetMap.begin(); mapIt != legObjSetMap.end(); ++mapIt)
	{
		legId = mapIt->first;
		set<string>& oidSet = mapIt->second;
		string whereClause = where;
		whereClause +=  " AND legend_id = " + Te2String(legId);
		string objStr;
		for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
			objStr += *setIt + ';';
		data = (unsigned char*)objStr.c_str();
		db->insertBlob("te_legend", "object_list", whereClause, data, objStr.size());
	}
*/
}

void
TeTheme::setOwnLegendsForObjects()
{

}

bool
TeTheme::saveLegendInCollection(TeSelectedObjects selectedObjects, std::string objectId) 
{
	return saveLegendInCollection(layer()->database(), selectedObjects, objectId); 
}

bool
TeTheme::saveLegendInCollection(TeDatabase* db, TeSelectedObjects selectedObjects, std::string objectId) 
{
	unsigned int i;
	if(!db || grouping_.groupMode_ == TeNoGrouping)
		return false; 

	TeAttrDataType		type = grouping_.groupAttribute_.type_;
	TeLegendEntryVector legVec = legend_;
	string			groupingAttr = grouping_.groupAttribute_.name_;
	
	string input;
	if(selectedObjects == TeSelectedByPointing)
	{
		input = " WHERE (grid_status = 1 OR grid_status = 3";
		input += " OR (grid_status IS NULL AND (c_object_status = 1 OR c_object_status = 3)))";
	}
	else if(selectedObjects == TeNotSelectedByPointing)
	{
		input = " WHERE (grid_status = 0 OR grid_status = 2";
		input += " OR (grid_status is null AND (c_object_status = 0 OR c_object_status = 2)))";
	}
	else if(selectedObjects == TeSelectedByQuery)
	{
		input = " WHERE (grid_status = 2 OR grid_status = 3";
		input += " OR (grid_status is null AND (c_object_status = 2 OR c_object_status = 3)))";
	}
	else if(selectedObjects == TeNotSelectedByQuery)
	{
		input = " WHERE (grid_status = 0 OR grid_status = 1";
		input += " OR (grid_status is null AND (c_object_status = 0 OR c_object_status = 1)))";
	}
	else if(selectedObjects == TeGrouped)
		input = " WHERE c_legend_id <> 0";
	else if(selectedObjects == TeNotGrouped)
		input = " WHERE c_legend_id = 0";
	

	string func;
	if(grouping_.groupFunction_.empty())
		func = " MIN";
	else
		func = grouping_.groupFunction_;

	if(grouping_.groupFunction_ == "COUNT")
		type = TeINT;

	string query = "SELECT MIN(" + collectionTable_ + ".c_object_id)";
	if(grouping_.groupNormAttribute_.empty())
		query += ", "+ func +"(" + groupingAttr + ")" + sqlGridFrom();
	else
		query += ", "+ func +"(" + groupingAttr + ") / "+ func +"(" + grouping_.groupNormAttribute_ + ")" + sqlGridFrom();
	
	if(selectedObjects != TeAll)
		query += input;

	query += " GROUP BY " + collectionTable_ + ".c_object_id";

	map<int, vector<string> > legMap;
	
	TeDatabasePortal* portal = db->getPortal();
	if(portal->query(query) == false)
	{
		delete portal;
		return false;
	}

	vector<string> idVec;
	vector<string> nullIdVec;
	vector<string> valVec;
	while(portal->fetchRow())
	{
		string val = portal->getData(1);
		string oid = portal->getData(0);
		if (val.empty() == false)
		{
			idVec.push_back(oid);
			valVec.push_back(val);
		}
		else
			nullIdVec.push_back(oid);
	}
	if (grouping_.groupMode_ == TeUniqueValue)
	{
		unsigned int j = 0;
		while( j < idVec.size())
		{
			string val = valVec[j];
			string oid = idVec[j];
			if(type == TeREAL)
			{
				double a = atof(val.c_str());
				val = Te2String(a, grouping_.groupPrecision_);
			}
			else if(type == TeINT)
			{
				int a = atoi(val.c_str());
				val = Te2String(a);
			}
				
			unsigned int siz = legend_.size();
			if(grouping_.groupNullAttr_ && nullIdVec.size() > 0)
				--siz;
			for(i=0; i < siz; i++)
			{
				TeLegendEntry& leg = legend_[i];
				if(val == leg.from())
				{
					legMap[leg.id()].push_back(oid);
					break;
				}
			}
			j++;
		}
	}
	else
	{
		unsigned int j = 0;
		while(j < idVec.size())
		{
			string val = valVec[j];
			string oid = idVec[j];
			if(type == TeREAL)
			{
				double a = atof(val.c_str());
				val = Te2String(a, grouping_.groupPrecision_);
			}
			
			unsigned int siz = legend_.size();
			if(grouping_.groupNullAttr_ && !nullIdVec.empty())
               --siz; 
			for(i=0; i < siz; i++)
			{
				TeLegendEntry& leg = legend_[i];
				int f = leg.from().find("mean");
				if(f >= 0)
					continue;
				double dval = atof(val.c_str());
				double dfrom = atof(leg.from().c_str());
				double dto = atof(leg.to().c_str());
				if(i < legend_.size()-1)
				{
					if(dval >= dfrom && dval < dto)
					{
						legMap[leg.id()].push_back(oid);
						break;
					}
				}
				else
				{
					if(dval >= dfrom && dval <= dto)
					{
						legMap[leg.id()].push_back(oid);
						break;
					}
				}
			}
			j++;
		}
	}
	delete portal;

	int legId = defaultLegend_.id();
	if (grouping_.groupNullAttr_)
		legId = legend_[legend_.size()-1].id();	
	for(i = 0; i < nullIdVec.size(); ++i)
	{
		string oid = nullIdVec[i];
		legMap[legId].push_back(oid);
	}

	vector<string> svec;
	map<int, vector<string> > :: iterator it = legMap.begin();
	while(it != legMap.end())
	{
    	// --- Generate In Clauses ----
		unsigned int i, j, k, size, chunkSize = 200, nChunks;
		string inClause;
		size = it->second.size();
		if (size % chunkSize)
			nChunks = size / chunkSize + 1;
		else
			nChunks = size / chunkSize;

		j = 0;		
		for (k = 0; k < nChunks; ++k)
		{
			i = 0;
			inClause = "(";
			while (j < size && i < chunkSize)
			{
				inClause += "'" + db->escapeSequence(it->second[j]) + "',";
				i++;
				j++;
			}
			inClause[inClause.size() - 1] = ')';
			svec.push_back(inClause);
		}

		//--- generateInClause

		for(i=0; i<svec.size(); ++i)
		{
			string up = "UPDATE " + collectionTable_ + " SET c_legend_id = " + Te2String(it->first);
			up += " WHERE c_object_id IN " + svec[i];
			if (!objectId.empty())
				up += " AND c_object_id='"+objectId+"'";
			if(db->execute(up) == false)
				continue;
		}
		it++;
		svec.clear();
	}
	if(legend_.size())
		visibleRep_ = visibleRep_ | 0x40000000;
	else
		visibleRep_ = visibleRep_ | 0xbfffffff;
	string upVis = "UPDATE te_theme SET visible_rep=" + Te2String(visibleRep_) + " WHERE theme_id=" + Te2String(id());
	return (db->execute(upVis));
}

bool 
TeTheme::populateCollection(std::string objectId)  
{
	TeDatabase* db = layer_->database();
	if(!db || collectionTable_.empty())
		return false;

	if(attTableVector_.empty())
		attTableVector_ = layer_->attrTables();
	
	TeRepresPointerVector& represVec = layer_->vectRepres();
	for (unsigned int i = 0; i < represVec.size(); ++i)
	{
		TeRepresentation* rep = represVec[i];
		if(rep->geomRep_ == TeTEXT)
			continue;

		string geomTable = layer_->tableName(rep->geomRep_); 

		string sqlSelect, sqlFrom, sqlWhere;
		
		sqlSelect = " SELECT DISTINCT "+ geomTable +".object_id ";  
		sqlFrom = tableJoin(attTableVector_, geomTable, "object_id");
		
		sqlWhere  = " WHERE NOT EXISTS (SELECT * FROM " + collectionTable_; 
		sqlWhere += " WHERE "+ collectionTable_ +".c_object_id = "+ geomTable +".object_id )";
		sqlWhere += " AND " + geomTable + ".object_id IS NOT NULL ";
		
		//attribute restriction
		if(!generateAttributeRest_.empty())
			sqlWhere += " AND ("+ generateAttributeRest_ +" )"; 

		//temporal restriction
		if(!generateTemporalRest_.empty())
		{
			string sqlTemp;
			initParse(generateTemporalRest_, db); 
					
			if(!yyparse(sqlTemp))  //0: accept  1: reject
				sqlWhere += " AND "+ sqlTemp;
			else
				return false;
		}

		TeKeys objs;

		//spatial restriction
		if(hasSpatialRes_)
		{
			if(boxRest_.isValid())
			{
				TeBox box = boxRest_;
				sqlWhere += " AND "+ db->getSQLBoxWhere(box, geomRepRest_);
			}
			else if (geomRest_)
			{
				TePrecision::instance().setPrecision(TeGetPrecision(layer()->projection()));

				if(db->spatialRelation(geomTable, geomRepRest_, geomRest_, objs, spatialRelation_))
				{
					string obs;
					for(unsigned int i=0; i<objs.size(); i++)
					{
						if(i!=0)
							obs += ",";
						obs += "'"+ objs[i] +"'";
					}
						
					sqlWhere += " AND "+ geomTable +".object_id IN ("+ obs +")";
				}
			}
		}

		//populate the collection table
		string popule;
		popule = " INSERT INTO "+ collectionTable_ +" (c_object_id) ";
		popule += sqlSelect +" FROM "+ sqlFrom + sqlWhere; 	
		if (!objectId.empty())
		{
			if (sqlWhere.length())
				popule += " AND ";
			popule += geomTable +".object_id='" +objectId+ "'";
		}		

		if (!db->execute(popule))
		{
			//Treats an error in the MySQL database system
			//Error: 1062 SQLSTATE: 23000 (ER_DUP_ENTRY) 
			//Message: Duplicate entry '%s' for key %d 
			if(db->dbmsName() != "MySQL" || db->errorNum()!=1062)
                return false;
		}
	}

//	int defaultLegend = defaultLegend_.id();
	string popule = "UPDATE " + collectionTable_;
	popule += " SET c_legend_id=0, c_legend_own=0, c_object_status=0 ";
	
	if (!objectId.empty())
		popule += " WHERE c_object_id='"+objectId+"'";

	if (!db->execute(popule))
		return false;

	themeBox_= getThemeBox();
	return true;
}


bool TeTheme::hasObjectsWithoutGeometries(TeGeomRep geomRep)
{
	if(!layer() || !layer()->database())
		return true;
	
	//get the geometry table
	string geomTable = layer()->tableName(geomRep);
	if(geomTable.empty())
		return true;

	TeDatabasePortal* portal = layer()->database()->getPortal();
	if(!portal)
		return true;

	//Verify the collection table or attribute tables
	if(layer()->database()->tableExist(collectionTable()))
	{
		string s = " SELECT COUNT(*) ";
		s+= " FROM "+  collectionTable();
		s+= " WHERE NOT EXISTS ";
		s+= " (SELECT * FROM "+ geomTable;
		s+= " WHERE "+ collectionTable() +".c_object_id "; 
		s+= " = "+ geomTable +".object_id ) ";
		
		if(!portal->query(s) || !portal->fetchRow())
		{
			delete portal;
			return true;
		}

		int numObjs = atoi(portal->getData(0));
		if(numObjs>0)
		{
			delete portal;
			return true;
		}
	}
	else
	{
		//for each static table
		for(unsigned int i=0; i<this->attrTables().size(); ++i)
		{
			if(	this->attrTables()[i].tableType() == TeAttrExternal ||
				this->attrTables()[i].tableType() == TeAttrMedia || 
				this->attrTables()[i].tableType() == TeGeocodingData)
				continue;

			string s = " SELECT COUNT(*) ";
			s+= " FROM "+  attrTables()[i].name();
			s+= " WHERE NOT EXISTS ";
			s+= " (SELECT * FROM "+ geomTable;
			s+= " WHERE "+ attrTables()[i].name() +"."+ attrTables()[i].linkName();
			s+= " = "+ geomTable +".object_id ) ";

			if(!portal->query(s) || !portal->fetchRow())
			{
				delete portal;
				return true;
			}

			int numObjs = atoi(portal->getData(0));
			if(numObjs>0)
			{
				delete portal;
				return true;
			}
		}
	}

	delete portal;
	return false;
}

bool TeTheme::removeObjectsWithoutGeometries(TeGeomRep geomRep)
{
	if(!layer() || !layer()->database() || !layer()->database()->tableExist(collectionTable()))
		return true;
	
	//get the geometry table
	string geomTable = layer()->tableName(geomRep);
	if(geomTable.empty())
		return true;

	string del = " DELETE FROM "+ collectionTable();
	del += " WHERE NOT EXISTS ";
	del += " (SELECT * FROM "+ geomTable; 
	del += " WHERE "+ collectionTable() +".c_object_id = ";
	del += geomTable +".object_id) ";

	if(!layer()->database()->execute(del))
		return false;

	del = " DELETE FROM "+ collectionAuxTable();
	del += " WHERE NOT EXISTS ";
	del += " (SELECT * FROM "+ geomTable; 
	del += " WHERE "+ collectionAuxTable() +".object_id = ";
	del += geomTable +".object_id) ";

	if(!layer()->database()->execute(del))
		return false;

	return true;
}

bool 
TeTheme::save()  
{
	TeDatabase* db = layer()->database();
	if(!db)
		return false;

	//insert theme in database 
	if(id()==0)
	{
		if(!db->insertTheme(this)) //updateThemeTable
		{
			db->deleteTheme(this->id());
			return false;
		}
	}
	
	//collection table 
	if(collectionTable().empty())
		collectionTable("te_collection_"+ Te2String(id())); 

	if(!db->createCollectionTable(collectionTable_))
	{
		db->deleteTheme(this->id());
		return false;
	}

	//collection aux table
	collectionAuxTable(collectionTable() + "_aux");
	addThemeTable(collectionAuxTable());

	if(!createCollectionAuxTable())
	{
		db->deleteTheme(this->id());
		return false;
	}

	return true;
}

set<string> TeTheme::getObjects(TeSelectedObjects selectedObjects)
{
	if (selectedObjects == TeAll)
	{
		if (objectSet_.empty() == false)
			return objectSet_;
		else
		{
			if (attTableVector_.empty())
				return set<string>();

			// Set the new set of objects
			if (layer_->hasGeometry(TeRASTER) == false)
			{
				TeDatabase *db = layer_->database();
				TeDatabasePortal* portal = db->getPortal();

				string q = "SELECT " + attTableVector_[0].name() + "." + attTableVector_[0].uniqueName() + sqlFrom();
				if (portal->query(q) == false)
				{
					delete portal;
					return set<string>();
				}

				while (portal->fetchRow())
					objectSet_.insert(portal->getData(0));

				delete portal;
				return objectSet_;
			}
		}
	}

	set<string> oidSet;
	map<string, int>::iterator it;
	string oid;

	if (selectedObjects == TeSelectedByPointing)
	{
		for (it = objStatusMap_.begin(); it != objStatusMap_.end(); ++it)
		{
			oid = it->first;
			if (objStatusMap_[oid] == TePOINTED || objStatusMap_[oid] == TePOINTED_QUERIED)
				oidSet.insert(oid);
		}
	}
	else if (selectedObjects == TeNotSelectedByPointing)
	{
		oidSet = getObjects();
		for (it = objStatusMap_.begin(); it != objStatusMap_.end(); ++it)
		{
			oid = it->first;
			if (objStatusMap_[oid] == TePOINTED || objStatusMap_[oid] == TePOINTED_QUERIED)
				oidSet.erase(oid);
		}
	}
	else if (selectedObjects == TeSelectedByQuery)
	{
		for (it = objStatusMap_.begin(); it != objStatusMap_.end(); ++it)
		{
			oid = it->first;
			if (objStatusMap_[oid] == TeQUERIED || objStatusMap_[oid] == TePOINTED_QUERIED)
				oidSet.insert(oid);
		}
	}
	else if (selectedObjects == TeNotSelectedByQuery)
	{
		oidSet = getObjects();
		for (it = objStatusMap_.begin(); it != objStatusMap_.end(); ++it)
		{
			oid = it->first;
			if (objStatusMap_[oid] == TeQUERIED || objStatusMap_[oid] == TePOINTED_QUERIED)
				oidSet.erase(oid);
		}
	}
	else if(selectedObjects == TeGrouped)
	{
		map<string, int>& objLegendMap = getObjLegendMap();
		for (it = objLegendMap.begin(); it != objLegendMap.end(); ++it)
		{
			string oid = it->first;
			if (objLegendMap[oid] != 0)
				oidSet.insert(oid);
		}
	}
	else if(selectedObjects == TeNotGrouped)
	{
		oidSet = getObjects();
		map<string, int>& objLegendMap = getObjLegendMap();
		for (it = objLegendMap.begin(); it != objLegendMap.end(); ++it)
			oidSet.erase(it->first);
	} 
	else if(selectedObjects == TeSelectedByPointingOrQuery)
	{
		oidSet = getObjects();
		for (it = objStatusMap_.begin(); it != objStatusMap_.end(); ++it)
		{
			oid = it->first;
			if (objStatusMap_[oid] == TeQUERIED || objStatusMap_[oid] == TePOINTED)
				oidSet.insert(oid);
		}
	} 
	else if(selectedObjects == TeSelectedByPointingAndQuery )
	{
		oidSet = getObjects();
		for (it = objStatusMap_.begin(); it != objStatusMap_.end(); ++it)
		{
			oid = it->first;
			if (objStatusMap_[oid] == TePOINTED_QUERIED )
				oidSet.insert(oid);
		}
	}

	return oidSet;
}

vector<string> TeTheme::getItemVector(TeSelectedObjects selectedObjects)
{
	vector<string> itemVec;

	//======================================================================================
	//Get all the items
	vector<string> allItemVec;
	string item;
	unsigned int i;

	if (selectedObjects == TeAll || selectedObjects == TeNotSelectedByPointing ||
		selectedObjects == TeNotSelectedByQuery)
	{
		vector<TeTable> tableVec;
		getAttTables(tableVec);
		
		string q = "SELECT ";
		for (i = 0; i < tableVec.size(); ++i)
		{
			if (i != 0)
				q += ",";
			q += tableVec[i].uniqueName();
		}
		q += sqlFrom();

		TeDatabase* db = layer()->database();
		TeDatabasePortal* portal = db->getPortal();

		if (portal->query(q) == false)
		{
			delete portal;
			return itemVec;
		}

		while(portal->fetchRow())
		{
			item = portal->getData(tableVec[0].uniqueName());
			for (i = 1; i < tableVec.size(); ++i)
				item += portal->getData(tableVec[i].uniqueName());
			allItemVec.push_back(item);
		}

		delete portal;
	}

	//======================================================================================
	// Get the items according to the selectedObjects variable
	map<string, int>& itemStatusMap = getItemStatusMap();
	map<string, int>::iterator it;

	if (selectedObjects == TeAll)
		return allItemVec;
	else if(selectedObjects == TeSelectedByPointing)
	{
		for (it = itemStatusMap.begin(); it != itemStatusMap.end(); ++it)
		{
			item = it->first;
			if (itemStatusMap[item] == TePOINTED || itemStatusMap[item] == TePOINTED_QUERIED)
				itemVec.push_back(item);
		}
	}
	else if(selectedObjects == TeNotSelectedByPointing)
	{
		for (i = 0; i < allItemVec.size(); ++i)
		{
			item = allItemVec[i];
			if (itemStatusMap[item] == TeDEFAULT || itemStatusMap[item] == TeQUERIED)
				itemVec.push_back(item);

			if (itemStatusMap[item] == TeDEFAULT)
				itemStatusMap.erase(item);
		}
	}
	else if(selectedObjects == TeSelectedByQuery)
	{
		for (it = itemStatusMap.begin(); it != itemStatusMap.end(); ++it)
		{
			item = it->first;
			if (itemStatusMap[item] == TeQUERIED || itemStatusMap[item] == TePOINTED_QUERIED)
				itemVec.push_back(item);
		}
	}
	else if(selectedObjects == TeNotSelectedByQuery)
	{
		for (i = 0; i < allItemVec.size(); ++i)
		{
			item = allItemVec[i];
			if (itemStatusMap[item] == TeDEFAULT || itemStatusMap[item] == TePOINTED)
				itemVec.push_back(item);

			if (itemStatusMap[item] == TeDEFAULT)
				itemStatusMap.erase(item);
		}
	}
	else if(selectedObjects == TeGrouped)
	{
		set<string> oidSet;
		map<string, int>& objLegendMap = getObjLegendMap();
		for (it = objLegendMap.begin(); it != objLegendMap.end(); ++it)
		{
			string oid = it->first;
			if (objLegendMap[oid] != 0)
				oidSet.insert(oid);
		}

		itemVec = getItemVector(oidSet);
	}
	else if(selectedObjects == TeNotGrouped)
	{
		set<string> oidSet = getObjects();
		map<string, int>& objLegendMap = getObjLegendMap();
		for (it = objLegendMap.begin(); it != objLegendMap.end(); ++it)
			oidSet.erase(it->first);

		itemVec = getItemVector(oidSet);
	}
	else if(selectedObjects == TeSelectedByPointingOrQuery )
	{
		for (it = itemStatusMap.begin(); it != itemStatusMap.end(); ++it)
		{
			item = it->first;
			if( (itemStatusMap[item] == TePOINTED ) ||
			  (itemStatusMap[item] == TeQUERIED ) ) {
			  
				itemVec.push_back(item);
			}
		}
	}	

	return itemVec;
}

vector<string> TeTheme::getItemVector(const set<string>& oidSet)
{
	vector<string> itemVec;
	vector<TeTable> tableVec = attrTables();
	set<string>::const_iterator it;
	unsigned int i;

	if (tableVec.size() == 1)
	{
		for (it = oidSet.begin(); it != oidSet.end(); ++it)
			itemVec.push_back(*it);
		return itemVec;
	}

	// Set the expression that represents the concatenation 
	// of the unique names of each theme table
	string concatIndexStr;
	vector<string> indexVec;
	TeDatabase* db = layer()->database();
	for (i = 0; i < tableVec.size(); ++i)
		indexVec.push_back(tableVec[i].name() + "." + tableVec[i].uniqueName());

	concatIndexStr = db->getConcatFieldsExpression(indexVec);

	vector<string> queryVec;
	string query;


	set<string>::const_iterator itB = oidSet.begin();
	set<string>::const_iterator itE = oidSet.end();
	vector<string> inClauseVec = generateInClauses(itB, itE, db);
	for (i = 0; i < inClauseVec.size(); ++i)
	{
		query = "SELECT " + tableVec[i].name() + "." + tableVec[i].uniqueName();
		query += ", " + concatIndexStr + sqlFrom() + " WHERE ";
		query += tableVec[i].name() + "." + tableVec[i].uniqueName() + " IN " + inClauseVec[i];
		queryVec.push_back(query);
	}

	TeDatabasePortal* portal = db->getPortal();
	for (i = 0; i < queryVec.size(); ++i)
	{
		if (i != 0)
			portal->freeResult();

		if(portal->query(queryVec[i]) == false)
		{
			delete portal;
			return itemVec;
		}

		while(portal->fetchRow())
			itemVec.push_back(portal->getData(1));
	}

	delete portal;
	return itemVec;
}


set<string> TeTheme::getObjects(const vector<string>& itemVec)
{
	set<string> oidSet;

	unsigned int i;
	TeDatabase* db = layer()->database();

	// Get the vector of tables of the theme
	vector<TeTable> tableVec;
	getAttTables(tableVec);

	vector<string> indexVec;
	for (i = 0; i < tableVec.size(); ++i)
		indexVec.push_back(tableVec[i].name() + "." + tableVec[i].uniqueName());

	string concatIndexStr = db->getConcatFieldsExpression(indexVec);

	std::vector<string>::const_iterator itemVec_it_begin( itemVec.begin() );
	std::vector<string>::const_iterator itemVec_it_end( itemVec.end() );
  
	vector<string> inClauseVec = generateInClauses( itemVec_it_begin, 
    itemVec_it_end, db);

	vector<string> queryVec;
	string selectClause = "SELECT " + concatIndexStr + ", ";
	selectClause += tableVec[0].name() + "." + tableVec[0].uniqueName() + " " + sqlFrom();

	for (i = 0; i < inClauseVec.size(); ++i)
	{
		string query = selectClause + " WHERE " + concatIndexStr + " IN " + inClauseVec[i];
		queryVec.push_back(query);
	}

	TeDatabasePortal *portal = db->getPortal();
	for (i = 0; i < queryVec.size(); ++i)
	{
		if (i != 0)
			portal->freeResult();

		if (portal->query(queryVec[i]))
		{
			while (portal->fetchRow())
				oidSet.insert(portal->getData(1));
		}
	}
	delete portal;

	return oidSet;
}

unsigned int 
TeTheme::getNumberOfObjects()
{
	int numRows = 0;
	string s = "SELECT COUNT(*) " + this->sqlFrom();	
	TeDatabasePortal* portal = this->layer()->database()->getPortal();
	if(portal->query(s) && portal->fetchRow() )
	{
		numRows = atoi(portal->getData(0));
	}
	delete portal;
	return numRows;
}

TeProjection* 
TeTheme::getThemeProjection()
{
	if (layer_)
		return layer_->projection();
	else
		return 0;
}

