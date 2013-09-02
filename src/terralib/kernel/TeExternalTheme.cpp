#include "TeExternalTheme.h"
#include "TeLayer.h"
#include "TeDatabase.h"
#include "TeDatabaseFactoryParams.h"
#include "TeDBConnectionsPool.h"
#include "TeGroupingAlgorithms.h"
#include "TeQuerier.h"
#include "TeQuerierParams.h"
#include "TeRasterTransform.h"


TeExternalTheme::TeExternalTheme(TeDatabase* sourceDB, const string& name, TeViewNode* parent, const int& view, const int& id)
	: TeTheme(name, 0, parent, view, id), remoteTheme_(0), sourceDB_(sourceDB)
{
	type(TeEXTERNALTHEME);
}

TeExternalTheme::TeExternalTheme(const TeViewNodeParams& params)
	: TeTheme(params), remoteTheme_(0), sourceDB_(0)
{ 
}

TeExternalTheme::~TeExternalTheme()
{
}

TeExternalTheme::TeExternalTheme(const TeExternalTheme& rhs) :
	TeTheme(rhs.viewNodeParams_)
{ 
//	*(TeTheme*)this = rhs;
	remoteTheme_ = rhs.remoteTheme_;
	sourceDB_ = rhs.sourceDB_;
	remoteDBConn_ = rhs.remoteDBConn_;
}

TeExternalTheme& TeExternalTheme::operator=(TeExternalTheme& rhs)
{
	*(TeTheme*)this = rhs;
	remoteTheme_ = rhs.remoteTheme_;
	sourceDB_ = rhs.sourceDB_;
	remoteDBConn_ = rhs.remoteDBConn_;
	return *this;
}

void 
TeExternalTheme::setSourceDatabase(TeDatabase* db)
{	
	this->sourceDB_ = db; 
}

TeDatabase* 
TeExternalTheme::getSourceDatabase()
{ 
	return sourceDB_; 
}

TeTheme* 
TeExternalTheme::getRemoteTheme() const
{
	return remoteTheme_;
}

void 
TeExternalTheme::setRemoteTheme(TeTheme* theme)
{ 
	this->remoteTheme_ = theme; 
}

string 
TeExternalTheme::getRemoteThemeName()
{ 
	return remoteTheme_->name();
}

void 
TeExternalTheme::loadObjectLegendMap()
{
	objLegendMap_.clear();
	objOwnLegendMap_.clear();
	objStatusMap_.clear();
	itemStatusMap_.clear();

	TeDatabasePortal* portal = sourceDB_->getPortal();

	if(!portal->query("SELECT c_object_id, c_legend_id, c_legend_own, c_object_status FROM " + collectionTable()))
		return;

	while(portal->fetchRow())
	{
		string objId = portal->getData(0);
		int status = portal->getInt(3);
		if(portal->getInt(1) != 0)
			objLegendMap_[portal->getData(0)] = portal->getInt(1);
		if(portal->getInt(2) != 0)
			objOwnLegendMap_[portal->getData(0)] = portal->getInt(2);
		if(status != 0)
			objStatusMap_[objId] = status;
	}

	portal->freeResult();

	if(!portal->query("SELECT object_id, unique_id, grid_status FROM " + collectionAuxTable()))
		return;
	
	while(portal->fetchRow())
	{
		if(portal->getInt(2) != 0)
		{
			itemStatusMap_[portal->getData(1)] = portal->getInt(2);
		}
	}

	portal->freeResult();
	delete portal;
}

int 
TeExternalTheme::getGridStatus(const int uniqueId, const std::string /* objectId */)
{
	string aux = Te2String(uniqueId);
	std::map<std::string, int >::iterator itGridStatus = itemStatusMap_.find(aux);
	if(itGridStatus == itemStatusMap_.end())
		return 0;
	return itGridStatus->second;
}
		
int 
TeExternalTheme::getObjectStatus(const std::string objectId)
{
	std::map<std::string, int>::iterator it = objStatusMap_.find(objectId);
	if(it == objStatusMap_.end())
		return 0;
	
	return it->second;
}

bool 
TeExternalTheme::setObjectLegendStatusMap(const std::string objId, const int status)
{
	if(objId.empty())
		return false;
	
	if( status == 0)
	{
		map<std::string, int>::iterator itLegStatus = objStatusMap_.find(objId);

		if(itLegStatus != objStatusMap_.end())
		{
			objStatusMap_.erase(itLegStatus);
			return true;
		}
		return false;
	}
	
	objStatusMap_[objId] = status;

	return true;
}

bool 
TeExternalTheme::setObjectGridStatusMap(const std::string objId, const int uniqueId, const int status)
{
	string aux = Te2String(uniqueId);
	if(objId.empty() || aux.empty())
		return false;
	
	if( status == 0 )
	{
		std::map<std::string, int >::iterator itGridStatus = itemStatusMap_.find(aux);

		if(itGridStatus != itemStatusMap_.end())
		{
			itemStatusMap_.erase(itGridStatus);
			return true;
		}
		return false;
	}
	
	itemStatusMap_[aux] = status;

	return true;
}

int 
TeExternalTheme::layerId()
{
	return -1;
}

void 
TeExternalTheme::layerId(int)
{
}

void 
TeExternalTheme::layer(TeLayer* layer)
{
	if(remoteTheme_)
		remoteTheme_->layer(layer);
}

TeLayer*  
TeExternalTheme::layer()
{
	if(remoteTheme_)
		return remoteTheme_->layer();
	return 0;
}

TeProjection* 
TeExternalTheme::getThemeProjection()
{
	if(remoteTheme_ && remoteTheme_->layer())
		return remoteTheme_->layer()->projection();
	return 0;
}

bool 
TeExternalTheme::hasRestriction()
{
	return false;
}

bool TeExternalTheme::hasAttrRest()
{
	return false;
}

bool TeExternalTheme::hasTemporalRest()
{
	return false;
}

bool TeExternalTheme::hasSpatialRest()
{
	return false;
}

string TeExternalTheme::sqlWhereRestrictions(TeRepresentation* /* rep */)
{
	return string("");
}

void TeExternalTheme::loadTablesJoin(const string& geomTable)
{
	remoteTheme_->loadTablesJoin(geomTable);
}

string TeExternalTheme::sqlGridJoin()
{
	return remoteTheme_->sqlGridJoin();
}

string TeExternalTheme::sqlGridFrom(const string& geomTable)
{
	return remoteTheme_->sqlGridFrom(geomTable);
}

bool TeExternalTheme::save(TeDatabase* db)  
{
	if(!db)
		return false;

	//insert theme in database 
	//TeDatabase::insertTheme and TeDatabase::updateTheme use the method saveMetadata
	if(id()==0)
	{
		if(!db->insertTheme(this)) //updateThemeTable
		{
			db->deleteTheme(this->id());
			return false;
		}
	}
	else
	{
		if(!db->updateTheme(this)) //updateThemeTable
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
	
	if(!TeTheme::createCollectionAuxTable(db))
	{
		db->deleteTheme(this->id());
		return false;
	}

	return true;
}

bool TeExternalTheme::save()
{
	if(!save(sourceDB_))
		return false;
	
	// a map to identify legends id between local theme and remote theme during copy.
	std::map<int, int> mapIdLegend;	

	createLegendMapId(mapIdLegend);

	if(!mapIdLegend.empty())
		copyRemoteCollection(mapIdLegend);
	
	mapIdLegend.clear();

	loadObjectLegendMap();

	return true;
}

bool TeExternalTheme::createCollectionAuxTable()
{
	return TeTheme::createCollectionAuxTable(sourceDB_);
}

bool TeExternalTheme::populateCollectionAux(std::string /* objectId */)
{
	throw;
	return false;
}

bool TeExternalTheme::buildGrouping(const TeGrouping& g, TeSelectedObjects /* selectedObjects */,
				                  vector<double>* dValuesVec)
{
	TeDatabase* db = remoteTheme_->layer()->database();
	if(!db)
		return false;
	
	grouping_ = g;
	unsigned int i;
	vector<TeSlice> slices;
	int	nullValues = 0;
	if (grouping_.groupMode_ == TeRasterSlicing)
	{
		int b = atoi(grouping_.groupAttribute_.name_.c_str());
		if (!remoteTheme_->layer()->raster() ||  
			b < 0 ||
			b > remoteTheme_->layer()->raster()->params().nBands() ||
			grouping_.groupNumSlices_ <= 0)
			return false;

		if (grouping_.groupMaxVal_ == TeMINFLOAT)
			grouping_.groupMaxVal_ = remoteTheme_->layer()->raster()->params().vmax_[b];

		if (grouping_.groupMinVal_ == TeMAXFLOAT)
			grouping_.groupMinVal_ = remoteTheme_->layer()->raster()->params().vmin_[b];

		TeGroupByEqualStep(grouping_.groupMinVal_, grouping_.groupMaxVal_,
			grouping_.groupNumSlices_, slices, grouping_.groupPrecision_);
	}
	else
	{
        if(grouping_.groupAttribute_.name_.empty())
			return false;

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
		query += " GROUP BY " + remoteTheme_->collectionTable() + ".c_object_id";
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

bool TeExternalTheme::buildGrouping(const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec)
{
	TeDatabase* db = remoteTheme_->layer()->database();

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
		if (!remoteTheme_->layer()->raster() ||  
			b < 0 ||
			b > remoteTheme_->layer()->raster()->params().nBands() ||
			grouping_.groupNumSlices_ <= 0)
			return false;

		if (grouping_.groupMaxVal_ == TeMINFLOAT)
			grouping_.groupMaxVal_ = remoteTheme_->layer()->raster()->params().vmax_[b];

		if (grouping_.groupMinVal_ == TeMAXFLOAT)
			grouping_.groupMinVal_ = remoteTheme_->layer()->raster()->params().vmin_[b];

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
				vec = sti.getPropertyVector();
				val = vec[0].value_;
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
					
bool TeExternalTheme::saveGrouping(TeSelectedObjects selectedObjects)
{
	return TeTheme::saveGrouping(sourceDB_, selectedObjects);
}

bool TeExternalTheme::saveLegendInCollection(TeSelectedObjects selectedObjects, std::string objectId)
{
	return saveLegendInCollection(sourceDB_, selectedObjects, objectId);
}

bool TeExternalTheme::saveLegendInCollection(TeDatabase* db, TeSelectedObjects /* selectedObjects */, std::string objectId)
{
	unsigned int i;
	if(grouping_.groupMode_ == TeNoGrouping)
		return false; 

	TeAttrDataType		type = grouping_.groupAttribute_.type_;
	TeLegendEntryVector legVec = legend_;
	string			groupingAttr = grouping_.groupAttribute_.name_;

	string collectionTableRemote = remoteTheme_->collectionTable();
	string func;

	if(grouping_.groupFunction_.empty())
		func = " MIN";
	else
		func = grouping_.groupFunction_;

	if(grouping_.groupFunction_ == "COUNT")
		type = TeINT;

	string query = "SELECT MIN(" + collectionTableRemote + ".c_object_id)";
	if(grouping_.groupNormAttribute_.empty())
		query += ", "+ func +"(" + groupingAttr + ")" + remoteTheme_->sqlGridFrom();
	else
		query += ", "+ func +"(" + groupingAttr + ") / "+ func +"(" + grouping_.groupNormAttribute_ + ")" + remoteTheme_->sqlGridFrom();
	
	query += " GROUP BY " + collectionTableRemote + ".c_object_id";

	map<int, vector<string> > legMap;
	
	TeDatabasePortal* portal = remoteTheme_->layer()->database()->getPortal();
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
	
	if(!db->execute(upVis))
		return false;
	
	loadObjectLegendMap();

	return true;
}

void TeExternalTheme::setLegendsForObjects()
{
}

bool TeExternalTheme::generateLabelPositions(const std::string& /* objectId */)
{
	throw;
	return false;
}

bool TeExternalTheme::deleteGrouping()
{
	TeDatabase* db = sourceDB_;
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

void TeExternalTheme::createRasterVisual(TeRaster* rst)
{
	if (rasterVisual_)
		delete rasterVisual_;

	if (!rst)
		rst = remoteTheme_->layer()->raster();

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

bool TeExternalTheme::addThemeTable(TeTable& table)
{
	if( remoteTheme_ )
		return remoteTheme_->addThemeTable(table);

	return false;
}

void TeExternalTheme::addThemeTable(string tableName)
{
	if( remoteTheme_ )
		return remoteTheme_->addThemeTable(tableName);

	return;
}

bool TeExternalTheme::isThemeTable(int tableId)
{
	if( remoteTheme_ )
		return remoteTheme_->isThemeTable(tableId);

	return false;
}

bool TeExternalTheme::isThemeTable(string tableName)
{
	if( remoteTheme_ )
		return remoteTheme_->isThemeTable(tableName);

	return false;
}

TeAttrTableVector& TeExternalTheme::attrTables()
{
	if( remoteTheme_ )
		return remoteTheme_->attrTables();
	attTableVector_.clear();
	return attTableVector_;
}

bool TeExternalTheme::setAttTables(TeAttrTableVector& attrs)
{
	if( remoteTheme_ )
		return remoteTheme_->setAttTables(attrs);

	return false;
}

bool TeExternalTheme::getAttTables(TeAttrTableVector& attrs, TeAttrTableType attType)
{
	if(remoteTheme_)
		return remoteTheme_->getAttTables(attrs, attType);

	return false;
}

bool TeExternalTheme::getTable(TeTable& table, const string tableName)
{
	if(remoteTheme_)
		return remoteTheme_->getTable(table, tableName);

	return false;
}

void TeExternalTheme::clearAttTableVector() 
{
	if(remoteTheme_)
		remoteTheme_->clearAttTableVector();
}

bool TeExternalTheme::getTemporalTable(TeTable& table)
{
	if(remoteTheme_)
		return remoteTheme_->getTemporalTable(table);

	return false;
}

bool TeExternalTheme::removeThemeTable(unsigned int index)
{
	if(remoteTheme_)
		return remoteTheme_->removeThemeTable(index);

	return false;
}

string TeExternalTheme::getTableName(const string& attrName)
{
	if(remoteTheme_)
		return remoteTheme_->getTableName(attrName);

	return "";
}

string TeExternalTheme::getAttribute(unsigned int index)
{
	if(remoteTheme_)
		return remoteTheme_->getAttribute(index);

	return "";
}


bool TeExternalTheme::loadThemeTables()
{
	if(!remoteTheme_)
		return false;

	bool result = remoteTheme_->loadThemeTables();

	if(result)
	{
		clearAttTableVector();
		attrTables() = remoteTheme_->attrTables();
	}

	return result;
}


TeAttributeList TeExternalTheme::sqlAttList()
{
	if(remoteTheme_)
		return remoteTheme_->sqlAttList();

	return TeAttributeList();
}

void TeExternalTheme::clearAttList()
{
	if(remoteTheme_)
		return remoteTheme_->clearAttList();
}

TeAttributeList TeExternalTheme::sqlNumAttList()
{
	if(remoteTheme_)
		return remoteTheme_->sqlNumAttList();

	return TeAttributeList();
}

void TeExternalTheme::clearNumAttList()
{
	if(remoteTheme_)
		remoteTheme_->clearNumAttList();
}

string TeExternalTheme::sqlJoin()
{
	if(remoteTheme_)
		return remoteTheme_->sqlJoin();

	return "";
}

string TeExternalTheme::sqlFrom()
{
	if(remoteTheme_)
		return remoteTheme_->sqlFrom();

	return "";
}

vector<string>&	TeExternalTheme::aliasVector()
{
	if(remoteTheme_)
		return remoteTheme_->aliasVector();
	aliasVector_.clear();
	return aliasVector_;
}

void TeExternalTheme::loadAliasVector()
{
	if(remoteTheme_)
		return remoteTheme_->loadAliasVector();

	return;
}

void TeExternalTheme::loadAttrLists()
{
	if(!remoteTheme_)
		return;

	remoteTheme_->loadAttrLists();

	sqlAttList_.clear();
	sqlNumAttList_.clear();

	sqlAttList_ = remoteTheme_->sqlAttList();
	sqlNumAttList_ = remoteTheme_->sqlNumAttList();

}

bool TeExternalTheme::locatePolygon(TeCoord2D &pt, TePolygon &polygon, const double& tol)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->locatePolygon(pt, polygon, tol);
}

bool TeExternalTheme::locatePolygonSet(TeCoord2D &pt, double tol, TePolygonSet &polygons)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->locatePolygonSet(pt, tol, polygons);
}

bool TeExternalTheme::locateLine(TeCoord2D &pt, TeLine2D &line, const double& tol)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->locateLine(pt, line, tol);
}

bool TeExternalTheme::locatePoint(TeCoord2D &pt, TePoint &point, const double& tol)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->locatePoint(pt, point, tol);
}

bool TeExternalTheme::locateCell(TeCoord2D &pt, TeCell &c, const double& tol)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->locateCell(pt, c, tol);
}

bool TeExternalTheme::hasObjectsWithoutGeometries(TeGeomRep geomRep)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->hasObjectsWithoutGeometries(geomRep);
}

bool TeExternalTheme::removeObjectsWithoutGeometries(TeGeomRep geomRep)
{
	if(!remoteTheme_)
		return false;

	return remoteTheme_->removeObjectsWithoutGeometries(geomRep);
}

int TeExternalTheme::createExternalThemeTable(TeDatabase* sourceDB)
{
	if(!sourceDB)
		return 0;

	if(sourceDB->tableExist("te_external_theme"))
		return -1;

	TeAttributeList attList;

	TeAttribute att1;
	att1.rep_.name_ = "theme_id";
	att1.rep_.isAutoNumber_ = false;
	att1.rep_.isPrimaryKey_ = true;
	att1.rep_.null_ = false;
	att1.rep_.type_ = TeINT;
	att1.rep_.numChar_ = 0;
	attList.push_back(att1);

	TeAttribute att2;
	att2.rep_.name_ = "database_id";
	att2.rep_.isAutoNumber_ = false;
	att2.rep_.isPrimaryKey_ = false;
	att2.rep_.null_ = false;
	att2.rep_.type_ = TeINT;
	att2.rep_.numChar_ = 0;
	attList.push_back(att2);

	TeAttribute att3;
	att3.rep_.name_ = "external_theme_id";
	att3.rep_.isAutoNumber_ = false;
	att3.rep_.isPrimaryKey_ = false;
	att3.rep_.null_ = false;
	att3.rep_.type_ = TeINT;
	att3.rep_.numChar_ = 0;
	attList.push_back(att3);

	if(!sourceDB->createTable("te_external_theme", attList))
		return 0;

	return 1;
}

bool TeExternalTheme::populateCollection(std::string /* objectId */)
{
	throw;
	return false;
}

void TeExternalTheme::createLegendMapId(std::map<int, int>& mapIdLegend)
{
	mapIdLegend.clear();

	mapIdLegend[remoteTheme_->outOfCollectionLegend().id()] = outOfCollectionLegend().id();
	mapIdLegend[remoteTheme_->withoutDataConnectionLegend().id()] = withoutDataConnectionLegend().id();
	mapIdLegend[remoteTheme_->defaultLegend().id()] = defaultLegend().id();
	mapIdLegend[remoteTheme_->pointingLegend().id()] = pointingLegend().id();
	mapIdLegend[remoteTheme_->queryLegend().id()] = queryLegend().id();
	mapIdLegend[remoteTheme_->queryAndPointingLegend().id()] = queryAndPointingLegend().id();

	for (int i = 0; i < remoteTheme_->grouping().groupNumSlices_; ++i)
	{
		mapIdLegend[remoteTheme_->legend()[i].id()] = legend()[i].id();
	}
	
	return;
}

bool TeExternalTheme::copyRemoteCollection(std::map<int, int>& mapIdLegend)
{
	if(mapIdLegend.empty())
		return false;
//copy collectionTable
	TeDatabasePortal* remotePortal = remoteTheme_->layer()->database()->getPortal();
	string strSQL  = "SELECT * from ";
	strSQL	+= remoteTheme_->collectionTable();

	if(remotePortal->query(strSQL))
	{
		while(remotePortal->fetchRow())
		{
			string c_obj_id = remotePortal->getData(0);
			int c_leg_id = atoi(remotePortal->getData(1));
			string lab_x = remotePortal->getData(2);
			string lab_y = remotePortal->getData(3);
//			string c_leg_own = remotePortal->getData(4);
			string c_leg_own = "0";
			string c_obj_stat = remotePortal->getData(5);

			int new_c_leg_id = mapIdLegend[c_leg_id];

			string strINS = "INSERT INTO ";
			strINS += this->collectionTable();
			strINS += " (c_object_id, c_legend_id, label_x, label_y, c_legend_own, c_object_status) ";
			strINS += "VALUES (";
			strINS += c_obj_id;
			strINS += ", ";
			strINS += Te2String(new_c_leg_id);
			strINS += ", ";
			strINS += lab_x;
			strINS += ", ";
			strINS += lab_y;
			strINS += ", ";
			strINS += c_leg_own;
			strINS += ", ";
			strINS += c_obj_stat;
			strINS += ")";

			if(!sourceDB_->execute(strINS))
				return false;
		}
	}
	else
		return false;

	remotePortal->freeResult();
	delete remotePortal;


//copy collectionTableAux
	TeDatabasePortal* remotePortalAux = remoteTheme_->layer()->database()->getPortal();
	string strSQLAux  = "SELECT * from ";
	strSQLAux	+= remoteTheme_->collectionAuxTable();

	if(remotePortalAux->query(strSQLAux))
	{
		while(remotePortalAux->fetchRow())
		{
			string c_obj_id = remotePortalAux->getData(0);
			string unique_id = remotePortalAux->getData(1);
			string grid_status = remotePortalAux->getData(2);

			string strINSAux = "INSERT INTO ";
			strINSAux += this->collectionAuxTable();
			strINSAux += " (object_id, unique_id, grid_status) ";
			strINSAux += "VALUES (";
			strINSAux += c_obj_id;
			strINSAux += ", ";
			strINSAux += unique_id;
			strINSAux += ", ";
			strINSAux += grid_status;
			strINSAux += ")";

			if(!sourceDB_->execute(strINSAux))
				return false;
		}
	}
	else
		return false;

	remotePortalAux->freeResult();
	delete remotePortalAux;

	return true;
}



bool TeExternalTheme::getRemoteThemeInfo(int& remoteThemeId, int& databaseId)
{
	if(!sourceDB_)
		return false;
	
	std::string strSQL = " SELECT * FROM te_external_theme, te_database_connection ";
	strSQL += " WHERE te_external_theme.database_id = te_database_connection.connection_id ";
	strSQL += " AND theme_id = " + Te2String(id());
    
	TeDatabasePortal* dbPortal = sourceDB_->getPortal();

    if(!dbPortal)
		return false;

    if(!dbPortal->query(strSQL) || !dbPortal->fetchRow())
    {
        dbPortal->freeResult();
        delete dbPortal;
		return false;
	}

	databaseId = dbPortal->getInt("database_id");
	remoteThemeId = dbPortal->getInt("external_theme_id");
	string dbmsName = dbPortal->getData("dbms_name"); 
	string hostName = dbPortal->getData("host_name"); 
	string dbName = dbPortal->getData("database_name");
	string userName = dbPortal->getData("user_name");
	string userPass = dbPortal->getData("user_password");
	int port = dbPortal->getInt("port_number");

	TeDBConnection dbConn(hostName, dbName, userName, dbmsName, port, userPass, databaseId);
	dbConn.setDatabase(0);
	setRemoteDBConnection(dbConn);

	dbPortal->freeResult();
    delete dbPortal;
	return true;
}

bool 
TeExternalTheme::loadMetadata(TeDatabase* db)
{
	if(remoteTheme_ || !db)
		return false;

	int remoteThemeId = -1;
	int remoteDBId = -1;

	this->setSourceDatabase(db);

	if(!getRemoteThemeInfo(remoteThemeId, remoteDBId))
		return false;	
	
	TeDatabase* remotedb = TeDBConnectionsPool::instance().getDatabase(getRemoteDBConnection().getDbKey());
		
	if(!remotedb || !remotedb->isConnected())
		return false;
	
	TeThemeMap::iterator itTheme = remotedb->themeMap().find(remoteThemeId);

	if(itTheme != remotedb->themeMap().end())
	{
// is theme already loaded?
		if(itTheme->second->getProductId()!=TeTHEME)
			return false;
		remoteTheme_ = static_cast<TeTheme*>(itTheme->second);
	}
	else
	{
// if theme is not loaded... we load it.
		remoteTheme_ = new TeTheme();
		remoteTheme_->id(remoteThemeId);
		if(!remotedb->loadTheme(remoteTheme_))
			return false;
	}

	loadObjectLegendMap();
	return true;
}

bool TeExternalTheme::saveMetadata(TeDatabase* db)
{
	if(!remoteTheme_ || !remoteTheme_->layer() || !remoteTheme_->layer()->database())
		return false;
	
	int idDatabase = this->getRemoteDBConnection().getId();
	int themeId = this->id();
	int remoteThemeId = remoteTheme_->id();

	TeDatabasePortal* portal = db->getPortal();
	if(!portal)
		return false;
	
	string sql = "SELECT * FROM te_external_theme WHERE theme_id = "+ Te2String(themeId);
	bool isUpdate = false;
	if(!portal->query(sql))
	{
		delete portal;
		return false;
	}
	if(portal->fetchRow())
		isUpdate = true;
	delete portal;

	string strSQL;
	if(isUpdate)
	{
		strSQL  = "UPDATE te_external_theme SET ";
		strSQL += " theme_id = "+ Te2String(themeId);
		strSQL += ", ";
		strSQL += " database_id = "+ Te2String(idDatabase);
		strSQL += ", ";
		strSQL += " external_theme_id = "+ Te2String(remoteThemeId);
		strSQL += " WHERE theme_id = "+ Te2String(themeId);
	}
	else
	{
		strSQL  = "INSERT INTO te_external_theme (theme_id, database_id, external_theme_id) VALUES (";
		strSQL += Te2String(themeId);
		strSQL += ", ";
		strSQL += Te2String(idDatabase);
		strSQL += ", ";
		strSQL += Te2String(remoteThemeId);
		strSQL += ")";
	}

	if(!db->execute(strSQL))
		return false;

	return true;
}

bool TeExternalTheme::eraseMetadata(TeDatabase* db)
{
	int themeId = this->id();
	
	string strSQL;
	strSQL  = " DELETE FROM te_external_theme ";
    strSQL += " WHERE theme_id = "+ Te2String(themeId);
	
	if(!db->execute(strSQL))
		return false;
	return true;
}

