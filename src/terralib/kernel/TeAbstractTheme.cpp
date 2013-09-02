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

#include "TeAbstractTheme.h"
#include "TeVisual.h"
#include "TeRasterTransform.h"

extern int  yyparse(string& sqlOut);
extern int  initParse(const string& strIn, TeDatabase* db);

TeAbstractTheme::TeAbstractTheme(const string& name, TeViewNode* parent, const int& view, const int& id, const TeViewNodeType& nodeType)
		: TeViewNode(name, parent, view, id, nodeType),
		generateAttributeRest_(""),
		generateTemporalRest_(""),
		hasSpatialRes_(false),
		geomRest_(0),
		minScale_(0.0),
		maxScale_(0.0),
		visibleRep_(0),
		enableVisibility_(1),
		rasterVisual_(0),
		themeBox_(TeBox())
{
	grouping_.groupMode_ = TeNoGrouping;

	//default legends
	TeVisual* visp = new TeVisual(TePOLYGONS);
	TeVisual* visl = new TeVisual(TeLINES);
	TeVisual* vispt = new TeVisual(TePOINTS);
	TeVisual* vist = new TeVisual(TeTEXT);

	TeColor	color;
	color.init(100, 220, 220);
	visp->color(color);
	visl->color(color);
	vispt->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	visl->contourColor(color);
	vispt->contourColor(color);

	outOfCollectionLegend_.setVisual(visp->copy(), TePOLYGONS);		
	outOfCollectionLegend_.setVisual(visl->copy(), TeLINES);		
	outOfCollectionLegend_.setVisual(vispt->copy(), TePOINTS);		
	outOfCollectionLegend_.setVisual(vist->copy(), TeTEXT);	

	color.init(220, 100, 220);
	visp->color(color);
	visl->color(color);
	vispt->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	visl->contourColor(color);
	vispt->contourColor(color);

	withoutDataConnectionLegend_.setVisual(visp->copy(), TePOLYGONS);		
	withoutDataConnectionLegend_.setVisual(visl->copy(), TeLINES);		
	withoutDataConnectionLegend_.setVisual(vispt->copy(), TePOINTS);		
	withoutDataConnectionLegend_.setVisual(vist->copy(), TeTEXT);	

	color.init(220, 0, 0);
	visp->color(color);
	color.init(0, 220, 220);
	visl->color(color);
	color.init(220, 90, 180);
	vispt->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	visl->contourColor(color);
	vispt->contourColor(color);
	color.init(0, 0, 0);
	vist->color(color);

	defaultLegend_.setVisual(visp->copy(), TePOLYGONS);		
	defaultLegend_.setVisual(visl->copy(), TeLINES);		
	defaultLegend_.setVisual(vispt->copy(), TePOINTS);		
	defaultLegend_.setVisual(vist->copy(), TeTEXT);		
	
	color.init(80, 240, 100);
	visp->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	pointingLegend_.setVisual(visp->copy(), TePOLYGONS);		
	pointingLegend_.setVisual(visl->copy(), TeLINES);		
	pointingLegend_.setVisual(vispt->copy(), TePOINTS);		
	pointingLegend_.setVisual(vist->copy(), TeTEXT);	

	color.init(210, 210, 0);
	visp->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);		
	queryLegend_.setVisual(visp->copy(), TePOLYGONS);		
	queryLegend_.setVisual(visl->copy(), TeLINES);		
	queryLegend_.setVisual(vispt->copy(), TePOINTS);		
	queryLegend_.setVisual(vist->copy(), TeTEXT);	
							
	color.init(255, 255, 0);
	visp->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);		
	queryAndPointingLegend_.setVisual(visp->copy(), TePOLYGONS);	
	queryAndPointingLegend_.setVisual(visl->copy(), TeLINES);		
	queryAndPointingLegend_.setVisual(vispt->copy(), TePOINTS);		
	queryAndPointingLegend_.setVisual(vist->copy(), TeTEXT);	

	delete visp;
	delete visl;
	delete vispt;
	delete vist;
}

TeAbstractTheme::TeAbstractTheme(const TeViewNodeParams& params) : 
		TeViewNode(params),
		generateAttributeRest_(""),
		generateTemporalRest_(""),
		hasSpatialRes_(false),
		geomRest_(0),
		minScale_(0.0),
		maxScale_(0.0),
		visibleRep_(0),
		enableVisibility_(1),
		rasterVisual_(0),
		themeBox_(TeBox())
{
	grouping_.groupMode_ = TeNoGrouping;
	//default legends
	TeVisual* visp = new TeVisual(TePOLYGONS);
	TeVisual* visl = new TeVisual(TeLINES);
	TeVisual* vispt = new TeVisual(TePOINTS);
	TeVisual* vist = new TeVisual(TeTEXT);
	TeColor	color;
	color.init(100, 220, 220);
	visp->color(color);
	visl->color(color);
	vispt->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	visl->contourColor(color);
	vispt->contourColor(color);

	outOfCollectionLegend_.setVisual(visp->copy(), TePOLYGONS);		
	outOfCollectionLegend_.setVisual(visl->copy(), TeLINES);		
	outOfCollectionLegend_.setVisual(vispt->copy(), TePOINTS);		
	outOfCollectionLegend_.setVisual(vist->copy(), TeTEXT);	

	color.init(220, 100, 220);
	visp->color(color);
	visl->color(color);
	vispt->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	visl->contourColor(color);
	vispt->contourColor(color);

	withoutDataConnectionLegend_.setVisual(visp->copy(), TePOLYGONS);		
	withoutDataConnectionLegend_.setVisual(visl->copy(), TeLINES);		
	withoutDataConnectionLegend_.setVisual(vispt->copy(), TePOINTS);		
	withoutDataConnectionLegend_.setVisual(vist->copy(), TeTEXT);	

	color.init(220, 0, 0);
	visp->color(color);
	color.init(0, 220, 220);
	visl->color(color);
	color.init(220, 90, 180);
	vispt->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	visl->contourColor(color);
	vispt->contourColor(color);
	color.init(0, 0, 0);
	vist->color(color);

	defaultLegend_.setVisual(visp->copy(), TePOLYGONS);		
	defaultLegend_.setVisual(visl->copy(), TeLINES);		
	defaultLegend_.setVisual(vispt->copy(), TePOINTS);		
	defaultLegend_.setVisual(vist->copy(), TeTEXT);		
	
	color.init(80, 240, 100);
	visp->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);
	pointingLegend_.setVisual(visp->copy(), TePOLYGONS);		
	pointingLegend_.setVisual(visl->copy(), TeLINES);		
	pointingLegend_.setVisual(vispt->copy(), TePOINTS);		
	pointingLegend_.setVisual(vist->copy(), TeTEXT);	

	color.init(210, 210, 0);
	visp->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);		
	queryLegend_.setVisual(visp->copy(), TePOLYGONS);		
	queryLegend_.setVisual(visl->copy(), TeLINES);		
	queryLegend_.setVisual(vispt->copy(), TePOINTS);		
	queryLegend_.setVisual(vist->copy(), TeTEXT);	
							
	color.init(255, 255, 0);
	visp->color(color);
	color.init(100, 100, 100);
	visp->contourColor(color);		
	queryAndPointingLegend_.setVisual(visp->copy(), TePOLYGONS);	
	queryAndPointingLegend_.setVisual(visl->copy(), TeLINES);		
	queryAndPointingLegend_.setVisual(vispt->copy(), TePOINTS);		
	queryAndPointingLegend_.setVisual(vist->copy(), TeTEXT);

	delete visp;
	delete visl;
	delete vispt;
	delete vist;
}

//! Copy constructor
TeAbstractTheme::TeAbstractTheme (const TeAbstractTheme& other) : 
	TeViewNode(other.viewNodeParams_) 
{
	generateAttributeRest_ = other.generateAttributeRest_;
	generateTemporalRest_ = other.generateTemporalRest_;
	generateSpatialRest_ = other.generateSpatialRest_; 
	spatialRelation_ = other.spatialRelation_;
	hasSpatialRes_ =  other.hasSpatialRes_;
	boxRest_ = other.boxRest_;
	geomRest_ = other.geomRest_; 
	geomRepRest_ = other.geomRepRest_;	
	minScale_ = other.minScale_;
	maxScale_ = other.maxScale_;
	visibleRep_ = other.visibleRep_;
	enableVisibility_ = other.enableVisibility_;

	grouping_ = other.grouping_;

	cleanLegend();
	legend_.resize(other.legend_.size());
	copy(other.legend_.begin(),other.legend_.end(),legend_.begin());
	outOfCollectionLegend_ = other.outOfCollectionLegend_;
	withoutDataConnectionLegend_ = other.withoutDataConnectionLegend_;	
	defaultLegend_ = other.defaultLegend_;
	pointingLegend_ = other.pointingLegend_;
	queryLegend_ = other.queryLegend_;
	queryAndPointingLegend_ = other.queryAndPointingLegend_; 
	
	objectSet_ = other.objectSet_;
	numLayerObjects_ = other.numLayerObjects_;
	itemStatusMap_ = other.itemStatusMap_;
	objStatusMap_ = other.objStatusMap_;
	rasterVisual_ = 0;
	if(other.rasterVisual_)
	{
		rasterVisual_ = new TeRasterTransform();
		(*rasterVisual_) = (*other.rasterVisual_);
	}
	themeBox_ = other.themeBox_;
}

//! Destructor
TeAbstractTheme::~TeAbstractTheme () 
{ 
	if (rasterVisual_)
		delete rasterVisual_;
	generateAttributeRest_.clear();
	generateTemporalRest_.clear();
	generateSpatialRest_.clear();
	cleanLegend();
	outOfCollectionLegend_.clear();
	withoutDataConnectionLegend_.clear();
	defaultLegend_.clear(); 
	pointingLegend_.clear(); 
	queryLegend_.clear(); 
	queryAndPointingLegend_.clear();
}

TeAbstractTheme& 
TeAbstractTheme::operator= (const TeAbstractTheme& other)
{
	if ( this != &other )
	{
		viewNodeParams_ = other.viewNodeParams_;
		
		generateAttributeRest_ = other.generateAttributeRest_;
		generateTemporalRest_ = other.generateTemporalRest_;
		generateSpatialRest_ = other.generateSpatialRest_; 
		spatialRelation_ = other.spatialRelation_;
		hasSpatialRes_ =  other.hasSpatialRes_;
		boxRest_ = other.boxRest_;
		geomRest_ = other.geomRest_; 
		geomRepRest_ = other.geomRepRest_;	
		minScale_ = other.minScale_;
		maxScale_ = other.maxScale_;
		visibleRep_ = other.visibleRep_;
		enableVisibility_ = other.enableVisibility_;

		grouping_ = other.grouping_;

		cleanLegend();
		legend_.resize(other.legend_.size());
		copy(other.legend_.begin(),other.legend_.end(),legend_.begin());
		outOfCollectionLegend_ = other.outOfCollectionLegend_;
		withoutDataConnectionLegend_ = other.withoutDataConnectionLegend_;	
		defaultLegend_ = other.defaultLegend_;
		pointingLegend_ = other.pointingLegend_;
		queryLegend_ = other.queryLegend_;
		queryAndPointingLegend_ = other.queryAndPointingLegend_; 
		
		objectSet_ = other.objectSet_;
		numLayerObjects_ = other.numLayerObjects_;
		itemStatusMap_ = other.itemStatusMap_;
		objStatusMap_ = other.objStatusMap_;
	
		if(rasterVisual_)
			delete rasterVisual_;
		rasterVisual_ = 0;
		if(other.rasterVisual_)
		{
			rasterVisual_ = new TeRasterTransform();
			(*rasterVisual_) = (*other.rasterVisual_);
		}
		themeBox_ = other.themeBox_;
	}
	return *this;
}

int TeAbstractTheme::visibleGeoRep()
{
	return (visibleRep_ & ~0x40000000 & ~0x80000000 & ~0x20000000);
}

void TeAbstractTheme::removeRasterVisual()
{ 
	if (rasterVisual_)
	{
		delete rasterVisual_;
		rasterVisual_ = 0;
	}
}

void TeAbstractTheme::cleanLegend()
{
   while (legend_.size())     
   {
       legend_[0].clear();
       legend_.erase(legend_.begin());
   }
   legend_.clear(); 
}

void 
TeAbstractTheme::setSpatialRest(TeBox& box, TeGeomRep rep, TeSpatialRelation relation)
{
	hasSpatialRes_ = true;
	boxRest_ = box;
	spatialRelation_ = relation;
	geomRepRest_ = rep;
	geomRest_ = 0;
}

void 
TeAbstractTheme::setSpatialRest(TeGeometry* geom, TeGeomRep rep, TeSpatialRelation relation)
{
	hasSpatialRes_ = true;
	geomRest_ = geom;
	spatialRelation_ = relation;
	geomRepRest_ = rep;
	boxRest_ = TeBox();
}

void TeAbstractTheme::legend (TeLegendEntry& leg)
{
	if(leg.group() == -6)	// queried and pointed objects visual
		queryAndPointingLegend_ = leg;
	else if(leg.group() == -5)	// queried objects visual
		queryLegend_ = leg;
	else if (leg.group() == -4)
		pointingLegend_ = leg;
	else if (leg.group() == -3)
		defaultLegend_ = leg;
	else if (leg.group() == -2)
		withoutDataConnectionLegend_ = leg;
	else if (leg.group() == -1)
		outOfCollectionLegend_ = leg;
	else if (leg.group() > -1)
		legend_.push_back (leg);
}

void 
TeAbstractTheme::resetGrouping ()
{
	if((grouping_.groupMode_ == TeRasterSlicing) && rasterVisual_)
			this->removeRasterVisual();

	grouping_.groupMode_ = TeNoGrouping;
	legend_.clear();
	return;
}

bool 
TeAbstractTheme::buildGrouping(const TeGrouping& g, vector<TeSlice>& slices)
{
	grouping_ = g;
	legend_.clear(); 
	
	for(unsigned int j=0; j<slices.size(); j++)
	{
		TeLegendEntry legend(slices[j]);
		legend.group(j);
		legend.theme(id());
		legend_.push_back(legend);
	}	
	return true;
}

bool
TeAbstractTheme::setGroupingVisual(int n, TeVisual* visual, TeGeomRep rep)
{
	if(	(n > grouping_.groupNumSlices_)	||
		((int)legend_.size() < n)				|| 
		(legend_.empty()) )
		return false;
	legend_[(n-1)].setVisual (visual->copy(), rep); 
	return true;
}

bool
TeAbstractTheme::setGroupingVisual(int n, TeGeomRepVisualMap& vismap)
{
	if(	(n > grouping_.groupNumSlices_)	||
		((int)legend_.size() < n)		|| 
		(legend_.empty()) )
		return false;

	TeGeomRepVisualMap::iterator it = vismap.begin();
	while (it != vismap.end())
	{
		legend_[(n-1)].setVisual(it->second->copy(),it->first);
		++it;
	}
	return true;
}

TeSliceVector 
TeAbstractTheme::getSlices()
{
	TeSliceVector sliceVec;
	for(unsigned int x=0; x<legend_.size(); ++x)
	{
		TeSlice slice = legend_[x].slice();
		sliceVec.push_back (slice);
	}
	return sliceVec;
}



void TeAbstractTheme::createRasterVisual(TeRaster* rst)
{
	if (rasterVisual_)
		delete rasterVisual_;

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

void TeAbstractTheme::setParent (TeViewNode* )
{
	viewNodeParams_.myParent_ = 0; 
	viewNodeParams_.myParentId_ = 0;
}

void TeAbstractTheme::setStatus(vector<string>& oidVec, vector<string>& itemVec, int status)
{
	vector<string>::iterator it;

	if(status == 0)
	{
		for(it=oidVec.begin(); it!=oidVec.end();++it)
		{
			string s = *it;
			if(objStatusMap_.find(s) != objStatusMap_.end())
				objStatusMap_.erase(s);
		}
		for(it=itemVec.begin(); it!=itemVec.end();++it)
		{
			string s = *it;
			if(itemStatusMap_.find(s) != itemStatusMap_.end())
				itemStatusMap_.erase(s);
		}
	}
	else
	{
		for(it=oidVec.begin(); it!=oidVec.end();++it)
		{
			string s = *it;
			objStatusMap_[s] = status;
		}
		for(it=itemVec.begin(); it!=itemVec.end();++it)
		{
			string s = *it;
			itemStatusMap_[s] = status;
		}
	}
}

void TeAbstractTheme::setStatusForObjectToggled(string oid)
{
	string uid;

	int& oidStatus = objStatusMap_[oid];
	if (oidStatus == TeDEFAULT)
		oidStatus = TePOINTED;
	else if (oidStatus == TePOINTED)
		oidStatus = TeDEFAULT;
	else if (oidStatus == TeQUERIED)
		oidStatus = TePOINTED_QUERIED;
	else if (oidStatus == TePOINTED_QUERIED)
		oidStatus = TeQUERIED;

	set<string> oidSet;
	oidSet.insert(oid);
	vector<string> uidVec = getItemVector(oidSet);
	for (unsigned int i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		int& uidStatus = itemStatusMap_[uidVec[i]];
		if (uidStatus == TeDEFAULT)
		{
			if (oidStatus == TePOINTED || oidStatus == TePOINTED_QUERIED)
				uidStatus = TePOINTED;
		}
		else if (uidStatus == TePOINTED)
		{
			if (oidStatus == TeDEFAULT || oidStatus == TeQUERIED)
				itemStatusMap_.erase(uid);
		}
		else if (uidStatus == TeQUERIED)
		{
			if (oidStatus == TePOINTED || oidStatus == TePOINTED_QUERIED)
				uidStatus = TePOINTED_QUERIED;
		}
		else if (uidStatus == TePOINTED_QUERIED)
		{
			if (oidStatus == TeDEFAULT || oidStatus == TeQUERIED)
				uidStatus = TeQUERIED;
		}
	}
	
	// Save the status id of the object
	if (oidStatus == TeDEFAULT)
		objStatusMap_.erase(oid);
}

void TeAbstractTheme::setStatusForItemsToggled(set<string>& oidSet, vector<string>& uidVec)
{
	unsigned int i;
	int oidStatus;
	string oid, uid;
	set<string>::iterator it;

	// Set the new status of the uids
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TeDEFAULT)
			itemStatusMap_[uid] = TePOINTED;
		else if (itemStatusMap_[uid] == TePOINTED)
			itemStatusMap_.erase(uid);
		else if (itemStatusMap_[uid] == TeQUERIED)
			itemStatusMap_[uid] = TePOINTED_QUERIED;
		else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			itemStatusMap_[uid] = TeQUERIED;
	}


	// Set the new status of the oids
	for (it = oidSet.begin(); it != oidSet.end(); ++it)
	{
		oid = *it;
		oidStatus = 0;
		set<string> objSet;
		objSet.insert(oid);
		vector<string> uidVec = getItemVector(objSet);
		for (i = 0; i < uidVec.size(); ++i)
		{
			uid = uidVec[i];
			if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			{
				oidStatus = TePOINTED_QUERIED;
				break;
			}
			oidStatus = MAX(oidStatus, itemStatusMap_[uid]);
			if (itemStatusMap_[uid] == TeDEFAULT)
				itemStatusMap_.erase(uid);
		}	
		
		if (oidStatus == 0)
			objStatusMap_.erase(oid);
		else
			objStatusMap_[oid] = oidStatus;
	}
}

void TeAbstractTheme::setStatusForNewItemsQueried(set<string>& oidSet, vector<string>& uidVec)
{
	unsigned int i;
	string oid, uid;
	vector<string> prevOidQVec;		// previous oids queried
	vector<string> notInOidSetVec;	// oid is not in oidSet
	vector<string> defaultVec;		// vector with oids or uids to be set as default
	map<string, int>::iterator mapIt;
	set<string>::iterator setIt;

	// Get the previous oids queried
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TeQUERIED || objStatusMap_[oid] == TePOINTED_QUERIED)
			prevOidQVec.push_back(mapIt->first);
	}

	// Check if the previous oids queried are in the oidSet; in positive case, remove
	// them from the oidSet; in negative case, put them in the vector notInOidSetVec
	for (i = 0; i < prevOidQVec.size(); ++i)
	{
		oid = prevOidQVec[i];
		if (oidSet.find(oid) != oidSet.end())			
			oidSet.erase(oid);				// object is in the input oidSet
		else
			notInOidSetVec.push_back(oid);	// object is not in the input oidSet
	}

	// For the objects in oidSet, set the queried status for them
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
			objStatusMap_[oid] = TeQUERIED;
		else if (objStatusMap_[oid] == TePOINTED)
			objStatusMap_[oid] = TePOINTED_QUERIED;
	}

	// For the objects not in oidSet, insert it in oidSet, and remove their queried status
	for (i = 0; i < notInOidSetVec.size(); ++i)
	{
		oid = notInOidSetVec[i];
		if (objStatusMap_[oid] == TeQUERIED)
			defaultVec.push_back(oid);
		else if (objStatusMap_[oid] == TePOINTED_QUERIED)
			objStatusMap_[oid] = TePOINTED;
		
		oidSet.insert(oid);
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Remove the queried status for the uids that were previously queried
	defaultVec.clear();
	for (mapIt = itemStatusMap_.begin(); mapIt != itemStatusMap_.end(); ++mapIt)
	{
		uid = mapIt->first;
		if (itemStatusMap_[uid] == TeQUERIED)
			defaultVec.push_back(uid);
		else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			itemStatusMap_[uid] = TePOINTED;			
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);

	// Set the queried status for the input uidVec
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TeDEFAULT)
			itemStatusMap_[uid] = TeQUERIED;
		else if (itemStatusMap_[uid] == TePOINTED)
			itemStatusMap_[uid] = TePOINTED_QUERIED;
	}
}

void TeAbstractTheme::setStatusForNewObjectsPointed(set<string>& oidSet)
{
	unsigned int i;
	string oid, uid;
	set<string> prevOidPointedSet;		// previous oids pointed
	set<string> objInInputOidSet;		// set containing objects in oidSet
	set<string> objNotInInputOidSet;	// set containing objects not in oidSet
	vector<string> defaultVec;		    // vector with oids or uids to be set as default
	vector<string> uidVec;				// vector of uids
	map<string, int>::iterator mapIt;
	set<string>::iterator setIt;

	// Get the previous oids pointed
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TePOINTED || objStatusMap_[oid] == TePOINTED_QUERIED)
			prevOidPointedSet.insert(mapIt->first);
	}

	// If the set of objects is empty, remove the pointed status of the previous oids and uids
	if (oidSet.empty())
	{
		for (setIt = prevOidPointedSet.begin(); setIt != prevOidPointedSet.end(); ++setIt)
		{
			oid = *setIt;
			if (objStatusMap_[oid] == TePOINTED)
				defaultVec.push_back(oid);
			else if (objStatusMap_[oid] == TePOINTED_QUERIED)
				objStatusMap_[oid] = TeQUERIED;

			oidSet.insert(oid);
		}

		for (i = 0; i < defaultVec.size(); ++i)
			objStatusMap_.erase(defaultVec[i]);

		defaultVec.clear();
//		uidVec = getUidVec(prevOidPointedVec.begin(), prevOidPointedVec.end(), this); 
		uidVec = getItemVector(prevOidPointedSet); 
		for (i = 0; i < uidVec.size(); ++i)
		{
			uid = uidVec[i];
			if (itemStatusMap_[uid] == TePOINTED)
				defaultVec.push_back(uid);
			else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
				itemStatusMap_[uid] = TeQUERIED;
		}

		for (i = 0; i < defaultVec.size(); ++i)
			itemStatusMap_.erase(defaultVec[i]);

		return;
	}

	// Check if the previous oids pointed are in the oidSet; in positive case, put
	// them in objInOidSetVec; in negative case, put them in the objNotInOidSetVec
	for (setIt = prevOidPointedSet.begin(); setIt != prevOidPointedSet.end(); ++setIt)
	{
		oid = *setIt;
		if (oidSet.find(oid) != oidSet.end())
			objInInputOidSet.insert(oid);		// object is in the input oidSet
		else
			objNotInInputOidSet.insert(oid);	// object is not in the input oidSet
	}

	// For the objects in oidSet, set the pointed status for them
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
			objStatusMap_[oid] = TePOINTED;
		else if (objStatusMap_[oid] == TeQUERIED)
			objStatusMap_[oid] = TePOINTED_QUERIED;
	}

	// For the objects not in oidSet, remove their pointed status
	defaultVec.clear();
	for (setIt = objNotInInputOidSet.begin(); setIt != objNotInInputOidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TePOINTED)
			defaultVec.push_back(oid);
		else if (objStatusMap_[oid] == TePOINTED_QUERIED)
			objStatusMap_[oid] = TeQUERIED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// For the objects in oidSet, set the pointed status for their uids
	uidVec = getItemVector(oidSet);
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TeDEFAULT)
			itemStatusMap_[uid] = TePOINTED;
		else if (itemStatusMap_[uid] == TeQUERIED)
			itemStatusMap_[uid] = TePOINTED_QUERIED;
	}

	// For the objects not in oidSet, remove the pointed status for their uids
	defaultVec.clear();
	uidVec = getItemVector(objNotInInputOidSet);
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TePOINTED)
			defaultVec.push_back(uid);
		else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			itemStatusMap_[uid] = TeQUERIED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);

	// Insert in the oidSet the objects that are not in the input oidSet
	for (setIt = objNotInInputOidSet.begin(); setIt != objNotInInputOidSet.end(); ++setIt)
		oidSet.insert(*setIt);

	// Remove from the oidSet the objects that were in the input oidSet
	for (setIt = objInInputOidSet.begin(); setIt != objInInputOidSet.end(); ++setIt)
		oidSet.erase(*setIt);
}

void TeAbstractTheme::setStatusForNewItemsPointed(vector<string>& itemVec)
{
	unsigned int i;
	string oid, item;
	map<string, int>::iterator mapIt;
	vector<string> defaultVec;

	//-----------------------------------------------------------------------
	// Set the pointing status for the input items
	//-----------------------------------------------------------------------

	// Remove the pointing status for all the items
	for (mapIt = itemStatusMap_.begin(); mapIt != itemStatusMap_.end(); ++mapIt)
	{
		item = mapIt->first;
		if (itemStatusMap_[item] == TePOINTED)
			defaultVec.push_back(item);
		else if (objStatusMap_[item] == TePOINTED_QUERIED)
			itemStatusMap_[item] = TeQUERIED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);

	for (i = 0; i < itemVec.size(); ++i)
	{
		item = itemVec[i];
		if (itemStatusMap_[item] == TeDEFAULT)
			itemStatusMap_[item] = TePOINTED;
		else if (itemStatusMap_[item] == TeQUERIED)
			itemStatusMap_[item] = TePOINTED_QUERIED;	
	}

	//-----------------------------------------------------------------------
	// Set the pointing status for the objects associated to the input items
	//-----------------------------------------------------------------------
	
	// Remove the pointing status for all the objects
	defaultVec.clear();
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TePOINTED)
			defaultVec.push_back(oid);
		else if (objStatusMap_[oid] == TePOINTED_QUERIED)
			objStatusMap_[oid] = TeQUERIED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Set the pointing status for the objects associated to the input items
	set<string> oidSet = getObjects(itemVec);
	set<string>::iterator setIt;
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
			objStatusMap_[oid] = TePOINTED;
		else if (objStatusMap_[oid] == TeQUERIED)
			objStatusMap_[oid] = TePOINTED_QUERIED;	
	}
}



void TeAbstractTheme::setStatusForObjectsAddedByPointing(set<string>& oidSet)
{
	if (oidSet.empty())
		return;

	unsigned int i;
	string oid, uid;
	vector<string> prevOidPointedVec;	// previous oids pointed
	vector<string> uidVec;				// vector of uids
	map<string, int>::iterator mapIt;
	set<string>::iterator setIt;

	// Get the previous oids pointed
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TePOINTED || objStatusMap_[oid] == TePOINTED_QUERIED)
			prevOidPointedVec.push_back(mapIt->first);
	}

	// If there is any object in the oidSet that is already in the previous oids pointed,
	// remove it from the oidSet
	for (i = 0; i < prevOidPointedVec.size(); ++i)
	{
		oid = prevOidPointedVec[i];
		if (oidSet.find(prevOidPointedVec[i]) != oidSet.end())
			oidSet.erase(oid);
	}

	// Set the pointed status for the objects added by pointing
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
			objStatusMap_[oid] = TePOINTED;
		if (objStatusMap_[oid] == TeQUERIED)
			objStatusMap_[oid] = TePOINTED_QUERIED;
	}

	// Set the pointed status for the uids of the objects added by pointing
	uidVec = getItemVector(oidSet);
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TeDEFAULT)
			itemStatusMap_[uid] = TePOINTED;
		else if (itemStatusMap_[uid] == TeQUERIED)
			itemStatusMap_[uid] = TePOINTED_QUERIED;
	}
}

void TeAbstractTheme::setStatusForItemsAddedByPointing(vector<string>& itemVec)
{
	unsigned int i;
	string oid, item;

	//-----------------------------------------------------------------------
	// Set the pointing status for the input items
	//-----------------------------------------------------------------------
	for (i = 0; i < itemVec.size(); ++i)
	{
		item = itemVec[i];
		if (itemStatusMap_[item] == TeDEFAULT)
			itemStatusMap_[item] = TePOINTED;
		else if (objStatusMap_[item] == TeQUERIED)
			itemStatusMap_[item] = TePOINTED_QUERIED;

	}

	//-----------------------------------------------------------------------
	// Set the pointing status for the objects associated to the input items
	//-----------------------------------------------------------------------
	set<string> oidSet = getObjects(itemVec);
	set<string>::iterator setIt;
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
			objStatusMap_[oid] = TePOINTED;
		else if (objStatusMap_[oid] == TeQUERIED)
			objStatusMap_[oid] = TePOINTED_QUERIED;	
	}
}


void TeAbstractTheme::setStatusForItemsAddedByQuerying(set<string>& oidSet, vector<string>& uidVec)
{
	unsigned int i;
	string oid, uid;
	set<string>::iterator setIt;

	// Set the status for the oids in oidSet as "queried"
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
			objStatusMap_[oid] = TeQUERIED;
		else if (objStatusMap_[oid] == TePOINTED)
			objStatusMap_[oid] = TePOINTED_QUERIED;
	}

	// Set the new status for the uidVec
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TeDEFAULT)
			itemStatusMap_[uid] = TeQUERIED;
		else if (itemStatusMap_[uid] == TePOINTED)
			itemStatusMap_[uid] = TePOINTED_QUERIED;
	}
}


void TeAbstractTheme::setStatusForItemsFilteredByQuerying(set<string>& oidSet, vector<string>& uidVec)
{
	unsigned int i;
	string oid, uid;
	vector<string> prevOidQVec;		// previous oids queried
	vector<string> notInOidSetVec;	// oid is not in oidSet
	vector<string> defaultVec;		// vector with oids or uids to be set as default
	map<string, int>::iterator mapIt;
	set<string>::iterator setIt;

	// Get the previous oids queried
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TeQUERIED || objStatusMap_[oid] == TePOINTED_QUERIED)
			prevOidQVec.push_back(mapIt->first);
	}

	// Check if the previous oids queried are in the oidSet; in positive case, remove
	// them from the oidSet; in negative case, put them in the vector notInOidSetVec
	for (i = 0; i < prevOidQVec.size(); ++i)
	{
		oid = prevOidQVec[i];
		if (oidSet.find(oid) != oidSet.end())			
			oidSet.erase(oid);				// object is in the input oidSet
		else
			notInOidSetVec.push_back(oid);	// object is not in the input oidSet
	}

	// For the objects not in oidSet, insert it in oidSet, and remove their queried status
	for (i = 0; i < notInOidSetVec.size(); ++i)
	{
		oid = notInOidSetVec[i];
		if (objStatusMap_[oid] == TeQUERIED)
			defaultVec.push_back(oid);
		else if (objStatusMap_[oid] == TePOINTED_QUERIED)
			objStatusMap_[oid] = TePOINTED;
		
		oidSet.insert(oid);
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Remove the queried status for the uids that were previously queried
	defaultVec.clear();
	for (mapIt = itemStatusMap_.begin(); mapIt != itemStatusMap_.end(); ++mapIt)
	{
		uid = mapIt->first;
		if (itemStatusMap_[uid] == TeQUERIED)
			defaultVec.push_back(uid);
		else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			itemStatusMap_[uid] = TePOINTED;			
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);

	// Set the queried status for the input uidVec
	for (i = 0; i < uidVec.size(); ++i)
	{
		uid = uidVec[i];
		if (itemStatusMap_[uid] == TeDEFAULT)
			itemStatusMap_[uid] = TeQUERIED;
		else if (itemStatusMap_[uid] == TePOINTED)
			itemStatusMap_[uid] = TePOINTED_QUERIED;
	}
}

void TeAbstractTheme::removePointingColor()
{
	unsigned int i;
	string oid, uid;
	vector<string> defaultVec;
	map<string, int>::iterator mapIt;

	// Remove the pointed status for the objects
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TePOINTED)
			defaultVec.push_back(oid);
		else if (objStatusMap_[oid] == TePOINTED_QUERIED)
			objStatusMap_[oid] = TeQUERIED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Remove the pointed status for the uids
	defaultVec.clear();
	for (mapIt = itemStatusMap_.begin(); mapIt != itemStatusMap_.end(); ++mapIt)
	{
		uid = mapIt->first;
		if (itemStatusMap_[uid] == TePOINTED)
			defaultVec.push_back(uid);
		else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			itemStatusMap_[uid] = TeQUERIED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);
}


void TeAbstractTheme::removeQueryColor()
{
	unsigned int i;
	string oid, uid;
	vector<string> defaultVec;
	map<string, int>::iterator mapIt;

	// Remove the pointed status for the objects
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TeQUERIED)
			defaultVec.push_back(oid);
		else if (objStatusMap_[oid] == TePOINTED_QUERIED)
			objStatusMap_[oid] = TePOINTED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Remove the pointed status for the uids
	defaultVec.clear();
	for (mapIt = itemStatusMap_.begin(); mapIt != itemStatusMap_.end(); ++mapIt)
	{
		uid = mapIt->first;
		if (itemStatusMap_[uid] == TeQUERIED)
			defaultVec.push_back(uid);
		else if (itemStatusMap_[uid] == TePOINTED_QUERIED)
			itemStatusMap_[uid] = TePOINTED;
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);
}


void TeAbstractTheme::invertObjectStatus()
{
	unsigned int i;
	string oid, uid;
	set<string> objInvToPointedStatusSet;	// objects to be inverted to the pointed status
	set<string> objInvToDefaultStatusSet;	// objects to be inverted to the default status
	vector<string> defaultVec;
	map<string, int>::iterator mapIt;
	set<string>::const_iterator setIt;

	// Get all the theme objects
	const set<string>& oidSet = getObjects();

	// Get the objects that are pointed, queried, or pointed and queried
	// and put their status to the default status, and get the objects
	// that are in the default status and put them in the pointed status
	for (setIt = oidSet.begin(); setIt != oidSet.end(); ++setIt)
	{
		oid = *setIt;
		if (objStatusMap_[oid] == TeDEFAULT)
		{
			objStatusMap_[oid] = TePOINTED;
			objInvToPointedStatusSet.insert(oid);
		}
		else if (objStatusMap_[oid] == TePOINTED || objStatusMap_[oid] == TeQUERIED ||
			     objStatusMap_[oid] == TePOINTED_QUERIED)
		{
			defaultVec.push_back(oid);
			objInvToDefaultStatusSet.insert(oid);
		}
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Set the status for the uids according the status of their objects
	vector<string> uidVec = getItemVector(objInvToPointedStatusSet);
	for (i = 0; i < uidVec.size(); ++i)
		itemStatusMap_[uidVec[i]] = TePOINTED;

	uidVec = getItemVector(objInvToDefaultStatusSet);
	for (i = 0; i < uidVec.size(); ++i)
		itemStatusMap_.erase(uidVec[i]);
}


void TeAbstractTheme::setObjectsToDefaultStatus()
{
	unsigned int i;
	string oid, uid;
	vector<string> defaultVec;
	map<string, int>::iterator mapIt;

	// Set the default status for objects that are pointed, queried, or pointed and queried
	for (mapIt = objStatusMap_.begin(); mapIt != objStatusMap_.end(); ++mapIt)
	{
		oid = mapIt->first;
		if (objStatusMap_[oid] == TePOINTED || objStatusMap_[oid] == TeQUERIED ||
			objStatusMap_[oid] == TePOINTED_QUERIED)
			defaultVec.push_back(oid);
	}

	for (i = 0; i < defaultVec.size(); ++i)
		objStatusMap_.erase(defaultVec[i]);

	// Set the default status for uids that are pointed, queried, or pointed and queried
	defaultVec.clear();
	for (mapIt = itemStatusMap_.begin(); mapIt != itemStatusMap_.end(); ++mapIt)
	{
		uid = mapIt->first;
		if (itemStatusMap_[uid] == TePOINTED || itemStatusMap_[uid] == TeQUERIED ||
			itemStatusMap_[uid] == TePOINTED_QUERIED)
			defaultVec.push_back(uid);
	}

	for (i = 0; i < defaultVec.size(); ++i)
		itemStatusMap_.erase(defaultVec[i]);
}





