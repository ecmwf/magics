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

#include "TeSTEvent.h"

///------------ TeSTEvent
void 
TeSTEvent::clear()
{
	TeBaseSTInstance<TePoint, long>::clear();
	geometries_ = TePoint(); 
	time_ = -1;
}

bool 
TeSTEvent::isTimeValid() 
{
	return (time_>=0);
}

///------------ TeSTEventSet

TeSTEventSet::TeSTEventSet(TeTheme* theme, const TeAttributeList& attrList) : 
	TeBaseSTInstanceSet<TePoint, long, TeSTEvent>(theme, attrList)
{
	kdTree_ = 0;
}

TeSTEventSet::~TeSTEventSet()
{
	if(kdTree_)
		delete kdTree_;
	kdTree_ = 0;
}

bool TeSTEventSet::buildKdTree(const int& n)
{
	if(!theme_)
		return false;
    if(kdTree_)
		delete kdTree_;

	vector< std::pair<TeCoord2D, TePoint> >	dataSetAux; //auxiliary structure for kdTree
	TeSTEventSet::iterator it = this->begin();
	int index=0;
	while(it!=this->end())
	{
		TePoint point = it->getGeometries();
		point.geomId(index);
		dataSetAux.push_back(pair<TeCoord2D, TePoint>(point.location(), point));
		++it;
		++index;
	}

	int bucketSize; 
	if(n<0)
		bucketSize= 30;
	else
		bucketSize= 2*n;
	kdTree_ = new kdTree(theme_->box(), bucketSize);
	kdTree_->build(dataSetAux);
	return true;
}

bool TeSTEventSet::search(const TeBox& b, vector<TeSTEvent* >& result)
{
	if(!kdTree_)
		return false;
	
	vector<kdNode*> nodes;
	kdTree_->search(b, nodes);
	for(unsigned int i=0; i<nodes.size(); ++i)
	{
		for(unsigned int j=0; j<nodes[i]->getData().size(); ++j) //vector<TePoint>
		{
			int index = ((nodes[i]->getData())[j]).geomId(); 
			TeSTEvent* ev = dynamic_cast<TeSTEvent*> (this->getSTInstance(index));
			result.push_back(ev); 
		}
	}
	return true;
}

bool TeSTEventSet::nearestNeighbourSearch(const TeCoord2D& coord, vector<TeSTEvent* >& result, 
										  vector<double>& distances, const unsigned int& k)
{
	vector<TePoint> res;
	for(unsigned int n=0; n<k; ++n)
		res.push_back (TePoint(TeMAXFLOAT,TeMAXFLOAT));
	kdTree_->nearestNeighborSearch(coord, res, distances, k); 
	for(unsigned int j=0; j<res.size(); ++j)
		result.push_back(dynamic_cast<TeSTEvent*>(getSTInstance(res[j].geomId()))); 
	return true;
}

void TeSTEventSet::clear()
{
	TeBaseSTInstanceSet<TePoint, long, TeSTEvent>::clear();
	if(kdTree_)
		delete kdTree_;
	kdTree_ = 0;
}

bool 
TeSTEventSet::buildImpl(TeQuerier* querier, const int& slide)
{
	int dt = CLOCKS_PER_SEC/4, steps = 0;
	int dt2 = CLOCKS_PER_SEC * 5;
	clock_t	t0, t1, t2;
	
	t0=t1=t2=clock();

	if(!querier)
		return false;

	if(!querier->loadInstances(slide))
		return false;

	//clear all structures
	attrList_->clear();
	instances_.clear();
	objectIdToInstances_.clear();
	timeToInstances_.clear();
	sliceToInstances_.clear();
	if(rTree_)
		delete rTree_;
	rTree_ = 0; 
	kdTree_->clear();
	
	//Builts another rTree
	TeBox b;
	if(theme_)
		b = theme_->box();
	else if(layer_)
		b = layer_->box();
	else
		b = this->box_; 
	rTree_ = new TeSAM::TeRTree<int>(b); 
	
	TeAttributeList l = querier->getAttrList();
	setAttributeList(l);

	int tot = querier->numElemInstances();
	if(TeProgress::instance())
	{
		string caption = "Building data";
		TeProgress::instance()->setCaption(caption.c_str());
		string msg = "Building in progress. Please, wait!";
		TeProgress::instance()->setMessage(msg);
		TeProgress::instance()->setTotalSteps(tot);
		t2 = clock();
		t0 = t1 = t2;
	}
	
	TeSTInstance obj;
	while(querier->fetchInstance(obj)) 
	{
		TeCoord2D p;
		obj.centroid(p);

		TeSTEvent event(obj.getObjectId(), TePoint(p), obj.getProperties(), 0, -1);
		this->insertSTInstance(event);

		if(TeProgress::instance())
		{
			steps++;
			t2 = clock();
			if (int(t2-t1) > dt)
			{
				t1 = t2;
				if(TeProgress::instance()->wasCancelled())
					return false;
				
				if((int)(t2-t0) > dt2)
					TeProgress::instance()->setProgress(steps);
			}
		}
	}
	if(TeProgress::instance())
		TeProgress::instance()->reset();

	return true;
}




