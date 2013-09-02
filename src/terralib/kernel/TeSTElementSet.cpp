/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include <TeSTElementSet.h>
#include <TeQuerier.h> 
#include <TeQuerierParams.h>

TeTimeInterval 
TeSTElementSet::totalTime() 
{	
	return TeTimeInterval(this->getMinTime().getT1(),this->getMaxTime().getT2());
} 

void 
TeSTElementSet::totalTime(TeTimeInterval t) 
{	
	minTime_ = TeTimeInterval(t.getT1(), t.getT1());
	maxTime_ = TeTimeInterval(t.getT2(), t.getT2());
}

	
bool 
TeSTElementSet::hasElement(const string& objId) 
{	
	return (numSTInstance(objId)>0);  
}

bool 
TeSTElementSet::setGeometry(const string& object_id, const TePolygonSet& geomSet, TeTimeInterval time)
{
	vector<TeSTInstance*> result;
	this->getSTInstances(result, object_id, time);
	vector<TeSTInstance*>::iterator it = result.begin();

	while(it!=result.end())
	{
		(*it)->setGeometry(geomSet);
		++it;
	}
	return true;
}

bool 
TeSTElementSet::setGeometry(const string& object_id, const TeLineSet& geomSet, TeTimeInterval time)
{
	vector<TeSTInstance*> result;
	this->getSTInstances(result, object_id, time);
	vector<TeSTInstance*>::iterator it = result.begin();

	while(it!=result.end())
	{
		(*it)->setGeometry(geomSet);
		++it;
	}
	return true;
}

bool 
TeSTElementSet::setGeometry(const string& object_id, const TePointSet& geomSet, TeTimeInterval time)
{
	vector<TeSTInstance*> result;
	this->getSTInstances(result, object_id, time);
	vector<TeSTInstance*>::iterator it = result.begin();

	while(it!=result.end())
	{
		(*it)->setGeometry(geomSet);
		++it;
	}
	return true;
}

bool 
TeSTElementSet::setGeometry(const string& object_id, const TeCellSet& geomSet, TeTimeInterval time)
{
	vector<TeSTInstance*> result;
	this->getSTInstances(result, object_id, time);
	vector<TeSTInstance*>::iterator it = result.begin();

	while(it!=result.end())
	{
		(*it)->setGeometry(geomSet);
		++it;
	}
	return true;
}

bool 
TeSTElementSet::setGeometry(const string& object_id, const TeTextSet& geomSet, TeTimeInterval time)
{
	vector<TeSTInstance*> result;
	this->getSTInstances(result, object_id, time);
	vector<TeSTInstance*>::iterator it = result.begin();

	while(it!=result.end())
	{
		(*it)->setGeometry(geomSet);
		++it;
	}
	return true;
}


bool 
TeSTElementSet::getGeometry(const string& object_id, TePolygonSet& geomSet, TeTimeInterval time)
{
	TeSTInstance* result = this->getSTInstance(object_id, time);
	if(!result)
		return false; 
	geomSet = result->getPolygons();
	return true;
}


bool 
TeSTElementSet::getGeometry(const string& object_id, TeLineSet& geomSet, TeTimeInterval time)
{
	TeSTInstance* result = this->getSTInstance(object_id, time);
	if(!result)
		return false; 
	geomSet = result->getLines();
	return true;
}

	
bool 
TeSTElementSet::getGeometry(const string& object_id, TePointSet& geomSet, TeTimeInterval time)
{
	TeSTInstance* result = this->getSTInstance(object_id, time);
	if(!result)
		return false; 
	geomSet = result->getPoints();
	return true;
}

	
bool 
TeSTElementSet::getGeometry(const string& object_id, TeCellSet& geomSet, TeTimeInterval time)
{
	TeSTInstance* result = this->getSTInstance(object_id, time);
	if(!result)
		return false; 
	geomSet = result->getCells();
	return true; 
}

bool 
TeSTElementSet::getGeometry(const string& object_id, TeTextSet& geomSet, TeTimeInterval time)
{
	TeSTInstance* result = this->getSTInstance(object_id, time);
	if(!result)
		return false; 
	geomSet = result->getTexts();
	return true;
}

bool 
TeSTElementSet::buildImpl(TeQuerier* querier, const int& slide)
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
		TeProgress::instance()->reset();
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
		this->insertSTInstance(obj);
		
		obj.clear();
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


