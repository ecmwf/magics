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

#include "TeQuerierImpl.h"
#include "TeTemporalSeries.h"

	
TeQuerierImpl::TeQuerierImpl(TeQuerierParams* par): params_(par), filledSerie_(false)
{
	TeAttributeList* att = new TeAttributeList();
	attrList_ = TeSharedPtr<TeAttributeList>(att);
	ts_ = 0;
	if(par->chronon()!=TeNOCHRONON) //if there is a chronon, build ts
	{
		TeGroupingAttr::iterator it = par->groupAttr().begin();
		if(it!=par->groupAttr().end())
			ts_ = new TeTemporalSeries(par->theme(), par->objId(), par->chronon(), it->first.name_, it->first.name_, it->second, it->second);
		else
			ts_ = new TeTemporalSeries(par->chronon(), par->theme());
		ts_->buildFrameIntervals();
	}
}

TeQuerierImpl::TeQuerierImpl( const TeQuerierImpl& other)
{
	if(ts_)
		delete ts_;

	if(params_)
		delete params_;

	ts_ = 0;
	params_ = 0;
	if(other.params_) 
	{
		params_ = new TeQuerierParams();
		*params_ = *(other.params_);
	}
	filledSerie_ = other.filledSerie_;
	attrList_ = other.attrList_;
}

TeQuerierImpl& 
TeQuerierImpl::operator= (const TeQuerierImpl& other)
{
	if ( this != &other )
	{
		if(ts_)
			delete ts_;

		if(params_)
			delete params_;

		ts_ = 0;
		params_ = 0;
		if(other.params_) 
		{
			params_ = new TeQuerierParams();
			*params_ = *(other.params_);
		}
		filledSerie_ = other.filledSerie_;
		attrList_ = other.attrList_;
	}
	return *this;
}

TeQuerierImpl::~TeQuerierImpl() 
{	
	if(ts_)
		delete (ts_);

	if(params_)
		delete params_;
}

bool 
TeQuerierImpl::loadTimeFrameInstances(int frame)
{
	if(frame>-1) 
	{
		TeTSEntry ent;
		if(!getTSEntry(ent, frame))
			return false;
		
		return(loadInstances(&ent));
	}
	else
		return(loadInstances());
}

bool 
TeQuerierImpl::getTSEntry(TeTSEntry& tsEntry, int frame)
{
	if(!ts_)
		return false;

	return(ts_->getTSEntry(tsEntry, frame)); 
}

int 
TeQuerierImpl::getNumTimeFrames()
{
	if(!ts_)
		return 0;

	return (ts_->numTimeFrames());
}

bool 
TeQuerierImpl::getTS(TeTemporalSeries& ts)
{
	if(!ts_)
		return false;
	
	ts = (*ts_);
	return true;
}

TeTSParams& 
TeQuerierImpl::getTSParams() 
{ 
	return (ts_->TSparams_); 
}

