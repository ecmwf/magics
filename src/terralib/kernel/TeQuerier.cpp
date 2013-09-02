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

#include "TeQuerier.h"
#include "TeQuerierImpl.h"


TeQuerier::TeQuerier(TeQuerierParams& params)
{
	Impl_ = TeQuerierImplFactory::make(params);
}

TeQuerier::TeQuerier(const TeQuerier& other)
{
	if(Impl_)
		delete Impl_;

	Impl_ = 0;
	if(other.Impl_ && other.Impl_->params())
	{
		Impl_ = TeQuerierImplFactory::make(*(other.Impl_->params()));
	}
}

TeQuerier&
TeQuerier::operator=(const TeQuerier& other)
{
	if ( this != &other )
	{
		if(Impl_)
			delete Impl_;

		Impl_ = 0;
		if(other.Impl_ && other.Impl_->params())
		{
			Impl_ = TeQuerierImplFactory::make(*(other.Impl_->params()));
		}
	}
	return *this;
}

TeQuerier::~TeQuerier()
{
	if(Impl_)
		delete (Impl_);
}

bool 
TeQuerier::loadInstances(int frame)
{
	if(!Impl_)
		return false;

	return(Impl_->loadTimeFrameInstances(frame));
}


bool 
TeQuerier::fetchInstance(TeSTInstance&  sto)
{
	if(!Impl_)
		return false;

	return(Impl_->fetchInstance(sto));
}


int 
TeQuerier::getNumTimeFrames()
{
	if(!Impl_) 
		return false;

	return (Impl_->getNumTimeFrames());
}

bool 
TeQuerier::getTSEntry(TeTSEntry& tsEntry, int frame)
{
	if(!Impl_) 
		return false;

	return(Impl_->getTSEntry(tsEntry, frame)); 
}


bool 
TeQuerier::getTS(TeTemporalSeries& ts)
{
	if(!Impl_) 
		return false;

	return (Impl_->getTS(ts));
}

TeTheme* 
TeQuerier::theme()
{
	if(!Impl_)
		return 0;

	return(Impl_->theme());
}


int 
TeQuerier::numElemInstances()
{
	if(!Impl_)
		return 0;

	return (Impl_->numElemInstances());
}

TeQuerierParams&
TeQuerier::params() 
{ 
	return (*(Impl_->params())); 
}

TeTSParams& 
TeQuerier::getTSParams() 
{ 
	return (Impl_->getTSParams()); 
}

void 
TeQuerier::clear()
{
	if(!Impl_)
		return;

	Impl_->clear();
}

TeAttributeList
TeQuerier::getAttrList()
{
	TeAttributeList temp;
	if(!Impl_)
		return temp;

	return (Impl_->getAttrList());
}

TeBox	
TeQuerier::getBox()
{
	TeBox b;
	if(!Impl_)
		return b;

	return (Impl_->params()->box());
}

void 
TeQuerier::refresh(TeQuerierParams& params)
{
	if(Impl_)
		delete (Impl_);
	
	Impl_ = TeQuerierImplFactory::make(params);
}

bool 
TeQuerier::loadGeometries(TeMultiGeometry& geometries, unsigned int& index)
{
	if(!Impl_)
		return false;

	return (Impl_->loadGeometries(geometries, index));
}


bool 
TeQuerier::loadGeometries(TeMultiGeometry& geometries)
{
	if(!Impl_)
		return false;

	return (Impl_->loadGeometries(geometries));
}


