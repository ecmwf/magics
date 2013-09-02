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
/*! \file TeProxMatrixSlicingStrategy.h
    \brief This file contains structures and definitions about slicing strategies of proximity matrix 
*/


#ifndef TeProxMatrixSlicingStrategy_H
#define TeProxMatrixSlicingStrategy_H

#include "TeProxMatrixImplementation.h"

struct TL_DLL TeProxMatrixSlicingParams
{
public:
	TeGPMSlicingStrategy		strategy_;
	double						zone_dist_;
	bool						zone_local_;

	TeProxMatrixSlicingParams(const TeGPMSlicingStrategy& type=TeNoSlicingStrategy):
		strategy_(type), zone_dist_(0.), zone_local_(false)
		{}

	bool operator==(const TeProxMatrixSlicingParams& other) const
	{
		return ((strategy_==other.strategy_) && (zone_dist_==other.zone_dist_) &&
			(zone_local_==other.zone_local_));
	}
};


//! An abstract class to representate slicing strategies of proximity matrix    
class TL_DLL TeProxMatrixSlicingStrategy  
{
protected:
	//! Slicing type
	TeProxMatrixSlicingParams params_; 

	//! Empty constructor
	TeProxMatrixSlicingStrategy(const TeGPMSlicingStrategy& type): params_(type) {}

public:
	//! Destructor
	virtual ~TeProxMatrixSlicingStrategy() {}

	//! Slice the proximity matrix 
	virtual bool Slice(TeProxMatrixImplementation* )=0;

	//! Equal operator
	virtual bool operator== (const TeProxMatrixSlicingStrategy& s) const {return (params_==s.params_);}

	//! Returns the slicing params
	TeProxMatrixSlicingParams& slicingParams() { return params_; }

};


//! A class to implement the no slicing strategy of proximity matrix (i.e., all neighbour are considered to be in the first slice).
class TL_DLL TeProxMatrixNoSlicingStrategy : public TeProxMatrixSlicingStrategy
{
public:
	//! Empty constructor
	TeProxMatrixNoSlicingStrategy (): TeProxMatrixSlicingStrategy(TeNoSlicingStrategy)
	{}

	//! No slice the proximity matrix 
	virtual bool Slice(TeProxMatrixImplementation* ) {return true;}

	//! Destructor
	~TeProxMatrixNoSlicingStrategy() {}
};


//! A class to implement the zone slicing strategy of proximity matrix (by local or newtork connection distance);
class TL_DLL TeProxMatrixZonesSlicingStrategy : public TeProxMatrixSlicingStrategy
{
public:
	//! Constructor
	TeProxMatrixZonesSlicingStrategy (double dist, bool local = true):
	  TeProxMatrixSlicingStrategy(TeZonesSlicingStrategy)
	  {
		  params_.zone_dist_=dist;
		  params_.zone_local_=local;
	  }

	//! Slice the proximity matrix through zone strategy 
	virtual bool Slice(TeProxMatrixImplementation* imp);

	//! Destructor
	~TeProxMatrixZonesSlicingStrategy() {}
};

#endif
