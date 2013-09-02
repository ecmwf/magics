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
/*! \file TeProxMatrixWeightsStrategy.h
    \brief This file contains structures and definitions about weight strategies of proximity matrix 
*/


#ifndef TeProxMatrixWeightsStrategy_H
#define TeProxMatrixWeightsStrategy_H

#include "TeProxMatrixImplementation.h"
#include <vector>


struct TL_DLL TeProxMatrixWeightsParams
{
public:
	TeGPMWeightsStrategy		strategy_;
	bool						norm_;
	double						a_;
	double						b_;
	double						c_;
	double						factor_;
	double						dist_ratio_;
	double						max_local_dist_;

	TeProxMatrixWeightsParams(): 
		strategy_(TeNoWeightsStrategy), norm_(false), a_(1.), 
		b_(1.), c_(1.), factor_(1.), dist_ratio_(0.), max_local_dist_(0.)
		{}

	TeProxMatrixWeightsParams(bool norm, const TeGPMWeightsStrategy& type): 
		strategy_(type), norm_(norm), a_(1.), b_(1.), c_(1.), 
		factor_(1.), dist_ratio_(0.), max_local_dist_(0.)
		{}

	bool operator== (const TeProxMatrixWeightsParams& other) const
	{
		return ((strategy_==other.strategy_) && (norm_==other.norm_) &&
			(a_==other.a_) && (b_==other.b_) && (c_==other.c_) &&
			(factor_==other.factor_) && (dist_ratio_==other.dist_ratio_) &&
			(max_local_dist_==other.max_local_dist_));
	}
};


//! An abstract class to representate weight strategies of proximity matrix    
class TL_DLL TeProxMatrixWeightsStrategy  
{
protected:
	TeProxMatrixWeightsParams params_; 

	//! Constructor
	TeProxMatrixWeightsStrategy(bool norm = false, const TeGPMWeightsStrategy& type=TeNoWeightsStrategy ):
		params_(norm, type)
	 { }

public:
	//! Compute weigths
	virtual bool ComputeWeigths (TeProxMatrixImplementation* ) =0;

	//! Destructor
	virtual ~TeProxMatrixWeightsStrategy() {}

	//! Equal operator
	virtual bool operator== (const TeProxMatrixWeightsStrategy& w) const {return (params_==w.params_);}

	//! Returns the strategy to weigt the matrix
	TeProxMatrixWeightsParams& weightsParams() { return params_; }
};

//! A class to implement the no weight strategy of proximity matrix (i.e., all weights are 1, only indicating that a connection exists).
class TL_DLL TeProxMatrixNoWeightsStrategy : public TeProxMatrixWeightsStrategy
{
public:
	//! Constructor
	TeProxMatrixNoWeightsStrategy (bool norm = true) : TeProxMatrixWeightsStrategy(norm, TeNoWeightsStrategy)
	{ }

	//! Compute weigths
	virtual bool ComputeWeigths (TeProxMatrixImplementation* imp);

	//! Destructor
	~TeProxMatrixNoWeightsStrategy() {}
};


//! A class to implement the inverse distance weight strategy of proximity matrix; if network distances were computed, they can be also considered.
//! Formula: w = (a*1/dist_centroids + b*1/dist_to_net + c*1/dist_net_connection)*factor
//! These values can be normalized or not.
class TL_DLL TeProxMatrixInverseDistanceStrategy : public TeProxMatrixWeightsStrategy
{
public:
	//! Constructor
	TeProxMatrixInverseDistanceStrategy  (double a = 1.0, double b = 1.0, double c = 1.0, double factor = 1.0, bool norm = true) : 
	  TeProxMatrixWeightsStrategy (norm, TeInverseDistanceStrategy)
	{ params_.a_ = a; params_.b_ = b; params_.c_ = c; params_.factor_ = factor;}

	//! Compute weights
	virtual bool ComputeWeigths (TeProxMatrixImplementation* imp); 

	//! Destructor	
	~TeProxMatrixInverseDistanceStrategy() {}
};


//! A class to implement the inverse distance weight strategy of proximity matrix; if network distances were computed, they can be also considered.
//! Formula: w = (a*1/(dist_centroids)2 + b*1/(dist_to_net)2 + c*1/(dist_net_connection)2)*factor
//! These values can be normalized or not.
class TL_DLL TeProxMatrixSquaredInverseDistanceStrategy : public TeProxMatrixWeightsStrategy
{
public:
	//! Constructor
	TeProxMatrixSquaredInverseDistanceStrategy  (double a = 1.0, double b = 1.0, double c = 1.0, double factor = 1.0, bool norm = true) : 
	  TeProxMatrixWeightsStrategy (norm, TeSquaredInverseDistStrategy)
	{ params_.a_ = a; params_.b_ = b; params_.c_ = c; params_.factor_ = factor;}

	//! Compute weights
	virtual bool ComputeWeigths (TeProxMatrixImplementation* imp); 

	//! Destructor	
	~TeProxMatrixSquaredInverseDistanceStrategy() {}
};


//! A class to implement the connection strenght weight strategy of proximity matrix
//! If centroids distance is smaller them max_local_distance, w will be only the local distance inverse (multiplied by factor).
//! otherwise, it will be w = 1/(dist_ratio*dist_to_net + dist_net_connection)*factor.
class TL_DLL TeProxMatrixConnectionStrenghtStrategy : public TeProxMatrixWeightsStrategy
{
public:
	//! Constructor
	TeProxMatrixConnectionStrenghtStrategy  (double dist_ratio = 1.0, double max_local_distance = 0.0, double factor = 1.0, bool norm = true) 
		: TeProxMatrixWeightsStrategy (norm, TeConnectionStrenghtStrategy )
	{ 
		params_.dist_ratio_ = dist_ratio;
		params_.max_local_dist_ = max_local_distance;
		params_.factor_ = factor;
	}

	//! Compute weights
	virtual bool ComputeWeigths (TeProxMatrixImplementation* imp); 

	//! Destructor	
	~TeProxMatrixConnectionStrenghtStrategy() {}
};

#endif 
