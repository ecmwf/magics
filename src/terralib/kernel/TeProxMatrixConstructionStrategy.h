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
/*! \file TeProxMatrixConstructionStrategy.h
    \brief This file contains structures and definitions about construction strategies of proximity matrix 
*/

#ifndef TeProxMatrixConstructionStrategy_H
#define TeProxMatrixConstructionStrategy_H

#include "TeProxMatrixImplementation.h"
#include "TeSTElementSet.h"
#include "TeNetwork.h"
#include "TeDatabase.h"
#include "TeMultiGeometry.h"


class TeSTEventSet;

TL_DLL bool TeFindObjectCentroid (TeMultiGeometry& mGeom, TeGeomRep rep, TeCoord2D& p);

struct TL_DLL TeProxMatrixConstructionParams
{
public:

	//construction parameters
	int						theme_id1_;
	TeGeomRep				geom_rep1_;
	int						theme_id2_;
	TeGeomRep				geom_rep2_;
	TeGPMConstructionStrategy		strategy_;
	double						max_distance_;
	int							num_neighbours_;
	double						max_net_distance_;
	double						max_connection_distance_;
	bool						calculate_distance_;

	//! Empty contructor 
	TeProxMatrixConstructionParams():
		theme_id1_(-1),  
		geom_rep1_(TeGEOMETRYNONE), 
		theme_id2_(-1), 
		geom_rep2_(TeGEOMETRYNONE), 
		strategy_(TeAdjacencyStrategy), 
		max_distance_(0.), 
		num_neighbours_(0), 
		max_net_distance_(0.),
		max_connection_distance_(0.), 
		calculate_distance_(false)
	{ }
	
	//! Constructor
	TeProxMatrixConstructionParams(const int& theme1, const TeGeomRep& geomRep1, const TeGPMConstructionStrategy& strType):
		theme_id1_(theme1), 
		geom_rep1_(geomRep1), 
		theme_id2_(-1), 
		geom_rep2_(TeGEOMETRYNONE), 
		strategy_(strType),
		max_distance_(0.), 
		num_neighbours_(0),
		max_net_distance_(0.),
		max_connection_distance_(0.), 
		calculate_distance_(false)
	{ }
			
	//! Operator ==
	bool operator==(const TeProxMatrixConstructionParams& other) const
	{
		return(	(theme_id1_==other.theme_id1_) && (geom_rep1_==other.geom_rep1_) &&
				(theme_id2_==other.theme_id2_) && (geom_rep2_==other.geom_rep2_) &&
				(strategy_==other.strategy_) && (max_distance_==other.max_distance_) &&
				(num_neighbours_==other.num_neighbours_) && (max_net_distance_==other.max_net_distance_) && 
				(max_connection_distance_==other.max_connection_distance_) &&
				(calculate_distance_==other.calculate_distance_));
	}
};

//! A templated class to representate construction strategies of proximity matrix
template<typename T>
class TeProxMatrixConstructionStrategy 
{
protected:
	//! Set of objetcs used to construct the matrix
	T*   objects_;

	//! Construction paramas
	TeProxMatrixConstructionParams  params_;

	//! Construct
	TeProxMatrixConstructionStrategy(); 

	//! Construct
	TeProxMatrixConstructionStrategy(T* objects, TeGeomRep geomRep, const TeGPMConstructionStrategy& type=TeAdjacencyStrategy);

	//! Copy construct
	TeProxMatrixConstructionStrategy(const TeProxMatrixConstructionStrategy& st);

public:
	//! Construct the proximity matrix 
	virtual bool Construct(TeProxMatrixImplementation*)=0;

	//! Destructor
	virtual ~TeProxMatrixConstructionStrategy() 
	{}

	//! Set the set of objects and its geometry representation
	void setSTObjects (T*  objects, TeGeomRep  geomRep); 
	
	//! Get the objects used to construct the matrix
	T* objects()  { return objects_; } 

	//! Returns the construction params
	TeProxMatrixConstructionParams& constructionParams() { return params_; }
	
	//! Verify if the type of the strategy, the object set and its geometry representation are equal
	virtual bool IsEqual (const TeProxMatrixConstructionStrategy<T>& other) const;  
};

template<typename T> 
TeProxMatrixConstructionStrategy<T>::TeProxMatrixConstructionStrategy() : 
	objects_(0), 
	params_(-1, TeGEOMETRYNONE, TeAdjacencyStrategy)
	{ } 

template<typename T> 
TeProxMatrixConstructionStrategy<T>::TeProxMatrixConstructionStrategy(T*  objects, TeGeomRep geomRep, const TeGPMConstructionStrategy& type) :
	objects_(objects)
	{
		if(objects->theme())
			params_.theme_id1_ = objects->theme()->id();
		params_.geom_rep1_ = geomRep;
		params_.strategy_ = type;
	} 

template<typename T> 
TeProxMatrixConstructionStrategy<T>::TeProxMatrixConstructionStrategy (const TeProxMatrixConstructionStrategy& st)
{
	objects_ = st.objects_; 
	params_ = st.params_;
}

template<typename T> void 
TeProxMatrixConstructionStrategy<T>::setSTObjects (T*  objects, TeGeomRep  geomRep)
{	
	objects_ = objects; 
	if(objects->theme())
		params_.theme_id1_ = objects->theme()->id();
	params_.geom_rep1_ = geomRep; 
}

template<typename T> bool 
TeProxMatrixConstructionStrategy<T>::IsEqual (const TeProxMatrixConstructionStrategy<T>& other) const
{
	return ((params_==other.params_) && 
		((&objects_)==(&other.objects_))); 
}


//! A class to implement the local adjacency strategy of proximity matrix
class TL_DLL TeProxMatrixLocalAdjacencyStrategy : public  TeProxMatrixConstructionStrategy<TeSTElementSet>
{
public:

	//! Constructor
	TeProxMatrixLocalAdjacencyStrategy (); 

	//! Constructor
	TeProxMatrixLocalAdjacencyStrategy (TeSTElementSet*  objects, TeGeomRep	geomRep, bool calcDistance=false);
	
	//! Copy constructor
	TeProxMatrixLocalAdjacencyStrategy (TeProxMatrixLocalAdjacencyStrategy& st);  
	 
	 //! Destructor
	virtual ~TeProxMatrixLocalAdjacencyStrategy() {}
	
	//! Construct the proximity matrix through local adjacency strategy 
	virtual bool Construct (TeProxMatrixImplementation* imp);
		
	//! Equal operator 
	bool operator== (const TeProxMatrixLocalAdjacencyStrategy& rhs) const;  
	
	//! Assignment operator 
	TeProxMatrixLocalAdjacencyStrategy& operator= (const TeProxMatrixLocalAdjacencyStrategy& rhs); 

};


//! A class to implement the local distance strategy of proximity matrix
template<typename Set>
class TeProxMatrixLocalDistanceStrategy : public TeProxMatrixConstructionStrategy<Set>
{
public:

	//! Constructor
	TeProxMatrixLocalDistanceStrategy ();
	
	//! Constructor
	TeProxMatrixLocalDistanceStrategy (Set*  objects, TeGeomRep geomRep, double max_distance); 
	
	//! Copy constructor
	TeProxMatrixLocalDistanceStrategy (const TeProxMatrixLocalDistanceStrategy<Set>& st); 
	
	//! Destructor
	virtual ~TeProxMatrixLocalDistanceStrategy(){}
	
	//! Construct the proximity matrix through local distance strategy 
	virtual bool Construct (TeProxMatrixImplementation* imp); 

	//! Equal operator 
	bool operator== (const TeProxMatrixLocalDistanceStrategy<Set>& s) const; 
		
	//! Assignment operator
	TeProxMatrixLocalDistanceStrategy<Set>& operator= (const TeProxMatrixLocalDistanceStrategy<Set>& rhs);

};


//! A class to implement the nearest neighbour strategy of proximity matrix
class TL_DLL TeProxMatrixNearestNeighbourStrategy : public  TeProxMatrixConstructionStrategy<TeSTEventSet>
{
public:
	//! Empty constructor
	TeProxMatrixNearestNeighbourStrategy ();

	//! Constructor
	// The STEventSet must be created with geometries, using kdTree and ordered by object_id
	TeProxMatrixNearestNeighbourStrategy (TeSTEventSet* objects, int num_neighbours);
	
	//! Copy constructor
	TeProxMatrixNearestNeighbourStrategy (const TeProxMatrixNearestNeighbourStrategy& st); 
	
	//! Destructor
	virtual ~TeProxMatrixNearestNeighbourStrategy(){}
	
	//! Construct the proximity matrix through local distance strategy 
	virtual bool Construct (TeProxMatrixImplementation* imp); 

	//! Equal operator 
	bool operator== (const TeProxMatrixNearestNeighbourStrategy& s) const; 
		
	//! Assignment operator
	TeProxMatrixNearestNeighbourStrategy& operator= (const TeProxMatrixNearestNeighbourStrategy& rhs);

};


//! A class to implement the closed network strategy of proximity matrix
class TL_DLL TeProxMatrixClosedNetworkStrategy : public TeProxMatrixConstructionStrategy<TeSTElementSet>
{
private:
	TeGraphNetwork*		net_;
 
public:
	//! Constructor
	TeProxMatrixClosedNetworkStrategy (	TeSTElementSet*  objects, TeGeomRep rep, double max_local_distance,
										double max_net_distance, double  max_connection_distance, 
										TeGraphNetwork* input_net); 

	//! Copy constructor
	TeProxMatrixClosedNetworkStrategy (const TeProxMatrixClosedNetworkStrategy& rhs);
		  
	//! Destructor
	virtual ~TeProxMatrixClosedNetworkStrategy()
	{	
		if(net_)
			delete (net_); 
	}

	//! Construct the proximity matrix through closed network strategy 
	virtual bool Construct (TeProxMatrixImplementation* imp); 
	
	//! Verify if is equal
	virtual bool IsEqual (const TeProxMatrixConstructionStrategy<TeSTElementSet>& other) const; 
	
	//! Assignment operator
	TeProxMatrixClosedNetworkStrategy& operator= (const TeProxMatrixClosedNetworkStrategy& rhs);

	//! Equal operator
	bool operator== (const TeProxMatrixClosedNetworkStrategy& other) const;  

	//! Get the objects used to construct the matrix
	TeSTElementSet* objects() { return objects_; }
};

//! A class to implement the open network strategy of proximity matrix (among a set of objetcs)
class TL_DLL TeProxMatrixOpenNetworkStrategy : public TeProxMatrixConstructionStrategy<TeSTElementSet>
{
private:
	TeGraphNetwork*		net_;

public:

	//! Constructor
	TeProxMatrixOpenNetworkStrategy (	TeSTElementSet*  objects, TeGeomRep rep, double max_local_distance, 
										double	max_net_distance, double  max_connetion_distance, 
										TeGraphNetwork* input_net); 
	//! Copy constructor
	TeProxMatrixOpenNetworkStrategy (const TeProxMatrixOpenNetworkStrategy& rhs); 
	
	//! Destructor
	virtual ~TeProxMatrixOpenNetworkStrategy()
	{
		if(net_)
			delete (net_); 
	}

	//! Construct the proximity matrix through open network strategy
	virtual bool Construct (TeProxMatrixImplementation* imp); 

	//! Verify if is equal
	virtual bool IsEqual (const TeProxMatrixConstructionStrategy<TeSTElementSet>& other) const; 

	//! Equal operator
	bool operator== (const TeProxMatrixOpenNetworkStrategy& other) const; 
	
	//! Assignment operator
	TeProxMatrixOpenNetworkStrategy& operator= (const TeProxMatrixOpenNetworkStrategy& rhs);
};


//! A class to implement the open network strategy of proximity matrix (relationships among objetc of two differnt sets)
class TL_DLL TeProxMatrixOpenNetworkStrategy2 : public TeProxMatrixConstructionStrategy<TeSTElementSet>
{
private:
	TeSTElementSet*		objects2_;
	TeGraphNetwork*		net_;
public:

	//! Constructor
	TeProxMatrixOpenNetworkStrategy2 (	TeSTElementSet*  objects1, TeGeomRep rep1,
										TeSTElementSet*  objects2, TeGeomRep rep2,
										double max_local_distance, 
										double	max_net_distance, double  max_connetion_distance, 
										TeGraphNetwork* input_net); 
	//! Copy constructor
	TeProxMatrixOpenNetworkStrategy2 (const TeProxMatrixOpenNetworkStrategy2& rhs); 
	
	//! Destructor
	virtual ~TeProxMatrixOpenNetworkStrategy2 ()
	{
		if(net_)
			delete (net_); 
	}

	//! Construct the proximity matrix through open network strategy
	virtual bool Construct (TeProxMatrixImplementation* imp); 

	//! Verify if is equal
	virtual bool IsEqual (const TeProxMatrixConstructionStrategy<TeSTElementSet>& other) const; 

	//! Equal operator
	bool operator== (const TeProxMatrixOpenNetworkStrategy2& other) const; 
	
	//! Assignment operator
	TeProxMatrixOpenNetworkStrategy2& operator= (const TeProxMatrixOpenNetworkStrategy2& rhs);

};

//////////////////////////////////////////////////////////////////////
// TeProxMatrixLocalDistanceStrategy 
//////////////////////////////////////////////////////////////////////
template<typename Set> 
TeProxMatrixLocalDistanceStrategy<Set>::TeProxMatrixLocalDistanceStrategy ():
		TeProxMatrixConstructionStrategy<Set> (0, TeGEOMETRYNONE, TeDistanceStrategy) 
		{}


template<typename Set> 
TeProxMatrixLocalDistanceStrategy<Set>::TeProxMatrixLocalDistanceStrategy (Set*  objects, TeGeomRep geomRep, double max_distance): 
		TeProxMatrixConstructionStrategy<Set>(objects, geomRep, TeDistanceStrategy)
		{
			TeProxMatrixConstructionStrategy<Set>::params_.max_distance_ = max_distance; 
		}

	
template<typename Set> 
TeProxMatrixLocalDistanceStrategy<Set>::TeProxMatrixLocalDistanceStrategy (const TeProxMatrixLocalDistanceStrategy<Set>& st): 
		TeProxMatrixConstructionStrategy<Set>(st)
		{} 

template<typename Set> bool 
TeProxMatrixLocalDistanceStrategy<Set>::Construct(TeProxMatrixImplementation* imp)
{
	if (imp == 0) 
		return false;
	
	// Iterate over all selected objects, selecting their neighbours
	TeSTElementSet::iterator itobj1 = TeProxMatrixConstructionStrategy<Set>::objects_->begin();
	
	// ----- progress bar
	int step = 0;
	if(TeProgress::instance())
		TeProgress::instance()->setTotalSteps(TeProxMatrixConstructionStrategy<Set>::objects_->numSTInstance());
	// -----

	TeProjection* proj = 0;
	if(TeProxMatrixConstructionStrategy<Set>::objects_->theme())
		proj = TeProxMatrixConstructionStrategy<Set>::objects_->theme()->layer()->projection();
	else if(TeProxMatrixConstructionStrategy<Set>::objects_->getLayer())
		proj = TeProxMatrixConstructionStrategy<Set>::objects_->getLayer()->projection();

	TePrecision::instance().setPrecision(TeGetPrecision(proj));
	double max_d = TeProxMatrixConstructionStrategy<Set>::params_.max_distance_;
		
	while ( itobj1 != TeProxMatrixConstructionStrategy<Set>::objects_->end())
	{
		// Gets the possible objects from RTree in the element set
		vector<TeSTInstance*> result;
		TeBox b = (*itobj1).getGeometries().getBox();
		TeBox bAux(b.x1()-max_d, b.y1()-max_d, b.x2()+max_d, b.y2()+max_d);
		
		TeProxMatrixConstructionStrategy<Set>::objects_->search(bAux, result);

		string object_id1 = (*itobj1).getObjectId();
		TeCoord2D coord1 = itobj1->getCentroid();
		for(unsigned int index =0; index<result.size(); ++index)
		{
			string object_id2 = result[index]->getObjectId();
			if(object_id1==object_id2)
				continue;
			
			TeCoord2D coord2 = result[index]->getCentroid();
			double dist = TeDistance(coord1, coord2);
			if(dist <= max_d)
			{
				if(!imp->isConnected (object_id1,object_id2))
				{
					TeProxMatrixAttributes attr;
					attr.CentroidDistance (dist);
					imp->connectObjects (object_id1, object_id2, attr);
					imp->connectObjects (object_id2, object_id1, attr);
				}
			}
		}

		if(TeProgress::instance())
		{
			if (TeProgress::instance()->wasCancelled())
			{
				TeProgress::instance()->reset();
				return false;
			}
			else
				TeProgress::instance()->setProgress(step);
		}	
		++step;
		++itobj1;
	}
	
	if (TeProgress::instance())
		TeProgress::instance()->reset();
	return true;
}

template<typename Set> bool 
TeProxMatrixLocalDistanceStrategy<Set>::operator== (const TeProxMatrixLocalDistanceStrategy<Set>& s) const 
{
	return ( TeProxMatrixConstructionStrategy<Set>::IsEqual(s)); 
}
	
template<typename Set> TeProxMatrixLocalDistanceStrategy<Set>& 
TeProxMatrixLocalDistanceStrategy<Set>::operator= (const TeProxMatrixLocalDistanceStrategy<Set>& rhs)
{
	if ( this != &rhs )
	{
		TeProxMatrixConstructionStrategy<Set>::objects_ = rhs.objects_;
		TeProxMatrixConstructionStrategy<Set>::params_ = rhs.params_;
	}
	return *this;
}

#endif
