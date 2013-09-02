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
/*! \file TeNeighbours.h
    \brief This file contains structures and definitions about neighborhood 
*/

#ifndef  __TERRALIB_INTERNAL_NEGHBOURS_H
#define  __TERRALIB_INTERNAL_NEGHBOURS_H

#include "TeUtils.h"
#include "TeAttribute.h"

#include <vector> 
#include <string>
#include <map> 
using namespace std;


//! Attributes associated with each neighborhood of a proximity matrix  
class TL_DLL TeProxMatrixAttributes  
{
	private:
		double	_weight;
		int		_slice;
		int		_order;
		double	_centroid_distance;
		double	_borders_length;
		double	_net_objects_distance;
		double	_net_minimum_path;


	public:
		
		//! Empty constructor 
		TeProxMatrixAttributes(); 

		//! Constructor 
		TeProxMatrixAttributes(const double& w, const int& slice, const int& order, 
			const double& cent_dist, const double& border_length, 
			const double& net_distance, const double& net_minimun_path): 
				_weight(w), _slice(slice), 
				_order(order), _centroid_distance(cent_dist), 
                _borders_length(border_length), 
                _net_objects_distance(net_distance), 
                _net_minimum_path(net_minimun_path)
		{}
		
		//! Copy constuctor 
		TeProxMatrixAttributes (const TeProxMatrixAttributes& att); 
		
		//! Return weight
		double	Weight() {return _weight;}

		//! Return slice
		int		Slice () {return _slice;}

		//! Return order
		int		Order() {return _order;}

		//! Return border length
		double	BorderLength() {return _borders_length;}

		//! Return centroid distance
		double	CentroidDistance() {return _centroid_distance;}

		//! Return network objects distance
		double	NetworkObjectsDistance() {return _net_objects_distance;}

		//! Return network minimum path
		double	NetworkMinimumPath () {return _net_minimum_path;}

		//! Set weight
		void Weight(double w) {_weight = w;}

		//! Set slice
		void Slice (int s) {_slice = s;}
		
		//! Set order
		void Order(int o) {_order = o;}

		//! Set border length
		void BorderLength(double l) {_borders_length = l;}

		//! Set centroid distance
		void CentroidDistance(double d) {_centroid_distance = d;}

		//! Set network objects distance
		void NetworkObjectsDistance(double d) {_net_objects_distance = d;}

		//! Set network minimum path
		void NetworkMinimumPath (double d) {_net_minimum_path = d;}

		//! Return the attributes as a TePropertyVector
		TePropertyVector getProperties ();

		//! Return if the border length was computed 
		bool WasBordersLengthComputed () {if (_borders_length == -1.0) return false; else return true;}
		
		//! Return if the centroid distance was computed
		bool WasCentroidDistanceComputed () {if (_centroid_distance == -1.0) return false; else return true;}
		
		//! Return if the network objects distance was computed
		bool WasNetworkObjectsDistanceComputed () {if (_net_objects_distance == -1.0) return false; else return true;}
		
		//! Return if the network minimal path was computed
		bool WasNetworkMinimumPathComputed () {if (_net_minimum_path == -1.0) return false; else return true;}

		//! Copy operator
		TeProxMatrixAttributes& operator= (const TeProxMatrixAttributes& att); 
		
		//! Comparison Operator
		bool operator==(const TeProxMatrixAttributes& att) const;
			
		//! Destructor
		virtual ~TeProxMatrixAttributes() {}
};

//! A map from a object to its attributes
typedef map<string, TeProxMatrixAttributes> TeNeighboursMap;


//! A class to representate the neighbours of a object 
class TL_DLL TeNeighbours  
{
private:
	typedef pair<string, TeProxMatrixAttributes>	neigh_values;
	typedef vector<neigh_values>					neigh_vector;

	neigh_vector _neigh; 
	

public:

	typedef neigh_vector::iterator iterator;
	typedef neigh_vector::const_iterator const_iterator;

	//! Empty constructor
	TeNeighbours () {};

	//! Copy constructor
	TeNeighbours(const TeNeighboursMap& neigh);

	//! Copy constructor
	TeNeighbours(const TeNeighbours& neigh);
	
	//! Return the number of the neighbours
	int size() const  { return _neigh.size();}

	//! Return a iterator to the begin of the neighbours
    iterator begin()     { return _neigh.begin();}

	//! Return a iterator to the one past end of the neighbours
    iterator end()       { return _neigh.end();}

	//! Return the n-th neighbour object_id, if n < map size.
	string ObjectId (int n);  

	//! Return the n-th neighbour object_id, if n < map size.
	string operator[](int n);  

	//! Return the n-th connection weight (corresponding to the n-th neighbour), if n < map size.
	double Weight (int n);		

	//! Return the connection weight, given the neighbour object_id 
	double Weight (const string& object_id);	
	
	//! Return the complete set of connection attributes (corresponding to the ith neighbour), packed in a TeProxMatrixAttributes object.
	TeProxMatrixAttributes Attributes (int n);
	
	//! Insert a new neighbour
	bool Insert (const string& object_id, const TeProxMatrixAttributes& attr);

	//! Remove a neighbour
	bool Remove (const string& object_id);

	//! Copy operator
	TeNeighbours& operator= (const TeNeighbours& neigh);

	//! Comparison Operator
	bool operator==(const TeNeighbours& p);

	//! Destructor
	virtual ~TeNeighbours() {}

};

#endif
