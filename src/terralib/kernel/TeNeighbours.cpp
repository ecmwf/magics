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

#ifdef WIN32
#pragma warning ( disable: 4786 )
#endif

#include "TeNeighbours.h"

TeProxMatrixAttributes::TeProxMatrixAttributes() 
{
	_weight = 1;					//default
	_slice	= 1;					//default
	_order  = 1;					//defaul
	_centroid_distance	= -1.0;		//not computed
	_borders_length		= -1.0;		//not computed
	_net_objects_distance		= -1.0;   //not computed
	_net_minimum_path= -1.0;		//not computed
}  


TeProxMatrixAttributes::TeProxMatrixAttributes (const TeProxMatrixAttributes& att) 
{		
	_weight = att._weight;
	_slice = att._slice; 
	_order = att._order; 
	_centroid_distance = att._centroid_distance;
	_borders_length = att._borders_length; 
	_net_objects_distance = att._net_objects_distance;
	_net_minimum_path = att._net_minimum_path;
}


TePropertyVector 
TeProxMatrixAttributes::getProperties ()
{
	TePropertyVector vec;
	TeProperty prop1; prop1.value_ = Te2String (_weight); vec.push_back ( prop1 );
	TeProperty prop2; prop2.value_ = Te2String (_slice); vec.push_back ( prop2 );
	TeProperty prop3; prop3.value_ = Te2String (_order); vec.push_back ( prop3 );
	TeProperty prop4; prop4.value_ = Te2String (_borders_length); vec.push_back ( prop4 );
	TeProperty prop5; prop5.value_ = Te2String (_centroid_distance); vec.push_back ( prop5 );	
	TeProperty prop6; prop6.value_ = Te2String (_net_objects_distance); vec.push_back ( prop6 );	
	TeProperty prop7; prop7.value_ = Te2String (_net_minimum_path); vec.push_back ( prop7 );
	return vec; 
}


TeProxMatrixAttributes& 
TeProxMatrixAttributes::operator= (const TeProxMatrixAttributes& att)
{		
	_weight = att._weight;
	_slice = att._slice; 
	_order = att._order; 
	_centroid_distance = att._centroid_distance;
	_borders_length = att._borders_length; 
	_net_objects_distance = att._net_objects_distance;
	_net_minimum_path = att._net_minimum_path;
	return *this;
}



bool 
TeProxMatrixAttributes::operator==(const TeProxMatrixAttributes& att) const
{
	return ((_weight == att._weight) && 
		(_slice == att._slice) && 
		(_order == att._order) && 
		(_centroid_distance == att._centroid_distance) && 
		(_borders_length == att._borders_length) &&  
		(_net_objects_distance == att._net_objects_distance) && 
		(_net_minimum_path == att._net_minimum_path));
}


TeNeighbours::TeNeighbours(const TeNeighboursMap& neigh)  
{

	TeNeighboursMap::const_iterator pos = neigh.begin(); 

	for (pos = neigh.begin(); pos != neigh.end(); ++pos)
		_neigh.push_back(make_pair (pos->first, pos->second));
}



TeNeighbours::TeNeighbours (const TeNeighbours& neigh)
{
	const_iterator it;
	for (it = neigh._neigh.begin(); it != neigh._neigh.end(); ++it) 
		_neigh.push_back(*it);
}

		
string
TeNeighbours::ObjectId (int n) 
{
	if (n < (int)_neigh.size()) 
		return _neigh[n].first;

	else {
		string empty;
		return empty;
	}
}


string 
TeNeighbours:: operator[](int n)
{
	if (n < (int)_neigh.size()) 
		return _neigh[n].first;

	else {
		string empty;
		return empty;
	}
}

double 
TeNeighbours:: Weight (int n)
{
	if (n < (int)_neigh.size()) 
		return _neigh[n].second.Weight();

	else
		return 0.0;
}		

	
double 
TeNeighbours:: Weight (const string& object_id)
{
	for (unsigned int i = 0; i< _neigh.size(); ++i) {
		if (object_id == _neigh[i].first) 
			return _neigh[i].second.Weight();
	}
	return 0.0;
}		

	
TeProxMatrixAttributes 
TeNeighbours:: Attributes (int n)
{
	TeProxMatrixAttributes attr;

	if (n < (int)_neigh.size()) 
		return _neigh[n].second;
	else
		return attr;
}
	
bool
TeNeighbours:: operator== (const TeNeighbours& neigh)
{
	if (_neigh == neigh._neigh) return true;
	else return false;
}


TeNeighbours& 
TeNeighbours::operator= (const TeNeighbours& neigh)
{
	if (*this == neigh)
	    return *this;

	_neigh.clear();

	const_iterator it;
	for (it = neigh._neigh.begin(); it != neigh._neigh.end(); ++it) 
		_neigh.push_back(*it);
 
    return *this; 
}


bool
TeNeighbours:: Insert (const string& object_id, const TeProxMatrixAttributes& attr)
{
	for (unsigned int i = 0; i< _neigh.size(); i++) {
		if (object_id == _neigh[i].first) 
			return false;
	}
	_neigh.push_back (make_pair (object_id, attr));
	return true;
}


bool 
TeNeighbours:: Remove (const string& object_id)
{
	iterator it = _neigh.begin();
	while (it!= _neigh.end()) 
		if (object_id == (*it).first)
		{
			_neigh.erase (it);
			return true;
		}
	return false;
}
