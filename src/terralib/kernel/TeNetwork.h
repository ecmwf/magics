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
/*! \file TeNetwork.h
    \brief This file defines class for handling networks in Terralib
*/
#ifndef TeNetwork_H
#define TeNetwork_H

#include "TeGeometry.h"
#include "graph.h"

class TeSTElementSet;


//! class for handling networks
class TL_DLL TeGraphNetwork
{
protected:
	//! Set of nodes and the cost of each edge
	br_stl::Graph<TeNode, double>	graph_;

	//! Set of geometric representation of edges
	TeLineSet						line_set_;	
	
	//! A map to associate each edge (line object_id) to its cost 
	map<string, double>				line_cost_;	

	
public:

	//! Empty constructor
	TeGraphNetwork() : graph_ (true) {}; // directed graph
	
		
	//! Create a graph from TeLineSet; the line size is considered the cost. In this case, graph non-directed.  
	TeGraphNetwork (TeLineSet& ls);
	

	//! Create a graph from TeLineSet; the cost is given by the map.    
	TeGraphNetwork (TeLineSet& ls, map<string, double>& line_costs);
	

	//! Create a network from a set of line objects; the cost is given by the attrName sto property. The graph is non-directed.    
	TeGraphNetwork (TeSTElementSet& stos, string& attrName);

	
	//! Assignment operator
	TeGraphNetwork& operator=(TeGraphNetwork& other)
	{
		if(this != &other)
		{
			graph_ = other.graph_;
			line_set_ = other.getLineSet();
			line_cost_ = other.getLineCosts();
		}
		return (*this);
	}

	//! Add lineset to graph. Useful specially for directed graphs (lines must be entered in both directions. It has to be tested for existing graphs.
	bool Add (TeLineSet& ls, map<string, double>& line_costs); 

	//! Calculate the minimun path
	bool minimumPath (TeNode& n1, TeNodeSet& set, vector<double>& dist);

	//! Get the i-th node 
	bool getNode (int i, TeNode& node); 
	
	//! Get the nearest network node of a specific point (p1)  
	bool nearestNodePoint (TeCoord2D& p1, int& lindex, TeCoord2D& pinter, double& distance, double tol = 0.0);

	//! Get the nearest network lines point from a specific point (p1) 
	bool nearestNetworkPoint (TeCoord2D& p1, int& lindex, TeCoord2D& pinter, double& distance, double tol = 0.0);

	//! Get line Set
	TeLineSet getLineSet () { return line_set_;}

	//! Get line costs
	map<string, double>	getLineCosts () {return line_cost_;}

	//! Insert a new line
	void insertLine (TeLine2D& line, const double& attr);	

	//! Insert a new node
	bool breakLineSet (TeNode& node, int i); //maybe should be done externally.

	//! Destructor
	virtual ~TeGraphNetwork () {}

};


//! class for handling networks
class TL_DLL TeNetwork
{
private:
	//! Set of nodes and the cost of each edge
	br_stl::Graph<TeNode, double>	graph_;
	
	//! Set of edges
	TeLineSet						line_set_;	
	
	//! A map to associate each edge (line object_id) to its arc 
	map<string, TeArc>				arcs_map_;	

public:
	//! Empty constructor
	TeNetwork() : graph_ (false) { };

	//! Create a graph from TeLineSet, the line size is considered the cost    
	TeNetwork (TeLineSet& ls);  

	//! Calculate the minimun path
	bool minimumPath (TeNode& n1, TeNodeSet& set, vector<double>& dist);


	//! Insert a new line
	void insertLine (TeLine2D& line, const double& attr);


	//! Insert a new node
	bool insertNode (TeNode& node, int i);

	//! Get the i-th node 
	bool getNode (int i, TeNode& node); 

	//! Get the nearest network point of a specific point (p1) 
	bool nearestNetworkPoint (TeCoord2D& p1, int& lindex, TeCoord2D& pinter, double& distance, double tol = 0.0);
	
	//! Get the nearest network node of a specific point (p1)  
	bool nearestNodePoint (TeCoord2D& p1, int& lindex, TeCoord2D& pinter, double& distance, double tol = 0.0);

	//! Destructor
	virtual ~TeNetwork () {}

	//! Get line Set
	TeLineSet getLineSet () { return line_set_;}

};

#endif 
