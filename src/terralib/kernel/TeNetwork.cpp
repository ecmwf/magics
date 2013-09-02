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

#include "TeNetwork.h"
#include "TeGeometryAlgorithms.h"
#include "TeSTElementSet.h"
#include "Gra_algo.h"



TeGraphNetwork:: TeGraphNetwork (TeLineSet& ls)  : graph_ (false)
{
	TeLineSet ls2;
  	ls2.copyElements(ls);

// The initial and final points of each line will be the nodes
	TeLineSet::iterator it = ls2.begin();
	while (it != ls2.end())
	{
			insertLine  ((*it), TeLength(*it));
			it++;
	}
}


TeGraphNetwork:: TeGraphNetwork (TeLineSet& ls, map<string, double>& line_costs)  : graph_ (false)
{
	if (ls.size() == line_costs.size())
	{
		TeLineSet::iterator it_line = ls.begin();
		
		while (it_line != ls.end())
		{
			string id = (*it_line).objectId ();
			map<string, double>:: iterator it_val = line_costs.find(id);
			if (it_val != line_costs.end())
				insertLine  ((*it_line), (*it_val).second); 
			it_line++;
		}
	}

}


bool
TeGraphNetwork:: Add (TeLineSet& ls, map<string, double>& line_costs) 
{
	if (ls.size() == line_costs.size())
	{
		TeLineSet::iterator it_line = ls.begin();
		
		while (it_line != ls.end())
		{
			string id = (*it_line).objectId ();
			map<string, double>:: iterator it_val = line_costs.find(id);
			if (it_val != line_costs.end())
				insertLine  ((*it_line), (*it_val).second); 
			it_line++;
		}
		return true;
	}
	else
		return false;

}


TeGraphNetwork:: TeGraphNetwork (TeSTElementSet& stos, string& attrName)  : graph_ (false)
{
	TeSTElementSet::iterator it = stos.begin();
	while (it != stos.end())
	{
		string value;
		if ((*it).getPropertyValue (attrName, value))
		{
			double val = atof(value.c_str());	
			TeLineSet objGeom;  
			if ((*it).getGeometry(objGeom))
			{
				double total_length = 0.0;
				TeLineSet::iterator it_geom = objGeom.begin();
				while (it_geom != objGeom.end())
				{
					total_length += TeLength (*it_geom);
					it_geom++;
				}

				it_geom = objGeom.begin();
				while (it_geom != objGeom.end())
				{
					double w = TeLength(*it_geom)/total_length;
					insertLine  (*it_geom, val*w);
					it_geom++;
				}	
			}
		}
		++it;
	}
}	


void
TeGraphNetwork:: insertLine (TeLine2D& line, const double& attr) 
{

	// Change line identifiers, to allow lines from different layers
	int j = line_set_.size(); 
	string line_objId = "l" + Te2String (j);
	line.objectId (line_objId); 


	// The initial and final nodes will be the first and last points of the line
	TeNode n1; 
	n1.add(line.first());
	string id1 = line.objectId() + "_p1"; 
	n1.objectId (id1);
	n1.geomId (0);

	TeNode n2; 
	n2.add(line.last());	
	string id2 = line.objectId() + "_p2";

	n2.objectId (id2);
	n2.geomId (0);

	for (unsigned int i = 0; i < graph_.size(); i++)
	{
		if (TeDistance (line.first(), graph_[i].first.location()) < 0.001)
			n1 = graph_[i].first;
			
		if (TeDistance (line.last(), graph_[i].first.location()) < 0.001)
			n2 = graph_[i].first;
	}

	if (n1 == n2)
	{
		cout << "linha circular" << endl;
	}
	else
	{
		graph_.insert (n1, n2, attr); // if graph is non-directed, edge from n2 to n1 will also be added
		line_cost_ [line.objectId()] = attr;
		line_set_.add (line);
	}

}



bool 
TeGraphNetwork:: minimumPath (TeNode& n1, TeNodeSet& nodeSet,vector<double>& result)
{
	vector<double> dist;
	vector<int> pred;

	// Compute minimum path to all vertex
	unsigned int j = 0;
	br_stl::Graph<TeNode, double>::iterator it = graph_.begin();
	while (it != graph_.end())
	{
	   if ((*it).first == n1)  
		    it = graph_.end();
	   else
	   {
		   it++;
		   j++;
	   }
	}
	if (j == graph_.size()) return false;

	br_stl::Dijkstra<br_stl::Graph<TeNode, double>, double> (graph_, dist, pred, j);

	for (unsigned int i = 0; i< nodeSet.size(); i++)
	{
		br_stl::Graph<TeNode, double>::iterator it = graph_.begin();
		unsigned int vertex = 0;
		while (it != graph_.end())
		{
			if ((*it).first == nodeSet[i])  
				it = graph_.end();
			else
			{  
				it++;
				vertex++;
			}
		}

		if (vertex == graph_.size()) return false;
		result.push_back (dist[vertex]);
	}


	return true;

}

bool 
TeGraphNetwork::getNode (int i, TeNode& node)
{ 
	if ((i >0) && (i < (int) graph_.size())) 
	{
		node = graph_[i].first;
		return true;
	}
	else 
		return false;
}

bool
TeGraphNetwork:: nearestNodePoint (TeCoord2D& p1, int& index, TeCoord2D& p2, double& distance, double tol)
{
		TeNodeSet node_set;
		for (unsigned int i = 0; i < graph_.size(); i++) 
			node_set.add (graph_[i].first);

		if (node_set.size() == 0) return false;

		TeNearest (p1, node_set, index, tol);
		p2 = node_set[index].elem();
		distance = TeDistance (p1, p2);
		return true;
}





bool
TeGraphNetwork:: breakLineSet (TeNode& node, int line)
{

	if ((line >= 0) && (line < (int) line_set_.size()))
	{
		// Break line
		int segment;
		if (TeLocateLineSegment (node.location(), line_set_[line], segment, 0.001))
		{
			if (segment <= (int) (line_set_[line].size()) - 2)
			{
				TeLine2D l1, l2;
				int j = 0;
				TeLine2D::iterator it = line_set_[line].begin();

				while (j <= segment)
				{
					l1.add (*it);
					it++;
					j++;
				}

				l1.add (node.location());
				l2.add (node.location ());

				while (it != line_set_[line].end())
				{
					l2.add (*it);
					it++;
				}
		
				string old_line_id = line_set_[line].objectId();
				string line1_id = old_line_id + "l1";
				string line2_id = old_line_id + "l2";

				l1.objectId(line1_id);
				l2.objectId(line2_id);
		
			    // Compute proportional costs for broken lines

				double d1   = TeLength (l1);
				double d2   = TeLength (l2);
				double d_tot = TeLength (line_set_[line]);

				double w1 = d1/d_tot;
				double w2 = d2/d_tot;

				double val1 = w1*line_cost_[old_line_id];
				double val2 = w2*line_cost_[old_line_id];

				// update line cost

				line_cost_[line1_id] = val1;
				line_cost_[line2_id] = val2;
				line_cost_.erase (old_line_id);

     			// update line set

				line_set_.add (l1);
				line_set_.add (l2);
				line_set_.erase (line);
			
				return true;
				}
			else return false;
			}
		else return false;
		}
	else return false;
}




bool
TeGraphNetwork:: nearestNetworkPoint (TeCoord2D& p1, int& lindex, TeCoord2D& pinter, double& distance, double tol)
{
		if (line_set_.size() == 0) return false;
		return TeNearest (p1, line_set_, lindex, pinter, distance, tol);

		bool TeNearest(TeCoord2D& pt, TeLineSet& ls , int& i, TeCoord2D& pi, const double& tol);

}


///////////////////////////////////// Old version - not modified

TeNetwork:: TeNetwork (TeLineSet& ls)  : graph_ (false)
{
	line_set_ = ls;


	// The initial and final points of each line will be the nodes
	TeLineSet::iterator it = ls.begin();
	int j = 0;
	while (it != line_set_.end())
	{
		string line_objId = "l" + Te2String (j);
		(*it).objectId (line_objId); //ANAP: modified to be able to use more than on layer.
		TeLine2D line = (*it);

		TeNode n1; 
		n1.add(line.first());
		string id1 = line.objectId() + "_p1"; 
		n1.objectId (id1);
		n1.geomId (0);

		TeNode n2; 
		n2.add(line.last());	
		string id2 = line.objectId() + "_p2";
		n2.objectId (id2);
		n2.geomId (0);

		for (unsigned int i = 0; i < graph_.size(); i++)
		{
			if (TeDistance (line.first(), graph_[i].first.location()) < 0.01)
				n1 = graph_[i].first;
			
			if (TeDistance (line.last(), graph_[i].first.location()) < 0.01)
				n2 = graph_[i].first;
		}

		graph_.insert (n1, n2, TeLength(line));

		TeArc arc (n1, n2);
		// This is kept for updating purposes
		arcs_map_[line.objectId()] = arc;
		it++;
		j++;
	}
}

void
TeNetwork:: insertLine (TeLine2D& line, const double& attr) 
{

	int j = line_set_.size();

	// The initial and final points of each line will be the nodes
	string line_objId = "l" + Te2String (j);
	line.objectId (line_objId); //ANAP: modified to be able to use more than one layer.

	TeNode n1; 
	n1.add(line.first());
	string id1 = line.objectId() + "_p1"; 
	n1.objectId (id1);
	n1.geomId (0);

	TeNode n2; 
	n2.add(line.last());	
	string id2 = line.objectId() + "_p2";

	n2.objectId (id2);
	n2.geomId (0);

	for (unsigned int i = 0; i < graph_.size(); i++)
	{
		if (TeDistance (line.first(), graph_[i].first.location()) < 0.01)
			n1 = graph_[i].first;
			
		if (TeDistance (line.last(), graph_[i].first.location()) < 0.01)
			n2 = graph_[i].first;
	}

	graph_.insert (n1, n2, attr);

	TeArc arc (n1, n2);
	// This is kept for updating purposes
	arcs_map_[line.objectId()] = arc;
	line_set_.add (line);

}


bool 
TeNetwork:: insertNode (TeNode& node, int line)
{
	if ((line >= 0) && (line < (int) line_set_.size()))
	{
		// Break line
		int segment;
		if (TeLocateLineSegment (node.location(), line_set_[line], segment, 0.001))
		{
			if (segment <= (int) (line_set_[line].size()) - 2)
			{
				TeLine2D l1, l2;
				int j = 0;
				TeLine2D::iterator it = line_set_[line].begin();

				while (j <= segment)
				{
					l1.add (*it);
					it++;
					j++;
				}

				l1.add (node.location());
				l2.add (node.location ());

				while (it != line_set_[line].end())
				{
					l2.add (*it);
					it++;
				}

				// get new and old lines ids

				string old_line_id = line_set_[line].objectId();
				string line1_id = old_line_id + "l1";
				string line2_id = old_line_id + "l2";

				l1.objectId(line1_id);
				l2.objectId(line2_id);
		
     			// update line set
				line_set_.add (l1);
				line_set_.add (l2);
				line_set_.erase (line);
	
				// update map_nodes 
				TeArc arc1 (arcs_map_[old_line_id].toNode(),  node);
				arcs_map_[line1_id] = arc1;

				TeArc arc2 (arcs_map_[old_line_id].fromNode(),node);
				arcs_map_[line2_id] = arc2;
				arcs_map_.erase(old_line_id);


/*			// insert new arc connection in the network
			graph_.insert (arcs_map_ [old_line_id].fromNode(),node, l1.length()); 
			graph_.insert (arcs_map_ [old_line_id].toNode(),  node, l2.length()); 
		//	graph_.remove (arcs_map_[old_line_id].fromNode(), _map_nodes[old_line_id].toNode()); 

			
*/
				return true;
				}
			else return false;
			}
		else return false;
		}
	else return false;
}


bool 
TeNetwork::getNode (int i, TeNode& node)
{ 
	if ((i >0) && (i < (int) graph_.size())) 
	{
		node = graph_[i].first;
		return true;
	}
	else 
		return false;
}


bool 
TeNetwork:: minimumPath (TeNode& n1, TeNodeSet& nodeSet,vector<double>& result)
{
	vector<double> dist;
	vector<int> pred;

	// Compute minimum path to all vertex
	unsigned int j = 0;
	br_stl::Graph<TeNode, double>::iterator it = graph_.begin();
	while (it != graph_.end())
	{
	   if ((*it).first == n1)  
		    it = graph_.end();
	   else
	   {
		   it++;
		   j++;
	   }
	}
	if (j == graph_.size()) return false;

	br_stl::Dijkstra<br_stl::Graph<TeNode, double>, double> (graph_, dist, pred, j);

	for (unsigned int i = 0; i< nodeSet.size(); i++)
	{
		br_stl::Graph<TeNode, double>::iterator it = graph_.begin();
		unsigned int vertex = 0;
		while (it != graph_.end())
		{
			if ((*it).first == nodeSet[i])  
				it = graph_.end();
			else
			{  
				it++;
				vertex++;
			}
		}

		if (vertex == graph_.size()) return false;
		result.push_back (dist[vertex]);
	}


	return true;

}


bool
TeNetwork:: nearestNodePoint (TeCoord2D& p1, int& index, TeCoord2D& p2, double& distance, double tol)
{
		TeNodeSet node_set;
		for (unsigned int i = 0; i < graph_.size(); i++) 
			node_set.add (graph_[i].first);

		if (node_set.size() == 0) return false;

		TeNearest (p1, node_set, index, tol);
		p2 = node_set[index].elem();
		distance = TeDistance (p1, p2);
		return true;
}

bool
TeNetwork:: nearestNetworkPoint (TeCoord2D& p1, int& lindex, TeCoord2D& pinter, double& distance, double tol)
{
		if (line_set_.size() == 0) return false;
		return TeNearest (p1, line_set_, lindex, pinter, distance, tol);

		bool TeNearest(TeCoord2D& pt, TeLineSet& ls , int& i, TeCoord2D& pi, const double& tol = 0.0);

}
