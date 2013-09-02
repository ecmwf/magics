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


#include "TeProxMatrixConstructionStrategy.h"
#include "TeProgress.h"
#include "TeSTEvent.h"
#include "TeSTInstance.h"

//////////////////////////////////////////////////////////////////////
// Auxiliary Functions for construction strategies
//////////////////////////////////////////////////////////////////////

bool
TeFindObjectCentroid (TeMultiGeometry& mGeom, TeGeomRep rep, TeCoord2D& p)
{		
		if(rep == TePOLYGONS)
		{
			TePolygonSet pols;
			if(mGeom.getGeometry(pols))
				p = TeFindCentroid (pols);
			else
				return false;
		}
		else if (rep == TeLINES)
		{
			TeLineSet lines;
			if(mGeom.getGeometry (lines))
				p = TeFindCentroid (lines);
			else
				return false;
		}
		else if (rep == TePOINTS)
		{
			TePointSet points;
			if(mGeom.getGeometry (points))
				p = TeFindCentroid (points);
			else
				return false;
		}
		else if (rep == TeCELLS)
		{
			TeCellSet cells;
			if(mGeom.getGeometry (cells))
				p = TeFindCentroid (cells);
			else
				return false;
		}
		else if (rep == TeTEXT)
		{
			TeTextSet texts;
			if(mGeom.getGeometry (texts))
				p = TeFindCentroid (texts);
			else
				return false;
		}		
		return true;
}

bool
TeFindObjectsCentroid (TeSTElementSet* objects, const string& object_id, TeGeomRep rep, TeCoord2D& p)
{
	if(rep==TePOLYGONS)
	{
		TePolygonSet polygons;
		if(objects->getGeometry (object_id, polygons))
			p = TeFindCentroid (polygons);
	}
	else if (rep==TeLINES)
	{
		TeLineSet lines;
		if(objects->getGeometry (object_id, lines))
			p = TeFindCentroid (lines);		
	}
	else if (rep==TePOINTS)
	{
		TePointSet points;
		if(objects->getGeometry (object_id, points))
			p = TeFindCentroid (points);					
	}
	else if (rep==TeCELLS)
	{
		TeCellSet cells;
		if(objects->getGeometry(object_id, cells))
			p = TeFindCentroid (cells);
	}
	return true;
}


bool  TeSelectEntryPoints (TeNodeSet  entry_set, map<string, string> entry_geom_map, TeGraphNetwork* net,
						double max_dist, TeSTElementSet* objects, TeGeomRep rep, TeProxMatrixImplementation* imp)
{

	// Compute minimum path for all the new nodes/geometries near the network
	// and connect the ones close enough
	TeNodeSet::iterator it_node1 = entry_set.begin();
	while ( it_node1 != entry_set.end())
	{
		vector<double> minimum_path;
		if (net->minimumPath ((*it_node1), entry_set, minimum_path))
		{
		int j  = 0;
		// Check maximum connection distance and connect selected objects
		TeNodeSet::iterator it_node2 = entry_set.begin();
		while ( it_node2 != entry_set.end())
		{
			// Get ids to simplify the code
			string node1_id   = (*it_node1).objectId(); // Created Node id, nearest point to Geom1
			string node2_id   = (*it_node2).objectId();	// Created Node id, nearest point to Geom1		
			string object1_id = entry_geom_map[node1_id]; // Input Geom1 id
			string object2_id = entry_geom_map[node2_id]; // Input Geom2 id

			// Check identity and maximum allowed distance through the network
	  		if ((object1_id != object2_id) && (minimum_path[j] <= max_dist))
				{	
					// Compute attributes and connect objects
					TeProxMatrixAttributes attr;
					
					TeCoord2D p1, p2;
					if (!TeFindObjectsCentroid (objects, object1_id, rep, p1))
						return false;

					if (!TeFindObjectsCentroid (objects, object2_id, rep, p2))
						return false;
				
					// Local istance between input geometries
					attr.CentroidDistance (TeDistance (p1, p2));

					// Total distance from input geometries centroids to network
					attr.NetworkObjectsDistance (TeDistance (p1, (*it_node1).location()) + 
												 TeDistance (p2, (*it_node2).location()));	
				
					// Minimum path from the nodes relatives to the two geometries
					attr.NetworkMinimumPath (minimum_path[j]);  
			
					imp->connectObjects (object1_id, object2_id, attr); // for networks, only one direction is connected
				}
			it_node2++;
			j++;
		}
		}
		it_node1++;
	}
	return true;
}


bool  TeSelectEntryPoints2 (TeNodeSet    entry_set1,
						  TeNodeSet    entry_set2,
						map<string, string> entry_geom_map,
						TeGraphNetwork* net,
						double max_dist, 
						TeSTElementSet* objects1,
						TeGeomRep rep1,
						TeSTElementSet* objects2,
						TeGeomRep rep2,
						TeProxMatrixImplementation* imp)
{

	// Compute minimum path for all the new nodes/geometries near the network
	// and connect the ones close enough
	TeNodeSet::iterator it_node1 = entry_set1.begin();
	while ( it_node1 != entry_set1.end())
	{
		vector<double> minimum_path;
		net->minimumPath ((*it_node1), entry_set2, minimum_path); 
		if (minimum_path.size() > 0)
		{
			int j  = 0;
		// Check maximum connection distance and connect selected objects
			TeNodeSet::iterator it_node2 = entry_set2.begin();
			while ( it_node2 != entry_set2.end())
			{
			    // Get ids to simplify the code
				string node1_id   = (*it_node1).objectId(); // Created Node id, nearest point to Geom1
				string node2_id   = (*it_node2).objectId();	// Created Node id, nearest point to Geom1		
				string object1_id = entry_geom_map[node1_id]; // Input Geom1 id
				string object2_id = entry_geom_map[node2_id]; // Input Geom2 id
	
		
				// Check identity and maximum allowed distance through the network
	  			if ((object1_id != object2_id) && (minimum_path[j] <= max_dist))
				{
					
					// Compute attributes and connect objects
					TeProxMatrixAttributes attr;

					TeCoord2D p1, p2;
					if (!TeFindObjectsCentroid (objects1, object1_id, rep1, p1))
						return false;

					if (!TeFindObjectsCentroid (objects2, object2_id, rep2,  p2))
						return false;
		
					// Local distance
					attr.CentroidDistance (TeDistance (p1, p2));

					// Total distance from input geometries centroids to network
					attr.NetworkObjectsDistance (TeDistance (p1, (*it_node1).location()) + 
												 TeDistance (p2, (*it_node2).location()));	
				
					// Minimum path from the nodes relatives to the two geometries
					attr.NetworkMinimumPath (minimum_path[j]);  
			
					imp->connectObjects (object1_id, object2_id, attr); // for networks, only one direction is connected
				}
				it_node2++;
				j++;
			}
		}
		it_node1++;
	}
	return true;
}


//////////////////////////////////////////////////////////////////////
// TeProxMatrixLocalAdjacencyStrategy 
//////////////////////////////////////////////////////////////////////

TeProxMatrixLocalAdjacencyStrategy::TeProxMatrixLocalAdjacencyStrategy () : 
		TeProxMatrixConstructionStrategy<TeSTElementSet> (0, TeGEOMETRYNONE, TeAdjacencyStrategy)
		{ }


TeProxMatrixLocalAdjacencyStrategy::TeProxMatrixLocalAdjacencyStrategy (TeSTElementSet*  objects, TeGeomRep	geomRep, bool calcDistance):
		TeProxMatrixConstructionStrategy<TeSTElementSet> (objects, geomRep, TeAdjacencyStrategy)
		{
			params_.calculate_distance_=calcDistance;
		}


TeProxMatrixLocalAdjacencyStrategy::TeProxMatrixLocalAdjacencyStrategy (TeProxMatrixLocalAdjacencyStrategy& st):
		TeProxMatrixConstructionStrategy<TeSTElementSet> (st)
		{ }

bool 
TeProxMatrixLocalAdjacencyStrategy::Construct (TeProxMatrixImplementation* imp)
{
	if (imp == 0) 
		return false;
	
	// Iterate over all selected objects, selecting their neighbours
	TeSTElementSet::iterator itobj1 = objects_->begin();
	
	// ----- progress bar
	int step = 0;
	if(TeProgress::instance())
		TeProgress::instance()->setTotalSteps(objects_->numSTInstance());
	// -----

	TeProjection* proj = 0;
	if(objects_->theme())
		proj = objects_->theme()->layer()->projection();
	else if(objects_->getLayer())
		proj = objects_->getLayer()->projection();
	TePrecision::instance().setPrecision(TeGetPrecision(proj));
		
	while ( itobj1 != objects_->end())
	{
		// Gets the possible adjacent objects from RTree in the element set
		vector<TeSTInstance*> result;
		objects_->search((*itobj1).getGeometries().getBox(), result);
		string object_id1 = (*itobj1).getObjectId(); 

		TePolygonSet	polSet1;
		TeCellSet		cellSet1;
		itobj1->getGeometry(polSet1);
		itobj1->getGeometry(cellSet1);
        
		for(unsigned int i=0; i<result.size(); ++i)
		{
			string object_id2 = result[i]->getObjectId(); 

			if(object_id1==object_id2)
				continue;
			
			bool touch = false; 
			if(params_.geom_rep1_==TePOLYGONS)
			{				
				TePolygonSet polSet2;
				result[i]->getGeometry(polSet2);
				for(unsigned int index=0; index<polSet1.size(); ++index)
				{					
					for(unsigned int index2=0; index2<polSet2.size(); ++index2)
					{
                        bool curTouches = TeTouches(polSet1[index], polSet2[index2]);
						if(touch && !curTouches)
						{
							//verifies if the current polygons are disjunt
							if(!TeDisjoint(polSet1[index], polSet2[index2]))
							{
								touch = false;
								index = polSet1.size();
								break;
							}
						}
						touch = curTouches;
					}
				}
			}
			else if(params_.geom_rep1_==TeCELLS)
			{
				TeCellSet cellSet2;
				result[i]->getGeometry(cellSet2);
				for(unsigned int index=0; index<cellSet1.size(); ++index)
				{					
					for(unsigned int index2=0; index2<cellSet2.size(); ++index2)
					{
                        bool curTouches = TeTouches(cellSet1[index], cellSet2[index2]);
						if(touch && !curTouches)
						{
							//verifies if the current polygons are disjoint
							if(!TeDisjoint(cellSet1[index], cellSet2[index2]))
							{
								touch = false;
								index = cellSet1.size();
								break;
							}
						}
						touch = curTouches;
					}
				}
			}

			if(touch)
			{
				if (!imp->isConnected (object_id1,object_id2))
				{
					TeProxMatrixAttributes attr;

					if(params_.calculate_distance_)
					{
						TeCoord2D p1 = (*itobj1).getCentroid();
						TeCoord2D p2 = result[i]->getCentroid();
						attr.CentroidDistance (TeDistance (p1, p2)); 
					}
					imp->connectObjects (object_id2, object_id1, attr);
					imp->connectObjects (object_id1, object_id2, attr);
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


TeProxMatrixLocalAdjacencyStrategy& 
TeProxMatrixLocalAdjacencyStrategy::operator= (const TeProxMatrixLocalAdjacencyStrategy& rhs)
{
	if ( this != &rhs )
	{
		objects_ = rhs.objects_;
		params_ = rhs.params_;
	}
	return *this;
}

bool 
TeProxMatrixLocalAdjacencyStrategy::operator== (const TeProxMatrixLocalAdjacencyStrategy& rhs) const   
{ 
	return (TeProxMatrixConstructionStrategy<TeSTElementSet>::IsEqual(rhs)); 
}
	



//////////////////////////////////////////////////////////////////////
// TeProxMatrixNearestNeighbourStrategy 
//////////////////////////////////////////////////////////////////////
TeProxMatrixNearestNeighbourStrategy::TeProxMatrixNearestNeighbourStrategy ():
	TeProxMatrixConstructionStrategy<TeSTEventSet> () 
	{ }

TeProxMatrixNearestNeighbourStrategy::TeProxMatrixNearestNeighbourStrategy (TeSTEventSet* objects, int num_neighbours):
	TeProxMatrixConstructionStrategy<TeSTEventSet> (objects, TePOINTS, TeNearestNeighboursStrategy) 
	{
		params_.num_neighbours_=num_neighbours;
	}
	
TeProxMatrixNearestNeighbourStrategy::TeProxMatrixNearestNeighbourStrategy (const TeProxMatrixNearestNeighbourStrategy& st):
	TeProxMatrixConstructionStrategy<TeSTEventSet>(st)
	{} 
	
bool 
TeProxMatrixNearestNeighbourStrategy::Construct (TeProxMatrixImplementation* imp)
{
	if (imp == 0) 
		return false;

	// ----- progress bar
	int step = 0;
	if(TeProgress::instance())
		TeProgress::instance()->setTotalSteps(objects_->numSTInstance());
	// -----

	TePrecision::instance().setPrecision(TeGetPrecision(objects_->getTheme()->layer()->projection()));
	TeSTEventSet::iterator it = objects_->begin();
	while(it!=objects_->end())
	{
		TePoint p = (*it).getGeometries();
		string object_id1=(*it).getObjectId();

		vector<TeSTEvent*>  result;
		vector<double>  dists;
		
		if(!objects_->nearestNeighbourSearch(p.location(), result, dists, (params_.num_neighbours_+1)))
		{
			++it;
			continue;
		}

		for(unsigned int j=0; j<result.size(); ++j)
		{
			if(dists[j]==TeMAXFLOAT)
				continue;

			string object_id2 = result[j]->getObjectId();
		
			if ((object_id1 != object_id2) && (!imp->isConnected (object_id1,object_id2)))
			{
				TeProxMatrixAttributes attr;
				attr.CentroidDistance (dists[j]); 
				
				imp->connectObjects (object_id1, object_id2, attr);
			}
		}


		//------ progress bar
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
		
		++it;
	}

	if (TeProgress::instance())
		TeProgress::instance()->reset();
	return true; 
}

bool 
TeProxMatrixNearestNeighbourStrategy::operator== (const TeProxMatrixNearestNeighbourStrategy& s) const
{
	return (TeProxMatrixConstructionStrategy<TeSTEventSet>::IsEqual(s)); 
}
		
TeProxMatrixNearestNeighbourStrategy& 
TeProxMatrixNearestNeighbourStrategy::operator= (const TeProxMatrixNearestNeighbourStrategy& rhs)
{
	if ( this != &rhs )
	{
		objects_ = rhs.objects_;
		params_ = rhs.params_;
	}
	return *this;
}

//////////////////////////////////////////////////////////////////////
// TeProxMatrixClosedNetworkStrategy 
//////////////////////////////////////////////////////////////////////

TeProxMatrixClosedNetworkStrategy::TeProxMatrixClosedNetworkStrategy (TeSTElementSet*  objects,
										TeGeomRep rep, double max_local_distance,
										double max_net_distance, double  max_connection_distance, 
										TeGraphNetwork* input_net) : 	
	TeProxMatrixConstructionStrategy<TeSTElementSet>(objects, rep, TeClosedNetworkStrategy),
	net_ (input_net) 
	{
		params_.max_distance_= max_local_distance; 
		params_.max_net_distance_ = max_net_distance; 
		params_.max_connection_distance_= max_connection_distance; 
	}



TeProxMatrixClosedNetworkStrategy::TeProxMatrixClosedNetworkStrategy (const TeProxMatrixClosedNetworkStrategy& rhs) :
	TeProxMatrixConstructionStrategy<TeSTElementSet>(rhs)
{
		TeGraphNetwork* n = new TeGraphNetwork();
		*n = *rhs.net_; 
		net_ = n;
}

	  
bool 
TeProxMatrixClosedNetworkStrategy::Construct (TeProxMatrixImplementation* imp)
{
	if (imp == 0) return false;

	// Connect local neighbours, based on the Local Distance Strategy

	TeProxMatrixLocalDistanceStrategy<TeSTElementSet> local(objects_, params_.geom_rep1_, params_.max_distance_);
	local.Construct (imp);

	// Connect neighbours through the network. The process is the following:
	// 1. The nearest node (entry points) in the network for all the input geometries
	//    are computed;
	// 2. If the distance from the geometry centroid to the entry point is smaller than the maximum allowed, 
	//    create a new node in the network corresponding to this entry point.
	// 3. Compute the minimum path among all these new nodes (entry points).
	// 4. For each pair of entry points, if the minimum path distance is smaller than the
	//    maximum allowed, the corresponding geometries will be connected.

	if (net_ == 0) return false;

	map<string, string> entry_geom_map; // maps input geometries to network entry points
	TeNodeSet entry_set; // entry points

	// ----- progress bar
	int step = 0;
	if(TeProgress::instance())
		TeProgress::instance()->setTotalSteps(objects_->numSTInstance());
	// -----

	// Iterate over all objects and select the ones that are close enough to the network
	TeSTElementSet::iterator itobj1 = objects_->begin();
	while ( itobj1 != objects_->end())
	{
		// Get object1 id and representation
		string object_id1 = (*itobj1).getObjectId(); 
		TeCoord2D p1, p2; 
			

		int i = 0;
		double min_distance = 0.;
	
		net_->nearestNodePoint (p1, i, p2, min_distance);
				
		TeNode node;
		if ((min_distance <= params_.max_net_distance_) && net_->getNode(i, node))
		{
			entry_geom_map[node.objectId()] = object_id1; // Associates geometry with closest net nodes
			entry_set.add (node);  // This set will be used as initial points in the shortest path algorithm
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

	TeSelectEntryPoints (entry_set, entry_geom_map, net_, params_.max_connection_distance_, objects_, params_.geom_rep1_, imp);
	if (TeProgress::instance())
		TeProgress::instance()->reset();
		
	return true;
}


bool 
TeProxMatrixClosedNetworkStrategy::IsEqual (const TeProxMatrixConstructionStrategy<TeSTElementSet>& other) const
{
	return (*this == (TeProxMatrixClosedNetworkStrategy&) other); 
}
	
TeProxMatrixClosedNetworkStrategy& 
TeProxMatrixClosedNetworkStrategy::operator= (const TeProxMatrixClosedNetworkStrategy& rhs)
{
	if (!(this==&rhs))
	{
		objects_ = rhs.objects_;
		params_ = rhs.params_;
			
		TeGraphNetwork* n = new TeGraphNetwork(); 
		*n = *rhs.net_; 
		net_ = n;       
	}
	return (*this);
}

	
bool 
TeProxMatrixClosedNetworkStrategy::operator== (const TeProxMatrixClosedNetworkStrategy& other) const 
{
	return (TeProxMatrixConstructionStrategy<TeSTElementSet>::IsEqual(other));
}
	   
//////////////////////////////////////////////////////////////////////
// TeProxMatrixOpenNetworkStrategy 
//////////////////////////////////////////////////////////////////////

TeProxMatrixOpenNetworkStrategy::TeProxMatrixOpenNetworkStrategy (TeSTElementSet*  objects, TeGeomRep rep, 
										double max_local_distance, 
										double	max_net_distance, double  max_connetion_distance, 
										TeGraphNetwork* input_net) : 	
	TeProxMatrixConstructionStrategy<TeSTElementSet>(objects, rep, TeOpenNetworkStrategy),
	net_ (input_net)
	{
		params_.max_distance_= max_local_distance; 
		params_.max_net_distance_ = max_net_distance; 
		params_.max_connection_distance_= max_connetion_distance; 
	}


TeProxMatrixOpenNetworkStrategy::TeProxMatrixOpenNetworkStrategy (const TeProxMatrixOpenNetworkStrategy& rhs): 
	TeProxMatrixConstructionStrategy<TeSTElementSet>(rhs)
{
	TeGraphNetwork* n = new TeGraphNetwork();
	*n = *rhs.net_; 
	net_ = n;
}

bool 
TeProxMatrixOpenNetworkStrategy::Construct (TeProxMatrixImplementation* imp)
{
	if (imp == 0) 
		return false;

	// Connect local neighbours, based on the Local Distance Strategy
	if (params_.max_distance_ > 0)
	{
		TeProxMatrixLocalDistanceStrategy<TeSTElementSet> local(objects_, params_.geom_rep1_, params_.max_distance_);
		local.Construct (imp);
	}

	// Connect neighbours through the network. The process is the following:
	// 1. The nearest point (entry points) in the network for all the input geometries
	//    are computed;
	// 2. If the distance from the geometry centroid to the entry point is smaller than the maximum allowed, 
	//    create a new node in the network corresponding to this entry point.
	// 3. Compute the minimum path among all these new nodes (entry points).
	// 4. For each pair of entry points, if the minimum path distance is smaller than the
	//    maximum allowed, the corresponding geometries will be connected.
	if (net_ == 0) 
		return false;

	map<string, string> entry_geom_map; // maps input geometries to network entry points
	TeNodeSet entry_set; // entry points

	// Iterate over all objects and select the ones that are close enough to the network
	TeSTElementSet::iterator itobj1 = objects_->begin();
	int id = 0;  // Nodes to be created will have sequential object_ids. 
				 // The existing network nodes have object_id equal to their position
	while ( itobj1 != objects_->end())

	{
		// Get object1 id and representation
		string object_id1 = (*itobj1).getObjectId();
		cout << object_id1 << "id " << id << endl;

		TeCoord2D p1; 
		if (!TeFindObjectCentroid (itobj1->getGeometries(), params_.geom_rep1_, p1))
			return false;


		int line_index;
		TeCoord2D pinter;
		double min_dist;

		if (net_->nearestNetworkPoint (p1, line_index, pinter, min_dist))
		{
			if (min_dist < params_.max_net_distance_)
			{
				TeNode new_node;
				new_node.add (pinter);
				new_node.objectId (Te2String (id));
				id++;

				net_->breakLineSet (new_node, line_index);
				entry_geom_map[new_node.objectId()] = object_id1; // Associates geometry with closest net nodes
				entry_set.add (new_node);  // This set will be used as initial points in the shortest path algorithm
			}
		}
		++itobj1;
	}

    TeLineSet lineSet = net_->getLineSet();
    map<string, double> lineCosts =  net_->getLineCosts();
	TeGraphNetwork net(lineSet, lineCosts);
	TeSelectEntryPoints (entry_set, entry_geom_map, &net, params_.max_connection_distance_, objects_, params_.geom_rep1_, imp);
	if (TeProgress::instance())
		TeProgress::instance()->reset();
	return true;
}


bool 
TeProxMatrixOpenNetworkStrategy::IsEqual (const TeProxMatrixConstructionStrategy<TeSTElementSet>& other) const
{
	return ((*this)==(TeProxMatrixOpenNetworkStrategy&)other); 
}

bool 
TeProxMatrixOpenNetworkStrategy::operator== (const TeProxMatrixOpenNetworkStrategy& other) const 
{
	return (TeProxMatrixConstructionStrategy<TeSTElementSet>::IsEqual(other));
}


TeProxMatrixOpenNetworkStrategy& 
TeProxMatrixOpenNetworkStrategy::operator= (const TeProxMatrixOpenNetworkStrategy& rhs)
{
	if (!(this==&rhs))
	{
		params_ = rhs.params_;
		objects_ = rhs.objects_;
		
		TeGraphNetwork* n = new TeGraphNetwork(); 
		*n = *rhs.net_;                           
		net_ = n;								
	}
	return (*this);
}


/////////////////////////////////////////////////////////////////////
// TeProxMatrixOpenNetworkStrategy2 
//////////////////////////////////////////////////////////////////////

TeProxMatrixOpenNetworkStrategy2::TeProxMatrixOpenNetworkStrategy2 (TeSTElementSet*  objects1, 
										TeGeomRep rep1,
										TeSTElementSet*  objects2, TeGeomRep rep2,
										double max_local_distance, 
										double	max_net_distance, double  max_connetion_distance, 
										TeGraphNetwork* input_net) : 	
	TeProxMatrixConstructionStrategy<TeSTElementSet>(objects1, rep1, TeOpenNetworkStrategy2),
	objects2_ (objects2),
	net_ (input_net)
	{
		if(objects2_->theme())
			params_.theme_id2_=objects2_->theme()->id();
		params_.geom_rep2_=rep2;
		params_.max_distance_= max_local_distance; 
		params_.max_net_distance_ = max_net_distance; 
		params_.max_connection_distance_= max_connetion_distance; 
	}  

TeProxMatrixOpenNetworkStrategy2::TeProxMatrixOpenNetworkStrategy2 (const TeProxMatrixOpenNetworkStrategy2& rhs)
                    : TeProxMatrixConstructionStrategy<TeSTElementSet>(rhs)
{
	objects2_ = rhs.objects2_;
	params_ = rhs.params_;
	
	TeGraphNetwork* n = new TeGraphNetwork();
	*n = *rhs.net_; 
	net_ = n;
}


bool 
TeProxMatrixOpenNetworkStrategy2:: Construct (TeProxMatrixImplementation* imp)
	{
		if (imp == 0) 
			return false;


		// Connect neighbours through the network. The process is the following:
		// 1. The nearest point (entry points) in the network for all the input geometries
		//    are computed;
		// 2. If the distance from the geometry centroid to the entry point is smaller than the maximum allowed, 
		//    create a new node in the network corresponding to this entry point.
		// 3. Compute the minimum path among all these new nodes (entry points).
		// 4. For each pair of entry points, if the minimum path distance is smaller than the
		//    maximum allowed, the corresponding geometries will be connected.
		if (net_ == 0) 
			return false;

		map<string, string> entry_geom_map; // maps input geometries to network entry points

	
		int id = 0;  // Nodes to be created will have sequential object_ids. 
					 // The existing network nodes have object_id equal to their position
		// Iterate over all objects and select the ones that are close enough to the network

		TeNodeSet entry_set1; // entry points
		TeSTElementSet::iterator itobj1 = objects_->begin();	
		while ( itobj1 != objects_->end())
		{
			// Get object1 id and representation
			string object_id1 = (*itobj1).getObjectId();
					cout << "object1 " << object_id1 << "id" << id << endl;

			TeCoord2D p1; 
			if (!TeFindObjectCentroid (itobj1->getGeometries(), params_.geom_rep1_, p1))
				return false;
		

			int line_index;
			TeCoord2D pinter;
			double min_dist;

			if (net_->nearestNetworkPoint (p1, line_index, pinter, min_dist))
			{
				if (min_dist < params_.max_net_distance_)
				{
					TeNode new_node;
					new_node.add (pinter);
					new_node.objectId (Te2String (id));
					id++;

					net_->breakLineSet (new_node, line_index);
					entry_geom_map[new_node.objectId()] = object_id1; // Associates geometry with closest net nodes
					entry_set1.add (new_node);  // This set will be used as initial points in the shortest path algorithm
				}
			}
			++itobj1;
		}


	// Iterate over all objects of interst (objects2) 
		TeNodeSet entry_set2; // entry points
		TeSTElementSet::iterator itobj2 = objects2_->begin();		
		while ( itobj2 != objects2_->end())
		{
			// Get object1 id and representation
			string object_id2 = (*itobj2).getObjectId();
					cout << "object2 " << object_id2 << "id" << id << endl;

			TeCoord2D p2; 
			if (!TeFindObjectCentroid (itobj1->getGeometries(), params_.geom_rep2_, p2))
				return false;
		
			int line_index;
			TeCoord2D pinter;
			double min_dist;

			if (net_->nearestNetworkPoint (p2, line_index, pinter, min_dist))
			{ 
				TeNode new_node;
				new_node.add (pinter);
				new_node.objectId (Te2String (id));
				id++;

				net_->breakLineSet (new_node, line_index);
				entry_geom_map[new_node.objectId()] = object_id2; // Associates geometry with closest net nodes
				entry_set2.add (new_node);  // This set will be used as initial points in the shortest path algorithm
			}
			++itobj2;
		}

        TeLineSet lineSet = net_->getLineSet();
        map<string, double> lineCosts = net_->getLineCosts();
		TeGraphNetwork net(lineSet, lineCosts);
		TeSelectEntryPoints2 (entry_set1, entry_set2, entry_geom_map, &net, params_.max_connection_distance_, 
			objects_, params_.geom_rep1_, objects2_, params_.geom_rep2_, imp);
			return true;
	}

bool 
TeProxMatrixOpenNetworkStrategy2::IsEqual (const TeProxMatrixConstructionStrategy<TeSTElementSet>& other) const
{
	return ((*this)==(TeProxMatrixOpenNetworkStrategy2&)other); 
}

bool 
TeProxMatrixOpenNetworkStrategy2:: operator== (const TeProxMatrixOpenNetworkStrategy2& other) const 
{
	return (TeProxMatrixConstructionStrategy<TeSTElementSet>::IsEqual(other));
}

TeProxMatrixOpenNetworkStrategy2& 
TeProxMatrixOpenNetworkStrategy2:: operator= (const TeProxMatrixOpenNetworkStrategy2& rhs)
{
		if (!(this==&rhs))
		{
			params_ = rhs.params_;
			objects_ = rhs.objects_;
			objects2_ = rhs.objects2_;
		
			TeGraphNetwork* n = new TeGraphNetwork(); 
			*n = *rhs.net_;  
			net_ = n;       
		}
		return (*this);
}


