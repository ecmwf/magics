/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001, 2002, 2003 INPE and Tecgraf/PUC-Rio.

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

#include "TeProxMatrixSlicingStrategy.h"
#include "TeProxMatrixWeightsStrategy.h"


// zone slicing strategy
bool TeProxMatrixZonesSlicingStrategy::Slice (TeProxMatrixImplementation* imp)
{
	if (imp == 0) return false;

	for (int obj = 0; obj < imp->numberOfObjects(); obj++)
	{
		TeNeighbours neigh;
		string object_id;
		TeProxMatrixAttributes attr;
		if (imp->getNeighbours (obj, object_id, neigh))
		{
			for (int i = 0; i < neigh.size(); i++)
			{
				attr = neigh.Attributes(i);
		
				double distance, d_centr, d_net, d_conn;

				if (attr.WasCentroidDistanceComputed())
					d_centr = attr.CentroidDistance();
				else
					d_centr = 0;
				

				if (attr.WasNetworkObjectsDistanceComputed())
					d_net = attr.NetworkObjectsDistance();
				else
					d_net = 0;
				

				if (attr.WasNetworkMinimumPathComputed())
					d_conn = attr.NetworkMinimumPath();
				else
					d_conn = 0;
				
			
			   if (params_.zone_local_) 
				   distance = d_centr;
			   else
				   distance = d_net + d_conn;

			   int zone = (int) (distance/params_.zone_dist_);
			   attr.Slice (zone);
			   imp->setConnectionAttributes (object_id, neigh[i], attr);

			}

		}
	}
	return true;
}


// inverse distance weight strategy
bool TeProxMatrixInverseDistanceStrategy:: 
ComputeWeigths (TeProxMatrixImplementation* imp)
{
	if (imp == 0) return false;

	for (int obj = 0; obj < imp->numberOfObjects(); obj++)
	{
		TeNeighbours neigh;
		string object_id;
		TeProxMatrixAttributes attr;
		if (imp->getNeighbours (obj, object_id, neigh))
		{
			double tot = 0.0;
			vector<double> w_vec;
	
			for (int i = 0; i < neigh.size(); i++)
			{
				TeProxMatrixAttributes attr = neigh.Attributes(i);
				double w = 1;
				double d_centr, d_net, d_conn;
				if (attr.WasCentroidDistanceComputed())
					if ((d_centr = attr.CentroidDistance()) != 0.0) 
							w += (params_.a_)*1/d_centr;

				if (attr.WasNetworkObjectsDistanceComputed())
					if ((d_net = attr.NetworkObjectsDistance()) != 0.0) 
							w += (params_.b_)*1/d_net;

				if (attr.WasNetworkMinimumPathComputed())
					if ((d_conn = attr.NetworkMinimumPath()) != 0.0) 
							w += (params_.c_)*1/d_conn;

				if (w != 1) w -= 1;

				w_vec.push_back (w*params_.factor_);
				tot += w;
			}
	
		
			for (int j = 0; j < neigh.size(); j++)
			{
					TeProxMatrixAttributes attr = neigh.Attributes(j);
					double w = w_vec[j];
					if (params_.norm_)
						if (tot != 0) w = w/tot;
					attr.Weight (w);
					imp->setConnectionAttributes (object_id, neigh[j], attr);
			}
		}
	}
	return true;
}



// inverse distance weight strategy
bool TeProxMatrixSquaredInverseDistanceStrategy:: 
ComputeWeigths (TeProxMatrixImplementation* imp)
{
	if (imp == 0) return false;

	for (int obj = 0; obj < imp->numberOfObjects(); obj++)
	{
		TeNeighbours neigh;
		string object_id;
		TeProxMatrixAttributes attr;
		if (imp->getNeighbours (obj, object_id, neigh))
		{
			double tot = 0.0;
			vector<double> w_vec;
	
			for (int i = 0; i < neigh.size(); i++)
			{
				TeProxMatrixAttributes attr = neigh.Attributes(i);
				double w = 1;
				double d_centr, d_net, d_conn;
				if (attr.WasCentroidDistanceComputed())
					if ((d_centr = attr.CentroidDistance()) != 0.0) 
							w += (params_.a_)*1/(d_centr*d_centr);

				if (attr.WasNetworkObjectsDistanceComputed())
					if ((d_net = attr.NetworkObjectsDistance()) != 0.0) 
							w += (params_.b_)*1/(d_net*d_net);

				if (attr.WasNetworkMinimumPathComputed())
					if ((d_conn = attr.NetworkMinimumPath()) != 0.0) 
							w += (params_.c_)*1/(d_conn*d_conn);

				if (w != 1) w -= 1;

				w_vec.push_back (w*params_.factor_);
				tot += w;
			}
	
		
			for (int j = 0; j < neigh.size(); j++)
			{
					TeProxMatrixAttributes attr = neigh.Attributes(j);
					double w = w_vec[j];
					if (params_.norm_)
						if (tot != 0) w = w/tot;
					attr.Weight (w);
					imp->setConnectionAttributes (object_id, neigh[j], attr);
			}
		}
	}
	return true;
}

// inverse distance weight strategy
bool TeProxMatrixConnectionStrenghtStrategy:: 
ComputeWeigths (TeProxMatrixImplementation* imp)
{
	if (imp == 0) return false;

	for (int obj = 0; obj < imp->numberOfObjects(); obj++)
	{
		TeNeighbours neigh;
		string object_id;
		TeProxMatrixAttributes attr;
		if (imp->getNeighbours (obj, object_id, neigh))
		{
			double tot = 0.0;
			vector<double> w_vec;
			
			for (int i = 0; i < neigh.size(); i++)
			{
				TeProxMatrixAttributes attr = neigh.Attributes(i);
				double w = 1;
				double d_centr = TeMAXFLOAT;
				double d_net =   TeMAXFLOAT;
				double d_conn =  TeMAXFLOAT;

				if (attr.WasCentroidDistanceComputed())
					d_centr = attr.CentroidDistance();

				
				if (d_centr == 0.0)
				{
					w = 1;
				}
				else
				{
					if (d_centr <= params_.max_local_dist_)
					{
						w = 1/d_centr;
					}
					else
					{
					
						if (attr.WasNetworkObjectsDistanceComputed())
							d_net = attr.NetworkObjectsDistance();

						if (attr.WasNetworkMinimumPathComputed())
							d_conn = attr.NetworkMinimumPath();

						//double distance = (dist_ratio_*d_net + d_conn)/1000; Anap - Jul04
						double distance = (params_.dist_ratio_*d_net + d_conn);
						if (distance != 0.0)
							w = 1/distance;
					}
				}
				//if (w > 1.0) w = 1.0; não precisa: o objeto pode ser mais próximo que 1 metro? jul04
				w_vec.push_back (w*params_.factor_);
				tot += w;
			}
		
			for (int j = 0; j < neigh.size(); j++)
			{
					
				TeProxMatrixAttributes attr = neigh.Attributes(j);
				double w = w_vec[j];
				if (params_.norm_)
					if (tot != 0) w = w/tot;
				attr.Weight (w);
				imp->setConnectionAttributes (object_id, neigh[j], attr);
				
			}
		}
	}
	return true;
}



bool TeProxMatrixNoWeightsStrategy:: 
ComputeWeigths (TeProxMatrixImplementation* imp)
{
	if (imp == 0) return false;
	// Normalize
	if (params_.norm_)
	{
		for (int obj = 0; obj < imp->numberOfObjects(); obj++)
		{
			TeNeighbours neigh;
			string object_id;
			TeProxMatrixAttributes attr;
			if (imp->getNeighbours (obj, object_id, neigh))
			{
	
				for (int j = 0; j < neigh.size(); j++)
				{
					double size = (double) neigh.size();
					double w = 1.0/size;
					TeProxMatrixAttributes attr = neigh.Attributes(j);
				
					attr.Weight (w);
					imp->setConnectionAttributes(object_id, neigh[j], attr);
				}
			}
		}
	}
	return true;
}

