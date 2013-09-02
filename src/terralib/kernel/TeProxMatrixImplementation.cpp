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

#include "TeProxMatrixImplementation.h"

#include <stdio.h>

TeProxMatrixGraphBreymann:: TeProxMatrixGraphBreymann () : 
      TeProxMatrixImplementation(TeGraphBreymann), graph_(true)
{ }

TeProxMatrixGraphBreymann:: TeProxMatrixGraphBreymann (TeProxMatrixGraphBreymann&  other) : 
      TeProxMatrixImplementation(TeGraphBreymann), graph_(true)
{	
//	graph_ = other.graph_;
	map_ = other.map_;
}

	
TeProxMatrixGraphBreymann&
TeProxMatrixGraphBreymann:: operator= (TeProxMatrixGraphBreymann& other)
{
	type_ = other.type_;
//	graph_ = other.graph_;  
	map_ = other.map_;
	return (*this);
}
 

void 
TeProxMatrixGraphBreymann::connectObjects (const string& object_id1, const string& object_id2, const TeProxMatrixAttributes& attr)
{
	int pos1 = graph_.insert (object_id1);
	int pos2 = graph_.insert (object_id2);

	graph_.connectVertices (pos1, pos2, attr);

	map_[object_id1] = pos1;
	map_[object_id2] = pos2;
}


bool 
TeProxMatrixGraphBreymann::setConnectionAttributes (const string& object_id1, const string& object_id2, const TeProxMatrixAttributes& attr)
{
	return(graph_.setEdgeValue (object_id1, object_id2, attr));
}

bool 
TeProxMatrixGraphBreymann::getConnectionAttributes (const string& object_id1, const string&object_id2, TeProxMatrixAttributes& attr)
{
	map_iterator pos1, pos2;

	if ((pos1 = map_.find(object_id1)) == map_.end()) 
		return false; // object_id1 not in the graph

	if ((pos2 = map_.find(object_id2)) == map_.end()) 
		return false; // object_id2 not in the graph

	// Object_id1 neighbours iterator
	br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
			start	= graph_[(*pos1).second].second.begin(),
			end		= graph_[(*pos1).second].second.end();

	while (start != end) 
	{	
		if ((*start).first == (*pos2).second) {
				attr = (*start).second;
				return true;
		}
		++start;
	}
	return false;		
}

bool 
TeProxMatrixGraphBreymann::getNeighboursNeighbours (const string& object_id, TeNeighbours& neigh, int max_order )
{
	bool ret_value = getNeighboursNeighbours (object_id, neigh, max_order, 1);
	neigh.Remove (object_id);
	return ret_value;
}

bool 
TeProxMatrixGraphBreymann::getNeighboursNeighbours (const string& object_id, TeNeighbours& neigh, int max_order, int current_order)
{

	map_iterator pos;

	if ((pos = map_.find(object_id)) == map_.end()) 
		return false; // object_id not in the graph

	// Object_id1 neighbours iterator
	br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
			start	= graph_[(*pos).second].second.begin(),
			end		= graph_[(*pos).second].second.end();

	// Insert all neighbours
	while (start != end) 
	{	
		TeProxMatrixAttributes attr = (*start).second;
		attr.Order (current_order); //Update attr attribute for that specific object
		if (neigh.Insert (graph_[(*start).first].first, attr)) 
		{
				// Get neighbours of neighbours that were not inserted before
			for (int recursive_loop = max_order - 1; recursive_loop > 0; --recursive_loop)
				getNeighboursNeighbours (graph_[(*start).first].first, neigh, recursive_loop, ++current_order);
	
		}
		++start;
	}
	return true;

}


bool
TeProxMatrixGraphBreymann::getNeighbours (const string& object_id, TeNeighbours& neigh)
{
	map_iterator pos;

	if ((pos = map_.find(object_id)) == map_.end()) 
		return false; // object_id not in the graph

	br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
			start	= graph_[(*pos).second].second.begin(),
			end		= graph_[(*pos).second].second.end();

	while (start != end) 
	{
		neigh.Insert (graph_[(*start).first].first, (*start).second);
		++start;
	}

	return true;
	
}

bool
TeProxMatrixGraphBreymann::getNeighbours (int i, string& object_id, TeNeighbours& neigh)
{

	if (i > (int) graph_.size())
		return false;

	// find object_id
	map_iterator pos = map_.begin();
	while (pos != map_.end())
	{
		if ((*pos).second == i)
		{
			object_id = (*pos).first;
			pos = map_.end();
		}
		else pos++;
	}

	br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
			start	= graph_[i].second.begin(),
			end		= graph_[i].second.end();


	while (start != end) 
	{
		neigh.Insert (graph_[(*start).first].first, (*start).second);
		start++;
	}

	return true;
	
}



TeProxMatrixImplementation* 
TeProxMatrixGraphBreymann::createCopy ()
{
	TeProxMatrixGraphBreymann* imp = new TeProxMatrixGraphBreymann (*this);
	return imp;

}


bool
TeProxMatrixGraphBreymann::saveTextFile (const string& name, map<string, string>* ids)
{
	string complete_name = name + ".txt";

	FILE*	fp = fopen(complete_name.c_str(),"w");
	if (fp)
	{
		fprintf (fp, "%d\n", ids->size()); //number of objects
	
		map<string, string>::iterator it;
		for (unsigned int i = 0; i < graph_.size(); i++)
		{

			string objId1, objId2;
			objId1 = graph_[i].first;
			if(ids)
			{
				it=ids->find(graph_[i].first);
				if(it!=ids->end())
					objId1 = it->second;
			}
						
			fprintf (fp, " %s ", objId1.c_str());
			
			br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
				start	= graph_[i].second.begin(),
				end		= graph_[i].second.end();
			//double sum = 0.0;
			while (start != end) 
			{
				objId2 = graph_[(*start).first].first;
				if(ids)
				{
					it=ids->find(objId2);
					if(it!=ids->end())
						objId2 = it->second;
				}
				
				fprintf (fp, " %s ", objId2.c_str());  
				//sum += (*start).second.Weight();
				start++;
			}

			//fprintf (fp, "Weights sum: %3.7f\n", sum);
			fprintf (fp, "\n");
		}
		fclose (fp);
		return true;
	}
	else 
		return false;
}


bool
TeProxMatrixGraphBreymann::saveGALFile (const string& name, map<string, string>* ids)
{

	string complete_name = name + ".GAL";
	FILE*	fp = fopen(complete_name.c_str(),"w");
	if (fp)
	{
		fprintf (fp, "%d\n", ids->size() ); // first line: number of elements in matrix
		map<string, string>::iterator it;
		for (unsigned int i = 0; i < graph_.size(); i++)
		{
			string objId1, objId2;
			objId1 = graph_[i].first;
			if(ids)
			{
				it = ids->find(objId1);
				if(it!=ids->end())
					objId1 = it->second;
			}
			
			fprintf (fp, "%s %d\n", objId1.c_str(), graph_[i].second.size());
			br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
				start	= graph_[i].second.begin(),
				end		= graph_[i].second.end();
			while (start != end) 
			{
				//fprintf (fp, "%s   %3.7f\n", graph_[(*start).first].first.c_str(), (*start).second.Weight() );
				objId2 = graph_[(*start).first].first;
				if(ids)
				{
					it=ids->find(objId2);
					if(it!=ids->end())
                        objId2 = it->second;
				}				

                fprintf (fp, "%s  ", objId2.c_str());
				start++;
			}
			fprintf (fp, "\n");
		}
		fclose (fp);
		return true;
	}
	else return false;
	
}


bool
TeProxMatrixGraphBreymann::saveGWTFile (const string& name, map<string, string>* ids)
{

	string complete_name = name + ".GWT";
	FILE*	fp = fopen(complete_name.c_str(),"w");
	if (fp)
	{
		fprintf (fp, "%d\n", ids->size() ); // first line: number of elements in matrix
		map<string, string>::iterator it;
		for (unsigned int i = 0; i < graph_.size(); i++)
		{
			br_stl::Graph<string, TeProxMatrixAttributes>::Successor::iterator 
				start	= graph_[i].second.begin(),
				end		= graph_[i].second.end();
			while (start != end) 
			{
				string objId1, objId2;
				objId1 = graph_[i].first;
				objId2 = graph_[(*start).first].first;
				if(ids)
				{
					it=ids->find(graph_[i].first);
					if(it!=ids->end())
						objId1 = it->second;
					it=ids->find(graph_[(*start).first].first);
					if(it!=ids->end())
						objId2 = it->second;
				}
				fprintf (fp, "%s %s		%3.7f\n", objId1.c_str(), objId2.c_str(), (*start).second.CentroidDistance());
				start++;
			}
		}
		fclose (fp);
		return true;
	}
	else 
		return false;
	
}

// -------- save from vector

bool
TeProxMatrixGraphBreymann::saveTextFile (const string& name, vector<string>* ids)
{
	string complete_name = name + ".txt";

	FILE*	fp = fopen(complete_name.c_str(),"w");
	if (fp)
	{
		fprintf (fp, "%d\n", ids->size()); //number of objects
	
		vector<string>::iterator it = ids->begin();
		int Id=1;
		while(it!=ids->end())
		{
			string objId1, objId2;
			objId1 = Te2String(Id);
			
			fprintf (fp, " %s ", objId1.c_str());

			TeNeighbours neigs;
			this->getNeighbours((*it), neigs);

			for(int j=0; j<neigs.size(); ++j)
			{
				string objId2aux = neigs[j];
				objId2 ="";
				for(unsigned int n=0; n<ids->size(); ++n)
				{
					if(objId2aux==ids->operator [](n))
					{
						objId2 = Te2String(n+1);
						break;
					}
				}
				fprintf (fp, " %s ", objId2.c_str());  
			}

			fprintf (fp, "\n");

			++it;
			++Id;
		}

		fclose (fp);
		return true;
	}
	else 
		return false;
}


bool
TeProxMatrixGraphBreymann::saveGALFile (const string& name, vector<string>* ids)
{

	string complete_name = name + ".GAL";
	FILE*	fp = fopen(complete_name.c_str(),"w");
	if (fp)
	{
		fprintf (fp, "%d\n", ids->size() ); // first line: number of elements in matrix
		
		vector<string>::iterator it = ids->begin();
		int Id=1;
		while(it!=ids->end())
		{
			string objId1, objId2;
			objId1 = Te2String(Id);
			
			TeNeighbours neigs;
			this->getNeighbours((*it), neigs);

			fprintf (fp, "%s %d\n", objId1.c_str(), neigs.size());

			for(int j=0; j<neigs.size(); ++j)
			{
				string objId2aux = neigs[j];
				objId2 ="";
				for(unsigned int n=0; n<ids->size(); ++n)
				{
					if(objId2aux==ids->operator[](n))
					{
						objId2 = Te2String(n+1);
						break;
					}
				}
				fprintf (fp, "%s  ", objId2.c_str());  
			}

			fprintf (fp, "\n");

			++it;
			++Id;
		}
		
		fclose (fp);
		return true;
	}
	else 
		return false;
	
}


bool
TeProxMatrixGraphBreymann::saveGWTFile (const string& name, vector<string>* ids)
{

	string complete_name = name + ".GWT";
	FILE*	fp = fopen(complete_name.c_str(),"w");
	if (fp)
	{
		fprintf (fp, "%d\n", ids->size() ); // first line: number of elements in matrix
		vector<string>::iterator it = ids->begin();
		int Id=1;
		while(it!=ids->end())
		{
			string objId1, objId2;
			objId1 = Te2String(Id);
			
			TeNeighbours neigs;
			this->getNeighbours((*it), neigs);

			for(int j=0; j<neigs.size(); ++j)
			{
				string objId2aux = neigs[j];
				double dist = (neigs.Attributes(j)).CentroidDistance();
				objId2 ="";
				for(unsigned int n=0; n<ids->size(); ++n)
				{
					if(objId2aux==ids->operator[](n))
					{
						objId2 = Te2String(n+1);
						break;
					}
				}
				fprintf (fp, "%s %s		%3.7f\n", objId1.c_str(), objId2.c_str(), dist);
			}
			++it;
			++Id;
		}
				
		fclose (fp);
		return true;
	}
	else 
		return false;
	
}
