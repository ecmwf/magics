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
/*! \file graph.h
    \brief This file contains structures and definitions used to manipulate a graph strucutre
	\note this code came from the book "Desining Components with C++ STL" -
	Ulrich Breymann - Addison-Wesley - pag 243
*/
#ifndef GRAPH_H
#define GRAPH_H
#include<cassert>
#include<vector>
#include<map>
#include<stack>
#include<iostream>
#include <TeDefines.h>

namespace br_stl
{
	// empty parameter class with a minimal set of operations
	// if there are no weights for edges necessary
	struct TL_DLL Empty
	{
	public:
		Empty(int=0) {}
		bool operator<(const Empty&) const { return true;}
	};

	inline std::ostream& operator<<(std::ostream& os, const Empty&) { return os;}
	inline std::istream& operator>>(std::istream& is, Empty& ) { return is;}

	template<class VertexType, class EdgeType>
	class Graph
	{
	public:
		// public type interface
		typedef std::map<int, EdgeType > Successor;
		typedef std::pair<VertexType, Successor> vertex;
		typedef std::vector<vertex> GraphType;
		typedef typename GraphType::iterator iterator;
		typedef typename GraphType::const_iterator const_iterator;

		/* The following constructor initializes the output channel with
		cerr. A parameter must be specified as to whether the graph is
		directed or undirected, because this is an essential property
		of a graph. */

		Graph(bool g, std::ostream& os = cerr)
		: directed(g), pOut(&os) 
		{ }

		bool isDirected() const { return directed;}

		/* A graph is a special kind of container to which something can
		be added and whose elements can be accessed. Therefore, typical
		container methods follow, which in their extent are limited to
		those needed in the book's examples. Thus, there is no method
		for explicit removal of a vertex or an edge from the graph. */

		size_t size() const  { return C.size();}

		iterator begin()     { return C.begin();}
		iterator end()       { return C.end();}

		// access to vertex i}
		vertex& operator[](int i)
		{
			// the access is safe, because C is a checkedVector
			return C[i];
		}

		// return the position of the vertex, if it does not exist return -1
		int getPosition(const VertexType& e); 

		// addition of a vertex
		int  insert(const VertexType& e);

		// addition of an edge between e1 and e2
		void insert(const VertexType& e1, const VertexType& e2,
					const EdgeType& Value);

		// addition of an edge between vertices no. i and j
		void connectVertices(int i, int j, const EdgeType& Value);

		// set the edge value between vertices no. i and j
		bool setEdgeValue(const VertexType& e1, const VertexType& e2, const EdgeType& Value);

		/* The following methods are useful tools for displaying
		information on a graph and check its structure.*/

		// checking of a read data model
		// output on the channel passed to check()
		void check(std::ostream& = std::cout);

		// determine the number of edges
		size_t CountEdges();

		/* determine whether the graph contains cycles and in which way it
		is connected. The method combines two tasks, because they can
		be carried out in a single run.*/
		void CycleAndConnect(std::ostream& = cout);
		int sucessors(int index);

	private:
		bool directed;
		GraphType C;          // container
		std::ostream* pOut;
	};      // class Graph


	template<class VertexType, class EdgeType>
	int Graph<VertexType,EdgeType>::getPosition(const VertexType& e) 
	{
		for(size_t i = 0; i < size(); ++i)
		if(e == C[i].first)
			return i;

		return -1;
	}
		
	/* In order to avoid ambiguities, a vertex is only entered if it does
	not yet exist. The sequential search is not particularly fast; on the
	other hand, this process is only needed once during the construction
	of the graph. The position of the vertex is returned. */

	template<class VertexType, class EdgeType>
	int Graph<VertexType,EdgeType>::insert(const VertexType& e) 
	{
		int pos = getPosition(e);
		if(pos<0)
		{
			// if not found, insert:
			C.push_back(vertex(e, Successor()));
			return size()-1;
		}

		return pos;
	}

	/* An edge is inserted by first inserting the vertices, if needed, and
	by determining their positions. The edge construction itself is 
	carried out by the function connectVertices(). It is passed the
	vertex numbers and, because of the absence of a search procedure, 
	it is very fast. */

	template<class VertexType, class EdgeType>
	void Graph<VertexType,EdgeType>::insert(const VertexType& e1,
										const VertexType& e2,
										const EdgeType& Value)
	{
		int pos1 = insert(e1);
		int pos2 = insert(e2);
		connectVertices(pos1, pos2, Value);
	}

	template<class VertexType, class EdgeType>
	void Graph<VertexType,EdgeType>::connectVertices(
				int pos1, int pos2, const EdgeType& Value) 
	{
		(C[pos1].second)[pos2] = Value;

		if(!directed)  // automatically insert opposite direction too
		(C[pos2].second)[pos1] = Value;
	}

	template<class VertexType, class EdgeType>
	bool Graph<VertexType,EdgeType>::setEdgeValue(const VertexType& e1,
										const VertexType& e2,
										const EdgeType& Value) 
	{
		int pos1 = getPosition(e1);
		int pos2 = getPosition(e2);

		if((pos1<0) || (pos2<0))
			return false;

		(C[pos1].second)[pos2] = Value;

		if(!directed)  // automatically insert opposite direction too
		(C[pos1].second)[pos2] = Value;

		return true;
	}


	template<class VertexType, class EdgeType>
	void Graph<VertexType,EdgeType>::check(std::ostream& os)
	{
		os << "The graph is ";
		if(!isDirected())
			os << "un";

		os << "directed and has "
		<< size() << " vertices and "
		<< CountEdges()
		<< " edges\n";
		CycleAndConnect(os);
	}

	template<class VertexType, class EdgeType>
	size_t Graph<VertexType,EdgeType>::CountEdges()
	{
		size_t edges = 0;
		iterator temp = begin();

		while(temp != end())
			edges += (*temp++).second.size();

		if(!directed)
			edges /= 2;
		return edges;
	}

	// Type for next function
	enum VertStatus {notVisited, visited, processed};

	template<class VertexType, class EdgeType>
	int Graph<VertexType, EdgeType>::sucessors(int index) {
		typename Successor::const_iterator
		start  = operator[](index).second.begin(),
		end    = operator[](index).second.end();
		int nSucc = 0;
		while(start != end)
		{
			nSucc++;
			++start;
		}

		return nSucc;
	}
	template<class VertexType, class EdgeType>
	void Graph<VertexType, EdgeType>::CycleAndConnect(std::ostream& os) {
		int Cycles = 0;
		int ComponentNumber = 0;
		std::stack<int, std::vector<int> > verticesStack;  // vertices to be visited

		/* In order to prevent multiple visits to vertices in possible
		cycles, which entails the risk of infinite loops, the vertices
		are marked for having been visited or finished being processed.
		This purpose is served by the vector VertexState. */
	       
		// assign all vertices the state `not visited'
		std::vector<VertStatus> VertexState(size(), notVisited);

		/* If, starting from one vertex, an attempt is made to reach all
		other vertices, success is not guaranteed in weakly or
		non-connected graphs. Therefore, each vertex is visited. If it
		is found that a vertex has already been visited, it does not
		need to be processed any further. */

		// visit all vertices
		for(size_t i = 0; i < size(); ++i)
		{
			if(VertexState[i] == notVisited)
			{
				++ComponentNumber;
				// store on stack for further processing
				verticesStack.push(i);

				// process stack
				while(!verticesStack.empty())
				{
					int theVertex = verticesStack.top();
					verticesStack.pop();
					if(VertexState[theVertex] == visited)
					VertexState[theVertex] = processed;
					else
					if(VertexState[theVertex] == notVisited)
					{
						VertexState[theVertex] = visited;
						// new vertex, earmark for processed mark
						verticesStack.push(theVertex);

						/* If one of the successors of a newly found vertex
							bears the visited mark, the algorithm has already
							passed this point once, and there is a cycle. */
		                     
						// earmark successor:
						typename Successor::const_iterator
							start  = operator[](theVertex).second.begin(),
							end    = operator[](theVertex).second.end();

						while(start != end)
						{
							int Succ = (*start).first;

							if(VertexState[Succ] == visited)
							{
								++Cycles;   // someone's been here already!
								(*pOut) << "at least vertex "
								<< operator[](Succ).first
								<< " lies in a cycle\n";
							}

							/* Otherwise, the vertex has already been
								processed and therefore should not be
								considered again, or it has not yet been
								visited and is earmarked on the stack. */
		                          
							if(VertexState[Succ] == notVisited)
								verticesStack.push(Succ);
							++start;
						}
					}
				}  // stack empty?
			}     //  if(VertexState}...
		}       // for()} ...

		/* Now we only need the output. In case of directed, weakly
		connected graphs, the algorithm counts several components. In
		order to make the output conform to the above definitions,
		although with less content of information, a distinction is
		made as to whether the graph is directed or not. */

		if(directed)
		{
			if(ComponentNumber == 1)
				os << "The graph is strongly connected.\n";
			else
				os << "The graph is not or weakly "
					"connected.\n";
		}
		else
			os	<< "The graph has "
				<< ComponentNumber
				<< " component(s)." << std::endl;

		os << "The graph has ";
		if(Cycles == 0)
			os << "no ";
		os << "cycles." << std::endl;
	}

	template<class VertexType, class EdgeType>
	std::ostream& operator<<(std::ostream& os, Graph<VertexType,EdgeType>& G)
	{
		// display of vertices with successors
		for(size_t i = 0; i < G.size(); ++i)
		{
			os << G[i].first << " <";
			typename Graph<VertexType,EdgeType>::Successor::const_iterator
								startN = G[i].second.begin(),
								endN   = G[i].second.end();

			while(startN != endN)
			{
				os << G[(*startN).first].first << ' ' // vertex
				<< (*startN).second << ' ';        // edge value
				++startN;
			}
			os << ">" << std::endl;
		}
		return os;
	}
} // namespace br_stl



#endif

