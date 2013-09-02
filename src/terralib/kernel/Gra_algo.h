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

//  include/gra_algo.h
#ifndef GRAPH_ALGORITHMS_H
#define GRAPH_ALGORITHMS_H 
#include <dynpq.h>
#include <graph.h>
#include <limits>
#include <iostream>

namespace br_stl {

template<class GraphType, class EdgeType>
void Dijkstra(
       GraphType& Gr,
       std::vector<EdgeType>& Dist,
       std::vector<int>& Pred,
       int Start) {
    /* The algorithm proceeds in such a way that the distances are
       estimated and the estimates gradually improved. The distance to
       the starting point is known (0). For all other vertices, the
       worst possible estimate is entered.*/

    Dist = std::vector<EdgeType>(Gr.size(),
          TeMAXFLOAT); // as good as infinity
    Dist[Start] = (EdgeType)0;

    /* The predecessor vector too is initialized with `impossible'
       values. Subsequently, a dynamic priority queue is defined and
       initialized with the distance vector: */

	Pred = std::vector<int>(Gr.size(), -1);
    dynamic_priority_queue<EdgeType> Q(Dist);

    // In the next step, all vertices are extracted  one by one from
    //  the priority queue, and precisely in the order of the estimated
    // distance towards the starting vertex. Obviously, the starting
    //vertex is dealt with first. No vertex is looked at twice. 

    int u;
    while(!Q.empty()) {
       u = Q.topIndex();   // extract vertex with minimum
       Q.pop();

       // Now, the distance estimates for all neighboring vertices of
       //  u are updated. If the previous estimate of the distance
        //  between the current neighbor of u and the starting vertex
       //   (Dist[Neighbor]) is worse than the distance between vertex u
        //  and the starting vertex (Dist[u]) plus the distance between
       //   u and the neighboring vertex (dist), the estimate is
        //  improved: this process is called relaxation. In this case,
        //  the path from the starting vertex to the neighbor cannot be
        //  longer than (Dist[u] + dist). In this case, u would have to
        //  be regarded as predecessor of the neighbor. 
          
        // improve estimates for all neighbors of u
        typename GraphType::Successor::const_iterator
                   I = Gr[u].second.begin();

        while(I != Gr[u].second.end()) {
            int Neighbor = (*I).first;
            EdgeType dist = (*I).second;

            // relaxation
            if(Dist[Neighbor] > Dist[u] + dist) {
               // improve estimate
               Q.changeKeyAt(Neighbor, Dist[u] + dist);
               // u is predecessor of the neighbor
               Pred[Neighbor] = u;
            }
            ++I;
        }
    }
	return;
}

template<class GraphType>
bool topoSort(
       GraphType& G,
       std::vector<int>& Result) {
    assert(G.isDirected());           // let's play this safe!
    int ResCounter = 0;
    Result = std::vector<int>(G.size(), -1);

    /* The vector Result takes the indices of the correspondingly
       distributed vertices. The counter ResCounter is the position in
       Result where the next entry belongs. */

    vector<int> PredecessorCount(G.size(), 0);
    int VerticesWithoutSuccessor = 0;

    /* For each vertex, the vector PredecessorCount counts how many
       predecessors it has. There are vertices without successors,
       whose number is kept in VerticesWithoutSuccessor. Furthermore,
       the algorithm remains stable if the precondition that a graph
       must not have cycles is violated. The variable
       VerticesWithoutSuccessor is used to recognize this situation
       (see below). */

    /* 
	for(size_t iv = 0; iv < G.size(); ++iv) {
        if(G[iv].second.size() > 0) {    // is predecessor
           typename GraphType::Successor::const_iterator I =
                G[iv].second.begin();
           while(I != G[iv].second.end())
               // update number of predecessors
               ++PredecessorCount[(*I++).first];
        }
        else  { // Vertex is no predecessor, that is, without successor
             // an excessively high number of predecessors is used 
             // for later recognition
             PredecessorCount[iv] =   G.size(); // too many!
             ++VerticesWithoutSuccessor;
        }
    }
	*/

    /* The dynamic priority queue is initialized with the vector of
       numbers of predecessors. At the beginning of the queue we find
       those vertices that have no predecessors and therefore are to
       be processed next. Only the vertices which are predecessors
       themselves, that is that have successors are processed. The
       subsequent loop is terminated when the queue only contains
       successor vertices which themselves are not predecessors. Their
       number of predecessors can never be 0 because further above
       they were initialized with too high a value.*/
       
    /* 
	dynamic_priority_queue<int> Q(PredecessorCount);

    // process all predecessors
    while(Q.topKey() == 0) {
        // determine vertex with predecessor number 0
       int oV = Q.topIndex();
       Q.pop();

       Result[ResCounter++] = oV;

       // In order to ensure that this vertex without predecessors oV
        //  is no longer considered in the next cycle, the number of
       //   predecessors of all its successors is decreased by 1. 
          
       typename GraphType::Successor::const_iterator
             I = G[oV].second.begin();
       while(I != G[oV].second.end()) {
          // decrease number of predecessors with 
          // changeKeyAt()}. Do not change directly!
          int V = (*I).first;
          Q.changeKeyAt(V, PredecessorCount[V] -1);
          ++I;
       }
    }

    // Now, all vertices without successors are entered. As a
    //   countercheck, the variable VerticesWithoutSuccessor is
    //   decreased. If the queue contains too many vertices, an error
    //   message is displayed.  
       
    while(!Q.empty()) {
         Result[ResCounter++] = Q.topIndex();
         Q.pop();
         --VerticesWithoutSuccessor;
    }

    if(VerticesWithoutSuccessor < 0)
       std::cerr << "Error: graph contains a cycle!\n";
    return VerticesWithoutSuccessor == 0;
	*/
	return false;
}

} // namespace br_stl
#endif

