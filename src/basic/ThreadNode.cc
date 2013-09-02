/*! \file ThreadNode.cc
    \brief Implementation of the Template class ThreadNode.
    
    Magics Team - ECMWF 2008
    
    Started: Tue 23-Sep-2008
    
    Changes:
    
*/



#include "ThreadNode.h"

using namespace magics;

ThreadNode::ThreadNode() 
{
}


ThreadNode::~ThreadNode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ThreadNode::print(ostream& out)  const
{
	out << "ThreadNode[";
	out << "]";
}

void ThreadNode::visit(GraphicsList& out)
{
	MagLog::dev() << " Thread Node to be implemented" << endl;
	
	VectorOfPointers<vector<GraphicsList*> > graphics;	
	
	for ( ItemsList::iterator node = this->items_.begin(); node != this->items_.end(); ++node) {
		// Create a List 
		
		graphics.push_back(new GraphicsList());
		(*node)->visit(*graphics.back());
	}
	
	// Concatenate the results! 
	for (VectorOfPointers<vector<GraphicsList*> >::iterator list = graphics.begin(); list != graphics.end(); ++list) {
		std::copy((*list)->begin(), (*list)->end(), back_inserter(out));
		(*list)->simple_clear();
	}
	
	
}


