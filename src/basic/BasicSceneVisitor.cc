/*! \file BasicSceneVisitor.cc
    \brief Implementation of the Template class BasicSceneVisitor.
    
    Magics Team - ECMWF 2008
    
    Started: Fri 19-Dec-2008
    
    Changes:
    
*/



#include "BasicSceneVisitor.h"

using namespace magics;

BasicSceneVisitor::BasicSceneVisitor() 
{
}


BasicSceneVisitor::~BasicSceneVisitor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void BasicSceneVisitor::print(ostream& out)  const
{
	out << "BasicSceneVisitor[";
	out << "]";
}

