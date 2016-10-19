/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MetgramGraph.cc
    \brief Implementation of the Template class MetgramGraph.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/



#include "MetgramGraph.h"
#include "PointsHandler.h"
#include "DateTime.h"
#include "Text.h"
#include "LegendVisitor.h"
#include "Timer.h"


using namespace magics;




MetgramGraph::MetgramGraph() 
{}


MetgramGraph::~MetgramGraph() 
{}

/*!
 Class information are given to the output-stream.
*/		
void MetgramGraph::print(ostream& out)  const
{
	out << "MetgramGraph[";
	out << "]";
}



void MetgramGraph::operator()(Data& data, BasicGraphicsObjectContainer& out)
{
	CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = out.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!
	
	Timer timer("Graph","");	
	(*style_)(points, out);
}

void MetgramGraph::visit(LegendVisitor& legend)
{
	style_->visit(legend);
}
