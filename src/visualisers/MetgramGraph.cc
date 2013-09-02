/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
