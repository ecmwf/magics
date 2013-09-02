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

/*! \file BoxPlotVisualiser.cc
    \brief Implementation of the Template class BoxPlotVisualiser.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/



#include "BoxPlotVisualiser.h"
#include "PointsHandler.h"
#include "DateTime.h"
#include "Text.h"



using namespace magics;



BoxPlotVisualiser::BoxPlotVisualiser() 
{}


BoxPlotVisualiser::~BoxPlotVisualiser() 
{}

/*!
 Class information are given to the output-stream.
*/		
void BoxPlotVisualiser::print(ostream& out)  const
{
	out << "BoxPlotVisualiser[";
	out << "]";
}



void BoxPlotVisualiser::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{
	CustomisedPointsList points; 
	std::set<string> request;
	
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!
	
	double user = (transformation.getAbsoluteMaxX() - transformation.getAbsoluteMinX())/visitor.absoluteWidth();
	
	
	
	if (points.empty()) return;	
	

	box_->cm(user);	
	whisker_->cm(user);
	

	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point)
	{
	
		(*box_)(visitor, **point);
	    
		whisker_->top(visitor, **point);
		whisker_->bottom(visitor, **point);		
	}
	

		

}

void BoxPlotVisualiser::visit(LegendVisitor&)
{
	
	// Not Yet!
    
   
}


