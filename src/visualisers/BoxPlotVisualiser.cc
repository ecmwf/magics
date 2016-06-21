/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
	
	request.insert("min");
	request.insert("max");
	request.insert("lower");
	request.insert("upper");
	request.insert("median");

	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!
	
	double user = (transformation.getAbsoluteMaxX() - transformation.getAbsoluteMinX())/visitor.absoluteWidth();
	
	double max = transformation.getAbsoluteMaxY();
	double min = transformation.getAbsoluteMinY();
	
	
	if (points.empty()) return;	
	

	box_->cm(user);	
	whisker_->cm(user);
	


	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point)
	{

		for ( std::set<string>::iterator key = request.begin(); key != request.end(); ++key ) {
			double val = (**point)[*key];
			if (val < min) (**point)[*key] = min;
			if  (val > max) (**point)[*key] = max;
		}


		(*box_)(visitor, **point);
	    
		whisker_->top(visitor, **point);
		whisker_->bottom(visitor, **point);		
	}
	

		

}

void BoxPlotVisualiser::visit(LegendVisitor&)
{
	
	// Not Yet!
    
   
}


