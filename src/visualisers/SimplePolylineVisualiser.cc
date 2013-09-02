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

#include "SimplePolylineVisualiser.h"
#include "PointsHandler.h"
#include "Polyline.h"
#include "LegendVisitor.h"
#include "Data.h"


using namespace magics;

    
SimplePolylineVisualiser::SimplePolylineVisualiser() 
{}

SimplePolylineVisualiser::~SimplePolylineVisualiser() 
{}

/*!
 Class information are given to the output-stream.
*/
void SimplePolylineVisualiser::print(ostream& out)  const
{
	out << "SimplePolylineVisualiser[";
	out << "]";
}


void SimplePolylineVisualiser::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{
   levelSelection_->set(*this);
  
   PointsHandler& points = data.points(parent.transformation(), true);
   points.setToFirst();
   
   Polyline* line = 0;
   levelSelection_->calculate(points.min(), points.max(), false);
   
   if (shade_) {
   		colourMethod_->set(*this);
   		colourMethod_->prepare(*levelSelection_);
   }
   const Transformation& transformation = parent.transformation();
   
   while ( points.more() )
   {
        if ( points.current().missing()  || line == 0 )
	{
            if ( line )  
            	parent.transformation()(*line, parent);
            line = new Polyline();
            line->setColour(*colour_);
            line->setLineStyle(style_);
            line->setThickness(thickness_);
            line->setFilled(shade_);            
	}
	if (!points.current().missing() )
	{
		line->push_back(transformation(points.current()));
		if ( shade_ && points.current().value() >= min_ && points.current().value() <= max_) {
			
			line->setFillColour(colourMethod_->colour(points.current().value()));
			FillShadingProperties* shading = new FillShadingProperties();    
			line->setShading(shading);
		}
		else 
			line->setFilled(false);
	}
	points.advance();
   }
   
   if ( !line ) {
	   MagLog::warning() << "Could not find lines to plot " << endl;
	   return;
   }

   if ( !line->empty() )
	   parent.transformation()(*line, parent);
}


void SimplePolylineVisualiser::visit(Data&, LegendVisitor& legend)
{
    if ( !legend_) return;
	if ( shade_ ) 
		colourMethod_->visit(legend);   
}
