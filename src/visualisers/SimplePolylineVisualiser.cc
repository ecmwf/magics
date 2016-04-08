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
{   map_["classic"] =  &SimplePolylineVisualiser::basic;
    map_["trajectory"] =   &SimplePolylineVisualiser::smooth;
}

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
void SimplePolylineVisualiser::basic(Data& data, BasicGraphicsObjectContainer& parent)
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



void SimplePolylineVisualiser::operator()(Data& data, BasicGraphicsObjectContainer& parent)
{
  
  map<string, Method>::iterator function = map_.find(method_);    
  if ( function != map_.end())  
    (this->*function->second)(data, parent);  
  else {
    MagLog::warning() << "Could not find method " << method_ << ": Use default visualisation" << endl;
    basic(data, parent);
  }
}


void SimplePolylineVisualiser::smooth(Data& data, BasicGraphicsObjectContainer& parent)
{
   const Transformation& transformation = parent.transformation();
   
   vector<PaperPoint> work;
   PointsHandler& points = data.points(parent.transformation(), true);
   points.setToFirst();
   

   while ( points.more() )
   {
    
	   work.push_back(transformation(points.current()));
     points.advance();
   }

   if ( work.empty() )
      return;

  if ( pivot_ == -1 )
    pivot_ =  work.size()/2;
  if ( factor_ == -1 )
    factor_ =  work.size()/2;
  
  double alpha;
  for ( int i = 0; i < work.size() -1; i++) {
      Polyline segment;
      segment.setLineStyle(style_);
      segment.setThickness(thickness_);
      Colour colour = *colour_;

      
      alpha = exp(-(abs(float(i -pivot_))/factor_));

       cout << "alpha = " << alpha << ", i = " << i << endl;
      colour.setAlpha(alpha);
      segment.setColour(colour);
      segment.push_back(work[i]);
      segment.push_back(work[i+1]);
      parent.transformation()(segment, parent);
    }

  

  

}

void SimplePolylineVisualiser::visit(Data&, LegendVisitor& legend)
{
    if ( !legend_) return;
	if ( shade_ ) 
		colourMethod_->visit(legend);   
}
