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

#include "SimplePolylineVisualiser.h"
#include "PointsHandler.h"
#include "Polyline.h"
#include "LegendVisitor.h"
#include "Data.h"
#include "Symbol.h"


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
      colourMethod_->prepare(*levelSelection_, *levelSelection_);
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



void SimplePolylineVisualiser::setup()
{

	if ( colour_list_.empty() )
		colour_list_.push_back("blue");
	if ( style_list_.empty() )
		style_list_.push_back("solid");
	if ( thickness_list_.empty() )
		thickness_list_.push_back(2.);

	vector<string>::iterator colour = colour_list_.begin();
	vector<string>::iterator style = style_list_.begin();
	vector<double>::iterator thickness = thickness_list_.begin();
	MagTranslator<string, LineStyle> translator;



	int max = colour_level_list_.empty() ? 0 : colour_level_list_.size() -1;
	for ( int i = 0; i < max; i++) {
		colour_map_[Interval(colour_level_list_[i], colour_level_list_[i+1]) ] = Colour(*colour);
		++colour;
		if ( colour == colour_list_.end() )
			colour = ( colour_policy_ == M_CYCLE ) ? colour_list_.begin() : --colour;
	}

	max = style_level_list_.empty() ? 0 : style_level_list_.size() -1;
	for ( int i = 0; i < max; i++) {
		style_map_[Interval(style_level_list_[i], style_level_list_[i+1]) ] = translator(*style);
		++style;
		if ( style == style_list_.end() )
			style = ( style_policy_ == M_CYCLE ) ? style_list_.begin() : --style;
	}
	max = thickness_level_list_.empty() ? 0 : thickness_level_list_.size() -1;
	for ( int i = 0; i < max; i++) {
			thickness_map_[Interval(thickness_level_list_[i], thickness_level_list_[i+1]) ] = *thickness;
			++thickness;
			if ( thickness == thickness_list_.end() )
				thickness = ( thickness_policy_ == M_CYCLE ) ? thickness_list_.begin() : --thickness;
		}
	max = transparency_level_list_.empty() ? 0 : transparency_level_list_.size() -1;
 
	for ( int i = 0; i < max; i++) {
		alpha_map_[Interval(transparency_level_list_[i], transparency_level_list_[i+1]) ] = i ;
	}

}

void SimplePolylineVisualiser::smooth(Data& data, BasicGraphicsObjectContainer& parent)
{
   setup();
   if ( legend_only_ )
	   return;
   const Transformation& transformation = parent.transformation();
   



   std::set<string> needs;

   needs.insert(pivot_key_);
   needs.insert(transparency_key_);
   needs.insert(colour_key_);
   needs.insert(style_key_);
   needs.insert(thickness_key_);


   double lastone = -std::numeric_limits<double>::max();

   CustomisedPointsList points;
   data.customisedPoints(parent.transformation(), needs, points, true);

   setup();

   map<double, vector<CustomisedPoint*> > work;
   map<double, vector<CustomisedPoint*> >::iterator where;


   for ( CustomisedPointsList::iterator point = points.begin(); point != points.end(); ++point) {


	   double index = priority(**point);
	   if ( index > lastone )
		   lastone = index;
	   where = work.find( index );
	   if ( where == work.end() ) {
		   work.insert(make_pair(index,vector<CustomisedPoint*>()));
		   where = work.find( index );
	   }
	   where->second.push_back(*point);

   }




   Symbol *marker = new Symbol();
   bool add = false;

   if ( pivot_marker_ == "all")
	   add = true;

   for ( map<double, vector<CustomisedPoint*> >::iterator where = work.begin(); where != work.end(); ++where) {

      for ( int i = 1; i <  where->second.size(); i++) {
    	  CustomisedPoint* last = where->second[i-1];
    	  CustomisedPoint* point = where->second[i];

    	  if ( point->missing() || last->missing() )
    		  continue;

    	  Polyline segment;

    	  segment.setLineStyle(style(*point));
    	  segment.setThickness(thickness(*point));
    	  Colour col = colour(*point);

    	  double a = alpha(*last);
    	 
    	  if ( where->first == lastone ) {
    		  if ( pivot_marker_ == "lastone")
    			  add = true;
          if  ( pivot_marker_ != "none")
    		    col =  *pivot_marker_colour_;         
    	  }
        col.setAlpha(a);
    	  segment.setColour(col);
    	  UserPoint geo(point->longitude(), point->latitude());
    	  UserPoint lastgeo(last->longitude(), last->latitude());
    	  segment.push_back(transformation(geo));
    	  segment.push_back(transformation(lastgeo));


    	  parent.transformation()(segment, parent);

    	  // Add Symbol if needed
    	  if ( add &&  alpha(*point) == 1 ) {
    		  CustomisedPoint::const_iterator value = point->find(colour_key_);

    		  double  x = ( value == point->end() ) ? 0 : value->second;
    		  if ( magCompare(pivot_marker_name_, "cyclone" ) ) {
    			  string name;
    			  if ( point->latitude() > 0 ) {
    				  name = ( x < 117 ) ? "WMO_TropicalCycloneNHLT" : "WMO_TropicalCycloneNHGE";
    			  }
    			  else {
    				  name = ( x < 117 ) ? "WMO_TropicalCycloneSHLT" : "WMO_TropicalCycloneSHGE";
    			  }
    			  marker->setSymbol(name);
    		  }
    		  else  {
    			  marker->setSymbol(pivot_marker_name_);

    		  }
    		  marker->setHeight(pivot_marker_height_);
    		  marker->setColour(*pivot_marker_colour_);
    	  	marker->push_back(transformation(geo));
    	  }
      }

    }

    
    parent.push_back(marker);





}

void SimplePolylineVisualiser::visit(Data&, LegendVisitor& legend)
{

    if ( !legend_) return;
	if ( shade_ ) 
		colourMethod_->visit(legend);   
	legend.newLegend();
		if   ( colour_map_.empty() ) {
				// no legend to plot
			return;
		}
		IntervalMap<Colour>::const_iterator interval;

		for ( interval = colour_map_.begin(); interval != colour_map_.end(); ++interval) {
				Polyline* box = new Polyline();

				double min =interval->first.min_;
				double max = interval->first.max_;

				box->setShading(new FillShadingProperties());

				box->setFillColour(interval->second);
				box->setFilled(true);

				legend.add(new BoxEntry(min, max, box));

			}
			legend.back()->last();
}

Colour 	SimplePolylineVisualiser::colour(const CustomisedPoint& point)
{
	Colour blue("blue");
	CustomisedPoint::const_iterator value = point.find(colour_key_);
	if ( value == point.end() )
		return blue;

	return colour_map_.find(value->second, blue);

}
LineStyle 	SimplePolylineVisualiser::style(const CustomisedPoint& point)
{
	LineStyle solid = M_SOLID;
	CustomisedPoint::const_iterator value = point.find(style_key_);
	if ( value == point.end() )
		return solid;
  
	return style_map_.find(value->second, solid);
}
double 	SimplePolylineVisualiser::thickness(const CustomisedPoint& point)
{
	double thickness = ( thickness_list_.size()) ? thickness_list_.front() : 4;
	CustomisedPoint::const_iterator value = point.find(thickness_key_);
	if ( value == point.end() )
		return thickness;
  
	return thickness_map_.find(value->second, thickness);

}
double SimplePolylineVisualiser::priority(const CustomisedPoint& point)
{
	double priority = 1;
	CustomisedPoint::const_iterator value = point.find(priority_key_);
	if ( value == point.end() )
		return priority;
	return value->second;

}
double 	SimplePolylineVisualiser::alpha(const CustomisedPoint& point)
{
	double alpha = 1;
	CustomisedPoint::const_iterator p = point.find(pivot_key_);
	if ( p == point.end() )
		return alpha;
	int pivot = alpha_map_.find(p->second, -1);
	if ( pivot == -1)
		return alpha;
	CustomisedPoint::const_iterator value = point.find(transparency_key_);
	if ( value == point.end() )
		return alpha;
	int index = alpha_map_.find(value->second, -1);
	alpha = exp(-(abs(float(index - pivot))/factor_));

	return alpha;
}

void SimplePolylineVisualiser::visit(LegendVisitor& legend)
{





}
