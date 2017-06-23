/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GridPlotting.cc
    \brief Implementation of the Template class GridPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 2-Feb-2004
    
    Changes:
    
*/

#include "GridPlotting.h"
#include "../basic/BasicSceneObject.h"
#include "Transformation.h"


using namespace magics;

GridPlotting::GridPlotting() : layer_(0)
{
}
 
GridPlotting::~GridPlotting() 
{
}

void NoGridPlotting::longitudes(vector<double>& lons, int freq) const
{
	 int i = 0;
	 double min = minx_ - lonIncrement_;
	 double max = maxx_ + lonIncrement_;
	 for(double lon = lonReference_; lon > min; lon -= lonIncrement_)
	 {
		 if ( (i % freq) == 0) {
			 lons.push_back(lon);

		 }

		 i++;
	 }
	 i = 0;
	 for(double lon = lonReference_ +  lonIncrement_ ; lon < max; lon += lonIncrement_)
	 {
		 i++;
		 if ( (i % freq) == 0) {
			 lons.push_back(lon);

		 }

	  }

}
void NoGridPlotting::latitudes(vector<double>& lats, int freq) const
{
	 int i = 0;
	 double min = miny_ - latIncrement_;
	 double max = maxy_ + latIncrement_;
	 for(double lat = latReference_; lat > min; lat -= latIncrement_)
	 {
		 if ( i % freq == 0) {
		 	 if ( lat < -90 )
		 	 	lats.push_back(180. + lat);
			 else 
			 	lats.push_back(lat);

		 }
		 i++;
	 }
	 i = 0;
	 for(double lat = latReference_ +  latIncrement_ ; lat < max; lat += latIncrement_)
	 {
		 i++;
		 if ( i % freq == 0) {
			 lats.push_back(lat);

		 }
	 }
}

const vector<double>& NoGridPlotting::longitudes(const Transformation& transformation) const
{
	transformation.boundingBox(minx_, miny_, maxx_, maxy_);
	maxy_ = std::min(85., maxy_);
	double min = minx_ - lonIncrement_;
	double max = maxx_ + lonIncrement_;

	if(longitudes_.empty())
	{
     	     for(double lon = lonReference_; lon > min; lon -= lonIncrement_)
     	     {
		     longitudes_.push_back(lon);
     	     }
	     for(double lon = lonReference_ +  lonIncrement_ ; lon < max; lon += lonIncrement_)
     	     {
		     longitudes_.push_back(lon);
     	     }
	} 

	// Here we prepare a small point in the middle!

	::sort(longitudes_.begin(), longitudes_.end(), std::greater<double>());
	return longitudes_;
}

const vector<double>& NoGridPlotting::latitudes(const Transformation& transformation) const
{
	transformation.boundingBox(minx_, miny_, maxx_, maxy_);
	maxy_ = std::min(85., maxy_);
	double min = miny_ - latIncrement_;
	double max = maxy_ + latIncrement_;
	if(latitudes_.empty())
	{
	       for(double lat = latReference_; lat > min; lat -=latIncrement_)
     	       {/*
     	       	if ( lat < -90 )
     	       		latitudes_.push_back(180. - lat);
     	       	else */
		       		latitudes_.push_back(lat);
     	       }
	       for(double lat = latReference_ + latIncrement_; lat < max; lat += latIncrement_)
     	       {
		       latitudes_.push_back(lat);
     	       }
	}
	::sort(latitudes_.begin(), latitudes_.end(), std::greater<double>());
	return latitudes_;
}    
void NoGridPlotting::operator()(DrawingVisitor& out) {
	
	
	const Transformation& transformation = out.transformation();

	this->latitudes(transformation);
	this->longitudes(transformation);
}
	
void GridPlotting::operator()(DrawingVisitor& out)
{
	const Transformation& transformation = out.transformation();
	

	layer_ = out.layoutPtr();
	


		this->latitudes(transformation);
		this->longitudes(transformation);

	// Wrep :here we add a small line in the lowerleft corner..
	// to force the driver to compute the output size even if 
	// no grid are going to be plotted
	Polyline* grid = new Polyline();
	grid->setColour(Colour("none"));
	grid->setThickness(0);

	grid->push_back(PaperPoint(transformation.getMinPCX(), transformation.getMinPCY()));
	grid->push_back(PaperPoint(transformation.getMinPCX(), transformation.getMinPCY()));
	out.push_back(grid);

	transformation.gridLongitudes(*this);
	transformation.gridLatitudes(*this);
}

void GridPlotting::operator()(magics::PreviewVisitor&)
{
}
void GridPlotting::addFrame(Polyline& poly) const
{
	if ( !grid_frame_ ) 
		return add(poly);
	
    poly.setColour(*grid_frame_colour_);
    poly.setThickness(grid_frame_thickness_);
    poly.setLineStyle(grid_frame_style_);

    poly.reproject(*layer_);
    
}
void GridPlotting::add(Polyline& poly) const
{
    poly.setColour(*colour_);
    poly.setThickness(thickness_);
    poly.setLineStyle(style_);

    poly.reproject(*layer_);
    
}



/*!
 Class information are given to the output-stream.
*/		
void GridPlotting::print(ostream& out)  const
{
	out << "GridPlotting[";
	GridPlottingAttributes::print(out);
	out << "]";
}



