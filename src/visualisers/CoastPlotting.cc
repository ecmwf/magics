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

/*! \file CoastPlotting.cc
    \brief Implementation of the Template class CoastPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 2-Feb-2004
    
    Changes:
    
*/

#include "CoastPlotting.h"
#include "Timer.h"
#include "SceneVisitor.h"

#include "ViewNode.h"
#include "ShapeDecoder.h"
#include "LegendVisitor.h"
#include "Polyline.h"
#include "ParameterSettings.h"

using namespace magics;

#define PATH(a) getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + a;


CoastPlotting::CoastPlotting() 
{
	MagLog::debug() << "DELETE COAST PLOTTING! " << endl;
}

NoCoastPlotting::NoCoastPlotting() 
{
	riversMethods_["on"] = &CoastPlotting::rivers;
	riversMethods_["off"] = &CoastPlotting::ignore;
}

void NoCoastPlotting::visit(LegendVisitor&)
{
}

void CoastPlotting::visit(LegendVisitor& legend)
{
	return;
/*
	Polyline* coast  = new Polyline();
	coast->setThickness(thickness_);
	coast->setColour(*colour_);
	coast->setLineStyle(style_);	

	LineEntry* entry = new LineEntry("Coastlines", coast);
	legend.add(entry);
*/
}



void CoastPlotting::operator()(PreviewVisitor& parent)
{
	const Transformation& transformation = parent.transformation();
	CoastPlotting& preview = parent.coastlines();
	transformation.coastSetting(preview.coastSet_, 10, 5);
	preview.decode(parent);

	for (vector<Polyline>::iterator poly = preview.coast_.begin(); poly != preview.coast_.end(); ++poly)
	{
		Polyline* npoly= poly->clone();
		npoly->setThickness(thickness_);
		npoly->setColour(*colour_);
		npoly->setLineStyle(style_);
		FillShadingProperties* shading = new FillShadingProperties();
		npoly->setFillColour(Colour("cream"));
		npoly->setShading(shading);
		npoly->setFilled(true);
		parent.push_back(npoly);
	}

	// Now we add the frame
	Polyline* frame = new Polyline();
	frame->setAntiAliasing(false);
	frame->setThickness(thickness_);
	frame->setColour(Colour("tan"));
	frame->setLineStyle(style_);	
	frame->push_back(PaperPoint(transformation.getMinX(), transformation.getMinY()));
	frame->push_back(PaperPoint(transformation.getMaxX(), transformation.getMinY()));
	frame->push_back(PaperPoint(transformation.getMaxX(), transformation.getMaxY()));
	frame->push_back(PaperPoint(transformation.getMinX(), transformation.getMaxY()));
	frame->push_back(PaperPoint(transformation.getMinX(), transformation.getMinY()));
	parent.push_back(frame);
}


void NoCoastPlotting::operator()(DrawingVisitor& parent)
{
	const Transformation& transformation = parent.transformation();
	transformation.coastSetting(coastSet_, parent.layout().absoluteWidth(), parent.layout().absoluteHeight());
	(*boundaries_)(coastSet_, parent.layout());
	layers(riversMethods_, rivers_, parent);
	(*cities_)(coastSet_, parent.layout());
}


void NoCoastPlotting::rivers(DrawingVisitor& visitor)
{
	const string file = PATH(coastSet_["rivers"]);

	ShapeDecoder rivers;
	rivers.setPath(file);
	rivers.needHoles(true);
	const Transformation& transformation = visitor.transformation();
	rivers.decode(transformation);

	for ( ShapeDecoder::const_iterator river = rivers.begin(); river != rivers.end(); ++river)
	{
		Polyline poly;
		poly.setColour(*rivers_colour_);
		poly.setThickness(rivers_thickness_);

		poly.setLineStyle(rivers_style_);
		(**river).setToFirst();
		while ((**river).more())
		{
		  poly.push_back(transformation((**river).current()));
		  (**river).advance();
		}
		transformation(poly, visitor.layout());
	}
}

void NoCoastPlotting::layers(map<string, Action>& methods, const string& val, DrawingVisitor& visitor)
{
	const string lval(lowerCase(val));
	map<string, Action>::iterator method = methods.find(lval);
	if ( method != methods.end() )
		(this->*method->second)(visitor);
}




/*! \brief Method to set resource files for GIS information
 
  \note We have to use '10m' resolution for administrative Provinces since only this 
   resolution contains data from OUTSIDE the USA and Canada
*/
void CoastPlotting::operator()(DrawingVisitor& parent)
{
  Timer timer("coastlines", "prepare the coastlines");
  //if ( !layer_) layer_ = &parent;

  const Transformation& transformation = parent.transformation();
  transformation.collect(meta_);

  coast_.clear();
  ocean_.clear();

  coastSet_["administrative_boundaries"] = "10m/ne_10m_admin_1_states_provinces";

  if ( magCompare(NoCoastPlottingAttributes::resolution_, "full") ) {
        string resol = "10m";
        coastSet_["resolution"] = "10m full";
        coastSet_["lakes"]      = resol + "_full/ne_" + resol + "_lakes";
        coastSet_["land"]       = resol + "_full/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "_full/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "_full/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
  }
  else if ( magCompare(NoCoastPlottingAttributes::resolution_, "high") ) {
        string resol = "10m";
        coastSet_["resolution"] = resol;
        coastSet_["lakes"]      = resol + "/ne_" + resol + "_lakes";
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
  }
  else if ( magCompare(NoCoastPlottingAttributes::resolution_, "medium") ) {
        string resol = "50m";
        coastSet_["resolution"] = resol;
        coastSet_["lakes"]      = resol + "/ne_" + resol + "_lakes";
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
  }
  else if ( magCompare(NoCoastPlottingAttributes::resolution_, "low") ) {
        string resol = "110m";
        coastSet_["resolution"] = resol;
        coastSet_["lakes"]      = resol + "/ne_" + resol + "_lakes";
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
  }
  else {       // automatic
        transformation.coastSetting(coastSet_, parent.layout().absoluteWidth(), parent.layout().absoluteHeight());
  }

  decode(parent.layout());

  // Now we have the coastlines and lakes..
  // let's call the relevant method!
  if ( land_ ) {
		if ( sea_ )
			landsea(parent.layout());
		else
			landonly(parent.layout());
  }
  else {
		if ( sea_ )
			seaonly(parent.layout());
  }

  nolandsea(parent.layout());
  (*boundaries_)(coastSet_, parent.layout());
  (*cities_)(coastSet_, parent.layout());
  layers(riversMethods_, rivers_, parent);
}


/*!
  Here we send a big rectangle for the ocean
  We send the coastlines as land and lakes as holes of coastlines as polylines.
*/
void CoastPlotting::landsea(Layout& out)
{
	landonly(out);
	seaonly(out);
}


/*!
  Here we send the coastlines as land.
  We send the lakes as holes ... and the holes in the lakes as land!
*/
void CoastPlotting::landonly(Layout& out)
{
	for (vector<Polyline>::iterator poly = coast_.begin(); poly != coast_.end(); ++poly)
	{
		Polyline* coast = poly->clone();
		setLandShading(*coast);
		out.push_back(coast);
	}
}


/*!
 Here we send send a big rectangle for the ocean.
 We send the coastlines as holes of the big one.
 We send the lakes as polylines ... and the holes in the lakes as holes!
*/
void CoastPlotting::seaonly(Layout& out)
{
	for (vector<Polyline>::iterator poly = ocean_.begin(); poly != ocean_.end(); ++poly)
	{
		Polyline* coast = poly->clone();
		setSeaShading(*coast);
		out.push_back(coast);
	}
}

void CoastPlotting::nolandsea(Layout& visitor)
{
	for (vector<Polyline>::iterator coast = coast_.begin(); coast != coast_.end(); ++coast)
	{
		if ( coast->empty() ) continue;

		Polyline* poly = new Polyline();
		setLine(*poly);
		Polyline::MagLine::const_iterator point = coast->begin();
		poly->push_back(*point);
		++point;
		while ( point!=coast->end() ) {

			const double dist = point->distance(poly->back());
			const double res = tonumber(coastSet_["resolution"]);

			if ( dist > visitor.transformation().patchDistance(res) ) {
					if ( !poly->empty() ) {
						visitor.push_back(poly);
						poly = new Polyline();
						setLine(*poly);
					}
			}

			poly->push_back(*point);
			++point;
		}
		visitor.push_back(poly);
		// now the lakes!
		for (Polyline::Holes::const_iterator lake = coast->beginHoles(); lake != coast->endHoles(); ++lake) {
			Polyline* poly = new Polyline();
			setLine(*poly);
			for ( Polyline::MagLine::const_iterator point = lake->begin(); point != lake->end(); ++point)
				poly->push_back(*point);
			visitor.push_back(poly);
		}
	}
}


void CoastPlotting::setLine(Polyline& line)
{
	line.setThickness(thickness_);
	line.setColour(*colour_);
	line.setLineStyle(style_);
	line.setShading(0);
	line.setFilled(false);
}


void CoastPlotting::setSeaShading(Polyline& line)
{
	FillShadingProperties* shading = new FillShadingProperties();
	line.setFillColour(*sea_colour_);
	line.setShading(shading);
	line.setColour(*sea_colour_);
	line.setFilled(true);
	line.setStroke(false);
}


void CoastPlotting::setLandShading(Polyline& line)
{
	FillShadingProperties* shading = new FillShadingProperties();
	line.setFillColour(*land_colour_);
	line.setColour(*land_colour_);
	line.setShading(shading);
	line.setFilled(true);
	line.setStroke(false);
}


CoastPlotting::~CoastPlotting() 
{}


/*!
 Class information are given to the output-stream.
*/
void CoastPlotting::print(ostream& out)  const
{
	out << "CoastPlotting[";
	CoastPlottingAttributes::print(out);
	out << "] ";
}


void CoastPlotting::decode(const Layout& parent )
{
	//Read the shape file ...
	Timer timer("geometry", "Simplify+clip");
	
	const Transformation& transformation = parent.transformation();

	vector<Polyline> coastlines;
	vector<Polyline> oceans;
	coast_.clear();
	ocean_.clear();

	ShapeDecoder coastline_decoder;
	const string file = PATH(coastSet_["land"]);
	coastline_decoder.setPath(file);
	coastline_decoder.decode(coastlines, transformation);

	const string file_ocean = PATH(coastSet_["ocean"]);
	coastline_decoder.setPath(file_ocean);
	coastline_decoder.decode(oceans, transformation);

	//! Secondly we try to put the lakes in the continents!!!
	for (vector<Polyline>::iterator coast = coastlines.begin(); coast != coastlines.end(); ++coast )
	{
		coast_.push_back(*coast);
	}
	for (vector<Polyline>::iterator coast = oceans.begin(); coast != oceans.end(); ++coast )
	{
		ocean_.push_back(*coast);
	}
}

void NoCoastPlotting::visit(MetaDataCollector& meta)
{
	meta["Coastlines Resolution"] = coastSet_["resolution"];
	meta["Coastlines DataSet"] = "Natural Earth";
	meta["Coastlines Date"] = "November 2015";
	for (map<string, string>::const_iterator entry = meta_.begin(); entry != meta_.end(); ++entry ) {
		meta[entry->first] = entry->second;
	}
}
