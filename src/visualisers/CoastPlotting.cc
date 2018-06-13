/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
	riversMethods_["efason"] = &CoastPlotting::efas;
	riversMethods_["efasoff"] = &CoastPlotting::ignore;
	riversMethods_["useron"] = &CoastPlotting::user;
	riversMethods_["useroff"] = &CoastPlotting::ignore;
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
cout << "CoastPlotting::operator()"<< endl;
	const Transformation& transformation = parent.transformation();
	CoastPlotting& preview = parent.coastlines();
	transformation.coastSetting(preview.coastSet_, 10, 5);
	preview.decode(parent);

	for (vector<Polyline*>::iterator poly = preview.coast_.begin(); poly != preview.coast_.end(); ++poly)
	{
		Polyline* npoly= (*poly)->clone();
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
cout << "NoCoastPlotting::operator()"<< endl;
	const Transformation& transformation = parent.transformation();
	transformation.coastSetting(coastSet_, parent.layout().absoluteWidth(), parent.layout().absoluteHeight());
	(*boundaries_)(coastSet_, parent.layout());
	layers(riversMethods_, rivers_, parent);
	layers(riversMethods_, "efas"+ efas_, parent);
	(*cities_)(coastSet_, parent.layout());
}

void NoCoastPlotting::efas(DrawingVisitor& visitor)
{
	map<string, string> data;
	data["extended"] = PATH("efas/ExtendedDomain/lines")
	data["current"] = PATH("efas/CurrentDomain/lines")

	map<string, string>::iterator file = data.find(lowerCase(efas_domain_));

	if ( file == data.end() ) {
		MagLog::warning() << " Can not find the EFAS domain " << efas_domain_ << ": revert to default [current]" << endl;
		file = data.find("current");
	}

	ShapeDecoder efas;
	efas.setPath(file->second);
	efas.needHoles(true);
	const Transformation& transformation = visitor.transformation();
	efas.decode(transformation);

	for ( ShapeDecoder::const_iterator river = efas.begin(); river != efas.end(); ++river)
	{
		Polyline poly;
		poly.setColour(*efas_colour_);
		poly.setThickness(efas_thickness_);

		poly.setLineStyle(efas_style_);
		(**river).setToFirst();
		while ((**river).more())
		{
		  poly.push_back(transformation((**river).current()));
		  (**river).advance();
		}
		transformation(poly, visitor.layout());
	}
}

void NoCoastPlotting::user(DrawingVisitor& visitor)
{


	ShapeDecoder user;
	user.setPath(user_layer_name_);
	user.needHoles(true);
	const Transformation& transformation = visitor.transformation();
	user.decode(transformation);

	for ( ShapeDecoder::const_iterator river = user.begin(); river != user.end(); ++river)
	{
		Polyline poly;
		poly.setColour(*user_layer_colour_);
		poly.setThickness(user_layer_thickness_);

		poly.setLineStyle(user_layer_style_);
		(**river).setToFirst();
		while ((**river).more())
		{
		  poly.push_back(transformation((**river).current()));
		  (**river).advance();
		}
		transformation(poly, visitor.layout());
	}
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


void CoastPlotting::release(vector<Polyline*>& polys)
{
 	for ( vector<Polyline*>::iterator poly = polys.begin(); poly != polys.end(); ++poly )
    {
        delete *poly;
    }
    polys.clear();

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

  if ( magCompare(NoCoastPlottingAttributes::resolution_, "high") ||
       magCompare(NoCoastPlottingAttributes::resolution_, "full") ) {
        string resol = "10m";
        coastSet_["resolution"] = resol;
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
  }
  else if ( magCompare(NoCoastPlottingAttributes::resolution_, "medium") ) {
        string resol = "50m";
        coastSet_["resolution"] = resol;
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
  }
  else if ( magCompare(NoCoastPlottingAttributes::resolution_, "low") ) {
        string resol = "110m";
        coastSet_["resolution"] = resol;
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
  layers(riversMethods_, "efas" + efas_, parent);
  layers(riversMethods_, "user" + user_layer_, parent);
  // We do need the coastlines

  release(coast_);
  release(ocean_);
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
	vector<Polyline*> clip;
	clipAndClose(out.transformation(), coast_, clip);

	for (vector<Polyline*>::iterator coast = clip.begin(); coast != clip.end(); ++coast)
	{

		setLandShading(**coast);
		out.push_back(*coast);
	}
}


/*!
 Here we send send a big rectangle for the ocean.
 We send the coastlines as holes of the big one.
 We send the lakes as polylines ... and the holes in the lakes as holes!
*/
void CoastPlotting::seaonly(Layout& out)
{
	vector<Polyline*> clip;
	clipAndClose(out.transformation(), ocean_, clip);
	for (vector<Polyline*>::iterator coast = clip.begin(); coast != clip.end(); ++coast)
	{
		setSeaShading(**coast);
		out.push_back(*coast);
	}
}

void CoastPlotting::nolandsea(Layout& out)
{
	
	vector<Polyline*> clips;
	clip(out.transformation(), coast_, clips);

	for (vector<Polyline*>::iterator coast = clips.begin(); coast != clips.end(); ++coast)
	{

		setLine(**coast);
		out.push_back(*coast);
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

	vector<Polyline*> coastlines;
	coast_.clear();
	ShapeDecoder coastline_decoder;
	const string file = PATH(coastSet_["land"]);
	coastline_decoder.setPath(file);
	coastline_decoder.decode(coastlines, transformation);

	for (vector<Polyline*>::iterator coast = coastlines.begin(); coast != coastlines.end(); ++coast )
	{
		coast_.push_back(*coast);
	}

  if( sea_ )
  {
	vector<Polyline*> oceans;
	ocean_.clear();
	const string file_ocean = PATH(coastSet_["ocean"]);
	coastline_decoder.setPath(file_ocean);
	coastline_decoder.decode(oceans, transformation);

	for (vector<Polyline*>::iterator coast = oceans.begin(); coast != oceans.end(); ++coast )
	{
		ocean_.push_back(*coast);
	}
  }
}

void CoastPlotting::clip(const Transformation& transformation, const vector<Polyline*>& in, vector<Polyline*>& out) const
{

	Polyline& geobox = transformation.getUserBoundingBox();
	Polyline& box = transformation.getPCBoundingBox();
	
	for (vector<Polyline*>::const_iterator poly = in.begin(); poly != in.end(); ++poly ) {

		(*poly)->southClean();
		vector<Polyline> clipped;
		geobox.intersect(**poly, clipped);
		// then we reproject!
		for (vector<Polyline>::iterator clip = clipped.begin(); clip != clipped.end(); ++clip ) {
			vector<Polyline> clip2;
			clip->reproject(transformation);
			box.intersect(*clip, clip2);
			for (vector<Polyline>::iterator c = clip2.begin(); c != clip2.end(); ++c ) {
				out.push_back(c->clone());
			}

		}
	}
	for (vector<Polyline*>::const_iterator poly = in.begin(); poly != in.end(); ++poly ) {
		(*poly)->southClean();
		//(*poly)->reproject(transformation);
		transformation(**poly, out);
	}
}


void CoastPlotting::clipAndClose(const Transformation& transformation, const vector<Polyline*>& in, vector<Polyline*>& out) const
{
	Polyline& geobox = transformation.getUserBoundingBox();
	Polyline& box = transformation.getPCBoundingBox();
	for (vector<Polyline*>::const_iterator poly = in.begin(); poly != in.end(); ++poly ) {

		
		vector<Polyline> clipped;
		geobox.intersect(**poly, clipped);
		// then we reproject!
		for (vector<Polyline>::iterator clip = clipped.begin(); clip != clipped.end(); ++clip ) {
			vector<Polyline> clip2;
			clip->reproject(transformation);
			box.intersect(*clip, clip2);
			for (vector<Polyline>::iterator c = clip2.begin(); c != clip2.end(); ++c ) {
				out.push_back(c->clone());
			}

		}
	}
}
void NoCoastPlotting::visit(MetaDataCollector& meta)
{
	meta["Coastlines Resolution"] = coastSet_["resolution"];
	meta["Coastlines DataSet"] = "Natural Earth";
	meta["Coastlines Date"] = "February 2015";
	for (map<string, string>::const_iterator entry = meta_.begin(); entry != meta_.end(); ++entry ) {
		meta[entry->first] = entry->second;
	}
}
