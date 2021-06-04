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
#include "SceneVisitor.h"
#include "Timer.h"

#include "LegendVisitor.h"
#include "ParameterSettings.h"
#include "Polyline.h"
#include "ShapeDecoder.h"
#include "ViewNode.h"

using namespace magics;


CoastPlotting::CoastPlotting() {
    MagLog::debug() << "DELETE COAST PLOTTING! " << endl;
}

NoCoastPlotting::NoCoastPlotting() {
    riversMethods_["on"]      = &CoastPlotting::rivers;
    riversMethods_["efason"]  = &CoastPlotting::efas;
    riversMethods_["efasoff"] = &CoastPlotting::ignore;
    riversMethods_["useron"]  = &CoastPlotting::user;
    riversMethods_["useroff"] = &CoastPlotting::ignore;
    riversMethods_["off"]     = &CoastPlotting::ignore;
}

void NoCoastPlotting::visit(LegendVisitor&) {}

void CoastPlotting::visit(LegendVisitor& legend) {
    return;
}

void CoastPlotting::operator()(PreviewVisitor& parent) {
    const Transformation& transformation = parent.transformation();
    CoastPlotting& preview               = parent.coastlines();
    transformation.coastSetting(preview.coastSet_, 10, 5);
    preview.decode(parent);

    for (vector<magics::Polyline*>::iterator poly = preview.coast_.begin(); poly != preview.coast_.end(); ++poly) {
        magics::Polyline* npoly = (*poly)->clone();
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
    magics::Polyline* frame = new magics::Polyline();
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

void NoCoastPlotting::operator()(DrawingVisitor& parent) {
    const Transformation& transformation = parent.transformation();
    transformation.coastSetting(coastSet_, parent.layout().absoluteWidth(), parent.layout().absoluteHeight());
    (*boundaries_)(coastSet_, parent.layout());
    layers(riversMethods_, rivers_, parent);
    layers(riversMethods_, "efas" + efas_, parent);
    (*cities_)(coastSet_, parent.layout());
}

void NoCoastPlotting::efas(DrawingVisitor& visitor) {
    map<string, string> data;
    data["extended"] = buildSharePath("efas/ExtendedDomain/lines");
    data["current"]  = buildSharePath("efas/CurrentDomain/lines");

    map<string, string>::iterator file = data.find(lowerCase(efas_domain_));

    if (file == data.end()) {
        MagLog::warning() << " Can not find the EFAS domain " << efas_domain_ << ": revert to default [current]"
                          << endl;
        file = data.find("current");
    }

    ShapeDecoder efas;
    efas.setPath(file->second);
    efas.needHoles(true);
    const Transformation& transformation = visitor.transformation();
    efas.decode(transformation);

    for (ShapeDecoder::const_iterator river = efas.begin(); river != efas.end(); ++river) {
        magics::Polyline poly;
        poly.setColour(*efas_colour_);
        poly.setThickness(efas_thickness_);

        poly.setLineStyle(efas_style_);
        (**river).setToFirst();
        while ((**river).more()) {
            poly.push_back(transformation((**river).current()));
            (**river).advance();
        }
        transformation(poly, visitor.layout());
    }
}

void NoCoastPlotting::user(DrawingVisitor& visitor) {
    ShapeDecoder user;
    user.setPath(user_layer_name_);
    user.needHoles(true);
    const Transformation& transformation = visitor.transformation();
    user.decode(transformation);

    for (ShapeDecoder::const_iterator river = user.begin(); river != user.end(); ++river) {
        magics::Polyline poly;
        poly.setColour(*user_layer_colour_);
        poly.setThickness(user_layer_thickness_);

        poly.setLineStyle(user_layer_style_);
        (**river).setToFirst();
        while ((**river).more()) {
            poly.push_back(transformation((**river).current()));
            (**river).advance();
        }
        transformation(poly, visitor.layout());
    }
}

void NoCoastPlotting::rivers(DrawingVisitor& visitor) {
    const string file = buildSharePath(coastSet_["rivers"]);

    ShapeDecoder rivers;
    rivers.setPath(file);
    rivers.needHoles(true);
    const Transformation& transformation = visitor.transformation();
    rivers.decode(transformation);

    for (ShapeDecoder::const_iterator river = rivers.begin(); river != rivers.end(); ++river) {
        magics::Polyline poly;
        poly.setColour(*rivers_colour_);
        poly.setThickness(rivers_thickness_);
        poly.setLineStyle(rivers_style_);

        (**river).setToFirst();
        while ((**river).more()) {
            poly.push_back(transformation((**river).current()));
            (**river).advance();
        }
        transformation(poly, visitor.layout());
    }
}

void NoCoastPlotting::layers(map<string, Action>& methods, const string& val, DrawingVisitor& visitor) {
    const string lval(lowerCase(val));
    map<string, Action>::iterator method = methods.find(lval);
    if (method != methods.end())
        (this->*method->second)(visitor);
}

void CoastPlotting::release(vector<magics::Polyline*>& polys) {
    for (vector<magics::Polyline*>::iterator poly = polys.begin(); poly != polys.end(); ++poly) {
        delete *poly;
    }
    polys.clear();
}

/*! \brief Method to set resource files for GIS information

  \note We have to use '10m' resolution for administrative Provinces since only this
   resolution contains data from OUTSIDE the USA and Canada
*/
void CoastPlotting::operator()(DrawingVisitor& parent) {
    Timer timer("coastlines", "prepare the coastlines");
    // if ( !layer_) layer_ = &parent;

    const Transformation& transformation = parent.transformation();
    transformation.collect(meta_);

    coast_.clear();
    ocean_.clear();

    coastSet_["administrative_boundaries"] = "10m/ne_10m_admin_1_states_provinces_lines";

    if (magCompare(NoCoastPlottingAttributes::resolution_, "high") ||
        magCompare(NoCoastPlottingAttributes::resolution_, "full")) {
        string resol            = "10m";
        coastSet_["resolution"] = resol;
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
    }
    else if (magCompare(NoCoastPlottingAttributes::resolution_, "medium")) {
        string resol            = "50m";
        coastSet_["resolution"] = resol;
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
    }
    else if (magCompare(NoCoastPlottingAttributes::resolution_, "low")) {
        string resol            = "110m";
        coastSet_["resolution"] = resol;
        coastSet_["land"]       = resol + "/ne_" + resol + "_land";
        coastSet_["ocean"]      = resol + "/ne_" + resol + "_ocean";
        coastSet_["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
        coastSet_["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
    }
    else {  // automatic
        transformation.coastSetting(coastSet_, parent.layout().absoluteWidth(), parent.layout().absoluteHeight());
    }

    decode(parent.layout());

    // clean and reproject !
    for (vector<magics::Polyline*>::iterator coast = coast_.begin(); coast != coast_.end(); ++coast) {
        (*coast)->southClean(transformation.addSouth());
        (*coast)->reproject(transformation);
    }
    for (vector<magics::Polyline*>::iterator ocean = ocean_.begin(); ocean != ocean_.end(); ++ocean) {
        (*ocean)->reproject(transformation);
    }


    // Now we have the coastlines and lakes..
    // let's call the relevant method!
    if (land_) {
        if (sea_)
            landsea(parent.layout());
        else
            landonly(parent.layout());
    }
    else {
        if (sea_)
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
void CoastPlotting::landsea(Layout& out) {
    landonly(out);
    seaonly(out);
}

/*!
  Here we send the coastlines as land.
  We send the lakes as holes ... and the holes in the lakes as land!
*/
void CoastPlotting::landonly(Layout& out) {
    vector<magics::Polyline*> clips;

    clipAndClose(out.transformation(), coast_, clips);

    for (vector<magics::Polyline*>::iterator coast = clips.begin(); coast != clips.end(); ++coast) {
        setLandShading(**coast);
        out.push_back(*coast);
    }
}


void CoastPlotting::seaonly(Layout& out) {
    vector<magics::Polyline*> clip;
    clipAndClose(out.transformation(), ocean_, clip);
    for (vector<magics::Polyline*>::iterator coast = clip.begin(); coast != clip.end(); ++coast) {
        setSeaShading(**coast);
        out.push_back(*coast);
    }
}

void CoastPlotting::nolandsea(Layout& out) {
    vector<magics::Polyline*> clips;


    clip(out.transformation(), coast_, clips);

    for (vector<magics::Polyline*>::iterator coast = clips.begin(); coast != clips.end(); ++coast) {
        setLine(**coast);
        out.push_back(*coast);
    }
}

void CoastPlotting::setLine(magics::Polyline& line) {
    line.setThickness(thickness_);
    line.setColour(*colour_);
    line.setLineStyle(style_);
    line.setShading(0);
    line.setFilled(false);
}

void CoastPlotting::setSeaShading(magics::Polyline& line) {
    FillShadingProperties* shading = new FillShadingProperties();
    line.setFillColour(*sea_colour_);
    line.setShading(shading);
    line.setColour(*sea_colour_);
    line.setFilled(true);
    line.setStroke(false);
}

void CoastPlotting::setLandShading(magics::Polyline& line) {
    FillShadingProperties* shading = new FillShadingProperties();
    line.setFillColour(*land_colour_);
    line.setColour(*land_colour_);
    line.setShading(shading);
    line.setFilled(true);
    line.setStroke(false);
}

CoastPlotting::~CoastPlotting() {}

/*!
 Class information are given to the output-stream.
*/
void CoastPlotting::print(ostream& out) const {
    out << "CoastPlotting[";
    CoastPlottingAttributes::print(out);
    out << "] ";
}

void CoastPlotting::decode(const Layout& parent) {
    // Read the shape file ...
    Timer timer("geometry", "Simplify+clip");

    const Transformation& transformation = parent.transformation();

    vector<magics::Polyline*> coastlines;
    coast_.clear();
    ShapeDecoder coastline_decoder;
    const string file = buildSharePath(coastSet_["land"]);
    coastline_decoder.setPath(file);
    coastline_decoder.decode(coastlines, transformation);

    for (vector<magics::Polyline*>::iterator coast = coastlines.begin(); coast != coastlines.end(); ++coast) {
        coast_.push_back(*coast);
    }

    if (sea_) {
        vector<magics::Polyline*> oceans;
        ocean_.clear();
        const string file_ocean = buildSharePath(coastSet_["ocean"]);
        coastline_decoder.setPath(file_ocean);
        coastline_decoder.decode(oceans, transformation);

        for (vector<magics::Polyline*>::iterator coast = oceans.begin(); coast != oceans.end(); ++coast) {
            (*coast)->close();
            ocean_.push_back(*coast);
        }
    }
}

void CoastPlotting::clip(const Transformation& transformation, const vector<magics::Polyline*>& in,
                         vector<magics::Polyline*>& out) const {
    magics::Polyline& box = transformation.getPCBoundingBox();
    for (vector<magics::Polyline*>::const_iterator poly = in.begin(); poly != in.end(); ++poly) {
        (*poly)->clip(box, out);
    }
}

void CoastPlotting::clipAndClose(const Transformation& transformation, const vector<magics::Polyline*>& in,
                                 vector<magics::Polyline*>& out) const {
    vector<magics::Polyline*> helper;
    magics::Polyline& box = transformation.getPCBoundingBox();
    for (vector<magics::Polyline*>::const_iterator poly = in.begin(); poly != in.end(); ++poly) {
        box.intersect(**poly, out);
    }
}

void NoCoastPlotting::visit(MetaDataCollector& meta) {
    meta["Coastlines Resolution"] = coastSet_["resolution"];
    meta["Coastlines DataSet"]    = "Natural Earth";
    meta["Coastlines Date"]       = "February 2015";
    for (map<string, string>::const_iterator entry = meta_.begin(); entry != meta_.end(); ++entry) {
        meta[entry->first] = entry->second;
    }
}
