/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MagPlus.h"
#include <iostream>

#include <MvRootSceneNodeWrapper.h>
#include <FortranSceneNodeWrapper.h>
#include <FortranViewNodeWrapper.h>
#include <CoastlinesWrapper.h>
#include <TephiGridWrapper.h>
#include <TaylorGridWrapper.h>
#include <AxisWrapper.h>
#include <VisualAction.h>
#include "MetaDataWrapper.h"
#include "ImportObjectHandlerWrapper.h"

#ifdef HAVE_GRIB
#include <GribDecoderWrapper.h>
#include <GribLoopWrapper.h>
#endif

#ifdef HAVE_NETCDF
#include <NetcdfDecoderWrapper.h>
#endif

#include <GeoPointsDecoderWrapper.h>
#include <BinningObjectWrapper.h>

#ifdef HAVE_ODB
#include <OdaGeoDecoderWrapper.h>
#include <OdaXYDecoderWrapper.h>
#endif

#ifdef HAVE_BUFR
#include <ObsDecoderWrapper.h>
#include <ObsPlottingWrapper.h>
#endif

#include <ImportPlotWrapper.h>
#include <ImportActionWrapper.h>
#include <InputDataWrapper.h>
#include <TableDecoderWrapper.h>
#include <GeoJSonWrapper.h>
#include <ContourWrapper.h>

#include <TextVisitor.h>
#include <TextVisitorWrapper.h>
#include <LegendVisitorWrapper.h>

#include <MultiVisdef.h>
#include <SymbolPlottingWrapper.h>
#include <GraphPlottingWrapper.h>
#include <WindWrapper.h>

#include <UserPoint.h>
#include "MagicsEvent.h"
#include "MagJSon.h"

#include <PostScriptDriverWrapper.h>

#ifdef HAVE_CAIRO
#include <CairoDriverWrapper.h>
#endif

#include <SVGDriverWrapper.h>
#include <KMLDriverWrapper.h>
#include <GeoJsonDriverWrapper.h>

#ifdef MAGICS_QT
#include <QtDriver.h>
#endif


template <class T>
void replace(magics::MagRequest& request, const string& name, T from, T to)
{
	if (request.countValues(name.c_str()) == 0 ) {
		request(name) = to;
		return;
	}
	T val = request(name);
	if (val == from)
		request(name) = to;

}

void replace_string(magics::MagRequest& request, const string& name, const string& from, const string& to)
{
	if (request.countValues(name) == 0 ) {
		request(name) = to;
		return;
	}
	string val =  request(name);
	if (val == from)
		request(name) = to;

}

template <class T>
void replace(magics::MagRequest& request, const string& name, T from, const string& newname, T to)
{
	if (request.countValues(name) == 0 ) {
		request(newname.c_str()) = to;
		return;
	}
	T val = request(name);
	if (val == from)
		request(newname) = to;
	else
		request(newname) = val;
}

string get(magics::MagRequest& request, const string& param, const string& val)
{

	string v = request(param);

	return ( !v.empty() ) ? v : val;
}

using namespace std;
using namespace magics;




map<string,  MagPlus::ObjectCreator > MagPlus::sceneCreators_;
map<string,  MagPlus::ObjectCreator > MagPlus::driverCreators_;
map<string,  MagPlus::ObjectCreator > MagPlus::sceneUpdators_;
map<string,  MagPlus::DataCreator > MagPlus::dataCreators_;



MagPlus::MagPlus() : root_(0), superpage_(-1), geographical_(true), mode_(interactif), currentMulti_(0)
{
#ifdef MAGICS_QT
	qtDriver_ = 0;
    qtScene_ = 0;
#endif
	if ( sceneUpdators_.empty()) {
		sceneUpdators_["PAGE"] = &MagPlus::page_update;
	}
	if ( dataCreators_.empty()) {
		dataCreators_["GEOPOINTS"] = &MagPlus::createGeopoints;
#ifdef HAVE_NETCDF
		dataCreators_["NETCDF_GEOPOINTS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_GEOVECTORS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_GEOMATRIX"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_GEO_POINTS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_GEO_VECTORS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_GEO_MATRIX"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_POINTS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_VECTORS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_MATRIX"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_XY_POINTS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_XY_VECTORS"] = &MagPlus::createnetcdf;
		dataCreators_["NETCDF_XY_MATRIX"] = &MagPlus::createnetcdf;
#endif
	}
 	if ( sceneCreators_.empty()) {
 		sceneCreators_["PAGE"] = &MagPlus::page;
 		sceneCreators_["NEWPAGE"] = &MagPlus::newpage;
 		sceneCreators_["MCOAST"] = &MagPlus::coastlines;
 		sceneCreators_["MTHERMOGRID"] = &MagPlus::tephigrid;
 		sceneCreators_["PCOAST"] = &MagPlus::oldcoastlines;


 		sceneCreators_["PAXIS"] = &MagPlus::axis;
 		sceneCreators_["CARTESIANVIEW"] = &MagPlus::cartesian;
 		sceneCreators_["PGRIB"] = &MagPlus::gribloop;
 		sceneCreators_["GEOJSON"] = &MagPlus::geojson;
        sceneCreators_["GRIBLOOP"] = &MagPlus::gribloop;
        sceneCreators_["DATALOOP"] = &MagPlus::dataloop;
        sceneCreators_["GEOPOINTS"] = &MagPlus::geopoints;
#ifdef HAVE_NETCDF
        sceneCreators_["NETCDF_GEOPOINTS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_GEOVECTORS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_GEOMATRIX"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_GEO_POINTS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_GEO_VECTORS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_GEO_MATRIX"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_POINTS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_VECTORS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_MATRIX"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_XY_POINTS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_XY_VECTORS"] = &MagPlus::netcdf;
        sceneCreators_["NETCDF_XY_MATRIX"] = &MagPlus::netcdf;
#endif
        sceneCreators_["BUFR"] = &MagPlus::bufr;
        sceneCreators_["INPUT_XY_POINTS"] = &MagPlus::input;
        sceneCreators_["INPUT_GEO_POINTS"] = &MagPlus::input;
        sceneCreators_["INPUT_XY_VECTORS"] = &MagPlus::input;
        sceneCreators_["INPUT_GEO_VECTORS"] = &MagPlus::input;
        sceneCreators_["INPUT_XY_BINNING"] = &MagPlus::input;
        sceneCreators_["INPUT_GEO_BINNING"] = &MagPlus::input;

        sceneCreators_["TABLE_XY_POINTS"] = &MagPlus::table;
        sceneCreators_["TABLE_GEO_POINTS"] = &MagPlus::table;
        sceneCreators_["TABLE_XY_VECTORS"] = &MagPlus::table;
        sceneCreators_["TABLE_GEO_VECTORS"] = &MagPlus::table;
        sceneCreators_["TABLE_XY_BINNING"] = &MagPlus::table;
        sceneCreators_["TABLE_GEO_BINNING"] = &MagPlus::table;
        sceneCreators_["cartesian"] = &MagPlus::cartesianGrid;
        sceneCreators_["tephigram"] = &MagPlus::tephiGrid;
        sceneCreators_["taylor"] = &MagPlus::taylorGrid;
#ifdef HAVE_ODB
	sceneCreators_["ODB_GEO_POINTS"] = &MagPlus::geoodb;
	sceneCreators_["ODB_GEO_VECTORS"] = &MagPlus::geoodb;
	sceneCreators_["ODB_XY_POINTS"] = &MagPlus::xyodb;
	sceneCreators_["ODB_XY_VECTORS"] = &MagPlus::xyodb;
	sceneCreators_["ODB_XY_BINNING"] = &MagPlus::xyodb;
#endif
	    sceneCreators_["VISDEFS"] = &MagPlus::visdef;
	    sceneCreators_["MULTI"] = &MagPlus::multi;
 		sceneCreators_["PCONT"] = &MagPlus::contour;
 		sceneCreators_["MCONT"] = &MagPlus::contour;
 		sceneCreators_["POBS"] = &MagPlus::obs;
 		sceneCreators_["MOBS"] = &MagPlus::obs;
 		sceneCreators_["PSYMB"] = &MagPlus::symbol;
 		sceneCreators_["MSYMB"] = &MagPlus::symbol;
        sceneCreators_["PSYMBPLUS"] = &MagPlus::symbol;
        sceneCreators_["PWIND"] = &MagPlus::wind;
        sceneCreators_["MWIND"] = &MagPlus::wind;
        sceneCreators_["MGRAPH"] = &MagPlus::graph;
 		sceneCreators_["SUPERPAGE"] = &MagPlus::superpage;
 		sceneCreators_["LAYER"] = &MagPlus::layer;
        sceneCreators_["PTEXT"] = &MagPlus::ptext;
        sceneCreators_["MTEXT"] = &MagPlus::text;
        sceneCreators_["MLEGEND"] = &MagPlus::legend;
        sceneCreators_["DEVICE"] = &MagPlus::device;
        sceneCreators_["MIMPORT"] = &MagPlus::import;
        sceneCreators_["PRASTER"] = &MagPlus::raster;
        sceneCreators_["PRASTERLOOP"] = &MagPlus::rasterloop;
        sceneCreators_["BINNING_OBJECT"] = &MagPlus::binning;
 	}

    if ( driverCreators_.empty())
    {
#ifdef MAGICS_QT
	driverCreators_["QTOUTPUT"] = &MagPlus::qtdriver;
#endif
 	driverCreators_["PSOUTPUT"] = &MagPlus::psdriver;
        driverCreators_["PNGOUTPUT"] = &MagPlus::pngdriver;
        driverCreators_["KMLOUTPUT"] = &MagPlus::kmldriver;
        driverCreators_["GEOJSONOUTPUT"] = &MagPlus::geojsondriver;
        driverCreators_["PDFOUTPUT"] = &MagPlus::pdfdriver;
        driverCreators_["SVGOUTPUT"] = &MagPlus::svgdriver;
        driverCreators_["EPSOUTPUT"] = &MagPlus::epsdriver;
    }
}

bool MagPlus::superpage(magics::MagRequest& in)
{
	MagLog::dev()<< "superpage--->" << endl;
	if ( root_) {
		root_->newpage();

		return false;
	}
	int superpage = in("SUPERPAGE_INDEX");
	in("LAYOUT") = "positional";

	if ( superpage == superpage_) return false;
	superpage_ = superpage;
	in("LAYOUT") = "positional";

    replace_string(in, "SUPER_PAGE_FRAME_COLOUR", "BLUE", "grey");

    in.print();
    MvRootSceneNodeWrapper helper;
	helper.set(in);

	root_ = helper.object();

	MagLog::dev()<< "<----superpage" << endl;
	return false;
}

bool MagPlus::newpage(magics::MagRequest& in)
{


	root_->newpage();

	return false;
}

bool MagPlus::layer(magics::MagRequest& in)
{
	cout << "MagPlus::layer" << endl;
	in.print();
	int visibility = in("VISIBILITY");
	visibility_ = visibility;
	zindex_ = in("STACKING_ORDER");
	transparency_ = in("TRANSPARENCY");
	id_ = (string) in("_ID");
	layer_ = (string) in("_NAME");
	

	return false;
}


bool MagPlus::psdriver(magics::MagRequest& in)
{
	PostScriptDriverWrapper helper;
	helper.set(in);

	drivers_.push_back(helper.object());
	mode_ = paper;
	return false;
}

bool MagPlus::epsdriver(magics::MagRequest& in)
{
	PostScriptDriverWrapper helper;
	helper.set(in);
	helper.me()->setEPS(true);
	drivers_.push_back(helper.object());
	mode_ = paper;
	return false;
}

bool MagPlus::pngdriver(magics::MagRequest&  in)
{
#ifdef HAVE_CAIRO
	CairoDriverWrapper helper;
	helper.set(in);
	helper.me()->setPNG();
	drivers_.push_back(helper.object());
	mode_ = paper;
#endif
	return false;
}

bool MagPlus::pdfdriver(magics::MagRequest& in)
{
#ifdef HAVE_CAIRO
	   CairoDriverWrapper helper;
	   helper.set(in);
	   helper.me()->setPDF();
       drivers_.push_back(helper.object());
       mode_ = paper;

#endif
       return false;
}

#ifdef MAGICS_QT
void MagPlus::setQtScene(QGraphicsScene *scene)
{
	qtScene_ =  scene;
}

bool MagPlus::qtdriver(magics::MagRequest& /*in*/)
{
	if ( !qtDriver_)
	{
		ASSERT(qtScene_);
		qtDriver_ = new magics::QtDriver();
        qtDriver_->setScene(qtScene_);

	}
#ifndef MAG_NEXT
	drivers_.push_back(qtDriver_);
#else
	if ( mvMode_ == creation )
		drivers_.push_back(qtDriver_);
	qtDriver_->setUpdateMode(mvMode_ != creation);
#endif
	return false;
}

#endif

bool MagPlus::svgdriver(magics::MagRequest& in)
{
	SVGDriverWrapper helper;
	helper.set(in);

	drivers_.push_back(helper.object());
	mode_ = paper;
	return false;
}

bool MagPlus::kmldriver(magics::MagRequest& in)
{
	KMLDriverWrapper helper;
	in("KML_DESCRIPTION") = "Metview/Magics++";
	helper.set(in);
	mode_ = paper;
	drivers_.push_back(helper.object());

	return false;
}

bool MagPlus::geojsondriver(magics::MagRequest& in)
{
	GeoJsonDriverWrapper helper;
	in("GEOJSON_DESCRIPTION") = "Metview/Magics++";
	helper.set(in);
	mode_ = paper;
	drivers_.push_back(helper.object());

	return false;
}

bool MagPlus::page_update(magics::MagRequest& in)
{

	// get the Metview ID;
	// reset the page!!!
	int id = in("_ID");
	FortranViewNodeWrapper* page = pages_[id];
	page->set(in);
	FortranViewNodeAttributes* node= page->object();
	page->object()->getReady();
	MagLog::progress() << "new geometry" << *node << endl;
	MagLog::broadcast();


	return false;
}

bool MagPlus::page(magics::MagRequest& in)
{
	MagLog::dev()<< "page and subpage--->" << endl;

	sceneCreators_["MLEGEND"] = &MagPlus::legend;


	while ( !empty() ) pop();

	geographical_ = true;
	
	FortranSceneNodeWrapper scenehelper;
	scenehelper.set(in);


	setIconInfo(in, *scenehelper.object());
	
	root_->insert(scenehelper.object());

	push(scenehelper.object());
	replace(in, "SUBPAGE_Y_LENGTH", 17.85, -1.); // reset to the default!
	in("SUBPAGE_MAP_PREVIEW") = "on";

	page_ = 0;

	string def = get(in, "SUBPAGE_MAP_AREA_DEFINITION", "FULL");
	if ( def == "FULL") {
		in("SUBPAGE_MAP_AREA_DEFINITION") = in.countValues("SUBPAGE_LOWER_LEFT_LONGITUDE") ? "CORNERS" : "FULL";
	}

	if ( (string) in("SUBPAGE_MAP_PROJECTION") != "NEXT" ) {
		FortranViewNodeWrapper* viewhelper = new FortranViewNodeWrapper();
		viewhelper->set(in);
		FortranViewNode* view = viewhelper->object();

		string id =  in("_ID");
		int i =  in("_ID");
		pages_[i] = viewhelper;
		if ( !id.empty() )
		{
			view->setInteractiveInfo(id.c_str(),
			in("ZOOM_NUMBER_OF_LEVELS"), in("ZOOM_CURRENT_LEVEL"));
		}
		top()->insert(view);

		push(view);

		string info = (string) in("SUBPAGE_METADATA_INFO");
		if ( info == "ON" ) {
			MetaDataVisitor *meta = new MetaDataVisitor();
			string path = (string) in("SUBPAGE_METADATA_JAVASCRIPT_PATH");
			meta->javascript_ = (path);
			view->push_back(meta);
		}
	}
	else {
		page_ = new FortranViewNodeWrapper();
		in("SUBPAGE_MAP_PROJECTION") = "cartesian";
		page_->set(in);
		int id = in("_ID");
		pages_[id] = page_;
	}

	MagLog::dev()<< "<----page and subpage" << endl;
	return false; // do not exit
}



bool MagPlus::cartesian(magics::MagRequest& in) {

	string projection = get(in, "MAP_PROJECTION", "cartesian");
 	in("SUBPAGE_MAP_PROJECTION") = projection;


	if ( !page_ ) page_ = new FortranViewNodeWrapper();
	page_->set(in);
	id_ =  (string)in("_ID");
	in("_ID") = id_;

	string zindex = in("STACKING_ORDER");
	zindex_ = zindex.empty() ? -1 : tonumber(zindex);
	
	string visibility = in("VISIBILITY");
	visibility_ = visibility.empty() ? true : tonumber(visibility);
	
	string transparency = in("TRANSPARENCY");
	transparency_ = transparency.empty() ? 0 : tonumber(transparency);
	

	layer_ =  (string)in("_NAME");
	FortranViewNode* view = page_->object();
	setIconInfo(in, *view);
	
	if (  !id_.empty()  )
	{
		
		view->setInteractiveInfo(id_.c_str(),
		in("ZOOM_NUMBER_OF_LEVELS"), in("ZOOM_CURRENT_LEVEL"));
	}
	top()->insert(view);
	push(view);

	in.print();
			

	map<string,  ObjectCreator >::iterator creator = sceneCreators_.find(projection);
    if ( creator != sceneCreators_.end() ) {

    	  (this->*creator->second)(in) ;
    }

    geographical_ = false;
	return false; // do not exit
}

bool MagPlus::cartesianGrid(magics::MagRequest& in) {
	string xtype = lowerCase(in("X_AXIS_TYPE"));
	string ytype = lowerCase(in("Y_AXIS_TYPE"));

	replace_string(in, "X_AXIS_TYPE", "", "regular");
	replace_string(in, "Y_AXIS_TYPE", "", "regular");
	replace_string(in, "X_AXIS_TYPE", "longitude", "regular");
	replace_string(in, "X_AXIS_TYPE", "LONGITUDE", "regular");
	replace_string(in, "X_AXIS_TYPE", "latitude", "regular");
	replace_string(in, "X_AXIS_TYPE", "LATITUDE", "regular");
	replace_string(in, "Y_AXIS_TYPE", "", "regular");
	replace_string(in, "Y_AXIS_TYPE", "longitude", "regular");
	replace_string(in, "Y_AXIS_TYPE", "latitude", "regular");
	replace_string(in, "Y_AXIS_TYPE", "LONGITUDE", "regular");
	replace_string(in, "Y_AXIS_TYPE", "LATITUDE", "regular");
	magics::MagRequest& haxis = in.getSubRequest("HORIZONTAL_AXIS");

	if ( haxis ) {

		// use the user defined one
		HorizontalAxis*	 axis = new HorizontalAxis();
		haxis.print();
		replace_string(haxis, "_NAME", "", "Horizontal Axis");
		replace_string(haxis, "_CLASS", "", "MAXIS");

		setIconInfo(haxis, *axis);
		AxisWrapper helper(axis);
		string type = haxis("AXIS_TYPE");
		if ( type != "POSITION_LIST" ) {
			haxis("AXIS_TYPE") = xtype;
			if ( xtype == "latitude" || xtype == "longitude") {
				haxis("AXIS_TYPE") = "regular";
				haxis("AXIS_TICK_LABEL_TYPE") = xtype;
			}
		}

		haxis.print();
		helper.set(haxis);

		axis->orientation_ = "HORIZONTAL";
		axis->position_ = ("bottom");
		top()->push_back(axis);
	}
	else {
		HorizontalAxis* haxis = new HorizontalAxis();
		haxis->icon("Horizontal Axis", "MAXIS");
		haxis->label_type_ = xtype;

		haxis->method_ = auto_ptr<AxisMethod>(MagTranslator<string, AxisMethod>()(in("X_AXIS_TYPE")));
		top()->push_back(haxis);
	}


	magics::MagRequest& vaxis = in.getSubRequest("VERTICAL_AXIS");


	if (  vaxis ) {

		// use the user defined one
		vaxis.print();
		VerticalAxis*	 axis = new VerticalAxis();
		replace_string(vaxis, "_NAME", "", "Vertical Axis");
		replace_string(vaxis, "_CLASS", "", "MAXIS");


		setIconInfo(vaxis, *axis);
		AxisWrapper helper(axis);
		string type = vaxis("AXIS_TYPE");
		if ( type != "POSITION_LIST" ) {
			vaxis("AXIS_TYPE") = ytype;
			if ( ytype == "latitude" || ytype == "longitude") {
				vaxis("AXIS_TYPE") = "regular";
				vaxis("AXIS_TICK_LABEL_TYPE") = ytype;
			}
		}

		helper.set(vaxis);
		axis->orientation_ = "VERTICAL";
		top()->push_back(axis);
	}
	else {
		VerticalAxis* vaxis = new VerticalAxis();
		vaxis->icon("Vertical Axis", "MAXIS");
		vaxis->label_type_ = ytype;
		vaxis->method_ = auto_ptr<AxisMethod>(MagTranslator<string, AxisMethod>()(in("Y_AXIS_TYPE")));
		top()->push_back(vaxis);

	}
	return true; //< @note return value was missing, what should it return?
}

bool MagPlus::tephiGrid(magics::MagRequest& in)
{
	magics::MagRequest& tephi = in.getSubRequest("THERMO_GRID");
	if ( tephi ) {

		// use the user defined one
		tephi.print();
		TephiGridWrapper helper;
		setIconInfo(tephi,*helper.object());
		helper.set(tephi);

		top()->push_back(helper.object());
		setIconInfo(in,*helper.object());

	}
	else {
		TephiGrid* grid = new TephiGrid();
		grid->icon("Tephigram Grid", "MTHERMO_GRID");
		top()->push_back(grid);

	}
	return true; //< @note return value was missing, what should it return?
}
bool MagPlus::taylorGrid(magics::MagRequest& in)
{
	magics::MagRequest& taylor = in.getSubRequest("TAYLOR_GRID");
	if ( taylor ) {

		// use the user defined one
		taylor.print();
		TaylorGridWrapper helper;
		setIconInfo(taylor,*helper.object());
		helper.set(taylor);

		top()->push_back(helper.object());
		setIconInfo(in,*helper.object());

	}
	else {
		TaylorGrid* grid = new TaylorGrid();
		grid->icon("Taylor Grid", "MTAYLOR");
		top()->push_back(grid);
	}
	return true; //< @note return value was missing, what should it return?
}
bool MagPlus::oldcoastlines(magics::MagRequest& in)
{
	replace_string(in, "MAP_COASTLINE_RESOLUTION", "MEDIUM", "automatic");
	coastlines(in);
	return true; //< @note return value was missing, what should it return?
}
bool MagPlus::coastlines(magics::MagRequest& in)
{
	MagLog::dev()<< "add coastlines" << endl;


	replace_string(in, "_NAME", "", "Coastlines");
	replace_string(in, "_CLASS", "", "MCOAST");
	CoastlinesWrapper helper;

	helper.set(in);

	top()->push_back(helper.object());
	setIconInfo(in,*helper.object());
	MagLog::dev()<< top() << endl;
	MagLog::dev()<< *helper.object() << endl;

	return false; // do not exit
}
bool MagPlus::tephigrid(magics::MagRequest& in)
{
	MagLog::dev()<< "add Tephi Grid" << endl;


	replace_string(in, "_NAME", "", "Thermogrid");
	replace_string(in, "_CLASS", "", "MTHERMOGRID");
	TephiGridWrapper helper;

	helper.set(in);

	top()->push_back(helper.object());
	setIconInfo(in,*helper.object());
	MagLog::dev()<< top() << endl;
	MagLog::dev()<< *helper.object() << endl;

	return false; // do not exit
}
bool MagPlus::axis(magics::MagRequest& in)
{
	return false; // do not exit
	MagLog::dev()<< "add axis" << endl;
	 string orientation =  in("AXIS_ORIENTATION");
	 Axis* axis = 0;
	 if ( magCompare(orientation, "vertical") )
		 axis = new VerticalAxis();
	 else
		 axis = new HorizontalAxis();

	AxisWrapper helper(axis);
	helper.set(in);

	top()->push_back(axis);
	MagLog::dev() << *axis << "\n";

	return false; // do not exit
}

bool MagPlus::import(magics::MagRequest& in)
{

	ImportObjectHandlerWrapper object;
	object.set(in);
	top()->push_back(object.object());
	return false; // do not exit
}

bool MagPlus::raster(magics::MagRequest& in)
{
	MagLog::dev()<< "import a raster object" << endl;

	in.print();

	in("IMPORT_FILE_NAME") =  in("IMPORT_FILE_PATH");
	in("IMPORT_FORMAT") =  in("IMPORT_FILE_TYPE");

	ImportActionWrapper object;
	ImportPlotWrapper visdef;

	object.set(in);
	visdef.set(in);
	setIconInfo(in, *object.object());
	setIconInfo(in, *visdef.object());


		VisualAction* action = new VisualAction();
		top()->push_back(action);
		push(action);
		top()->data(object.object());
		top()->visdef(visdef.object());
		pop();

	return false; // do not exit
}

bool MagPlus::binning(magics::MagRequest& in)
{
	MagLog::dev()<< "add binning" << endl;
	in.print();

	BinningObjectWrapper binning;
	binning.set(in);
	top()->binning(binning.object());
	return true;
}

bool MagPlus::grib(magics::MagRequest& in)
{
#ifdef HAVE_GRIB
	MagLog::dev()<< "add grib" << endl;
	in.print();

	VisualAction* action = new VisualAction();
	top()->push_back(action);
	push(action);

	GribDecoderWrapper grib;
	grib.set(in);
	setIconInfo(in, *grib.object());
	top()->data(grib.object());
#else
	MagLog::dev()<< "can NOT add grib (disabled in Magics!)" << endl;
#endif
	return false; // do not exit
}

bool MagPlus::table(magics::MagRequest& in)
{
	MagLog::dev()<< "add table" << endl;

	in.print();
	// First set the table_filename...
	magics::MagRequest&  data = in.getSubRequest("TABLE_DATA");
	if(data)
	{
	  	if(data.getVerb() == "TABLE" || data.getVerb() == "NOTE" || data.getVerb() == "GEOPOINTS")
		{
	  		string x = data("PATH");
			in("TABLE_FILENAME") = x;
		}

		else if( data.getVerb() == "TABLE_READER" )
		{
			magics::MagRequest&  d = data.getSubRequest("DATA");
			if (d)
			{
				in("TABLE_FILENAME") = d("PATH");
			}
			else
			{
				in("TABLE_FILENAME") = data("TABLE_FILENAME");

			}
		}
	}

	if ( geographical_ ) {
		VisualAction* action = new VisualAction();
		top()->push_back(action);
		push(action);
		// Create Data
		TableDecoderWrapper xy;
		in.print();
		magics::MagRequest&  bin = in.getSubRequest("TABLE_BINNING");
		if (bin) {

			in("TABLE_BINNING") = "on";
			xy.set(in);
			bin("TABLE_BINNING") = "on";
			xy.set(bin);
		}
		else {
			in("TABLE_BINNING") = "off";
			xy.set(in);
			bin("TABLE_BINNING") = "off";
			xy.set(bin);
		}
		xy.set(data);
		setIconInfo(in, *xy.object());
		top()->data(xy.object());

	}
	else {
		VisualAction* action = new VisualAction();
		top()->push_back(action);
		push(action);
		// Create Data
		TableDecoderWrapper xy;
		magics::MagRequest&  bin = in.getSubRequest("TABLE_BINNING");
		if (bin) {
			in("TABLE_BINNING") = "on";
			xy.set(in);
			data("TABLE_BINNING") = "on";
			xy.set(data);
			bin("TABLE_BINNING") = "on";
			xy.set(bin);
		}
		else {
			in("TABLE_BINNING") = "off";
			xy.set(in);
			data("TABLE_BINNING") = "off";
			xy.set(data);
			bin("TABLE_BINNING") = "off";
			xy.set(bin);
		}


		setIconInfo(in, *xy.object());
		top()->data(xy.object());
	}


	return false; // do not exit
}

bool MagPlus::input(magics::MagRequest& in)
{
	MagLog::dev()<< "add input" << endl;
	in.print();
	if ( geographical_ ) {
		VisualAction* action = new VisualAction();
		top()->push_back(action);
		push(action);
		// Create Data
		InputDataWrapper xy;
		magics::MagRequest&  bin = in.getSubRequest("INPUT_BINNING");
		if (bin) {
			in("INPUT_BINNING") = "on";
			xy.set(in);
			bin("INPUT_BINNING") = "on";
			xy.set(bin);
		}
		else {
			in("INPUT_BINNING") = "off";
						xy.set(in);
						bin("INPUT_BINNING") = "off";
						xy.set(bin);
		}
		setIconInfo(in, *xy.object());
		top()->data(xy.object());

	}
	else {
		VisualAction* action = new VisualAction();
		top()->push_back(action);
		push(action);
		// Create Data
		InputDataWrapper xy;
		magics::MagRequest&  bin = in.getSubRequest("INPUT_BINNING");
		if (bin) {
			in("INPUT_BINNING") = "on";
			xy.set(in);
			bin("INPUT_BINNING") = "on";
			xy.set(bin);
		}
		else {
			in("INPUT_BINNING") = "off";
			xy.set(in);
			bin("INPUT_BINNING") = "off";
			xy.set(bin);
		}
		setIconInfo(in, *xy.object());
		top()->data(xy.object());
	}


	return false; // do not exit
}

void MagPlus::setIconInfo(magics::MagRequest& mv, MetviewIcon& object)
{
	string iconname =  get(mv, "_NAME", "");
	string iconclass =  get(mv, "_CLASS", "");
	if(iconclass.empty())
		 iconclass =  get(mv, "_VERB", "");
	string iconid =  get(mv, "_ID", "");
	object.icon(iconname, iconclass, iconid);
	if ( layer_.empty() ) 
		layer_ = "UNKNOW";
	object.layerInfo(visibility_, zindex_, transparency_, id_, layer_);
}

bool MagPlus::gribloop(magics::MagRequest& in)
{
#ifdef HAVE_GRIB
	MagLog::dev()<< "add gribloop" << endl;
	in.print();
	string loop("loop");
	string mode =  get(in, "GRIB_VISIT_MODE", loop);

	if ( !magCompare(mode, loop) )
		// we assume it is not an animation...
		return grib(in);

	string file =  get(in, "GRIB_INPUT_FILE_NAME", "");


	in("GRIB_LOOP_PATH") = file.c_str();
	VisualAnimation* action = new VisualAnimation();
	top()->push_back(action);
	push(action);

	GribLoopWrapper grib;
	grib.set(in);
	setIconInfo(in, *grib.object());
	

	action->loop(grib.object());
#else
	MagLog::dev()<< "can NOT add gribloop (disabled in Magics!)" << endl;
#endif
	return false; // do not exit
}

bool MagPlus::rasterloop(magics::MagRequest& in)
{
	MagLog::dev()<< "add rasterloop" << endl;
	in.print();

	VisualAnimation* geoloop = new VisualAnimation();
	top()->push_back(geoloop);
	push(geoloop);

		ImportPlotWrapper visdef;

		visdef.set(in);

		setIconInfo(in, *visdef.object());

	ImportLoop* loop = new ImportLoop();
	setIconInfo(in, *loop);
	geoloop->loop(loop);
	geoloop->visdef(visdef.object());

	in.countValues("RASTERS");

	magics::MagRequest& rasters = in.getSubRequest("RASTERS");
	rasters.print();

	//At this point we do not know the exact type,
	//later we refine it
	loop->setInfo("_datatype","RASTERLOOP");

	bool first =true;

	while ( rasters ) {
		string name =  rasters("LAYERS");
		string format  =   rasters("IMPORT_FILE_TYPE");
		string path = rasters("IMPORT_FILE_PATH");
		string type =  rasters("IMPORT_FILE_TYPE");
		string time =  rasters("TIME"); //Should not work for WMS

		rasters("IMPORT_FILE_NAME") = path;
		rasters("IMPORT_FORMAT") =  rasters("IMPORT_FILE_TYPE");
		rasters("IMPORT_VALID_TIME") =  rasters("TIME");

		ImportActionWrapper object;
		object.set(rasters);

		visdef.set(rasters);
		object.object()->icon(name, format);

		//WMS related part

		string service_name =  rasters("SERVICE");

		if(service_name == "WMS")
		{
			string service_title =  rasters("SERVICE_TITLE");
			string url =  rasters("URL");
			string title =  rasters("TITLE");
			string description =  rasters("DESCRIPTION");
			string legend =  rasters("LEGEND");
			string logo =  rasters("LOGO");
			string dimName = rasters("DIM_NAME");
			string dimValue = rasters("DIM_VALUE");

			if(first)
			{
				loop->setInfo("_datatype","RASTERLOOP_WMS");
				loop->setInfo("service_name",service_name);
				loop->setInfo("service_title",service_title);
				loop->setInfo("url",url);
				loop->setInfo("name",name);
				loop->setInfo("title",title);
				loop->setInfo("description",description);
				loop->setInfo("legend",legend);
				loop->setInfo("logo",logo);
				loop->setInfo("dimName",dimName);
				loop->setInfo("dimValue",dimValue);
			}

			object.object()->setInfo("_datatype","RASTERLOOP_WMS");
			object.object()->setInfo("service_name",service_name);
			object.object()->setInfo("service_title",service_title);
			object.object()->setInfo("url",url);
			object.object()->setInfo("name",name);
			object.object()->setInfo("title",title);
			object.object()->setInfo("description",description);
			object.object()->setInfo("legend",legend);
			object.object()->setInfo("logo",logo);
			object.object()->setInfo("dimName",dimName);
			object.object()->setInfo("dimValue",dimValue);

			//Temporal dimensions
			vector<string> dimNameLst,dimValueLst;

    			std::stringstream ssN(dimName);
   			std::string item;

    			while(std::getline(ssN, item,'/'))
			{
        			dimNameLst.push_back(item);
   			}

			std::stringstream ssV(dimValue);
    			while(std::getline(ssV, item,'/'))
			{
        			dimValueLst.push_back(item);
   			}

			if(dimNameLst.size() == dimValueLst.size())
			{
				for(unsigned int i=0; i < dimNameLst.size(); i++)
				{
					object.object()->setInfo(dimNameLst[i],dimValueLst[i]);

					if(dimNameLst[i] == "TIME" && dimValueLst[i].size() >=9)
					{
						string dataDate=dimValueLst[i].substr(0,8);
						string dataTime=dimValueLst[i].substr(8);

						object.object()->setInfo("date",dataDate);
						object.object()->setInfo("dataDate",dataDate);
						object.object()->setInfo("time.dataDate",dataDate);
						object.object()->setInfo("validityDate",dataDate);
						object.object()->setInfo("time.validityDate",dataDate);

						object.object()->setInfo("time",dataTime);
						object.object()->setInfo("dataTime",dataTime);
						object.object()->setInfo("time.dataTime",dataTime);
						object.object()->setInfo("validityTime",dataTime);
						object.object()->setInfo("time.validityTime",dataTime);

					}
					else if(dimNameLst[i] == "DIM_RUN" && dimValueLst[i].size() >=9)
					{
						string dataDate=dimValueLst[i].substr(0,8);
						string dataTime=dimValueLst[i].substr(8);

						object.object()->setInfo("date",dataDate);
						object.object()->setInfo("dataDate",dataDate);
						object.object()->setInfo("time.dataDate",dataDate);

						object.object()->setInfo("time",dataTime);
						object.object()->setInfo("dataTime",dataTime);
						object.object()->setInfo("time.dataTime",dataTime);

					}
					else if(dimNameLst[i] == "DIM_FORECAST")
					{
						object.object()->setInfo("step",dimValueLst[i]);
						object.object()->setInfo("stepRange",dimValueLst[i]);
						object.object()->setInfo("time.stepRange",dimValueLst[i]);
					}
					else if(dimNameLst[i] == "ELEVATION")
					{
						object.object()->setInfo("level",dimValueLst[i]);
						object.object()->setInfo("vertical.level",dimValueLst[i]);
					}

				}

			}

		}



		loop->add(object.object());

		rasters.advance();

		first=false;
	}
	pop();
	return false; // do not exit
}


#ifdef HAVE_ODB
bool MagPlus::geoodb(magics::MagRequest& in)
{
	MagLog::dev()<< "add geo odb" << endl;
	in.print();
	string path(in("ODB_FILENAME"));
	if (path == "OFF") {
		magics::MagRequest& odb = in.getSubRequest("ODB_DATA");
		path = string(odb("PATH"));
	}
	in("ODB_FILENAME") = path.c_str();

	static map<string, string> types;
    	if ( types.empty() )
	{
        	types["ODB_GEO_POINTS"] = "geopoint";
        	types["ODB_GEO_VECTORS"] = "geovector";
    	}

	in("ODB_TYPE") = types[in.getVerb()].c_str();
	VisualAction* action = new VisualAction();
	top()->push_back(action);
	push(action);

	OdaGeoDecoderWrapper geoodb;
	geoodb.set(in);
	setIconInfo(in, *geoodb.object());
	top()->data(geoodb.object());

	//Meta-data
	geoodb.object()->initInfo();

	return false; // do not exit
}

bool MagPlus::xyodb(magics::MagRequest& in)
{
	MagLog::dev()<< "add xy odb" << endl;
	in.print();
	string path(in("ODB_FILENAME"));
	if (path == "OFF") {
		magics::MagRequest& odb = in.getSubRequest("ODB_DATA");
		path = string(odb("PATH"));
	}
	in("ODB_FILENAME") = path.c_str();

	static map<string, string> types;
    	if ( types.empty() ) {
        	types["ODB_XY_POINTS"] = "xypoint";
        	types["ODB_XY_VECTORS"] = "xyvector";
		types["ODB_XY_BINNING"] = "xybinning";
   	 }

	in("ODB_TYPE") = types[in.getVerb()].c_str();
	VisualAction* action = new VisualAction();
	top()->push_back(action);
	push(action);

	OdaXYDecoderWrapper xyodb;

	magics::MagRequest&  bin = in.getSubRequest("ODB_BINNING");
	if(bin)
	{
		in("ODB_BINNING") = "on";
		bin("ODB_BINNING") = "on";
	}
	else
	{
	  	in("ODB_BINNING") = "off";
		bin("ODB_BINNING") = "off";
	}
	xyodb.set(in);
	xyodb.set(bin);

	setIconInfo(in, *xyodb.object());
	top()->data(xyodb.object());
	geographical_ = false;

	//Meta-data
	xyodb.object()->initInfo();

	return false; // do not exit
}
#endif

#ifdef HAVE_NETCDF
static map<string, string> nctypes;

void checknctypes() {
	   if ( nctypes.empty() ) {
	        nctypes["NETCDF_POINTS"] = "xypoint";
	        nctypes["NETCDF_VECTORS"] = "vector";
	        nctypes["NETCDF_MATRIX"] = "matrix";
	        nctypes["NETCDF_XY_POINTS"] = "xypoint";
	        nctypes["NETCDF_XY_VECTORS"] = "vector";
	        nctypes["NETCDF_XY_MATRIX"] = "matrix";
	        nctypes["ARRAY"] = "matrix";
	        nctypes["MATRIX"] = "complex_matrix";

	        nctypes["NETCDF_GEOPOINTS"] = "geopoint";
	        nctypes["NETCDF_GEOVECTORS"] = "geovector";
	        nctypes["NETCDF_GEOMATRIX"] = "geomatrix";
	        nctypes["NETCDF_GEO_POINTS"] = "geopoint";
	        nctypes["NETCDF_GEO_VECTORS"] = "geovector";
	        nctypes["NETCDF_GEO_MATRIX"] = "geomatrix";
	    }
}

bool MagPlus::netcdf(magics::MagRequest& in)
{
	MagLog::dev()<< "add netcdf" << endl;
	in.print();
	checknctypes();
	string path = get(in, "NETCDF_FILENAME", "OFF");
	if (path == "OFF") {
		magics::MagRequest& netcdf = in.getSubRequest("NETCDF_DATA");
		path = string(netcdf("PATH"));
	}

	in("NETCDF_FILENAME") = path.c_str();
	    static map<string, string> types;

    string type = get(in, "NETCDF_POSITION_TYPE", in.getVerb());
   	in("NETCDF_TYPE") = nctypes[type].c_str();

	VisualAnimation* action = new VisualAnimation();
	top()->push_back(action);
	push(action);

	NetcdfDecoderWrapper geonet;
	geonet.set(in);

	NetcdfLoop* loop = new NetcdfLoop(geonet.object());
	setIconInfo(in, *geonet.object());
	setIconInfo(in, *loop);
	action->loop(loop);

	return false; // do not exit
}


Data* MagPlus::createnetcdf(magics::MagRequest& in)
{
	// Extract the path ..
	MagLog::dev()<< "add xy netcdf" << endl;
	in.print();
	checknctypes();
	string path = get(in, "NETCDF_FILENAME", "OFF");
	if (path == "OFF") {
		magics::MagRequest& netcdf = in.getSubRequest("NETCDF_DATA");
		path = string(netcdf("PATH"));
	}
	in("NETCDF_FILENAME") = path.c_str();

	string type = get(in, "NETCDF_POSITION_TYPE", in.getVerb());

	in("NETCDF_TYPE") = nctypes[type].c_str();

	NetcdfDecoderWrapper netcdf;
	netcdf.set(in);
	netcdf.object()->initInfo();

	return  netcdf.object();
}
#endif    // NetCDF

Data* MagPlus::createGeopoints(magics::MagRequest& in)
{
	// Extract the path ..
	magics::MagRequest& record = in.getSubRequest("RECORD");

	string path =  record("PATH");
	in("GEO_INPUT_FILE_NAME") = path;


	GeoPointsDecoderWrapper geopoints;
	geopoints.set(in);


	//Meta-data
	geopoints.object()->initInfo();
	return  geopoints.object();
}


bool MagPlus::geopoints(magics::MagRequest& in)
{

#ifdef test_dataloop
	in.read("/tmp/cgs/jira/MAGP-268/dataloop");
	dataloop(in);
	return false;
#endif

	// Extract the path ..
	magics::MagRequest& record = in.getSubRequest("RECORD");

	string path =  record("PATH");
	in("GEO_INPUT_FILE_NAME") = path;
	in.print();
	VisualAction* action = new VisualAction();
	top()->push_back(action);
	push(action);

	GeoPointsDecoderWrapper geopoints;
	geopoints.set(in);	
	setIconInfo(in, *geopoints.object());
	top()->data(geopoints.object());

	//Meta-data
	geopoints.object()->initInfo();

	return false; // do not exit
}

bool MagPlus::geojson(magics::MagRequest& in)
{
	VisualAction* action = new VisualAction();
	top()->push_back(action);
	push(action);

	GeoJSonWrapper geo;
	geo.set(in);
	setIconInfo(in, *geo.object());
	top()->data(geo.object());

	//Meta-data
	geo.object()->initInfo();

	return false; // do not exit
}

bool MagPlus::bufr(magics::MagRequest& in)
{
#ifdef HAVE_BUFR
	/*
	// Extract the path ..
	magics::MagRequest record = in("RECORD");

	in("GEO_INPUT_FILE_NAME") = record("PATH");
	*/
	in.print();
	VisualAction* action = new VisualAction();
	top()->push_back(action);
	push(action);

	ObsDecoderWrapper obs;
	obs.set(in);
	top()->data(obs.object());
	setIconInfo(in, *obs.object());
#endif
	return false; // do not exit
}
bool MagPlus::symbol(magics::MagRequest& in)
{
	if ( in.countValues("SYMBOL_INPUT_MARKER_LIST") ) {
		in("SYMBOL_MARKER") = in("SYMBOL_INPUT_MARKER_LIST");
	}
    string verb = in.getVerb();
    if ( verb == "PSYMBPLUS" ) {
        in("SYMBOL_TABLE_MODE") = "advanced";
        in("SYMBOL_TYPE") = "marker";
    }

    FortranAutomaticLegendVisitor* node = new FortranAutomaticLegendVisitor();
    		LegendMethod* method = new ContinuousLegendMethod();
    		node->method_ = auto_ptr<LegendMethod>(method);
    		node->getReady();
    		//top()->legend(node);
	if ( geographical_ ) {
		SymbolPlottingWrapper symbol;
		symbol.set(in);
		setIconInfo(in, *symbol.object());
		MagLog::dev()<< "add symbol" << *symbol.object() << endl;
		top()->visdef(symbol.object());
		pop();
	}
	else {
		SymbolPlottingWrapper symbol;
		symbol.set(in);
		setIconInfo(in, *symbol.object());
		MagLog::dev()<< "add symbol" << *symbol.object() << endl;
		top()->visdef(symbol.object());
		pop();

	}

	return false; // do not exit
}

bool  MagPlus::graph(magics::MagRequest& in)
{
	GraphPlottingWrapper graph ;
	graph.set(in);
	MagLog::dev()<< "add graph" << *graph.object() << endl;
	top()->visdef(graph.object());
	pop();
	return false; // do not exit
}

bool MagPlus::obs(magics::MagRequest& in)
{
#ifdef HAVE_BUFR
	ObsPlottingWrapper visdef;
	
	visdef.set(in);
	
	MagLog::dev()<< "add obs" << *visdef.object() << endl;
	top()->visdef(visdef.object());
	pop();


#endif
	return false; // do not exit
}
bool MagPlus::dataloop(magics::MagRequest& in)
{
	MagLog::dev()<< "add generic dataloop" << endl;
	in.print();

	VisualAnimation* loop = new VisualAnimation();
	top()->push_back(loop);
	push(loop);



	DataList* data = new DataList();
	setIconInfo(in, *data);
	loop->loop(data);


	magics::MagRequest& entries = in.getSubRequest("ENTRIES");


	//At this point we do not know the exact type,
	//later we refine it
	loop->setInfo("_datatype","DATALOOP");



	while ( entries ) {
		string verb = entries.getVerb();

		map<string,  DataCreator >::iterator creator = dataCreators_.find(verb);
		if ( creator != dataCreators_.end() ) {
			magics::MagRequest& entry = entries.justOneRequest();
			Data* x = (this->*creator->second)(entry);
			data->add(x);

		}
		MagLog::warning() << "Ignore entry " << verb << endl;
		entries.advance();


	}

	return false; // do not exit
}

bool MagPlus::wind(magics::MagRequest& in)
{


	WindWrapper wind;
	wind.set(in);
	setIconInfo(in, *wind.object());
	MagLog::dev()<< "add wind" << *wind.object() << endl;
	if (currentMulti_) {
		currentMulti_->push_back(wind.object());
	}
	else {
		top()->visdef(wind.object());
		pop();
	}

	return false; // do not exit
}

bool MagPlus::visdef(magics::MagRequest& in)
{
	MagLog::dev()<< "found visdef" << endl;

	MagRequest& visdefs = in.getSubRequest("ACTIONS");

	visdefs.print();
	bool dopop = false;
	while (visdefs) {

		string verb = visdefs.getVerb();
		{
	   	    map<string,  ObjectCreator >::iterator creator = sceneHandler_->find(verb);
	   	    if ( creator != sceneHandler_->end() ) {
	   	    	  magics::MagRequest& visdef = visdefs.justOneRequest();
	   	    	 // we keep the action
	   	    	  BasicSceneObject* action = top();
	   	    	  (this->*creator->second)(visdef);
	   	    	  // we pout it back!
	   	    	  push(action);
	   	    	  dopop = true;
	   	    }
		}
		visdefs.advance();
	}
	if ( dopop ) pop();
	MagLog::dev()<< "<---end visdef" << endl;
	return false;
}

bool MagPlus::multi(magics::MagRequest& in)
{
	MultiVisdef* multi = new MultiVisdef();

	top()->visdef(multi);
	MagRequest& visdef1 = in.getSubRequest("1D_VISDEF");
	MagRequest& visdef2 = in.getSubRequest("2D_VISDEF");
	pop();
	visdef1.print();
	visdef2.print();
	while (visdef1) {
		currentMulti_ = multi->oneDimension();
		string verb = visdef1.getVerb();
		{
	   	    map<string,  ObjectCreator >::iterator creator = sceneHandler_->find(verb);
	   	    if ( creator != sceneHandler_->end() ) {
	   	    	  magics::MagRequest& visdef = visdef1.justOneRequest();
	   	    	  (this->*creator->second)(visdef);
	   	    }
		}
		visdef1.advance();
	}


	visdef2.print();
	while (visdef2) {
		currentMulti_ = multi->twoDimension();
		string verb = visdef2.getVerb();
		{
			map<string,  ObjectCreator >::iterator creator = sceneHandler_->find(verb);
			if ( creator != sceneHandler_->end() ) {
				  magics::MagRequest& visdef = visdef2.justOneRequest();
				  (this->*creator->second)(visdef);
			}
		}
		visdef2.advance();
	}
	currentMulti_=0;;
	MagLog::dev()<< "<---end visdef" << endl;
	return false;
}



bool MagPlus::contour(magics::MagRequest& in)
{
	MagLog::dev()<< "add contour" << endl;

	//replace(in, "CONTOUR_LABEL_HEIGHT", 0.3, 0.2);
	string legend  = get(in, "CONTOUR_LEGEND", "ON");

	ContourWrapper contour;
	contour.set(in);
	setIconInfo(in, *contour.object());
	if (currentMulti_)
		currentMulti_->push_back(contour.object());
	else {
		top()->visdef(contour.object());
		pop();
	}

	return false; // do not exit
}

bool MagPlus::ptext(magics::MagRequest& in)
{
	MagLog::dev()<< "add Text" << endl;
	in.print();
	sceneCreators_["MLEGEND"] = &MagPlus::ignore;

	replace(in, "TEXT_REFERENCE_CHARACTER_HEIGHT", 2.0 , "TEXT_FONT_SIZE", 0.3);
	replace_string(in, "TEXT_COLOUR", "BLUE", "navy");
	in("TEXT_HTML") = "on";
	text(in);
	legend(in);

	return false; // do not exit
}

bool MagPlus::text(magics::MagRequest& in)
{

	MagLog::dev()<< "add Text-->" << endl;
	in.print();
	MagLog::dev()<< "<--add Text" << endl;
	string mode = get(in, "TEXT_MODE", "automatic");


	in("TEXT_HTML") = "on";

	TextVisitor* node;
	if (magCompare(mode, "positional") )
		node = new FortranPositionalTextVisitor();
	else
		node = new FortranAutomaticTextVisitor();

	TextVisitorWrapper helper(node);
	helper.set(in);
	setIconInfo(in, *helper.object());
	top()->text(node);


	return false; // do not exit
}


bool MagPlus::legend(magics::MagRequest& in)
{

	MagLog::dev()<< "add legend-->" << endl;
	in.print();
	MagLog::dev()<< "<--add legend" << endl;
	string mode = get(in, "LEGEND_BOX_MODE", "automatic");

	LegendVisitor* legend;
	if ( magCompare(mode, "positional") ) {
		legend = new FortranPositionalLegendVisitor();

	}
	else
		legend = new FortranAutomaticLegendVisitor();
	LegendVisitorWrapper helper(legend);
	helper.set(in);
	setIconInfo(in, *helper.object());
	top()->legend(legend);
	return false; // do not exit
}

bool MagPlus::ignore(magics::MagRequest&)
{
	return false; // do not exit
}

bool MagPlus::device(magics::MagRequest& in)
{
	MagLog::dev()<< "add device" << endl;
	in.print();
	XmlNode* driver = 0;
	if ( !in.countValues("FORMAT") ) return false;
    string fmt =  in("FORMAT");


    string format(fmt);

    if ( format == "POSTSCRIPT") {

    	map<string, string> attributes;
    	attributes["output_fullname"] =  string(in("FILE"));

    	driver = new XmlNode("ps", attributes);
    }

	output_.set(*driver, drivers_);
    if ( driver) delete(driver);
	return false; // do not exit
}


void MagPlus::execute( magics::MagRequest& in)
{
	cout << "MagPlus::execute-->" << endl;
	in.print();
	cout << "<---MagPlus::execute" << endl;
	try {
	// Start from a fersh tree!
#ifndef MAG_NEXT
		if (root_)
		{
			delete root_;
			root_ = 0;
			superpage_ = -1;
			drivers_.clear();
			mode_ = interactif;
		}
		sceneHandler_ =  &sceneCreators_;
#else
		mvMode_ = (root_) ?  update : creation;
		sceneHandler_ = (root_) ? &sceneUpdators_ : &sceneCreators_;

#endif

   while ( in ) {
   		string verb = in.getVerb();
   	    MagLog::dev()<< "create-->" << verb <<endl;
        {
   	    map<string,  ObjectCreator >::iterator creator = sceneHandler_->find(verb);
   	    if ( creator != sceneHandler_->end() ) {
   	    	  magics::MagRequest& request = in.justOneRequest();
   	    	  if ( (this->*creator->second)(request) ) return;
   	    }
        }
        {
        map<string,  ObjectCreator >::iterator creator = driverCreators_.find(verb);
   	    if ( creator != driverCreators_.end() ) {
   	    	  magics::MagRequest& request = in.justOneRequest();
   	    	  if ( (this->*creator->second)(request) ) return;
   	    }
   	    }
   		in.advance();
   }
        if ( !root_ ) {
        	MagLog::warning() << "Sorry, nothing to to display!" << endl;
        	return;
        }
		ASSERT(root_);

			root_->getReady();
			drivers_.setDriversWidth(root_->absoluteWidth());
			drivers_.setDriversHeight(root_->absoluteHeight());
			root_->mode(mode_);

			root_->execute();

		drivers_.openDrivers();
		drivers_.dispatch(root_->root());
		drivers_.closeDrivers();
		MetaDataVisitor::collect();
   }
   catch (MagicsException& e)
   {
   	/*! \todo Why is this MagMagException empty???  */
	   MagLog::error() << "Something went really wrong!" << e << endl;

   }
}



void MagPlus::notify(MagicsEvent& event)
{
	MagLog::dev()<< "NOTIFY---" << event << endl;
	 for (vector<MagicsObserver*>::iterator observer = observers_.begin(); observer != observers_.end(); ++observer)
			event.notify(**observer);
}

void MagPlus::unregisterObserver(MagicsObserver* observer)
{

    observers_.erase(std::remove_if(observers_.begin(), observers_.end(),
    	bind2nd(std::equal_to<MagicsObserver*>(), observer)), observers_.end());

}

void setDouble(const string& key, const ParamJSon& json, MagRequest& out)
{
	ParamJSon::const_iterator param = json.find(key);
	if ( param != json.end() )
		out(key) = tonumber(param->second);
}

void setString(const string& key, const ParamJSon& json, MagRequest& out)
{
	ParamJSon::const_iterator param = json.find(key);
	if ( param != json.end() )
		out(key) = param->second;
}
void MagPlus::decode(MagRequest& out, const string& json)
{
	ParamJSon params(json);
/*
	'{"subpage_lower_left_latitude" : "40.8424",\
	"subpage_lower_left_longitude" : "-20.5033",\
	"subpage_map_area_definition" : "corners",\
	"subpage_map_hemisphere" : "north",\
	"subpage_map_projection" : "polar_stereographic",\
	"subpage_map_vertical_longitude" : "0",\
	"subpage_upper_right_latitude" : "62.073",\
	"subpage_upper_right_longitude" : "28.3054"}'
*/
	setDouble("subpage_lower_left_latitude", params, out);
	setDouble("subpage_lower_left_longitude", params, out);
	setString("subpage_map_area_definition", params, out);
	setString("subpage_map_hemisphere", params, out);
	setString("subpage_map_projection", params, out);
	setDouble("subpage_map_vertical_longitude", params, out);
	setDouble("subpage_upper_right_latitude", params, out);
	setDouble("subpage_upper_right_longitude", params, out);

}

//_____________________________________________________________________

#ifdef STANDALONE
int main( int argc, char** argv )
{
    MvApplication theApp( argc, argv );
    MagPlusService magplus;
    theApp.run();
}
#endif
