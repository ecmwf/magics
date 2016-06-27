/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file FortranMagics.cc
    \brief Implementation of the Template class FortranMagics.

    Magics Team - ECMWF 2007

    Started: Fri 9-Mar-2007

    Changes:

*/


#include "magics_ecmwf_log.h"
#include "FortranMagics.h"
#include "Timer.h"

#include "RootSceneNode.h"
#include "SceneNode.h"
#include "ViewNode.h"
#include "Coastlines.h"
#include "TextVisitor.h"
#include "LegendVisitor.h"
#include "VisualAction.h"
#ifdef HAVE_GRIB
#include "GribDecoder.h"
#endif
#include "MapGenDecoder.h"
#include "GeoPointsDecoder.h"
#include "Contour.h"
#include "UserPoint.h"
#include "SymbolInput.h"
#include "SymbolPlotting.h"
#include "Wind.h"
#include "Axis.h"
#include "XYList.h"
#include "GraphPlotting.h"
#include "InputMatrix.h"
#include "BoxPlotDecoder.h"
#include "BoxPlotVisualiser.h"
#include "SimplePolylineInput.h"
#include "SimplePolylineVisualiser.h"
#include "TitleTemplate.h"
#include "TaylorGrid.h"

#include "ImportAction.h"
#include "ImportPlot.h"
#include "ImportObjectHandler.h"

#include "MetaData.h"

#ifdef HAVE_NETCDF
#include "NetcdfDecoder.h"
#endif

#ifdef HAVE_BUFR
#include "ObsDecoder.h"
#include "EpsBufr.h"
#endif

#include "ObsJSon.h"

#include "ObsPlotting.h"

using namespace magics;

FortranMagics::FortranMagics() :  drivers_(0), output_(0), action_(0), empty_(true),
		gribindex_(0),legend_todo_(false), symbolinput_todo_ (false), matrixinput_todo_(false), polyinput_todo_(false)

{
	ASSERT (singleton_ == 0);
	singleton_ = this;

	writeMagLog("fortran");
}


FortranMagics::~FortranMagics()
{
	if ( drivers_ ) delete drivers_;
//	if ( root_ ) delete root_;
	if ( output_ ) delete output_;
	singleton_ = 0;

	/*
	ParameterManager::release();
	TitleTemplate::release();

	GribDecoder::releaseContext();
	*/
}

/*!
 Class information are given to the output-stream.
*/
void FortranMagics::print(ostream& out)  const
{
	out << "FortranMagics[";
	out << "]";
}

void FortranMagics::popen()
{
   MagLog::info() << "popen()" << endl;

   if(getEnvVariable("MAGPLUS_QUIET").empty() )
   {
	MagLog::userInfo() << "------------------------------------------------------------------\n";
	MagLog::userInfo() << "\n";
	MagLog::userInfo() << "			  "<< getMagicsVersionString() <<"\n";
	MagLog::userInfo() << "\n";
	MagLog::userInfo() << " Meteorological Applications Graphics Integrated Colour System\n";
	MagLog::userInfo() << "\n";
	MagLog::userInfo() << "			    Developed By\n";
	MagLog::userInfo() << "\n";
	MagLog::userInfo() << "   The European Centre for Medium-Range Weather Forecasts\n";
	MagLog::userInfo() << "\n";
	//MagLog::userInfo() << "		      Copyright ECMWF "<<MAGICS_COPYRIGHT_PERIOD<<"\n";
	MagLog::userInfo() << "\n";
	MagLog::userInfo() << "------------------------------------------------------------------\n";
   }
  // actions_.push(&FortranMagics::legend);
   actions_.push(&FortranMagics::subpage);
   actions_.push(&FortranMagics::page);
   actions_.push(&FortranMagics::superpage);
   actions_.push(&FortranMagics::drivers);
}

/*! \brief Main dispatch method
  Here is where the real magics is happen. Everything is dispatched, followed
  by a comprehensive clean-up.
*/
void FortranMagics::pclose()
{
	MagLog::info()<< "pclose()" << endl;
	if (!empty_) {
		finish();
		dispatch();
	}

	if ( root_ && drivers_ ) {

		BasicGraphicsObject* object = root_->close();
		if ( object ) {

			/***   Start clean-up  ***/
			drivers_->dispatch(object);
			drivers_->closeDrivers();

			delete root_;
			delete drivers_;
			delete output_;

			drivers_ = 0;
			root_ = 0;
			output_ = 0;
		}

	}

	// the Magics log messages are not broadcast until the next log event - therefore, the
	// last log message will not be broadcast. We fix that by flushing the message streams
	// - we only need to do one of them, and all will be flushed behind the scenes.
	MagLog::info().flush();

	// We reset all the parameters to their default,
	// then a consecutive call to popen will not be affected by the current values.
	ParameterManager::reset();


	if(getEnvVariable("MAGPLUS_QUIET").empty() )
	{
	/*
		MagLog::userInfo() << "------------------------------------------------------------------\n";
		MagLog::userInfo() << " Output files generated:\n";
		stringarray::iterator it = output_resource_list_.begin();
		stringarray::iterator itend = output_resource_list_.end();
		for(; it != itend; it++)
		{
			MagLog::userInfo() << "  - "<<(*it)<<"\n";
		}
		MagLog::userInfo() << "\n";
	*/
		MagLog::userInfo() << "------------------------------------------------------------------\n";
		MagLog::userInfo() << "    COMPLETED\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "    Any problems or suggestions? Please contact us at\n";
		MagLog::userInfo() << "                   magics@ecmwf.int\n";
		MagLog::userInfo() << "------------------------------------------------------------------\n";
	}
}

void FortranMagics::drivers()
{
	if (!drivers_)  drivers_ = new DriverManager();
	if (!output_)   output_ = new OutputHandler();
	output_->set(*drivers_);
}

void FortranMagics::subpage()
{
	axisContainer_ = new FortranViewNode();

	axisContainer_->push_back(new MetaDataVisitor());
	top()->push_back(axisContainer_);
	axisContainer_->getReady();
	push(axisContainer_);
}


void FortranMagics::page()
{
	if ( empty() ) return;

	while (top() != root_) {
		pop();
		if ( empty() ) break;
	}


	FortranSceneNode* node = new FortranSceneNode();
	root_->insert(node);
	// Just a test for metadata collection
	MetaDataVisitor* meta = new MetaDataVisitor();
	node->push_back(meta);
	push(node);
}


void FortranMagics::newpage()
{
	if ( empty() ) return;
	BasicSceneObject* to = top();

	while ( to != root_) {
		pop();
		to = top();
		if ( empty() ) break;
	}

	root_->newpage();

	//push(node);
}


void FortranMagics::superpage()
{
	root_ = new FortranRootSceneNode();

		push(root_);

		root_->getReady();
		drivers_->setDriversWidth(root_->absoluteWidth());
			drivers_->setDriversHeight(root_->absoluteHeight());
			drivers_->openDrivers();
}


void FortranMagics::simplelegend()
{
	// used fronm the python interface!
	// add a new legend!
	legends_.clear();

	legend();
}

void FortranMagics::legend()
{
    if ( legends_.empty() == false ) return;

    if (!legend_todo_) return;

    legend_todo_ = false;
    string mode;
    ParameterManager::get("legend_box_mode", mode);

    if (magCompare(mode, "positional") )
    	legends_.push_back(new FortranPositionalLegendVisitor());
    else
    	legends_.push_back(new FortranAutomaticLegendVisitor());
}


void FortranMagics::plegend()
{
	legend_todo_ = true;
	legends_.clear();
    actions_.push(&FortranMagics::legend);
}

void FortranMagics::poverlay()
{
	actions();


		action_ = new VisualAction();
		ImportAction* input = new ImportAction();
		ImportPlot* plot = new ImportPlot();
		top()->push_back(action_);
		action_->data(input);

		action_->visdef(plot);

}


void FortranMagics::pimport()
{
	ImportObjectHandler* object = new ImportObjectHandler();
	later_.push_back(object);
}


void FortranMagics::pnew(const string& type)
{
	MagLog::info() << "pnew(" << type << ")" << endl;

	if ( magCompare(type, "subpage") )
	{
		if ( empty_ ) return;
		finish();
		pop();
		actions_.push(&FortranMagics::legend);
		actions_.push(&FortranMagics::subpage);
	}
	if ( magCompare(type, "page") )
	{
		   if ( empty_ ) return;
				finish();
				dispatch();
        empty_ = true;
		pop();
		actions_.push(&FortranMagics::legend);
		actions_.push(&FortranMagics::subpage);
		actions_.push(&FortranMagics::page);

	}
	if ( magCompare(type, "super_page") || magCompare(type, "superpage"))
	{
			if ( empty_ ) {
				actions_.push(&FortranMagics::newpage);
				return;
			}
			finish();
			dispatch();
            empty_ = true;
			actions_.push(&FortranMagics::legend);
		    actions_.push(&FortranMagics::subpage);
		    actions_.push(&FortranMagics::page);
			actions_.push(&FortranMagics::newpage);

	}
	// WE reset !
	axisContainer_ = 0;
	action_ = 0;


	string legend;
	ParameterManager::get("legend", legend);
	legend_todo_ = magCompare(legend, "on");
}


void FortranMagics::actions()
{
	Timer timer("magics", "setting");
	while (!actions_.empty())
	{
		Action action = actions_.top();
		(this->*action)();
		actions_.pop();
		empty_ = false;
	}
}


void FortranMagics::pcoast()
{
	actions();

	Coastlines* coastlines = new Coastlines();
	axisContainer_->push_back(coastlines);
}

void FortranMagics::ptaylor()
{
	actions();

	TaylorGrid* taylor = new TaylorGrid();
	top()->push_back(taylor);
}
#include "TephiGrid.h"
void FortranMagics::ptephi()
{
	actions();

	TephiGrid* tephi = new TephiGrid();
	top()->push_back(tephi);
}
void FortranMagics::pobs()
{
	actions();
#ifdef HAVE_BUFR
	action_ = new VisualAction();
	ObsDecoder* obs = new ObsDecoder();
	if ( obs->defined() ) {
		action_->data(obs);
		top()->push_back(action_);
		action_->visdef(new ObsPlotting());
		return;
	}
#endif


	action_ = new VisualAction();
	action_->data(new ObsJSon());
	top()->push_back(action_);
	action_->visdef(new ObsPlotting());
	return;


    MagLog::warning() << "No Support for Obs Plotting" << endl;
}


#include "MatrixTestDecoder.h"
void FortranMagics::ptest()
{
	actions();
	action_ = new VisualAction();
	action_->data(new MatrixTestDecoder());
	top()->push_back(action_);
}


/*!  \brief Finish plot by checking axis, legend and texts
 *
*/
void FortranMagics::finish()
{
	if ( !empty_ ) {
		actions(); // The flag to force the generation of the plot has been set!
		while ( !axis_.empty() )
		{
			axisContainer_->push_back(axis_.top());
			axis_.pop();
		}
	}

	if ( !axisContainer_ ) return;
	// check if we have to add a legend!
	if ( !legends_.empty() && axisContainer_ &&  !axisContainer_->items_empty() )
	{
		legend();
		for (vector<LegendVisitor* >::iterator legend = legends_.begin();  legend != legends_.end(); ++legend)
		{
			top()->legend(*legend);
		}
		legends_.clear();
	}

	// Check any text
	for (vector<BasicSceneObject* >::iterator other = later_.begin();  other != later_.end(); ++other)
	{
		top()->push_back(*other);
	}
	later_.clear();

	for (vector<FortranTextVisitor* >::iterator text = texts_.begin();  text != texts_.end(); ++text)
	{
		top()->text(*text);
	}
	texts_.clear();
}


/*!  \brief Dispatch scene graph to drivers
 * \sa DriverManager::dispatch()
 * \sa FortranRootSceneNode::visualise()
*/
void FortranMagics::dispatch()
{
	if ( !root_ )  // Nothing has been done so far!
		return;
	drivers_->dispatch(root_->visualise());
	MetaDataVisitor::collect();
	root_->release();
}


void FortranMagics::pmapgen()
{
	actions();

	action_ = new VisualAction();
	action_->data(new MapGenDecoder());
	top()->push_back(action_);

}

#ifdef HAVE_GRIB
void FortranMagics::pgrib()
{
	actions();
	action_ = new VisualAction();
	static string gribfile;

	string grib;
	ParameterManager::get("grib_input_file_name", grib);
	int index;
	ParameterManager::get("grib_field_position", index);


	if ( grib == gribfile  )
	{
		if ( index == gribindex_ )
		{
    		gribindex_++;
    	}
		else
		{
    		gribindex_ = index;
    	}
    	ParameterManager::set("grib_field_position", gribindex_);
    }
	else
	{
    	gribfile = grib;
    	gribindex_ = index;
    }

	action_->data(new GribDecoder());
	top()->push_back(action_);
}

void FortranMagics::pimage()
{
	MagLog::warning() <<" pimage is deprecated! Please use pcont."<< endl;
}
#else
void FortranMagics::pgrib()
{
}

void FortranMagics::pimage()
{
	MagLog::warning() <<" pimage is deprecated! Please use pcont."<< endl;
}
#endif

void FortranMagics::pgeo()
{
	actions();
	action_ = new VisualAction();
	action_->data(new GeoPointsDecoder());
	top()->push_back(action_);
}

void FortranMagics::pnetcdf()
{
#ifdef HAVE_NETCDF
	actions();

	action_ = new VisualAction();
	action_->data(new NetcdfDecoder());
	top()->push_back(action_);


#endif
}
#include "InputData.h"
void FortranMagics::pinput()
{

	actions();

		action_ = new VisualAction();
		action_->data(new InputData());
		top()->push_back(action_);


}
#include "TableDecoder.h"
void FortranMagics::ptable()
{

	actions();

		action_ = new VisualAction();
		action_->data(new TableDecoder());
		top()->push_back(action_);

}

#ifdef HAVE_ODB
#include "OdaDecoder.h"
#endif
void FortranMagics::podb()
{
	actions();
#ifdef HAVE_ODB


    	action_ = new VisualAction();
    	action_->data(new OdaGeoDecoder());
    	top()->push_back(action_);

#endif
}


void FortranMagics::data(Data* data)
{
	ASSERT ( action_ == 0);
	action_ = new VisualAction();
	action_->data(data);
	top()->push_back(action_);
}



bool FortranMagics::geographical()
{
	string projection;
	ParameterManager::get("subpage_map_projection", projection);
	if ( magCompare(projection, "cartesian") ) return false;
	if ( magCompare(projection, "taylor") ) return false;
	if ( magCompare(projection, "thermo") ) return false;
	if ( magCompare(projection, "tephiinfo") ) return false;
	return true;
}

template <class T>
void param(const string& from, const string& to, T& val)
{
	ParameterManager::get(from, val);
	ParameterManager::set(to,val);
}


void split(VisualAction& action) {
/*	string split;
	//ParameterManager::get("contour_line_plotting", split);
	if ( magCompare(split, "split"))
	{
			MagLog::warning() << " contour_line_plotting is deprecated" << endl;
			double level;
			ParameterManager::get("contour_split_level", level);
			double max, min;
			ParameterManager::get("contour_max_level", max);
			ParameterManager::get("contour_min_level", min);
			// set the below contour
			ParameterManager::set("contour_max_level", level);

			LineStyle style, highlightstyle;
			double thickness, highlightthickness;
			string colour, highlightcolour;

			param("contour_below_line_style", "contour_line_style", style);
			param("contour_below_line_thickness", "contour_line_thickness", thickness);
			param("contour_below_line_colour", "contour_line_colour", colour);
			param("contour_below_highlight_style", "contour_highlight_line_style", highlightstyle);
			param("contour_below_highlight_thickness", "contour_highlight_line_thickness", highlightthickness);
			param("contour_below_highlight_colour", "contour_highlight_line_colour",highlightcolour);

			action.visdef(new Contour());

			// set the above contour
			ParameterManager::set("contour_max_level", max);
			ParameterManager::set("contour_min_level", level);
			param("contour_above_line_style", "contour_line_style", style);
			param("contour_above_line_thickness", "contour_line_thickness", thickness);
			param("contour_above_line_colour", "contour_line_colour", colour);
			param("contour_above_highlight_style", "contour_highlight_line_style", style);
			param("contour_above_highlight_thickness", "contour_highlight_line_thickness", thickness);
			param("contour_above_highlight_colour", "contour_highlight_line_colour", colour);
			action.visdef(new Contour());
			// Should we reset???
	}
	else {   */
		action.visdef(new Contour());
//	}
}


void FortranMagics::pcont()
{
	// First check any split contour! I hate it!
	Timer timer("pcont", "setting");
	actions();

		if ( !action_  || matrixinput_todo_ )
		{
			action_ = new VisualAction();
			InputMatrix* input = new InputMatrix();
			matrixinput_todo_ = false;
			if (input->defined() )
				action_->data(input);
			else {
				delete input;
#ifdef HAVE_GRIB
			// Sylvie: Is this causing GribDecoder MagExceptions when matrx input is faulty?
				action_->data(new GribDecoder());
#else
				MagLog::warning() <<" Attempt to decode data from GRIB, but GRIB support is disabled!"<< endl;
#endif
		}
		top()->push_back(action_);
	}

	split(*action_);


}

void FortranMagics::pwind()
{
	actions();
	if ( matrixinput_todo_ ) {
		action_ = 0;
	}
	if ( !action_  )
	{
		action_ = new VisualAction();
		InputMatrix* input = new InputMatrix();
		matrixinput_todo_ = false;
		if (input->defined() )
			action_->data(input);
		else {
			delete input;
#ifdef HAVE_GRIB
			// Sylvie: Is this causing GribDecoder MagExceptions when matrx input is faulty?
			GribDecoder* grib = new GribDecoder();
			grib->dimension(2);
			action_->data(grib);
#else
			MagLog::warning() <<" Attempt to decode Wind from GRIB, but GRIB support is disabled!"<< endl;
#endif
		}
		top()->push_back(action_);
	}
	action_->set2D();
	action_->visdef(new Wind());
	action_ = 0;
}





void FortranMagics::ptext()
{
	string mode;
	ParameterManager::get("text_mode", mode);

	FortranTextVisitor* node;
	if ( magCompare(mode, "positional") )
		node = new FortranPositionalTextVisitor();
	else
		node = new FortranAutomaticTextVisitor();

	texts_.push_back(node);
	empty_ = false;
}


void FortranMagics::psymb()
{
	actions();

 	string mode;
 	string wind;
 	ParameterManager::get("symbol_position_mode", mode);
 	ParameterManager::get("symbol_type", wind);
 	if ( magCompare(mode, "graph")  )
	{
 		action_ = new VisualAction();
 		SymbolInput* input = new SymbolInput();
		top()->push_back(action_);
		action_->data(input);
		MagLog::dev() << *input << "\n";
		SymbolPlotting* symbol = new SymbolPlotting();
		MagLog::dev() << *symbol << "\n";
		action_->visdef(symbol);
		action_ = 0;
	}
	else
	{



			    if ( !action_ || symbolinput_todo_ ) {
			    	action_ = new VisualAction();
			    	SymbolInput* input = new SymbolInput();
			    	top()->push_back(action_);
			    	action_->data(input);
			    	MagLog::dev() << *input << "\n";
			    	symbolinput_todo_ = false;
			    }
			    if (magCompare(wind, "wind")) {
			    	Wind* wind = new Wind();
			    	MagLog::dev() << *wind << "\n";
			    	action_->visdef(wind);
			    }
			    else {
			    	SymbolPlotting* symbol = new SymbolPlotting();
			    	MagLog::dev() << *symbol << "\n";
			    	action_->visdef(symbol);
			    }
			action_ = 0;

	}

}

void FortranMagics::pline()
{
	actions();


	if ( !action_ ) {
		action_ = new VisualAction();

		polyinput_todo_ = false;
		SimplePolylineInput* input = new SimplePolylineInput();
		top()->push_back(action_);
		action_->data(input);
	}

	action_->visdef(new SimplePolylineVisualiser());



}
#include "GeoJSon.h"
void  FortranMagics::geojson()
{
	actions();


	action_ = new VisualAction();

	GeoJSon* geo = new GeoJSon();

	top()->push_back(action_);
	action_->data(geo);

}
#include "WrepJSon.h"
#include "EpsXmlInput.h"
#include "EpsGraph.h"
void  FortranMagics::wrepjson()
{
	actions();


	action_ = new VisualAction();

	WrepJSon* wrep = new WrepJSon();

	top()->push_back(action_);
	action_->data(wrep);

}
void  FortranMagics::metbufr()
{
#ifdef HAVE_BUFR
	actions();
	action_ = new VisualAction();

	EpsBufr* bufr = new EpsBufr();

	top()->push_back(action_);
	action_->data(bufr);
	return;

#endif
	MagLog::warning() << "No Support for Weather Parameters Plotting" << endl;

}
void  FortranMagics::epsinput()
{
	actions();
	action_ = new VisualAction();

	EpsXmlInput* input = new EpsXmlInput();

	top()->push_back(action_);
	action_->data(input);
}
#include "MetgramGraph.h"
void  FortranMagics::metgraph()
{
	actions();
	if ( !action_ ) {
		MagLog::error() << "epscloud -> No data defined " << endl;
		exit(1);
	}
	MetgramGraph* graph = new MetgramGraph();

	action_->visdef(graph);
}

void  FortranMagics::epscloud()
{
	actions();
	if ( !action_ ) {
		MagLog::error() << "epscloud -> No data defined " << endl;
		exit(1);
	}
	EpsCloud* epscloud = new EpsCloud();

	action_->visdef(epscloud);
}
void  FortranMagics::epsplumes()
{
	actions();
	if ( !action_ ) {
		MagLog::error() << "epsplumes -> No data defined " << endl;
		exit(1);
	}
	EpsPlume* plumes = new EpsPlume();

	action_->visdef(plumes);
}

void FortranMagics::epsgraph()
{
	actions();
	if ( !action_ ) {
			MagLog::error() << "epscloud -> No data defined " << endl;
			exit(1);
		}
	EpsGraph* epsgraph = new EpsGraph();
	action_->visdef(epsgraph);
}

void FortranMagics::epswave()
{
	actions();
	if ( !action_ ) {
		MagLog::error() << "epscloud -> No data defined " << endl;
		exit(1);
	}
	EpsWave* eps = new EpsWave();
	action_->visdef(eps);
}

void FortranMagics::epswind()
{
	actions();
	if ( !action_ ) {
		MagLog::error() << "epscloud -> No data defined " << endl;
		exit(1);
	}
	EpsWind* epswind = new EpsWind();
	action_->visdef(epswind);
}


void FortranMagics::epsbar()
{
	EpsBar* epsbar = new EpsBar();
	action_->visdef(epsbar);
}
void FortranMagics::epsshading()
{
	actions();
	if ( !action_ ) {
		MagLog::error() << "epscloud -> No data defined " << endl;
		exit(1);
	}
	EpsShade* eps = new EpsShade();
	action_->visdef(eps);
}

void FortranMagics::paxis()
{
	try {
		string orientation;

		ParameterManager::get("axis_orientation", orientation);
//
		if (magCompare(orientation, "vertical") ) {
			Axis* vaxis = new VerticalAxis();
			MagLog::dev() << *vaxis << "\n";
			axis_.push(vaxis);
		}
		else {
			Axis* haxis = new HorizontalAxis();
			MagLog::dev() << *haxis << "\n";
			axis_.push(haxis);
		}
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
 	empty_= false; // Force the generation of the plot!
}

void FortranMagics::prepare()
{
    actions();
}

void FortranMagics::pgraph()
{
	actions();




	GraphPlotting* graph = new GraphPlotting();


	// do we need a new Action?
	// we check the previous one!
	// if it looks ok we create a new one

	if ( ( action_ && action_->isValid() ) || !action_ ) {
		action_ = new VisualAction();
		top()->push_back(action_);
		XYList* input = new XYList();
		action_->data(input);
		MagLog::dev() << *input << "\n";
	}




	action_->visdef(graph);
}


void FortranMagics::pboxplot()
{
	actions();

	action_ = new VisualAction();

 	BoxPlotDecoder* input = new BoxPlotDecoder();
 	BoxPlotVisualiser* plot = new BoxPlotVisualiser();
	top()->push_back(action_);
	action_->data(input);
	MagLog::dev() << *input << "\n";
	action_->visdef(plot);
}

FortranMagics* FortranMagics::singleton_ = 0;
