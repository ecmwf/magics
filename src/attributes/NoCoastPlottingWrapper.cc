
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file NoCoastPlottingAttributes.h
    \\brief Definition of NoCoastPlotting Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "NoCoastPlottingWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


NoCoastPlottingWrapper::NoCoastPlottingWrapper(): nocoastplotting_(new NoCoastPlotting())

{

	
	
	
} 
NoCoastPlottingWrapper::NoCoastPlottingWrapper(NoCoastPlotting* nocoastplotting): nocoastplotting_(nocoastplotting)
{
	
	
} 

NoCoastPlottingWrapper::~NoCoastPlottingWrapper()
{
	
}

void NoCoastPlottingWrapper::set(const MagRequest& request)
{
	
	

	if  (request.countValues("MAP_COASTLINE_RESOLUTION") ) {
		string resolution_value = request("MAP_COASTLINE_RESOLUTION");
		nocoastplotting_->resolution_ = resolution_value;
		}
	if  (request.countValues("MAP_COASTLINE_LAND_SHADE") ) {
		string land_value = request("MAP_COASTLINE_LAND_SHADE");
		nocoastplotting_->land_ = MagTranslator<string, bool>()(land_value);
		}
	if  (request.countValues("MAP_COASTLINE_SEA_SHADE") ) {
		string sea_value = request("MAP_COASTLINE_SEA_SHADE");
		nocoastplotting_->sea_ = MagTranslator<string, bool>()(sea_value);
		}
	if  (request.countValues("MAP_PREVIEW") ) {
		string preview_value = request("MAP_PREVIEW");
		nocoastplotting_->preview_ = MagTranslator<string, bool>()(preview_value);
		}
	if  (request.countValues("MAP_RIVERS") ) {
		string rivers_value = request("MAP_RIVERS");
		nocoastplotting_->rivers_ = rivers_value;
		}
	if  (request.countValues("MAP_RIVERS_THICKNESS") ) {
		int rivers_thickness_value = request("MAP_RIVERS_THICKNESS");
		nocoastplotting_->rivers_thickness_ = rivers_thickness_value;
		}
	if  (request.countValues("MAP_EFAS") ) {
		string efas_value = request("MAP_EFAS");
		nocoastplotting_->efas_ = efas_value;
		}
	if  (request.countValues("MAP_EFAS_DOMAIN") ) {
		string efas_domain_value = request("MAP_EFAS_DOMAIN");
		nocoastplotting_->efas_domain_ = efas_domain_value;
		}
	if  (request.countValues("MAP_EFAS_THICKNESS") ) {
		int efas_thickness_value = request("MAP_EFAS_THICKNESS");
		nocoastplotting_->efas_thickness_ = efas_thickness_value;
		}
	if  (request.countValues("MAP_USER_LAYER") ) {
		string user_layer_value = request("MAP_USER_LAYER");
		nocoastplotting_->user_layer_ = user_layer_value;
		}
	if  (request.countValues("MAP_USER_LAYER_NAME") ) {
		string user_layer_name_value = request("MAP_USER_LAYER_NAME");
		nocoastplotting_->user_layer_name_ = user_layer_name_value;
		}
	if  (request.countValues("MAP_USER_LAYER_PROJECTION") ) {
		string user_layer_projection_value = request("MAP_USER_LAYER_PROJECTION");
		nocoastplotting_->user_layer_projection_ = user_layer_projection_value;
		}
	if  (request.countValues("MAP_USER_LAYER_THICKNESS") ) {
		int user_layer_thickness_value = request("MAP_USER_LAYER_THICKNESS");
		nocoastplotting_->user_layer_thickness_ = user_layer_thickness_value;
		}
	
	if  (request.countValues("MAP_COASTLINE_LAND_SHADE_COLOUR") ) {
		string land_colour_value = request("MAP_COASTLINE_LAND_SHADE_COLOUR");
		nocoastplotting_->land_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(land_colour_value));
	}
		
	if  (request.countValues("MAP_COASTLINE_SEA_SHADE_COLOUR") ) {
		string sea_colour_value = request("MAP_COASTLINE_SEA_SHADE_COLOUR");
		nocoastplotting_->sea_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(sea_colour_value));
	}
		
	
		string boundaries_value = request.countValues("MAP_BOUNDARIES") ?  (string) request("MAP_BOUNDARIES") : "off";
	MagLog::debug() << " MAP_BOUNDARIES set to " << boundaries_value << endl;
	NoBoundariesWrapper* boundaries_wrapper = 0;
#ifdef MAGICS_EXCEPTION
	try
#endif
	{
		boundaries_wrapper = SimpleFactory<NoBoundariesWrapper>::create(boundaries_value);
	}
#ifdef MAGICS_EXCEPTION
	 catch (NoFactoryException) {
#else
	 if (!boundaries_wrapper) {
#endif
		MagLog::warning() << "[" << boundaries_value << "] is not a valid value for boundaries: reset to default -> [off]" << endl;
		boundaries_wrapper = SimpleFactory<NoBoundariesWrapper>::create("off");
	}
	boundaries_wrapper->set(request);
	nocoastplotting_->boundaries_ =  unique_ptr<NoBoundaries>(boundaries_wrapper->object());
	delete boundaries_wrapper;
	
		string cities_value = request.countValues("MAP_CITIES") ?  (string) request("MAP_CITIES") : "off";
	MagLog::debug() << " MAP_CITIES set to " << cities_value << endl;
	NoCitiesWrapper* cities_wrapper = 0;
#ifdef MAGICS_EXCEPTION
	try
#endif
	{
		cities_wrapper = SimpleFactory<NoCitiesWrapper>::create(cities_value);
	}
#ifdef MAGICS_EXCEPTION
	 catch (NoFactoryException) {
#else
	 if (!cities_wrapper) {
#endif
		MagLog::warning() << "[" << cities_value << "] is not a valid value for cities: reset to default -> [off]" << endl;
		cities_wrapper = SimpleFactory<NoCitiesWrapper>::create("off");
	}
	cities_wrapper->set(request);
	nocoastplotting_->cities_ =  unique_ptr<NoCities>(cities_wrapper->object());
	delete cities_wrapper;
	
	if  (request.countValues("MAP_RIVERS_STYLE") ) {
		string rivers_style_value = request("MAP_RIVERS_STYLE");
		nocoastplotting_->rivers_style_ = MagTranslator<string, LineStyle>()(rivers_style_value);
	}
		
	if  (request.countValues("MAP_RIVERS_COLOUR") ) {
		string rivers_colour_value = request("MAP_RIVERS_COLOUR");
		nocoastplotting_->rivers_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(rivers_colour_value));
	}
		
	
	if  (request.countValues("MAP_EFAS_STYLE") ) {
		string efas_style_value = request("MAP_EFAS_STYLE");
		nocoastplotting_->efas_style_ = MagTranslator<string, LineStyle>()(efas_style_value);
	}
		
	if  (request.countValues("MAP_EFAS_COLOUR") ) {
		string efas_colour_value = request("MAP_EFAS_COLOUR");
		nocoastplotting_->efas_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(efas_colour_value));
	}
		
	
	if  (request.countValues("MAP_USER_LAYER_STYLE") ) {
		string user_layer_style_value = request("MAP_USER_LAYER_STYLE");
		nocoastplotting_->user_layer_style_ = MagTranslator<string, LineStyle>()(user_layer_style_value);
	}
		
	if  (request.countValues("MAP_USER_LAYER_COLOUR") ) {
		string user_layer_colour_value = request("MAP_USER_LAYER_COLOUR");
		nocoastplotting_->user_layer_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(user_layer_colour_value));
	}
		
	
}

void NoCoastPlottingWrapper::print(ostream& out)  const
{
	out << "NoCoastPlottingWrapper[]";
}


    





#include "BoundariesWrapper.h"
static SimpleObjectMaker<Boundaries, NoBoundaries> NoCoastPlotting_map_boundaries_boundaries ("boundaries");
static SimpleObjectMaker<BoundariesWrapper, NoBoundariesWrapper> NoCoastPlotting_map_boundaries_boundaries_wrapper ("boundaries");


#include "BoundariesWrapper.h"
static SimpleObjectMaker<Boundaries, NoBoundaries> NoCoastPlotting_map_boundaries_on ("on");
static SimpleObjectMaker<BoundariesWrapper, NoBoundariesWrapper> NoCoastPlotting_map_boundaries_on_wrapper ("on");


#include "NoBoundariesWrapper.h"

static SimpleObjectMaker<NoBoundariesWrapper> NoCoastPlotting_map_boundaries_noboundaries_Wrapper("noboundaries");


#include "NoBoundariesWrapper.h"

static SimpleObjectMaker<NoBoundariesWrapper> NoCoastPlotting_map_boundaries_off_Wrapper("off");



#include "CitiesWrapper.h"
static SimpleObjectMaker<Cities, NoCities> NoCoastPlotting_map_cities_cities ("cities");
static SimpleObjectMaker<CitiesWrapper, NoCitiesWrapper> NoCoastPlotting_map_cities_cities_wrapper ("cities");


#include "CitiesWrapper.h"
static SimpleObjectMaker<Cities, NoCities> NoCoastPlotting_map_cities_on ("on");
static SimpleObjectMaker<CitiesWrapper, NoCitiesWrapper> NoCoastPlotting_map_cities_on_wrapper ("on");


#include "NoCitiesWrapper.h"

static SimpleObjectMaker<NoCitiesWrapper> NoCoastPlotting_map_cities_nocities_Wrapper("nocities");


#include "NoCitiesWrapper.h"

static SimpleObjectMaker<NoCitiesWrapper> NoCoastPlotting_map_cities_off_Wrapper("off");








