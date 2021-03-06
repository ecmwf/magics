
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file WindPlottingAttributes.h
    \\brief Definition of WindPlotting Attributes class.

    This file is automatically generated.
    Do Not Edit!

*/

#include "MagRequest.h"
#include "WindPlottingWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "MagicsGlobal.h"

using namespace magics;



WindPlottingWrapper::WindPlottingWrapper(): windplotting_(new WindPlotting())


{


	

}
WindPlottingWrapper::WindPlottingWrapper(WindPlotting* windplotting): windplotting_(windplotting)
{

	
}

WindPlottingWrapper::~WindPlottingWrapper()
{

}

void WindPlottingWrapper::set(const MagRequest& request)
{

	

	if  (request.countValues("LEGEND") ) {
		string legend_value = request("LEGEND");
		
		windplotting_->legend_ = MagTranslator<string, bool>()(legend_value);
		
		}
	if  (request.countValues("WIND_LEGEND_ONLY") ) {
		string legend_only_value = request("WIND_LEGEND_ONLY");
		
		windplotting_->legend_only_ = MagTranslator<string, bool>()(legend_only_value);
		
		}
	if  (request.countValues("WIND_LEGEND_TEXT") ) {
		string legend_text_value = request("WIND_LEGEND_TEXT");
		windplotting_->legend_text_ = legend_text_value;
		}
	if  (request.countValues("WIND_ADVANCED_METHOD") ) {
		string advanced_method_value = request("WIND_ADVANCED_METHOD");
		windplotting_->advanced_method_ = advanced_method_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_PARAMETER") ) {
		string colour_method_value = request("WIND_ADVANCED_COLOUR_PARAMETER");
		windplotting_->colour_method_ = colour_method_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_MAX_VALUE") ) {
		double max_value = request("WIND_ADVANCED_COLOUR_MAX_VALUE");
		windplotting_->max_ = max_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_MIN_VALUE") ) {
		double min_value = request("WIND_ADVANCED_COLOUR_MIN_VALUE");
		windplotting_->min_ = min_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_LEVEL_COUNT") ) {
		int count_value = request("WIND_ADVANCED_COLOUR_LEVEL_COUNT");
		windplotting_->count_ = count_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_LEVEL_TOLERANCE") ) {
		int tolerance_value = request("WIND_ADVANCED_COLOUR_LEVEL_TOLERANCE");
		windplotting_->tolerance_ = tolerance_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_REFERENCE_LEVEL") ) {
		double reference_value = request("WIND_ADVANCED_COLOUR_REFERENCE_LEVEL");
		windplotting_->reference_ = reference_value;
		}
	if  (request.countValues("WIND_ADVANCED_COLOUR_LEVEL_INTERVAL") ) {
		double interval_value = request("WIND_ADVANCED_COLOUR_LEVEL_INTERVAL");
		windplotting_->interval_ = interval_value;
		}
	doublearray  list_value;
	for (int i = 0; i < request.countValues("WIND_ADVANCED_COLOUR_LEVEL_LIST"); i++)
		list_value.push_back((double)request("WIND_ADVANCED_COLOUR_LEVEL_LIST", i));
	if ( !list_value.empty() )
		windplotting_->list_ = list_value;
	if  (request.countValues("WIND_ADVANCED_COLOUR_DIRECTION") ) {
		string direction_value = request("WIND_ADVANCED_COLOUR_DIRECTION");
		windplotting_->direction_ = direction_value;
		}
	stringarray  colours_value;
	for (int i = 0; i < request.countValues("WIND_ADVANCED_COLOUR_LIST"); i++)
		colours_value.push_back((string)request("WIND_ADVANCED_COLOUR_LIST", i));
	if ( !colours_value.empty() )
		windplotting_->colours_ = colours_value;
	
	
		string levels_value = request.countValues("WIND_ADVANCED_COLOUR_SELECTION_TYPE") ?  (string) request("WIND_ADVANCED_COLOUR_SELECTION_TYPE") : "count";
	MagLog::debug() << " WIND_ADVANCED_COLOUR_SELECTION_TYPE set to " << levels_value << endl;
	LevelSelectionWrapper* levels_wrapper = 0;
	try
	{
		levels_wrapper = SimpleFactory<LevelSelectionWrapper>::create(levels_value);
	}
	 catch (NoFactoryException&) {
		if (MagicsGlobal::strict()) {
            throw;
        }
		MagLog::warning() << "[" << levels_value << "] is not a valid value for levels: reset to default -> [count]" << endl;
		levels_wrapper = SimpleFactory<LevelSelectionWrapper>::create("count");
	}
	levels_wrapper->set(request);
	windplotting_->levels_ =  unique_ptr<LevelSelection>(levels_wrapper->object());
	delete levels_wrapper;
	
		string colourMethod_value = request.countValues("WIND_ADVANCED_COLOUR_TABLE_COLOUR_METHOD") ?  (string) request("WIND_ADVANCED_COLOUR_TABLE_COLOUR_METHOD") : "calculate";
	MagLog::debug() << " WIND_ADVANCED_COLOUR_TABLE_COLOUR_METHOD set to " << colourMethod_value << endl;
	ColourTechniqueWrapper* colourMethod_wrapper = 0;
	try
	{
		colourMethod_wrapper = SimpleFactory<ColourTechniqueWrapper>::create(colourMethod_value);
	}
	 catch (NoFactoryException&) {
		if (MagicsGlobal::strict()) {
            throw;
        }
		MagLog::warning() << "[" << colourMethod_value << "] is not a valid value for colourMethod: reset to default -> [calculate]" << endl;
		colourMethod_wrapper = SimpleFactory<ColourTechniqueWrapper>::create("calculate");
	}
	colourMethod_wrapper->set(request);
	windplotting_->colourMethod_ =  unique_ptr<ColourTechnique>(colourMethod_wrapper->object());
	delete colourMethod_wrapper;
	if  (request.countValues("WIND_ADVANCED_COLOUR_MAX_LEVEL_COLOUR") ) {
		string maxColour_value = request("WIND_ADVANCED_COLOUR_MAX_LEVEL_COLOUR");
		windplotting_->maxColour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(maxColour_value));
	}
		
	if  (request.countValues("WIND_ADVANCED_COLOUR_MIN_LEVEL_COLOUR") ) {
		string minColour_value = request("WIND_ADVANCED_COLOUR_MIN_LEVEL_COLOUR");
		windplotting_->minColour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(minColour_value));
	}
		
	
	if  (request.countValues("WIND_ADVANCED_COLOUR_LIST_POLICY") ) {
		string colour_policy_value = request("WIND_ADVANCED_COLOUR_LIST_POLICY");
		windplotting_->colour_policy_ = MagTranslator<string, ListPolicy>()(colour_policy_value);
	}
		
	
}

void WindPlottingWrapper::print(ostream& out)  const
{
	out << "WindPlottingWrapper[]";
}


#include "CountSelectionTypeWrapper.h"
static SimpleObjectMaker<CountSelectionType, LevelSelection> WindPlotting_wind_advanced_colour_selection_type_count ("count");
static SimpleObjectMaker<CountSelectionTypeWrapper, LevelSelectionWrapper> WindPlotting_wind_advanced_colour_selection_type_count_wrapper ("count");


#include "IntervalSelectionTypeWrapper.h"
static SimpleObjectMaker<IntervalSelectionType, LevelSelection> WindPlotting_wind_advanced_colour_selection_type_interval ("interval");
static SimpleObjectMaker<IntervalSelectionTypeWrapper, LevelSelectionWrapper> WindPlotting_wind_advanced_colour_selection_type_interval_wrapper ("interval");


#include "LevelListSelectionTypeWrapper.h"
static SimpleObjectMaker<LevelListSelectionType, LevelSelection> WindPlotting_wind_advanced_colour_selection_type_list ("list");
static SimpleObjectMaker<LevelListSelectionTypeWrapper, LevelSelectionWrapper> WindPlotting_wind_advanced_colour_selection_type_list_wrapper ("list");



#include "CalculateColourTechniqueWrapper.h"
static SimpleObjectMaker<CalculateColourTechnique, ColourTechnique> WindPlotting_wind_advanced_colour_table_colour_method_calculate ("calculate");
static SimpleObjectMaker<CalculateColourTechniqueWrapper, ColourTechniqueWrapper> WindPlotting_wind_advanced_colour_table_colour_method_calculate_wrapper ("calculate");


#include "ListColourTechniqueWrapper.h"
static SimpleObjectMaker<ListColourTechnique, ColourTechnique> WindPlotting_wind_advanced_colour_table_colour_method_list ("list");
static SimpleObjectMaker<ListColourTechniqueWrapper, ColourTechniqueWrapper> WindPlotting_wind_advanced_colour_table_colour_method_list_wrapper ("list");






