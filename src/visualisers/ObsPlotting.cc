/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsPlotting.cc
    \brief Implementation of the Template class ObsPlotting.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 23-Mar-2005
    
    Changes:
    
*/

#include "ObsPlotting.h"
#include "ObsTable.h"
#include "Layout.h"
#include "MetaData.h"
#include "ProgressObject.h"
using namespace magics;

ObsPlotting::ObsPlotting() 
{
	ObsTable::print();
	if ( ring_size_ == -1 )
		ring_size_ = size_;
}

ObsPlotting::~ObsPlotting() 
{
}

static int COUNT;
void ObsPlotting::operator()(Data& data, BasicGraphicsObjectContainer& out)
{
	ObsTable::print();
	std::set<string> needs;
	std::set<string> info;
	multimap<string, string> types;

	info.insert("type");

	data.getInfo(info, types);
	
	for (multimap<string, string>::const_iterator type = types.find("type"); type != types.end(); ++type)
	{		
		try {
			const ObsTemplate& obs = ObsTable::getTemplate(type->second);
			obs.set(this);
			obs.visit(needs);
		}
		catch (std::exception&)
		{
			MagLog::warning() << " Magics++ has no observation template for: " << type->second << "\n"
			               << " Please contact the Graphic team.\n";
		}
	}

	CustomisedPointsList values;	
	
	const Transformation& transformation = out.transformation();
	bool all = false;
	// false : we only need the points in the area
	data.customisedPoints(transformation, needs, values, all);

	out.push_back(new ClearObject());


	for (multimap<string, string>::const_iterator type = types.find("type"); type != types.end(); ++type)
	{
		try {
			const ObsTemplate& obs = ObsTable::getTemplate(type->second);


		
			for (CustomisedPointsList::const_iterator val = values.begin(); val != values.end(); ++val) 	{

				if ( type->second == (*val)->type() ) {
					obs(*(*val), out);
				}
			}
		}
		catch (std::exception&)
		{
		}
	}
}

	

/*!
 \brief Class information are given to the output-stream.
*/		
void ObsPlotting::print(ostream& out)  const
{
	out << "ObsPlotting[";
	out << "]";
}
void ObsPlotting::visit(MetaDataVisitor& visitor)
{
	visitor.add("ObsPlotting", "info");
	visitor.add("obs_plotted", tostring(COUNT));
}


#include "ObsItemFamily.h"
static SimpleObjectMaker<ObsStationTriangle, ObsItem> ObsStationTriangle("obs_station_triangle");
static SimpleObjectMaker<ObsStationRing, ObsItem> ObsStationRing("obs_station_ring");
static SimpleObjectMaker<ObsTimePlot, ObsItem> ObsTimePlot("obs_time_plot");
static SimpleObjectMaker<ObsWind, ObsItem> ObsWind("obs_wind");
static SimpleObjectMaker<ObsCloudAndWind, ObsItem> ObsCloudAndWind("obs_cloud_wind");
static SimpleObjectMaker<ObsTemperature, ObsItem> ObsTemperature("obs_temperature");
static SimpleObjectMaker<ObsPressure, ObsItem> ObsPressure("obs_pressure");
static SimpleObjectMaker<ObsPressureTendency, ObsItem> ObsPressureTendency("obs_pressure_tendency");
static SimpleObjectMaker<ObsDewPoint, ObsItem> ObsDewPoint("obs_dewpoint");
static SimpleObjectMaker<ObsVisibility, ObsItem> ObsVisibility("obs_visibility");
static SimpleObjectMaker<ObsPresentWeather, ObsItem> ObsPresentWeather("obs_present_weather");
static SimpleObjectMaker<ObsPressureLevel, ObsItem> ObsPressureLevel("obs_pressure_level");
static SimpleObjectMaker<ObsIdentifier, ObsItem> ObsIdentifier("obs_identification");
static SimpleObjectMaker<ObsCloud, ObsItem> ObsCloud("obs_cloud");
static SimpleObjectMaker<ObsPastWeather, ObsItem> ObsPastWeather("obs_past_weather");
static SimpleObjectMaker<ObsHeight, ObsItem> ObsHeight("obs_height");
static SimpleObjectMaker<ObsThickness, ObsItem> ObsThicjness("obs_thickness");
static SimpleObjectMaker<ObsDemoItem1, ObsItem> ObsDemo1("obs_demo_item_1");
static SimpleObjectMaker<ObsDemoItem2, ObsItem> ObsDemo2("obs_demo_item_2");
static SimpleObjectMaker<ObsWave, ObsItem> Wave("obs_wave_period_height");
static SimpleObjectMaker<ObsEra, ObsItem> Era("obs_era");
static SimpleObjectMaker<ObsSeaTemperature, ObsItem> SeaTempe("obs_sea_temperature");

