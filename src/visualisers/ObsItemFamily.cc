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

/*! \file ObsItemFamily.cc
    \brief Implementation of the Template class ObsItemfamily.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/
#include "ObsItemFamily.h"
#include "iomanip"
#include "Transformation.h"

#include "Flag.h"
#include "Text.h"
#include "PaperPoint.h"

using namespace magics;


map<int, string> ObsCloudAndWind::origins_;

void  ObsWind::setOrigins() 
{
	
}
void ObsWind::visit(std::set<string>& tokens)
{


	if (!owner_->wind_visible_) return;
	tokens.insert(speed_);
	tokens.insert(direction_);

}
void ObsWind::operator()( CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (!owner_->wind_visible_) return;

	map<string, double>::iterator it = point.begin();
	map<string, double>::iterator en = point.end();
	for(;it != en; ++it)
	{
	 MagLog::debug() << " >>> "<<it->first<<" -> " << it->second << endl;
	}
	Colour colour =  owner_->wind_colour_->automatic() ? *owner_->colour_ : *owner_->wind_colour_;

	CustomisedPoint::const_iterator speed = point.find(speed_);
	if ( speed == point.end() ) return; 
	CustomisedPoint::const_iterator direction = point.find(direction_);
	if ( direction == point.end() ) return; 

	FlagItem* flag = new FlagItem();
	flag->setColour(colour);
	flag->length(owner_->size_ * 2.5); // Size to be adjusted later!

	const string origin = "circle";


MagLog::debug() << "OBS ITEM - ObsWind - Lon/Lat: "<<point.longitude()<<" / "<<point.latitude()
     << "\n\twind_speed:     " << point[speed_]
     << "\n\twind_direction: " << point[direction_]
     << "\n\tcloud amount:   " << point["total_cloud"]<<" -> "<<origin << std::endl;


	flag->setOriginHeight(owner_->ring_size_);
	flag->setOriginMarker(origin);
	flag->x(0);
	flag->y(0);
	
	const Transformation& transformation = symbol.parent().transformation();
	PaperPoint pp(point.longitude(), point.latitude());
	std::pair<double, double> wind = std::make_pair(speed->second, direction->second);
	transformation.reprojectSpeedDirection(pp, wind);
	
	flag->speed(speed->second);
	flag->direction(direction->second);

	if (point.latitude() <0 ) 
		flag->setHemisphere(SOUTH);
	symbol.add(flag);
}
void  ObsCloudAndWind::setOrigins() 
{
	if ( !origins_.empty() ) return;
	origins_[0] = "N_0";
	origins_[1] = "N_1";
	origins_[2] = "N_2";
	origins_[3] = "N_3";
	origins_[4] = "N_4";
	origins_[5] = "N_5";
	origins_[6] = "N_6";
	origins_[7] = "N_7";
	origins_[8] = "N_8";
	origins_[59] = "N_9";
	origins_[60] = "N_9";
	origins_[61] = "N_9";
	origins_[62] = "N_9";
}

void ObsCloudAndWind::operator()( CustomisedPoint& point, ComplexSymbol& symbol) const
{

	if (!owner_->wind_visible_) return;

	Colour colour =  owner_->wind_colour_->automatic() ? *owner_->colour_ : *owner_->wind_colour_;

	map<string, double>::iterator it = point.begin();
	map<string, double>::iterator en = point.end();
	for(;it != en; ++it)
	{
	 MagLog::debug() << " >>> "<<it->first<<" -> " << it->second << endl;
	}
	symbol.setHeight(owner_->size_);

	int total_cloud = maground((point["total_cloud"]/100.)*8);
	MagLog::debug() << "total_cloud-->" << point["total_cloud"] << "--->" << total_cloud << endl;
	map<int, string>::const_iterator marker = origins_.find(total_cloud);
	string origin;

	if  ( marker != origins_.end() )
		origin =  marker->second;
	else {

		// here we check that the sky is not obscured
			marker = origins_.find(int(point["low_cloud"]));
			origin = marker != origins_.end() ? marker->second : "magics_13";

	}

	CustomisedPoint::const_iterator ispeed = point.find("wind_speed");
	double speed = ( ispeed == point.end() ) ? 0 : ispeed->second;
	CustomisedPoint::const_iterator idirection = point.find("wind_direction");
	double direction = ( idirection == point.end() ) ? 0 : idirection->second;

	if ( speed == 0 && direction == 0) {
		// No wind information ...Just plot the nebulosity
		SymbolItem*  object = new SymbolItem();
		object->x(0);
		object->y(0);


		object->colour(colour);


		object->symbol(origin);

		object->height(owner_->ring_size_*.35);

		symbol.add(object);
		return;
	}

	FlagItem* flag = new FlagItem();
	flag->setColour(colour);
	flag->length(owner_->size_*2.5); // Size to be adjusted later!




	MagLog::debug() << "OBS ITEM - ObsWind - Lon/Lat: "<<point.longitude()<<" / "<<point.latitude()
     << "\n\twind_speed:     " << point["wind_speed"] 
     << "\n\twind_direction: " << point["wind_direction"]
     << "\n\tcloud amount:   " << point["total_cloud"]<< "--->" << total_cloud << "--->" << origin << std::endl;

	flag->setOriginHeight(owner_->ring_size_);
	flag->setOriginMarker(origin);
	flag->x(0);
	flag->y(0);
	
	const Transformation& transformation = symbol.parent().transformation();
	PaperPoint pp(point.longitude(), point.latitude());
	std::pair<double, double> wind = std::make_pair(speed, direction);
	transformation.reprojectSpeedDirection(pp, wind);
	
	flag->speed(speed);
	flag->direction(direction);

	if (point.latitude() <0 ) 
		flag->setHemisphere(SOUTH);
	symbol.add(flag);
}

void ObsPressure::visit(std::set<string>& tokens)
{

	if (!owner_->pressure_visible_) return;

	tokens.insert("msl_pressure");
}

void ObsPressure::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->pressure_visible_) return;

	CustomisedPoint::const_iterator value = point.find("msl_pressure");
	if ( value == point.end() ) return;
	
	Colour colour =  owner_->pressure_colour_->automatic() ? *owner_->colour_ : *owner_->pressure_colour_;

	TextItem*  object = new TextItem();
	// for the pressure we show the 3 last digits of the pressure in decaPascal.
	object->x(column_);
	object->y(row_);
	MagFont font;
	font.colour(colour);
	font.name("sansserif");
	font.size(owner_->size_);
	
	ostringstream os;
	double pressure = fmod(value->second/10, 1000);
	os <<  setw(3) << setfill('0') << pressure;
	object->text(os.str());


	MagLog::debug() << "\tPressure:  " << value->second << " = " << pressure << " -> "<<os.str() << "\n";

	object->font(font);
	symbol.add(object);
}

void ObsPressureLevel::visit(std::set<string>& tokens)
{

	if (!owner_->upper_air_visible_) return;
	tokens.insert("pressure");
}

void ObsPressureLevel::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->upper_air_visible_) return;
	CustomisedPoint::const_iterator value = point.find("pressure");
	if ( value == point.end() ) return;
	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);

	MagFont font("sansserif");
	Colour colour =  owner_->upper_air_colour_->automatic() ? *owner_->colour_ : *owner_->upper_air_colour_;
	font.colour(colour);
	
	const double pressure = value->second/100.;

	object->text(tostring(pressure));

	MagLog::debug() << "\tPressureLevel: " << value->second << " = " << pressure << "\n";
	font.size(owner_->size_);
	object->font(font);
	symbol.add(object);
}
	
void ObsPressureTendency::visit(std::set<string>& tokens)
{

	if (! owner_->pressure_tendency_visible_) return;
	tokens.insert("pressure_tendency_amount");
	tokens.insert("pressure_tendency_characteristic");
}
	
void ObsPressureTendency::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (! owner_->pressure_tendency_visible_) return;
	CustomisedPoint::const_iterator value = point.find("pressure_tendency_amount");
	if ( value == point.end() ) return;

	Colour colour =  owner_->pressure_tendency_colour_->automatic() ? *owner_->colour_ : *owner_->pressure_tendency_colour_;
	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);
	
	ostringstream os;	
	if(value->second>=0) os <<  setw(2) << setfill('0') << value->second * .1;
	else os << "-" << setw(2) << setfill('0') << value->second * -.1;
	
	// The Pressure tendancy is red if negative!
	MagFont font("sansserif");
	if ( value->second < 0 )
		colour = Colour("red");
	font.colour(( value->second < 0 ) ? Colour("red") :colour);

	object->text(os.str());
	font.size(owner_->size_);
	object->font(font);
	symbol.add(object);
	value = point.find("pressure_tendency_characteristic");
	if ( value == point.end() ) return; 
	SymbolItem*  tendancy = new SymbolItem();
	tendancy->x(column_+1);
	tendancy->y(row_);	
	
	tendancy->colour(colour);
	ostringstream oss;	
	oss << "a_"  << value->second;
	tendancy->symbol(oss.str());

	MagLog::debug() << "\tPressure tendency--->" << oss.str() << "\n";

	tendancy->height(owner_->size_*0.2); // A bit too big !
	symbol.add(tendancy);
}

	
void ObsDewPoint::visit(std::set<string>& tokens)
{

	if (!owner_->dewpoint_visible_) return;
	tokens.insert("dewpoint");
}

void ObsDewPoint::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->dewpoint_visible_) return;
	CustomisedPoint::const_iterator value = point.find("dewpoint");
	if ( value == point.end() ) return; 
	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	Colour colour =  owner_->dewpoint_colour_->automatic() ? *owner_->colour_ : *owner_->dewpoint_colour_;
	font.colour(colour);
	font.size(owner_->size_);
	object->x(column_);
	object->y(row_);
	
	// The temperature is displayed in Celsius.
	const double tempe = maground(value->second-273.15);

	MagLog::debug() << "\tDewPoint--->" << point["dewpoint_2meters"] << " = " << tempe << "\n";

	object->text(tostring(tempe));
	object->font(font);
	//object->setJustification(MCENTRE);	
	
	symbol.add(object);
}

	
void ObsVisibility::visit(std::set<string>& tokens)
{
	if (!owner_->visibility_visible_) return;
	tokens.insert("horizontal_visibility");
}
/*! 
   Documnetaion found at :	
     http://www.zetnet.co.uk/sigs/weather/Met_Codes/vvcode.htm 
*/
void ObsVisibility::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (!owner_->visibility_visible_) return;
	CustomisedPoint::const_iterator value = point.find("horizontal_visibility");
	if ( value == point.end() ) return;
	Colour colour =  owner_->visibility_colour_->automatic() ? *owner_->colour_ : *owner_->visibility_colour_;
	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);

	MagFont font("sansserif");
	font.colour(colour);
	
	const double vv = value->second;
	string val;
	if ( vv <= 5000. )
		val = tostring(vv/100.);
	else if (vv <= 30000.)
		val = tostring(vv/1000. + 50.);
	else if (vv <= 70000.)
		val = tostring((((vv/1000.)-30)/5) + 80);
	else  
		val = "99";

	object->text(val);

	MagLog::debug() << "\tVisibility: " << vv << " = " << val << "\n";

	font.size(owner_->size_);
	object->font(font);
	//object->setJustification(MRIGHT);

	symbol.add(object);
}
	
void ObsPresentWeather::visit(std::set<string>& tokens)
{

	if (!owner_->present_ww_visible_) return;
	tokens.insert("present_weather");

}

static map<int, string> presentweather;
void ObsPresentWeather::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->present_ww_visible_) return;
	if ( presentweather.empty() ) {
		presentweather[100] = "ww_00";
		presentweather[101] = "ww_01";
		presentweather[102] = "ww_02";
		presentweather[103] = "ww_03";
		presentweather[104] = "ww_04";
		presentweather[110] = "ww_10";
		presentweather[120] = "ww_45";
		presentweather[121] = "ww_60";
		presentweather[122] = "ww_20";
		presentweather[123] = "ww_61";
		presentweather[130] = "ww_45"; // Fog
		presentweather[131] = "ww_41";
		presentweather[132] = "ww_42";
		presentweather[133] = "ww_44";
		presentweather[134] = "ww_46";
		presentweather[140] = "ww_60";
		presentweather[141] = "ww_61";
		presentweather[150] = "ww_51";
		presentweather[151] = "ww_51";
		presentweather[152] = "ww_52";
		presentweather[153] = "ww_55";
		presentweather[157] = "ww_58";
		presentweather[158] = "ww_59";
		presentweather[160] = "ww_60";
		presentweather[161] = "ww_61";
		presentweather[162] = "ww_62";
		presentweather[163] = "ww_65";
		presentweather[180] = "ww_80";
		presentweather[181] = "ww_80";
		presentweather[182] = "ww_81";
		presentweather[183] = "ww_81";
		presentweather[189] = "ww_89";

	}
	CustomisedPoint::const_iterator value = point.find("present_weather");
	if ( value == point.end() ) return; 
	if ( value->second < 4 )
		return;
	if ( value->second > 500 )
		return;
	string ww;

	if ( value->second < 100 ) {
		ostringstream os;
		os << "ww_" << setw(2) << setfill('0') << value->second;
		ww = os.str();
	}
	else {
		map<int, string>::iterator w = presentweather.find(value->second);
		if ( w == presentweather.end() ) {
			MagLog::warning() << "Present Weather " << value->second << " is not recognised yet, pease conatct the magics team " << endl;
		}
		else
			ww = w->second;
	}

	if ( ww.empty() )
		return;
	SymbolItem*  object = new SymbolItem();
	object->x(column_);
	object->y(row_);
	
	Colour colour =  owner_->present_ww_colour_->automatic() ? *owner_->colour_ : *owner_->present_ww_colour_;
	object->colour(colour);
	

	object->symbol(ww);
	MagLog::debug() << "\tPresent Weather--->" << ww << " in " << colour << "\n";
	//time->setJustification(MRIGHT);
	object->height(owner_->size_);
	symbol.add(object);
}


void ObsTemperature::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->temperature_visible_) return;
	CustomisedPoint::const_iterator value = point.find("temperature");
	if ( value == point.end() )  return;

	Colour colour =  owner_->temperature_colour_->automatic() ? *owner_->colour_ : *owner_->temperature_colour_;

	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	font.colour(colour);
	font.size(owner_->size_);
	object->font(font);
	object->x(column_);
	object->y(row_);
	
	// The temperature is displayed in Celsius.		
	double tempe = maground(value->second-273.15);

	MagLog::debug() << "\tTemperature: " << tempe << " from "<<value->second<<"\n";

	object->text(tostring(tempe));
	//object->setJustification(MCENTRE);

	symbol.add(object);
}

//////////////////////////////////////////////////////////////////////////////////

void ObsTimePlot::visit(std::set<string>&) 
{

	if (!owner_->time_plot_visible_) return;
}
	
void ObsTimePlot::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{ 
	if (!owner_->time_plot_visible_) return;
	CustomisedPoint::const_iterator value = point.find("time");
	if ( value == point.end() ) return;

	MagLog::debug() << "\tTimePlot: " << value->second << "at[" << column_ << ", " << row_ << "]" << endl;


	Colour colour =  owner_->time_plot_colour_->automatic() ? *owner_->colour_ : *owner_->time_plot_colour_;

	TextItem*  time = new TextItem();
	MagFont font("sansserif");
	font.colour(colour);
	font.size(owner_->size_);
	time->x(column_);
	time->y(row_);
	time->font(font);
	time->text(tostring(value->second));
	//time->setJustification(MRIGHT);
	
	symbol.add(time);
} 

/////////////////////////////////////////////////////////////////////////////////

void ObsHeight::visit(std::set<string>& tokens ) 
{

	if (!owner_->height_visible_) return;
	tokens.insert("geopotential");
}
	
void ObsHeight::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{ 
	if (!owner_->height_visible_) return;
	CustomisedPoint::const_iterator value = point.find("geopotential");
	if ( value == point.end() ) return;
	double geop = maground(value->second/98.1);

	MagLog::debug() << "\tGeopotential: " << geop << "at[" << column_ << ", " << row_ << "]" << endl;
	Colour colour =  owner_->height_colour_->automatic() ? *owner_->colour_ : *owner_->height_colour_;
	TextItem*  height = new TextItem();
	MagFont font("sansserif");
	font.colour(colour);
	font.size(owner_->size_);
	height->x(column_);
	height->y(row_);

	height->text(tostring(geop));
	//height->setJustification(MLEFT);
	height->font(font);
	symbol.add(height);
} 

///////////////////////////////////////////////////////////////////////////////

void ObsThickness::visit(std::set<string>& tokens ) 
{

	if (!owner_->thickness_visible_) return;
	tokens.insert("thickness");
}


void ObsThickness::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{ 
	if (!owner_->thickness_visible_) return;
	CustomisedPoint::const_iterator value = point.find("thickness");
	if ( value == point.end() ) return;
	const double thickness = maground(value->second/98.1);
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tThickness: " << thickness << "at[" << column_ << ", " << row_ << "]" << endl;
#endif
	Colour colour =  owner_->thickness_colour_->automatic() ? *owner_->colour_ : *owner_->thickness_colour_;
	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	font.colour(colour);
	font.size(owner_->size_);
	object->x(column_);
	object->y(row_);
	
	object->text(tostring(thickness));
	//object->setJustification(MLEFT);
	object->font(font);
	symbol.add(object);
} 


////////////////////////////////////////////////////////////////////////////////////////

void ObsIdentifier::visit(std::set<string>&) 
{

	if (!owner_->identifier_visible_) return;
}
	
void ObsIdentifier::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{ 
	if (!owner_->identifier_visible_) return;
	TextItem*  time = new TextItem();
#ifdef OBS_DEBUG_
	MagLog::debug() << "Identification for " << point.identifier() << "at[" << column_ << ", " << row_ << "]" << endl;
#endif
	Colour colour =  owner_->identifier_colour_->automatic() ? *owner_->colour_ : *owner_->identifier_colour_;
	MagFont font("sansserif");
	font.colour(colour);
	font.size(owner_->size_);
	time->x(column_);
	time->y(row_);
	time->text(point.identifier());
	//time->setJustification(MRIGHT);
	time->font(font);
	
	symbol.add(time);
} 

///////////////////////////////////////////////////////////////////////////////////////

void ObsPastWeather::visit(std::set<string>& tokens) 
{

	if (!owner_->past_ww_visible_) return;
	tokens.insert("past_weather_1");
	tokens.insert("past_weather_2");
}
/*
WMO table 020004: PAST WEATHER (1)

code  meaning
----  -------
   0  CLOUD COVERING 1/2 OR LESS OF THE SKY THROUGHOUT THE APPROPRIATE PERIOD
   1  CLOUD COVERING MORE THAN 1/2 OF THE SKY DURING PART OF THE APPROPRIATE
      PERIOD AND COVERING 1/2 OR LESS DURING PART OF THE PERIOD
   2  CLOUD COVERING MORE THAN 1/2 OF THE SKY THROUGHOUT THE APPROPRIATE PERIOD
   3  SANSTORM, DUSTSTORM OR BLOWING SNOW
   4  FOG OR ICE OR THICK HAZE
   5  DRIZZLE
   6  RAIN
   7  SNOW, OR RAIN AND SNOW MIXED
   8  SHOWER(S)
   9  THUNDERSTORM(S) WITH OR WITHOUT PRECIPITATION
  10  NO SIGNIFICANT WEATHER OBSERVED
  11  VISIBILITY REDUCED
  12  BLOWING PHENOMENA, VISIBILITY REDUCED
  13  FOG
  14  PRECIPITATION
  15  DRIZZLE
  16  RAIN
  17  SNOW OR ICE PELLETS
  18  SHOWERS OR INTERMITTENT PRECIPITATION
  19  THUNDERSTORM
  31  MISSING VALUE
*/
static map<int, string> pastweather;
void ObsPastWeather::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{ 
	if ( pastweather.empty() ) {
		pastweather[3] = "W_3";
		pastweather[4] = "W_4";
		pastweather[5] = "W_5";
		pastweather[6] = "W_6";
		pastweather[7] = "W_6";
		pastweather[8] = "W_8";
		pastweather[9] = "W_9";
		pastweather[11] = "W_4";
		pastweather[12] = "W_4";
		pastweather[13] = "W_4";
		pastweather[14] = "W_6";
		pastweather[15] = "W_5";
		pastweather[17] = "W_7";
		pastweather[18] = "W_8";
		pastweather[19] = "W_9";
	}

	if (!owner_->past_ww_visible_) return;
	CustomisedPoint::const_iterator value = point.find("past_weather_1");
	if ( value == point.end() ) return;

	Colour colour =  owner_->past_ww_colour_->automatic() ? *owner_->colour_ : *owner_->past_ww_colour_;

	map<int, string>::iterator ww =  pastweather.find(value->second);

	if( ww != pastweather.end() )
	{
		SymbolItem*  object = new SymbolItem();
		object->x(column_);
		object->y(row_);
		object->colour(colour);

		object->symbol(ww->second);

		MagLog::debug() << "\tPast Weather 1-> " << ww->second << "\n";

		object->height(owner_->size_);
		symbol.add(object);
	}
		//second past weather
	value = point.find("past_weather_2");
	if ( value == point.end() ) return;
	ww =  pastweather.find(value->second);
	if( ww != pastweather.end())
		{
			SymbolItem* object2 = new SymbolItem();
			object2->x(column_*2);
			object2->y(row_);
			object2->colour(colour);

			object2->symbol(ww->second);

			MagLog::debug() << "\tPast Weather 2-> " <<  ww->second << "\n";

			object2->height(owner_->size_);
			symbol.add(object2);
		}

} 

///////////////////////////////////////////////////////////////////////////////////

void ObsCloud::visit(std::set<string>& tokens) 
{


	if (!owner_->cloud_visible_) return;
	if ( owner_->low_ )
	{
		tokens.insert("low_cloud");
		tokens.insert("low_cloud_nebulosity");
		tokens.insert("low_cloud_height");
	}
	if ( owner_->medium_ ) tokens.insert("medium_cloud");
	if ( owner_->high_ ) tokens.insert("high_cloud");

}


static map<int, string> clouds_;

void ObsCloud::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{ 
	if ( clouds_.empty() ) {
		clouds_[1] = "CH_1"; // CIRROCUMULUS (CC)
		clouds_[2] = "CH_2"; // 2 2 CIRROSTRATUS (CS)
		clouds_[3] = "CH_3"; // 3 3 ALTOCUMULUS (AC)
		clouds_[4] = "CH_4"; // 4 4 ALTOSTRATUS (AS)
		clouds_[5] = "CH_5"; // 5 5 NIMBOSTRATUS (NS)
		clouds_[6] = "CH_6"; // 6 6 STRATOCUMULUS (SC)
		clouds_[7] = "CH_7"; // 7 7 STRATUS (ST)
		clouds_[8] = "CH_8"; // 8 8 CUMULUS (CU)
		clouds_[9] = "CH_0"; // 9 9 CUMULONIMBUS (CB)
		//10 10 NO CH CLOUDS
		clouds_[11] = "CH_1"; // 11 11 CIRRUS FIBRATUS, SOMETIMES UNCINUS, NOT PROGRESSIVELY INVADING THE SKY
		clouds_[12] = "CH_2"; //12 12 CIRRUS SPISSATUS, IN PATCHES OR ENTANGLED SHEAVES, WHICH USUALLY DO NOT INCREASE AND SOMETIMES SEEM TO BE THE REMAINS OF THE UPPER PART OF A CUMULONIMBUS; OR CIRRUS CASTELLANUS OR FLOCCUS
		clouds_[13] = "CH_3"; //13 13 CIRRUS SPISSATUS CUMULONIMBOGENITUS
		clouds_[14] = "CH_4"; // 14 14 CIRRUS UNCINUS OR FIBRATUS, OR BOTH, PROGRESSIVELY INVADING THE SKY; THEY GENERALLY THICKEN AS A WHOLE
		clouds_[15] = "CH_5"; //15 15 CIRRUS (OFTEN IN BANDS) AND CIRROSTRATUS, OR CIRROSTRATUS ALONE, PROGRESSIVELY INVADING THE SKY; THEY GENERALLY THICKEN AS A WHOLE, BUT THE CONTINUOUS VEIL DOES NOT REACH 45 DEGREES ABOVE THE HORIZON
		clouds_[16] = "CH_6"; //16 16 CIRRUS (OFTEN IN BANDS) AND CIRROSTRATUS, OR CIRROSTRATUS ALONE, PROGRESSIVELY INVADING THE SKY; THEY GENERALLY THICKEN AS A WHOLE; THE CONTINUOUS VEIL EXTENDS MORE THAN 45 DEGREES ABOVE THE HORIZON, WITHOUT THE SKY BEING TOTALLY COVERED
		clouds_[17] = "CH_7"; //17 17 CIRROSTRATUS COVERING THE WHOLE SKY
		clouds_[18] = "CH_8"; //18 18 CIRROSTRATUS NOT PROGRESSIVELY INVADING THE SKY AND NOT ENTIRELY COVERING IT
		clouds_[19] = "CH_9"; //19 19 CIRROCUMULUS ALONE, OR CIRROCUMULUS PREDOMINANT AMONG THE CH CLOUDS
		//20 20 NO CM CLOUDS
		clouds_[21] = "CM_1"; //21 21 ALTOSTRATUS TRANSLUCIDUS
		clouds_[22] = "CM_2"; //22 22 ALTOSTRATUS OPACUS OR NIMBOSTRATUS
		clouds_[23] = "CM_3"; //23 23 ALTOCUMULUS TRANSLUCIDUS AT A SINGLE LEVEL
		clouds_[24] = "CM_4"; //24 24 PATCHES (OFTEN LENTICULAR) OF ALTOCUMULUS TRANSLUCIDUS, CONTINUALLY CHANGING AND OCCURRING AT ONE OR MORE LEVELS
		clouds_[25] = "CM_5"; //25 25 ALTOCUMULUS TRANSLUCIDUS IN BANDS, OR ONE OR MORE LAYERS OF ALTOCUMULUS TRANSLUCIDUS OR OPACUS, PROGRESSIVELY INVADING THE SKY; THESE ALTOCUMULUS CLOUDS GENERALLY THICKEN AS A WHOLE
		clouds_[26] = "CM_6"; //26 26 ALTOCUMULUS CUMULOGENITUS (OR CUMULONIMBOGENITUS)
		clouds_[27] = "CM_7"; //27 27 ALTOCUMULUS TRANSLUCIDUS OR OPACUS IN TWO OR MORE LAYERS, OR ALTOCUMULUS OPACUS IN A SINGLE LAYER, NOT PROGRESSIVELY INVADING THE SKY, OR ALTOCUMULUS WITH ALTOSTRATUS OR NIMBOSTRATUS
		clouds_[28] = "CM_8"; //		28 28 ALTOCUMULUS CASTELLANUS OR FLOCCUS
		clouds_[29] = "CM_9"; //29 29 ALTOCUMULUS OF A CHAOTIC SKY, GENERALLY AT SEVERAL LEVELS
		//30 30 NO CL CLOUDS
		clouds_[31] = "CL_1"; //31 31 CUMULUS HUMILIS OR CUMULUS FRACTUS OTHER THAN OF BAD WEATHER,* OR BOTH
		clouds_[32] = "CL_2"; //32 32 CUMULUS MEDIOCRIS OR CONGESTUS, TOWERING CUMULUS (TCU), WITH OR WITHOUT CUMULUS OF SPECIES FRACTUS OR HUMILIS OR STRATOCUMULUS, ALL HAVING THEIR BASES AT THE SAME LEVEL
		clouds_[33] = "CL_3"; //33 33 CUMULONIMBUS CALVUS, WITH OR WITHOUT CUMULUS, STRATOCUMULUS OR STRATUS
		clouds_[34] = "CL_4"; //34 34 STRATOCUMULUS CUMULOGENITUS
		clouds_[35] = "CL_5"; //35 35 STRATOCUMULUS OTHER THAN STRATOCUMULUS CUMULOGENITUS
		clouds_[36] = "CL_6"; //36 36 STRATUS NEBULOSUS OR STRATUS FRACTUS OTHER THAN OF BAD WEATHER,* OR BOTH
		clouds_[37] = "CL_7"; //37 37 STRATUS FRACTUS OR CUMULUS FRACTUS OF BAD WEATHER,* OR BOTH (PANNUS), USUALLY BELOW ALTOSTRATUS OR NIMBOSTRATUS
		clouds_[38] = "CL_8"; //38 38 CUMULUS AND STRATOCUMULUS OTHER THAN STRATOCUMULUS CUMULOGENITUS, WITH BASES AT DIFFERENT LEVELS
		clouds_[39] = "CL_9"; //39 39 CUMULONIMBUS CAPILLATUS (OFTEN WITH AN ANVIL), WITH OR WITHOUT CUMULONIMBUS CALVUS, CUMULUS, STRATOCUMULUS, STRATUS OR PANNUS
		//0 40 CH
		//41 41 CM
		//42 42 CL
		//59 59 CLOUD NOT VISIBLE OWING TO DARKNESS, FOG, DUSTSTORM, SANDSTORM, OR OTHER ANALOGOUS PHENOMENA
		//60 60 CH CLOUDS INVISIBLE OWING TO DARKNESS, FOG, BLOWING DUST OR SAND, OR OTHER SIMILAR PHENOMENA, OR BECAUSE OF A CONTINUOUS LAYER OF LOWER CLOUDS
		//61 61 CM CLOUDS INVISIBLE OWING TO DARKNESS, FOG, BLOWING DUST OR SAND, OR OTHER SIMILAR PHENOMENA, OR BECAUSE OF CONTINUOUS LAYER OF LOWER CLOUDS
		//62 62 CL CLOUDS INVISIBLE OWING TO DARKNESS, FOG, BLOWING DUST OR SAND, OR OTHER SIMILAR PHENOMENA
	}

	if (!owner_->cloud_visible_) return;
	symbol.setHeight(owner_->size_);


	CustomisedPoint::const_iterator height = point.find("low_cloud_height");
	CustomisedPoint::const_iterator nebul = point.find("low_cloud_nebulosity");
	ostringstream nh;
	string sep = "";

	if ( nebul != point.end() )
	{
		nh << nebul->second;
		sep = "/";
	}
	if ( height != point.end() )
	{
		double x = height->second;
		if (x<50) x = 0; 
		else if (x<100) x = 1; 
		else if (x<200) x = 2; 
		else if (x<300) x = 3; 
		else if (x<600) x = 4; 
		else if (x<1000) x = 5; 
		else if (x<1500) x = 6; 
		else if (x<2000) x = 7; 
		else if (x<2500) x = 8; 
		else  x = 9; 
		nh << sep << x;
	}

	if (!nh.str().empty())
	{
		TextItem*  object = new TextItem();
		object->x(lowColumn_);
		object->y(lowRow_-1);
		MagFont font;
		font.name("sansserif");
		font.colour(*owner_->low_colour_);
		font.size(owner_->size_* 0.8);

		MagLog::debug() << "\tLow Cloud--->" << nh.str() << "\n";
		object->text(nh.str());
		object->font(font);
		symbol.add(object);
	}

	CustomisedPoint::const_iterator value = point.find("low_cloud");
	if ( value != point.end() ) {
		map<int, string>::iterator low = clouds_.find(value->second);
		if ( low != clouds_.end() )
		{
			SymbolItem*  cloud = new SymbolItem();
			cloud->x(lowColumn_);
			cloud->y(lowRow_);
			cloud->colour(*owner_->low_colour_);

			cloud->symbol(low->second);

			MagLog::debug() << "\tLow Cloud--->" << value->second << "-->" << low->second << "\n";

			cloud->height(owner_->size_);
			symbol.add(cloud);
		}
	}

	value = point.find("medium_cloud");
	if ( value != point.end() ) {
		map<int, string>::iterator medium = clouds_.find(value->second);

		if ( medium != clouds_.end() )
		{
			SymbolItem*  cloud = new SymbolItem();
			cloud->x(mediumColumn_);
			cloud->y(mediumRow_);
			cloud->colour(*owner_->medium_colour_);
			cloud->symbol(medium->second);

			MagLog::debug() << "\tMedium Cloud--->" << value->second<< " --> "<< medium->second  << "\n";

			cloud->height(owner_->size_);
			symbol.add(cloud);
		}
	}
	
	value = point.find("high_cloud");
	if ( value != point.end() ) {
		map<int, string>::iterator high = clouds_.find(value->second);

		if ( high != clouds_.end() )
		{
			SymbolItem*  cloud = new SymbolItem();
			cloud->x(highColumn_);
			cloud->y(highRow_);
			cloud->colour(*owner_->high_colour_);
			cloud->symbol(high->second);

			MagLog::debug() << "\tHigh Cloud--->"  <<value->second << "-->" << high->second << "\n";

			cloud->height(owner_->size_);
			symbol.add(cloud);
		}
	}
}

void ObsCloud::set(const map<string, string>& def)
{   
	lowRow_ = atoi(find(def, "low_row").c_str());
	lowColumn_ = atoi(find(def, "low_column").c_str());

	mediumRow_ = atoi(find(def, "medium_row").c_str());
	mediumColumn_ = atoi(find(def, "medium_column").c_str());

	highRow_ = atoi(find(def, "high_row").c_str());
	highColumn_ = atoi(find(def, "high_column").c_str());

	colour_ = find(def, "colour");
}

void ObsDemoItem1::visit(std::set<string>& tokens)
{
	tokens.insert("temperature");
}

void ObsDemoItem1::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if ( point.find("temperature") == point.end() ) {
			MagLog::debug() << "No info for temperature given!" << endl;
			return;
		}

		TextItem*  object = new TextItem();
		object->x(column_);
		object->y(row_);

		object->text(tostring(point["temperature"]/3));
		symbol.add(object);
}

void ObsDemoItem2::visit(std::set<string>& tokens)
{
	tokens.insert("info_for_demo2");
}

void ObsDemoItem2::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	string text;
	if ( point.find("info_for_demo2") == point.end() )
			text = "todo";

	else
		text=tostring(point["info_for_demo2"]);

	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);
	MagFont font;
	font.name("sansserif");
	font.colour(*owner_->colour_);
	font.size(owner_->size_);
	//object->colour(attributes_text_colour_);
	object->text(text);
	object->font(font);
	symbol.add(object);
}



void  ObsEra::visit(std::set<string>& tokens)
{
	tokens.insert(key_);
}

void  ObsEra::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	CustomisedPoint::iterator pt = point.find(key_);
	if (pt == point.end())
		return;
	string text = tostring(point[key_]);
	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);
	MagFont font;
	font.name("sansserif");
	font.colour(colour_);
	font.size(owner_->size_);

	object->text(text);
	object->font(font);
	symbol.add(object);

}

void  ObsSeaTemperature::visit(std::set<string>& tokens)
{
	if (!owner_->sea_temperature_visible_) return;
	tokens.insert("sea_temperature");
}

void  ObsSeaTemperature::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->sea_temperature_visible_) return;
	CustomisedPoint::const_iterator value = point.find("sea_temperature");
	if ( value == point.end() )  return;
	Colour colour =  owner_->sea_temperature_colour_->automatic() ? *owner_->colour_ : *owner_->sea_temperature_colour_;
		TextItem*  object = new TextItem();
		MagFont font("sansserif");
		font.colour(colour);
		font.size(owner_->size_);
		object->font(font);
		object->x(column_);
		object->y(row_);

		// The temperature is displayed in Celsius.
		double tempe = maground(value->second-273.15);

		MagLog::debug() << "\tTemperature: " << tempe << " from "<<value->second<<"\n";

		object->text(tostring(tempe));


		symbol.add(object);

}
void  ObsWave::visit(std::set<string>& tokens)
{
	if (!owner_->waves_visible_) return;
	tokens.insert("wind_wave_direction");
	tokens.insert("wind_wave_period");
	tokens.insert("wind_wave_height");

	tokens.insert("swell_direction");
	tokens.insert("swell_period");
	tokens.insert("swell_height");

	tokens.insert("wave_direction");
	tokens.insert("wave_period");
	tokens.insert("wave_height");


}

void  ObsWave::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!owner_->waves_visible_) return;

	CustomisedPoint::const_iterator period = point.find("wave_period");
	CustomisedPoint::const_iterator height = point.find("wave_height");

	Colour colour =  owner_->waves_colour_->automatic() ? *owner_->colour_ : *owner_->waves_colour_;

	if ( period == point.end() ||  height == point.end() )  return;

	MagLog::debug() << "\theight: " << height->second << " period "<< period->second <<"\n";
	// height of waves in units of 0.5
	double h = maground(height->second/0.5);
	double p = maground(period->second);
	ostringstream oss;

	oss << setw(2) << setfill('0') <<  p  << setw(2) << setfill('0') << h;

	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	font.colour(colour);
	font.size(owner_->size_);
	object->font(font);
	object->x(column_);
	object->y(row_);

	object->text(oss.str());

	symbol.add(object);
}
