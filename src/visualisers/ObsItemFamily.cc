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
	if (!attributes_ ) attributes_ = new ObsWindAttributes();
	if ( !colour_.empty() )  attributes_->colour_ = auto_ptr<Colour>(new Colour(colour_));
	if (!attributes_->visible_) return;
	tokens.insert(speed_);
	tokens.insert(direction_);

}
void ObsWind::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;

	map<string, double>::iterator it = point.begin();
	map<string, double>::iterator en = point.end();
	for(;it != en; ++it)
	{
	 MagLog::debug() << " >>> "<<it->first<<" -> " << it->second << endl;
	}

	CustomisedPoint::const_iterator speed = point.find(speed_);
	if ( speed == point.end() ) return; 
	CustomisedPoint::const_iterator direction = point.find(direction_);
	if ( direction == point.end() ) return; 

	FlagItem* flag = new FlagItem();
	flag->setColour(*attributes_->colour_);
	flag->length(size_ * 1.5); // Size to be adjusted later!

	const string origin = "circle";


MagLog::debug() << "OBS ITEM - ObsWind - Lon/Lat: "<<point.longitude()<<" / "<<point.latitude()
     << "\n\twind_speed:     " << point[speed_]
     << "\n\twind_direction: " << point[direction_]
     << "\n\tcloud amount:   " << point["cloud_amount"]<<" -> "<<origin <<endl;


	flag->setOriginHeight(0.01);
	flag->setOriginMarker(origin);
	flag->x(0);
	flag->y(0);
	
	const Transformation& transformation = symbol.parent().transformation();
	PaperPoint pp(point.longitude(), point.latitude());
	pair<double, double> wind = make_pair(speed->second, direction->second);
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
}

void ObsCloudAndWind::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;

	map<string, double>::iterator it = point.begin();
	map<string, double>::iterator en = point.end();
	for(;it != en; ++it)
	{
	 MagLog::debug() << " >>> "<<it->first<<" -> " << it->second << endl;
	}

	CustomisedPoint::const_iterator speed = point.find("wind_speed");
	if ( speed == point.end() ) return; 
	CustomisedPoint::const_iterator direction = point.find("wind_direction");
	if ( direction == point.end() ) return; 

	FlagItem* flag = new FlagItem();
	flag->setColour(*attributes_->colour_);
	flag->length(size_ * 1.5); // Size to be adjusted later!

	map<int, string>::const_iterator marker = origins_.find(int(point["cloud_amount"]));
	const string origin = ( marker != origins_.end()) ? marker->second : "magics_13";


MagLog::debug() << "OBS ITEM - ObsWind - Lon/Lat: "<<point.longitude()<<" / "<<point.latitude()
     << "\n\twind_speed:     " << point["wind_speed"] 
     << "\n\twind_direction: " << point["wind_direction"]
     << "\n\tcloud amount:   " << point["cloud_amount"]<<" -> "<<origin <<endl;

	flag->setOriginHeight(size_);
	flag->setOriginMarker(origin);
	flag->x(0);
	flag->y(0);
	
	const Transformation& transformation = symbol.parent().transformation();
	PaperPoint pp(point.longitude(), point.latitude());
	pair<double, double> wind = make_pair(speed->second, direction->second);
	transformation.reprojectSpeedDirection(pp, wind);
	
	flag->speed(speed->second);
	flag->direction(direction->second);

	if (point.latitude() <0 ) 
		flag->setHemisphere(SOUTH);
	symbol.add(flag);
}

void ObsPressure::visit(std::set<string>& tokens)
{
	if ( !attributes_ ) attributes_ = new ObsPressureAttributes();
	if (!attributes_->visible_) return;
	if ( !colour_.empty() )  attributes_->colour_ = auto_ptr<Colour>(new Colour(colour_));
	tokens.insert("msl_pressure");
}

void ObsPressure::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("msl_pressure");
	if ( value == point.end() ) return;
	
	TextItem*  object = new TextItem();
	// for the pressure we show the 3 last digits of the pressure in decaPascal.
	object->x(column_);
	object->y(row_);
	MagFont font;
	font.colour(*attributes_->colour_);
	font.name("sansserif");
	font.size(size_);
	
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
	if ( !attributes_ ) attributes_ = new ObsPressureLevelAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("pressure");
}

void ObsPressureLevel::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("pressure");
	if ( value == point.end() ) return;
	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);
	
	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	
	const double pressure = value->second/100.;

	object->text(tostring(pressure));

	MagLog::debug() << "\tPressureLevel: " << value->second << " = " << pressure << "\n";
	font.size(size_);
	object->font(font);
	symbol.add(object);
}
	
void ObsPressureTendency::visit(std::set<string>& tokens)
{
	if ( !attributes_ ) attributes_ = new ObsPressureTendencyAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("pressure_tendency_amount");
	tokens.insert("pressure_tendency_characteristic");
}
	
void ObsPressureTendency::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("pressure_tendency_amount");
	if ( value == point.end() ) return; 
	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);
	
	ostringstream os;	
	if(value->second>0) os <<  setw(2) << setfill('0') << value->second * .1;
	else os << "-" << setw(2) << setfill('0') << value->second * -.1;
	
	// The Pressure tendancy is red if negative!
	MagFont font("sansserif");
	font.colour(( value->second < 0 ) ? Colour("red") : *attributes_->colour_);

	object->text(os.str());
	font.size(size_);
	object->font(font);
	symbol.add(object);
	value = point.find("pressure_tendency_characteristic");
	if ( value == point.end() ) return; 
	SymbolItem*  tendancy = new SymbolItem();
	tendancy->x(column_+1);
	tendancy->y(row_);	
	
	tendancy->colour(*attributes_->colour_);
	ostringstream oss;	
	oss << "a_"  << value->second;
	tendancy->symbol(oss.str());
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tPressure tendency--->" << oss.str() << "\n";
#endif
	tendancy->height(size_);
	symbol.add(tendancy);
}

	
void ObsDewPoint::visit(std::set<string>& tokens)
{
	if ( !attributes_ ) attributes_ = new ObsDewPointAttributes();
	if (!attributes_->visible_) return;
	if ( !colour_.empty() )  attributes_->colour_ = auto_ptr<Colour>(new Colour(colour_));
	tokens.insert("dewpoint");
}

void ObsDewPoint::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("dewpoint");
	if ( value == point.end() ) return; 
	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	
	font.colour(*attributes_->colour_);
	font.size(size_);
	object->x(column_);
	object->y(row_);
	
	// The temperature is displayed in Celsius.
	const double tempe = maground(value->second-273.25);
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tDewPoint--->" << point["dewpoint_2meters"] << " = " << tempe << "\n";
#endif
	object->text(tostring(tempe));
	object->font(font);
	//object->setJustification(MCENTRE);	
	
	symbol.add(object);
}

	
void ObsVisibility::visit(std::set<string>& tokens)
{
	if ( !attributes_ ) attributes_ = new ObsVisibilityAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("horizontal_visibility");
}
/*! 
   Documnetaion found at :	
     http://www.zetnet.co.uk/sigs/weather/Met_Codes/vvcode.htm 
*/
void ObsVisibility::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("horizontal_visibility");
	if ( value == point.end() ) return;

	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);

	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	
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

	font.size(size_);
	object->font(font);
	//object->setJustification(MRIGHT);

	symbol.add(object);
}
	
void ObsPresentWeather::visit(std::set<string>& tokens)
{
	if ( !attributes_ ) attributes_ = new ObsPresentWeatherAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("present_weather");
	if ( !colour_.empty() )  attributes_->colour_ = auto_ptr<Colour>(new Colour(colour_));
}

void ObsPresentWeather::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("present_weather");
	if ( value == point.end() ) return; 
	SymbolItem*  object = new SymbolItem();
	object->x(column_);
	object->y(row_);
	
	object->colour(*attributes_->colour_);
	//object->setFont("sansserif");
	ostringstream os;
	
	os << "ww_" << setw(2) << setfill('0') << value->second;

	object->symbol(os.str());
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tPresent Weather--->" << os.str() << "\n";
#endif
	//time->setJustification(MRIGHT);
	object->height(size_);
	symbol.add(object);
}


void ObsTemperature::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const
{
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("temperature");
	if ( value == point.end() )  return;

	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	font.size(size_);
	object->font(font);
	object->x(column_);
	object->y(row_);
	
	// The temperature is displayed in Celsius.		
	double tempe = maground(value->second-273.25);
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tTemperature: " << tempe << " from "<<value->second<<"\n";
#endif
	object->text(tostring(tempe));
	//object->setJustification(MCENTRE);

	symbol.add(object);
}

//////////////////////////////////////////////////////////////////////////////////

void ObsTimePlot::visit(std::set<string>&) 
{
	if ( !attributes_ ) attributes_ = new ObsTimePlotAttributes();
	if (!attributes_->visible_) return;
}
	
void ObsTimePlot::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const 
{ 
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("time");
	if ( value == point.end() ) return;
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tTimePlot: " << value->second << "at[" << column_ << ", " << row_ << "]" << endl;
#endif
	TextItem*  time = new TextItem();
	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	font.size(size_);
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
	if ( !attributes_ ) attributes_ = new ObsHeightAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("geopotential");
}
	
void ObsHeight::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const 
{ 
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("geopotential");
	if ( value == point.end() ) return;
	double geop = maground(value->second/98.1);
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tGeopotential: " << geop << "at[" << column_ << ", " << row_ << "]" << endl;
#endif
	TextItem*  height = new TextItem();
	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	font.size(size_);
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
	if ( !attributes_ ) attributes_ = new ObsThicknessAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("thickness");
}


void ObsThickness::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const 
{ 
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("thickness");
	if ( value == point.end() ) return;
	const double thickness = maground(value->second/98.1);
#ifdef OBS_DEBUG_
	MagLog::debug() << "\tThickness: " << thickness << "at[" << column_ << ", " << row_ << "]" << endl;
#endif
	TextItem*  object = new TextItem();
	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	font.size(size_);
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
	if ( !attributes_ ) attributes_ = new ObsIdentifierAttributes();
	MagLog::debug() << " ObsIdentifier" << *attributes_ << endl;
	if (!attributes_->visible_) return;
}
	
void ObsIdentifier::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const 
{ 
	if (!attributes_->visible_) return;
	TextItem*  time = new TextItem();
#ifdef OBS_DEBUG_
	MagLog::debug() << "Identification for " << point.identifier() << "at[" << column_ << ", " << row_ << "]" << endl;
#endif
	MagFont font("sansserif");
	font.colour(*attributes_->colour_);
	font.size(size_);
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
	if ( !attributes_ ) attributes_ = new ObsPastWeatherAttributes();
	if (!attributes_->visible_) return;
	tokens.insert("past_weather_1");
	tokens.insert("past_weather_2");
}

void ObsPastWeather::operator()(CustomisedPoint& point,  ComplexSymbol& symbol) const 
{ 
	if (!attributes_->visible_) return;
	CustomisedPoint::const_iterator value = point.find("past_weather_1");
	if ( value == point.end() ) return;
	if(value->second > 2)
	{
		SymbolItem*  object = new SymbolItem();
		object->x(column_);
		object->y(row_);
		object->colour(*attributes_->colour_);
		ostringstream os;	
		os << "W_"  << value->second;	
		object->symbol(os.str());
#ifdef OBS_DEBUG_
		MagLog::debug() << "\tPast Weather 1-> " << os.str() << "\n";
#endif
		object->height(size_);
		symbol.add(object);

		//second past weather
		value = point.find("past_weather_2");
		if ( value == point.end() ) return;
		if(value->second > 2)
		{
			SymbolItem* object2 = new SymbolItem();
			object2->x(column_*2);
			object2->y(row_);
			object2->colour(*attributes_->colour_);
			ostringstream os2;	
			os2 << "W_"  << value->second;	
			object2->symbol(os2.str());
#ifdef OBS_DEBUG_
			MagLog::debug() << "\tPast Weather 2-> " << os2.str() << "\n";
#endif
			object2->height(size_);
			symbol.add(object2);
		}
	}
} 

///////////////////////////////////////////////////////////////////////////////////

void ObsCloud::visit(std::set<string>& tokens) 
{

	if (!attributes_ ) attributes_ = new ObsCloudAttributes();
	if (!attributes_->visible_) return;
	if ( attributes_->low_ )
	{
		tokens.insert("low_cloud");
		tokens.insert("low_cloud_nebulosity");
		tokens.insert("low_cloud_height");
	}
	if ( attributes_->medium_ ) tokens.insert("medium_cloud");
	if ( attributes_->high_ ) tokens.insert("high_cloud");
	if ( !colour_.empty() )  {

		attributes_->medium_colour_ = auto_ptr<Colour>(new Colour(colour_));
		attributes_->low_colour_ = auto_ptr<Colour>(new Colour(colour_));
		attributes_->high_colour_ = auto_ptr<Colour>(new Colour(colour_));
	}
}
	
void ObsCloud::operator()(CustomisedPoint& point, ComplexSymbol& symbol) const 
{ 
	if (!attributes_->visible_) return;
	
	CustomisedPoint::const_iterator value = point.find("low_cloud");
	if ( value != point.end() )
	{
		SymbolItem*  cloud = new SymbolItem();
		cloud->x(lowColumn_);
		cloud->y(lowRow_);
		cloud->colour(*attributes_->low_colour_);
		ostringstream oss;	
		int type = int(value->second - (int (value->second/10) *10));
		oss << "CL_"  << type;
		cloud->symbol(oss.str());
#ifdef OBS_DEBUG_
		MagLog::debug() << "\tLow Cloud--->" << oss.str() << "\n";
#endif
		cloud->height(size_);
		symbol.add(cloud);
	}

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
		font.colour(*attributes_->low_colour_);
		font.size(size_*.6);
#ifdef OBS_DEBUG_
		MagLog::debug() << "\tLow Cloud Nebulosity--->" << value->second << "\n";
#endif
		object->text(nh.str());
		object->font(font);
		symbol.add(object);
	}

	value = point.find("medium_cloud");
	if ( value != point.end() )

	value = point.find("medium_cloud");
	if ( value != point.end() )
	{
		SymbolItem*  cloud = new SymbolItem();
		cloud->x(mediumColumn_);
		cloud->y(mediumRow_);
		
		cloud->colour(*attributes_->medium_colour_);
		ostringstream oss;
		int type = int(value->second - (int (value->second/10.) *10));
		oss << "CM_"  << type;
		cloud->symbol(oss.str());

		MagLog::debug() << "\tMedium Cloud--->" <<value->second<<" > "<< oss.str() << "\n";

		cloud->height(size_);
		symbol.add(cloud);
	}
	
	value = point.find("high_cloud");
	if ( value != point.end() )
	{
		SymbolItem*  cloud = new SymbolItem();
		cloud->x(highColumn_);
		cloud->y(highRow_);
		cloud->colour(*attributes_->high_colour_);
		ostringstream oss;	
		int type = int(value->second - (int (value->second/10) *10));
		oss << "CH_"  << type;
		cloud->symbol(oss.str());
#ifdef OBS_DEBUG_
		MagLog::debug() << "\tHigh Cloud--->" << oss.str() << "\n";
#endif
		cloud->height(size_);
		symbol.add(cloud);
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
			text = attributes_->text_;

	else
		text=tostring(point["info_for_demo2"]);

	TextItem*  object = new TextItem();
	object->x(column_);
	object->y(row_);
	MagFont font;
	font.name("sansserif");
	font.colour(attributes_->text_colour_);
	font.size(size_);
	//object->colour(attributes_text_colour_);
	object->text(text);
	object->font(font);
	symbol.add(object);
}

void ObsEra::set(const map<string, string>& def)
{
	key_ = find(def, "key");
	colour_ = find(def, "colour");
	row_ = atoi(find(def, "row").c_str());
	column_ = atoi(find(def, "column").c_str());
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
	font.size(size_);

	object->text(text);
	object->font(font);
	symbol.add(object);

}
