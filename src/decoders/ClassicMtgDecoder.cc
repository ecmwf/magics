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

/*! \file ClassicMtgDecoder.cc
    \brief Implementation of the Template class ClassicMtgDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/

#include "ClassicMtgDecoder.h"
#include "TextVisitor.h"
#include "MetaData.h"
#include "CustomisedPoint.h"
#include "DateTime.h"
#include "spot_database.h"
#include "Timer.h"

#include "Factory.h"
#include <locale>
#include <limits>

using namespace magics;

namespace magics
{

class MtgTempe: public MetgramParameter
{
public:
	MtgTempe() : MetgramParameter("temperature", "Temperature (&#176;C)", "167.128")  
	{
	}
	~MtgTempe() {}
	const string& title() const {
		return title_;
	}
	const string& xml() const {
		return title_;
	}	
	virtual double operator()(double value) const { 
		return value - 273.15;
	}	
	virtual spot_query_result* prepare(const ClassicMtgDecoder&, vector<CustomisedPoint*>&);

protected :
	
   
};

class MtgMSL: public MetgramParameter
{
public:
	MtgMSL() : MetgramParameter("msl", "MSL Pressure (hPa)", "151.128")  
	{
		
	}
	~MtgMSL() {}
	const string& title() const {
		return title_;		
	}
	
	const string& xml() const {		
		return title_;		
	}	
	
	virtual double operator()(double value) const { 
		return value/100;		
	}	
	
	virtual spot_query_result* prepare(const ClassicMtgDecoder&, vector<CustomisedPoint*>&);

protected :
	
   
};

class MtgHumidity: public MetgramParameter
{
public:
	MtgHumidity() : MetgramParameter("humidity", "850 hPa Relative Humidity (%)", "157.128")  
	{
		
	}
	~MtgHumidity() {}
	const string& title() const { return title_; }
	const string& xml() const   { return title_; }	
	virtual double operator()(double value) const { 
		if (value < 0.05 ) return 0;
		if ( value > 100 ) return 100;
		return value; 
	}	
	virtual spot_query_result* prepare(const ClassicMtgDecoder&, vector<CustomisedPoint*>&);

protected :
	
   
};

class MtgCloudCover: public MetgramParameter
{
public:
	MtgCloudCover() : MetgramParameter("cloud_cover", "Cloud Amount (%)", "164.128")  
	{
		
	}
	~MtgCloudCover() {}
	const string& title() const { return title_; }
	const string& xml() const   { return title_; }	
	virtual double operator()(double value) const { 
	
		value *= 100;
		if (value < 0.1 ) return 0;
		if ( value > 100 ) return 100;
		return value; 
	}	
	virtual spot_query_result* prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
	{
		spot_query* query = decoder.newQuery();
		
		spot_set_parameter(query,"param", "164.128"); 
		spot_query_result* result = spot_handle_query(query);
		interpretResult(result,out, "curve1");
		
		spot_delete_query(query);		
		return result; 
	}

protected :
	
   
};


class MtgPrecipitation: public MetgramParameter
{
public:
	MtgPrecipitation() : MetgramParameter("precipitation", "Precipitation (mm/3h to 144 hours then mm/6h)", "143.128")  
	{
		
	}
	~MtgPrecipitation() {}
	const string& title() const { return title_; }
	const string& xml() const   { return title_; }	
	virtual double operator()(double value) const { 
		value *= 1000;
		if (value < 0.05 ) return 0;
		return value; 
	}	
	virtual spot_query_result* prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
	{
		spot_query* query = decoder.newQuery();
		
		spot_set_parameter(query,"param", "143.128"); 
		spot_query_result* result = spot_handle_query(query);
		interpretResult(result,out, "curve1");
		
		spot_delete_query(query);		
		if (maxy_ < 3 ) maxy_ = 3;
		else if (maxy_ < 5 ) maxy_ = 5;
		else if (maxy_ < 10 ) maxy_ = 10;
		else if (maxy_ < 30 ) maxy_ = 30;
		else if (maxy_ < 50 ) maxy_ = 50;
		else if (maxy_ < 100 ) maxy_ = 100;
		return result; 
	}

protected :
	
   
};

class Mtg10mWind: public MetgramParameter
{
public:
	Mtg10mWind() : MetgramParameter("10m_wind", "10m Wind (kt) ", "143.128")  
	{
		
	}
	~Mtg10mWind() {}
	const string& title() const { return title_; }
	const string& xml() const   { return title_; }	
	virtual double operator()(double value) const { 
		return value; 
	}	
	virtual spot_query_result* prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
	{
		spot_query* query = decoder.newQuery();
		
		spot_set_parameter(query,"param", "165.128"); 
		spot_query_result* result = spot_handle_query(query);
		interpretResult(result,out, "curve1");
		
		spot_delete_query(query);		
		query = decoder.newQuery();
		spot_set_parameter(query,"param", "166.128"); 
		result = spot_handle_query(query);
	
		interpretResult(result, out, "curve2");
		spot_delete_query(query);
	
			return result; 
	}

protected :
	
   
};

SimpleObjectMaker<MtgTempe, MetgramParameter> mt("temperature");
SimpleObjectMaker<MtgMSL, MetgramParameter> msl("msl");
SimpleObjectMaker<MtgHumidity, MetgramParameter> humidity("humidity");
SimpleObjectMaker<MtgCloudCover, MetgramParameter> cloud_cover("cloud_cover");
SimpleObjectMaker<MtgPrecipitation, MetgramParameter> precipitation("precipitation");
SimpleObjectMaker<Mtg10mWind, MetgramParameter> tenmwind("10m_wind");


static string get_spot(spot_query_result* result, const char* param)
{
	char tmp[1024];
	size_t len = 1024;
	
	spot_get_parameter(result,param,tmp, &len); 
	
	return string(tmp, len-1);
}


void MetgramParameter::interpretResult(spot_query_result* result, vector<CustomisedPoint*>& out, const string& key)
{
	Timer timer("access spot","");
	size_t len = 1024;
	char tmp[1024];
	double missing = INT_MAX;
	
	string date = get_spot(result,"date");
	string time = get_spot(result,"time");
	map<string, int> columns;
	map<int, int> rows;
	
	vector<double> xpos;
	vector<double> ypos;
	
	
	// Get the columns names;
	for(int i = 0 ; i < spot_get_column_count(result); i++){
    	spot_get_column_name(result, i,tmp,&len);
    	columns[tmp] = i;
    	len = 1024;
	}
	
	double values[1024];
	size_t nb = 1024;
	// get the rows names
	for(int i = 0 ; i < spot_get_row_count(result); i++){
    	spot_get_row_name(result, i,tmp,&len);   	
    	int x = atoi(tmp);
    	rows[x] = i;
    	len = 1024;    	
	}	
	bool rain = false;
	
	base_ = DateTime(date, MagTime(time));

	DateTime next;

	for (map<int, int>::const_iterator step = rows.begin(); step != rows.end(); ++step) {
		
		
		
		
		next = base_ +  Second(step->first * 3600);    
		vector<CustomisedPoint*>::iterator p;
		for (p = out.begin(); p != out.end(); ++p) {
			if ( (**p)["year"] != next.date().year() ) continue;
			if ( (**p)["month"] !=  next.date().month() ) continue;
			if ( (**p)["day"] != next.date().day() ) continue;
			if ( (**p)["hours"] != next.time().hours()) continue;
			if ( (**p)["minutes"] != next.time().minutes()) continue;
			if ( (**p)["seconds"] != next.time().seconds()) continue;
			break;
		}
		
		CustomisedPoint* point;
		
		if (  p != out.end() ) {
			point = *p;
		}
		else {
			point = new CustomisedPoint();
			out.push_back(point);
			point->longitude(step->first);
			xpos.push_back(step->first);
	
	
		
			(*point)["year"]    = next.date().year();
			(*point)["month"]    = next.date().month();
			(*point)["day"]    = next.date().day();
			(*point)["hours"]   = next.time().hours();
			(*point)["minutes"] = next.time().minutes();
			(*point)["seconds"] = next.time().seconds();
			//(*point)["shift"] = (*parameter_)(step->first);
			(*point)["shift"] = 0;
		}	
		
		      	
		
		
		
		spot_get_row(result, step->second,values,&nb);
		bool ok = true;
		for (map<string, int>::const_iterator info = columns.begin(); info != columns.end(); ++info) {
			// dirty to recognize rain!
			if ( values[info->second] > missing ) {
				(*point)["as_rain"] = 1;
				values[info->second] = 0;
				rain = true;
			}
	    	if ( !isnan(values[info->second]) && values[info->second] < missing ) {
	    		(*point)[key] = (*this)(values[info->second]);	  
	    		ypos.push_back( (*this)(values[info->second]));	 
	    		if (rain) (*point)["as_rain"] = 1;
	    		MagLog::dev() << " SPOTBASE returns " << info->first << " for " <<(*this)(values[info->second]) << " at " << step->first <<  endl;
		
	    	}
	    	else {
	    		MagLog::warning() << " SPOTBASE returns nan for " << info->first << " : data ignored for step " << (*point)["shift"]<< endl;
	    		ok = false;
	    	}
	    	
	    	
		}
    	
		nb=1024;
	
	}
	
	
	if ( !xpos.empty() ) { 
		minx_ = std::min(minx_, *std::min_element(xpos.begin(), xpos.end()));
		maxx_ = std::max(maxx_, *std::max_element(xpos.begin(), xpos.end()));
	
		MagLog::debug() << "MIN=" << minx_ << "MAX=" << maxx_ << endl;
	}
    miny_ = std::min(miny_, *std::min_element(ypos.begin(), ypos.end()));
	maxy_ = std::max(maxy_, *std::max_element(ypos.begin(), ypos.end()));
	
	MagLog::debug() << "MIN=" << miny_ << "MAX=" << maxy_ << endl;
}


void MetgramParameter::setTransformation(Transformation& transformation)
{
	transformation.setDataMinMaxX(minx_*3600, maxx_*3600, base_);

	double maxy =  (maxy_ == miny_ )  ? maxy_ + 1 : maxy_;
	transformation.setDataMinMaxY(miny_, maxy_);
}

spot_query_result* MtgTempe::prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
{
	spot_query* query = decoder.newQuery();
	
	spot_set_parameter(query,"param", "167.128"); 
	spot_query_result* result;
	{
	Timer timer("Spot Handle","");
	result = spot_handle_query(query);
	}
	interpretResult(result,out, "curve1");
	spot_delete_result(result);
	spot_delete_query(query);
	
	query = decoder.newQuery();
	spot_set_parameter(query,"param", "130.128"); 
	{
	Timer timer("Spot Handle","");
	result = spot_handle_query(query);
	}
	
	interpretResult(result, out, "curve2");
	spot_delete_query(query);
	out.front()->insert(make_pair("temperature", 1));
	return result; 
	
}

spot_query_result* MetgramParameter::prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
{
	spot_query* query = decoder.newQuery();
	
	spot_set_parameter(query,"param", code_.c_str()); 
	spot_query_result* result;
	{
	Timer timer("Spot Handle","");
	result = spot_handle_query(query);
	}

	spot_delete_query(query);

	return result; 
}

spot_query_result* MtgMSL::prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
{
	spot_query* query = decoder.newQuery();
	
	spot_set_parameter(query,"param", "151.128"); 
	spot_query_result* result;
	{
	Timer timer("Spot Handle","");
	result = spot_handle_query(query);
	}
	interpretResult(result,out, "curve1");

	spot_delete_query(query);

	return result;
}

spot_query_result* MtgHumidity::prepare(const ClassicMtgDecoder& decoder, vector<CustomisedPoint*>& out)
{
	spot_query* query = decoder.newQuery();
	
	spot_set_parameter(query,"param", "157.128"); 
	spot_query_result* result;
	{
	Timer timer("Spot Handle","");
	result = spot_handle_query(query);
	}
	interpretResult(result,out, "curve1");
	
	spot_delete_query(query);
	
	return result; 	
}

ClassicMtgDecoder::ClassicMtgDecoder() :spot_(0) 
{
}


ClassicMtgDecoder::~ClassicMtgDecoder() 
{
	
}

/*!
 Class information are given to the output-stream.
*/		
void ClassicMtgDecoder::print(ostream& out)  const
{
	out << "ClassicMtgDecoder[";
	ClassicMtgDecoderAttributes::print(out);
	out << "]";
}

template <class T>
inline void read(const string& str, T& i) 
{
	
	std::stringstream ss(str);
	ss >> i;

}


}

void ClassicMtgDecoder::visit(TextVisitor& title)
{
	decode();
	moreTitle(title);
	if (parameter_) 
		title.add(new TextEntry(parameter_->title()));
	
}
/*
void ClassicMtgDecoder::visit(MetaData&)
{
	
	
	
	
}
*/

void ClassicMtgDecoder::moreTitle(TextVisitor&) const 
{
}

spot_query* ClassicMtgDecoder::newQuery() const
{
Timer time("openSpot","");
	if (! spot_) {
		spot_ = spot_open_database(database_.c_str());	
	}
	if ( !spot_ )
	{
		MagLog::error() << database_ << ": Not a valid Epsgram Database\n";
		throw MagicsException(database_ +": Not a valid Epsgram Database");
	}

	spot_query*  query  =  spot_new_query(spot_);
	spot_set_location (query, latitude_, longitude_);

	spot_set_method(query,"EPSGRAM");
 
	if (!date_.empty() ) {
		MagLog::debug() << "Set the date --->" << date_ << "\n";
		spot_set_parameter(query,"date", date_.c_str());
	}
   
	if (!time_.empty() ) {
		MagLog::debug() << "Set the date --->" << time_ << "\n";
		spot_set_parameter(query,"time", time_.c_str());
	}

	return query;
}


void ClassicMtgDecoder::customisedPoints(const std::set<string>&, CustomisedPointsList& out) 
{
	decode();
	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point)
		out.push_back(*point);
}


void ClassicMtgDecoder::decode() 
{
	if ( !points_.empty() ) return; 

#ifdef MAGICS_EXCEPTION            
	try {
		parameter_ = SimpleFactory<MetgramParameter>::create(param_);        
	}
	catch (NoFactoryException& e)
	{
		// The data do not know how to verify the criter ....
		MagLog::warning() << "Eps Data access: parameter [" << param_ << "] unknown\n";   
		parameter_ = new MetgramParameter(param_, param_title_, param_);
	} 
#else
	parameter_ = SimpleFactory<MetgramParameter>::create(param_);
	if ( !parameter_ )
	{
		// The data do not know how to verify the criter ....
		MagLog::warning() << "Eps Data access: parameter [" << param_ << "] unknown\n";
		parameter_ = new MetgramParameter(param_, param_title_, param_);
	}
#endif   
	parameter_->scaling(param_scaling_);
	parameter_->offset(param_offset_);
	spot_query_result *result;
	{
		Timer timer("prepare param","");
	    result = parameter_->prepare(*this, points_);
	}
	if ( !result )
	{
		MagLog::error() << " No EpsData\n";
		return;
	}
	Timer ("prepare meta data","");
	double lat, lon;

	spot_get_location(result, &lat, &lon);
	
	spot_get_index_value(result, "orog", &detz_);
	spot_get_index_value(result, "mask", &mask_);

	if ( lon > 180 ) lon -= 360.;

	grid_ = UserPoint(lon, lat);

	map<string, int> columns;
	map<int, int> rows;
	

	long resol; 
	
	read(get_spot(result,"numberOfPointsAlongAMeridian"), resol);
	ostringstream rs, dets;
	
	rs << "T" << resol-1;
	dets << "T" << (2*resol)-1;
	resolution_ = rs.str();

	date_ = get_spot(result,"date");
	time_ = get_spot(result,"time");

	spot_delete_result(result);

	spot_close_database(spot_);
	//delete spot_;
	spot_ = 0;
}

void ClassicMtgDecoder::moreTitle(TextVisitor& title)
{
	if ( !long_title_ ) return;
	
	DateTime base(date_, MagTime(time_));
	ostringstream out;
	tm convert = base;
	locale loc("");        
    
	out.imbue(loc);   
	const std::time_put<char>& tfac = use_facet<time_put<char> >(loc); 
	string format = "%A %e %B %Y %H UTC";
	tfac.put(out, out, ' ', &convert, format.c_str(), format.c_str()+format.length()); 
	vector<string> lines;
	
	string height = "";
	if (height_ < INT_MAX  ) {
		ostringstream sh;
		sh << " (" << height_ << "m) ";
		height = sh.str();
	}
	else {
		height = " ";
	}
	
	lines.push_back(station_ + " " + grid_.asLatitude() + " " + grid_.asLongitude() + " " + tostring(maground(detz_)) + " m");
	lines.push_back("ECMWF Forecast from " + out.str());
	
	string landsea = (mask_>= 0.5) ? "Nearest land grid point" : "Nearest sea grid point";
	
	lines.push_back(landsea + " (" + resolution_ + ")" );
	
	
	
	lines.push_back("");
	
	for ( vector<string>::const_iterator line = lines.begin(); line != lines.end(); ++line)
	 	title.add(new TextEntry(*line));
}

void ClassicMtgDecoder::visit(Transformation& transformation)
{
	decode();
	
	assert(parameter_);
	parameter_->setTransformation(transformation);

	
}

