/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SpotDecoder.h
    \brief Implementation of the Template class SpotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/
 


#include "EpsgramDecoder.h"
#include "MetaData.h"
#include "CustomisedPoint.h"
#include "DateTime.h"
#include "TextVisitor.h"
#include "LegendVisitor.h"
#include "XmlReader.h"
#include "IntervalMap.h"
#include "EfiLegendEntry.h"

#include "Factory.h"
#include <locale>
#include <limits>
#include <cfloat>

using namespace magics;

namespace magics
{

EpsParameter::EpsParameter() : title_("eps10")
{
		specifics_["eps10"] = &EpsParameter::specific10;
		specifics_["eps15"] = &EpsParameter::specific15;
		minx_ = std::numeric_limits<double>::max();
		maxx_ = -minx_;
		miny_ = std::numeric_limits<double>::max();
		maxy_ = std::numeric_limits<double>::min();
}

string EpsParameter::x(const string& prefix, const string& name) const
{
	static map<string, string> names;
//	if ( names.empty() ) {
//		names["0"] = "min";
//		
//		names["1"] = "one";
//		names["10"] = "ten";
//		names["25"] = "twentyfive";
//		names["50"] = "median";
//		names["75"] = "seventyfive";
//		names["90"] = "ninty";
//	    names["99"] = "nintynine";
//
//		names["100"] = "max";
//
//	}
	return ( names.find(name) == names.end() ) ?  prefix + lowerCase(name) : prefix + names.find(name)->second;  
}

void EpsParameter::specific15(CustomisedPoint& point) const 
{		
	point["shift"] = shift_ * 3600;
	point["shift"] = 0 * 3600;
	point["width"] = 3 * 3600;
	point["last"] = 15 * (24 * 3600);
}

void EpsParameter::specific10(CustomisedPoint& point) const 
{		
	point["width"] = 1.5 * 3600;
	point["last"] = 10.25 * (24 * 3600);
}

void EpsParameter::steps(const vector<double>& steps)
{
	for(vector<double>::const_iterator step = steps.begin(); step != steps.end(); ++step) 
		steps_.insert(make_pair(*step, *step));
}


class Eps2mTemperature: public EpsParameter
{
public:
	Eps2mTemperature(const string& name, const string& title, const string& code) : EpsParameter(name, title, code)
	{}
	Eps2mTemperature(const string& code) : EpsParameter("min", "2m Temperature (deg C) (No reduction ( working on it...) ", code)
	{}
	Eps2mTemperature() : EpsParameter("min", "2m Temperature (deg C) (No reduction ( working on it...) ", "167.128")  
	{}
	~Eps2mTemperature() {}
	const string& title() const
	{
		ostringstream title;
		title << "2m Temperature " << " (&#176;C) " << reduction_  <<  " from " << maground(detz_) << " m ("  << detResolution_ << ") and " << maground(epsz_) << " m (" << epsResolution_ << ")";
		title_ = title.str();
		return title_;
	}

	const string& xml() const
	{
		ostringstream xml;
		xml << "2m Temperature (deg C) ";
		xml_ = xml.str();
		return xml_;
	}

	double eps(double value) const
	{
		reduction_ =  "reduced to the station height";
		if ( height_ == INT_MAX && detz_)
		{
			//reduction_ =  "reduced to the " + detResolution_ + " orography";
			const double correction = ( detz_ - epsz_ ) * 0.0065;
			return value - correction;
		} 
		if ( height_ == INT_MAX )
		{
			//reduction_ =  "no reduction";
			return value;
		}

		const double correction = (height_ - epsz_ ) * 0.0065;
		return value - correction; 
	}

	double deterministic(double value) const
	{
		if ( height_ == INT_MAX )
			return value;
		const double correction = (height_ - detz_ ) * 0.0065;
		return value - correction ; 
	}
   
	void specific15(CustomisedPoint& point) const
	{
		point["shift"] = 6 * 3600;
		point["shift"] = 0;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 *3600);
	}

	void specific10(CustomisedPoint& point) const
	{
		point["width"] = 1.5 * 3600;
		point["last"] = 10.25 * (24 *3600);
	}	
  
	virtual double operator()(double value, const string& data) const
	{
		if ( !correction_ ) return value - 273.15;
		
		if ( map_.empty() ) {
			map_["forecast"] = &Eps2mTemperature::deterministic;
			map_["control"] = &Eps2mTemperature::eps;
			map_["median"] = &Eps2mTemperature::eps;
			map_["twentyfive"] = &Eps2mTemperature::eps;
			map_["min"] = &Eps2mTemperature::eps;
			map_["max"] = &Eps2mTemperature::eps;
			map_["seventyfive"] = &Eps2mTemperature::eps;
			map_["ten"] = &Eps2mTemperature::eps;
			map_["ninty"] = &Eps2mTemperature::eps;
		}
		map<string,  CorrectionFunction >::const_iterator correction = map_.find(data);		
		return ( correction != map_.end() ) ?  (this->*correction->second)(value) - 273.15 : value - 273.15; 
	}
	
protected :
	typedef double (Eps2mTemperature::*CorrectionFunction)(double) const;
	mutable string reduction_;
	static map<string, CorrectionFunction > map_;
};

string EpsParameter::height() 
{
	ostringstream height;
	if ( height_ < INT_MAX )
		height << " " << height_ << " m";
	else 
		height << " " << maground(detz_) << " m (" << detResolution_ << ")";

	return height.str();;
}

class EpsTemperature: public Eps2mTemperature
{
public:
	EpsTemperature() {}
	~EpsTemperature() {}
	virtual double operator()(double) const 
	{		
		return shift_;
	} 
  
	const string& title() const
	{
		ostringstream title;
		title << "2m min/max temperature (&#176;C) " << reduction_ << " from "  << maground(epsz_) << "m (" << epsResolution_ << ")";
		title_ = title.str();
		return title_;
	}

	const string& xml() const
	{
		ostringstream xml;
		xml << "2m Min Temperature (deg C) ";
		xml_ = xml.str();
		return xml_;
	}
	
	void specific15(CustomisedPoint& point) const
	{	
		point[( shift_ == -3 ) ? "left" : "right" ] = 1;	
		point[( shift_ == -3 ) ? "tmin" : "tmax" ] = 1;
		point["shift"] = shift_ * 3600;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 *3600);
	}
	
	void specific10(CustomisedPoint& point) const
	{
		point["width"] = 1.5 * 3600;
		point["last"] = 10.25 * (24 *3600);
	}	

	spot_query_result* prepare(const SpotDecoder& decoder, vector<CustomisedPoint*>& out)
	{
		spot_query* query = decoder.newQuery();

		spot_set_parameter(query,"param", "122.128");
		shift_ = -3; 
		spot_query_result* result = spot_handle_query(query);
		interpretResult(result,out);
		spot_delete_query(query);
		spot_delete_result(result);	

		query = decoder.newQuery();	
		spot_set_parameter(query,"param", "121.128");
		shift_ = 3; 
		result = spot_handle_query(query);
		interpretResult(result,out);
		spot_delete_query(query);		
		return result; 
	}

protected :
	typedef double (Eps2mTemperature::*CorrectionFunction)(double) const;
	mutable double shift_;
};


class ClTemperature: public EpsTemperature
{
public:
    ClTemperature() {}
    ~ClTemperature() {}
    const string& title() const {
		title_= "";
		return title_;
	} 
};


map<string,  Eps2mTemperature::CorrectionFunction> Eps2mTemperature::map_;

class EpsPrecipitation: public EpsParameter
{
public:
	EpsPrecipitation(const string& code = "143.128") : 
		EpsParameter("precip", "Total Precipitation (mm/24h)", code) { step_ = 0; }
	~EpsPrecipitation() {}
	virtual double operator()(double) const 
	{
		return 12;
	} 
	const string& title() const {
		title_ = "Total Precipitation for " + type_;
		if ( type_ == "eps10" ) title_ =  "Total Precipitation (mm/6h)";
		if ( type_ == "eps15" ) title_ =  "Total Precipitation (mm/24h)";
		return title_;
	}
	virtual double operator()(double value, const string&) const { 
		if (value < 0 ) return 0;
		return value*1000;
	} 
	 
	void specific15(CustomisedPoint& point) const {		
		point["shift"] = -12 * 3600;
		point["shift"] = 0;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 * 3600);
	}
	void specific10(CustomisedPoint& point) const {		
		point["shift"] = -3 * 3600;
		point["width"] = 1.5 * 3600;
		point["last"] = 10.25 * (24 * 3600);
	}
	void setTransformation(Transformation& transformation)
	{

	if ( type_ == "eps15" ) {
		transformation.setDataMinMaxX(0, maxx_+18*3600, base_);

	}
	if ( type_ == "eps10" ) {
		transformation.setDataMinMaxX(0, maxx_ + 6*3600, base_);

	}
	else {
		transformation.setDataMinMaxX(minx_, maxx_ - 6*3600, base_);
	}
	
	transformation.setDataMinMaxY(miny_, maxy_);
}
protected : 
	map<string, string> titles_;
};


class ClimEpsPrecipitation: public EpsPrecipitation
{
public:
	ClimEpsPrecipitation() : 	EpsPrecipitation("143.128") { title_ = ""; }
	const string& title() const {
		static string none;
		return none;
	}
	int x(int val) const { 
		if ( val == 20 ) return val+256; 
		if ( val == 44 ) return val+256; 
		if ( val == 68 ) return val+256; 
		if ( val == 92 ) return val+256; 
		else return val;
	} 
	string x(const string& val) const { return val; } 
	 void specific15(CustomisedPoint& point) const {		
		point["shift"] = 0;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 * 3600);
	}
};


class EpsCloudCover: public EpsParameter
{
public:
	EpsCloudCover(const string& code = "164.128") : 
		EpsParameter("cloud-cover", "Daily mean of Total Cloud Cover (okta)", code) {}
	~EpsCloudCover() {}

	const string& title() const
	{
		title_ = "Cloud Cover for " + type_;
		if ( type_ == "eps10" ) title_ =  "Total Cloud Cover (okta)";
		if ( type_ == "eps15" ) title_ =  "Daily mean of Total Cloud Cover (okta)";
		return title_;
	}

	virtual double operator()(double value, const string&) const
	{
		return 8*value;
	} 

	void specific15(CustomisedPoint& point) const {		
		point["shift"] = 6 * 3600;
		point["shift"] = 0 * 3600;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 * 3600);
	}

	void specific10(CustomisedPoint& point) const
	{		
		point["width"] = 1.5 * 3600;
		point["last"] = 10.25 * (24 * 3600);
	}
	
};

class ClimEpsTemperature: public Eps2mTemperature
{
public:
	ClimEpsTemperature() : 	Eps2mTemperature("167.128") {}

	int x(int val) const
	{ 
		if ( val == 20 ) return val+256; 
		if ( val == 44 ) return val+256; 
		if ( val == 68 ) return val+256; 
		if ( val == 92 ) return val+256; 
		else return val;
	} 

	const string& title() const
	{
		title_ = "";
		return title_;
	}

	void specific15(CustomisedPoint& point) const
	{
		point["shift"] = 0;
	}
};


class ClimEpsMinTemperature: public Eps2mTemperature
{
public:
	ClimEpsMinTemperature() : Eps2mTemperature("122.128") {}

	int x(int val) const
	{ 
		if ( val == 20 ) return val+256; 
		if ( val == 44 ) return val+256; 
		if ( val == 68 ) return val+256; 
		if ( val == 92 ) return val+256; 
		else return val;
	} 

	const string& title() const
	{
		title_ = "";
		return title_;
	}

	void specific15(CustomisedPoint& point) const
	{
		point["shift"] = 0;
	}
	
};


class ClimEpsMaxTemperature: public Eps2mTemperature
{
public:
	ClimEpsMaxTemperature() : Eps2mTemperature("121.128") {}

	int x(int val) const
	{ 
		if ( val == 20 ) return val+256; 
		if ( val == 44 ) return val+256; 
		if ( val == 68 ) return val+256; 
		if ( val == 92 ) return val+256; 
		else return val;
	}
	 
	const string& title() const
	{
		title_ = "";
		return title_;
	}

	void specific15(CustomisedPoint& point) const
	{
		point["shift"] = 0;
	}
};

class ClimEpsCloudCover: public EpsCloudCover
{
public:
	ClimEpsCloudCover() : 	EpsCloudCover("164.128") {}
	int x(int val) const { 
		if ( val == 20 ) return val+256; 
		if ( val == 44 ) return val+256; 
		if ( val == 68 ) return val+256; 
		if ( val == 92 ) return val+256; 
		else return val;
	}
	 
	const string& title() const
	{
		title_ = "";
		return title_;
	}

	void specific15(CustomisedPoint& point) const
	{
		point["shift"] = 0;
	}
};


class EfiTemperature: public Eps2mTemperature
{
public:
	EfiTemperature(const string& name, const string& title, const string& code) : 
		Eps2mTemperature(name, title, code) {}
	~EfiTemperature() {}
	const string& title() const
	{
		title_ = "Efi 2m temperature";
		return title_;
	}

	string x(const string& prefix, const string& val) const
	{ 
		 return (prefix == "clim_") ? prefix + val :prefix +  tostring( 2*tonumber(val)); 
	}

	void setTransformation(Transformation& transformation)
	{
	    transformation.setDataMinMaxX(minx_, maxx_);
	}

	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos)
	{ 
		xpos.push_back(x); 
	}
    
};


class EfiMaxTemperature: public EfiTemperature
{
public:
	EfiMaxTemperature() : EfiTemperature("efi-2mt-max", "clim", "121.128") {}
	~EfiMaxTemperature() {}
	const string& title() const {
		title_ = "CDF for 24h max 2m temperature (&#176;C)";
        return title_;
	}
};

class EfiMinTemperature: public EfiTemperature
{
public:
	EfiMinTemperature() : EfiTemperature("efi-2mt-min", "clim", "122.128") {}
	~EfiMinTemperature() {}
	const string& title() const {
		title_ = "CDF for 24h min 2m temperature (&#176;C)";
        return title_;
	}
};

class EfiMeanTemperature: public EfiTemperature
{
public:
	EfiMeanTemperature() : EfiTemperature("efi-2mt-mean", "clim", "167.128") {}
	~EfiMeanTemperature() {}
	const string& title() const {
		title_ = "CDF for 24h mean 2m temperature (&#176;C)";
        return title_;
	}
};

class ClimTemperature: public Eps2mTemperature
{
public:
	ClimTemperature(const string& name, const string& title, const string& code) : 
		Eps2mTemperature(name, title, code) {}
	~ClimTemperature() {}
	const string& title() const {
		title_ = "2m temperature";
        return title_;
	}
	void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_);
	    transformation.setDataMinMaxY(miny_, maxy_);
	}
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) 
	{ xpos.push_back(x); }
	
};

class ClimMaxTemperature: public ClimTemperature
{
public:
	ClimMaxTemperature() : ClimTemperature("clim-2mt-max", "clim", "121.128") {}
	~ClimMaxTemperature() {}
	const string& title() const {
		title_ = "2m temperature max";
        return title_;
	}
};

class ClimMinTemperature: public ClimTemperature
{
public:
	ClimMinTemperature() : ClimTemperature("clim-2mt-min", "clim", "122.128") {}
	~ClimMinTemperature() {}
	const string& title() const {
		title_ = "2m temperature min";
        return title_;
	}
};
class ClimMeanTemperature: public ClimTemperature
{
public:
	ClimMeanTemperature() : ClimTemperature("clim-2mt-mean", "clim", "167.128") {}
	~ClimMeanTemperature() {}
	const string& title() const {
		title_ = "2m temperature mean";
        return title_;
	}
};

class EfiWindGust: public EpsParameter
{
public:
	EfiWindGust() : EpsParameter("efi-wind-gust", "Wind Gust", "123.128") {}
	~EfiWindGust() {}
	const string& title() const {
		title_ = "CDF for 24h maximum wind gust (m/s)";
        return title_;
	}
	virtual double operator()(double value, const string&) const { 
		return value; 
	} 
	virtual double operator()(double val) const { return val; } 

	string x(const string& prefix, const string& val) const { 
			 return (prefix == "clim_") ? prefix + val :prefix +  tostring( 2*tonumber(val)); 
		}
	void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_);
		
	}
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) { xpos.push_back(x); }
};

class EfiWind: public EpsParameter
{
public:
	EfiWind() : EpsParameter("efi-wind", "Wind", "165.128") {}
	~EfiWind() {}
	const string& title() const {
		title_ = "CDF for 24h maximum wind gust (m/s)";
        return title_;
	}
	virtual double operator()(double value, const string&) const { 
		return value; 
	} 
	virtual double operator()(double val) const { return val; } 
	string x(const string& val) const 
		{ return tostring( 2*tonumber(val)); } 
	void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_);
		
	}
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) { xpos.push_back(x); }
};

class ClimWindGust: public EpsParameter
{
public:
	ClimWindGust() : EpsParameter("clim-wind-gust", "Wind Gust", "123.128") {}
	~ClimWindGust() {}
	const string& title() const {
		title_ = "Clim Wind Gust";
        return title_;
	}
	virtual double operator()(double val) const { return val; } 
	virtual double operator()(double value, const string&) const { 
			return value; 
		} 
	
	void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_);
		
	}
	
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) { xpos.push_back(x); }
};
class ClimWind: public EpsParameter
{
public:
	ClimWind() : EpsParameter("clim-wind-gust", "Wind Gust", "165.128") {}
	~ClimWind() {}
	const string& title() const {
		title_ = "Clim Wind";
        return title_;
	}
	virtual double operator()(double val) const { return val; } 
	virtual double operator()(double value, const string&) const { 
			return value; 
		} 
	
	void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_);
		
	}
	
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) { xpos.push_back(x); }
};
class EfiPrecipitation: public EpsParameter
{
public:
	EfiPrecipitation() : EpsParameter("efi-precip", "Precipitation", "143.128") {}
	~EfiPrecipitation() {}
	const string& title() const {
		title_ = "CDF for 24h precipitation (mm)";
        return title_;
	}
	virtual double operator()(double value, const string&) const { 
		if (value < 0 ) return 0;
		return value*1000;
	 } 
	 string x(const string& prefix, const string& val) const { 
		 return (prefix == "clim_") ? prefix + val :prefix +  tostring( 2*tonumber(val)); 
	} 
	 void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_);
		
	}
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) { xpos.push_back(x); }
	

};
class PlumeGeopotential: public EpsParameter
{
public:
	PlumeGeopotential() : EpsParameter("z500", "z500", "129.128") {}
	~PlumeGeopotential() {}
	const string& title() const {
		double range =  maxy_ - miny_; 
		
		title_ = "Geopotential 500hPa - Probability for 2.5 dam intervals      Range : " + tostring(maground(range)) + " dam";
        return title_;
	}
	virtual double operator()(double value, const string&) const { 
		
		return value/100;
	 } 
	double plumesInterval() { return 2.5; }
};


class PlumeTemperature: public EpsParameter
{
public:
		PlumeTemperature() : EpsParameter("t850", "t850", "130.128") {}
		~PlumeTemperature() {}
		const string& title() const {
			double range =  maxy_ - miny_; 
			title_ = "Temperature 850hPa - Probability for 1&#176;C intervals      Range :  " + tostring(maground(range))  + " &#176;C";
	        return title_;
		}
		virtual double operator()(double value, const string&) const { 
			
			return value-273.15;
		 } 
		double plumesInterval() { return 1.; }
	};
	
	class PlumePrecip: public EpsParameter
	{
	public:
	
		PlumePrecip() : EpsParameter("rr", "rr", "143.128") {}
			~PlumePrecip() {}
			const string& title() const {
				title_ = "Ensemble members of Total Precipitation - Accum rate mm/12h";
		        return title_;
			}
			virtual double operator()(double value, const string&) const { 
				if (value < 0 ) return 0;
				return value*1000;
			 } 
			
		};
class ClimPrecipitation: public EpsParameter
{
public:
	ClimPrecipitation() : EpsParameter("clim-precip", "Precipitation", "143.128") {}
	~ClimPrecipitation() {}
	const string& title() const {
		title_ = "Clim Precipitation";
        return title_;
	}
	virtual double operator()(double value, const string&) const { 
		if (value < 0 ) return 0;
		return value*1000;
	 } 
	 void setTransformation(Transformation& transformation)
    {
	    transformation.setDataMinMaxX(minx_, maxx_); 
		
	}
	virtual void stepvalues(double, vector<double>&) {}  
	virtual void xvalues(double x, vector<double>& xpos) { xpos.push_back(x); }
};

class EpsWindFF: public EpsParameter
{
public:
	EpsWindFF(const string& code = "165.128") : EpsParameter("10m-wind", "Daily mean of 10m Wind Speed (m/s)", code ) {}
	~EpsWindFF() {}
	const string& title() const {
		title_ = "10m Wind Speed (m/s) for " + type_;
		if ( type_ == "eps10" ) title_ =  "10m Wind Speed (m/s)";
		if ( type_ == "eps15" ) title_ =  "Daily mean of 10m Wind Speed (m/s)";
        return title_;
	}
	 void specific15(CustomisedPoint& point) const {		
		point["shift"] = 6 * 3600;
		point["shift"] = 0 * 3600;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 * 3600);
	}
    void specific10(CustomisedPoint& point) const {		
		point["width"] = 1.5 * 3600;
		point["last"] = 10.25 * (24 * 3600);
	}
};

class ClimEpsWindFF: public EpsWindFF
{
public:
	ClimEpsWindFF() : 	EpsWindFF("165.128") {}
	int x(int val) const { 
		if ( val == 20 ) return val+256; 
		if ( val == 44 ) return val+256; 
		if ( val == 68 ) return val+256; 
		if ( val == 92 ) return val+256; 
		else return val;
	}
	const string& title() const {
		title_ = "";
		return title_;
	}
	void specific15(CustomisedPoint& point) const {		
		point["shift"] = 0;
		
	}
};

class EpsWindDD: public EpsParameter
{
public:
	EpsWindDD() : EpsParameter("10m-wind-dd", "Daily mean of 10m Wind", "249.140") {}
	~EpsWindDD() {}
	
	const string& title() const {
		title_ = "10m Wind Speed (m/s) for " + type_;
		if ( type_ == "eps10" ) title_ =  "10m Wind Speed (m/s)";
		if ( type_ == "eps15" ) title_ =  "Daily distribution of 10m Wind Direction";
        return title_;
	}
	
	void specific15(CustomisedPoint& point) const {		
		point["shift"] = 6 * 3600;
		point["shift"] = 0 * 3600;
		point["width"] = 3 * 3600;
		point["last"] = 15 * (24 * 3600);
	}
   
};
}
static SimpleObjectMaker<EfiMaxTemperature, EpsParameter> efi_2mt_max("efi-2mt-max");
static SimpleObjectMaker<EfiMinTemperature, EpsParameter> efi_2mt_min("efi-2mt-min");
static SimpleObjectMaker<EfiMeanTemperature, EpsParameter> efi_2mt_mean("efi-2mt-mean");
static SimpleObjectMaker<ClimMaxTemperature, EpsParameter> clim_2mt_max("clim-2mt-max");
static SimpleObjectMaker<ClimMinTemperature, EpsParameter> clim_2mt_min("clim-2mt-min");
static SimpleObjectMaker<ClimMeanTemperature, EpsParameter> clim_2mt_mean("clim-2mt-mean");

static SimpleObjectMaker<EfiWindGust, EpsParameter> efi_wind_gust("efi-wind-gust");
static SimpleObjectMaker<EfiWind, EpsParameter> efi_wind("efi-wind");
static SimpleObjectMaker<ClimWindGust, EpsParameter> clim_wind_gust("clim-wind-gust");
static SimpleObjectMaker<ClimWind, EpsParameter> clim_wind("clim-wind");

static SimpleObjectMaker<EfiPrecipitation, EpsParameter> efi_precip("efi-precip");
static SimpleObjectMaker<ClimPrecipitation, EpsParameter> clim_eps_precip("clim-precip");

static SimpleObjectMaker<Eps2mTemperature, EpsParameter> mt("2mt");
static SimpleObjectMaker<ClimEpsMaxTemperature, EpsParameter> tempe_clim_max("clim-eps-2mt-max");
static SimpleObjectMaker<ClimEpsMinTemperature, EpsParameter> tempe_clim_min("clim-eps-2mt-min");
static SimpleObjectMaker<ClimEpsTemperature, EpsParameter> tempe_clim("clim-eps-2mt");

static SimpleObjectMaker<PlumeGeopotential, EpsParameter> z500("z500");
static SimpleObjectMaker<PlumeTemperature, EpsParameter> t850("t850");
static SimpleObjectMaker<PlumePrecip, EpsParameter> rr("rr");


static SimpleObjectMaker<EpsTemperature, EpsParameter> temperature("temperature");
static SimpleObjectMaker<ClTemperature, EpsParameter> cltemperature("clim-temperature");
static SimpleObjectMaker<EpsWindFF, EpsParameter> wind("10m-wind");
static SimpleObjectMaker<ClimEpsWindFF, EpsParameter> clim_wind_ff("clim-10m-wind");
static SimpleObjectMaker<EpsWindDD, EpsParameter> windDD("10m-wind-dd");

static SimpleObjectMaker<EpsCloudCover, EpsParameter> cloud("cloud-cover");
static SimpleObjectMaker<ClimEpsCloudCover, EpsParameter> clim_cloud("clim-eps-cloud-cover");

static SimpleObjectMaker<EpsPrecipitation, EpsParameter> precip("precip");
static SimpleObjectMaker<ClimEpsPrecipitation, EpsParameter> clim_precip("clim-eps-precip");


spot_query_result* EpsParameter::prepare(const SpotDecoder& decoder, vector<CustomisedPoint*>& out)
{
		spot_query* query = decoder.newQuery();
		
	
		
		spot_set_parameter(query,"param", code_.c_str()); 
		spot_query_result* result = spot_handle_query(query);
		interpretResult(result,out);
		spot_delete_query(query);		
	    return result; 
	
}

template <class T>
inline void read(const string& str, T& i) 
{
	
	std::stringstream ss(str);
	ss >> i;

}
static string get_spot(spot_query_result* result, const char* param)
{
	char tmp[1024];
	size_t len = 1024;
	
	spot_get_parameter(result,param,tmp, &len); 
	
	return string(tmp, len-1);
}


string convert(const string& name)
{
	static map<string, string> names;
	if ( names.empty() ) {
		names["0"] = "min";
		
		names["1"] = "one";
		names["10"] = "ten";
		names["25"] = "twentyfive";
		names["50"] = "median";
		names["75"] = "seventyfive";
		names["90"] = "ninty";
	    names["99"] = "nintynine";

		names["100"] = "max";

	}	
	
	
	return ( names.find(name) == names.end() ) ?  lowerCase(name) : names.find(name)->second;  
	
}

struct SortHelper
{
	bool operator()(CustomisedPoint* first, CustomisedPoint* second) {
		return first->longitude() < second->longitude();
	}
};


void EpsParameter::interpretResult(spot_query_result* result, vector<CustomisedPoint*>& out)
{
	size_t len = 1024;
	char tmp[1024];
	
	vector<double> xpos;
	vector<double> ypos;
	
	if ( result == 0 ) {
		MagLog::warning() << "No Information for parameter " << name_ << endl;
		minx_ = 0;
		maxx_ = 60;
		miny_ = 0;
		maxy_ = 1;
		DateTime base;	
		base_ = string(base);
		return;
	}
		
		
	
	string date = get_spot(result,"date");
	string time = get_spot(result,"time");
	map<string, int> columns;
	map<int, int> rows;
	
	
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

	DateTime base(date, MagTime(time));
	DateTime next;
	
	base_ = string(base);
	
	
	len = 1024;
	
	for(int i = 0 ; i < spot_get_column_count(result); i++){
    	spot_get_column_name(result, i,tmp,&len);        
    	columns[tmp] = i;
    	len = 1024;
	}
	
	
	for(int i = 0 ; i < spot_get_row_count(result); i++){
    	spot_get_row_name(result, i,tmp,&len);
    	
    	int x = atoi(tmp);
    	rows[x] = i;
    	len = 1024;    	
	}	
	
	long resol;


	read(get_spot(result,"numberOfPointsAlongAMeridian"), resol);
	
    spot_get_index_value(result, "epsz", &epsz_);
    spot_get_index_value(result, "detz", &detz_);    
   
    ostringstream rs, dets;	
    rs << "T" << resol-1;
    dets << "T" << (2*resol)-1;
    detResolution_ = dets.str() ;
    epsResolution_ = rs.str();
	 
	MagLog::debug() << "Code ---> " << code_ << endl;
	MagLog::debug() << "Resolution ---> " << resol << endl;
	MagLog::debug() << "Det height ---> " << detz_ << endl;
	MagLog::debug() << "Eps height ---> " << epsz_ << endl;
	MagLog::debug() << "Det resoltion ---> " << detResolution_ << endl;
	MagLog::debug() << "Eps resoltion ---> " << epsResolution_ << endl;
  
	
	for (map<int, int>::const_iterator step = rows.begin(); step != rows.end(); ++step) { 

		CustomisedPoint* point = new CustomisedPoint();		
		point->latitude(0);
		point->longitude((x(step->first) ) * 3600);				
		(*point)["resolution"] = resol;
		 
		Second s = x(step->first) * 3600;
		DateTime valid = base + s;
		point->base(base);
		point->valid(valid);      
		spot_get_row(result, step->second,values,&nb);
		
        bool ok = true;
	
		for (map<string, int>::const_iterator info = columns.begin(); info != columns.end(); ++info) {
		    	
			if ( !steps_.empty() && steps_.find(step->first) == steps_.end()) {
				ok = false;
				continue;
			}
           
            MagLog::debug()  << step->first << " -> " << info->first << " = " << values[info->second] << "->" << (*this)(values[info->second], info->first) << endl;
	    	(*point)["step"]    = x(step->first) * 3600;
	    	stepvalues(x(step->first) * 3600, xpos);
	 
			map<string, EpsParameter::SpecificFunction>::const_iterator function = specifics_.find(type_);
			(*point)["width"] = 1*3600;
			if ( type_ == "eps10" ) 
				specific10(*point);
			if ( type_ == "eps15" ) 
				specific15(*point);

		if ( !std::isnan(values[info->second]) ) {
		  (*point)[x(prefix_, info->first)] = (*this)(values[info->second], info->first);
		  //MagLog::debug() <<  x(info->first) << "----->" << (*this)(values[info->second], info->first) << endl;

		   ypos.push_back((*this)(values[info->second], info->first));

		  xvalues((*this)(values[info->second], info->first), xpos);

	    	  if ( info->first == "forecast"  && step->first > 240 )
	    	   (*point)["forecast"] = DBL_MIN;
	    	}
	    	else {
	    		MagLog::warning() << " SPOTBASE returns nan for " << info->first << " : data ignored for step " << (*point)["shift"]<< endl;
	    		ok = false;
	    	}
		}
    	if ( ok ) {
            out.push_back(point);
    	}
		nb=1024;
	}
	
	vector<double> maxs;
	vector<double> allvalues;

    for (vector<CustomisedPoint*>::iterator point = out.begin(); point != out.end(); ++point) {
    	maxs.push_back((**point)["max"]);
    	allvalues.push_back((**point)["seventyfive"]);
    	allvalues.push_back((**point)["ninty"]);
    	if ( (*point)->find("forecast") != (*point)->end() ) {
    		allvalues.push_back((**point)["forecast"]);
    		maxs.push_back((**point)["forecast"]);
    	}
    	if ( (*point)->find("control") != (*point)->end() ) {
    		allvalues.push_back((**point)["control"]);  
    		maxs.push_back((**point)["control"]);
    	}
    }

    if ( threshold_ < 30) {
    	double limit = *std::max_element(allvalues.begin(), allvalues.end());
    	std::sort(maxs.begin(), maxs.end());

    	double mm = maxs.back();
    	double p1 = 1;
    	double p2;
    	int i = maxs.size() -1 ;
    	int n = maxs.size();
    	for ( vector<double>::reverse_iterator val = maxs.rbegin(); val != maxs.rend(); ++val) {
    		p2 = (*val)/maxs.back();
    	
    		if ( p2 < (1 - i/n)*percentile_ && (p1-p2) > threshold_/n && *val >= limit ) {
    			mm = *val;
    		}
   			p1 = p2;
			i--;
  	  	}
    
    	miny_ = std::min(miny_, *std::min_element(ypos.begin(), ypos.end()));
        maxy_ = std::max(mm, 1.);
    }
    else {
    	 miny_ = std::min(miny_, *std::min_element(ypos.begin(), ypos.end()));
    	 maxy_ = std::max(maxy_,*std::max_element(ypos.begin(), ypos.end()));
    }
    
	
	std::sort(out.begin(), out.end(), SortHelper());
	minx_ = std::min(minx_, *std::min_element(xpos.begin(), xpos.end()));
	maxx_ = std::max(maxx_, *std::max_element(xpos.begin(), xpos.end()));
	

    
	
	if ( miny_  == maxy_ ) {
		if ( miny_ == 9999. ) {
			miny_ = 0;
			maxy_ = 20;
		}
		else 
			maxy_ = miny_ + 1;
	}
}


SpotDecoder::SpotDecoder() :parameter_(0), prefix_(""),  spot_(0), shift_(0)
{
}




SpotDecoder::~SpotDecoder() 
{
	if (parameter_) delete parameter_;
}


/*!
 Class information are given to the output-stream.
*/		
void SpotDecoder::print(ostream& out)  const
{
	out << "SpotDecoder[";
	out << "]";
}





PointsHandler& SpotDecoder::points()
{
	decode();
	
	pointsHandlers_.push_back(new PointsHandler(*this));
	return *(pointsHandlers_.back());
}

void SpotDecoder::visit(TextVisitor& title)
{
	decode();
	moreTitle(title);
	
	if ( !magCompare(parameter_->title(), "ignore")  )
			title.update("spot", "parameter",  parameter_->title());
}

static string build_date(const string& date, const string& time)
{
	ostringstream out;
	if ( date.empty() ) {
		out << "Unknown date";
	}
	else {
		DateTime base(date, MagTime(time));
		tm convert = base;	
    	locale loc("");
    	out.imbue(loc);   
    	const std::time_put<char>& tfac = use_facet<time_put<char> >(loc); 
    	string format = "%A %e %B %Y %H UTC";
    	tfac.put(out, out, ' ', &convert, format.c_str(), format.c_str()+format.length());    	
	}
	return out.str();
}


void EpsgramDecoder::visit(MetaDataVisitor& metadata)
{
	decode();
	ostringstream meta;
	meta << "<epsgram>\n";
	//toxml(meta, "input", 1);
	meta << "\t<output\n";
	meta << "\t\tparameter =\'" << parameter_->xml() << "\'\n";
	meta << "\t\tdate =\'" << SpotDecoder::date_ << "\'\n";
	meta << "\t\ttime =\'" << SpotDecoder::time_ << "\'\n";
	
	string date = build_date(SpotDecoder::date_, SpotDecoder::time_);

	meta << "\t\tnice_date =\'" <<  date << "\'\n";
	string landsea = (mask_>= 0.5) ? "no" : "yes";
	meta << "\t\tsea_point =\'" <<  landsea << "\'\n";

	string height = "";
	if (SpotDecoder::height_ < INT_MAX  ) {
		ostringstream sh;
		sh << SpotDecoder::height_ << "m";
		height = sh.str();
		meta << "\t\theight =\'" <<  height << "\'\n";
	}

	UserPoint position(SpotDecoder::longitude_, SpotDecoder::latitude_);
	meta << "\t\tstation =\'" <<  SpotDecoder::station_ << "\'\n";
	meta << "\t\tlatitude =\'" <<  grid_.y() << "\'\n";
	meta << "\t\tlongitude =\'" <<  grid_.x() << "\'\n";
	meta << "\t\tresolution =\'" <<  resolution_ << "\'/>\n";
	
	meta << "</epsgram>\n";

	metadata.add(new MetaDataEntry(meta.str()));
}


EpsgramDecoder::EpsgramDecoder()
{
}

EpsgramDecoder::~EpsgramDecoder() 
{
}

void EpsgramDecoder::moreTitle(TextVisitor& title) const 
{
	
	static map<string, string> titles;
	if ( titles.empty() ) {
		titles["eps10"] = "";
		titles["eps15"] = "Extended Range Forecast based on EPS Distribution ";
		titles["epsrose"] = "Deterministic Forecast and EPS Distribution ";
		titles["epsplume"] = "ECMWF ensemble forecast ";
	}
	string date = build_date(SpotDecoder::date_, SpotDecoder::time_);
	 
    ostringstream lt;
    for (stringarray::const_iterator t = title_.begin(); t != title_.end(); ++t)
    	lt  << *t;
   
    string long_title = SpotDecoder::station_;    
 	title.add(new TextEntry(title_text_));
	
	string landsea = (mask_>= 0.5) ? " (EPS land point)" : " (EPS sea point)";
	
	
	string height = long_title_height_ ? parameter_->height() : " ";

	string epsz = "";
	ostringstream sd;
	//sd  <<  maground(epsz_) << " m";
    sd  <<  "undef" << " m";
	epsz = sd.str();
	
	MagLog::dev() << "title-->" << long_title << "(" << long_title.size() << ")\n";

    string station = long_title;
    if ( long_title_point_ ) {
    	station = station + string(" ")  + grid_.asLatitude() + " " + grid_.asLongitude() + landsea + height;
    }
    
 	title.update("spot", "base_date",  date);
 	title.update("spot", "station",  station);
	
}


void SpotDecoder::customisedPoints(const std::set<string>&, CustomisedPointsList& out) 
{
	try {
		decode();
	}
	catch ( ... ) {
		return; // no data..
	}
	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point)
    {
    		out.push_back(*point);
    }
}


void SpotDecoder::decode(bool check) 
{
    
	if ( check == false && !points_.empty() ) return;
	int date;
	
	if (!date_.empty()) {
		read(date_, date);	 
		if ( date <= 0) {
			MagDate d(0);
	 		d += date;
	 		date_ = tostring(d.yyyymmdd());	
		}
	}
	   
#ifdef MAGICS_EXCEPTION            
    try { 
		parameter_ = SimpleFactory<EpsParameter>::create(param_);       
    }
    catch (NoFactoryException& e) {
    	parameter_ = new EpsParameter(param_, param_title_, param_);
    }
#else
	parameter_ = SimpleFactory<EpsParameter>::create(param_);
	if ( !parameter_ )
	{
		// The data do not know how to verify the criter ....
		MagLog::warning() << "Eps Data access: parameter [" << param_ << "] unknown\n";
		return;
	}
#endif   
     parameter_->scaling(scaling_);
     parameter_->offset(offset_);
     parameter_->shift(shift_);
	 parameter_->type(type_);
	 parameter_->steps(steps_);
	 parameter_->correction(SpotDecoder::correction_);
	 parameter_->stationHeight(height_);
	 parameter_->percentile_ = percentile_;
	 parameter_->threshold_ = threshold_;
	 parameter_->prefix_ = prefix_;
    
	spot_query_result *result = parameter_->prepare(*this, points_);
	
	if ( !result ) {
		MagLog::error() << " No EpsData\n";
		date_ = "";
		time_ = "";
		return;
	}
	
    double lat, lon;
 	
    spot_get_location(result, &lat, &lon);

    spot_get_index_value(result, "mask", &mask_);
    
    if ( type_ == "epsrose" ) {
    	// The mask is strange Seems to be 1 for sea and 0 fr land! 
    	//MagLog::dev()<< "MASK--> " << mask_ << endl;
    	if ( mask_ == 0 ) mask_ = 1;
    	else mask_ = 0;
    }
    if ( lon > 180 ) 
    	lon -= 360.;

	grid_ = UserPoint(lon, lat);

	long resol; 
	
	read(get_spot(result,"numberOfPointsAlongAMeridian"), resol);
	spot_get_index_value(result, "bathymetry", &bathymetry_);

	ostringstream rs, dets;
	
    rs << "T" << resol-1;
    dets << "T" << (2*resol)-1;
    //dets << "resolution to be checked!";
    resolution_ = dets.str();
   
	date_ = get_spot(result,"date");
	time_ = get_spot(result,"time");
		
	MagLog::debug() << "Date /time ---> [" << date_ << ", " << time_ << "]\n";

  

    spot_delete_result(result);
    
    
    //spot_close_database(spot_);
    
//    delete spot_; 
//    delete spot_;
    spot_ = 0;

     
}



MatrixHandler& EpsgramDecoder::matrix()
{
	decode();
	vector<double> values;
	vector<double> steps;
	// Find the min and the max ..
	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point)
	{
		 MagLog::dev() << **point << endl;
		 MagLog::dev() << **point << endl;
		 for ( int s = 0; s != 50; s++) {		    
				string key = tostring(s);
			    map<string, double>::const_iterator member = (*point)->find(key);
				if ( member != (*point)->end() ) 
					values.push_back(member->second );
		 }

		 steps.push_back((**point)["step"]);
	}

	const double step = parameter_->plumesInterval();
	double from = maground(*min_element(values.begin(), values.end())) - step;
	double to = maground(*max_element(values.begin(), values.end())) + step ;

	IntervalMap<int> array;
	matrix_.set(array.size(), points_.size());
	for ( double a = from; a <= to; a = a + step ) {
		array.insert(make_pair(Interval(a-step, a+step), 0));
		matrix_.rowsAxis().push_back(a);

	}



	for (vector<double>::iterator s = steps.begin(); s != steps.end(); ++s) {				
		matrix_.columnsAxis().push_back(*s);
	}

	matrix_.setMapsAxis();
	int column = 0;

	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point)
	{

		for (IntervalMap<int>::iterator interval = array.begin(); interval!= array.end(); ++interval)
		{
			interval->second = 0;
		}

		for ( int s = 0; s != 50; s++)
		{
			string key = tostring(s);

			map<string, double>::const_iterator step = (*point)->find(key);
			if (step != (*point)->end() ) {

			  IntervalMap<int>::iterator interval = array.get(step->second);
			  interval->second++;
			}
		}
		int row = 0;

		for (IntervalMap<int>::iterator interval = array.begin(); interval!= array.end(); ++interval)
		{

			matrix_[column+row*steps.size()] = interval->second*2; // in percentage!
			row++;
		}
	    column++;
	}


	matrixHandlers_.push_back(new  MatrixHandler(matrix_));
	return *(matrixHandlers_.back());
}

spot_query* SpotDecoder::newQuery() const
{
	return newQuery(database_);
}

spot_query* SpotDecoder::newQuery(const string& database) const
{
	MagLog::dev() << "Accessing Database ->" << database << endl;

	if (! spot_) {
			
			spot_ = spot_open_database(database.c_str());
	}
	if ( !spot_ )
	{
		MagLog::error() << database << ": Not a valid Epsgram Database\n";
		throw MagicsException(database +": Not a valid Epsgram Database");
	}

	spot_query*  query  =  spot_new_query(spot_);
	spot_set_location (query, latitude_, longitude_);
	MagLog::dev()<< "lat---> " << latitude_ << "lon-->" << longitude_ << endl;
	spot_set_method(query,"EPSGRAM");

    //MagLog::dev()<< "resol---> " << resol << endl;
    //MagLog::dev() << "bathymetry---> " << bathymetry_ << endl;
    
 
     if (!date_.empty() ) {
     	MagLog::dev() << "Set the date --->" << date_ << "\n";
     	spot_set_parameter(query,"date", date_.c_str());
     }
   
     if (!time_.empty() ) {
     	MagLog::dev() << "Set the date --->" << time_ << "\n";
     	spot_set_parameter(query,"time", time_.c_str());
     }
     
	return query;
}

void EpsgramDecoder::set()
{
	 SpotDecoder::database_ = EpsgramDecoderAttributes::database_;
	 SpotDecoder::station_ = EpsgramDecoderAttributes::station_;
	 SpotDecoder::param_ = EpsgramDecoderAttributes::param_;
	 SpotDecoder::latitude_ = EpsgramDecoderAttributes::latitude_;
	 SpotDecoder::longitude_ = EpsgramDecoderAttributes::longitude_;
	 SpotDecoder::type_ = EpsgramDecoderAttributes::type_;
	 SpotDecoder::date_ = EpsgramDecoderAttributes::date_; 
	 SpotDecoder::time_ = EpsgramDecoderAttributes::time_;
	 SpotDecoder::height_ = EpsgramDecoderAttributes::height_;
	 SpotDecoder::correction_ = EpsgramDecoderAttributes::correction_;
	 SpotDecoder::param_title_ = EpsgramDecoderAttributes::param_title_;
	 SpotDecoder::percentile_ = EpsgramDecoderAttributes::percentile_;
	 SpotDecoder::threshold_ = EpsgramDecoderAttributes::threshold_;

	 scaling_ = param_scaling_factor_;
	 offset_ = param_offset_factor_;
	 shift_ = param_hour_shift_;
}

EfigramDecoder::EfigramDecoder() : first_(true)
{
}

EfigramDecoder::~EfigramDecoder()
{
}

void EfigramDecoder::decode()
{
	vector<double> values;
	if ( !first_ ) return;
	first_ = false;

	map<string, string> codes;
	
	codes["efi-precip"] = "143.128";
	codes["efi-2mt-max"] = "121.128";
	codes["efi-2mt-mean"] = "167.128";
	codes["efi-2mt-min"] = "122.128";
	codes["efi-wind-gust"] = "123.128";
	codes["efi-wind"] = "165.128";
    
	int prefix = 1; 
	string lastdate = "";
	string lasttime = "";
	int laststep = 0;

	SpotDecoder::param_ = EfigramDecoderAttributes::param_;
	SpotDecoder::latitude_ = EfigramDecoderAttributes::latitude_;
	SpotDecoder::longitude_ = EfigramDecoderAttributes::longitude_;       		 
	SpotDecoder::correction_ = false; // EfigramDecoderAttributes::correction_;
	legends_.push_back("Climate t+ [24-48h]");
	vector<int>::iterator step = EfigramDecoderAttributes::steps_.begin();

	for (vector<string>::iterator date = dates_.begin(); date != dates_.end(); ++date )
	{
		SpotDecoder::steps_.clear();
		SpotDecoder::steps_.push_back(*step);
		int legend_step = *step +12;
		SpotDecoder::database_ = EfigramDecoderAttributes::database_ + "/" + *date + "/epsdb";
		SpotDecoder::prefix_ = tostring(prefix) + "_"; 

		try {

			date_ = "";
			time_ = "";
			step_ = *step;
			SpotDecoder::decode(true);
			values.push_back(parameter_->minx_);
			values.push_back(parameter_->maxx_);

			lastdate = date_;
			lasttime = time_;
			laststep = *step;

			if ( EfigramDecoderAttributes::legend_ ==  false) 
    				continue;
			// Now the legend...	

			if ( legend_database_.empty() )
			{
    				legends_.push_back("Climate t+ [24-48h]");
    				return;
			}

			size_t len = 1024;
			char tmp[1024];

    			map<string, int> columns;
    			map<int, int> rows;
    			// build name of database!
    			string database = legend_database_ + "/" + *date + "/epsdb";;
    			MagLog::dev() << "Accessing Database for legend-->" << database << endl;
    			spot_query* query = newQuery(database);		
			spot_set_parameter(query,"param", codes[SpotDecoder::param_].c_str()); 
			spot_query_result* result = spot_handle_query(query);	

			// Get the columns names;
			for(int i = 0 ; i < spot_get_column_count(result); i++)
			{
    				spot_get_column_name(result, i,tmp,&len);
    				columns[tmp] = i;
    				len = 1024;
			}

			double values[1024];
			size_t nb = 1024;
			// get the rows names
			for(int i = 0 ; i < spot_get_row_count(result); i++)
			{
				spot_get_row_name(result, i,tmp,&len);   	
				int x = atoi(tmp);
				rows[x] = i;
				len = 1024;    	
			}	

			string date = get_spot(result,"date");
			string time = get_spot(result,"time");

			DateTime base(date, MagTime(time));

			ostringstream out;
			tm convert = base;	
			locale loc("");      
			out.imbue(loc);   
			const std::time_put<char>& tfac = use_facet<time_put<char> >(loc); 
			string format = "%e %b-%Hh";
			tfac.put(out, out, ' ', &convert, format.c_str(), format.c_str()+format.length()); 

			ostringstream legend;

			for (map<int, int>::const_iterator s = rows.begin(); s != rows.end(); ++s)
			{	
 				spot_get_row(result, s->second,values,&nb);	
				for (map<string, int>::const_iterator info = columns.begin(); info != columns.end(); ++info)
				{
					MagLog::dev() << "legend_step--> " << legend_step
					           << "\nefi read --->" <<  s->first << " = " << values[info->second] << endl;
					if ( s->first == legend_step )
					{						
						legend << "Eps t+ [" << legend_step - 24 << "-" << legend_step;			
						legend << "h] ";		
						efi_.push_back(int(maground(double(values[info->second]*100))));
						MagLog::dev() << "efi--->" <<  values[info->second] << " " << efi_.back() << endl;
					}
				}
			}
			legends_.push_back(legend.str());

			MagLog::dev() << "legend ---" <<  legends_.back() << endl;
			spot_delete_result(result);
			spot_delete_query(query);

			//spot_close_database(spot_);
			//	delete spot_;
			spot_ = 0;
			spot_ = 0;

			prefix++;
		}
		catch (...) {
			date_ = lastdate;
			time_ = lasttime;
			step_ = laststep;
		}
		++step;
	}
    

       //Now the Clim! 
	{
		SpotDecoder::steps_.clear();
		SpotDecoder::steps_.push_back(clim_step_);

		SpotDecoder::database_ = EfigramDecoderAttributes::clim_database_ + "/" + clim_date_ + "/epsdb";
		SpotDecoder::prefix_ = "clim_"; 
    
		try {
			date_ = "";
			time_ = "";
			SpotDecoder::decode(true);
			values.push_back(parameter_->minx_);
    					values.push_back(parameter_->maxx_);
		}
		catch (...) {
			MagLog::info() <<" No Clim" << endl;
		}
    	
		date_ = lastdate;
		time_ = lasttime;

		if (values.empty() )
		{
		    minx_ = 0;
		    maxx_ = 100;
		}
		else
		{
		    minx_ = *min_element(values.begin(), values.end());
		    maxx_ = *max_element(values.begin(), values.end());
		}
        }
}

void EfigramDecoder::set()
{
	
}

#include "Polyline.h"
#include "PaperPoint.h"



void EfigramDecoder::visit(LegendVisitor& legend)
{ 
    legend.columns_ = 5;
    for ( vector<string>::iterator entry = legends_.begin(); entry != legends_.end(); ++entry) {
        LegendEntry *step = new LegendEntry(*entry);
        legend.add(step);
    }
    for ( vector<string>::iterator entry = legends_.begin(); entry != legends_.end(); ++entry) {
        
        legend.add(new EmptyEntry());
        legend.add(new EmptyEntry());
    }    
    EfiEntry* efi = new EfiEntry(-9999);
    efi->style("title");
    efi->textColour(*legend.colour_);
    efi->textHeight(legend.getFont_size()); 
	legend.add(efi);
	
	if ( legend_colours_.empty() ) {
		legend_colours_.push_back("blue");
	}
    vector<string>::const_iterator colour = legend_colours_.begin();
    for ( unsigned int i = 0; i < efi_.size(); i++) {
        EfiEntry* efi = new EfiEntry(efi_[i]);
        if ( i == 0 ) {
            efi->style("top");
        }
        if ( i == efi_.size()-1 ) {
            efi->style("bottom");
        }
	    efi->textColour(*legend.colour_);
	    efi->textHeight(legend.getFont_size()); 
	    efi->colour(Colour(*colour));
	    efi->type(legend_type_);
	    efi->normalColour(*legend_normal_colour_);
	    efi->normalThickness(legend_normal_thickness_);
	    legend.add(efi);
	    vector<string>::const_iterator next = colour;
	    ++next;
	    if ( next != legend_colours_.end() ) 
	    	++colour;
	}
}

void EfigramDecoder::visit(TextVisitor& title)
{
	if ( !long_title_ && !title_) return;
	try {
		decode();
	}
	catch ( ... ) {
		return;
	}

	if ( long_title_ )
	{
		DateTime base(date_, MagTime(time_));
		DateTime from = base + Second((step_-12)*3600);
		DateTime to = base + Second((step_+12)*3600);
		MagLog::debug() << "HELOOOO-->Date/time ---> [" << date_ << ", " << time_ << ", " << step_ << "]\n";
		ostringstream out;
        
		tm cfrom = from;	
		tm cto = to;
		locale loc("");      
		out.imbue(loc);   
		out << "valid for 24 hours from ";
		const std::time_put<char>& tfac = use_facet<time_put<char> >(loc); 
		string format = "%A %e %B %Y %H UTC";
		tfac.put(out, out, ' ', &cfrom, format.c_str(), format.c_str()+format.length()); 
		out << " to ";
		tfac.put(out, out, ' ', &cto, format.c_str(), format.c_str()+format.length()); 

		ostringstream line;
		UserPoint position(SpotDecoder::longitude_, SpotDecoder::latitude_);

		line << "Forecast and M-Climate cumulative distribution functions with EFI values at " << position.asLatitude() << "/" << position.asLongitude() << endl;

		title.add(new TextEntry(line.str()));
		title.add(new TextEntry(out.str()));
		title.add(new TextEntry(""));
	}
	if (parameter_) 
		title.add(new TextEntry(parameter_->title()));
}


void EfigramDecoder::visit(MetaDataVisitor&)
{
}


void SpotDecoder::visit(Transformation& transformation)
{
	try {
		decode();
	}
	catch (...)
	{
		return;
	}
	ASSERT(parameter_);
	parameter_->setTransformation(transformation);
}


void EpsParameter::setTransformation(Transformation& transformation)
{
	
	if ( type_ == "eps15" ) {
		transformation.setDataMinMaxX(0, maxx_ + 18*3600, base_);
	}
	else if ( type_ == "epswave" ) {
		transformation.setDataMinMaxX(0, maxx_ + 6*3600, base_);
	}
	else if ( type_ == "epsrose" ) {
		transformation.setDataMinMaxX(0, maxx_ + 18*3600, base_);
	}
	else if ( type_ == "eps10" ) {
		transformation.setDataMinMaxX(0, maxx_ + 6*3600, base_);
	}
	else  {
	    transformation.setDataMinMaxX(0, maxx_ + 12*3600, base_);
    }
	
	transformation.setDataMinMaxY(miny_, maxy_);
}
    

void EfigramDecoder::visit(Transformation& transformation)
{	
	decode();
	transformation.setDataMinMaxX(minx_, maxx_);
}
