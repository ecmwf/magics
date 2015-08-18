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

/*! \file MagicsCalls.cc
    \brief Implementation of Fortran and C interface

 To use the C interface "magics_api.h" must be included.

 Changes: 13-06-2006 Added C interface (Stephan)

 \sa magics_api.h
*/

#include <MagLog.h>
#include <FortranMagics.h>
#include <WebFormat.h>
#include <MagicsParameter.h>

extern "C" {
#include <magics_api.h>
}



static FortranMagics* magics_ = 0;

/*! \defgroup compatibility Compatibility to MAGICS 6 for deprecated parameters

  The CompatibilityHelper allows the handling of deprecated parameters and set the
  equivalent new parameter are gives info/warning messages. The class is located in 
  file src/common/MagicsCalls.cc .
  
  To add a new parameter produce a new class which inherits from CompatibilityHelper.
  The produce a static object of that class: static 'class_name' 'para_name';

*/

/*! \brief Ensures backwards compability with Magics 6.x

  This base class checks if parameter are set which are not supported anymore 
  in Magics++ but need to be supported for backwards compability.
  
*/
class CompatibilityHelper 
{
public :
	CompatibilityHelper(const string& name) {
		compatibility_[name] = this;
	}
	CompatibilityHelper() {}
	
	void set(const string& name) {
		compatibility_[name] = this;
	} 

	virtual ~CompatibilityHelper() {}
	template <class P>
	static bool check(const string& param, P value) 
	{
		map<string, CompatibilityHelper*>::const_iterator tool = compatibility_.find(lowerCase(param));
		if ( tool == compatibility_.end() ) return false;
		else return (*(tool->second))(value);
	}
	static void reset(const string& param)  {
		map<string, CompatibilityHelper*>::const_iterator tool = compatibility_.find(lowerCase(param));
		if ( tool != compatibility_.end() )
			(*(tool->second)).reset();
	}
	static bool check(const string& param, const string& value) 
	{
		map<string, CompatibilityHelper*>::const_iterator tool = compatibility_.find(lowerCase(param));
		if ( tool == compatibility_.end() ) return false;
		else return (*(tool->second))(string(value));
	}
	virtual void reset()            		   { }
	virtual bool operator()(int)           { return false; }
	virtual bool operator()(const string&) { return false; }
	virtual bool operator()(double)        { return false; }
	virtual bool operator()(const doublearray&)        { return false; }
	virtual bool operator()(const stringarray&)        { return false; }
	virtual bool operator()(const intarray&)        { return false; }
	virtual bool operator()(bool)        { return false; }
protected:
	static map<string, CompatibilityHelper*> compatibility_;
};

class NoMoreGribex : public CompatibilityHelper
{
public :
	NoMoreGribex(const string& param) : CompatibilityHelper(param), parameter_(param) {}
	~NoMoreGribex() {}
	bool operator()(const string&)
	{
		MagLog::info() << "Compatibility issue: the parameter [" << parameter_ << "] is discontinued.\n"
		            << "              Please use the grib_api interface  instead." << std::endl;
		return true;
	}

protected :
	string parameter_;
};


class IgnoreConverter : public CompatibilityHelper
{
public :
	IgnoreConverter(const string& param) : CompatibilityHelper(param), parameter_(param) {}
	~IgnoreConverter() {}
	bool operator()(const string&)
	{
		MagLog::info() << "Deprecated: Parameter " << parameter_ << " is not needed anymore --> setting is ignored" << std::endl;
		return true;
	}

protected :
	string parameter_;
};


class ComingSoonConverter : public CompatibilityHelper
{
public :
	ComingSoonConverter(const string& param) : CompatibilityHelper(param), parameter_(param) {}
	~ComingSoonConverter() {}
	bool operator()(const string&)
	{
		MagLog::info() << "Coming soon: Parameter " << parameter_ << " will be implemented soon" << std::endl;
		return true;
	}

protected :
	string parameter_;
};

map<string, CompatibilityHelper*> CompatibilityHelper::compatibility_;


/*! \brief Prints info about old parameter
*/
class GribSubareaExtraction: public CompatibilityHelper {
public :
	GribSubareaExtraction() : CompatibilityHelper("grib_subarea_extraction") {}
	~GribSubareaExtraction() {}
	bool operator()(const string& )
	{
		MagLog::info() << "Compatibility issue: Parameter grib_subarea_extraction not required anymore.\n"<< std::endl;
		return true;
	}
};

/*! \brief Prints info about old parameter
*/
class GribFieldPosition: public CompatibilityHelper {
public :
	GribFieldPosition() : CompatibilityHelper("grib_field_position") {}
	~GribFieldPosition() {}
	bool operator()(int)
	{
		ASSERT(magics_);
		magics_->resetGrib();
		return false;
	}
};
class SimpleTranslator: public CompatibilityHelper {
public:
	SimpleTranslator(const string& from, const string& to, bool both=false) : CompatibilityHelper(from), from_(from), to_(to), both_(both) {}
	~SimpleTranslator() {}
	void deprecated() {
		MagLog::warning() << "Compatibility issue: Parameter " << from_ << " is deprecated : consider using " << to_ << " instead\n";
	}
	bool operator()(double val)
	{
		if ( both_ )
			ParameterManager::set(from_, val);
		else {
			deprecated();
		}
		if ( CompatibilityHelper::check(to_, val) ) return true;
		ParameterManager::set(to_, val);
		return true;
	}
	virtual void reset()
	{	if ( both_ )
			ParameterManager::reset(from_);
		else {
			deprecated();

		}
		ParameterManager::reset(to_);
	}
	virtual bool operator()(int val)
	{
		if ( both_ )
					ParameterManager::set(from_, val);
				else {
					deprecated();

				}
		ParameterManager::set(to_, val);
				return true;
	}
	virtual bool operator()(const string& val)
	{
		if ( both_ )
					ParameterManager::set(from_, val);
				else {
					deprecated();

				}
		ParameterManager::set(to_, val);
				return true;
	}

	virtual bool operator()(const doublearray& val) {
		if ( both_ )
					ParameterManager::set(from_, val);
				else {
					deprecated();

				}
		ParameterManager::set(to_, val);
				return true;
	}
	virtual bool operator()(const stringarray& val)    {
		if ( both_ )
					ParameterManager::set(from_, val);
				else {
					deprecated();

				}
		ParameterManager::set(to_, val);
				return true;
	}
	virtual bool operator()(bool val)      {
		if ( both_ )
					ParameterManager::set(from_, val);
				else {
					deprecated();

				}
		ParameterManager::set(to_, val);
				return true;
	}
	string from_;
	string to_;
	bool both_; // If both_ is true set the 2 parameters the old, and the new!
};

class ActionInterceptor: public CompatibilityHelper {
public:
	ActionInterceptor(const string& from, FortranMagics::Action action) : CompatibilityHelper(from), action_(action){}
	~ActionInterceptor() {}

	bool operator()(double val)
	{
		(magics_->*action_)();
		return false;
	}

	virtual bool operator()(int val)
	{
		(magics_->*action_)();
		return false;
	}
	virtual bool operator()(const string& val)
	{
		(magics_->*action_)();
		return false;
	}

	virtual bool operator()(const doublearray& val) {
		(magics_->*action_)();
		return false;
	}
	virtual bool operator()(const stringarray& val)    {
		(magics_->*action_)();
		return false;
	}
	virtual bool operator()(bool val)      {
		(magics_->*action_)();
		return false;
	}

	FortranMagics::Action action_;

};

/*! \brief Converts WIND_ARROW_LEGEND into LEGEND

  The convention to enable Legends for Wind in Magics++ has changed.
*/
class WindArrowLegend: public CompatibilityHelper {
public :
	WindArrowLegend() : CompatibilityHelper("wind_arrow_legend") {}
	~WindArrowLegend() {}
	bool operator()(const string& leg)
	{
		MagLog::info() << "Compatibility issue: wind_arrow_legend is deprecated.\n"
		            << "               Please use legend instead."<< std::endl;
		ParameterManager::set("legend", leg);
		return true;
	}
};

/*! \brief Converts WIND_ARROW_LEGEND into LEGEND

  The convention to enable Legends for Wind in Magics++ has changed.
*/
class CoastlinesResolution: public CompatibilityHelper {
public :
	CoastlinesResolution() : CompatibilityHelper("map_coastline_resolution")  { }
	~CoastlinesResolution() {}
	bool operator()(const string& res)
	{

		if ( magCompare(res, "high") || magCompare(res, "medium") ) {
			MagLog::warning() << "Magics is using a new dataset for coastlines naturalearth.\n"
		            << "             You can experience longer processing time and bigger output files \n"
					<< "             if you use medium or high on a large area.\n"
					<< "             if so, you may want to come back to low or automatic resolution.\n";
		}


		return false;
	}
};


/*! \brief Converts ps_file_name into output_file_root_name.

  The convention to describe filenames in Magics++ has changed.
*/
class PsFileName: public CompatibilityHelper {
public :
	PsFileName() : CompatibilityHelper("ps_file_name") {}
	~PsFileName() {}
	bool operator()(const string& file)
	{
		MagLog::info() << "Compatibility issue: ps_file_name is deprecated.\n"
		            << "               Please use output_name instead."<< std::endl;
		ParameterManager::set("output_legacy_name", file);
		ParameterManager::set("output_file_minimal_width", 0);  
		return true;
	}
};

/*! \brief Gives warning for using ps_device 
*/
class PsDevice: public CompatibilityHelper {
public :
	PsDevice() : CompatibilityHelper("ps_device") {}
	~PsDevice() {}
	bool operator()(const string& )
	{
		MagLog::info() << "Compatibility issue: ps_device was removed.\n"
		            << "               Please use other PostScript driver parameter instead."<< std::endl;
		return true;
	}
};

/*! \brief Converts ps_device into output_ps_device.
*/
class OutputPsDevice: public CompatibilityHelper {
public :
	OutputPsDevice() : CompatibilityHelper("output_ps_device") {}
	~OutputPsDevice() {}
	bool operator()(const string& )
	{
		MagLog::info() << "Compatibility issue: output_ps_device is deprecated."<< std::endl;
		return true;
	}
};

/*! \brief Removes ps_help
*/
class PsHelp: public CompatibilityHelper {
public :
	PsHelp() : CompatibilityHelper("ps_help") {}
	~PsHelp() {}
	bool operator()(const string& )
	{
		MagLog::info() << "Compatibility issue: Parameter ps_help was removed.\n"<< std::endl;
		return false;
	}
};

/*! \brief Removes ps_metric
*/
class PsMetric: public CompatibilityHelper {
public :
	PsMetric() : CompatibilityHelper("ps_metric") {}
	~PsMetric() {}
	bool operator()(const string& )
	{
		MagLog::info() << "Compatibility issue: Parameter ps_metric was removed.\n"<< std::endl;
		return false;
	}
};

/*! 
*/
class GraphType: public CompatibilityHelper {
public :
	GraphType() : CompatibilityHelper("graph_type") {}
	~GraphType() {}
	bool operator()(const string& type)
	{
		if ( magCompare(type, "curve") ) {
			ParameterManager::set("graph_shade", "off");
		}
		if ( magCompare(type, "bar") ) {
			ParameterManager::set("graph_shade", "on");
		}
		if ( magCompare(type, "area") ) {
			ParameterManager::set("graph_shade", "on");
		}
		return false;
	}
};

class GraphValuesConverter : public CompatibilityHelper
{
public:
	GraphValuesConverter(const string& from, const string & to) : 
		CompatibilityHelper(from), from_(from), to_(to) {}
	
	bool operator()(const doublearray& values)
	{
		MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
		            << "               Please use " << to_ << " instead."<< std::endl;
		ParameterManager::set(to_, values);  
		return true;
	}
	bool operator()(const stringarray& values)
	{
		MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
		            << "               Please use " << to_ << " instead."<< std::endl;
		ParameterManager::set(to_, values);  
		return true;
	}
	bool operator()(const string& value)
	{
		MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
		            << "               Please use " << to_ << " instead."<< std::endl;
		ParameterManager::set(to_, value);
		return true;
	}
	bool operator()(double value)
	{
		MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
		            << "               Please use " << to_ << " instead."<< std::endl;
		ParameterManager::set(to_, value);
		return true;
	}
	void reset() {
		ParameterManager::reset(to_);
	}
protected:
	string from_;
	string to_;
};

class ValuesConverter : public CompatibilityHelper
{
public:
	ValuesConverter(const string& from, const string & to, bool done=true) :
		CompatibilityHelper(from), from_(from), to_(to), done_(done) {}
	
	bool operator()(const doublearray& values)
	{
		
		ParameterManager::set(to_, values);  
		return done_;;
	}
	bool operator()(const stringarray& values)
	{
		
		ParameterManager::set(to_, values);  
		return done_;
	}
	bool operator()(const string& values)
		{

			ParameterManager::set(to_, values);
			return done_;
		}
protected:
	string from_;
	string to_;
	bool done_;
};



static GraphValuesConverter graph_curve_x_values("graph_curve_x_values", "x_values");
static GraphValuesConverter graph_curve_y_values("graph_curve_y_values", "y_values");
static GraphValuesConverter graph_curve_date_x_values("graph_curve_date_x_values", "x_date_values");
static GraphValuesConverter graph_curve_date_y_values("graph_curve_date_y_values", "y_date_values");



static GraphValuesConverter graph_curve_x_base_date("graph_curve_x_base_date", "x_base_date");
static GraphValuesConverter graph_curve_x_date_offset("graph_curve_x_date_offset", "x_date_offset");
static GraphValuesConverter graph_curve_y_base_date("graph_curve_y_base_date", "y_base_date");
static GraphValuesConverter graph_curve_y_date_offset("graph_curve_y_date_offset", "y_date_offset");

static GraphValuesConverter graph_curve_x2_base_date("graph_curve_x2_base_date", "x2_base_date");
static GraphValuesConverter graph_curve_x2_date_offset("graph_curve_x2_date_offset", "x2_date_offset");
static GraphValuesConverter graph_curve_y2_base_date("graph_curve_y2_base_date", "y2_base_date");
static GraphValuesConverter graph_curve_y2_date_offset("graph_curve_y2_date_offset", "y2_date_offset");

static GraphValuesConverter graph_curve2_x_values("graph_curve2_x_values", "x2_values");
static GraphValuesConverter graph_curve2_y_values("graph_curve2_y_values", "y2_values");
static GraphValuesConverter graph_curve2_date_x_values("graph_curve2_date_x_values", "x2_date_values");
static GraphValuesConverter graph_curve2_date_y_values("graph_curve2_date_y_values", "y2_date_values");

static  ValuesConverter symbol_input_text_list("symbol_input_text_list", "symbol_texts");
static  ValuesConverter contour_hilo_text_blanking("contour_hilo_text_blanking", "contour_hilo_blanking");
static GraphValuesConverter graph_bar_x_values("graph_bar_x_values", "x_values");
static GraphValuesConverter graph_bar_y_values("graph_bar_y_values", "y_values");
static GraphValuesConverter graph_bar_date_x_values("graph_bar_date_x_values", "x_date_values");
static GraphValuesConverter graph_bar_date_y_values("graph_bar_date_y_values", "y_date_values");

static GraphValuesConverter graph_bar_x_lower_values("graph_bar_x_lower_values", "x_lower_values");
static GraphValuesConverter graph_bar_x_upper_values("graph_bar_x_upper_values", "x_upper_values");
static GraphValuesConverter graph_bar_date_x_lower_values("graph_bar_date_x_lower_values", "x_lower_date_values");
static GraphValuesConverter graph_bar_date_x_upper_values("graph_bar_date_x_upper_values", "x_upper_date_values");

static GraphValuesConverter graph_bar_y_lower_values("graph_bar_y_lower_values", "y_lower_values");
static GraphValuesConverter graph_bar_y_upper_values("graph_bar_y_upper_values", "y_upper_values");
static GraphValuesConverter graph_bar_date_y_lower_values("graph_bar_date_y_lower_values", "y_lower_date_values");
static GraphValuesConverter graph_bar_date_y_upper_values("graph_bar_date_y_upper_values", "y_upper_date_values");


/*! \brief Converts device_file_name into output_file_root_name.

  The convention to describe filenames in Magics++ has changed.
*/
class DeviceFileName: public CompatibilityHelper {
public :
	DeviceFileName() : CompatibilityHelper("device_file_name") {}
	~DeviceFileName() {}
	bool operator()(const string& file)
	{
		MagLog::info() << "Compatibility issue: Parameter device_file_name is deprecated.\n"
		            << "               Please use output_name instead."<< std::endl;
		ParameterManager::set("output_legacy_name", file);
		ParameterManager::set("output_file_minimal_width", 0);  
		return true;
	}
};

/*! \brief Converts device_width into output_width.

  The convention to describe filenames in Magics++ has changed.
*/
class DeviceWidth: public CompatibilityHelper {
public :
	DeviceWidth() : CompatibilityHelper("device_width") {}
	~DeviceWidth() {}
	bool operator()(const int width)
	{
		MagLog::info() << "Compatibility issue: Parameter device_width is deprecated.\n"
		               << "             Please use output_width instead."<< std::endl;
		ParameterManager::set("output_width", width);
		return true;
	}
};

/*! \brief Converts device_ into output_width.

  The convention to describe filenames in Magics++ has changed.
*/
class DeviceQualityLevel: public CompatibilityHelper {
public :
	DeviceQualityLevel() : CompatibilityHelper("device_quality_level") {}
	~DeviceQualityLevel() {}
	bool operator()(const int quality)
	{
		MagLog::info() << "Compatibility issue: Parameter device_quality_level is deprecated.\n"
		               << "             Please use output_jpg_quality instead."<< std::endl;
		ParameterManager::set("output_jpg_quality", quality);
		return true;
	}
};

/*! \brief Converts text_quality into text_font and text_font_style.

*/
class TextQuality: public CompatibilityHelper {
public :
	TextQuality(const string& base = "") : CompatibilityHelper() {
		base_ = ( base.empty() ) ? "" : base + "_";
		set(base_ + "quality");
	}
	~TextQuality() {}
	bool operator()(const string& quality)
	{
		MagLog::info() << "Compatibility issue: Parameter " << base_ << "quality is deprecated.\n"
		            << "               Please use " << base_ << "font and " << base_ << "font_style instead."<< std::endl;

		if(magCompare(quality,"low") )
		{
			ParameterManager::set(base_ +"font","serif");
			ParameterManager::set(base_ +"font_style","normal");
		}
		else if(magCompare(quality,"medium") )
		{
			ParameterManager::set(base_ +"font", "sansserif");
			ParameterManager::set(base_ +"font_style","normal");
		}
		else if(magCompare(quality,"high") )
		{
			ParameterManager::set(base_ +"font","sansserif");
			ParameterManager::set(base_ +"font_style", "bold");
		}
		else
		{
			MagLog::warning()<<"The setting "<<quality<<" for the parameter " << base_ << "_quality is not valid! Default font is used."<< std::endl;
			ParameterManager::set(base_ +"font", "sansserif");
			ParameterManager::set(base_ + "font_style", "normal");
		}

		return true;
	}
protected:
	string base_;
};



class TextHeight: public CompatibilityHelper {
public :
	TextHeight(const string& from, const string& to) : CompatibilityHelper(from), from_(from), to_(to) {}
	~TextHeight() {}
	bool operator()(double height)
	{
		MagLog::warning() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
		               << "               Please use " << to_ << " instead. " << to_ << " has been set to "<<height<< std::endl;
		ParameterManager::set(to_, height);
		return true;
	}	
	
protected: 
	string from_;
	string to_;
};
class TextFontHeight: public CompatibilityHelper {
public :
	TextFontHeight(const string& from, const string& to) : CompatibilityHelper(from), from_(from), to_(to) {}
	~TextFontHeight() {}
	bool operator()(double height)
	{
		if (from_ != to_ ) {
			MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
		               << "               Please use " << to_ << " instead. " << to_ << " has been set to "<<height<< std::endl;
		}
		else {
			MagLog::info() << from_ << " is now expecting a string : consider to change your setting for psetc " << endl;
		}
		ParameterManager::set(to_, tostring(height));
		return true;
	}	
	bool operator()(const string& height)
		{
			
			ParameterManager::set(to_, height);
			return true;
		}	
	
protected: 
	string from_;
	string to_;
};
/*! \brief Converts gd_file_name into output_file_root_name.

  The convention to describe GD filenames in Magics++ has changed between version 1.1 and 1.2.
*/
class GdFileName: public CompatibilityHelper {
public :
	GdFileName() : CompatibilityHelper("gd_file_name") {}
	~GdFileName() {}
	bool operator()(const string& file)
	{
		MagLog::info() << "Compatibility issue: Parameter gd_file_name is deprecated.\n"
		            << "              Please use output_name instead."<< std::endl;
		ParameterManager::set("output_legacy_name", file);
		ParameterManager::set("output_file_minimal_width", 0); 
		return true;
	}
};

class SubpageMapProjectionNone: public CompatibilityHelper {
public :
	SubpageMapProjectionNone() : CompatibilityHelper("subpage_map_projection") {}
	~SubpageMapProjectionNone() {}
	bool operator()(const string& projection)
	{
		string fix = projection;
		if ( magCompare(projection, "none") )
		{
			fix="cartesian";
			MagLog::info() << "Compatibility issue: The value [none] for Parameter subpage_map_projection is deprecated.\n"
			               << "               Please use [cartesian] instead."<< std::endl;
		}
		ParameterManager::set("subpage_map_projection",fix);
		return true;
	}
};

/*! \brief 'device' is not a valid parameter anymore 

   set device to gd and gf_format
*/
class DeviceCompatibilityHelper: public CompatibilityHelper {
public :
	DeviceCompatibilityHelper() : CompatibilityHelper("device") {}
	~DeviceCompatibilityHelper() {}
	bool operator()(const string& device)
	{
		MagLog::info() << "Compatibility issue: the parameter device is deprecated.\n"
		               << "              Please use the parameter output_format instead!"<< endl;

		if( magCompare(device, "jpeg") || magCompare(device, "jpg") || magCompare(device, "png") || magCompare(device, "gif")) {
			ParameterManager::set("output_format", device);
			return true;
		}
		else
		{
			ParameterManager::set("output_format", device);
			return true;
		}
		return false;
	}
};

class Legend: public CompatibilityHelper {
public :
	Legend(const string& legend) : CompatibilityHelper(legend) {}
	~Legend() {}
	bool operator()(const string& legend)
	{
		MagTranslator<string, bool> translator;
		if ( translator(legend) == false ) {
			ParameterManager::set("legend", "off");
			return false;
		}
		MagLog::info() << "Compatibility issue: The legend is turned on!\n";	
		ParameterManager::set("legend", "on");
		// Act as a Action routine!...
		magics_->plegend();
		return false;
	}
};

class WindArrowIndexHead: public CompatibilityHelper {
public :
	WindArrowIndexHead() : CompatibilityHelper("wind_arrow_head_index") {}
	~WindArrowIndexHead() {}
	bool operator()(int index)
	{
	    MagLog::info() << "Compatibility issue: Parameter wind_arrow_index_head does not exist anymore.\n"
	                   << "            use wind_arrow_head_shape and wind_arrow_head_ratio instead." << endl;
	    const int head_index = (int)index/10;
	    const int ratio = index % 10;
	    double head_ratio;

	    if (ratio == 1)       head_ratio = 0.3;
	    else if (ratio == 2)  head_ratio = 0.6;
	    else if (ratio == 3)  head_ratio = 1.;
	    else if (ratio == 4)  head_ratio = 1.3;
	    else if (ratio == 5)  head_ratio = 1.6;
	    else if (ratio == 6)  head_ratio = 2.0;
	    else
	    {
	    	MagLog::warning() << "invalid ratio " << ratio << " revert to default 1." << endl;
	    	head_ratio = 1.;
	    }

	    MagLog::info() << "  wind_arrow_head_index set to " << head_index
	                   << "AND wind_arrow_head_ratio set to " << head_ratio << endl;

	    ParameterManager::set("wind_arrow_head_shape", head_index);
	    ParameterManager::set("wind_arrow_head_ratio", head_ratio);
	    return true;
	}
};

static WindArrowLegend windarrowlegend;
static PsFileName ps_file_name;
static GribSubareaExtraction grib_subarea_extraction;
static GribFieldPosition grib_field_position;
static GdFileName gd_file_name;
static PsDevice ps_device;
static OutputPsDevice output_ps_device;
static PsHelp ps_help;
static PsMetric ps_metric;
static DeviceCompatibilityHelper device;
static DeviceFileName device_file_name;
static DeviceQualityLevel device_quality_level;
static DeviceWidth device_width;
static TextQuality text_quality("text");
static TextQuality legend_text_quality("legend_text");
static TextQuality axis_tick_label_quality("axis_tick_label");
static TextQuality axis_Title_quality("axis_title");
static TextQuality page_id_quality("page_id_line");
static TextQuality contour_label_quality("contour_label") ;
static TextQuality map_label_quality("map_label_quality");
static WindArrowIndexHead wind_arrow_index_head;

static TextFontHeight text_reference_character_height("text_reference_character_height", "text_font_size");
static TextFontHeight text_height("text_font_size", "text_font_size");
static TextFontHeight legend_text_font_size("legend_text_font_size", "legend_text_font_size");



static SubpageMapProjectionNone subpage_map_projection_none;
static NoMoreGribex grib_mode("grib_mode");
static NoMoreGribex grib_product_block("grib_product_block");
static NoMoreGribex grib_grid_block("grib_grid_block");
static NoMoreGribex grib_output_field("grib_output_field");
static NoMoreGribex grib_input_type("grib_input_type");

static IgnoreConverter graph_position_mode("graph_position_mode");
static IgnoreConverter axis_date_units("axis_date_units");
static IgnoreConverter legend_entry_maximum_width("legend_entry_maximum_width");



static IgnoreConverter graph_curve_interpolation("graph_curve_interpolation");

static GraphType graph_type;
static Legend legend("legend");


static SimpleTranslator legend_text_height("legend_text_height", "legend_text_font_size");
static SimpleTranslator graph_shade_colour("graph_shade_colour", "graph_bar_colour", true);
static SimpleTranslator graph_bar_colour("graph_bar_colour", "graph_shade_colour", true);
static SimpleTranslator subpage_map_area_definition("subpage_map_area_definition", "subpage_map_area_definition_polar", true);
static SimpleTranslator wind_arrow_legend("wind_arrow_legend", "legend");
#ifdef MAGICS_ODB
static SimpleTranslator odb_latitude("odb_latitude", "odb_latitude_variable");
static SimpleTranslator odb_longitude("odb_longitude", "odb_longitude_variable");
static SimpleTranslator odb_y_component("odb_y_component", "odb_y_component_variable");
static SimpleTranslator odb_x_component("odb_x_component", "odb_x_component_variable");
static SimpleTranslator odb_x("odb_x", "odb_x_variable");
static SimpleTranslator odb_y("odb_y", "odb_y_variable");
static SimpleTranslator odb_value("odb_value", "odb_value_variable");
#endif

static SimpleTranslator netcdf_value_variable("netcdf_field_variable_name", "netcdf_value_variable");
static SimpleTranslator netcdf_latitude_variable("netcdf_latitude_variable_name", "netcdf_latitude_variable");
static SimpleTranslator netcdf_longitude_variable("netcdf_longitude_variable_name", "netcdf_longitude_variable");
static SimpleTranslator netcdf_x_component_variable("netcdf_x_component_variable_name", "netcdf_x_component_variable");
static SimpleTranslator netcdf_y_component_variable("netcdf_y_component_variable_name", "netcdf_y_component_variable");
static SimpleTranslator netcdf_x_variable("netcdf_x_variable_name", "netcdf_x_variable");
static SimpleTranslator netcdf_y_variable("netcdf_y_variable_name", "netcdf_y_variable");
static SimpleTranslator netcdf_x_position_variable("netcdf_x_position_variable_name", "netcdf_x_variable");
static SimpleTranslator netcdf_y_position_variable("netcdf_y_position_variable_name", "netcdf_y_variable");
static SimpleTranslator netcdf_x2_variable_name("netcdf_x2_variable_name", "netcdf_x_auxiliary_variable");
static SimpleTranslator graph_x_missing_value("graph_x_missing_value", "x_missing_value");
static SimpleTranslator graph_y_missing_value("graph_y_missing_value", "y_missing_value");
static SimpleTranslator symbol_marker("symbol_marker", "symbol_marker_index");


// Clean font parametermap_cities_font_
static SimpleTranslator map_cities_font_name("map_cities_font_name", "map_cities_font");
static SimpleTranslator eps_maximum_font_name("eps_maximum_font_name", "eps_maximum_font");
static SimpleTranslator magnifier_text_font_name("magnifier_text_font_name", "magnifier_text_font");
static SimpleTranslator symbol_text_font_name("symbol_text_font_name", "symbol_text_font");


extern "C" {

/* **************************************************************************

***
***  Fortran 77 interface
***

****************************************************************************/

void popen_()
{
	if (magics_ == 0) 
		magics_ = new FortranMagics();
	magics_->popen();
}

void pcoast_()
{
	magics_->pcoast();
}

void ptaylor_()
{
	magics_->ptaylor();

}
void ptephi_()
{
	magics_->ptephi();

}
void pgrib_()
{
	magics_->pgrib();
}

void pmapgen_()
{
	magics_->pmapgen();
}

void ptest_()
{
	magics_->ptest();
}


void podb_()
{
#ifdef MAGICS_ODB
	magics_->podb();
#else
	MagLog::warning() << "ODB support is NOT enabled!\n";
#endif
}


void pimport_()
{
	magics_->pimport();
}
void poverlay_()
{
	magics_->poverlay();
}
void pnetcdf_()
{
	magics_->pnetcdf();
}

void pcont_()
{
	magics_->pcont();
}

void pobs_()
{
	magics_->pobs();
}

void praw_()
{
#ifdef MAGICS_NETPBM
	MagLog::warning() << "praw->not implemented\n";  
#else
	MagLog::warning() << "Netpbm NOT supported!" << endl;
#endif    
}

void pimage_()
{
	magics_->pimage();
}

void pplot_()
{
	MagLog::warning() << "pplot has no effect ... use pimport instead" << endl;
}

void pnew_(const char* name, int length)
{
	std::string n(name, length);
	mag_new(n.c_str());
}

void ptext_()
{
    magics_->ptext();
}

void pwind_()
{
     magics_->pwind();
}


void pline_()
{
	magics_->pline();
}

void psymb_()
{
	magics_->psymb();
}

void pclose_()
{
	magics_->pclose();

	delete magics_;
	magics_ = 0;
}

void pact_(const char*, const char*, const char*, int, int, int)
{
	MagLog::dev() << "PACT will NOT be implemented!\n";    
}

void presets_() {

	ParameterManager::reset();
}

void preset_(const char* name, int length)
{
	std::string n(name, length);
	CompatibilityHelper::reset(n);
	mag_reset(n.c_str());
}

void psetc_(const char* name, const char* value, int namel, int valuel)
{
	try {        
		string val = string(value, valuel);
		string::size_type index = val.find_last_not_of(" ");    
		val = (index == string::npos) ? "" :  val.substr(0, index+1);
		if ( CompatibilityHelper::check(string(name, namel), val) ) return;
		ParameterManager::set(string(name, namel), val);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}

void pseti_(const char* name, const int* value, int namel)
{
	try {
		if ( CompatibilityHelper::check(string(name, namel), int(*value)) ) return;
		ParameterManager::set(string(name, namel), int(*value));
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}

void pset1i_(const char* name, const int* data, const int* dim, int length)
{
	std::string n(name, length);
	mag_set1i(n.c_str(), data, *dim);
}

void pset2i_(const char* name, const int* data, const int* dim, const int* dim2, int length)
{
	std::string n(name, length);
	mag_set2i(n.c_str(), data, *dim, *dim2);	    
}

void pset3i_(const char* name, const int* data, const int* dim, const int* dim2, const int* dim3, int length)
{
	std::string n(name, length);
	mag_set3i(n.c_str(), data, *dim, *dim2, *dim3);	     
}

void pset1c_(const char* name, const char* value, const int *dim, int namel, int l)
{
    stringarray values;
    string work(value, (*dim)*l);
    int first =0;
    for ( int i = 0; i < *dim; i++) {
    	
        // remove the  space at the end of the string
        string val = work.substr(first, l);
        
        string::size_type index = val.find_last_not_of(" ");
        if (index == string::npos)
        	 values.push_back("");
        else 
        	values.push_back(val.substr(0, index+1));        
        first += l;
    }
    
    try {
    	string n(name, namel);
    	if ( CompatibilityHelper::check(n, values) ) return;
        ParameterManager::set(n, values);
    }
    catch (MagicsException& e)
    {
        MagLog::error() << e << "\n";
    }
}


void penqi_(const char* name, int* value, int length)
{
	std::string n(name, length);
	mag_enqi(n.c_str(), value);
}


void penqc_(const char* name, char* value, int length, int vlength)
{
	
	std::string n(name, length);
	
	mag_enqc( n.c_str(), value);
	
	
	
	for (int i = strlen(value); i < vlength; i++)
		value[i]=' ';
	
}

void ppie_()
{
	MagLog::warning() << "ppie-> is deprecated and will NOT be implemented.\n";   
}


void pgraph_()
{
	magics_->pgraph();
}

void paxis_()
{
	 magics_->paxis();
}

void pgeo_()
{
	magics_->pgeo();
}
void pinput_()
{
	magics_->pinput();
}
void ptable_()
{
	magics_->ptable();
}



void peps_()
{
	MagLog::warning() << "peps-->not yet implemented\n";   
}


void pboxplot_()
{
	magics_->pboxplot();
}
void pwrepjson_()
{
	magics_->wrepjson();
}
void pgeojson_()
{
	magics_->geojson();
}
void pepsinput_()
{
	magics_->epsinput();
}
void pepscloud_()
{
	magics_->epscloud();
}
void pepsplumes_()
{
	magics_->epsplumes();
}

void pepsgraph_() {
	magics_->epsgraph();
}

void pepswave_() {
	magics_->epswave();
}

void pepswind_()
{
	magics_->epswind();
}

void pepsbar_()
{
	magics_->epsbar();
}

void pepsshading_()
{
	magics_->epsshading();
}

void pprint_()
{
	MagLog::warning() << "pprint-->not yet implemented\n";
}

void pinfo_(){mag_info();}


/* **************************************************************************

***
***  C interface  ( calling Fortran 90 interface above )
***

****************************************************************************/
void mag_open()  {popen_();}
void mag_close() {pclose_();}
void mag_coast() {pcoast_();}
void mag_grib()  {pgrib_();}
void mag_mapgen()  {pmapgen_();}
void mag_line()  {pline_();}
void mag_legend()  {magics_->simplelegend();}
void mag_test()  {ptest_();}
void mag_odb()   {podb_();}
void mag_import(){pimport_();}
void mag_overlay(){poverlay_();}
void mag_netcdf(){pnetcdf_();}
void mag_cont()  {pcont_();}
void mag_input()  {pinput_();}
void mag_table()  {ptable_();}
void mag_obs()   {pobs_();}
void mag_raw()   {praw_();}
void mag_image() {pimage_();}
void mag_plot()  {pplot_();}
void mag_text()	 {ptext_();}
void mag_wind()  {pwind_();}
void mag_symb()  {psymb_();}
void mag_boxplot()  {pboxplot_();}
void mag_taylor()  {ptaylor_();}
void mag_tephi()  {ptephi_();}
void mag_geojson()  { pgeojson_(); }
void mag_wrepjson()  { pwrepjson_(); }
void mag_epsinput()  { pepsinput_(); }
void mag_epscloud()  { pepscloud_(); }

void mag_epsgraph()  	   { pepsgraph_(); }
void mag_epswave()       { pepswave_(); }
void mag_epswind()       { pepswind_(); }
void mag_epsbar()        { pepsbar_(); }
void mag_epsshading()    { pepsshading_(); }
void mag_epsplumes()    { pepsplumes_(); }

void mag_new(const char* page)
{
	magics_->pnew(page);
}

void mag_reset(const char* name)
{
	try {
		ParameterManager::reset(name);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}	
}

void mag_setc(const char* name, const char* value)
{
	string n(name);
	string v(value);
	psetc_(n.c_str(), value, n.size(), v.size());
}

void mag_setr(const char* name, const double value)
{
	std::string n(name);

	try {
		if ( CompatibilityHelper::check(n, value) ) return;
		ParameterManager::set(n, value);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}

void mag_seti(const char* name, const int value)
{
	string n(name);
	pseti_(name, &value, n.size());
}

void mag_setp(const char* name, void* value)
{
	string n(name);
#ifdef MAGICS_CAIRO
    if ( magCompare(n, "output_cairo_drawing_context") ) {
       ParameterManager::set("output_cairo_drawing_context", (CairoPtr)value); 
    }
#endif
}

void mag_act(const char* a, const char* b, const char* c)
{
	string aa(a);
	string bb(b);
	string cc(c);
	pact_(a, b, c,aa.size(),bb.size(),cc.size());
}

void mag_set1r(const char* name, const double *data, const int dim1)
{
	std::string n(name);
	floatarray values;
	for ( int i = 0; i < dim1; i++) values.push_back(data[i]);        

	try {
		if ( CompatibilityHelper::check(n, values) ) return;
		ParameterManager::set(n, values);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}

void mag_set2r(const char* name, const double *data, const int dim1, const int dim2)
{
	string param(name);
	Matrix matrix;
	for (int i = 0; i < (dim2) * (dim1); i++) {
	  matrix.push_back(data[i]);
	}

	matrix.set(dim2, dim1);

	try {
		if ( CompatibilityHelper::check(param, matrix) ) return;
		ParameterManager::set(param, matrix);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
	MagLog::dev() << "Parameter " << string(name) << " set to " << matrix << "\n";
}

void mag_set3r(const char*, const double *, const int, const int, const int)
{
	MagLog::warning() << "pset3r --> not yet implemented\n";
}

void mag_set1i(const char* name, const int *data, const int dim1)
{
	std::string param(name);
	intarray values;
	for ( int i = 0; i < dim1; i++) values.push_back(data[i]);        

	try {
		if ( CompatibilityHelper::check(param, values) ) return;
		ParameterManager::set(param, values);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}

void mag_set2i(const char* name, const int *data, const int dim1, const int dim2)
{
	string param(name);
	Matrix matrix;
	for (int i = 0; i < (dim2) * (dim1); i++) matrix.push_back(data[i]);

	matrix.set(dim2, dim1);
	try {
			if ( CompatibilityHelper::check( param, matrix) ) return;
			ParameterManager::set(param, matrix);
		}
		catch (MagicsException& e)
		{
			MagLog::error() << e << "\n";
		}
	MagLog::dev() << "Parameter " << param << " set to " << matrix << "\n";
}

void mag_set3i(const char* , const int *, const int , const int , const int )
{
	MagLog::warning() << "pset3i --> not yet implemented\n";
}

void mag_set1c(const char* name, const char** data, const int dim)
{
	string param(name);

//	MagLog::dev() << "entry in the new mag_set1c\n";    
//	MagLog::dev() << "\tmag_set1c("  << dim << " entries);\n";
	stringarray values;

	for ( int i = 0; i < dim; i++)
	{
		string work (data[i]);
		// remove the space at the start and end of the string

		string::size_type index1 = work.find_first_not_of(" ");
		string::size_type index2 = work.find_last_not_of(" ");
		string val =  (index1 == string::npos || index2 == string::npos) ? "" : work.substr(index1, index2+1);
		values.push_back(val);        
	}
    
	try {
		if ( CompatibilityHelper::check(param, values) ) return;
		ParameterManager::set(param, values);
	}
	catch (MagicsException& e)
	{
		MagLog::error() << e << "\n";
	}
}

void mag_enqr(const char* fname, double *value)
{
	string name(fname);
	vector<string> special;
	special.push_back("subpage_x_position");
	special.push_back("subpage_y_position");
	special.push_back("subpage_x_length");
	special.push_back("subpage_y_length");
	// parameters needs magics to get reday! 

	 string projection;
	 
	 ParameterManager::get("subpage_map_projection", projection);

	for (vector<string>::iterator param = special.begin(); param != special.end(); ++param)  
		if (magCompare(name, *param) ) {
			double val;
			ParameterManager::get(name,val);
			if ( !magCompare(projection, "cartesian")  )  {
				magics_->prepare();
		 		name  = name + "_internal";	
			}
		}
	double magics;
	ParameterManager::get(name,magics);
	*value=magics;
	 MagLog::dev() << "mag_enqr->" << name << " = " << magics << endl;
}


void mag_enqi(const char* name, int *value)
{
	int magics;
	ParameterManager::get(string(name),magics);
	*value=magics;
}

void mag_enqc(const char* name, char* value)
{
	string magics;

	if (magCompare(string(name), "magics_version") ) {
		magics = getMagicsVersionString();
	}
	else
		ParameterManager::get(string(name),magics);

 	strcpy(value, magics.c_str() );
}


void mag_pie()   {ppie_();}
void mag_graph() {pgraph_();}
void mag_axis()  {paxis_();}
void mag_geo()   {pgeo_();}
void mag_eps()   {peps_();}
void mag_print() {pprint_();}

void mag_info()
{
	MagLog::userInfo() << "INFO:\n"
		<< "INFO: "<<getMagicsVersionString()<<"\n"
		<< "INFO:\n"
		<< "INFO: Machine: "<<getEnvVariable("HOSTNAME")<<" is running "<<getEnvVariable("VENDOR")<<" "<<getEnvVariable("OSTYPE")<<" "<<getEnvVariable("MACHTYPE")<<"\n"
		<< "INFO:\n"
		<< "INFO: $MAGPLUS_HOME    = "<<getEnvVariable("MAGPLUS_HOME")<<"\n"
		<< "INFO: $TMPDIR          = "<<getEnvVariable("TMPDIR")<<"\n"
		<< "INFO: $ODB_LIBS        = "<<getEnvVariable("ODB_LIBS")<<"\n"
		<< "INFO: $LD_LIBRARY_PATH = "<<getEnvVariable("LD_LIBRARY_PATH")<<"\n"
		<< "INFO:\n";
}

}// end of extern "C"

MagicsParameter<double> paxis_min_value("axis_min_value", 0);
MagicsParameter<double> paxis_max_value("axis_max_value", 100);
MagicsParameter<string> pgraph_axis_control("graph_axis_control", "off");
MagicsParameter<string> paxis_date_max_value("axis_date_max_value", "");
MagicsParameter<string> paxis_date_min_value("axis_date_min_value", "");

class AxisConverter : public CompatibilityHelper
{
public:
	AxisConverter(const string& from, const string& horiz, const string& vert) :
		CompatibilityHelper(from), from_(from), vertical_(vert), horizontal_(horiz) {}
	
	bool operator()(double val)
	{
		ParameterManager::set(from_, val);

		update(val);

		return false;
	}

	template <class T>
	void update(T val)
	{
		string orientation;
		ParameterManager::get("axis_orientation", orientation);
		

		if ( magCompare(orientation, "horizontal") )
		{
			ParameterManager::set(horizontal_, val);
		}
		else
		{
			ParameterManager::set(vertical_, val);

		}
	}

	void update(const string& val)
	{
			string orientation;
			ParameterManager::get("axis_orientation", orientation);


			if ( magCompare(orientation, "horizontal") )
			{
				if (magCompare(val, "position_list") ){
					MagLog::warning() << "position_list is now using the user coordinates system and not cm" << endl;
					MagLog::warning() << "please check your coordinates system" << endl;

				}
				else
					ParameterManager::set(horizontal_, val);
			}
			else
			{
				if (magCompare(val, "position_list") ){
					MagLog::warning() << "position_list is now using the user coordinates system and not cm" << endl;
					MagLog::warning() << "please check your coordinates system" << endl;

				}
				else
					ParameterManager::set(vertical_, val);

			}
		}

	bool operator()(const string& value)
	{
		ParameterManager::set(from_, value);
		update(value);
		return false;
	}
protected:
	string from_;
	string horizontal_;
	string vertical_;
};


static AxisConverter axis_type("axis_type", "subpage_x_axis_type", "subpage_y_axis_type");


static AxisConverter axis_min_value("axis_min_value", "subpage_x_min", "subpage_y_min");
static AxisConverter axis_max_value("axis_max_value", "subpage_x_max", "subpage_y_max");
static AxisConverter axis_min_date_value("axis_date_min_value", "subpage_x_date_min", "subpage_y_date_min");
static AxisConverter axis_max_date_value("axis_date_max_value", "subpage_x_date_max", "subpage_y_date_max");
static AxisConverter graph_axis_control("graph_axis_control", "subpage_x_automatic", "subpage_y_automatic");

void execute_magml(const char* file)
{
	WebInterpretor::magml(file);
}

void execute_json(const char* file)
{
	WebInterpretor::json(file);
}

void set_param(const char* param, const char* value)
{
	WebInterpretor::set(param, value);
}

static ActionInterceptor symbol_input_x_position("symbol_input_x_position", &FortranMagics::flagInputSymbol);
static ActionInterceptor symbol_input_y_position("symbol_input_y_position", &FortranMagics::flagInputSymbol);
static ActionInterceptor symbol_input_wind_direction("symbol_input_wind_direction", &FortranMagics::flagInputSymbol);
static ActionInterceptor symbol_input_wind_speed("symbol_input_wind_speed", &FortranMagics::flagInputSymbol);

static ActionInterceptor input_field("input_field", &FortranMagics::flagInputMatrix);
static ActionInterceptor input_simple_field("input_simple_field", &FortranMagics::flagInputMatrix);
static ActionInterceptor input_wind_u_component("input_wind_u_component", &FortranMagics::flagInputMatrix);
static ActionInterceptor input_wind_v_component("input_wind_v_component", &FortranMagics::flagInputMatrix);
static ActionInterceptor input_wind_speed("input_wind_speed", &FortranMagics::flagInputMatrix);
static ActionInterceptor input_wind_direction("input_wind_direction", &FortranMagics::flagInputMatrix);

static ActionInterceptor polyline_input_latitudes("polyline_input_latitudes", &FortranMagics::flagInputPoly);
static ActionInterceptor polyline_input_longitudes("polyline_input_longitudes", &FortranMagics::flagInputPoly);
static ActionInterceptor polyline_input_positions_filename("polyline_input_positions_filename", &FortranMagics::flagInputPoly);

class PageIDWarning : public CompatibilityHelper
{
public :
	PageIDWarning() : CompatibilityHelper("page_id_line_logo_plot"){}
	~PageIDWarning() {}
	bool operator()(const string& val)
	{
		if ( magCompare(val, "user" ) ){
			MagLog::warning() << "The value user for page_id_line_logo_plot is now deprecated.\n"
			                  << "               Please use pimport to add your own logo."<< endl;
			ParameterManager::set("page_id_line_logo_plot", "off");
		}
		else
			ParameterManager::set("page_id_line_logo_plot", val);
		return true;
	}
};
static PageIDWarning page_id_line_logo_plot;
static CoastlinesResolution map_coastline_resolution;
