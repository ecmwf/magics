/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
			MagLog::info() << "Magics is using the \'"<<res<<"\' dataset for coastlines from Natural Earth.\n"
			  << "        You can speed up your processing time by setting them to \'low\'.\n";
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

/*! \brief nforms users that output_resolution is dreprecated

  This parameter is not required anymore.
*/
class OutputResolution: public CompatibilityHelper {
public :
	OutputResolution() : CompatibilityHelper("output_resolution") {}
	~OutputResolution() {}
	bool operator()(int )
	{
		MagLog::info() << "Deprecated parameter: output_resolution is not used anymore.\n"
		            << "        Vector formats already used highes resolution and PNG uses 300 DPI."<< std::endl;
		return true;
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

static ValuesConverter symbol_input_text_list("symbol_input_text_list", "symbol_texts");
static ValuesConverter contour_hilo_text_blanking("contour_hilo_text_blanking", "contour_hilo_blanking");
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

class ContourAutomaticSetting: public CompatibilityHelper {
public :
	ContourAutomaticSetting() : CompatibilityHelper("contour_automatic_setting") {}
	~ContourAutomaticSetting() {}
	bool operator()(const string& setting)
	{
		//cout << " setting -->" << setting << endl;
		if ( magCompare(setting, "eccharts") ) {
			MagLog::info() << "Compatibility issue: ecchart automatic contour is deprecated, consider using ecmwf\n";
			return false;
		}
		if ( magCompare(setting, "web") ) {
			MagLog::warning() << "Compatibility issue: web automatic contour is now deprecated, use ecmwf instead\n";
			ParameterManager::set("contour_automatic_setting", "ecmwf");
			return true;
		}
		if ( magCompare(setting, "on") ) {
			MagLog::warning() << "Compatibility issue: on for  automatic contour is now deprecated, use ecmwf instead\n";
			ParameterManager::set("contour_automatic_setting", "ecmwf");
			return true;
		}
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

static ContourAutomaticSetting contourautomaticsetting;
static OutputResolution outputresolution;
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

static SimpleTranslator input_metadata("input_mars_metadata", "input_metadata");
static SimpleTranslator contour_predefined_setting("contour_predefined_setting", "contour_style_name");
static SimpleTranslator netcdf_x_position_variable("netcdf_x_position_variable", "netcdf_x_variable");
static SimpleTranslator netcdf_y_position_variable("netcdf_y_position_variable", "netcdf_y_variable");
static SimpleTranslator legend_text_height("legend_text_height", "legend_text_font_size");
static SimpleTranslator graph_shade_colour("graph_shade_colour", "graph_bar_colour", true);
static SimpleTranslator graph_bar_colour("graph_bar_colour", "graph_shade_colour", true);
static SimpleTranslator subpage_map_area_definition("subpage_map_area_definition", "subpage_map_area_definition_polar", true);
static SimpleTranslator wind_arrow_legend("wind_arrow_legend", "legend");
#ifdef HAVE_ODB
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
static SimpleTranslator netcdf_x_position_variable_name("netcdf_x_position_variable_name", "netcdf_x_variable");
static SimpleTranslator netcdf_y_position_variable_name("netcdf_y_position_variable_name", "netcdf_y_variable");
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

MAGICS_EXPORT void popen_()
{

	if (magics_ == 0)
		magics_ = new FortranMagics();
	magics_->popen();
}

MAGICS_EXPORT void pcoast_()
{
	magics_->pcoast();
}

MAGICS_EXPORT void ptaylor_()
{
	magics_->ptaylor();
}

MAGICS_EXPORT void ptephi_()
{
	magics_->ptephi();
}

MAGICS_EXPORT void pgrib_()
{
	magics_->pgrib();
}

MAGICS_EXPORT const char* metagrib_()
{
	return magics_->metagrib();
}

MAGICS_EXPORT const char* version()
{
	static string version = getMagicsVersionString();
	return version.c_str();
}

MAGICS_EXPORT const char* home()
{
	static string home = getEnvVariable("MAGPLUS_HOME");
	return home.c_str();
}

MAGICS_EXPORT const char* metanetcdf_()
{
	return magics_->metanetcdf();
}

MAGICS_EXPORT const char* metainput_()
{
	return magics_->metainput();
}

MAGICS_EXPORT void pmapgen_()
{
	magics_->pmapgen();
}

MAGICS_EXPORT void ptest_()
{
	magics_->ptest();
}

MAGICS_EXPORT void podb_()
{
#ifdef HAVE_ODB
	magics_->podb();
#else
	MagLog::warning() << "ODB support is NOT enabled!\n";
#endif
}

MAGICS_EXPORT void pimport_()
{
	magics_->pimport();
}

MAGICS_EXPORT void poverlay_()
{
	magics_->poverlay();
}

MAGICS_EXPORT void pnetcdf_()
{
	magics_->pnetcdf();
}

MAGICS_EXPORT void pcont_()
{
	magics_->pcont();
}

MAGICS_EXPORT void pobs_()
{
	magics_->pobs();
}

MAGICS_EXPORT void praw_()
{
#ifdef MAGICS_NETPBM
	MagLog::warning() << "praw->not implemented\n";
#else
	MagLog::warning() << "Netpbm NOT supported!" << endl;
#endif
}

MAGICS_EXPORT void pimage_()
{
	magics_->pimage();
}

MAGICS_EXPORT void pplot_()
{
	MagLog::warning() << "pplot has no effect ... use pimport instead" << endl;
}

MAGICS_EXPORT void pnew_(const char* name, int length)
{
	std::string n(name, length);
	mag_new(n.c_str());
}

MAGICS_EXPORT void ptext_()
{
    magics_->ptext();
}

MAGICS_EXPORT void pwind_()
{
     magics_->pwind();
}

MAGICS_EXPORT void pline_()
{
	magics_->pline();
}

MAGICS_EXPORT void psymb_()
{
	magics_->psymb();
}

MAGICS_EXPORT int pclose_()
{
	int code = magics_->pclose();

	delete magics_;
	magics_ = 0;
	return code;
}

MAGICS_EXPORT void pact_(const char*, const char*, const char*, int, int, int)
{
	MagLog::dev() << "PACT will NOT be implemented!\n";
}

MAGICS_EXPORT void presets_()
{
	ParameterManager::reset();
}

MAGICS_EXPORT void preset_(const char* name, int length)
{
	std::string n(name, length);
	CompatibilityHelper::reset(n);
	mag_reset(n.c_str());
}

MAGICS_EXPORT void psetc_(const char* name, const char* value, int namel, int valuel)
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

MAGICS_EXPORT void pseti_(const char* name, const int* value, int namel)
{
	try {
		if ( CompatibilityHelper::check(string(name, namel), int(*value)) ) return;
		ParameterManager::set(string(name, namel), int(*value));
	}
	catch (MagicsException& e)
	{
		(void)e; // prevent 'unreferenced local variable' compiler warning
		double fvalue = *value;
		mag_setr(name, fvalue);
	}
}

MAGICS_EXPORT void pset1i_(const char* name, const int* data, const int* dim, int length)
{
	std::string n(name, length);
	try {
		mag_set1i(n.c_str(), data, *dim);
	}
	catch (MagicsException& e)
	{
		(void)e;
		int s = *dim;
		double *fvalue = new double[s];
		for (int i =0; i < s; i++)
			fvalue[i] = data[i];
		mag_set1r(name, fvalue, *dim);
		delete[] fvalue;
	}
}

MAGICS_EXPORT void pset2i_(const char* name, const int* data, const int* dim1, const int* dim2, int length)
{
	std::string n(name, length);
	try {
		mag_set2i(n.c_str(), data, *dim1, *dim2);
	}

	catch (MagicsException& e)
	{
		(void)e;
		int dim = *dim1 * *dim2;
		double *fvalue = new double[dim];
		for (int i =0; i < dim; i++)
			fvalue[i] = data[i];
		mag_set2r(name, fvalue, *dim1, *dim2);
		delete[] fvalue;
	}

}

MAGICS_EXPORT void pset3i_(const char* name, const int* data, const int* dim, const int* dim2, const int* dim3, int length)
{
	std::string n(name, length);
	mag_set3i(n.c_str(), data, *dim, *dim2, *dim3);
}

MAGICS_EXPORT void pset1c_(const char* name, const char* value, const int *dim, int namel, int l)
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

MAGICS_EXPORT void penqi_(const char* name, int* value, int length)
{
	std::string n(name, length);
	mag_enqi(n.c_str(), value);
}

MAGICS_EXPORT void penqc_(const char* name, char* value, int length, int vlength)
{
	std::string n(name, length);
	mag_enqc( n.c_str(), value);

	for (int i = strlen(value); i < vlength; i++)
		value[i]=' ';
}

MAGICS_EXPORT void ppie_()
{
	MagLog::warning() << "ppie-> is deprecated and will NOT be implemented.\n";
}

MAGICS_EXPORT void pgraph_()
{
	magics_->pgraph();
}

MAGICS_EXPORT void paxis_()
{
	 magics_->paxis();
}

MAGICS_EXPORT void pgeo_()
{
	magics_->pgeo();
}

MAGICS_EXPORT void pinput_()
{
	magics_->pinput();
}

MAGICS_EXPORT void ptable_()
{
	magics_->ptable();
}

MAGICS_EXPORT void peps_()
{
	MagLog::warning() << "peps-->not yet implemented\n";
}

MAGICS_EXPORT void pboxplot_()
{
	magics_->pboxplot();
}

MAGICS_EXPORT void pwrepjson_()
{
	magics_->wrepjson();
}

MAGICS_EXPORT void pgeojson_()
{
	magics_->geojson();
}

MAGICS_EXPORT void pepsinput_()
{
	magics_->epsinput();
}

MAGICS_EXPORT void pmetgraph_()
{
	magics_->metgraph();
}

MAGICS_EXPORT void pmetbufr_()
{
	magics_->metbufr();
}

MAGICS_EXPORT void pepscloud_()
{
	magics_->epscloud();
}

MAGICS_EXPORT void pepsplumes_()
{
	magics_->epsplumes();
}

MAGICS_EXPORT void pepsgraph_() {
	magics_->epsgraph();
}

MAGICS_EXPORT void pepslight_() {
	magics_->epslight();
}

MAGICS_EXPORT void pepswave_() {
	magics_->epswave();
}

MAGICS_EXPORT void pepswind_()
{
	magics_->epswind();
}

MAGICS_EXPORT void pepsbar_()
{
	magics_->epsbar();
}

MAGICS_EXPORT void pepsshading_()
{
	magics_->epsshading();
}

MAGICS_EXPORT void pprint_()
{
	MagLog::warning() << "pprint-->not yet implemented\n";
}

MAGICS_EXPORT void pinfo_(){mag_info();}


/* **************************************************************************

***
***  C interface  ( calling Fortran 90 interface above )
***

****************************************************************************/

#define PYTHON(python, magics) MAGICS_EXPORT const char* python() { try { magics(); } catch (exception e) { return e.what(); } return NULL;}
#define PYTHONS(python, magics) MAGICS_EXPORT const char* python() { try { return magics(); } catch (exception e) { return e.what(); } return NULL;}
MAGICS_EXPORT void mag_open()  {popen_();}
PYTHON(py_open, popen_)
MAGICS_EXPORT int mag_close() { return pclose_();}
PYTHON(py_close, pclose_)
MAGICS_EXPORT void mag_coast() {pcoast_();}
PYTHON(py_coast, pcoast_)
MAGICS_EXPORT void mag_grib()  {pgrib_();}
PYTHON(py_grib, pgrib_)
PYTHONS(py_metagrib, metagrib_)
MAGICS_EXPORT void mag_mapgen()  {pmapgen_();}
PYTHON(py_mapgen, pmapgen_)
MAGICS_EXPORT void mag_line()  {pline_();}
PYTHON(py_line, pline_)
MAGICS_EXPORT void mag_legend()  {magics_->simplelegend();}
PYTHON(py_legend, magics_->simplelegend)
MAGICS_EXPORT void mag_test()  {ptest_();}
MAGICS_EXPORT void mag_odb()   {podb_();}
PYTHON(py_odb, podb_)
MAGICS_EXPORT void mag_import(){pimport_();}
PYTHON(py_import, pimport_)
MAGICS_EXPORT void mag_overlay(){poverlay_();}
PYTHON(py_overlay, poverlay_)
MAGICS_EXPORT void mag_netcdf(){pnetcdf_();}
PYTHON(py_netcdf, pnetcdf_)
PYTHONS(py_metanetcdf, metanetcdf_)
PYTHONS(py_metainput, metainput_)
MAGICS_EXPORT void mag_cont()  {pcont_();}
PYTHON(py_cont, pcont_)
MAGICS_EXPORT void mag_input()  {pinput_();}
PYTHON(py_input, pinput_)
MAGICS_EXPORT void mag_table()  {ptable_();}
PYTHON(py_table, ptable_)
MAGICS_EXPORT void mag_obs()   {pobs_();}
PYTHON(py_obs, pobs_)
MAGICS_EXPORT void mag_raw()   {praw_();}
PYTHON(py_raw, praw_)
MAGICS_EXPORT void mag_image() {pimage_();}
PYTHON(py_image, pimage_)
MAGICS_EXPORT void mag_plot()  {pplot_();}
PYTHON(py_plot, pplot_)
MAGICS_EXPORT void mag_text()	 {ptext_();}
PYTHON(py_text, ptext_)
MAGICS_EXPORT void mag_wind()  {pwind_();}
PYTHON(py_wind, pwind_)
MAGICS_EXPORT void mag_symb()  {psymb_();}
PYTHON(py_symb, psymb_)
MAGICS_EXPORT void mag_boxplot()  {pboxplot_();}
PYTHON(py_boxplot, pboxplot_)
MAGICS_EXPORT void mag_taylor()  {ptaylor_();}
PYTHON(py_taylor, ptaylor_)
MAGICS_EXPORT void mag_tephi()  {ptephi_();}
PYTHON(py_tephi, ptephi_)
MAGICS_EXPORT void mag_geojson()  { pgeojson_(); }
PYTHON(py_geojson, pgeojson_)
MAGICS_EXPORT void mag_wrepjson()  { pwrepjson_(); }
PYTHON(py_wrepjson, pwrepjson_)
MAGICS_EXPORT void mag_epsinput()  { pepsinput_(); }
PYTHON(py_epsinput, pepsinput_)
MAGICS_EXPORT void mag_epscloud()  { pepscloud_(); }
PYTHON(py_epscloud, pepscloud_)
MAGICS_EXPORT void mag_metgraph()  { pmetgraph_(); }
PYTHON(py_metgraph, pmetgraph_)
MAGICS_EXPORT void mag_metbufr()  { pmetbufr_(); }
PYTHON(py_metbufr, pmetbufr_)

MAGICS_EXPORT void mag_epsgraph()  	   { pepsgraph_(); }
PYTHON(py_epsgraph, pepsgraph_)

MAGICS_EXPORT void mag_epswave()       { pepswave_(); }
PYTHON(py_epswave, pepswave_)

MAGICS_EXPORT void mag_epswind()       { pepswind_(); }
PYTHON(py_epswind, pepswind_)

MAGICS_EXPORT void mag_epsbar()        { pepsbar_(); }
PYTHON(py_epsbar, pepsbar_)

MAGICS_EXPORT void mag_epsshading()    { pepsshading_(); }
PYTHON(py_epsshading, pepsshading_)

MAGICS_EXPORT void mag_epsplumes()    { pepsplumes_(); }
PYTHON(py_epsplumes, pepsplumes_)

MAGICS_EXPORT void mag_epslight()    { pepslight_(); }
PYTHON(py_epslight, pepslight_)

MAGICS_EXPORT const char* py_new(const char* page)
{
	try {
		mag_new(page);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_new(const char* page)
{
	magics_->pnew(page);
}

MAGICS_EXPORT const char* py_reset(const char* name)
{
	try {
		mag_reset(name);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_reset(const char* name)
{

	ParameterManager::reset(name);
}

MAGICS_EXPORT const char* py_setc(const char* name, const char* value)
{
	try {
		mag_setc(name, value);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_setc(const char* name, const char* value)
{
	string n(name);
	string v(value);
	psetc_(n.c_str(), value, n.size(), v.size());
	//cout << "setc("<<name<<","<<value<<")"<<endl;
}

MAGICS_EXPORT const char* py_setr(const char* name, const double value)
{
	try {
		mag_setr(name, value);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_setr(const char* name, const double value)
{
	std::string n(name);

	if ( CompatibilityHelper::check(n, value) ) return;
	ParameterManager::set(n, value);
}

MAGICS_EXPORT const char* py_seti(const char* name, const int value)
{
	try {
		mag_seti(name, value);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_seti(const char* name, const int value)
{
	string n(name);
	pseti_(name, &value, n.size());
	//cout << "seti("<<name<<","<<value<<")"<<endl;
}

MAGICS_EXPORT void mag_setp(const char* name, void* value)
{
#ifdef HAVE_CAIRO
    string n(name);
    if ( magCompare(n, "output_cairo_drawing_context") ) {
       ParameterManager::set("output_cairo_drawing_context", (CairoPtr)value);
    }
#endif
}

MAGICS_EXPORT void mag_act(const char* a, const char* b, const char* c)
{
	string aa(a);
	string bb(b);
	string cc(c);
	pact_(a, b, c,aa.size(),bb.size(),cc.size());
}

MAGICS_EXPORT const char* py_set1r(const char* name, const double *data, const int dim1)
{
	try {
		mag_set1r(name, data, dim1);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_set1r(const char* name, const double *data, const int dim1)
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

MAGICS_EXPORT const char* py_set2r(const char* name, const double *data, const int dim1, const int dim2)
{
	try {
		mag_set2r(name, data, dim1, dim2);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_set2r(const char* name, const double *data, const int dim1, const int dim2)
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

MAGICS_EXPORT void mag_set3r(const char*, const double *, const int, const int, const int)
{
	MagLog::warning() << "pset3r --> not yet implemented\n";
}

MAGICS_EXPORT const char* py_set1i(const char* name, const int *data, const int dim1)
{
	try {
		mag_set1i(name, data, dim1);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_set1i(const char* name, const int *data, const int dim1)
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

MAGICS_EXPORT const char* py_set2i(const char* name, const int *data, const int dim1, const int dim2)
{
	try {
		mag_set2i(name, data, dim1, dim2);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_set2i(const char* name, const int *data, const int dim1, const int dim2)
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

MAGICS_EXPORT void mag_set3i(const char* , const int *, const int , const int , const int )
{
	MagLog::warning() << "pset3i --> not yet implemented\n";
}

MAGICS_EXPORT const char* py_set1c(const char* name, const char** data, const int dim1)
{
	try {
		mag_set1c(name, data, dim1);
	}
	catch (exception e) {
		return e.what();
	}
	return NULL;
}

MAGICS_EXPORT void mag_set1c(const char* name, const char** data, const int dim)
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
	//cout << "set1c("<<name<<","<<data[0]<<","<<dim<<")"<<endl;
}

MAGICS_EXPORT void mag_enqr(const char* fname, double *value)
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

	for (vector<string>::iterator param = special.begin(); param != special.end(); ++param) {
		if (magCompare(name, *param) ) {
			double val;
			ParameterManager::get(name,val);
			if ( !magCompare(projection, "cartesian")  )  {
				magics_->prepare();
		 		name  = name + "_internal";
			}
		}
	}
	double magics;
	ParameterManager::get(name,magics);
	*value=magics;
	 MagLog::dev() << "mag_enqr->" << name << " = " << magics << endl;
}


MAGICS_EXPORT void mag_enqi(const char* name, int *value)
{
	int magics;
	ParameterManager::get(string(name),magics);
	*value=magics;
}

MAGICS_EXPORT void mag_enqc(const char* name, char* value)
{
	string magics;

	if (magCompare(string(name), "magics_version") ) {
		magics = getMagicsVersionString();
	}
	else
		ParameterManager::get(string(name),magics);

 	strcpy(value, magics.c_str() );
}


MAGICS_EXPORT void mag_pie()   {ppie_();}

MAGICS_EXPORT void mag_graph() {pgraph_();}
PYTHON(py_graph, pgraph_)
MAGICS_EXPORT void mag_axis()  {paxis_();}
PYTHON(py_axis, paxis_)
MAGICS_EXPORT void mag_geo()   {pgeo_();}
PYTHON(py_geo, pgeo_)
MAGICS_EXPORT void mag_eps()   {peps_();}
PYTHON(py_eps, peps_)
MAGICS_EXPORT void mag_print() {pprint_();}
PYTHON(py_print, pprint_)

MAGICS_EXPORT void mag_info()
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

PYTHON(py_info, mag_info)
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

		if (magCompare(val, "position_list") ){
			MagLog::info() << "position_list is now using the user coordinates system and not cm" << endl;
			return;
		}

		if ( magCompare(orientation, "horizontal") )
		{
			ParameterManager::set(horizontal_, val);
		}
		else
		{
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
