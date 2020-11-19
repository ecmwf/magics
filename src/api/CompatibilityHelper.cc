/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsCalls.cc
    \brief Implementation of Fortran and C interface

 To use the C interface "magics_api.h" must be included.

 Changes: 13-06-2006 Added C interface (Stephan)

 \sa magics_api.h
*/

#include "CompatibilityHelper.h"
#include "MagException.h"
#include "MagLog.h"
#include "MagicsSettings.h"

/*! \defgroup compatibility Compatibility to MAGICS 6 for deprecated parameters

  The CompatibilityHelper allows the handling of deprecated parameters and set
  the equivalent new parameter are gives info/warning messages. The class is
  located in file src/common/MagicsCalls.cc .

  To add a new parameter produce a new class which inherits from
  CompatibilityHelper. The produce a static object of that class: static
  'class_name' 'para_name';

*/

#include "FortranMagics.h"
#include "MagicsParameter.h"
#include "WebFormat.h"


namespace magics {

void CompatibilityHelper::resetAll() {
    for (auto j = compatibility_.begin(); j != compatibility_.end(); ++j) {
        (*j).second->reset();
    }
}

class NoMoreGribex : public CompatibilityHelper {
public:
    NoMoreGribex(const string& param) : CompatibilityHelper(param), parameter_(param) {}
    ~NoMoreGribex() {}
    bool operator()(const string&) {
        MagLog::info() << "Compatibility issue: the parameter [" << parameter_ << "] is discontinued.\n"
                       << "              Please use the grib_api interface  instead." << std::endl;
        return true;
    }

protected:
    string parameter_;
};

class IgnoreConverter : public CompatibilityHelper {
public:
    IgnoreConverter(const string& param) : CompatibilityHelper(param), parameter_(param) {}
    ~IgnoreConverter() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Deprecated: parameter '" + parameter_ + "'");
        }
        MagLog::info() << "Deprecated: Parameter " << parameter_ << " is not needed anymore --> setting is ignored"
                       << std::endl;
        return true;
    }

protected:
    string parameter_;
};

class ComingSoonConverter : public CompatibilityHelper {
public:
    ComingSoonConverter(const string& param) : CompatibilityHelper(param), parameter_(param) {}
    ~ComingSoonConverter() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + parameter_ + "' not yet implemented");
        }
        MagLog::info() << "Coming soon: Parameter " << parameter_ << " will be implemented soon" << std::endl;
        return true;
    }

protected:
    string parameter_;
};

map<string, CompatibilityHelper*> CompatibilityHelper::compatibility_;

/*! \brief Prints info about old parameter
 */
class GribSubareaExtraction : public CompatibilityHelper {
public:
    GribSubareaExtraction() : CompatibilityHelper("grib_subarea_extraction") {}
    ~GribSubareaExtraction() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'grib_subarea_extraction' not required anymore");
        }
        MagLog::info() << "Compatibility issue: Parameter grib_subarea_extraction "
                          "not required anymore.\n"
                       << std::endl;
        return true;
    }
};

/*! \brief Prints info about old parameter
 */
class GribFieldPosition : public CompatibilityHelper {
public:
    GribFieldPosition() : CompatibilityHelper("grib_field_position") {}
    ~GribFieldPosition() {}
    bool operator()(int) {
        FortranMagics::instance().resetGrib();
        return false;
    }
};
class SimpleTranslator : public CompatibilityHelper {
public:
    SimpleTranslator(const string& from, const string& to, bool both = false) :
        CompatibilityHelper(from), from_(from), to_(to), both_(both) {}
    ~SimpleTranslator() {}
    void deprecated() {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
        }
        MagLog::warning() << "Compatibility issue: Parameter " << from_ << " is deprecated : consider using " << to_
                          << " instead\n";
    }
    bool operator()(double val) {
        if (both_)
            ParameterManager::set(from_, val);
        else {
            deprecated();
        }
        if (CompatibilityHelper::check(to_, val))
            return true;
        ParameterManager::set(to_, val);
        return true;
    }
    virtual void reset() {
        if (both_)
            ParameterManager::reset(from_);
        else {
            // deprecated();
        }
        ParameterManager::reset(to_);
    }
    virtual bool operator()(int val) {
        if (both_)
            ParameterManager::set(from_, val);
        else {
            deprecated();
        }
        ParameterManager::set(to_, val);
        return true;
    }
    virtual bool operator()(const string& val) {
        if (both_)
            ParameterManager::set(from_, val);
        else {
            deprecated();
        }
        ParameterManager::set(to_, val);
        return true;
    }

    virtual bool operator()(const doublearray& val) {
        if (both_)
            ParameterManager::set(from_, val);
        else {
            deprecated();
        }
        ParameterManager::set(to_, val);
        return true;
    }
    virtual bool operator()(const stringarray& val) {
        if (both_)
            ParameterManager::set(from_, val);
        else {
            deprecated();
        }
        ParameterManager::set(to_, val);
        return true;
    }
    virtual bool operator()(bool val) {
        if (both_)
            ParameterManager::set(from_, val);
        else {
            deprecated();
        }
        ParameterManager::set(to_, val);
        return true;
    }
    string from_;
    string to_;
    bool both_;  // If both_ is true set the 2 parameters the old, and the new!
};

class ActionInterceptor : public CompatibilityHelper {
public:
    ActionInterceptor(const string& from, FortranMagics::Action action) : CompatibilityHelper(from), action_(action) {}
    ~ActionInterceptor() {}

    bool operator()(double val) {
        (FortranMagics::instance().*action_)();
        return false;
    }

    virtual bool operator()(int val) {
        (FortranMagics::instance().*action_)();
        return false;
    }
    virtual bool operator()(const string& val) {
        (FortranMagics::instance().*action_)();
        return false;
    }

    virtual bool operator()(const doublearray& val) {
        (FortranMagics::instance().*action_)();
        return false;
    }
    virtual bool operator()(const stringarray& val) {
        (FortranMagics::instance().*action_)();
        return false;
    }
    virtual bool operator()(bool val) {
        (FortranMagics::instance().*action_)();
        return false;
    }

    FortranMagics::Action action_;
};

/*! \brief Converts WIND_ARROW_LEGEND into LEGEND

  The convention to enable Legends for Wind in Magics++ has changed.
*/
class WindArrowLegend : public CompatibilityHelper {
public:
    WindArrowLegend() : CompatibilityHelper("wind_arrow_legend") {}
    ~WindArrowLegend() {}
    bool operator()(const string& leg) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'wind_arrow_legend' is deprecated. Please use 'legend'");
        }

        MagLog::info() << "Compatibility issue: wind_arrow_legend is deprecated.\n"
                       << "               Please use legend instead." << std::endl;
        ParameterManager::set("legend", leg);
        return true;
    }
};

/*! \brief Converts WIND_ARROW_LEGEND into LEGEND

  The convention to enable Legends for Wind in Magics++ has changed.
*/
class CoastlinesResolution : public CompatibilityHelper {
public:
    CoastlinesResolution() : CompatibilityHelper("map_coastline_resolution") {}
    ~CoastlinesResolution() {}
    bool operator()(const string& res) {
        if (magCompare(res, "high") || magCompare(res, "medium")) {
            MagLog::info() << "Magics is using the \'" << res << "\' dataset for coastlines from Natural Earth.\n"
                           << "        You can speed up your processing time by "
                              "setting them to \'low\'.\n";
        }

        return false;
    }
};

/*! \brief Converts ps_file_name into output_file_root_name.

  The convention to describe filenames in Magics++ has changed.
*/
class PsFileName : public CompatibilityHelper {
public:
    PsFileName() : CompatibilityHelper("ps_file_name") {}
    ~PsFileName() {}
    bool operator()(const string& file) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'ps_file_name' is deprecated. Please use 'output_name'");
        }

        MagLog::info() << "Compatibility issue: ps_file_name is deprecated.\n"
                       << "               Please use output_name instead." << std::endl;
        ParameterManager::set("output_legacy_name", file);
        ParameterManager::set("output_file_minimal_width", 0);
        return true;
    }
};

/*! \brief Gives warning for using ps_device
 */
class PsDevice : public CompatibilityHelper {
public:
    PsDevice() : CompatibilityHelper("ps_device") {}
    ~PsDevice() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'ps_device' is deprecated'");
        }

        MagLog::info() << "Compatibility issue: ps_device was removed.\n"
                       << "               Please use other PostScript driver "
                          "parameter instead."
                       << std::endl;
        return true;
    }
};

/*! \brief Converts ps_device into output_ps_device.
 */
class OutputPsDevice : public CompatibilityHelper {
public:
    OutputPsDevice() : CompatibilityHelper("output_ps_device") {}
    ~OutputPsDevice() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'output_ps_device' is deprecated'");
        }
        MagLog::info() << "Compatibility issue: output_ps_device is deprecated." << std::endl;
        return true;
    }
};

/*! \brief Removes ps_help
 */
class PsHelp : public CompatibilityHelper {
public:
    PsHelp() : CompatibilityHelper("ps_help") {}
    ~PsHelp() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'ps_help' is deprecated'");
        }
        MagLog::info() << "Compatibility issue: Parameter ps_help was removed.\n" << std::endl;
        return false;
    }
};

/*! \brief Removes ps_metric
 */
class PsMetric : public CompatibilityHelper {
public:
    PsMetric() : CompatibilityHelper("ps_metric") {}
    ~PsMetric() {}
    bool operator()(const string&) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'ps_metric' is deprecated'");
        }
        MagLog::info() << "Compatibility issue: Parameter ps_metric was removed.\n" << std::endl;
        return false;
    }
};

/*!
 */
class GraphType : public CompatibilityHelper {
public:
    GraphType() : CompatibilityHelper("graph_type") {}
    ~GraphType() {}
    bool operator()(const string& type) {
        if (magCompare(type, "curve")) {
            ParameterManager::set("graph_shade", "off");
        }
        if (magCompare(type, "bar")) {
            ParameterManager::set("graph_shade", "on");
        }
        if (magCompare(type, "area")) {
            ParameterManager::set("graph_shade", "on");
        }
        return false;
    }
};

/*! \brief nforms users that output_resolution is dreprecated

  This parameter is not required anymore.
*/
class OutputResolution : public CompatibilityHelper {
public:
    OutputResolution() : CompatibilityHelper("output_resolution") {}
    ~OutputResolution() {}
    bool operator()(int) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'output_resolution' is deprecated'");
        }
        MagLog::info() << "Deprecated parameter: output_resolution is not used anymore.\n"
                       << "        Vector formats already used highes resolution and PNG uses "
                          "300 DPI."
                       << std::endl;
        return true;
    }
};

class GraphValuesConverter : public CompatibilityHelper {
public:
    GraphValuesConverter(const string& from, const string& to) : CompatibilityHelper(from), from_(from), to_(to) {}

    bool operator()(const doublearray& values) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
        }
        MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
                       << "               Please use " << to_ << " instead." << std::endl;
        ParameterManager::set(to_, values);
        return true;
    }
    bool operator()(const stringarray& values) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
        }
        MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
                       << "               Please use " << to_ << " instead." << std::endl;
        ParameterManager::set(to_, values);
        return true;
    }
    bool operator()(const string& value) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
        }
        MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
                       << "               Please use " << to_ << " instead." << std::endl;
        ParameterManager::set(to_, value);
        return true;
    }
    bool operator()(double value) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
        }
        MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
                       << "               Please use " << to_ << " instead." << std::endl;
        ParameterManager::set(to_, value);
        return true;
    }
    void reset() { ParameterManager::reset(to_); }

protected:
    string from_;
    string to_;
};

class ValuesConverter : public CompatibilityHelper {
public:
    ValuesConverter(const string& from, const string& to, bool done = true) :
        CompatibilityHelper(from), from_(from), to_(to), done_(done) {}

    bool operator()(const doublearray& values) {
        ParameterManager::set(to_, values);
        return done_;
        ;
    }
    bool operator()(const stringarray& values) {
        ParameterManager::set(to_, values);
        return done_;
    }
    bool operator()(const string& values) {
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
class DeviceFileName : public CompatibilityHelper {
public:
    DeviceFileName() : CompatibilityHelper("device_file_name") {}
    ~DeviceFileName() {}
    bool operator()(const string& file) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'device_file_name' is deprecated. Please use 'output_name'");
        }
        MagLog::info() << "Compatibility issue: Parameter device_file_name is deprecated.\n"
                       << "               Please use output_name instead." << std::endl;
        ParameterManager::set("output_legacy_name", file);
        ParameterManager::set("output_file_minimal_width", 0);
        return true;
    }
};

/*! \brief Converts device_width into output_width.

  The convention to describe filenames in Magics++ has changed.
*/
class DeviceWidth : public CompatibilityHelper {
public:
    DeviceWidth() : CompatibilityHelper("device_width") {}
    ~DeviceWidth() {}
    bool operator()(const int width) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'device_width' is deprecated. Please use 'output_width'");
        }
        MagLog::info() << "Compatibility issue: Parameter device_width is deprecated.\n"
                       << "             Please use output_width instead." << std::endl;
        ParameterManager::set("output_width", width);
        return true;
    }
};

/*! \brief Converts device_ into output_width.

  The convention to describe filenames in Magics++ has changed.
*/
class DeviceQualityLevel : public CompatibilityHelper {
public:
    DeviceQualityLevel() : CompatibilityHelper("device_quality_level") {}
    ~DeviceQualityLevel() {}
    bool operator()(const int quality) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'device_quality_level' is deprecated. Please use 'output_jpg_quality'");
        }
        MagLog::info() << "Compatibility issue: Parameter device_quality_level is "
                          "deprecated.\n"
                       << "             Please use output_jpg_quality instead." << std::endl;
        ParameterManager::set("output_jpg_quality", quality);
        return true;
    }
};

/*! \brief Converts text_quality into text_font and text_font_style.

*/
class TextQuality : public CompatibilityHelper {
public:
    TextQuality(const string& base = "") : CompatibilityHelper() {
        base_ = (base.empty()) ? "" : base + "_";
        set(base_ + "quality");
    }
    ~TextQuality() {}
    bool operator()(const string& quality) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + base_ + "quality' is deprecated. Please use '" + base_ +
                                  "font' and '" + base_ + "font_style'");
        }

        MagLog::info() << "Compatibility issue: Parameter " << base_ << "quality is deprecated.\n"
                       << "               Please use " << base_ << "font and " << base_ << "font_style instead."
                       << std::endl;

        if (magCompare(quality, "low")) {
            ParameterManager::set(base_ + "font", "serif");
            ParameterManager::set(base_ + "font_style", "normal");
        }
        else if (magCompare(quality, "medium")) {
            ParameterManager::set(base_ + "font", "sansserif");
            ParameterManager::set(base_ + "font_style", "normal");
        }
        else if (magCompare(quality, "high")) {
            ParameterManager::set(base_ + "font", "sansserif");
            ParameterManager::set(base_ + "font_style", "bold");
        }
        else {
            MagLog::warning() << "The setting " << quality << " for the parameter " << base_
                              << "_quality is not valid! Default font is used." << std::endl;
            ParameterManager::set(base_ + "font", "sansserif");
            ParameterManager::set(base_ + "font_style", "normal");
        }

        return true;
    }

protected:
    string base_;
};

class TextHeight : public CompatibilityHelper {
public:
    TextHeight(const string& from, const string& to) : CompatibilityHelper(from), from_(from), to_(to) {}
    ~TextHeight() {}
    bool operator()(double height) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
        }

        MagLog::warning() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
                          << "               Please use " << to_ << " instead. " << to_ << " has been set to " << height
                          << std::endl;
        ParameterManager::set(to_, height);
        return true;
    }

protected:
    string from_;
    string to_;
};
class TextFontHeight : public CompatibilityHelper {
public:
    TextFontHeight(const string& from, const string& to) : CompatibilityHelper(from), from_(from), to_(to) {}
    ~TextFontHeight() {}
    bool operator()(double height) {
        if (from_ != to_) {
            if (MagicsSettings::strict()) {
                throw MagicsException("Parameter '" + from_ + "' is deprecated. Please use '" + to_ + "'");
            }

            MagLog::info() << "Compatibility issue: Parameter " << from_ << " is deprecated.\n"
                           << "               Please use " << to_ << " instead. " << to_ << " has been set to "
                           << height << std::endl;
        }
        else {
            if (MagicsSettings::strict()) {
                throw MagicsException("Parameter '" + from_ + "' is now a string");
            }

            MagLog::info() << from_
                           << " is now expecting a string : consider to change your "
                              "setting for psetc "
                           << endl;
        }
        ParameterManager::set(to_, tostring(height));
        return true;
    }
    bool operator()(const string& height) {
        ParameterManager::set(to_, height);
        return true;
    }

protected:
    string from_;
    string to_;
};
/*! \brief Converts gd_file_name into output_file_root_name.

  The convention to describe GD filenames in Magics++ has changed between
  version 1.1 and 1.2.
*/
class GdFileName : public CompatibilityHelper {
public:
    GdFileName() : CompatibilityHelper("gd_file_name") {}
    ~GdFileName() {}
    bool operator()(const string& file) {
        if (MagicsSettings::strict()) {
            throw MagicsException(
                "The value 'none' for parameter 'subpage_map_projection' is deprecated. Please use 'cartesian'");
        }
        MagLog::info() << "Compatibility issue: Parameter gd_file_name is deprecated.\n"
                       << "              Please use output_name instead." << std::endl;
        ParameterManager::set("output_legacy_name", file);
        ParameterManager::set("output_file_minimal_width", 0);
        return true;
    }
};

class SubpageMapProjectionNone : public CompatibilityHelper {
public:
    SubpageMapProjectionNone() : CompatibilityHelper("subpage_map_projection") {}
    ~SubpageMapProjectionNone() {}
    bool operator()(const string& projection) {
        string fix = projection;
        if (magCompare(projection, "none")) {
            if (MagicsSettings::strict()) {
                throw MagicsException(
                    "The value [none] for parameter 'subpage_map_projection' is deprecated. Please use 'cartesian'");
            }


            fix = "cartesian";
            MagLog::info() << "Compatibility issue: The value [none] for Parameter "
                              "subpage_map_projection is deprecated.\n"
                           << "               Please use [cartesian] instead." << std::endl;
        }
        ParameterManager::set("subpage_map_projection", fix);
        return true;
    }
};

/*! \brief 'device' is not a valid parameter anymore

   set device to gd and gf_format
*/
class DeviceCompatibilityHelper : public CompatibilityHelper {
public:
    DeviceCompatibilityHelper() : CompatibilityHelper("device") {}
    ~DeviceCompatibilityHelper() {}
    bool operator()(const string& device) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Parameter 'device' is deprecated. Please use 'output_format'");
        }

        MagLog::info() << "Compatibility issue: the parameter device is deprecated.\n"
                       << "              Please use the parameter output_format instead!" << endl;

        if (magCompare(device, "jpeg") || magCompare(device, "jpg") || magCompare(device, "png") ||
            magCompare(device, "gif")) {
            ParameterManager::set("output_format", device);
            return true;
        }
        else {
            ParameterManager::set("output_format", device);
            return true;
        }
        return false;
    }
};

class Legend : public CompatibilityHelper {
public:
    Legend(const string& legend) : CompatibilityHelper(legend) {}
    ~Legend() {}
    bool operator()(const string& legend) {
        MagTranslator<string, bool> translator;
        if (translator(legend) == false) {
            ParameterManager::set("legend", "off");
            return false;
        }
        MagLog::info() << "Compatibility issue: The legend is turned on!\n";
        ParameterManager::set("legend", "on");
        // Act as a Action routine!...
        FortranMagics::instance().plegend();
        return false;
    }
};

class ContourAutomaticSetting : public CompatibilityHelper {
public:
    ContourAutomaticSetting() : CompatibilityHelper("contour_automatic_setting") {}
    ~ContourAutomaticSetting() {}
    bool operator()(const string& setting) {
        // cout << " setting -->" << setting << endl;
        if (magCompare(setting, "eccharts")) {
            if (MagicsSettings::strict()) {
                throw MagicsException("'ecchart' is automatic deprecated. Please use 'ecmwf'");
            }

            MagLog::info() << "Compatibility issue: ecchart automatic contour is "
                              "deprecated, consider using ecmwf\n";
            return false;
        }
        if (magCompare(setting, "web")) {
            if (MagicsSettings::strict()) {
                throw MagicsException("'ecchart' is automatic deprecated. Please use 'ecmwf'");
            }


            MagLog::warning() << "Compatibility issue: web automatic contour is now "
                                 "deprecated, use ecmwf instead\n";
            ParameterManager::set("contour_automatic_setting", "ecmwf");
            return true;
        }
        if (magCompare(setting, "on")) {
            if (MagicsSettings::strict()) {
                throw MagicsException("'ecchart' is automatic deprecated. Please use 'ecmwf'");
            }


            MagLog::warning() << "Compatibility issue: on for  automatic contour is "
                                 "now deprecated, use ecmwf instead\n";
            ParameterManager::set("contour_automatic_setting", "ecmwf");
            return true;
        }
        return false;
    }
};

class WindArrowIndexHead : public CompatibilityHelper {
public:
    WindArrowIndexHead() : CompatibilityHelper("wind_arrow_head_index") {}
    ~WindArrowIndexHead() {}
    bool operator()(int index) {
        if (MagicsSettings::strict()) {
            throw MagicsException(
                "Parameter 'wind_arrow_index_head' is deprecated. Please use 'wind_arrow_head_ratio'");
        }


        MagLog::info() << "Compatibility issue: Parameter wind_arrow_index_head "
                          "does not exist anymore.\n"
                       << "            use wind_arrow_head_shape and "
                          "wind_arrow_head_ratio instead."
                       << endl;
        const int head_index = (int)index / 10;
        const int ratio      = index % 10;
        double head_ratio;

        if (ratio == 1)
            head_ratio = 0.3;
        else if (ratio == 2)
            head_ratio = 0.6;
        else if (ratio == 3)
            head_ratio = 1.;
        else if (ratio == 4)
            head_ratio = 1.3;
        else if (ratio == 5)
            head_ratio = 1.6;
        else if (ratio == 6)
            head_ratio = 2.0;
        else {
            MagLog::warning() << "invalid ratio " << ratio << " revert to default 1." << endl;
            head_ratio = 1.;
        }

        MagLog::info() << "  wind_arrow_head_index set to " << head_index << "AND wind_arrow_head_ratio set to "
                       << head_ratio << endl;

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
static TextQuality contour_label_quality("contour_label");
static TextQuality map_label_quality("map_label_quality");
static WindArrowIndexHead wind_arrow_index_head;

// static TextFontHeight text_reference_character_height("text_reference_character_height", "text_font_size");
// static TextFontHeight text_height("text_font_size", "text_font_size");
// static TextFontHeight legend_text_font_size("legend_text_font_size", "legend_text_font_size");

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
static SimpleTranslator subpage_map_area_definition("subpage_map_area_definition", "subpage_map_area_definition_polar",
                                                    true);
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

// =================================================================


MagicsParameter<double> paxis_min_value("axis_min_value", 0);
MagicsParameter<double> paxis_max_value("axis_max_value", 100);
MagicsParameter<string> pgraph_axis_control("graph_axis_control", "off");
MagicsParameter<string> paxis_date_max_value("axis_date_max_value", "");
MagicsParameter<string> paxis_date_min_value("axis_date_min_value", "");

class AxisConverter : public CompatibilityHelper {
public:
    AxisConverter(const string& from, const string& horiz, const string& vert) :
        CompatibilityHelper(from), from_(from), vertical_(vert), horizontal_(horiz) {}

    bool operator()(double val) {
        ParameterManager::set(from_, val);
        update(val);
        return false;
    }

    template <class T>
    void update(T val) {
        string orientation;
        ParameterManager::get("axis_orientation", orientation);

        if (magCompare(orientation, "horizontal")) {
            ParameterManager::set(horizontal_, val);
        }
        else {
            ParameterManager::set(vertical_, val);
        }
    }

    void update(const string& val) {
        string orientation;
        ParameterManager::get("axis_orientation", orientation);

        if (magCompare(val, "position_list")) {
            MagLog::info() << "position_list is now using the user coordinates system and not cm" << endl;
            return;
        }

        if (magCompare(orientation, "horizontal")) {
            ParameterManager::set(horizontal_, val);
        }
        else {
            ParameterManager::set(vertical_, val);
        }
    }

    bool operator()(const string& value) {
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

void execute_magml(const char* file) {
    WebInterpretor::magml(file);
}

void execute_json(const char* file) {
    WebInterpretor::json(file);
}

void set_param(const char* param, const char* value) {
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
static ActionInterceptor polyline_input_positions_filename("polyline_input_positions_filename",
                                                           &FortranMagics::flagInputPoly);

class PageIDWarning : public CompatibilityHelper {
public:
    PageIDWarning() : CompatibilityHelper("page_id_line_logo_plot") {}
    ~PageIDWarning() {}
    bool operator()(const string& val) {
        if (magCompare(val, "user")) {
            MagLog::warning() << "The value user for page_id_line_logo_plot is now deprecated.\n"
                              << "               Please use pimport to add your own logo." << endl;
            ParameterManager::set("page_id_line_logo_plot", "off");
        }
        else
            ParameterManager::set("page_id_line_logo_plot", val);
        return true;
    }
};
static PageIDWarning page_id_line_logo_plot;
static CoastlinesResolution map_coastline_resolution;

}  // namespace magics
