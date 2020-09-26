
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file ArrowPlottingAttributes.h
    \\brief Definition of ArrowPlotting Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "ArrowPlottingAttributes.h"
#include "MagicsParameter.h"
#include "ParameterSettings.h"

using namespace magics;

ArrowPlottingAttributes::ArrowPlottingAttributes():
	calm_indicator_size_(ParameterManager::getDouble("wind_arrow_calm_indicator_size")),
	calm_below_(ParameterManager::getDouble("wind_arrow_calm_below")),
	head_(ParameterManager::getInt("wind_arrow_head_shape")),
	ratio_(ParameterManager::getDouble("wind_arrow_head_ratio")),
	max_speed_(ParameterManager::getDouble("wind_arrow_max_speed")),
	min_speed_(ParameterManager::getDouble("wind_arrow_min_speed")),
	thickness_(ParameterManager::getInt("wind_arrow_thickness")),
	unit_system_(ParameterManager::getString("wind_arrow_unit_system")),
	unit_velocity_(ParameterManager::getDouble("wind_arrow_unit_velocity")),
	legend_unit_(ParameterManager::getString("wind_arrow_legend_text")),
	fixed_velocity_(ParameterManager::getDouble("wind_arrow_fixed_velocity"))
	,
	calm_(MagTranslator<string, CalmIndicator>().magics("wind_arrow_calm_indicator")),
	colour_(MagTranslator<string, Colour>().magics("wind_arrow_colour")),
	origin_position_(MagTranslator<string, ArrowPosition>().magics("wind_arrow_origin_position")),
	style_(MagTranslator<string, LineStyle>().magics("wind_arrow_style"))
	 
{
} 


ArrowPlottingAttributes::~ArrowPlottingAttributes()
{
	
}

    
void ArrowPlottingAttributes::set(const std::map<string, string>& params)
{
	vector<string> prefix(2);
	int i = 0;
	prefix[i++] = "wind";
	prefix[i++] = "wind_arrow";
	
	setAttribute(prefix, "wind_arrow_calm_indicator_size", calm_indicator_size_, params);
	setAttribute(prefix, "wind_arrow_calm_below", calm_below_, params);
	setAttribute(prefix, "wind_arrow_head_shape", head_, params);
	setAttribute(prefix, "wind_arrow_head_ratio", ratio_, params);
	setAttribute(prefix, "wind_arrow_max_speed", max_speed_, params);
	setAttribute(prefix, "wind_arrow_min_speed", min_speed_, params);
	setAttribute(prefix, "wind_arrow_thickness", thickness_, params);
	setAttribute(prefix, "wind_arrow_unit_system", unit_system_, params);
	setAttribute(prefix, "wind_arrow_unit_velocity", unit_velocity_, params);
	setAttribute(prefix, "wind_arrow_legend_text", legend_unit_, params);
	setAttribute(prefix, "wind_arrow_fixed_velocity", fixed_velocity_, params);
	
	setMember(prefix, "wind_arrow_calm_indicator", calm_, params);
	setMember(prefix, "wind_arrow_colour", colour_, params);
	setAttribute(prefix, "wind_arrow_origin_position", origin_position_, params);
	setAttribute(prefix, "wind_arrow_style", style_, params);
	
}

void ArrowPlottingAttributes::copy(const ArrowPlottingAttributes& other)
{
	calm_indicator_size_ = other.calm_indicator_size_;
	calm_below_ = other.calm_below_;
	head_ = other.head_;
	ratio_ = other.ratio_;
	max_speed_ = other.max_speed_;
	min_speed_ = other.min_speed_;
	thickness_ = other.thickness_;
	unit_system_ = other.unit_system_;
	unit_velocity_ = other.unit_velocity_;
	legend_unit_ = other.legend_unit_;
	fixed_velocity_ = other.fixed_velocity_;
	calm_ = unique_ptr<CalmIndicator>(other.calm_->clone());
	colour_ = unique_ptr<Colour>(other.colour_->clone());
	origin_position_ = other.origin_position_;
	style_ = other.style_;
	
} 


bool ArrowPlottingAttributes::accept(const string& node)
{	
	
	if ( magCompare(node, "arrow")  )
		return true;
	if ( acceptNode(node, calm_) )
		return true;
	
	return false;
}

void ArrowPlottingAttributes::set(const XmlNode& node)
{
	bool apply = false;

	if ( this->accept(node.name()) == false ) 
		return;

	if ( magCompare(node.name(), "arrow")  )
		apply = true;
	

	if ( apply )
		set(node.attributes());
	else {
		setMember(node.name(), calm_, node);
		
	}
	for (auto &elt : node.elements())
	{
		setMember(elt->name(), calm_, *elt); 
		
	}
}

void ArrowPlottingAttributes::print(ostream& out)  const
{
	out << "Attributes[";
	out << " calm_indicator_size = " <<  calm_indicator_size_;
	out << " calm_below = " <<  calm_below_;
	out << " head = " <<  head_;
	out << " ratio = " <<  ratio_;
	out << " max_speed = " <<  max_speed_;
	out << " min_speed = " <<  min_speed_;
	out << " thickness = " <<  thickness_;
	out << " unit_system = " <<  unit_system_;
	out << " unit_velocity = " <<  unit_velocity_;
	out << " legend_unit = " <<  legend_unit_;
	out << " fixed_velocity = " <<  fixed_velocity_;
	out << " calm = " <<  *calm_;
	out << " colour = " <<  *colour_;
	out << " origin_position = " <<  origin_position_;
	out << " style = " <<  style_;
	
	out << "]" << "\n";
}

void ArrowPlottingAttributes::toxml(ostream& out)  const
{
	out <<  "\"arrow\""; 
	out << ", \"wind_arrow_calm_indicator_size\":";
	niceprint(out,calm_indicator_size_);
	out << ", \"wind_arrow_calm_below\":";
	niceprint(out,calm_below_);
	out << ", \"wind_arrow_head_shape\":";
	niceprint(out,head_);
	out << ", \"wind_arrow_head_ratio\":";
	niceprint(out,ratio_);
	out << ", \"wind_arrow_max_speed\":";
	niceprint(out,max_speed_);
	out << ", \"wind_arrow_min_speed\":";
	niceprint(out,min_speed_);
	out << ", \"wind_arrow_thickness\":";
	niceprint(out,thickness_);
	out << ", \"wind_arrow_unit_system\":";
	niceprint(out,unit_system_);
	out << ", \"wind_arrow_unit_velocity\":";
	niceprint(out,unit_velocity_);
	out << ", \"wind_arrow_legend_text\":";
	niceprint(out,legend_unit_);
	out << ", \"wind_arrow_fixed_velocity\":";
	niceprint(out,fixed_velocity_);
	out << ", \"wind_arrow_calm_indicator\":";
	calm_->toxml(out);
	out << ", \"wind_arrow_colour\":";
	niceprint(out, *colour_);
	out << ", \"wind_arrow_origin_position\":";
	niceprint(out, origin_position_);
	out << ", \"wind_arrow_style\":";
	niceprint(out, style_);
	
}

static MagicsParameter<double> wind_arrow_calm_indicator_size("wind_arrow_calm_indicator_size", 0.3, "");
static MagicsParameter<double> wind_arrow_calm_below("wind_arrow_calm_below", 0.5, "");
static MagicsParameter<int> wind_arrow_head_shape("wind_arrow_head_shape", 0, "");
static MagicsParameter<double> wind_arrow_head_ratio("wind_arrow_head_ratio", 0.3, "");
static MagicsParameter<double> wind_arrow_max_speed("wind_arrow_max_speed", 1.0e+21, "");
static MagicsParameter<double> wind_arrow_min_speed("wind_arrow_min_speed", -1.0e+21, "");
static MagicsParameter<int> wind_arrow_thickness("wind_arrow_thickness", 1, "");
static MagicsParameter<string> wind_arrow_unit_system("wind_arrow_unit_system", "paper", "");
static MagicsParameter<double> wind_arrow_unit_velocity("wind_arrow_unit_velocity", 25.0, "");
static MagicsParameter<string> wind_arrow_legend_text("wind_arrow_legend_text", "m/s", "");
static MagicsParameter<double> wind_arrow_fixed_velocity("wind_arrow_fixed_velocity", 0.0, "");
static MagicsParameter<string> wind_arrow_calm_indicator("wind_arrow_calm_indicator", "off", ""); 
static MagicsParameter<string> wind_arrow_colour("wind_arrow_colour", "blue", ""); 
static MagicsParameter<string> wind_arrow_origin_position("wind_arrow_origin_position", "tail", ""); 
static MagicsParameter<string> wind_arrow_style("wind_arrow_style", "solid", ""); 
#include "CalmIndicator.h"
static SimpleObjectMaker<CalmIndicator , CalmIndicator> calm_CalmIndicator("calm");
static SimpleObjectMaker<CalmIndicator , CalmIndicator> on_CalmIndicator("on");
static SimpleObjectMaker<NoCalmIndicator , CalmIndicator> nocalm_NoCalmIndicator("nocalm");
static SimpleObjectMaker<NoCalmIndicator , CalmIndicator> off_NoCalmIndicator("off");