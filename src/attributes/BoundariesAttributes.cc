
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file BoundariesAttributes.h
    \\brief Definition of Boundaries Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "BoundariesAttributes.h"
#include "MagicsParameter.h"
#include "ParameterSettings.h"

using namespace magics;

BoundariesAttributes::BoundariesAttributes():
	thickness_(ParameterManager::getInt("map_boundaries_thickness")),
	disputed_(ParameterManager::getBool("map_disputed_boundaries")),
	disputed_thickness_(ParameterManager::getInt("map_disputed_boundaries_thickness")),
	admistrative_(ParameterManager::getBool("map_administrative_boundaries")),
	administrative_list_(ParameterManager::getStringArray("map_administrative_boundaries_countries_list")),
	administrative_thickness_(ParameterManager::getInt("map_administrative_boundaries_thickness"))
	,
	style_(MagTranslator<string, LineStyle>().magics("map_boundaries_style")),
	colour_(MagTranslator<string, Colour>().magics("map_boundaries_colour")),
	disputed_style_(MagTranslator<string, LineStyle>().magics("map_disputed_boundaries_style")),
	disputed_colour_(MagTranslator<string, Colour>().magics("map_disputed_boundaries_colour")),
	administrative_style_(MagTranslator<string, LineStyle>().magics("map_administrative_boundaries_style")),
	administrative_colour_(MagTranslator<string, Colour>().magics("map_administrative_boundaries_colour"))
	 
{
} 


BoundariesAttributes::~BoundariesAttributes()
{
	
}

    
void BoundariesAttributes::set(const std::map<string, string>& params)
{
	vector<string> prefix(3);
	int i = 0;
	prefix[i++] = "map";
	prefix[i++] = "map_boundaries";
	prefix[i++] = "map_boundaries";
	
	setAttribute(prefix, "map_boundaries_thickness", thickness_, params);
	setAttribute(prefix, "map_disputed_boundaries", disputed_, params);
	setAttribute(prefix, "map_disputed_boundaries_thickness", disputed_thickness_, params);
	setAttribute(prefix, "map_administrative_boundaries", admistrative_, params);
	setAttribute(prefix, "map_administrative_boundaries_countries_list", administrative_list_, params);
	setAttribute(prefix, "map_administrative_boundaries_thickness", administrative_thickness_, params);
	
	setAttribute(prefix, "map_boundaries_style", style_, params);
	setMember(prefix, "map_boundaries_colour", colour_, params);
	setAttribute(prefix, "map_disputed_boundaries_style", disputed_style_, params);
	setMember(prefix, "map_disputed_boundaries_colour", disputed_colour_, params);
	setAttribute(prefix, "map_administrative_boundaries_style", administrative_style_, params);
	setMember(prefix, "map_administrative_boundaries_colour", administrative_colour_, params);
	
}

void BoundariesAttributes::copy(const BoundariesAttributes& other)
{
	thickness_ = other.thickness_;
	disputed_ = other.disputed_;
	disputed_thickness_ = other.disputed_thickness_;
	admistrative_ = other.admistrative_;
	administrative_list_ = other.administrative_list_;
	administrative_thickness_ = other.administrative_thickness_;
	style_ = other.style_;
	colour_ = unique_ptr<Colour>(other.colour_->clone());
	disputed_style_ = other.disputed_style_;
	disputed_colour_ = unique_ptr<Colour>(other.disputed_colour_->clone());
	administrative_style_ = other.administrative_style_;
	administrative_colour_ = unique_ptr<Colour>(other.administrative_colour_->clone());
	
} 


bool BoundariesAttributes::accept(const string& node)
{	
	
	if ( magCompare(node, "boundaries")  )
		return true;
	
	return false;
}

void BoundariesAttributes::set(const XmlNode& node)
{
	bool apply = false;

	if ( this->accept(node.name()) == false ) 
		return;

	if ( magCompare(node.name(), "boundaries")  )
		apply = true;
	

	if ( apply )
		set(node.attributes());
	else {
		
	}
	for (auto &elt : node.elements())
	{
		
	}
}

void BoundariesAttributes::print(ostream& out)  const
{
	out << "Attributes[";
	out << " thickness = " <<  thickness_;
	out << " disputed = " <<  disputed_;
	out << " disputed_thickness = " <<  disputed_thickness_;
	out << " admistrative = " <<  admistrative_;
	out << " administrative_list = " <<  administrative_list_;
	out << " administrative_thickness = " <<  administrative_thickness_;
	out << " style = " <<  style_;
	out << " colour = " <<  *colour_;
	out << " disputed_style = " <<  disputed_style_;
	out << " disputed_colour = " <<  *disputed_colour_;
	out << " administrative_style = " <<  administrative_style_;
	out << " administrative_colour = " <<  *administrative_colour_;
	
	out << "]" << "\n";
}

void BoundariesAttributes::toxml(ostream& out)  const
{
	out <<  "\"boundaries\""; 
	out << ", \"map_boundaries_thickness\":";
	niceprint(out,thickness_);
	out << ", \"map_disputed_boundaries\":";
	niceprint(out,disputed_);
	out << ", \"map_disputed_boundaries_thickness\":";
	niceprint(out,disputed_thickness_);
	out << ", \"map_administrative_boundaries\":";
	niceprint(out,admistrative_);
	out << ", \"map_administrative_boundaries_countries_list\":";
	niceprint(out,administrative_list_);
	out << ", \"map_administrative_boundaries_thickness\":";
	niceprint(out,administrative_thickness_);
	out << ", \"map_boundaries_style\":";
	niceprint(out, style_);
	out << ", \"map_boundaries_colour\":";
	niceprint(out, *colour_);
	out << ", \"map_disputed_boundaries_style\":";
	niceprint(out, disputed_style_);
	out << ", \"map_disputed_boundaries_colour\":";
	niceprint(out, *disputed_colour_);
	out << ", \"map_administrative_boundaries_style\":";
	niceprint(out, administrative_style_);
	out << ", \"map_administrative_boundaries_colour\":";
	niceprint(out, *administrative_colour_);
	
}

static MagicsParameter<int> map_boundaries_thickness("map_boundaries_thickness", 1, "");
static MagicsParameter<string> map_disputed_boundaries("map_disputed_boundaries", "on", "");
static MagicsParameter<int> map_disputed_boundaries_thickness("map_disputed_boundaries_thickness", 1, "");
static MagicsParameter<string> map_administrative_boundaries("map_administrative_boundaries", "off", "");
static MagicsParameter<stringarray> map_administrative_boundaries_countries_list("map_administrative_boundaries_countries_list", stringarray(), "");
static MagicsParameter<int> map_administrative_boundaries_thickness("map_administrative_boundaries_thickness", 1, "");
static MagicsParameter<string> map_boundaries_style("map_boundaries_style", "solid", ""); 
static MagicsParameter<string> map_boundaries_colour("map_boundaries_colour", "grey", ""); 
static MagicsParameter<string> map_disputed_boundaries_style("map_disputed_boundaries_style", "dash", ""); 
static MagicsParameter<string> map_disputed_boundaries_colour("map_disputed_boundaries_colour", "automatic", ""); 
static MagicsParameter<string> map_administrative_boundaries_style("map_administrative_boundaries_style", "dash", ""); 
static MagicsParameter<string> map_administrative_boundaries_colour("map_administrative_boundaries_colour", "automatic", ""); 