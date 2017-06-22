/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisItem.cc
    \brief Implementation of the Template class AxisItem.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 11-Oct-2005
    
    Changes:
    
*/


#include "AxisItem.h"
#include <locale>
#include <DateTime.h>
#include "MagLog.h"
#include <cfloat>
#include "Axis.h"
#include "MagFont.h"
#include "MagicsFormat.h"

using namespace magics;

AxisItem::AxisItem(double position, const string& label, int level) :
	position_(position), label_(label), level_(level), colour_("undef"), height_(DBL_MIN), font_("undef"), style_("undef")
{
	
}

AxisItem::AxisItem(double position, const string& label, int level, const string& colour, double height) :
	position_(position), label_(label), level_(level), colour_(colour), height_(height), font_("undef"), style_("undef")
{
	
}
AxisItem::AxisItem(double position, const string& format) :
	position_(position), label_(tostring(position)), level_(0),
	colour_("undef"), height_(DBL_MIN), font_("undef"), style_("undef")
{
	if ( same(position_, 0) ) position_ = 0;



	ostringstream nice;

	nice << MagicsFormat(format.empty() ? "(automatic)" : format, position);


	label_ = nice.str();

}



AxisHyperItem::AxisHyperItem(double position, vector<double>& values) : AxisItem(position, "")
{
	// Specialised in lat/lon axis!!!
	double lat = values[1];
	double lon = values[0];
	
	ostringstream lab;
		string ns = "&#176;";
		if ( lat < 0 ) ns += "S";
		if ( lat >= 0 ) ns += "N";
		float y = float(maground(abs(lat)*100)) / 100;
		lab <<  y << ns;
		
		
		string ew = "&#176;";
			if ( lon < 0 ) ew += "W";
			if ( lon >= 0 ) ew += "E";
			float x = float(maground(abs(lon)*100)) / 100;
			lab << "/" << x << ew;
			

		label_ =  lab.str();    
}

AxisHyperItem::~AxisHyperItem()
{
	
	
}

AxisItem::~AxisItem() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisItem::print(ostream& out)  const
{
	out << "AxisItem[";
	out << label_ << " at " << position_;
	out << "]";
}

AxisDateItem::AxisDateItem(double position, DateTime date, int level, const string& colour, double height)
 : AxisItem(position, "", level, colour, height), date_(date), defaultColour_("undef")
{ 
	//format("%w");
	//if (label_ == "0" ) colour_ = "red"; // Sunday!
}

void AxisDateItem::format(const string& format, int digit) const
{	
	std::locale loc("");
    
	ostringstream out;
	out.imbue(loc);

	const std::time_put<char>& facet = std::use_facet<std::time_put<char> >(loc); 
  
	tm convert = date_;
	facet.put(out, out, ' ', &convert, format.c_str(), format.c_str()+format.length());    
	string label = (digit == -1 ) ?  out.str() : out.str().substr(0, digit);
	label_ = label;
	id_ = label;
	
	//MagLog::dev() << " add Day ---> " << label_ << "\n";
}

bool AxisDateItem::sunday() const
{ 
	format("%w", -1);
	return  (label_ == "0" );
}

bool AxisDateItem::runday() const
{ 
	format("%w", -1);
	return  (label_ == "1" || label_ == "4");
}

void AxisItem::setFont(MagFont& font)
{
	if ( font_ != "undef" )
		font.name(font_);
	if ( colour_ != "undef" )
			font.colour(colour_);
	if ( style_ != "undef" )
				font.style(style_);
	if ( height_ != DBL_MIN)
		font.size(height_);

}
