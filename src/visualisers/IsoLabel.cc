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

/*! \file IsoLabel.cc
    \brief Implementation of the Template class IsoLabel.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
    
*/



#include "IsoLabel.h"
#include "Label.h"
#include "MagicsFormat.h"
#include "Polyline.h"
using namespace magics;

IsoLabel::IsoLabel() 
{
	methods_["text"] = &IsoLabel::text;
	methods_["number"] = &IsoLabel::number;
	methods_["both"] = &IsoLabel::both;
}


IsoLabel::~IsoLabel() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void IsoLabel::print(ostream& out)  const
{
	out << "IsoLabel[";
	out << "]";
}

void IsoLabel::operator()(Polyline& object, double l)  const
{
	const_iterator do_it = find(l);
	if ( do_it == end() ) {
        	return;
	}
    Colour colour =  ( colour_ == "contour_line_colour" ) ? object.getColour() : Colour(colour_);
	std::map<string, Method>::const_iterator method = methods_.find(lowerCase(type_));

	string text;
	if ( method == methods_.end() )
	{
		MagLog::warning() << "contour_label_type: " << type_ << " is unknown : use number instead" << endl; 
		text = number(l);
	}
	else 
		text = (this->*method->second)(l);
	
	Label label(text);

	label.setVisible(true);
	label.setHeight(height_);
	label.setBlanking(blanking_);
	MagFont font(font_, "", height_); 
	font.colour(colour );
	label.font(font);
	object.setLabel(label);
} 


void NoIsoLabel::operator()(Polyline& /*object*/, double) const
{ 
	Label label("");
	label.setVisible(false);
	//object.setLabel(label);
}


string IsoLabel::number(double l) const
{
	ostringstream nice;
	nice << MagicsFormat(format_, l); 
	return nice.str();
}

string IsoLabel::text(double ) const {
	return text_;
}

string IsoLabel::both(double l) const
{
	return number(l) + " " + text_;
}

