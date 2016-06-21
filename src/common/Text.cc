/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Text.cc
    \brief Implementation of Text graphics class (template).
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/

#include "Text.h"
#include "TagHandler.h"



using namespace magics;

Text::Text() :  justification_(MCENTRE), blanking_(false), verticalAlign_(MBASE), angle_(0)
{
}

Text::~Text() 
{
} 

/*
class AddHelper
{
public:
	AddHelper(vector<NiceText>& text) : text_(text) {}
	~AddHelper() {}
	void operator()(const NiceText& text) 
		{ text_.push_back(text); }
protected:
	vector<NiceText>& text_;
};
*/



double  Text::getFontMax()
{
	double max= -1;
	for (vector<NiceText>::const_iterator nice = nice_.begin(); nice != nice_.end(); ++nice) {
		if (nice->font().size() > max ) 
			max = nice->font().size();
	}
	if ( max < 0) max = getFont().size();
//	MagLog::dev() << " Text::getFontMax()-->" << max << endl;
	return max;
}


void Text::print(ostream& out)  const
{
	out << "Text[";
	for (vector<NiceText>::const_iterator nice = nice_.begin(); nice != nice_.end(); ++nice) {
	   out << *nice << endl;
	}
	for (Text::const_iterator point = this->begin(); point != this->end(); ++point)
		out << ", " << *point;
	out << "]";
}


void Text::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
}

void Text::setText(const string& text) {
	TagHandler dummy;

	TagConverter converter(dummy);

	converter.font(getFont());

	converter.decode(text, this);



}
