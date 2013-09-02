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
