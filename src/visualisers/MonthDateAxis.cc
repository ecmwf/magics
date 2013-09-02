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

/*! \file MonthDateAxis.cc
    \brief Implementation of the Template class MonthDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/



#include "MonthDateAxis.h"

using namespace magics;


map<string,  MonthDateAxis::Formatter > MonthDateAxis::formatters_;

MonthDateAxis::MonthDateAxis() 
{
	labelColour_ = colour_->name();
	labelHeight_ = height_;
}


MonthDateAxis::~MonthDateAxis() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void MonthDateAxis::print(ostream& out)  const
{
	out << "MonthDateAxis[";
	out << "]";
}

void  MonthDateAxis::one(AxisItem& item) const
{
	string format = "%b";
	item.format(format);
	item.colour(colour_->name());
	item.height(height_);
}

void  MonthDateAxis::full(AxisItem& item) const
{
	string format = "%B";
	item.format(format);
	item.colour(colour_->name());
	item.height(height_);
}
void  MonthDateAxis::three(AxisItem& item) const
{
	string format = "%B";
	item.format(format);
	item.colour(colour_->name());
	item.height(height_);
	string lab = item.label();
	string slabel = lab.substr(0, 3);
	item.label(slabel);

}
void  MonthDateAxis::user(AxisItem& item) const
{
	item.format(MonthDateAxisAttributes::format_);
	item.colour(colour_->name());
	item.height(height_);
}
void MonthDateAxis::label(AxisItem& item) const
{
	if ( formatters_.empty() ) {
		formatters_["one"] = &MonthDateAxis::one;
		formatters_["full"] = &MonthDateAxis::full;
		formatters_["three"] = &MonthDateAxis::three;
		formatters_["user"] = &MonthDateAxis::user;
	}


	map<string,  Formatter >::iterator format = formatters_.find(lowerCase(composition_));
	if  ( format == formatters_.end() ) {
			this->one(item);
	}
	else {
		 (this->*format->second)(item);
	}

	
}
