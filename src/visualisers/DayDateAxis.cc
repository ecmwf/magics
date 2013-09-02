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

/*! \file DayDateAxis.cc
    \brief Implementation of the Template class DayDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/



#include "DayDateAxis.h"

using namespace magics;


DayDateAxis::DayDateAxis() 
{
	labelColour_ = colour_;
	labelHeight_ = height_;
}


DayDateAxis::~DayDateAxis() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void DayDateAxis::print(ostream& out)  const
{
	out << "DayDateAxis[";
	out << "]";
}

void DayDateAxis::label(AxisItem& item)  const
{
	// For information on format : http://stdcxx.apache.org/doc/stdlibref/time-get.html#Table%C2%A034
	item.colour( item.sunday()  ? "red" : colour_);
	static map<pair<string, string>, string> formats_;
	if ( formats_.empty() ) {
			formats_[make_pair("both", "one")] = "%a %e";
			formats_[make_pair("both", "three")] = "%a %e";
			formats_[make_pair("both", "full")] = "%A %e";
			formats_[make_pair("number", "one")] = "%e";
			formats_[make_pair("number", "three")] = "%e";
			formats_[make_pair("number", "full")] = "%e";
			formats_[make_pair("day", "one")] = "%a";
			formats_[make_pair("day", "three")] = "%a";
			formats_[make_pair("day", "full")] = "%a";
		}
		formats_[make_pair("both","user")] = DayDateAxisAttributes::format_;
		formats_[make_pair("day","user")] = DayDateAxisAttributes::format_;
		formats_[make_pair("number","user")] = DayDateAxisAttributes::format_;

		map<pair<string, string>, string>::iterator iformat = formats_.find(make_pair(lowerCase(type_), lowerCase(composition_)));
		string format = ( iformat == formats_.end() ) ? "%a %e" : iformat->second;
		item.format(format);


	item.height(height_);
/*
	string in = item.label();	
	string out;
	for ( string::iterator x = in.begin(); x != in.end(); ++x)
		if (!isspace(*x)) out.push_back(*x); 
	
	item.label(out);
*/
}




void DateAxisManipulator::labels(vector<AxisItem*>& items)
{
	unsigned int index = 0;

	int previous;
	string id;
	
	for (unsigned int current = 0; current < items.size(); current++) 
	{	
		label(*items[current]);
		if ( current  == 0 ) {
			previous = current;
			id = items[current]->id();
			items[current]->label("");
			continue;
		}
		if ( items[current]->id() == id ) {		
			items[current]->label("");
			continue;
		}
		
		// here the label is diffrent
		
		AxisItem* date = items[index]->clone();
		
		
		id = items[current]->id();
		
		
		label(*date);
		string name =  date->label();
		if ( index == 0 ) {
			// here, if the label is too long we can have an overlay..
			// we put only if the size of the label is small ddd DD =(6)
			if ( name.size() > 6 ) name = "";
		}
		date->label(name);	
		date->height(labelHeight());	
		date->level(position_);		
		items[current]->label("");
		push_back(date);		
		index = current;
		previous = current;
	}



	if (index < items.size() ) {
		AxisItem* date = items[index]->clone();
		label(*date);
		items[index]->label(""); 
		date->level(position_);		
		date->height(labelHeight());	
		push_back(date);		 
	}
}
