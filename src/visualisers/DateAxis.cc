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

/*! \file DateAxisMethod.cc
    \brief Implementation of the Template class DateAxisMethod.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/

#include "DateAxis.h"
#include "Axis.h"

using namespace magics;


DateAxisMethod::DateAxisMethod()
{
	update();
	dateCreators_["automatic"] = &DateAxisMethod::automatic;
	dateCreators_["years"] = &DateAxisMethod::years;
	dateCreators_["months"] = &DateAxisMethod::months;
	dateCreators_["days"] = &DateAxisMethod::days;
	dateCreators_["hours"] = &DateAxisMethod::hours;


}


DateAxisMethod::~DateAxisMethod()
{
}

/*!
 Class information are given to the output-stream.
*/		
void DateAxisMethod::print(ostream& out)  const
{
	out << "DateAxisMethod[";
	out << "]";
}

double DateAxisMethod::getMin() const
{
	update();
	return from_ - base_;
}


double DateAxisMethod::getMax() const
{
	update();
	return to_  - base_;
}

void DateAxisMethod::update() const
{ 
	//from_ = (date_min_ != "undef" ) ? DateTime(date_min_) : DateTime();
	//to_   = (date_max_ != "undef" ) ? DateTime(date_max_) : DateTime();

	//base_ = (DateAxisMethodAttributes::reference_ != "undef" ) ? DateTime(DateAxisMethodAttributes::reference_) : from_;
}


void DateAxisMethod::prepare(const Axis& axis, AxisItems& items)
{
	update();
	
	interval_ = axis.interval_;
	map<string,  DateHelper >::iterator method = dateCreators_.find(lowerCase(method_));
	if ( method != dateCreators_.end() )
		(this->*method->second)(items);
	else {
		MagLog::warning() << "Could not the method " << method_ << " to setup the axis date: using the automatic method..." << endl;
		automatic(items);
	}

	

	AxisItems dates;
	for ( AxisItems::const_iterator item = items.begin(); item != items.end(); ++item) {
		if ( (**item).date() )
			dates.push_back((*item)->clone());
	}
	position_ = 0;
	hours_label(dates, items);
	days_label(dates, items);
	months_label(dates, items);
	years_label(dates, items);

	for ( AxisItems::const_iterator item = items.begin(); item != items.end(); ++item)
		{
		if ( (**item).date() )
			MagLog::dev() << " found at " << (**item).position() << " --->" << (**item).label() << "\n";
		}


	


}



void DateAxisMethod::updateX(const Transformation& transformation)
{
	const double min = transformation.getMinX();
		const double max = transformation.getMaxX();


		base_ = DateTime(transformation.getReferenceX());
		from_ = base_ + Second(min);
		to_ = base_ + Second(max);

		MagLog::dev() << "base-> " << base_ << endl;
		MagLog::dev() << "from-> " << from_ << endl;
		MagLog::dev() << "to-> " << to_ << endl;

}

void DateAxisMethod::updateY(const Transformation& transformation)
{
	const double min = transformation.getMinY();
	const double max = transformation.getMaxY();


	base_ = DateTime(transformation.getReferenceY());
	from_ = base_ + Second(min);
	to_ = base_ + + Second(max);

	MagLog::dev() << "base-> " << base_ << endl;
	MagLog::dev() << "from-> " << from_ << endl;
	MagLog::dev() << "to-> " << to_ << endl;
}




HyperAxis::HyperAxis()
{
}

HyperAxis::~HyperAxis()
{
}

	
void HyperAxis::prepare(double, double, AxisItems&) const
{
}

void HyperAxis::updateX(const Transformation&)
{
}

void HyperAxis::updateY(const Transformation&) 
{
}

void HyperAxis::print(ostream&) const
{
}

void HyperAxis::update() const
{
}




void DateAxisMethod::automatic(AxisItems& list)
{
	DateTime min = (from_ < to_) ? from_ : to_;
	DateTime max = (from_ < to_) ? to_ : from_;
	double days  = (max-min)/(24*3600);

	if (days > 2500 ) { // 7 Years labels!
		hours_ = false;
		days_  = "off";
		months_ = false;
		years_ = true;
		// axis_tick_label_frequency',3
		years(list);
		return;
	}



	if ( days > 300 ) {
		hours_ = false;
		days_  = "off";
		years_ = true;
		months(list);
		//call psetr('axis_tick_interval',1.)
		 //call pseti('axis_tick_label_frequency',2)
		return;
	}

	if  (days > 3 ) {
		hours_ = false;
		months_ = true;
		this->days(list);
		return;
	}


	hours_ = true;

	hours(list);

}

void DateAxisMethod::years(AxisItems& list)
{

		days_ = "off";
		months_ = false;
		DateTime label;
		DateTime tick;

		DateTime min = (from_ < to_) ? from_ : to_;
			DateTime max = (from_ < to_) ? to_ : from_;
		long position  =  0;

		int frequency;
		const long long seconds_a_year=24*3600*365;
		

		if ( interval_ == INT_MAX ) {
		    if ( max-min < 20*seconds_a_year ) {
		    	frequency = 1;
		    }
		    else if ( max-min < 50*seconds_a_year ) {
		    	frequency = 2;
		    }
		    else if ( max-min < 100*seconds_a_year ) {
		    	frequency = 5;
		    }
		    else
		    	frequency = 10;
		}
		else {
			frequency = (int) interval_;
		}
		if ( frequency == 0)
			frequency = 1;
		int count = 0;

		for ( int  year = min.date().year() ; year <= max.date().year() ; year++)
		{
			if ((count++ % frequency)==0) {
				MagDate date(year, 1, 1);
				label = DateTime(date, MagTime(position, 0, 0));
				list.push_back(new AxisDateItem(label - base_, label));
				tick = DateTime(date, MagTime(0, 0, 0));

				list.push_back(new AxisTickItem(tick - base_, ""));

				if (frequency == 1) {
					for ( int i = 0; i < 4; i++) {
						MagDate t = MagDate(year, 1+(3*i), 1);
						DateTime tick(t, MagTime(0,0,0) );
						list.push_back(new AxisMinorTickItem(tick - base_));
					}
				}
			}
			else {
				DateTime tick(MagDate(year, 1, 1), MagTime(0, 0, 0));
				list.push_back(new AxisMinorTickItem(tick - base_));

			}


		}
}
void DateAxisMethod::years_label(AxisItems& in, AxisItems& list)
{
	if ( !years_ )
		return;
	string last;
	for ( AxisItems::iterator item = in.begin(); item != in.end(); ++item) {
		AxisItem* year = (*item)->clone();
		year->format("%Y", -1);
		if ( last == year->label() ) {
			delete year;
			continue;
		}
		last =  year->label();
		year->level(position_);
		year->height(year_height_, year_font_, year_font_style_);
		year->colour(year_colour_->name());
		list.push_back(year);
	}

}
void DateAxisMethod::months(AxisItems& list)
{
	DateTime label;
	DateTime tick;
	days_ = "off";


 DateTime min = (from_ < to_) ? from_ : to_;
	DateTime max = (from_ < to_) ? to_ : from_;

	long position  =  0;

	int frequency;

	if ( interval_ == INT_MAX ) {
		if ( max-min  < 20*24*3600*30 ) {
			frequency = 1;
		}
		else if ( max-min  < 120*24*3600*30 ) {
			frequency = 2;
		}
		else {
			frequency = 3;
		}
	}
	else {
		frequency = (int) interval_;
	}
	if ( frequency == 0)
		frequency = 1;
	int count = 0;


	MagDate date = min.date();
	while ( date <= max.date() ) {
		int year  = date.year();
		int month  = date.month();

		if ((count++ % frequency)==0) {
			label = DateTime(date, MagTime(position, 0, 0));
			list.push_back(new AxisDateItem(label - base_, label));
			tick = DateTime(date, MagTime(0, 0, 0));

			list.push_back(new AxisTickItem(tick - base_, ""));



		}
		else {
			tick = DateTime(MagDate(year, month, 1), MagTime(0, 0, 0));
			list.push_back(new AxisMinorTickItem(tick - base_));

		}
		month++;
		if ( month == 13 ) {
			year++;
			month=1;
		}

		date = MagDate(year, month, 1);
	}
}
void DateAxisMethod::months_label(AxisItems& in, AxisItems& list)
{
	if ( !months_ )
		return;
	map <string, pair<string, int> > formats;
	formats["one"] = std::make_pair("%b", 1);
	formats["full"] = std::make_pair("%B", -1);
	formats["three"] = std::make_pair("%B", 3);
	AxisItem* last = 0;
	

	for ( AxisItems::iterator item = in.begin(); item != in.end(); ++item) {
		AxisItem* month = (*item)->clone();
		map <string, pair<string, int> >::iterator fmt = formats.find(lowerCase(month_composition_));
		if ( fmt != formats.end() ) {
			month->format(fmt->second.first, fmt->second.second);
		}
		else {
			MagLog::warning() << "could not find type " << month_composition_ << "for formatting month lables" << endl;
			month->format("%B", 3);
		}
		AxisItem* current = (*item)->clone();
		current->format("%B", 3);
		if ( last && last->label() == month->label() ) {
			delete month;
			continue;
		}
		if ( last ) {
			delete last;
		}
		last = current;

		month->colour(month_colour_->name());
		month->level(position_);
		month->height(month_height_, month_font_, month_font_style_);


		list.push_back(month);
	}
	position_++;
	if ( last )
		delete last;
}
void DateAxisMethod::days(AxisItems& list)
{

	DateTime label;
	DateTime tick;

	DateTime min = (from_ < to_) ? from_ : to_;
	DateTime max = (from_ < to_) ? to_ : from_;
	long position  =  ( max-min  < 20*24*3600 ) ? 12 : 0;

	int frequency;
	int day = (max-min) / (24*3600);

	if ( interval_ == INT_MAX ) {
		if ( day  < 20 ) {
			frequency = 1;
		}
		else if ( day < 60 ) {
			frequency = 2;
		}
		else if ( day  < 120 ) {
			frequency = 5;
		}

		else {
			frequency = 10;
		}
	}
	else {
		frequency = (int) interval_;
	}
	if ( frequency == 0)
		frequency = 1;
	int count = 0;

	for ( MagDate date = min.date(); date <= max.date(); ++date)
	{
		label = DateTime(date, MagTime(position, 0, 0));
		if ((count++ % frequency) == 0 ) {

			list.push_back(new AxisDateItem(label - from_, label));
			tick = DateTime(date, MagTime(0, 0, 0));

			list.push_back(new AxisTickItem(tick - from_, ""));
			if ( frequency == 1 ) {
				for (int i = 6; i < 24; i+=6 ) {
				tick = DateTime(date, MagTime(i, 0, 0));
				list.push_back(new AxisMinorTickItem(tick - from_));
				}
			}
		}
		else {
			tick = DateTime(date, MagTime(0, 0, 0));
			list.push_back(new AxisMinorTickItem(tick - from_));
		}

	}
}
void DateAxisMethod::days_label(AxisItems& in, AxisItems& list)
{
	if ( magCompare(days_, "off" ) )
		return;

	map <string, pair<string, int> > formats;

	if ( magCompare(days_, "number" ) ) {
		formats["one"] = std::make_pair("%e", -1);
		formats["three"] = std::make_pair("%e", -1);
		formats["full"] = std::make_pair("%e", -1);
	}
	else {
		formats["one"] = std::make_pair("%a", 1);
		formats["three"] = std::make_pair("%a", 3);
		formats["full"] = std::make_pair("%A", -1);
	}


	string last;
	for ( AxisItems::iterator item = in.begin(); item != in.end(); ++item) {
		if ( (*item)->date() == false )
			continue;
		AxisItem* day = (*item)->clone();
		if (day->sunday() )
			day->colour(sunday_colour_->name());
		else
			day->colour(day_colour_->name());
		map <string, pair<string, int> >::iterator fmt = formats.find(lowerCase(day_composition_));
		if ( fmt != formats.end() ) {
			day->format(fmt->second.first, fmt->second.second);
		}
		else {
			MagLog::warning() << "could not find type " << day_composition_ << "for formatting day labels" << endl;
			day->format("%e", -1);
		}
		if ( magCompare(days_, "both" ) ) {
			string label = day->label();
			day->format("%e", -1);
			label = label  + day->label();
			day->label(label);
		}
		if ( last == day->label() ) {
			delete day;
			continue;
		}
		last = day->label();

		day->level(position_);
		day->height(day_height_, day_font_, day_font_style_);

		list.push_back(day);
	}
	position_++;
}
void DateAxisMethod::hours(AxisItems& list)
{
	DateTime label;
	DateTime tick;

	DateTime min = ( from_ < to_ ) ? from_ : to_;
	DateTime max = ( from_ < to_ ) ? to_ : from_;


	int frequency;

	if ( interval_ == INT_MAX ) {
		    if ( max-min  < 24*3600 ) {
			frequency = 1; // every hour
		    }
		    else if ( max-min < 2*24*3600 ) {
			frequency = 2; // every other hour!
		    }
		    else {
			frequency = 3;
		    }
		}
		else {
			frequency = (int) interval_;
		}
		if ( frequency == 0)
			frequency = 1;
		for ( MagDate date = min.date(); date <= max.date(); ++date)
		{
			for (int i = 0; i < 24; i++ )
			{
				tick = DateTime(date, MagTime(i, 0, 0));
				if ( i%frequency == 0) {
					list.push_back(new AxisDateItem(tick - base_, tick));
					list.push_back(new AxisTickItem(tick - base_, ""));
				}
				else {
					list.push_back(new AxisMinorTickItem(tick - base_));
				}
			}
		}
}
void DateAxisMethod::hours_label(AxisItems& in, AxisItems& list)
{

	if ( !hours_ )
		return;
	string last;
	for ( AxisItems::iterator item = in.begin(); item != in.end(); ++item) {
		AxisItem* hour = (*item)->clone();
		hour->format("%H h", -1);
		if ( last == hour->label() ) {
			delete hour;
			continue;
		}
		last = hour->label();
		hour->level(position_);
		hour->height(hour_height_, hour_font_, hour_font_style_);
		hour->colour(hour_colour_->name());
		list.push_back(hour);
	}
	position_++;
}


