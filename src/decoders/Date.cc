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


#include "MagException.h"
#include "DateTime.h"
#include "Tokenizer.h"

#include "MagLog.h"

#include <time.h>
#include <iomanip>

using namespace magics;

static const char* months[] = 
{ "jan","feb","mar","apr","may","jun",
  "jul","aug","sep","oct","nov","dec"
};

static void check(const MagDate& date,long value)
{
	if(value <= 999999 ) value += 19000000;

	if(value != date.yyyymmdd())
	{
		ostringstream  os;
		os << "Invalid date " << value << " becomes " << date << ends;
		throw MagicsException(os.str());
	}
}


MagDate::MagDate(long date) : julian_(dateToJulian(date))
{
	if(date) check(*this,date);
}

MagDate::MagDate(long year,long month,long day) : julian_(dateToJulian(year*10000 + month*100 +day))
{
	check(*this,year*10000 + month*100 +day);
}

// Warning, unchecked...
MagDate::MagDate(long year,long dayOfYear)
{
	julian_ = dateToJulian(year*10000 + 101); // 1 of jan
	julian_ += (dayOfYear - 1);
	ASSERT(this->year() == year);
}

long MagDate::parse(const string& s)
{
	Tokenizer parse("-");
	vector<string> result;

	parse(s,result);

	bool err   = false;
	long value = 0;
	int i;

	switch(result.size())
	{
		case 1:
			switch(s.length())
			{
				case 3:
					// For climatology
					err = true;
					for(i = 0; i < 12; i++)
						if(s == months[i])
						{
							value = 1900 * 10000 + (i+1)*100 + 1;
							err   = false;
							break;
						}
					break;

				case 6:
				case 8:
					value   = atol(s.c_str());
					break;

				default:
					err = true;
					break;
			}
			break;

		// MagDates as yyyy-ddd

		case 2:

			if(result[0].length() != 2 && result[0].length() != 4) err = true;
			if(result[1].length() != 3) err = true;

			{
				long year = atol(result[0].c_str());
				long day  = atol(result[1].c_str());

				MagDate date(year,1,1);
				date += day - 1;
				value = date.yyyymmdd();
			}

			break;

		// MagDates as yyyy-mm-dd

		case 3:

			if(result[0].length() != 2 && result[0].length() != 4) err = true;
			if(result[1].length() > 3) err = true;
			if(result[2].length() > 3) err = true;

			value = atol(result[0].c_str()) * 10000 +
					atol(result[1].c_str()) * 100 +
					atol(result[2].c_str());

			break;


		default:
			err = true;
			break;
	}
/*
 *   Stephan: Removed exception - caused Metview to not plot the field (but title) - see MAGP-455 
 */
//	if(err) 
//		throw MagicsException(string("Invalid date ") + s);
	MagLog::warning() << "The date read looks invalid: " << s << ". Please check your title!\n";


	// Come back here....
	// temp patch for monthly means
	if( (value%100) == 0) value++;

	return value;
}

MagDate::MagDate(const string& s)
{
	long value = parse(s);
	julian_    = dateToJulian(value);
	check(*this,value);
}

MagDate::operator string() const
{
	ostringstream os;
	os << *this << ends;
	return os.str();
}

MagDate::operator tm() const
{
	struct tm tm_date = { 0, 0, 0, 0, 0, 0, 0, 0 };
	tm_date.tm_mday = day();
	tm_date.tm_mon = month() - 1;
	tm_date.tm_year = year() - 1900;
    
	int mm = month();
	int yy = year();
	if (mm < 3)
	{
		mm += 12;
		--yy;
	}

	tm_date.tm_wday  = (day() + (13 * mm - 27)/5 + yy + yy/4 - yy/100 + yy/400) % 7;
	return tm_date;
}

// Returns a date in the format yyyymmdd from a julian number

long MagDate::julianToMagDate(long jdate)
{
	long x,y,d,m,e;
	long day,month,year;

	x = 4 * jdate - 6884477;
	y = (x / 146097) * 100;
	e = x % 146097;
	d = e / 4;

	x = 4 * d + 3;
	y = (x / 1461) + y;
	e = x % 1461;
	d = e / 4 + 1;

	x = 5 * d - 3;
	m = x / 153 + 1;
	e = x % 153;
	d = e / 5 + 1;

	if( m < 11 )
		month = m + 2;
	else
		month = m - 10;

	day = d;
	year = y + m / 11;

	return year * 10000 + month * 100 + day;
}

long MagDate::today(void)
{
	time_t now;
	time(&now);

#ifdef _THREAD_SAFE
	struct tm t;
	gmtime_r(&now,&t);
	long td = (1900 + t.tm_year) * 10000 + (t.tm_mon+1)* 100 + t.tm_mday;
#else
	struct tm *t;
	t = gmtime(&now);
	long td = (1900 + t->tm_year) * 10000 + (t->tm_mon+1)* 100 + t->tm_mday;
#endif
	return dateToJulian(td);
}

/*!
 Returns a julian number from a yyyymmdd date
*/
long MagDate::dateToJulian(long ddate)
{
	long  m1,y1,a,b,c,d,j1;

	long month,day,year;

	year = ddate / 10000;
	ddate %= 10000;
	month  = ddate / 100;
	ddate %= 100;
	day = ddate;

	// Negative dates are relative to today
	if(ddate <= 0)
		return today() + ddate;

	if(year < 100)
	{
//		throw SeriousBug("Please, use 4 digits dates... 2000 is near");
//      MagLog::warning() << "Please, use 4 digits dates... 2000 is near" << "\n";
		year = year + 1900;
	}

	if(month > 2)
	{
		m1 = month - 3;
		y1 = year;
	}
	else
	{
		m1 = month + 9;
		y1 = year - 1;
	}
	a = 146097*(y1/100)/4;
	d = y1 % 100;
	b = 1461*d/4;
	c = (153*m1+2)/5+day+1721119;
	j1 = a+b+c;

	return(j1);
}


void MagDate::print(ostream& s) const
{
	long ddate = julianToMagDate(julian_);
	long month,day,year;

	year = ddate / 10000;
	ddate %= 10000;
	month  = ddate / 100;
	ddate %= 100;
	day = ddate;

	char oldfill = s.fill();
	s << year << '-' << std::setw(2) << std::setfill('0') << month 
		<< '-' << std::setw(2) << std::setfill('0') << day << std::setfill(oldfill);
}

long MagDate::year() const
{
	long ddate = julianToMagDate(julian_);
	return ddate / 10000;
}

long MagDate::month() const
{
	long ddate = julianToMagDate(julian_);
	ddate %= 10000;
	return ddate / 100;
}

long MagDate::day() const
{
	long ddate = julianToMagDate(julian_);
	return ddate % 100;
}

long MagDate::yyyymmdd() const
{
	return julianToMagDate(julian_);
}

string MagDate::monthName(long n)
{
	ASSERT(n >= 1 && n <= 12);
	return months[n-1];
}
