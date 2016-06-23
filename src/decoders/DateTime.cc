/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "DateTime.h"

#include "Tokenizer.h"

#include "MagException.h"
#include "MagLog.h"
using namespace magics;

DateTime::DateTime(const MagDate& d, const MagTime& t):
	date_(d), time_(t)
{
}

DateTime::DateTime(const string& s)
{
	if ( s.empty() || s == "undef" ) {
		return;
	}
	Tokenizer parse(" ");
	vector<string> result;

	parse(s,result);
	
	if ( result.size() == 1 ) {
		date_ = MagDate(result[0]);
	    time_ = MagTime("00:00:00");
		return;
	}
	
	if ( result.size() == 2 ) {
		date_ = MagDate(result[0]);
	    time_ = MagTime(result[1]);
		return;
	}
	MagLog::error() << "Check date format:" << s << "\n";
	
}


DateTime::DateTime(time_t thetime)
{
#ifdef _THREAD_SAFE
	struct tm t;
	gmtime_r(&thetime,&t);
	long td   = (1900 + t.tm_year) * 10000 + (t.tm_mon+1)* 100 + t.tm_mday;
	long hour = t.tm_hour;
	long min  = t.tm_min;
	long sec  = t.tm_sec;
#else
	struct tm *t;
	t = gmtime(&thetime);
	long td   = (1900 + t->tm_year) * 10000 + (t->tm_mon+1)* 100 + t->tm_mday;
	long hour = t->tm_hour;
	long min  = t->tm_min;
	long sec  = t->tm_sec;
#endif

	date_ = MagDate(td);
	time_ = MagTime(hour,min,sec);
}

void DateTime::print(ostream& s) const
{
	s << date_ << ' ' << time_;
}

DateTime::operator string() const
{
	ostringstream os;
	os << *this;
	return os.str();
}

DateTime& DateTime::operator=(const DateTime& other)
{
	date_ = other.date_;
	time_ = other.time_;
	return *this;
}

Second DateTime::operator-(const DateTime& other) const
{
	Second date = (date_ - other.date_) * 24 * 3600;
	Second time = time_ - other.time_;

	return date + time;
}

DateTime DateTime::operator+(const Second& s) const
{
	MagDate d = date();
	long t = time();
	d += long(s) / (24 * 3600);
	t += long(s) % (24 * 3600); 
	if ( t < 0 ) {
		d -= 1;
		while (t <= (-3600 * 24) ) {
			d -= 1;
			t += 3600 * 24;
		}
	    t += 3600 * 24;
	}
	while (t >= 3600 * 24)
	{
		d += 1;
		t -= 3600 * 24;
	}
	
	return DateTime(d,Second(t));
}

DateTime DateTime::round(const Second& rnd) const
{
	long long seconds = double(date_.julian_) * 24.0 * 3600 + Second(time_);
	seconds = (seconds / long(rnd)) * rnd;
	
	long	d = seconds / (3600 * 24);
	Second	t = seconds % (3600 * 24);

	return DateTime(MagDate(d,true),MagTime(t));
}

DateTime::operator tm() const
{
	struct tm tm_date = tm(date_);
    tm_date.tm_sec = time_.seconds();
	tm_date.tm_min = time_.minutes();
	tm_date.tm_hour = time_.hours();
	return tm_date;
}

DateTime::operator double() const
{
	return double(date_.julian_) * 24.0 * 3600 + Second(time_);
}


#include <locale>
using namespace std;
static std::locale& getLocale()
{
static locale loc= locale::classic();
    try {
        loc = locale("");
    }
    catch (...)
    {
        MagLog::info() << "Problem to setup the locale\n"
                       << "Check your LANG variable - current value: " <<  getEnvVariable("LANG")<< endl;
        loc= locale::classic();
    }
    return loc;
}

string DateTime::tostring(const string& format) const
{
        ostringstream out;
        tm convert = *this;
        out.imbue(getLocale());
        const std::time_put<char>& tfac = use_facet<time_put<char> >(getLocale());

        tfac.put(out, out, ' ', &convert, format.c_str(), format.c_str()+format.length());

        return out.str();
}

