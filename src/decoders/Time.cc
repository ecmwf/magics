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

#include "DateTime.h"
#include "MagException.h"
#include "MagTranslator.h"
#include <time.h>
#include "Tokenizer.h"


using namespace magics;

inline void printMagTime(ostream& s, long n)
{
	if(n<10) s << '0';
	s << n;
}

magics::MagTime::MagTime(long seconds):
	seconds_(seconds)
{
	if(seconds >= 86400 || seconds < 0)
	{
		string msg = "MagTime in seconds cannot exceed 86400 ";
//		MagTranslator<long,string> t;
//		msg += t(seconds);
		throw MagicsException(msg);
	}
}

magics::MagTime::MagTime(const string& s)
{
	Tokenizer parse(":");
	vector<string> result;

	parse(s,result);

	long hh = 0, mm = 0 , ss = 0;
	bool err = false;
	long t = atol(s.c_str());
	//MagLog::debug() << "time-->[" << s << "]=" << s.length() << "\n";
	switch(result.size())
	{
		case 0: 
            break;
		case 1:
			// hh or hhmm or hhmmss
			switch(s.length())
			{
				
				case 2: hh = t; break;
				case 4: hh = t/100; mm = t % 100; break;
				case 6: hh = t/10000; mm = (t%10000)/100; ss = (t%10000)%100; break;
				default: err = true; break;
			}
			break;

		case 2:
			// hh:mm
			err =  result[0].length() != 2 
				|| result[1].length() != 2;

			hh = atol(result[0].c_str());
			mm = atol(result[1].c_str());

			break;
			
		case 3:
			// hh:mm:ss
			//MagLog::dev()<< result[0].length() << ":" << result[1].length() << ":" << result[2].length() << endl;
			err =  result[0].length() != 2 
			    || result[1].length() != 2 
				|| result[2].length() != 2;

			hh = atol(result[0].c_str());
			mm = atol(result[1].c_str());
			ss = atol(result[2].c_str());
			err = false;

			break;

		default: 
			err = true;
			break;
	}

	if(err) {
		throw MagicsException(string("Invalid time ") + s);
	}
	
	if(hh >= 24 || mm >= 60 || ss >= 60
	   || hh < 0 || mm < 0 || ss < 0)
	{
        string msg = "Wrong input for time: ";
//        MagTranslator<long,string> t;
//        msg += t(hh); msg += " hours ";
//        msg += t(mm); msg += " minutes ";
//        msg += t(ss); msg += " seconds";
        throw MagicsException(msg);
	}

	seconds_ = hh*3600+mm*60+ss;
}

magics::MagTime::operator string() const
{
	ostringstream os;
	os << *this << ends;
	return os.str();
}

magics::MagTime::MagTime(const MagTime& other):
	seconds_(other.seconds_)
{
}

magics::MagTime& magics::MagTime::operator=(const MagTime& other)
{
	seconds_ = other.seconds_;
	return *this;
}

magics::MagTime::MagTime(long hh, long mm, long ss):
	seconds_(hh*3600+mm*60+ss)
{
	if(hh >= 24 || mm >= 60 || ss >= 60
	   || hh < 0 || mm < 0 || ss < 0)
	{
        string msg = "Wrong input for time: ";
//        MagTranslator<long,string> t;
//        msg += t(hh); msg += " hours ";
//        msg += t(mm); msg += " minutes ";
//        msg += t(ss); msg += " seconds";
        throw MagicsException(msg);
	}
}

magics::MagTime::~MagTime()
{
}

long magics::MagTime::hours() const
{
	long l = seconds_;
	return l / 3600;
}

long magics::MagTime::minutes() const
{
	long l = seconds_;
	return (l % 3600) / 60;
}

long magics::MagTime::seconds() const
{
	long l = seconds_;
	return l % 60;
}

long magics::MagTime::hhmmss() const
{
	return hours() * 10000 + minutes() * 100 + seconds();
}

void magics::MagTime::print(ostream& s) const
{
	printMagTime(s,hours());
	s << ':';
	printMagTime(s,minutes());
	s << ':';
	printMagTime(s,seconds());
}

magics::MagTime magics::MagTime::now()
{
	time_t now;
	time(&now);
	struct tm *pt;

#ifdef _THREAD_SAFE
	struct tm t;
	gmtime_r(&now,&t);
	pt = &t;
#else
	struct tm *t;
	t = gmtime(&now);
	pt = t;
#endif
	return MagTime(pt->tm_hour,pt->tm_min,pt->tm_sec);
}
