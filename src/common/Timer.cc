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

#ifndef marsmachine_H
#include "magics.h"
#endif

#ifndef Timer_H
#include "Timer.h"
#endif

#ifndef Seconds_H
#include "Seconds.h"
#endif

#include "Mutex.h"
#include "AutoLock.h"
#include "MagLog.h"

namespace magics {

timeval operator-(const timeval& a,const timeval& b)
{
	timeval diff;

	diff.tv_sec  = a.tv_sec  - b.tv_sec;
	diff.tv_usec = a.tv_usec - b.tv_usec;

	if (diff.tv_usec < 0)
	{
		diff.tv_sec--;
		diff.tv_usec += 1000000;
	}
	return diff;
}

}
using namespace magics;

Timer::Timer(const string& name, const string& detail):
	name_(name),
	details_(detail), 
	cpu_(clock())
{
	gettimeofday(&start_,0);

}
static Mutex lockprofiles_;
Timer::~Timer()
{
	timeval now;
	gettimeofday(&now,0);

	 char start[256], stop[256];


     sprintf(start, "%f", (start_.tv_sec + start_.tv_usec / 1000000.0));
     sprintf(stop, "%f", (now.tv_sec + now.tv_usec / 1000000.0));




	const double  s   = elapsed();
	clock_t cpu =  clock();
	const Seconds	sec1(s);
	const Seconds	sec2(double(cpu-cpu_)/CLOCKS_PER_SEC);
	MagLog::profile() << name_ << ": "
		<< sec1 << " elapsed, " 
		<< sec2 << " cpu" << "\n";

	ostringstream e;
			e << elapsed();
			
			ostringstream c;
			c << double(cpu-cpu_)/CLOCKS_PER_SEC;
			

	{
		AutoLock<Mutex> lock(lockprofiles_);
	
		profiles_.push_back(ProfileInfo(name_, details_, start, stop, e.str(), c.str()));
	}
}

double Timer::elapsed()
{
	timeval stop;
	gettimeofday(&stop,0);  
	timeval diff = stop - start_;
	return (double)diff.tv_sec + ((double)diff.tv_usec / 1000000.);
}

ProfileInfo::ProfileInfo(const string& name, const string& details,
		const string& start, const string& stop, const string& elapsed, const string& cpu) :
	name_(name), details_(details), start_(start), stop_(stop), elapsed_(elapsed), cpu_(cpu)  {
		
	}

ostream& operator<<(ostream& s,const ProfileInfo& p)
			{ s << "\t\"" << p.name_ << "\" : {" << endl;
			   s << "\t\t\"details\" : \"" << p.details_ << "\"," << endl;
			   s << "\t\t\"start\" : " << p.start_ << "," << endl;
			   s << "\t\t\"stop\" : " << p.stop_ << "," << endl;
			   s << "\t\t\"elapsed\" : " << p.elapsed_ << "," << endl;
			   s << "\t\t\"cpu\" : " << p.cpu_ << endl;
			   s << "\t}";
			   return s;
			   }
	
vector<ProfileInfo> Timer::profiles_;
