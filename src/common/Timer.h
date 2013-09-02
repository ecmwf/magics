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

// File Timer.h
// Baudouin Raoult - ECMWF May 96

#ifndef Timer_H
#define Timer_H

#ifndef magics_H
#include "magics.h"
#endif

//#ifndef MagLog_H
//#include "MagLog.h"
//#endif

#include <time.h>
#include <sys/time.h>
#include <sys/types.h>




class ProfileInfo 
{
public:
	ProfileInfo(const string& name, const string& details,
			const string& start, const string& stop,
			const string& elapsed, const string& cpu);
	string name_;
	string details_;
	string elapsed_;
	string cpu_;
	string start_;
	string stop_;
	
	// -- Friends
	    //! Overloaded << operator to call print().
		friend ostream& operator<<(ostream& s,const ProfileInfo& p);
	
};

// This stack object prints the elapse time between the call to
// its contructor and the call to its destructor
namespace magics {
class Timer {
public:

// -- Contructors

	Timer(const string& name, const string& details);

// -- Destructor

	~Timer();

// -- Methods

	double elapsed();
	static vector<ProfileInfo>::const_iterator begin() 
			{ return profiles_.begin(); }
		static vector<ProfileInfo>::const_iterator end() 
			{ return profiles_.end(); }

private:

// No copy allowed

	Timer(const Timer&);
	Timer& operator=(const Timer&);

// -- Members
	
	string         name_;
	string 		  details_;
	struct timeval start_;

	clock_t        cpu_;
	static vector<ProfileInfo> profiles_;
	
	
	

// -- Methods
	
	ostream& put(ostream&,double);

};

timeval operator-(const timeval&,const timeval&);

} // namespace magics

#endif
