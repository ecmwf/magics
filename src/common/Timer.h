/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Timer.h
// Baudouin Raoult - ECMWF May 96

#ifndef Timer_H
#define Timer_H

#include "magics.h"


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
