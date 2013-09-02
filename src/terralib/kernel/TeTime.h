/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeTime.h
    \brief This file contains structures and definitions to deal with date and time
*/
#ifndef  __TERRALIB_INTERNAL_TIME_H
#define  __TERRALIB_INTERNAL_TIME_H

#include "TeDefines.h"
#include "TeDataTypes.h"

#include <time.h> 
#include <stdio.h>
#include <string>
#include <iostream>

//!  A class for supporting date and time. 
class TL_DLL TeTime
{
    struct tm	ts_; 		// Unix time structure
    time_t		last_;		// time in seconds
	TeChronon	chronon_;

public:
	
	//!	Set the date and time from the system clock.
	time_t now(void);

	//!	Set all time information with NULL value 
	TeTime(void);
	
	//!	Copy constructor.
	TeTime(const TeTime& t);

	//!	Set the date from the system clock and the time from its parameter list. Chronon defaulted to second.
	TeTime(int h, int m, int s, TeChronon chronon);

	//!	Set date and time from arguments, though the time fields.  
	TeTime(	const std::string& dt, TeChronon chronon, const std::string& mask, 
			const std::string& dateS = "/", const std::string& timeS = ":", const std::string& indPM = "PM");

	//!	Return a time of day std::string in format "hh:mm:ss"
	std::string getTime();

	//!	Return a std::string to the date in the form "yyyy-mm-dd"
	std::string getDate(void);

	//!	Return a pointer to the date and time in the form passed in mask.  
	std::string getDateTime (const std::string& mask= "DDsMMsYYYYsHHsmmsSS", const std::string& dateS="/", const std::string& timeS=":", const std::string& indPM="PM", const std::string& indAM="AM");

	//! Reset time to the specified arguments. Return time_t: number of seconds since 0:00:00 Jan 1 1987
	/*!
      \param y  year
	  \param m  month
	  \param d  day
	  \param h  hour
	  \param min  minutes
      \param s  seconds
	 */
	time_t Set(int y, int m, int d, int h = 0, int min = 0, int s = 0);

	//! Get year value as an four digit integer
	int year()	{return ts_.tm_year+1900;}  
	
	//! Get month value
	int month() {return ts_.tm_mon+1;}
	
	//! Get day of the month value
	int day()	{return ts_.tm_mday;} 
	
	//! Get hour value as an integer
	int hour()	{return ts_.tm_hour;}
	
	//! Get minute value as an integer
	int minute() {return ts_.tm_min;}
	
	//! Get second value as an integer
	int second() {return ts_.tm_sec;} 

	//! Get week day as in integer
	int weekDay() { return ts_.tm_wday;} //(0-6) 0=Sunday 

	//! Get year day as in integer
	int yearDay() { return ts_.tm_yday;} //(0-365) 01/01=0 
	
	//! Get chronon definition
	TeChronon chronon () {return chronon_;}

	//! Set chronon definition
	void chronon (TeChronon c);  
	
	//!	Assignment operator for TeTime objects.
	TeTime& operator=(const TeTime&);

	//!	Add 'delta' chronon units to the current date. Return current date.
	TeTime& operator+=(int delta);

	//! Increase a chronon unit in the time (Prefix operator)
	TeTime& operator++();

	//!	Add 'delta' chronon units to the current date and return a new date. Return new date.
	TeTime operator+(int delta);

	//!	Subtract 'delta' chronon units from the current date. Return current date.
	TeTime& operator-=(int delta);
			
	//! Operator ==
	bool operator==(const TeTime& time) const;  

	//! Operator <
	bool operator<(const TeTime& time) const;

	//! Operator <=
	bool operator<=(const TeTime& time) const;

	//!	Returns the difference betwwen the times in seconds
	int operator-(const TeTime& other);
	
	//! Verify if the time has null values  
	bool isValid();  

    //!	Normal destructor.
    ~TeTime(void) { }

};

//! Operator to display an instance of TeTime
TL_DLL std::ostream& operator<<(std::ostream& os, TeTime& N);


#endif


