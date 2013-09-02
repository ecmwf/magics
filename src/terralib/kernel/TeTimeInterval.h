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
/*! \file TeTimeInterval.h
    \brief This file contains structures and definitions to deal with date and time intervals
*/

#ifndef  __TERRALIB_INTERNAL_TIMEINTERVAL_H
#define  __TERRALIB_INTERNAL_TIMEINTERVAL_H

#include "TeTime.h" 

//! This class provides a set of functions for manipulating date and time intervals
class TL_DLL TeTimeInterval
{
private:

	TeTime  t1_;
	TeTime  t2_;
	TeChronon  intChronon_;

	void adjustIntervaltoChronon ();

public:
	
	//!	Set the date and time from the system clock
	void now(void) {t1_.now(); t2_.now();}

	//! Empty constructor
	TeTimeInterval (void): 	t1_(), t2_() {}

	//!	Set all time information from the system clock
	TeTimeInterval (TeTime& t1, TeTime& t2): 	t1_(t1), t2_(t2) {}

	//!	Copy constructor.
	TeTimeInterval(const TeTimeInterval& t): t1_(t.t1_), t2_(t.t2_) {}

	//!	Set date and time from arguments, though the time fields defaults to zero.
	TeTimeInterval(const std::string& t1, const std::string& t2, TeChronon chronon=TeSECOND, const std::string& mask="YYYYsMMsDDsHHsmmsSS", const std::string& dateS = "/", const std::string& timeS = ":", const std::string& indPM = "PM");

	//!	Set date and time from arguments, though the time fields defaults to zero.
	TeTimeInterval(const std::string& t1, TeChronon chronon=TeSECOND, const std::string& mask="YYYYsMMsDDsHHsmmsSS", int delta = 0, int numsteps = 1): 
		t1_ (t1,  chronon, mask)
	{
		t2_ = t1_ + (delta-1)*numsteps; 
		adjustIntervaltoChronon();
	}
 
	//! Returns the chronon of the interval
	TeChronon intervalChronon() { return  intChronon_; }

	//! Sets the chronon of the interval
	void intervalChronon(TeChronon c);  

	//!	Return a time of day std::string in format "hh:mm:ss"
	std::string getInitialTime()			{return t1_.getTime();}

	//!	Return a time of day std::string in format "hh:mm:ss"
	std::string getFinalTime()			{return t2_.getTime();}

	//!	Return a std::string to the date in the form "yyyy-mm-dd"
	std::string getInitialDate(void)		{return t1_.getDate();}

	//!	Return a std::string to the date in the form "yyyy-mm-dd"
	std::string getFinalDate(void)		{return t2_.getDate();}

	//! Return the time t1_
	TeTime& getT1 (void) {return t1_;}

	//! Return the time t2_
	TeTime& getT2 (void) {return t2_;}

	//!	Return a pointer to the date and time in the form passed in mask.  
	std::string getInitialDateTime (const std::string& mask="YYYYsMMsDDsHHsmmsSS", const std::string& dateS="/", const std::string& timeS=":", const std::string& indPM="PM", const std::string& indAM="AM") {return t1_.getDateTime(mask, dateS, timeS, indPM, indAM);}

	//!	Return a pointer to the date and time in the form passed in mask.  
	std::string getFinalDateTime (const std::string& mask="YYYYsMMsDDsHHsmmsSS", const std::string& dateS="/", const std::string& timeS=":", const std::string& indPM="PM", const std::string& indAM="AM") {return t2_.getDateTime(mask, dateS, timeS, indPM, indAM);}

	
	//! Reset time to the specified arguments. Return time_t: number of seconds since 0:00:00 Jan 1 1987
	/*!
      \param y  year
	  \param m  month
	  \param d  day
	  \param h  hour
	  \param min  minutes
      \param s  seconds
	*/
	time_t setInitialTime (int y, int m, int d, int h = 0, int min = 0, int s = 0)
	{return t1_.Set(y, m, d, h, min, s);}


	//! Reset time to the specified arguments. Return time_t: number of seconds since 0:00:00 Jan 1 1987
	/*!
      \param y  year
	  \param m  month
	  \param d  day
	  \param h  hour
	  \param min  minutes
      \param s  seconds
	*/
	time_t setFinalTime (int y, int m, int d, int h = 0, int min = 0, int s = 0) 
	{return t2_.Set(y, m, d, h, min, s);}

	//!	Assignment operator for TeTime objects.
	TeTimeInterval& operator=(const TeTimeInterval& t) {t1_ = t.t1_;  t2_ = t.t2_; intChronon_ = t.intChronon_; return (*this);}

	//! Operator ==
	bool operator==(const TeTimeInterval& other) const { return (this->t1_ == other.t1_ && this->t2_ == other.t2_); } 

	bool operator<(const TeTimeInterval& other) const { return (this->t1_ < other.t1_ && this->t2_ < other.t2_); } 

	//! Compute legnth of time interval in units of chonon
	int length ();

	//! Shift the interval, begining at time t (in the form "yyyy-mm-dd hh:mm:ss").
	/*!
		If t is not provided, the current final time becomes the initial time. 
		The interval length is maintained.
	*/
	void shift (const std::string& t = "");

	
	//! Shift the interval, begining delta units after the initial time. The interval length is maintained.
	void shiftPlus (int delta);

	//! Shift the interval, begining delta units before the initial time. The interval length is maintained.
	void shiftMinus (int delta);

	//! verify if the times have null values
	bool isValid() 
	{ return (t1_.isValid() || t2_.isValid()); } 
	
    //!	Normal destructor.
	~TeTimeInterval(void) { }

	//! Verify if a specific time is during the interval time
	bool  during (TeTime& time);

	//! Verify if a specific time is before the interval time
	bool  before (TeTime& time);

};

//! Standard output operator definition
TL_DLL std::ostream& operator<<(std::ostream& os, TeTime& N);

#endif


