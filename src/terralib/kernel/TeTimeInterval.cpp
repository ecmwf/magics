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

#include "TeTimeInterval.h"

TeTimeInterval :: TeTimeInterval(const std::string& s1, const std::string& s2, TeChronon chronon, 
								 const std::string& mask, const std::string& dateS, const std::string& timeS, 
								 const std::string& indPM)
{
	intChronon_ = chronon;
	//build t1 and t2 with the chronon parameter 
	TeTime t1 (s1, chronon, mask, dateS, timeS, indPM);
	TeTime t2 (s2, chronon, mask, dateS, timeS, indPM);
	t1_ = t1;
	t2_ = t2;
	adjustIntervaltoChronon();

}

void 
TeTimeInterval::intervalChronon(TeChronon c)
{ 
	intChronon_ = c; 
	t1_.chronon(c); 
	t2_.chronon(c); 
	adjustIntervaltoChronon();
}

void  
TeTimeInterval::adjustIntervaltoChronon ()
{
	int m2 = t2_.month();
	int d2 = 31;
	int d1 = 1;

	if ((m2 == 4) || (m2 == 6) || (m2 == 9) || (m2 == 11))  //april, june, september, november
		d2 = 30;
	if (m2 == 2) //february
		d2 = 28;

	switch (intChronon_)
	{
	case TeYEAR :		
	    t1_.Set(t1_.year(), 1 , 1, 0, 0, 0);
		t2_.Set(t2_.year(), 12, 31, 23, 59, 59);
	   break;
	case TeMONTH :
		t1_.Set(t1_.year(), t1_.month(), d1, 0, 0, 0);
	    t2_.Set(t2_.year(), t2_.month(), d2, 23, 59, 59);
	   break;
	case TeDAY :
		t1_.Set(t1_.year(), t1_.month(), t1_.day(), 0, 0, 0);
	    t2_.Set(t2_.year(), t2_.month(), t2_.day(), 23, 59, 59);
	   break;
	case TeHOUR :
		t1_.Set(t1_.year(), t1_.month(), t1_.day(), t1_.hour(), 0, 0);
	    t2_.Set(t2_.year(), t2_.month(), t2_.day(), t2_.hour(), 59, 59);
	   break;
	case TeMINUTE :
		t1_.Set(t1_.year(), t1_.month(), t1_.day(), t1_.hour(), t1_.minute(), 0);
	    t2_.Set(t2_.year(), t2_.month(), t2_.day(), t2_.hour(), t2_.minute(), 59);
        break;
    default:
        break;
	}	
}


int 
TeTimeInterval :: length ()
{
	return 1;
}

void  
TeTimeInterval :: shift (const std::string& t)
{
	TeTime new_t1; 
	if (t == "")  
		TeTime new_t1 (t2_);  
	else		  
		TeTime new_t1 (t, t2_.chronon(), "YYYYsMMsDDsHHsmmsSS");

	TeTime new_t2 = new_t1 + length();

	t1_ = new_t1;
	t2_ = new_t2;
	adjustIntervaltoChronon();
}


void  
TeTimeInterval :: shiftPlus (int delta)
{
	t1_ += delta;
	t2_ = t1_ + (delta - 1);
	adjustIntervaltoChronon();
}


void  
TeTimeInterval :: shiftMinus (int delta)
{
	t1_ -= delta;  
	t2_ = t1_ + (-delta); 
	adjustIntervaltoChronon();
}

bool   
TeTimeInterval :: during (TeTime& time)
{
	if(intChronon_==TeMONTHOFYEAR)
		return ((this->getT1().month()<=time.month()) && (this->getT2().month()>=time.month()));
	
	if (intChronon_==TeDAYOFMONTH)
		return ((this->getT1().day()<=time.day()) && (this->getT2().day()>=time.day()));

	if (intChronon_== TeDAYOFWEEK)
		return ((this->getT1().weekDay()<=time.weekDay()) && (this->getT2().weekDay()>=time.weekDay())); 

	if (intChronon_== TeDAYOFYEAR)
		return ((this->getT1().yearDay()<=time.yearDay()) && (this->getT2().yearDay()>=time.yearDay())); 
		
	if (intChronon_== TeHOUROFDAY)
		return ((this->getT1().hour()<=time.hour()) && (this->getT2().hour()>=time.hour())); 
		
	if (intChronon_==TeMINUTEOFHOUR)
		return ((this->getT1().minute()<=time.minute()) && (this->getT2().minute()>=time.minute())); 
	
	if (intChronon_==TeSECONDOFMINUTE)
		return ((this->getT1().second()<=time.second()) && (this->getT2().second()>=time.second())); 

	return ((this->getT1()<=time) && (time<=this->getT2())); 
}

bool   
TeTimeInterval :: before (TeTime& time)
{
	if(intChronon_==TeMONTHOFYEAR)
		return (time.month() < this->getT1().month());
	
	if (intChronon_==TeDAYOFMONTH)
		return (time.day() < this->getT1().day());

	if (intChronon_== TeDAYOFWEEK)
		return (time.weekDay() < this->getT1().weekDay());

	if (intChronon_== TeDAYOFYEAR)
		return (time.yearDay() < this->getT1().yearDay());
		
	if (intChronon_== TeHOUROFDAY)
		return (time.hour() < this->getT1().hour());
		
	if (intChronon_==TeMINUTEOFHOUR)
		return (time.minute() < this->getT1().minute());
	
	if (intChronon_==TeSECONDOFMINUTE)
		return (time.second() < this->getT1().second());

	return (time < this->getT1());
}
