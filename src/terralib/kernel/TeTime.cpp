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

#include <TeTime.h>
#include <memory.h>
#include <TeUtils.h>
#include <cstdlib>
#include <iostream>

//return the number of the month
int getMonth(const std::string& month)
{
	std::string tempM = TeConvertToUpperCase(month);
	if(tempM=="JAN")
		return 0;
	else if(tempM=="FEB")
		return 1;
	else if(tempM=="MAR")
		return 2;
	else if(tempM=="APR")
		return 3;
	else if(tempM=="MAY")
		return 4; 
	else if(tempM=="JUN")
		return 5;
	else if(tempM=="JUL")
		return 6;
	else if(tempM=="AUG")
		return 7;
	else if(tempM=="SEP")
		return 8;
	else if(tempM=="OCT")
		return 9;
	else if(tempM=="NOV")
		return 10;
	else if(tempM=="DEC")
		return 11;

	return -1;
}

//return the month 
std::string getMonth(int i)
{
	if(i==0)
		return "Jan";
	else if(i==1)
		return "Feb";
	else if(i==2)
		return "Mar";
	else if(i==3)
		return "Apr";
	else if(i==4)
		return "May";
	else if(i==5)
		return "Jun";
	else if(i==6)
		return "Jul";
	else if(i==7)
		return "Aug";
	else if(i==8)
		return "Sep";
	else if(i==9)
		return "Oct";
	else if(i==10)
		return "Nov";
	else if(i==11)
		return "Dec";

	return "";
}


TeTime::TeTime()
{
    /*
	set the tm_isdst field to 0 to indicate that standard time is in effect, 
	or to a value greater than 0 to indicate that daylight savings time is in effect, 
	or to a value less than zero to have the C run-time library code compute whether 
	standard time or daylight savings time is in effect. 
	(The C run-time library assumes the United States’s rules for implementing 
	the calculation of Daylight Saving Time).
	*/

	chronon_ = TeSECOND;
	ts_.tm_sec = 0;
	ts_.tm_min = 0;
	ts_.tm_hour = 0;
	ts_.tm_isdst = -1;
	ts_.tm_mday = 1;
	ts_.tm_mon = 0;
	ts_.tm_year = 0;
}

TeTime::TeTime(const TeTime& other)
{
	memcpy(&ts_, &other.ts_, sizeof(struct tm));
	last_ = other.last_;
	chronon_ = other.chronon_;
}

TeTime::TeTime(int h, int m, int s, TeChronon chronon)
{
	now();
	ts_ = *localtime(&last_);
	ts_.tm_sec = s;
	ts_.tm_min = m;
	ts_.tm_hour = h;
	ts_.tm_isdst = -1;
	ts_.tm_mday = 1;
	ts_.tm_mon = 0;
	ts_.tm_year = 0;
	last_ = mktime(&ts_);
	chronon_ = chronon;
}

TeTime::TeTime(const std::string& ds, TeChronon chronon, const std::string& mask, const std::string& dateS, const std::string& timeS, const std::string& indPM)
{
	chronon_ = chronon;
	ts_.tm_sec = 0;
	ts_.tm_min = 0;
	ts_.tm_hour = 0;
	ts_.tm_isdst = -1; 
	ts_.tm_mday = 1;
	ts_.tm_mon = 0;
	ts_.tm_year = 0;
	
	bool flag = true;
	std::string tempDT, tempM;
	std::string maskTemp = mask;
	
	int posBeginMask = 0;
	int posBeginDate = 0;
	int posEndMask;
	int posEndTotalMask = 0;
	
	if (mask == "YYYYMMDD" || mask == "yyyymmdd" || mask == "aaaammdd" || mask == "AAAAMMDD")
	{
		sscanf(ds.c_str(),"%4d%2d%2d",&ts_.tm_year,&ts_.tm_mon,&ts_.tm_mday);
	}
	else if (mask == "DDMMAAAA" || mask == "ddmmaaaa" || mask == "DDMMYYYY" || mask == "ddmmyyyy")
	{
		sscanf(ds.c_str(),"%2d%2d%4d",&ts_.tm_mday,&ts_.tm_mon,&ts_.tm_year);
	}
	else if (mask == "DDAAAAMM" || mask == "ddaaaamm" || mask == "DDYYYYMM" || mask == "ddyyyymm")
	{
		sscanf(ds.c_str(),"%2d%4d%2d",&ts_.tm_mday,&ts_.tm_year,&ts_.tm_mon);
	}
	else if (mask == "MMDDAAAA" || mask == "mmddaaaa" || mask == "MMDDYYYY" || mask == "mmddyyyy")
	{
		sscanf(ds.c_str(),"%2d%4d%2d",&ts_.tm_mon,&ts_.tm_mday,&ts_.tm_year);
	}
	else if (mask == "AAAADDMM" || mask == "aaaaddmm" || mask == "YYYYDDMM" || mask == "yyyyddmm")
	{
		sscanf(ds.c_str(),"%4d%2d%2d",&ts_.tm_year,&ts_.tm_mday,&ts_.tm_mon);
	}
	else
	{
		while(flag)
		{
			posEndMask = maskTemp.find("s");
			posEndTotalMask += posEndMask;
			
			if(posEndMask==-1)
			{
				flag = false;
				posEndTotalMask = mask.size();
			}
			else
			{
				if(posBeginMask>0)
					++posEndTotalMask;
				
				maskTemp = maskTemp.substr(posEndMask+1);
			}

			tempDT.clear();
			tempM = mask.substr(posBeginMask,(posEndTotalMask-posBeginMask));

			//fill the datetime
			std::string dt = ds.substr(posBeginDate,1);
			while((dt!=dateS) && (dt!=timeS) && (dt!=" ") && (posBeginDate < (int)ds.size()))
			{
				tempDT += dt;
				++posBeginDate;
				dt = ds.substr(posBeginDate,1);
			}
			++posBeginDate;
			
			//day
			if(tempM.find(68)==0)
				ts_.tm_mday = atoi(tempDT.c_str());
			
			//month
			else if (tempM.find(77)==0)
			{
				if(tempDT.size()>2)
					ts_.tm_mon = getMonth(tempDT)+1;
				else
					ts_.tm_mon = atoi(tempDT.c_str());
			}

			//year
			else if (tempM.find(89)==0)
			{
				ts_.tm_year = atoi(tempDT.c_str());
				if(ts_.tm_year < 100) 
					ts_.tm_year += 1900; 
			}

			//hour
			else if (tempM.find(72)==0)
				ts_.tm_hour = atoi(tempDT.c_str());
			
			//minute
			else if (tempM.find(109)==0)
				ts_.tm_min = atoi(tempDT.c_str());
			
			//second
			else if (tempM.find(83)==0)
				ts_.tm_sec = atoi(tempDT.c_str());
			
			//PM or AM indicator
			else if ((tempM.find(84)==0) && (tempDT==indPM))
				ts_.tm_hour += 12;
				
			posBeginMask = posEndTotalMask+1;

			if((posBeginDate)>(int)(ds.size()-1) || (posBeginMask)>(int)(mask.size()-1))
				flag = false;
		}
	}
			
	switch (chronon_) 
	{
	case TeYEAR :
		ts_.tm_mon = 1;
        break;
	case TeMONTH :
		ts_.tm_mday = 1;
        break;
	case TeDAY :
		ts_.tm_hour = 0;
        break;
	case TeHOUR :
		ts_.tm_min = 0;
        break;
	case TeMINUTE :
		ts_.tm_sec = 0;
        break;
	default:
        break;
	}
    
	ts_.tm_year -= 1900;
    --ts_.tm_mon;

	last_ = mktime(&ts_);
	if (last_ != time_t(-1))
		ts_ = *localtime(&last_);
}

time_t TeTime::now(void)
{
	time(&last_);
	ts_ = *localtime(&last_);
	return last_;
}

std::string
TeTime::getTime() 
{
	char tbuf[9];
    sprintf(tbuf, "%02d:%02d:%02d",
                                ts_.tm_hour,ts_.tm_min,ts_.tm_sec);
    return tbuf;
}


std::string
TeTime::getDate()  
{
	char dbuf[12];
	sprintf(dbuf,"%4d-%02d-%02d",ts_.tm_year+1900,ts_.tm_mon+1,ts_.tm_mday);
	return dbuf;
}

std::string
TeTime::getDateTime (const std::string& mask, const std::string& dateS, const std::string& timeS, const std::string& indPM, const std::string& indAM)
{
	std::string result = "";

	bool flag = true;
	std::string tempM;
	std::string maskTemp = mask;
	
	int posBeginMask = 0;
	int posEndMask;
	int posEndTotalMask = 0;

	bool firstTime = true;
	bool pm = false;
	bool am = false;
 
	//verify if the mask is AM and PM
	std::string tempAPM = mask.substr(mask.size()-1, 1);
	if(tempAPM=="T")
	{
		if((ts_.tm_hour>12) || ((ts_.tm_hour==12) && (ts_.tm_min>0)) ||
			((ts_.tm_hour==12) && (ts_.tm_min==0) && (ts_.tm_sec>0)))
			pm=true;
		else
			am=true;
	}

	while(flag)
	{
		posEndMask = maskTemp.find("s");
		posEndTotalMask += posEndMask;

		if(posEndMask==-1)
		{
			flag = false;
			posEndTotalMask = mask.size();
		}
		else
		{
			if(posBeginMask>0)
				++posEndTotalMask;

			maskTemp = maskTemp.substr(posEndMask+1);
		}

		tempM = mask.substr(posBeginMask,(posEndTotalMask-posBeginMask));

		//day
		if(tempM.find(68)==0)
			result +=  Te2String(ts_.tm_mday) + dateS;
		
		//month
		else if (tempM.find(77)==0)
		{
			if(tempM=="MMM")
				result += getMonth(ts_.tm_mon) + dateS;
			else
				result += Te2String(ts_.tm_mon+1) + dateS;
		}

		//year
		else if (tempM.find(89)==0)
		{
			if(tempM=="YYYY")
				result +=  Te2String(ts_.tm_year+1900) + dateS;
			else
			{
				std::string tempY = Te2String(ts_.tm_year);
				tempY = tempY.substr(tempY.size()-2, 2);
				result += tempY + dateS;
			}
		}

		//hour
		else if (tempM.find(72)==0)
		{
			if(firstTime)
				result.replace(result.size()-1, 1, " ");

			if(pm)
				result += Te2String(ts_.tm_hour-12) + timeS;
			else
				result += Te2String(ts_.tm_hour) + timeS;
			
			firstTime = false;
		}

		//minute
		else if (tempM.find(109)==0)
		{
			if(firstTime)
				result.replace(result.size()-1, 1, " ");

			result += Te2String(ts_.tm_min) + timeS;
			firstTime = false;
		}

		//second
		else if (tempM.find(83)==0)
		{
			if(firstTime)
				result.replace(result.size()-1, 1, " ");

			result += Te2String(ts_.tm_sec) + timeS;
			firstTime = false;
		}
		
		//PM or AM indicator
		else if (tempM.find(84)==0)
		{
			result.replace(result.size()-1, 1, " ");

			if(pm)
				result += indPM;
			else if(am)
				result += indAM; 
			
		}
		
		posBeginMask = posEndTotalMask+1;
	}

	if((!am) && (!pm))
		result.replace(result.size()-1, 1, " ");

	return result;
}

time_t 
TeTime::Set(int y, int m, int d, int h, int mt, int s)
{
    if (y > -1) ts_.tm_year = y-1900;
    if (m > -1) ts_.tm_mon = m-1;


    if (d > -1) ts_.tm_mday = d;
    if (h > -1) ts_.tm_hour = h;
    if (mt > -1) ts_.tm_min = mt;
    if (s > -1) ts_.tm_sec = s;
	ts_.tm_isdst =-1;
    last_ = mktime(&ts_);
    if (last_ == -1) last_ = 0;
    return last_;
}

TeTime& 
TeTime::operator=(const TeTime& other)
{
	memcpy(&ts_, &other.ts_, sizeof(struct tm));
	last_ = other.last_;
	chronon_ = other.chronon_;
	return *this;			
}


TeTime&
TeTime::operator+=(int delta) 
{
	if (delta == 0)
		return *this;

	int deltaux=delta;
	switch (chronon_)
	{
	case TeYEAR :
		ts_.tm_isdst = -1;
		ts_.tm_year += delta;	
		last_ = mktime(&ts_);
		break;
	case TeMONTH :
	case TeMONTHOFYEAR:
		ts_.tm_isdst = -1;
		ts_.tm_year += (ts_.tm_mon+delta)/12;
		ts_.tm_mon = (ts_.tm_mon+delta)%12;
		last_ = mktime(&ts_);
		break;
	case TeDAY :
	case TeDAYOFMONTH:
	case TeDAYOFWEEK:
	case TeDAYOFYEAR:
		deltaux *= 3600*24;
		last_ += deltaux;
		ts_ = *localtime(&last_);
		break;
	case TeHOUR :
	case TeHOUROFDAY :
		deltaux *= 3600;
		last_ += deltaux;
		ts_ = *localtime(&last_);
		break;
	case TeMINUTE :
	case TeMINUTEOFHOUR:
		deltaux *= 60;;
		last_ += deltaux;
		ts_ = *localtime(&last_);
		break;
	case TeSECOND :
	case TeSECONDOFMINUTE :
		last_ += deltaux;
		ts_ = *localtime(&last_);
		break;
	default :
		break;
	}

    return *this;
}

TeTime&
TeTime::operator++() 
{
	this->operator +=(1);
	return *this;
}

TeTime
TeTime::operator+(int delta)
{
	TeTime temp(*this);
    temp += delta;
    return temp;
}

TeTime&
TeTime::operator-=(int delta) 
{
    last_ -= delta;
    ts_ = *localtime(&last_);
    return *this;
}

bool 
TeTime::operator==(const TeTime& time) const
{
	return ((chronon_==time.chronon_) && (ts_.tm_sec==time.ts_.tm_sec) && (ts_.tm_min==time.ts_.tm_min) && 
		(ts_.tm_hour==time.ts_.tm_hour) && (ts_.tm_isdst==time.ts_.tm_isdst) && (ts_.tm_mday==time.ts_.tm_mday) &&
		(ts_.tm_mon==time.ts_.tm_mon) && (ts_.tm_year==time.ts_.tm_year));
}

bool 
TeTime::operator<(const TeTime& time) const  
{
	if(chronon_!=time.chronon_)
		return false;
	
	if(ts_.tm_year>time.ts_.tm_year)
		return false; 
	else if(ts_.tm_year<time.ts_.tm_year)
		return true;

	if(ts_.tm_mon>time.ts_.tm_mon)
		return false;
	else if (ts_.tm_mon<time.ts_.tm_mon)
		return true;

	if(ts_.tm_mday>time.ts_.tm_mday) 
		return false;
	else if (ts_.tm_mday<time.ts_.tm_mday) 
		return true;

	if(ts_.tm_hour>time.ts_.tm_hour) 
		return false;
	else if (ts_.tm_hour<time.ts_.tm_hour)
		return true;

	if(ts_.tm_min>time.ts_.tm_min) 
		return false;
	else if (ts_.tm_min<time.ts_.tm_min)
		return true;

	if(ts_.tm_sec>time.ts_.tm_sec) 
		return false;
	else if (ts_.tm_sec<time.ts_.tm_sec)
		return true;

	return false;
}

bool 
TeTime::operator<=(const TeTime& time) const 
{
	return (this->operator<(time) || this->operator==(time));
}

int 
TeTime::operator-(const TeTime& other)
{
	if(this->chronon_!=other.chronon_)
		return 0;
	int t = (int)this->last_- other.last_;
	return (t);
}

bool 
TeTime::isValid()  
{
	if(	(chronon_==TeSECOND) && (ts_.tm_sec==0) && (ts_.tm_min==0) && 
		(ts_.tm_hour==0) && (ts_.tm_mday==1) &&
		(ts_.tm_mon==0) && (ts_.tm_year==0))
		return false;
	else
		return true;
}

void 
TeTime::chronon (TeChronon c)
{
	if(chronon_==c)
		return;

	chronon_=c;
}


std::ostream& operator<<(std::ostream& os, TeTime& N)
{
	os << N.getDateTime();
	return os;
}




