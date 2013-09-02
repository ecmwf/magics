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

// Baudouin Raoult - ECMWF Sep 96

#ifndef DateTime_H
#define DateTime_H

#include "magics.h"

#include <time.h>


namespace magics {
typedef long Second;

class MagDate {
public:

// -- Contructors

	MagDate() : julian_(0) { }
	MagDate(long);
	MagDate(long,long,long);
	MagDate(const string&);
	MagDate(long,long);

    

	MagDate(const MagDate& other):
		julian_(other.julian_) {}

	MagDate& operator=(const MagDate& other)
		{ julian_ = other.julian_; return *this; }

// -- Destructor

	~MagDate() {}

// -- Convertors
	
	operator string() const;
    operator tm() const;

// -- Operators

	bool operator==(const MagDate& other) const
		{ return julian_ == other.julian_ ;}

	bool operator!=(const MagDate& other) const
		{ return julian_ != other.julian_ ;}

	bool operator<(const MagDate& other)  const
		{ return julian_ <  other.julian_ ;}

	bool operator>(const MagDate& other)  const
		{ return julian_ >  other.julian_ ;}

	bool operator<=(const MagDate& other)  const
		{ return julian_ <=  other.julian_ ;}

	bool operator>=(const MagDate& other)  const
		{ return julian_ >=  other.julian_ ;}


	MagDate& operator++() { julian_++; return *this; }
	MagDate& operator--() { julian_--; return *this; }

	MagDate& operator+=(long d) { julian_ += d; return *this; }
	MagDate& operator-=(long d) { julian_ -= d; return *this; }

// -- Methods

	long year() const;
	long month() const;
	long day() const;
	long yyyymmdd() const;

	long julian() const { return julian_;                  }
	MagDate round(int n)   { return MagDate((julian_/n)*n,true); }
	string monthName(long n);
    

// -- Class methods

	static long         parse(const string&);

// -- Friends

	friend ostream& operator<< (ostream& s, const MagDate& date)
		{ date.print(s); return s; }

protected:

	MagDate(long julian,bool) : julian_(julian) {}

// -- Members
	// None

// -- Methods
	// None

// -- Overridden methods
	// None

// -- Class members
	// None

// -- Class methods
	// None

private:

// -- Members
	
	long julian_;

// -- Methods

	void print(ostream&) const;

// -- Class methods

	static long julianToMagDate(long);
	static long dateToJulian(long);
	static long today();

// -- Friends

	friend long operator-(const MagDate& d1, const MagDate& d2)
		{ return (d1.julian_ - d2.julian_); }

//	friend long operator-(const MagDate& d1)
//		{ NOTIMP; return 0; }

	friend MagDate operator+(const MagDate& d1, const long n)
		{ return MagDate::julianToMagDate(d1.julian_ + n); }

	friend MagDate operator+(const long n, const MagDate& d1)
		{ return d1+n; }

	friend MagDate operator+(const MagDate& d1, const MagDate& d2)
		{ return MagDate::julianToMagDate(d1.julian_ + d2.julian_); }


	friend class DateTime;
};


class MagTime {

public:

// -- MagExceptions
	// None

// -- Contructors

	MagTime(long, long, long);
	MagTime(long seconds = 0);
	MagTime(const string&);


// -- Copy

	MagTime(const MagTime&);
	MagTime& operator=(const MagTime&);

// -- Destructor

	~MagTime();

// -- Convertors

	operator string() const;
	operator Second() const { return seconds_; }

// -- Operators

	bool operator==(const MagTime& other) const
		{ return seconds_ == other.seconds_; }

	bool operator!=(const MagTime& other) const
		{ return (seconds_ != other.seconds_); }

	bool operator>(const MagTime& other) const
		{ return (seconds_ > other.seconds_); }

	bool operator<(const MagTime& other) const
		{ return (seconds_ < other.seconds_); }

	bool operator>=(const MagTime& other) const
		{ return (seconds_ >= other.seconds_); }

	bool operator<=(const MagTime& other) const
		{ return (seconds_ <= other.seconds_); }

	Second operator-(const MagTime& other) const
		{ return seconds_ - other.seconds_; }

//	MagTime operator+(const MagTime& other) const
//		{ return seconds_ + other.seconds_; }

	MagTime& operator+=(const Second& sec)
		{ seconds_ += sec; return *this; }	

	MagTime& operator-=(const Second& sec)
		{ seconds_ -= sec; return *this; }	

// -- Methods
	long hours() const;
	long minutes() const;
	long seconds() const;
	long hhmmss() const;


// -- Class Methods

	static MagTime now();

protected:

// -- Methods

	void print(ostream&) const;

private:

// -- Members

	Second seconds_;

	friend ostream& operator<<(ostream& s,const MagTime& t)
		{ t.print(s); return s; }

};



class XmlNode;

class DateTime {
public:

// -- Contructors

	DateTime(time_t = ::time(0));
	DateTime(const MagDate&, const MagTime&);
	DateTime(const string&);
	string tostring(const string&) const;
	
	DateTime* clone() const { return new DateTime(date(), time()); }
	void set(const XmlNode&) { }
	void set(const map<string, string>&) { }
	 virtual void toxml(ostream&) const {
   		
    }   


// -- Destructor

	virtual ~DateTime() {}

// -- Operators

	bool operator<(const DateTime& other) const
		{ return (date_ == other.date_)
			?(time_ < other.time_)
			:(date_ < other.date_); }

	bool operator==(const DateTime& other) const
		{ return (date_ == other.date_) && (time_ == other.time_); }

	bool operator!=(const DateTime& other) const
		{ return (date_ != other.date_) || (time_ != other.time_); }

	bool operator>(const DateTime& other) const
		{ return (date_ == other.date_)
			?(time_ > other.time_)
			:(date_ > other.date_); }

	bool operator>=(const DateTime& other) const
		{ return !(*this < other); }

	bool operator<=(const DateTime& other) const
		{ return !(*this > other); }

	DateTime& operator=(const DateTime&);

	Second operator-(const DateTime&) const;
	DateTime operator+(const Second&) const;

	operator string() const;
    operator tm() const;
    
    operator double() const;

// -- Methods

	const MagDate& date() const { return date_; }
	const MagTime& time() const { return time_; }

	DateTime round(const Second& seconds) const;



protected:

// -- Members

	MagDate date_;
	MagTime time_;

// -- Methods
	// None

// -- Overridden methods
	// None

// -- Class members
	// None

// -- Class methods
	// None

private:

// -- Members
	// None

// -- Methods

	void print(ostream&) const;

// -- Class methods

// -- Friends

	friend ostream& operator<<(ostream& s,const DateTime& p)
		{ p.print(s); return s; }
};

}
#endif
