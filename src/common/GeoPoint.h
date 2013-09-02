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

// File UserPoint
// Sylvie Lamy-Thepaut ECMWF 2002
#ifndef UserPoint_H
#define UserPoint_H

#include "magics.h"
#include <cmath>
#include <iomanip>
#include <stack>

namespace magics {

class UserPoint {

public:

	UserPoint(double lon, double lat, double val = 0, 
			bool missing = false, bool border = false) :
        y_(lat), x_(lon), height_(0), 
        value_(val), colour_(val), marker_(val), size_(val),
        high_(false), low_(false), missing_(missing), border_(border) {}
    
    UserPoint() :
        y_(0), x_(0), height_(0), 
        value_(0), colour_(0), marker_(0), size_(0),
        high_(false), low_(false), missing_(false), border_(false)  {}
	virtual ~UserPoint() {}
    
	void latitude(double lat) { y_ = lat; }
	void longitude(double lon) { x_ = lon; }
	void height(double height) { height_ = height; }
	void value(double val) { value_ = val; }
	void colour(double col) { colour_ = col; }
	void marker(double marker) { marker_ = marker; }
	void size(double size) { size_ = size; }
	void high(bool high) { high_ = high; }
	void low(bool low) { low_ = low; }
	void flagMissing() { missing_ = true; }
	void flagBorder() { border_ = true; }

	double y_;
	double x_;
	double latitude() const { return y_; }
	double longitude() const { return x_; }
	double height() const { return height_; }
	double value() const { return value_; }
	double colour() const { return colour_; }
	double marker() const { return marker_; }
	double size() const { return size_; }
	bool high() const { return high_; }
	bool low() const { return low_; }
	bool missing() const { return missing_; }
	bool border() const { return border_; }
	bool ignore() const { return missing_ || border_; }
    
	inline double y() const { return y_; }
	inline double x() const { return x_; }
    
	bool in(double left, double right, double bottom, double top, std::stack<UserPoint>& points) const
	{
		
		UserPoint point = *this;
		if (y_ > top)
		{
			return false;
		}
		if (y_ < bottom)
		{
			return false;
		}
        
		while ( point.x_ < left ) 
			point.x_ += 360;
		while ( point.x_ > left + 360. )             
			point.x_ -= 360.;

		if ( point.x_ < left )
		{
			//MagLog::dev() << longitude_ << "<" << left << "\n";
			return false;
		}
		if (point.x_ > right )
		{
			//MagLog::dev() << longitude_ << ">" << right << "\n";
			return false;
		}
		
		points.push(point);
		while (true) {
			point.x_ += 360;
			if ( point.x_ >  left && point.x_ < right) {
				points.push(point);
			}
			else 
				break;
		}
				
		return true;
	}

	bool wrapPB(const UserPoint& p) const
	{ 
		return abs(x_ - p.x_) > 180; 
	}
    
	bool operator==(const UserPoint& other) const
	{
		return ( same(other.x_, x_) &&  
                 same(other.y_, y_) &&  
                 same(other.height_, height_) &&  
                 same(other.value_, value_));
	}
	bool operator!=(const UserPoint& other) const
	{
        return ( !same(other.x_, x_) || 
                 !same(other.y_, y_) || 
                 !same(other.height_, height_) || 
                 !same(other.value_, value_));
	}
    
	bool  operator<(const UserPoint& other) const
	{
		if (other == *this) return false;
		return (same(other.x_,x_)) ? (other.y_ < y_) : (x_ < other.x_ );
	}

	UserPoint left()const
	{ 
		return UserPoint(x_ - 360, y_, value_, height_); 
	}

	UserPoint right() const
	{  
		return UserPoint(x_  + 360, y_, value_, height_); 
	}
    
	UserPoint shift(double left, double right) const
	{ 
		UserPoint point(*this);
		if ( left <= x_ &&  x_ <= right) return point;
		while (point.x_ <= left && point.x_ <= right) 
			point.x_ += 360;
		while (point.x_ >= right && point.x_ > left) 
			point.x_ -= 360;
		return point;
	}

	string writeLongitude() const
	{
		ostringstream lon;
//		string ew = "#232"; // degree symbol
		string ew = "&#176;";
		UserPoint nice = shift(-180, 180);

		if ( nice.x_ < 0 ) ew += "W";
		if ( nice.x_ >= 0 ) ew += "E";
		float x = float(maground(abs(nice.x_)*100)) / 100;
		lon << x << ew;
		return  lon.str();    
	} 

	string writeLatitude() const
	{
		ostringstream lat;
		//string ns = "#232"; // degree symbol
		string ns = "&#176;";
		if ( y_ < 0 ) ns += "S";
		if ( y_ >= 0 ) ns += "N";
		float y = float(maground(abs(y_)*100)) / 100;
		lat <<  y << ns;
		return  lat.str();    
	} 

protected:
	virtual void print(ostream& out) const 
	{ 
		out << "UserPoint[";
		out << x_ << "(lon), ";
		out << y_ << "(lat), ";
		out << height_ << "(height), ";
		out << value_ << "(val)]";
		out << colour_ << "(colour)]";
	}

private:

	double height_;
	double value_;
	double colour_;
	double marker_;
	double size_;
	bool high_;
	bool low_;
	bool missing_;
	bool border_;

// -- Friends
	friend ostream& operator<<(ostream& s,const UserPoint& p)
		{ p.print(s); return s; }
};

}
#endif
