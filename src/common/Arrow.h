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

/*! \file Arrow.h
    \brief Definition of the Template class Arrow.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mar-2005

*/

#ifndef Arrow_H
#define Arrow_H

#include "magics.h"

#include "BasicGraphicsObject.h"
#include "BaseDriver.h"
#include "ArrowProperties.h"

namespace magics {


class ArrowPoint 
{
public:
	ArrowPoint(double x, double y, const PaperPoint& point) :
		 x_(x), y_(y), point_(point) {}
	ArrowPoint(const PaperPoint& point) :
		 x_(0), y_(0), point_(point) {}
	double x_;
	double y_;
	PaperPoint point_;

	void set(double speed, double direction)
	{
		x_ = speed * cos(RAD(270.-direction));
		y_ = speed * sin(RAD(270.-direction));

	}
	double norm()  const {return sqrt((x_ * x_) + (y_ * y_));}
	double angle() const {return atan2(y_,x_);}// 0 = east, clockwise

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream& out) const
	{
		out << "Arrow[" << point_.x() << ", " << point_.y() << ", " << x_ << ", " << y_ << "]" << "\n";
	}

private:	   
	friend ostream& operator<<(ostream& s,const ArrowPoint& p)
		{ p.print(s); return s; } 
};


class Arrow: public BasicGraphicsObject, public ArrowProperties, public vector<ArrowPoint> {

public:
	Arrow() {}
	virtual ~Arrow() {}

	virtual void redisplay(const BaseDriver& driver) const { driver.redisplay(*this); }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const {}

private:
	//! Copy constructor - No copy allowed
	Arrow(const Arrow&);
	//! Overloaded << operator to copy - No copy allowed
	Arrow& operator=(const Arrow&);

// -- Friends
	friend class BaseDriver;
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Arrow& p)
		{ p.print(s); return s; }
};

} // namespace magics

#endif
