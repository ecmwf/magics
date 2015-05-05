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

/*! \file BasePointsHandler.h
    \brief Definition of the Template base class AbstractPoints.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 16-Jan-2004
    
    Changes:
    
*/

#ifndef AbstractPoints_H
#define AbstractPoints_H

#include "magics.h"
#include "UserPoint.h"

#include "VectorOfPointers.h"
#include "MagException.h"

namespace magics {

class AbstractPoints;


class MinMaxHelper
{
public:
    MinMaxHelper(AbstractPoints& points);
    void operator()(const UserPoint& point)
    {
    	if ( point.missing() ) return;
        if (point.x() < minX_ ) minX_ = point.x();
        if (point.x() > maxX_ ) maxX_ = point.x();
        if (point.y() < minY_ ) minY_ = point.y();
        if (point.y() > maxY_ ) maxY_ = point.y();
        
        if (point.value() < min_ ) min_ = point.value();
        if (point.value() > max_ ) max_ = point.value();
     
    }
    double minX_;
    double maxX_;
    double minY_;
    double maxY_;
    double min_;
    double max_;
};

class BasePointsHandler 
{
public:
	BasePointsHandler() {}
	virtual ~BasePointsHandler() { };
	virtual double minX() const { return INT_MIN; }
	virtual double minY() const { return INT_MIN; }
	virtual double maxX() const { return INT_MAX; }
	virtual double maxY() const { return INT_MAX; }
	virtual double min() const  { return INT_MIN; }
	virtual double max() const  { return INT_MIN; }
};


class AbstractPoints : public BasePointsHandler {

public:
	AbstractPoints() : helper_(0) {};
	virtual ~AbstractPoints() { if (helper_) delete helper_; };
	//! Method to go through the collection. The Class 0 needs to define 
	// void operator()(const P&)
	template <class O>
	void for_each(O& object)
	{
		setToFirst();
		while (more()) {
		  object(current());
		  advance();
		}  
	}
	//! Method to set the current position to the first point.(abstract)
	virtual void setToFirst()  {ASSERT(false);};
	//! Method to test the end of collection.
	virtual bool more()  {ASSERT(false);return false;};
	//! Method to return the current value
	virtual const UserPoint& current()  {ASSERT(false);};
	//! Method to advance
	virtual void advance()  {ASSERT(false);};

	virtual int size()  {
		points_ = 0;
		for_each(*this);
		return points_;
	}

	virtual double minX()  {
		if ( !helper_ ) helper_ = new MinMaxHelper(*this);
		return helper_->minX_;
	}
	virtual double minY()  {
		if ( !helper_ ) helper_ = new MinMaxHelper(*this);
		return helper_->minY_;
	}
	virtual double maxX()  {
		if ( !helper_ ) helper_ = new MinMaxHelper(*this);
		return helper_->maxX_;
	}
	virtual double maxY()  {
		if ( !helper_ ) helper_ = new MinMaxHelper(*this);
		return helper_->maxY_;
	}
	virtual double min()  {
		if ( !helper_ ) helper_ = new MinMaxHelper(*this);
		return helper_->min_;
	}
	virtual double max()  {
		if ( !helper_ ) helper_ = new MinMaxHelper(*this);
		return helper_->max_;
	}

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { out << "AbstractPoints"; }
	void operator()(const UserPoint&) const { points_++; }
	mutable MinMaxHelper* helper_;
	mutable int points_;

private:
	//! Copy constructor - No copy allowed
	AbstractPoints(const AbstractPoints&);
	//! Overloaded << operator to copy - No copy allowed
	AbstractPoints& operator=(const AbstractPoints&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AbstractPoints& p)
		{ p.print(s); return s; }
};


class PointsList : public vector<UserPoint*>, public AbstractPoints {

public:
	PointsList()  {}
	~PointsList() {}
	// Implements the AbstractPoints interface
	virtual void setToFirst() 	{ current_ = this->begin(); }
	virtual bool more() 	{ return current_ != this->end(); }
	virtual UserPoint& current() { return **current_; }
	virtual void advance() 	{ current_++; }
	virtual int size() 	{ return vector<UserPoint*>::size(); }


protected :
	mutable  vector<UserPoint*>::const_iterator current_;
};

} // namespace magics

#endif
