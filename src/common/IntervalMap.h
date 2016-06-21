/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IntervalMap.h
    \brief Definition of the Template class IntervalMap
    
    Magics Team - ECMWF 2004
    
    Started: Tue 22-Jun-2004
    
    Changes:
    
*/
 
#ifndef IntervalMap_H
#define IntervalMap_H

#include "magics.h"

namespace magics {

class Interval
{
public:
    Interval(double min = 0, double max = 0) : min_(min), max_(max) {}
    Interval(const Interval& other) : min_(other.min_), max_(other.max_) {}
    virtual ~Interval() {}
    virtual bool between(double val) const
    { 
        //MagLog::debug() << min_ << "<" << val << "<" << max_ << "?" << "\n";
    	if(same(min_,val)) return true;
        return (min_ < val && val < max_); 
    }
    bool  operator<(const Interval& other) const
    {
        if ( other.min_ == min_ ) 
            return other.max_ > max_;
        else 
            return other.min_ > min_;
    }
    double min_;
    double max_;
    virtual void print(ostream& out) const { out << "[" << min_ << ", " << max_ << "]"; } 
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Interval& p)
		{ p.print(s); return s; }
};


class OutOfRangeMagException : public MagicsException
{
public:
	OutOfRangeMagException():MagicsException("OutOfRangeMagException"){}
}; 


template <class C>
class IntervalMap : public map<Interval, C>
{
	
public:
	IntervalMap() {}
	~IntervalMap() {}
	
	bool accept(double val) const
		{
			for (typename map<Interval, C>::const_iterator interval = this->begin(); interval != this->end(); ++interval)
			{
				if (interval->first.between(val))
					return true;
			}
			//MagLog::warning() << "IntervalMap - Could not find value "<< val <<" in interval!" << endl;
			return false;
		}

	const C& find(double val, const C& def) const
	{
		for (typename map<Interval, C>::const_iterator interval = this->begin(); interval != this->end(); ++interval)
		{
			if (interval->first.between(val))
				return interval->second;
		}
		//MagLog::warning() << "IntervalMap - Could not find value "<< val <<" in interval!" << endl;
		return def;
	}
	 typename map<Interval, C>::iterator   get(double val)  {
		for (typename map<Interval, C>::iterator interval = this->begin(); interval != this->end(); ++interval)
				{
					if (interval->first.between(val))
						return interval;
				}
		throw MagicsException("not in range"); 
	}
	
	 typename map<Interval, C>::const_iterator   find(double val) const {
		for (typename map<Interval, C>::const_iterator interval = this->begin(); interval != this->end(); ++interval)
				{
					if (interval->first.between(val))
						return interval;
				}
		throw MagicsException("not in range"); 
	}

};

} // namespace magics
#endif	
