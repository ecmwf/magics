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

/*! \file Intervals.h
    \brief Definition of the Template class Intervals.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 9-Jun-2004
    
    Changes:
    
*/

#ifndef Intervals_H
#define Intervals_H

#include "magics.h"
#include "MagLog.h"


namespace magics {

struct Interval
{
    Interval(double min = 0, double max = 0) : min_(min), max_(max) 
        {
            MagLog::debug() << "Interval[" << min_ << "," << max_ << "]" << "\n";
        }
    bool between(double val) const { 
        //MagLog::debug() << min_ << "<" << val << "<" << max_ << "?" << "\n";
        return (min_ <= val && val < max_); 
    }
    double min_;
    double max_;
};


class Intervals : public vector<Interval> { 

public:
	Intervals(const doublearray&);
	virtual ~Intervals();
    double find(double val) const;
    void set(const doublearray&);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	Intervals(const Intervals&);
    //! Overloaded << operator to copy - No copy allowed
	Intervals& operator=(const Intervals&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Intervals& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
