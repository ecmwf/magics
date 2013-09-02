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

/*! \file HiLo.cc
    \brief Implementation of the Template class HiLo.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 22-Jun-2004
    
    Changes:
    
*/

#include "HiLo.h"
#include "Filter.h"
#include "MatrixHandler.h"
#include "Transformation.h"


using namespace magics;


HiLo::HiLo()
{
}


HiLo::~HiLo()
{
}

/*!
 Class information are given to the output-stream.
*/	

void HiLo::print(ostream& out)  const
{
   out << "HiLo";
}



void HiLo::set(const map<string, string>& map)
{
   HiLoAttributes::set(map);
}


void HiLo::operator()(const PaperPoint& point)
{
    double value = point.value();
    if ( value > this->hilo_max_value_ ) return;
    if ( value < this->hilo_min_value_ ) return;
    if ( point.high() ) {
         if ( value > this->hi_max_value_ ) return;
         if ( value < this->hi_min_value_ ) return;    
    }
    if ( point.low() ) {
         if ( value > this->lo_max_value_ ) return;
         if ( value < this->lo_min_value_ ) return;
    }

    (*this->type_)(point, *this);
    (*this->marker_)(point, *this);
    //(*this->position_write_)(point);
}


struct SortHiLo 
{
    SortHiLo() {}
    ~SortHiLo() {}
    bool operator()(const UserPoint& first, const UserPoint& second)
    {   
        if (first.high() && second.low()) return true;
        if (first.low() && second.high()) return false;
        if (first.low() && second.low())  return first.value() < second.value(); 
        if (first.high() && second.high()) return first.value() > second.value(); 
        return false;
     }
};


struct Radius 
{
    Radius(double radius, const UserPoint& reference): radius_(radius), reference_(reference) {}
    ~Radius() {}
    bool operator()(const UserPoint& point)
    {   
      if (!same(point)) return false; 
      double dist = distance(point, reference_);
      if ( zero(dist) ) return false;
      return  dist  < radius_ * radius_;
    }
    
    bool same(const UserPoint& point) {
        if ( reference_.high() ) return point.high();
        if ( reference_.low() ) return point.low();
        return false;
    }

    double distance(const UserPoint& p1, const UserPoint& p2) {
        return ((p1.x() -p2.x())*(p1.x() -p2.x())) + ((p1.y() -p2.y())*(p1.y() -p2.y()));
    }
    
    double radius_;
    const UserPoint& reference_;
};



void HiLo::operator()(MatrixHandler& data, BasicGraphicsObjectContainer& parent)
{
    double radius;
    double area_width, area_height, area_max;
    const Transformation& transformation = parent.transformation();

    MinMaxFilter filter(data, this->window_size_, this->window_size_);
    filter.Process();

    std::sort(filter.begin(), filter.end(), SortHiLo());

    // compute a sensible radius within which we will not generate two Hi/Li close to each other
    area_height = data.row    (data.rows()    -1, 0) - data.row    (0, 0);
    area_width  = data.column (0, data.columns() -1) - data.column (0, 0);

    area_max = max (area_height, area_width);
    if (area_max > 180) area_max = 150;  // assumption: a width > 180 means polar-stereo - so adjust.

    radius = (area_max / 10.0);

    // printf ("area_height: %f, area_width: %f, area_max: %f, radius: %f\n", area_height, area_width, area_max, radius);

    for (vector<UserPoint>::const_iterator point = filter.begin(); point != filter.end(); ++point) {
        vector<UserPoint>::iterator last = std::remove_if(filter.begin(), filter.end(), Radius(radius, *point));
        filter.erase(last, filter.end());
    }

    clear();

    for (MinMaxFilter::const_iterator point = filter.begin(); point != filter.end(); ++point) {
    	PaperPoint xy = transformation(*point);
    	xy.low(point->low());
    	xy.high(point->high());
    	if ( transformation.in(xy) ) 
        	(*this)(xy);
    }
   
    // Now we feed the task
    for ( vector<BasicGraphicsObject*>::const_iterator object = begin(); object != end(); ++object)
    	parent.push_back(*object);
}
    


