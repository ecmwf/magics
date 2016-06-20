/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DataConverter.cc
    \brief Implementation of the Template class DataConverter.
    
    Magics Team - ECMWF 2009
    
    Started: Wed 22-Jul-2009
    
    Changes:
    
*/



#include "DataConverter.h"

using namespace magics;

DataConverter::DataConverter(Data* data) : data_(data)
{
}


DataConverter::~DataConverter() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void DataConverter::print(ostream& out)  const
{
	out << "DataConverter[";
	out << "]";
}

PointsHandler& DataConverter::points()
{ 
       if ( points_.empty()) {
            PointsHandler& points = data_->points();
            points.setToFirst();
                while (points.more()) {
                    UserPoint geo = points.current();
                    points_.push_back(new UserPoint(geo.x(), geo.y(), geo.value()));
                    points.advance();
                }
       }
       
       pointsHandlers_.push_back(new PointsHandler(points_));
       return *(pointsHandlers_.back());
     
      
       
        
}
