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
