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

/*! \file MapGenDecoder.cc
    \brief Implementation of the Template class MapGenDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 12-Dec-2005
    
    Changes:
    
*/



#include "MapGenDecoder.h"
#include "CustomisedPoint.h"

using namespace magics;


MapGenDecoder::MapGenDecoder()
{
}


MapGenDecoder::~MapGenDecoder()
{
}

/*!
 Class information are given to the output-stream.
*/	

void MapGenDecoder::print(ostream& out)  const
{
	out << "MapGenDecoder[";
	out << "]";
}


void MapGenDecoder::decode()
{
	if ( !this->empty() ) return;
	try {
		char line[1024];
		ifstream in(path_.c_str());
		float lat, lon;
		int i = 0;
		while ( in.good() )
		{
			in.getline(line, 1024);
            
            string test(line);
            
			if ( test.empty() ) 
                continue;
			if ( strncmp(line,"# -b", 4) == 0 ) 
			   if ( record_ < 0 ) {
				   if ( this->empty() ) push_back(new PointsList());
				   this->back()->push_back(new UserPoint(0,0,0, true));
			   }
			   else
				   push_back(new PointsList());
			else {
				sscanf(line, "%f %f", &lon, &lat);
				this->back()->push_back(new UserPoint(lon, lat, i++));
			}						
		}
	
		in.close();
	}
	catch (...)
	{
		MagLog::error() << "MapGen file : can not open " << path_ << endl;
	}
	MagLog::dev() << "Map gen file--->" << this->size() << endl; 
}


void MapGenDecoder::customisedPoints(const std::set<string>&, magics::CustomisedPointsList& out)
{
	PointsHandler& list = points();
	
	list.setToFirst();
	int i = 0;
     while (list.more()) {        
    	  UserPoint point = list.current();
          CustomisedPoint* cpoint = new CustomisedPoint();
		    	(*cpoint)["x"] = point.x();
		    	(*cpoint)["y"] = point.y();
		    	(*cpoint)["value"] = i++;
		    	(*cpoint)["x_lower"] = 0;
		    	(*cpoint)["y_lower"] = 0;
		    	(*cpoint)["x_upper"] = point.x();
		    	(*cpoint)["y_upper"] = point.y();
		    	out.push_back(cpoint);
          list.advance();
      
    }  	
		
}
