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

/*! \file PpmDecoder.cc
    \brief Implementation of the Template class PpmDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 18-May-2005
    
    Changes:
    
*/

#include "PpmDecoder.h"
extern "C" {
#include <pam.h>
}
using namespace magics;


PpmDecoder::PpmDecoder() 
{
}


PpmDecoder::~PpmDecoder() 
{
}

/*!
 Class information are given to the output-stream.
*/	

void PpmDecoder::print(ostream& out)  const
{
	out << "PpmDecoder";
}

	
void  PpmDecoder::decode()
{
	MagLog::dev() << " PpmDecoder::decode()---> not yet\n";
	int argc  =0;
	struct pam inpam;
	int row;
	 
	char *argv[1];
	string ppm = "ppm";
	argv[0] =  "ppm";
	pnm_init(&argc, argv);

	FILE* in = fopen(filename_.c_str(), "r");

	if (!in) {
   		MagLog::error() << "PpmDecoder: can not open file : " << filename_ << "\n";
   		return;
	}
   
	pnm_readpaminit(in, &inpam, 256);
//	int i;
	tuple* tuplerow = pnm_allocpamrow(&inpam);
   
	MagLog::debug() << " matrix [" << inpam.width << ", " << inpam.height << ", " << inpam.depth << "]\n";
   
	double lon = (maxLon_ - minLon_)/inpam.width;
	double west = minLon_;
   
	for (int i = 0; i < inpam.width; i++) {
		matrix_.columnsAxis().push_back(west);
		west += lon;
		
	}
	double lat = (maxLat_ - minLat_)/inpam.height;
	double north = maxLat_;
    
	for (int i = 0; i < inpam.height; i++) {
		matrix_.rowsAxis().push_back(north);
		north -= lat;
	
	}
    
	matrix_.setMapsAxis();
	double missing = std::numeric_limits<double>::max();

	matrix_.reserve(inpam.height* inpam.width);
	
   
	matrix_.missing(missing);
 
	north = maxLat_;
	west = minLon_;
 
  for (row = 0; row < inpam.height; row++) {
       int column;
       north-=lat;
       west = minLon_;
       pnm_readpamrow(&inpam, tuplerow);
       for (column = 0; column < inpam.width; ++column) {
       	   west+=lon;
           for (unsigned int plane = 0; plane < inpam.depth; ++plane) {
    			 matrix_.push_back(tuplerow[column][plane]);   
    			 //points_.push_back(UserPoint(west, north, tuplerow[column][plane]));   
    			// MagLog::debug() <<    UserPoint(west, south, tuplerow[column][plane]) << endl;
           }
       }
   }
    

   pnm_freepamrow(tuplerow);
}

	
void  PpmDecoder::decodeRaster()
{
	decode();
	
	MagLog::dev() << " PpmDecoder::decode()---> not yet\n";
	raster_.setUpperRightCorner(maxLon_, maxLat_);
	raster_.setLowerLeftCorner(minLon_, minLat_);
	
	
	
	
	raster_.setXResolution(matrix_.XResolution());
	raster_.setYResolution(matrix_.YResolution());
	
	int columns = matrix_.columns();
	int rows = matrix_.rows();
	
	raster_.setColumns(columns);
	raster_.setRows(rows);
	
	raster_.setProjection(new TeLatLong(TeDatum()));
//	double missing = std::numeric_limits<double>::max();
	
	raster_.reserve(columns*rows);
	MagLog::debug() << "MIN --->" << points_.min();
	MagLog::debug() << "MAX --->" << points_.max();	
    
	for (int row = 0; row < rows; row++)
		for (int column = 0; column < columns; column++)
			raster_.push_back(matrix_(row, column));
	}

	
void  PpmDecoder::decodePoints()
{
	if ( !points_.empty() ) return;
	decode();	
	
	
	
		
		

}
