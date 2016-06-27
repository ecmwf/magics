/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MatrixTestDecoder.h
    \brief Implementation of the Template class MatrixTestDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 25-Mar-2004
    
    Changes:
    
*/



#include "MatrixTestDecoder.h"
#include "Factory.h"
#include <limits>

using namespace magics;

MatrixTestDecoder::MatrixTestDecoder()
{
	
	ifstream f("/home/graphics/cgs/public/map.txt");
	ASSERT(f);
	int rows, columns;
	double lon, lat, inclon, inclat;
	f >> rows >> columns;
	f >> lat >> lon >> inclat >> inclon;
	matrix_.set(rows, columns);
	double missing = -std::numeric_limits<double>::max();
     
	matrix_.missing(missing);
	
	char c;
    while (! f.eof() )
    {
      f.get(c);      
   
      if (isdigit(c) )   
      	matrix_.push_back(atoi(&c));
      if (c == '.') 
      	matrix_.push_back(missing);
         
    }
    f.close();
    
    
  
	
	
    for (int i = 0; i < columns; i++) {
           matrix_.columnsAxis().push_back(lon);
           lon+=inclon;
    }
    
    for (int i = 0; i < rows; i++) {
    	matrix_.rowsAxis().push_back(lat);
    	lat+=inclat;
    }

   
    matrix_.setMapsAxis();

    
   
}



MatrixTestDecoder::~MatrixTestDecoder() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void MatrixTestDecoder::print(ostream& out)  const
{
	out << "MatrixTestDecoder[";
	
	out << "]";
}


