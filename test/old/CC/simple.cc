/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "magplus.h"
#include "stdarg.h"
using namespace std;
using namespace magics;




int main(int, char **)
{
   Magics magics;
  
   MagCoastlines coastlines;
   MagGrib grib;  
   MagContour contour;
   
   vector<double> levels;
   
   for (double level = 500; level < 600; level+=20)
        levels.push_back(level);
   magics("contour_level_list") = levels;
   coastlines("map_coastline_colour") = "navy";
   
   grib("grib_input_file_name") = "../data/z500.grb";
  
   contour("contour_line_colour") = "pink";
   contour("level_selection_type") = "list";
   contour.insert(make_pair("contour_line_colour", 2));
  
     
   magics.add(coastlines);
   magics.add(grib);
   magics.add(contour);   
 
   magics.execute();
   
 
}
