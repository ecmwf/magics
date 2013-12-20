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
