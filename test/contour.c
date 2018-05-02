/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "magics_api.h"
#include <stdio.h>

int main()
{
    mag_open();
    const int num_formats = 3;
    const char *formats[num_formats];
    formats[0] = "ps";
    formats[1] = "png";
    formats[2] = "pdf";
    mag_set1c("output_formats",formats,num_formats);
    mag_setc("output_name","contour"); // which is different each time

    /* load the data */
    mag_setc("grib_input_type","file");
    mag_setc("grib_input_file_name", "data.grib");
    mag_grib();

    /* define the contouring parameters */
    mag_setc("contour","on");
    mag_setc("contour_line_colour","sky");
    mag_setc("CONTOUR_HIGHLIGHT_COLOUR", "GREEN");
    mag_setc("contour_label","on");
    mag_cont();

    /* three pages */
    for(int i=1;i<4;i++)
    {
      /* specify area (SOUTH, WEST, NORTH, EAST ) */
      mag_setr("SUBPAGE_LOWER_LEFT_LATITUDE",  i *  10.0);
      mag_setr("SUBPAGE_LOWER_LEFT_LONGITUDE", i * -20.0);
      mag_setr("SUBPAGE_UPPER_RIGHT_LATITUDE", i *  30.0);
      mag_setr("SUBPAGE_UPPER_RIGHT_LONGITUDE",i *  20.0);

      /* draw background land shading */
      mag_setc("map_coastline_colour", "khaki");
      mag_setc("map_grid_colour","grey");
      mag_setc("map_coastline_land_shade","on");
      mag_setc("map_coastline_land_shade_colour","rgb(0.86,0.86,0.86)");
      mag_setc("map_boundaries","on");
      mag_coast();

      /* Draw titles and coastlines */
      mag_text();
      /* set up the coastline attributes */
      mag_setc("map_coastline_colour", "khaki");
      mag_setc("map_grid_colour","grey");
      mag_reset("map_coastline_land_shade");
      mag_coast();
      mag_cont();
      mag_new ("SUPER_PAGE");
    }

    /* close magics and trigger plot generation */
    mag_close();
    return 0;
}
