/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <magics_api.h>

int main()
{
	/* open magics and set the output device */
	mag_open();

	/* load the data */
	mag_setc("grib_input_type", "file");
	mag_setc("grib_input_file_name", "../data/z500.grb");
	mag_grib();
 
	/* set up the coastline attributes */
	mag_setc("map_coastline_colour", "khaki");
	mag_setc("map_grid_colour",	 "grey");     

	/* define the contouring parameters */
	mag_setc("contour",	       "on");
	mag_setc("contour_line_colour","sky");
	mag_setc("CONTOUR_HIGHLIGHT_COLOUR","GREEN");
	mag_setc("contour_label",      "on");
	mag_cont();

	/* plot the title text and the coastlines */
	mag_text();
	mag_coast();

	mag_new("SUPER_PAGE");

	/* Area specification (SOUTH, WEST, NORTH, EAST ) */
	mag_setr("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0);
	mag_setr("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0);
	mag_setr("SUBPAGE_UPPER_RIGHT_LATITUDE",   65.0);
	mag_setr("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0);

	/* Plot page 2 */
	mag_text();
	mag_coast();
	mag_cont();

	mag_close();

	return 0;
}
