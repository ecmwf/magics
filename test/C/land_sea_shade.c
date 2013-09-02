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

#include <magics_api.h>

int main()
{
	/* open magics and set the output filename */
	mag_open ();
	mag_setc ("output_name", "land_sea_shade");


	/* Set up the coastline attributes */
	mag_setc ("map_coastline", 	  "on");
	mag_setc ("map_coastline_colour", "grey");
	mag_setc ("map_grid_colour",	  "grey");

	/* First page: land and sea shading together: */
	mag_setc  ("text_line_1", "LAND and SEA shading");
	mag_setc  ("map_coastline_land_shade",        "on");
	mag_setc  ("map_coastline_land_shade_colour", "cream");
	mag_setc  ("map_coastline_sea_shade",         "on");
	mag_setc  ("map_coastline_sea_shade_colour",  "blue");
	mag_coast ();
	mag_text  ();

	/* Second page: land shading only: */
	mag_new   ("super_page");
	mag_setc  ("text_line_1", "LAND shading only");
	mag_setc  ("map_coastline_land_shade", "on");
	mag_setc  ("map_coastline_sea_shade",  "off");
	mag_coast ();
	mag_text  ();

	/* Third page: sea shading only: */
	mag_new   ("super_page");
	mag_setc  ("text_line_1", "SEA shading only");
	mag_setc  ("map_coastline_land_shade",  "off");
	mag_setc  ("map_coastline_sea_shade",   "on");
	mag_coast ();
	mag_text  ();

	/* Fourth page:  land and sea shading together, zoom in: */
	mag_new   ("super_page");
	mag_setc  ("text_line_1", "LAND and SEA shading");
	mag_setr  ("subpage_lower_left_latitude",	   30.0);
	mag_setr  ("subpage_lower_left_longitude",    -30.0);
	mag_setr  ("subpage_upper_right_latitude",     68.0);
	mag_setr  ("subpage_upper_right_longitude",    70.0);
	mag_setc  ("map_coastline_land_shade",        "on");
	mag_setc  ("map_coastline_sea_shade",         "on");
	mag_coast ();
	mag_text  ();

	mag_close ();
	return 0;
}
