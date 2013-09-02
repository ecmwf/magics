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
	/* open magics and set the output device */
	mag_open();
	mag_setc("output_format", "ps");
	mag_setc("output_name", "contour_test");

	/* load the data */

	mag_setc ("grib_input_file_name", "../data/z500.grb");
	mag_grib ();

 
	/* set up the coastline attributes */

	mag_setc ("map_coastline_colour",    "khaki");
	mag_setc ("map_grid_colour",	     "grey");     


	/* define the contouring parameters */

	mag_setc ("contour_line_colour",      "sky");
	mag_setc ("contour_highlight_colour", "green");
	mag_cont ();


	/* plot the title text and the coastlines */

	mag_text  ();
	mag_coast ();


	/* Start a new page - we will plot a smaller area */

	mag_new ("super_page");

	mag_setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0);
	mag_setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0);
	mag_setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   68.0);
	mag_setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0);

	mag_text  ();
	mag_coast ();
	mag_cont  ();

	mag_close ();
	return 0;
}
