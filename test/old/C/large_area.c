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

/* This example demonstrates plotting an area with larger than
360 degrees of longitude. The data and coastline are wrapped
around seamlessly */

int main()
{
	/* Open magics and set the output filename */
	mag_open ();
	mag_setc ("OUTPUT_NAME", "large_area");

	/* Set our geographical area - larger than 360 degrees wide */
	mag_setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    -60.0);
	mag_setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -220.0);
	mag_setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",    60.0);
	mag_setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",  320.0);

	/* Pass the data to MAGICS */
	mag_setc ("GRIB_INPUT_FILE_NAME", "../data/z500.grb");
	mag_grib ();

	/* Set up the coastline attributes */
	mag_setc ("MAP_COASTLINE_COLOUR", "GREY");
	mag_setc ("MAP_GRID_COLOUR",      "GREY");

	/* Define the contour */
	mag_setc ("CONTOUR_SHADE",            "ON");
	mag_setc ("CONTOUR_SHADE_TECHNIQUE",  "POLYGON_SHADING");
	mag_setc ("CONTOUR_SHADE_METHOD",     "AREA_FILL");
	mag_setc ("CONTOUR_HILO",             "OFF");
	mag_cont ();

	/* Set up and plot the title text */
	mag_setc ("TEXT_LINE_1",  "Area wider than 360 degrees");
	mag_text ();

	/* Plot the coastlines and then close */
	mag_coast ();

	mag_close ();
	return 0;
}
