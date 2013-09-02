#include <magics_api.h>

int main()
{
    /* open magics and set the output device */
    mag_open ();
    mag_setc ("output_format",    "ps");
    mag_setc ("output_name",      "cont_colours");

    /* load the data */
    mag_setc ("grib_input_type",      "file");
    mag_setc ("grib_input_file_name", "data/z500.grb");
    mag_grib ();
 
    /* set up the coastline attributes */
    mag_setc ("map_coastline_colour", "khaki");
    mag_setc ("map_grid_colour",      "grey");     

    /* define the contouring parameters */
    mag_setc ("contour",                  "on");
    mag_setc ("contour_line_colour",      "sky");
    mag_setc ("CONTOUR_HIGHLIGHT_COLOUR", "GREEN");
    mag_setc ("contour_label",            "on");
    mag_cont ();

    /* plot the title text and the coastlines */
    mag_text  ();
    mag_coast ();

    mag_new ("SUPER_PAGE");

    /* Area specification (SOUTH, WEST, NORTH, EAST ) */
    mag_setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0);
    mag_setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0);
    mag_setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   65.0);
    mag_setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0);

    /* Plot page 2 */
    mag_text  ();
    mag_coast ();
    mag_cont  ();

    mag_close ();

    return 0;
}
