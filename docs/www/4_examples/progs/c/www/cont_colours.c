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

    mag_new ("super_page");

    /* Area specification (SOUTH, WEST, NORTH, EAST ) */
    mag_setr ("subpage_lower_left_latitude",    35.0);
    mag_setr ("subpage_lower_left_longitude",  -30.0);
    mag_setr ("subpage_upper_right_latitude",   65.0);
    mag_setr ("subpage_upper_right_longitude",  70.0);

    /* Plot page 2 */
    mag_text  ();
    mag_coast ();
    mag_cont  ();

    mag_close ();

    return 0;
}
