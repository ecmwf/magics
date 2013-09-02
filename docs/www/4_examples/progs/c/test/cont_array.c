#include <magics_api.h>

int main()
{
    const int nNumLon = 240;
    const int nNumLat = 121;
    
    double dLonStep  =  1.5;
    double dLatStep  = -1.5;
    double dLonFirst =  0.0;
    double dLatFirst = 90.0;
    int    i, j;
    double dLon, dLat;
    
    double aadField[nNumLat][nNumLon];


    /* set up our field data */
    
    for (i = 0; i < nNumLat; i++)
    {
        dLat = dLatFirst + (i * dLatStep);

        for (j = 0; j < nNumLon; j++)
        {
            dLon = dLonFirst + (j * dLonStep); 
            aadField [i][j] = dLat + dLon;
        }
    }



    /* open magics and set the output device */

    mag_open ();
    mag_setc ("output_format",    "ps");
    mag_setc ("output_name",      "cont_array");


    /* pass the data to MAGICS */

    mag_setr ("input_field_initial_latitude",  dLatFirst);             
    mag_setr ("input_field_initial_longitude", dLonFirst);            
    mag_setr ("input_field_latitude_step",     dLatStep);               
    mag_setr ("input_field_longitude_step",    dLonStep);   

    mag_set2r ("input_field", &(aadField[0][0]), nNumLon, nNumLat);
    

    /* set up the coastline attributes */

    mag_setc ("map_coastline_colour", "grey");
    mag_setc ("map_grid_colour",      "grey");     
    mag_coast ();


    /* define the contour */

    mag_setc ("contour",                         "off");
    mag_setc ("contour_line_colour",             "blue");
    mag_setc ("contour_highlight_colour",        "blue");
    mag_setc ("contour_grid_value_plot",         "on");
    mag_setc ("contour_grid_value_plot_type",    "value");
    mag_seti ("contour_grid_value_lat_frequency", 8);
    mag_seti ("contour_grid_value_lon_frequency", 8);
    mag_cont  ();


    /* plot the text */

    mag_setc ("text_line_1",  "Grid values from array (lat + lon)");
    mag_text ();





    /* ---------------------------------------------
      Start a new page, this time showing a subarea
     --------------------------------------------- */


    mag_new ("super_page");


     /* define map area and projection */

    mag_setr ("subpage_lower_left_longitude",  -10.0);
    mag_setr ("subpage_lower_left_latitude",   -10.0);
    mag_setr ("subpage_upper_right_longitude", 170.0);
    mag_setr ("subpage_upper_right_latitude",   85.0);


    /* adjust the grid value plotting */

    mag_seti ("contour_grid_value_lat_frequency", 4);
    mag_seti ("contour_grid_value_lon_frequency", 4);
    mag_setr ("contour_grid_value_height",        0.3);


    /* Plot */

    mag_coast ();
    mag_cont  ();
    mag_text  ();


    mag_close ();

    return 0;
}
