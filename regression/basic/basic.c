
#include <magics_api.h>

int main()
{
	/* open magics and set the output device */
	mag_open();
	mag_setc("output_name", "basic");
    mag_setc("output_format", "png");
    mag_setc("output_name_first_page_number", "off");

    mag_setc("page_id_line", "off");

    mag_setc("grib_input_file_name", "data.grib");
    mag_grib();

    mag_cont();
 //   mag_set1c("text_lines", (/"<b>grib-api test (key=name):</b> <grib_info key="name"/>"/), 1)
    mag_text();
    
	mag_coast ();
	mag_close ();
	return 0;
}

       
