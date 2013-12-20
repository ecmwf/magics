extern "C" {
#include <magics_api.h>
} 

int main()
{
mag_open();
mag_setc("output_format","ps");
mag_setc("output_name",  "c_arrays");

mag_setr("INPUT_FIELD_INITIAL_LATITUDE",90.0);		  
mag_setr("INPUT_FIELD_INITIAL_LONGITUDE",0.0);		  
mag_setr("INPUT_FIELD_LATITUDE_STEP",-1.5);		  
mag_setr("INPUT_FIELD_LONGITUDE_STEP",1.5);

const int NLON = 361;
const int NLAT = 181;

double FIELD[NLON][NLAT];

for(int i1=0;i1<NLON;i1++)
 for(int i2=0;i2<NLAT;i2++)
   FIELD[i1][i2] = i1;

mag_set2r("INPUT_FIELD",*FIELD,NLAT,NLON);
mag_setr("INPUT_FIELD_INITIAL_LONGITUDE",-180.);
mag_setr("INPUT_FIELD_INITIAL_LATITUDE",90.);
mag_setr("INPUT_FIELD_LONGITUDE_STEP",1.);
mag_setr("INPUT_FIELD_LATITUDE_STEP",-1.);


mag_setc("map_coastline_colour", "khaki");
mag_setc("map_grid_colour",      "grey");	

mag_setc("contour",  		"on");
mag_setc("contour_line_colour",	"sky");
mag_setc("CONTOUR_HIGHLIGHT_COLOUR", "GREEN");
mag_setc("contour_label",		"on");
mag_cont();

mag_text();
mag_coast();

mag_new("SUPER_PAGE");

mag_setr("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0);
mag_setr("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0);
mag_setr("SUBPAGE_UPPER_RIGHT_LATITUDE",   65.0);
mag_setr("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0);

mag_cont();
mag_text();
mag_coast();

mag_close();

return 0;
}
