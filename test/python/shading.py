import Magics
from numpy import *

Magics.init ()

formats = ["pdf", "PNG", "ps"]

Magics.set1c("output_formats",formats)
Magics.setc("output_name","shading_py")

colour = ["NAVY", "BLUE", "CYAN", "GREEN","YELLOW", "ORANGE", "RED", "PURPLE"]
level  =  array([480., 490., 500., 530., 545., 560., 575., 590., 600.])
markers  = array([0,1, 2, 3, 4, 5, 6, 7, 8], dtype=int32)
print markers.dtype
test   = array([450., 515., 530.])


Magics.setc ("legend", "on")
Magics.setc ("map_coastline_colour","grey")
Magics.setc ("map_grid_colour","grey")   
    
# pass the data to Magics

Magics.setc ("grib_input_type","file")
Magics.setc ("grib_input_file_name","../data/z500.grb")
Magics.grib ()
      
 
# define the contour     
     
Magics.setc ("CONTOUR_LEVEL_SELECTION_TYPE", "LEVEL_LIST")
Magics.set1r("CONTOUR_LEVEL_LIST", level)
Magics.set1i("contour_shade_marker_table",markers)
Magics.setc ("CONTOUR_SHADE",            "ON")      
Magics.setc ("CONTOUR_SHADE_TECHNIQUE",  "marker")
Magics.setc ("contour_line_colour",      "evergreen")
Magics.setc ("contour_highlight_colour", "evergreen")
Magics.setc ("CONTOUR_SHADE_COLOUR_METHOD", "list")
Magics.set1c("CONTOUR_SHADE_COLOUR_LIST", colour)
Magics.setc ("contour_hilo","off")     
Magics.setc ("legend","on")  
Magics.cont ()
      

# generate text for title
Magics.setc ("text_line_1", "Contours with HATCH shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")

# plot the coastlines and title

Magics.text ()
Magics.coast ()
Magics.new_page("PAGE")
      
Magics.setc ("CONTOUR_SHADE_METHOD",     "DOT")
Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    45.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -58.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   55.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE", -40.0)

Magics.setc ("CONTOUR_SHADE_TECHNIQUE",  "POLYGON_SHADING")
Magics.setc ("CONTOUR_SHADE_METHOD",     "HATCH")
Magics.cont ()
Magics.coast ()
  

Magics.setc ("text_line_1", "Contours with DOT shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text ()
      
      
Magics.new_page("PAGE")

Magics.setc ("CONTOUR_SHADE_METHOD",     "AREA_FILL")
Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    40.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -56.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   55.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",   -20.0)
Magics.cont ()
Magics.coast ()
      
Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text ()
      
      
Magics.new_page("PAGE")
      
Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    55.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -56.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   75.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",   -20.0)
Magics.cont ()
Magics.coast ()

Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text ()
        
Magics.new_page("PAGE")
      
Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    10.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  20.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   60.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",   70.0)
Magics.cont ()
Magics.coast ()
Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text ()
      
            
Magics.new_page("PAGE")
      
Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    -70.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -98.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   -30.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",   -50.0)
Magics.cont()
Magics.coast()

Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text()
               
Magics.new_page("PAGE")
Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    -40.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -118.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   -30.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",   -102.0)
Magics.cont()
Magics.coast()

Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text()
      
Magics.new_page("PAGE")

Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    30.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  -30.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   50.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",  70.0)
Magics.cont()
Magics.coast()

Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text()

Magics.new_page("PAGE")

Magics.setr ("SUBPAGE_LOWER_LEFT_LATITUDE",    -90.0)
Magics.setr ("SUBPAGE_LOWER_LEFT_LONGITUDE",  140.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LATITUDE",   -60.0)
Magics.setr ("SUBPAGE_UPPER_RIGHT_LONGITUDE",  180.0)
Magics.cont()
Magics.coast()
Magics.setc ("text_line_1", "Contours with solid shading")
Magics.seti ("text_line_count",1)
Magics.setr ("text_reference_character_height",0.5)
Magics.setc ("text_justification","centre")
Magics.text()

Magics.finalize()
