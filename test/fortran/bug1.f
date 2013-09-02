      program test
      
    
      call popen 
      call psetr("SUBPAGE_LOWER_LEFT_LATITUDE",   20.)
      call psetr("SUBPAGE_LOWER_LEFT_LONGITUDE",  -5.0)
      call psetr("SUBPAGE_UPPER_RIGHT_LONGITUDE", 40.0)
      call psetr("SUBPAGE_UPPER_RIGHT_LATITUDE", 50.0)
      call pcoast    
      call psetc("grib_input_type", "file") 
      call pseti("grib_field_position", 2)
      call pseti("contour_level_count", 10)
      call pseti("contour_level_tolreance", -2)
      call psetc("grib_input_file_name", 
     +        "../data/plot:soil_temperature:0:sfc.grib")
      call psetc("contour_label", "off")  
      call psetc("contour_shade",            "on")  
      call psetc("CONTOUR_SHADE_METHOD",       "hatch")  
      call psetc("contour_shade_technique",  "polygon_shading") 
      call psetc("contour_grid_value_plot",  "off")
      call pgrib
      call pcont
      call pclose
      
      end
