
      program wind1
c
c     open map and define plotter
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'wind_1.ps')
c
c     define map area and projection (default: cylindrical)
c
      call psetr ('subpage_lower_left_longitude',-15.0)
      call psetr ('subpage_lower_left_latitude',30.0)
      call psetr ('subpage_upper_right_longitude',40.0)
      call psetr ('subpage_upper_right_latitude',70.0)
      
C Definition of the background
c
      call psetc ('map_coastline_colour','tan') 
      call pseti ('map_coastline_thickness', 2) 
      call psetc ('map_coastline_land_shade', 'on')   
      call psetc ('map_coastline_land_shade_colour',
     +        'HSL(0.40, 0.15,0.95)')
      call psetc ('map_grid_colour','grey')    
c
      
      call pcoast
c
c     pass the data to magics
c
 
      call psetc ('grib_input_type','file')
      call pseti ('grib_wind_position_1',1)
      call pseti ('grib_wind_position_2',2)
      call psetc ('grib_input_file_name', 'data/uv500_tc.grib')
      call pgrib


c      wind flags greater than or equal to 5 m/s
c
      call psetc ('wind_field_type','flags')     
      call psetr ('wind_flag_min_speed',5.0)
      call psetc ('wind_flag_origin_marker','dot')
      call psetc ('wind_flag_colour','navy')          
      

      call pwind
c
      call pclose
      end
