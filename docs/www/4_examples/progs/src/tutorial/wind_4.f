C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      program wind4
c
c     open map and define plotter
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'wind_4.ps')
      
      
c
c     define map area and projection (default: cylindrical)
c
      call psetr ('subpage_lower_left_longitude',-15.0)
      call psetr ('subpage_lower_left_latitude',30.0)
      call psetr ('subpage_upper_right_longitude',40.0)
      call psetr ('subpage_upper_right_latitude',70.0)
      
      call psetc ('map_coastline_colour','tan')
      call psetc ('map_grid_colour','tan')
      call psetc ('map_coastline_land_shade', 'on')   
      call psetc ('map_coastline_land_shade_colour',
     +        'RGB(0.9, 0.9,0.9)')
      call pseti ('map_coastline_thickness',2)
      call psetc ('map_grid_line_style','dot')
      call psetr ('map_grid_latitude_increment',5.0)
      call psetr ('map_grid_longitude_increment',5.0)
      
      call pcoast
c
c     pass the data to magics
c
      
      call psetc ('grib_input_type','file')
      call pseti ('grib_wind_position_1',1)
      call pseti ('grib_wind_position_2',2)
      call psetc ('grib_input_file_name',
     x  'data/uv500_tc.grib')
      call pgrib
c
      call pseti ('wind_flag_thickness', 3)
      call psetc ('wind_field_type','flags')     
      call psetr ('wind_flag_max_speed',5.0)
      call psetc ('wind_flag_origin_marker','dot')
      call psetc ('wind_flag_colour','green')          
     
      call pwind
      
      
      call psetr ('wind_flag_min_speed',5.0)    
      call psetr ('wind_flag_max_speed',10.0)
      call psetc ('wind_flag_colour','yellow')
      
      call pwind      
     
      call psetr ('wind_flag_min_speed',10.0)    
      call psetr ('wind_flag_max_speed',15.0)
      call psetc ('wind_flag_colour','orange')
      
      call pwind
      
      call psetr ('wind_flag_min_speed',15.0)    
      call preset ('wind_flag_max_speed')
      call psetc ('wind_flag_colour','red')
      
      call pwind
      
      call pclose
c
      return
      end
      
