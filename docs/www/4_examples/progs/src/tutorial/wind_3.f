C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      program wind3
c
c     open map and define plotter
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'wind_3.ps')
c
c
c     polar stereographic projection - european area
c
      call psetc ('subpage_map_projection','polar_stereographic')
      call psetc ('subpage_map_area_definition','centre')
      call psetr ('subpage_map_centre_longitude',0.)
      call psetr ('subpage_map_centre_latitude',50.)
      call psetr ('subpage_map_vertical_longitude',0.)
      call psetr ('subpage_map_scale',15.e6)
      call psetc ('map_coastline_colour','black')
      call psetc ('map_grid_colour','black')
      call pseti ('map_coastline_thickness',3)
      call psetc ('map_grid_line_style','dot')
      call psetr ('map_grid_latitude_increment',5.0)
      call psetr ('map_grid_longitude_increment',5.0)
      
      
      call psetc ('legend','on')                 
      call pcoast
      
c
c     pass the data to magics
c
      call psetc ('grib_specification','off')
      call psetc ('grib_input_type','file')
      call pseti ('grib_wind_position_1',1)
      call pseti ('grib_wind_position_2',2)
      call psetc ('grib_input_file_name',
     x  'data/uv500_tc.grib')
      call pgrib
      call pwind

      call psetc ('grib_input_file_name',
     x  'data/z500_tc.grib')
      call pgrib
      
      call psetc ('contour_highlight', 'off')
      call psetc ('contour_line_colour', 'grey') 
      call psetc ('contour_line_style', 'dash')
      
      call psetr ('contour_shade_min_level_density',10.)
      call psetr ('contour_shade_max_level_density', 70.)
      call psetc ('contour_shade_min_level_colour','green')
      call psetc ('contour_shade_max_level_colour','red')
      call psetc ('contour_shade','on')
      call pcont
      
      call pclose
      end
