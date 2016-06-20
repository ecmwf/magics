C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      program contour1
c
c     open magics and define plotter  
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'contour_1.ps')
c
c     define map area and projection (default: cylindrical)
c
      call psetr ('subpage_lower_left_longitude',-15.0)
      call psetr ('subpage_lower_left_latitude',30.0)
      call psetr ('subpage_upper_right_longitude',40.0)
      call psetr ('subpage_upper_right_latitude',70.0)
c
c     pass the data to magics
c
                       
      call psetc ('grib_subarea_extraction','off')
      call psetc ('grib_input_type','file')
      call psetc ('grib_input_file_name', 'data/z500_tc.grib')
      call pgrib

c
c     european cylindrical area
c
      call psetc ('map_coastline_colour','tan')
      call psetc ('map_grid_colour','tan')
      call psetc ('map_grid_line_style','dash')
     
      call pcoast
c
c     dashed thick (factor 4) and evergreen contours - interval 8.0
c
      call psetc ('contour_level_selection_type','interval')
      call psetr ('contour_interval',8.0)          
      call psetc ('contour_line_style','dash')     
      call pseti ('contour_line_thickness',4)
      call psetc ('contour_line_colour', 'evergreen')
      call psetc ('contour_highlight_colour', 'evergreen')
      call psetc ('contour_hi_colour', 'orange')
      call psetc ('contour_lo_colour', 'brown')
      call pcont
      
      
      
      call pclose
      end
      
