C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      program contour4
c
c     open magics and define plotter  
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'contour_4.ps')
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
      call psetc ('legend','on')                   
      call psetc ('grib_input_type','file')
      call psetc ('grib_input_file_name','data/z500_tc.grib')
      call pgrib

   
c
c     shaded (area_fill) contours - coloured blue to red in a clockwise
c     direction. 
c
      call psetc ('contour_shade','on')     
      
      call psetc('contour_shade_method','area_fill') 
      call psetc ('contour_shade_colour_method', 'calculate')
      call psetc('contour_shade_min_level_colour','blue')
      call psetc('contour_shade_max_level_colour','red')
      call psetc('contour_shade_colour_direction','clockwise')  
      call psetc('contour_level_selection_type', 'count')
      call pcont
      call pcoast
      
C
C   add a automatic text
C
      call ptext
      
      call pclose

      return
      end

