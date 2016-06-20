C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program coast_3
c
c     open magics and define printer
c
      call popen
      call psetc ('ps_device',    'ps_a4')
      call psetc ('ps_file_name', 'coast_3.ps')
c
c     Third map - 2 Subpages on a A4 page (portrait format)  
c     Global map with shading on the first page 
C     North hemisphere in Polar Sterographic on the second 
c     
c     
C Setting of the layout      
      call psetr ('super_page_y_length', 29.7)
      call psetr ('super_page_x_length', 21.)                
      call psetr ('page_y_length', 14.5)
      call psetr ('page_x_length', 21.)
      
C Setting of the mapping      
      call psetc ('subpage_map_projection','cylindrical') 
             
C Setting of the Coastlines attributes
      call psetc ('map_coastline_land_shade','on')         
      call psetc ('map_coastline_land_shade_colour','cream')  
      call psetc ('map_grid_colour', 'grey') 
      call psetc ('map_coastline_colour', 'grey')  
               
      call pcoast
      
C Setting of the text
      call psetc ('text_line_1', 'Cylindrical')
      call ptext
      
      call pnew("PAGE")

C Change the projection      
      call psetc ('subpage_map_projection','polar_stereographic') 

C Make the labels bigger 
     
      call psetr ('map_label_height', 0.4)
      
      call pcoast
C Change the text         
      call psetc ('text_line_1', 'Polar stereographic')  
      call ptext         
             
      call pclose

      end
