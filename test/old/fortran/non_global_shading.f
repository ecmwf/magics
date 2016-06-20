C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program shading
c
c     this program demonstrates magics contouring facilities. 
c     the meteorological data field is a standard global 500 hpa
c     model output field on a regular 1.5 degree grid. 
c     contours and coastlines are projected onto a 
c     polar stereographic map.


c     open magics
     
      character colour*20
      dimension colour(8)
      real level(9), test(3)
      data colour /'NAVY', 'BLUE', 'CYAN', 'GREEN',
     +            'YELLOW', 'ORANGE', 'RED', 'PURPLE'/
      data level /450., 500., 515., 530., 545.,
     + 560., 575., 590., 600./ 
      data test /450., 515., 530./ 
     
      call popen

      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','ng_shading.ps')
      
      call psetc ("legend", 'off')
      
       
      
               
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')   
      
    
     
c     pass the data to magics

      call psetc ('grib_input_type','file')
      call psetc ('grib_input_file_name','../data/z500_ng.grb')
      call pgrib
     
 
c     define the contour     
     
      call psetc ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      call pset1r ('CONTOUR_LEVEL_LIST', level, 9)
      call psetc ('CONTOUR_SHADE',            'ON')      
      call psetc ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
      call psetc ('CONTOUR_SHADE_METHOD',     'AREA_FILL')
      call psetc ('contour_line_colour',      'evergreen')
      call psetc ('contour_highlight_colour', 'evergreen')
      call psetc ('CONTOUR_SHADE_COLOUR_METHOD', 'list')
      call pset1c ('CONTOUR_SHADE_COLOUR_LIST', colour, 8)
      call psetc ('contour_hilo','off')     
      call psetc ('legend','off')  
      call pcont
      

c     generate text for title

      call psetc ('text_line_1', 'Contours with solid shading')
      call pseti ('text_line_count',1)
      call psetr ('text_reference_character_height',0.5)
      call psetc ('text_justification','centre')

C     Define the geographical area for our first page

      

c     plot the coastlines and title

      call ptext
      call pcoast
      
      
      call pclose

      stop
      end
