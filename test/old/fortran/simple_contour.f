C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program contour
c
c     this program demonstrates magics contouring facilities. 
c     the meteorological data field is a standard global 500 hpa
c     model output field on a regular 1.5 degree grid. 
c     contours and coastlines are projected onto a 
c     cylindrical map. Output is written in SVG

      real level(9)
      data level /450., 500., 515., 530., 545.,
     + 560., 575., 590., 600./ 

c     open magics

      call popen

>>>> ORIGINAL simple_contour.f#1
      call psetc ('ps_device','svg')
      call psetc ('ps_file_name','contour.svg')
 
==== THEIRS simple_contour.f#2
      call psetc ('device','svg')
      call psetc ('svg_file_name','contour.svg')

==== YOURS simple_contour.f
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','contour.ps')
 
<<<<
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')   
>>>> ORIGINAL simple_contour.f#1
      
    
     
==== THEIRS simple_contour.f#2

==== YOURS simple_contour.f
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -90.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -180.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   +90.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   360.0)
   
    
     
<<<<
c     pass the data to magics

      call psetc ('grib_input_type','file')
      call psetc ('grib_input_file_name','../data/z500.grb')
      call pgrib

c     define the contour     
     
      call psetc ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      call pset1r ('CONTOUR_LEVEL_LIST', level, 9)
     
      call psetc ('contour_hilo','on')     
      call psetc ('legend','on')  
      call pcont

c     generate text for title

      call psetc ('text_line_1','Simple Contour')
      call pseti ('text_line_count',1)
      call psetr ('text_reference_character_height',0.5)
      call psetc ('text_justification','centre')

c     plot the coastlines and title

      call ptext
      call pcoast

      call pclose

      stop
      end
