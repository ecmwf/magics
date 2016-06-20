C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM IMAG08
C
C     PLOT AN IMAGE WITH VARIOUS SHADING TECHNIQUES	
C

      dimension x(1),y(1),MARKERS(50)
      
      CALL POPEN
C
C     PASS GRIB FILE WITH IMAGE
C
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','image08.ps')
      CALL PSETC ('PAGE_ID_LINE','OFF')
      CALL PSETR ('SUPER_PAGE_X_LENGTH',29.7)                       
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',29.7)                       
      CALL PSETR ('PAGE_X_LENGTH',29.7)                       
      CALL PSETR ('PAGE_Y_LENGTH',29.7)                       
      CALL PSETR ('PAGE_X_POSITION',0.0)                       
      CALL PSETR ('PAGE_X_POSITION',0.00)                       
      CALL PSETC ('SUBPAGE_MAP_PROJECTION','SATELLITE')
C      CALL PSETC('IMAGE_SUBAREA_SELECTION','ON')
C      CALL PSETR('SUBPAGE_MAP_CORNER_LATITUDE',80.)
C      CALL PSETR('SUBPAGE_MAP_CORNER_LONGITUDE',80.)
C      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',40.)
C      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',0.)
      CALL PSETC('LEGEND','ON')
C
C     SELECT THE PROJECTION (MUST BE DONE BEFORE CALLING PGRIB)
C
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     x  'data/oldts/im96011000.img')  
      CALL PGRIB
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PGRIB
      CALL PSETC('CONTOUR_HILO','OFF') 
      CALL PSETC('CONTOUR_LABEL','OFF') 
      CALL PSETC('CONTOUR','OFF') 
      CALL PSETC('CONTOUR_HIGHLIGHT','OFF')               
      CALL PSETC('CONTOUR_SHADE','ON') 
      CALL PSETC('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PSETC('CONTOUR_LINE_COLOUR','BLACK')      
      CALL PSETR('CONTOUR_INTERVAL',4.0)
      CALL PSETC('CONTOUR_LEVEL_SELECTION_TYPE','INTERVAL')
      CALL PSETC('MAP_COASTLINE_COLOUR','BLACK')          
      CALL PSETC('MAP_GRID_COLOUR','BLACK')          
      CALL PSETC('CONTOUR_SHADE_MIN_LEVEL_COLOUR','BLUE')
      CALL PSETC('CONTOUR_SHADE_MAX_LEVEL_COLOUR','RED')
      CALL PSETC('CONTOUR_SHADE_COLOUR_DIRECTION','CLOCKWISE')
      
C     
C     USE DEFAULT SHADING TECHNIQUE (POLYGON)
C
      CALL PCONT
      CALL PCOAST
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
      CALL PSETC ('TEXT_LINE_1',
     X' Polygon Shading on Satellite projection')
      CALL PTEXT
      CALL PNEW('PAGE')


C
C     USE CELL SHADING
C

      CALL PSETC('CONTOUR_SHADE_TECHNIQUE','CELL_SHADING') 
      CALL PCONT
      CALL PCOAST
      CALL PSETC ('TEXT_LINE_1',
     X' CELL Shading on Satellite projection')
      CALL PTEXT
      CALL PNEW('PAGE')


C
C     USE MARKER SHADING
C

      DO i= 1,50
      	MARKERS(i) = 15.
      ENDDO

      CALL PSETC('CONTOUR_SHADE_TECHNIQUE','MARKER') 
      CALL PSET1I('CONTOUR_SHADE_MARKER_TABLE',MARKERS,50)
      CALL PSETr('CONTOUR_INTERVAL',5.0)
      CALL PCONT
      CALL PCOAST
      CALL PSETC ('TEXT_LINE_1',
     X' Marker Shading on Satellite projection')
      CALL PTEXT
C
C     CLOSE MAGICS
C
9999  CALL PCLOSE

      STOP
      END
