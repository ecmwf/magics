C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONT11

      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','cont11.ps')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/cont11')
      CALL PSETC('MAP_GRID','OFF') 
      CALL PSETC('MAP_COASTLINE_COLOUR','BLACK')          
      CALL PSETC('MAP_GRID_COLOUR','BLACK')          
      CALL PSETR('MAP_GRID_LATITUDE_INCREMENT',30.0) 
      CALL PSETR('MAP_GRID_LONGITUDE_INCREMENT',60.0) 
C
C     SELECT THE PROJECTION (MUST BE DONE BEFORE CALLING PGRIB)
C
      CALL PSETC ('SUBPAGE_MAP_PROJECTION','AITOFF')
      CALL PSETC ('GRIB_INPUT_TYPE', 'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/complicate.grb')
      CALL PGRIB
      CALL PSETC('CONTOUR_HIGHLIGHT','OFF')               
      CALL PSETC('CONTOUR_HILO','OFF') 
      CALL PSETC('CONTOUR','OFF') 
      CALL PSETC('CONTOUR_SHADE','ON') 
      CALL PSETR('CONTOUR_SHADE_CELL_RESOLUTION',40.0)
      CALL PSETR('CONTOUR_SHADE_MIN_LEVEL',1.0)
      CALL PSETC('CONTOUR_SHADE_TECHNIQUE','CELL_SHADING') 
      CALL PCONT
      CALL PCOAST
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',0.5)
      ALONG=0.0
      CALL PSETR ('TEXT_REAL_1',ALONG)
      CALL PSETC ('TEXT_LINE_1',
     X' Shade Technique = CELL_SHADING'//
     X' ,vert long = @TEXT_REAL_1@ degrees')
      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PTEXT
C
C     CLOSE MAGICS
      CALL PCLOSE

      STOP
      END
