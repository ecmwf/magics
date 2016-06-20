C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTALTERNATE

C     This program is intended to illustrate how the LINEAR contouring
C     method copes with different scenarios. The question we are asking
C     is: under what circumstances is it adequate?


      PARAMETER (NBANDS=3)
      CHARACTER*16 CTAB
      DIMENSION  RLLEV (NBANDS)
      DIMENSION  RULEV (NBANDS)
      DIMENSION  CTAB  (NBANDS)
      DATA       RLLEV / 233.,  237.,     241./
      DATA       RULEV / 235.,  239.,     243./
      DATA       CTAB  /'RED', 'ORANGE', 'BLUE'/


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE('cont_alternate')


C     Set up the paper size

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)



C     Set up the coastline attributes

      CALL PSETC ('MAP_GRID_COLOUR',              'GREY') 
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   5.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  5.0)
      CALL PSETC ('MAP_COASTLINE',                'ON')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',     'FILE')
      CALL PSETC ('GRIB_SPECIFICATION',  'OFF')
#if defined (MAGPLUS)
      CALL PSETC ('GRIB_AUTOMATIC_SCALING', 'OFF')
#else
      CALL PSETC ('GRIB_SCALING', 'OFF')
#endif
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/t300.grb')
      CALL PGRIB
      

C     Define the general contour attributes     

      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_LABEL_COLOUR',     'BLACK')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',   1)
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSETC ('CONTOUR_HILO',              'OFF')
      CALL PSETC ('CONTOUR_SHADE',             'ON')
      CALL PSETC ('CONTOUR_SHADE_METHOD',      'AREA_FILL')
      


C     Define and draw each contour in turn

      CALL CONTOUR_BAND (RLLEV(1), RULEV(1), CTAB(1))
      CALL CONTOUR_BAND (RLLEV(2), RULEV(2), CTAB(2))
      CALL CONTOUR_BAND (RLLEV(3), RULEV(3), CTAB(3))



C     Set up the title text

      CALL PSETI ('TEXT_LINE_COUNT',     1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PSETC ('TEXT_LINE_1', 'Contouring of alternate bands')


      CALL PTEXT
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END



C     ----------------------------------------------------
C     CONTOUR_BAND
C
C     Defines and plots a contour between two given values
C     in the given colour
C     ----------------------------------------------------

      SUBROUTINE CONTOUR_BAND (RLOWER, RUPPER, COLOUR)
      
      CHARACTER*16 COLOUR
      DIMENSION  RLEVLIST (2)

    
      RLEVLIST(1) = RLOWER
      RLEVLIST(2) = RUPPER


      CALL PSETI  ('CONTOUR_LEVEL_COUNT',            2)
      CALL PSET1R ('CONTOUR_LEVEL_LIST',             RLEVLIST, 2)
      CALL PSETC  ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', COLOUR)
      
      CALL PCONT


      RETURN
      END






#include "parse_command_line.h"




