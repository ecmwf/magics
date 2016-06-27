C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM HIRLAM

C     This program demonstrates magics contouring facilities. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a map.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_hirlam')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Plot the different sets of data

      CALL CONT_TEST ('data/Finland-GRIB_Z700',
     +                'Hirlam / Finland / Z700', 0)


      CALL CONT_TEST ('data/hirlam-sweden-t.grb',
     +                'Hirlam / Sweden / T1000', 1)


      CALL WIND_TEST ('data/hirlam-sweden-uv.grb',
     +                'Hirlam / Sweden / UV1000', 1)


      CALL CONT_TEST ('data/hirlam-iceland-t.grb',
     +                'Hirlam / Iceland / T500', 1)


      CALL WIND_TEST ('data/hirlam-iceland-uv.grb',
     +                'Hirlam / Iceland / UV500', 1)


      CALL PCLOSE

      STOP
      END


C    ------------------------------------------------------------
C    DATA_TEST
C    Plots the data first unshaded and then shaded
C    ------------------------------------------------------------

      SUBROUTINE CONT_TEST (FNAME, TITLE, INEW)
      
          CHARACTER*(*) FNAME
          CHARACTER*(*) TITLE


C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


c         Load the data

          CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
          CALL PSETC ('GRIB_INPUT_FILE_NAME',  FNAME)
          CALL PGRIB


C         Draw unshaded

          CALL PRESET ('CONTOUR_SHADE')
          CALL PCONT
          CALL PCOAST
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PTEXT


C         Draw shaded

          CALL PNEW  ('SUPER_PAGE')
          CALL PSETC ('CONTOUR_SHADE',        'ON')  
          CALL PSETC ('CONTOUR_SHADE_METHOD', 'AREA_FILL')
          CALL PCONT
          CALL PCOAST
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PTEXT

      RETURN
      END

C    ------------------------------------------------------------
C    WIND_TEST
C    Plots the data first unshaded and then shaded
C    ------------------------------------------------------------

      SUBROUTINE WIND_TEST (FNAME, TITLE, INEW)
      
          CHARACTER*(*) FNAME
          CHARACTER*(*) TITLE


C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


c         Load the data

          CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
          CALL PSETI ('GRIB_WIND_POSITION_2', 2)
          CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
          CALL PSETC ('GRIB_INPUT_FILE_NAME',  FNAME)
          CALL PGRIB


C         Draw wind arrows

          CALL PWIND
          CALL PCOAST
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PTEXT


      RETURN
      END



#include "parse_command_line.h"
