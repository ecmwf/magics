C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM WIND


      INTEGER ISTYLE, IRATIO, INDEX
      CHARACTER*80  TITLE


      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_arrows_new')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 145.0)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv500.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETC ('LEGEND', 'ON')



      DO 20 ISTYLE = 0, 3
        DO 40 IRATIO = 2, 5

          CALL PSETI ('WIND_ARROW_HEAD_SHAPE', ISTYLE)
          CALL PSETR ('WIND_ARROW_HEAD_RATIO', IRATIO * 0.1)
          CALL PWIND

          WRITE(TITLE,'(A,I,A,4F6.1)') 'SHAPE: ',
     X          ISTYLE, ', RATIO: ', IRATIO * 0.1
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PTEXT

          CALL PNEW  ('SUPER_PAGE')

  40    CONTINUE
  20  CONTINUE



C      And now with thickness set to > 1 ...

      CALL PSETI ('WIND_ARROW_THICKNESS', 3)

      DO 200 ISTYLE = 0, 3
        DO 400 IRATIO = 2, 5

          CALL PSETI ('WIND_ARROW_HEAD_SHAPE', ISTYLE)
          CALL PSETR ('WIND_ARROW_HEAD_RATIO', IRATIO * 0.1)
          CALL PWIND

          WRITE(TITLE,'(A,I,A,4F6.1)') 'SHAPE: ',
     X          ISTYLE, ', RATIO: ', IRATIO * 0.1
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PTEXT

          CALL PNEW  ('SUPER_PAGE')

  400    CONTINUE
  200  CONTINUE


      CALL PCLOSE
      END


#include "parse_command_line.h"
