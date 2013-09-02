      PROGRAM WIND


      INTEGER ISTYLE, IRATIO, INDEX
      CHARACTER STRING*2


      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_arrows')


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
        DO 40 IRATIO = 1, 6

c         Calculate the arrow head index

          INDEX = (ISTYLE * 10) + IRATIO
          WRITE(UNIT=STRING, FMT='(I1,I1)') ISTYLE, IRATIO

          CALL PSETI ('WIND_ARROW_HEAD_INDEX', INDEX)
          CALL PWIND

          CALL PSETC ('TEXT_LINE_1', STRING)
          CALL PTEXT

          CALL PNEW  ('SUPER_PAGE')


  40    CONTINUE
  20  CONTINUE



C      And now with thickness set to > 1 ...

      CALL PSETI ('WIND_ARROW_THICKNESS', 3)

      DO 200 ISTYLE = 0, 3
        DO 400 IRATIO = 1, 6

c         Calculate the arrow head index

          INDEX = (ISTYLE * 10) + IRATIO
          WRITE(UNIT=STRING, FMT='(I1,I1)') ISTYLE, IRATIO

          CALL PSETI ('WIND_ARROW_HEAD_INDEX', INDEX)
          CALL PWIND

          CALL PSETI ('TEXT_LINE_COUNT', 2)
          CALL PSETC ('TEXT_LINE_1', STRING)
          CALL PSETC ('TEXT_LINE_2', 'Thickness = 3')
          CALL PTEXT

          CALL PNEW  ('SUPER_PAGE')


  400    CONTINUE
  200  CONTINUE


      CALL PCLOSE
      END


#include "parse_command_line.h"
