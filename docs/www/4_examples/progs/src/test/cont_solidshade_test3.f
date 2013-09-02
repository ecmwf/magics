      PROGRAM test

      CALL POPEN

      CALL PARSE_COMMAND_LINE ('cont_solidshade_test3')

      CALL PSETR ('SUBPAGE_X_LENGTH', 0.8*29.7)
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'CYLINDRICAL')

      CALL PSETC ('GRIB_SPECIFICATION', 'OFF')
      CALL PSETC ('GRIB_INPUT_TYPE', 'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/lsp25.grib')

c     CALL PGRIB

C     loop increments were originally 40 and 20 but this caused a crash!
      do 100 i=-160,160,80
      do 100 j=-60,60,40

      reflon = i+cos(j*1.0)
      reflat = j+sin(j*1.0)

      size = 3.0+cos(j*1.)*2.

      rinc = sin(j*1.)*10+10

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE', reflon - size/2.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', reflon + size/2.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE', reflat)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE', reflat + size/2.0)

      call pseti ('GRIB_FIELD_POSITION', 1)
      CALL PGRIB

c      CALL PCOAST

      CALL PTEXT

c     CALL PSETC ('CONTOUR', 'OFF')
c     CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL', rinc)

c     CALL PSETR ('CONTOUR_MIN_LEVEL', 2.7)
c     CALL PSETR ('CONTOUR_MAX_LEVEL', 2.8)

      CALL PSETC ('CONTOUR_SHADE', 'ON')
      CALL PSETC ('CONTOUR_SHADE_METHOD', 'AREA_FILL')
c     CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL', 6.0)
c     CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL', 18.0)
c     CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'RED')
c     CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'BLUE')

c     CALL PSETC ('CONTOUR_LABEL', 'OFF')
      CALL PSETC ('CONTOUR_HILO', 'OFF')

      CALL PCONT

      CALL PNEW ('PAGE')

100   CONTINUE

      CALL PCLOSE

      END




#include "parse_command_line.h"
