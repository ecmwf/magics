      PROGRAM IMAGE_MET9ZOOMED

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('image_met9_zoomed')

C     Set up our coastline parameters

      CALL PSETC('MAP_COASTLINE_COLOUR', 'BLUE')
      CALL PSETI('MAP_COASTLINE_THICKNESS', 4)
      CALL PSETC('MAP_LABEL_COLOUR',     'BLUE')
      CALL PSETC('MAP_GRID_COLOUR',      'GREY')
C      CALL PSETC('MAP_GRID',             'OFF')

C     Set the projection
C     At time of writing, satellite projection does not work

      CALL PSETC('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')
c      CALL PSETC('SUBPAGE_MAP_PROJECTION','SATELLITE')


C     Zoom in to see if it fits the coastline

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -35.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   -10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   40.0)


C     Set the image-plotting parameters

      CALL PSETC ('IMAGE_COLOUR_TABLE_CREATION_MODE', 'EQUIDISTANT')
      CALL PSETC ('IMAGE_MIN_LEVEL_COLOUR',           'WHITE')
      CALL PSETC ('IMAGE_MAX_LEVEL_COLOUR',           'BLACK')
      CALL PSETI ('IMAGE_PIXEL_SELECTION_FREQUENCY',   20)


C     Load and plot each input data

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/Met_9_VIS.image')
C     +  '/home/graphics/cgi/metview/Tests/Satellite/met9_new.image')
      CALL PGRIB
      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT


C     Try a new area

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     15.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    40.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   60.0)

      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT


C     Try a new area

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     48.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -15.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    62.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',    5.0)

      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT


C     Try a new area

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -25.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   15.0)

      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT


      CALL PCLOSE
      END


#include "parse_command_line.h"
