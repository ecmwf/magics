      PROGRAM IMAGE_BASIC

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('image_basic')

C     Set up our coastline parameters

      CALL PSETC('MAP_COASTLINE_COLOUR', 'TAN')
      CALL PSETC('MAP_GRID_COLOUR',      'TAN')
      CALL PSETC('MAP_LABEL_COLOUR',     'TAN')

C     Set the projection
C     At time of writing, satellite projection does not work

      CALL PSETC('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')
C     CALL PSETC('SUBPAGE_MAP_PROJECTION','SATELLITE')


C     Set the image-plotting parameters
C     We set the pixel frequency simply in order to make the
C     output file size smaller; it's the number of pixels per cm.

      CALL PSETC ('IMAGE_COLOUR_TABLE_CREATION_MODE', 'EQUIDISTANT')
      CALL PSETR ('IMAGE_OUTLIER_REJECTION',           0.0)
      CALL PSETC ('IMAGE_MIN_LEVEL_COLOUR',           'WHITE')
      CALL PSETC ('IMAGE_MAX_LEVEL_COLOUR',           'BLACK')
      CALL PSETI ('IMAGE_LEVEL_COUNT',                 20)
      CALL PSETI ('IMAGE_PIXEL_SELECTION_FREQUENCY',   45)


C     Load and plot each input data

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
c      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/met7fib.grb')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            '/usr/local/share/magics/data/Met_7_WVB.image')
      CALL PGRIB
      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/Met_8_FIB')
      CALL PGRIB
      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT

      CALL PCLOSE
      END


#include "parse_command_line.h"
