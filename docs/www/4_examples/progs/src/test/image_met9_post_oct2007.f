C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM IMAGE_MET9ZOOMED

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('image_met9_post_oct2007')

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

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            'data/Met_9_VIS_post_oct2007.image')
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

c      CALL PGRIB
      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT


c      CALL PGRIB
      CALL PIMAGE
      CALL PCOAST
      CALL PTEXT


      CALL PCLOSE
      END


#include "parse_command_line.h"
