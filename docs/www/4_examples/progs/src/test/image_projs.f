C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM IMAGE_BASIC

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('image_projs')



C     Load the data

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            '/usr/local/share/magics/data/Met_7_WVB.image')

      CALL PGRIB


C     Set up our coastline parameters

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',      'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',     'TAN')


C     Set the image-plotting parameters
C     We set the pixel frequency simply in order to make the
C     output file size smaller; it's the number of pixels per cm.

      CALL PSETC ('IMAGE_COLOUR_TABLE_CREATION_MODE', 'EQUIDISTANT')
      CALL PSETR ('IMAGE_OUTLIER_REJECTION',           0.0)
      CALL PSETC ('IMAGE_MIN_LEVEL_COLOUR',           'WHITE')
      CALL PSETC ('IMAGE_MAX_LEVEL_COLOUR',           'BLACK')
      CALL PSETI ('IMAGE_LEVEL_COUNT',                 20)
      CALL PSETI ('IMAGE_PIXEL_SELECTION_FREQUENCY',   5)


C     Set up the unchanging parameters for the text box
C     that will report the projection being used

      CALL PSETI ('TEXT_LINE_COUNT',         1)
      CALL PSETR ('TEXT_BOX_X_POSITION',    15.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',     5.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',      12.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',       1.0)
      CALL PSETC ('TEXT_BOX_BLANKING',      'ON')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'SOLID')
      CALL PSETC ('TEXT_COLOUR',            'PURPLE')


C     Try this projection...

      CALL PSETC  ('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')
      CALL PIMAGE
      CALL PCOAST
      CALL PSETC  ('TEXT_MODE', 'TITLE')
      CALL PRESET ('TEXT_LINE_1')
      CALL PTEXT
      CALL PSETC  ('TEXT_MODE', 'POSITIONAL')
      CALL PSETC  ('TEXT_LINE_1', 'Projection: CYLINDRICAL')
      CALL PTEXT


C     Try this projection...

      CALL PNEW   ('SUPER_PAGE')
      CALL PSETC  ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PIMAGE
      CALL PCOAST
      CALL PSETC  ('TEXT_MODE', 'TITLE')
      CALL PRESET ('TEXT_LINE_1')
      CALL PTEXT
      CALL PSETC  ('TEXT_MODE', 'POSITIONAL')
      CALL PSETC  ('TEXT_LINE_1', 'Projection: POLAR_STEREOGRAPHIC')
      CALL PTEXT


C     Try this projection...

      CALL PNEW   ('SUPER_PAGE')
      CALL PSETC  ('SUBPAGE_MAP_PROJECTION','SATELLITE')
      CALL PIMAGE
      CALL PCOAST
      CALL PSETC  ('TEXT_MODE', 'TITLE')
      CALL PRESET ('TEXT_LINE_1')
      CALL PTEXT
      CALL PSETC  ('TEXT_MODE', 'POSITIONAL')
      CALL PSETC  ('TEXT_LINE_1', 'Projection: SATELLITE')
      CALL PTEXT


      CALL PCLOSE
      END


#include "parse_command_line.h"
