C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM IMPORT_1
C
C     Import image on lat/lon grid
C
C     Open MAGICS and set the output device
C
      CALL POPEN
      CALL PARSE_COMMAND_LINE ('import_1')
C
C     Load the image to import
C
      CALL PSETC ('import_file_name',
     +            'data/Metview4_logo_127px.png')
      CALL PSETC ('import_format',     'png')
      CALL PSETR ('import_x_position',  23.)
      CALL PSETR ('import_y_position',  16.)
      CALL PSETR ('import_width',        5.2)
      CALL PSETR ('import_height',       3.)      
      CALL PIMPORT  
C
      CALL PCOAST
C
C  START new page
C
C      CALL PNEW ('PAGE')
C      
C      CALL PSETC ('import_file_name','data/blue.png')
C      CALL PSETR ('import_x_position', -180.)
C      CALL PSETR ('import_y_position', -90.)
C      CALL PSETC ('import_format',     'png')
C      CALL PSETR ('import_width',      360.)
C      CALL PSETR ('import_height',     180.)
C     
C      CALL PIMPORT
C
C      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
C      CALL PSETC ('MAP_GRID_COLOUR',      'orange')
C      CALL PCOAST
      CALL PCLOSE
C
      STOP
      END

#include "parse_command_line.h"
