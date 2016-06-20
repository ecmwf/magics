C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTGRIDVALS

C     This program demonstrates magics contouring facilities. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a 
C     polar stereographic map.
C     A subarea is viewed and grid values are shown.


      PARAMETER (NLEV=6)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /40.,50.,60.,70.,80.,90./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_gridvals_ex')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    60.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -6.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   68.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  6.0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',         'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',    'data/r850.grb')
      CALL PSETC ('GRIB_SCALING',            'OFF')
      CALL PSETC ('GRIB_SPECIFICATION',      'OFF')
      CALL PSETC ('GRIB_SUBAREA_EXTRACTION', 'OFF')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_LABEL_COLOUR',     'BLACK')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',   1)
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R('CONTOUR_LEVEL_LIST',            RLEV, NLEV)
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE', 'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 1)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)
      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT', 0.2)
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1',  'Contours with grid values displayed')
      CALL PSETI ('TEXT_LINE_COUNT',     1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PTEXT


C     Plot the coastlines

      CALL PCOAST



C     ------------------------------------------------------------
C     New page - this time with a larger area
C     ------------------------------------------------------------

      CALL PNEW ('SUPER_PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   73.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)

      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE', 'MARKER')

C     Plot everything

      CALL PTEXT
      CALL PCOAST
      CALL PCONT

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
