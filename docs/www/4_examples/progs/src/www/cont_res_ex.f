C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTRES

C     This program demonstrates magics contouring facilities. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a 
C     polar stereographic map.
C     Different pages show the different contouring resolutions.
C     Note that MAGICS 6.9 does not support this option, and therefore
C     its output will be identical on all pages.


      PARAMETER (NLEV=7)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /40.,50.,60.,70.,80.,90., 100./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_res_ex')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    50.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -6.7)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   68.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  19.5)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/r850.grb')
      CALL PGRIB
      

C     Define the contour     

#if defined (MAGPLUS)

      CALL PSETC ('CONTOUR_METHOD',     'AKIMA760')
      CALL PSETR ('AKIMA_RESOLUTION_X',  1.5)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  1.5)
      
#endif

      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_LABEL_COLOUR',     'BLACK')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',   1)
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R('CONTOUR_LEVEL_LIST',            RLEV, NLEV)
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'OFF')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 4)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 4)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.4)
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETI ('TEXT_LINE_COUNT',     1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      
#if defined (MAGPLUS)

      CALL PSETC ('TEXT_LINE_1',  
     x            'Contouring AKIMA760, default resolution (1.5/1.5).')

#else

      CALL PSETC ('TEXT_LINE_1',  
     x            'Contouring CONICON, only one resolution available.')

#endif

      CALL PTEXT
      CALL PCOAST



#if defined (MAGPLUS)

C     New page, this time using a different contouring resolution

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('AKIMA_RESOLUTION_X',  1.2)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  1.2)
      CALL PSETC ('TEXT_LINE_1', 
     x            'Contouring AKIMA760, resolution (1.2/1.2)')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different contouring resolution

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('AKIMA_RESOLUTION_X',  1.0)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  1.0)
      CALL PSETC ('TEXT_LINE_1', 
     x            'Contouring AKIMA760, resolution (1.0/1.0)')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST
      
      
      
C     New page, this time using a different contouring resolution

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('AKIMA_RESOLUTION_X',  0.8)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  0.8)
      CALL PSETC ('TEXT_LINE_1', 
     x            'Contouring AKIMA760, resolution (0.8/0.8)')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST
      
      
      
C     New page, this time using a different contouring resolution

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('AKIMA_RESOLUTION_X',  0.5)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  0.5)
      CALL PSETC ('TEXT_LINE_1', 
     x            'Contouring AKIMA760, resolution (0.5/0.5)')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different contouring resolution

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('AKIMA_RESOLUTION_X',  0.1)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  0.1)
      CALL PSETC ('TEXT_LINE_1',
     x            'Contouring AKIMA760, resolution (0.1/0.1)')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST
      
#endif


C     Close

      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"

