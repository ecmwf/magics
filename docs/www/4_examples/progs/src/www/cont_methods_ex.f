      PROGRAM CONTMETHODS

C     This program demonstrates magics contouring facilities. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a 
C     polar stereographic map.
C     Different pages show the different contouring techniques.
C     Note that MAGICS 6.9 can only perform LINEAR and CONICON
C     contouring.


      PARAMETER (NLEV=7)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /40.,50.,60.,70.,80.,90., 100./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_methods_ex')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    50.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -6.7)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   68.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  19.5)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/r850.grb')
      CALL PGRIB
      

C     Define the contour     

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
      

C     Set up the title text

      CALL PSETI ('TEXT_LINE_COUNT',     1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')


#if defined (MAGPLUS)

C     First page, this time using the linear contouring method

      CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
      CALL PSETC ('TEXT_LINE_1',
     x            'Contouring using method LINEAR')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different contouring method

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('CONTOUR_METHOD',     'AKIMA760')
      CALL PSETR ('AKIMA_RESOLUTION_X',  0.1)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  0.1)
      CALL PSETC ('TEXT_LINE_1', 
     x            'Contouring using method AKIMA760, resolution 0.1.')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST


C     New page, this time using a different contouring method

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('CONTOUR_METHOD',     'AKIMA474')
      CALL PSETR ('AKIMA_RESOLUTION_X',  0.1)
      CALL PSETR ('AKIMA_RESOLUTION_Y',  0.1)
      CALL PSETC ('TEXT_LINE_1',  
     x            'Contouring using method AKIMA474, resolution 0.1.')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST

#else

C     First page, this time using the default contouring method

      CALL PSETC ('CONTOUR_METHOD',     'CONICON')
      CALL PSETC ('TEXT_LINE_1',
     x            'Contouring using method CONICON')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST


C     New page, this time using a different contouring method

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
      CALL PSETC ('TEXT_LINE_1',    'Contouring using method LINEAR')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST

#endif

      
C     Close

      CALL PCLOSE

      STOP
      END

#include "parse_command_line.h"



