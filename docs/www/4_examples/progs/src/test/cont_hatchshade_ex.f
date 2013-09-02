      PROGRAM CONTHATCHSHADE

C     This program demonstrates hatch shading.


      PARAMETER (NLEV=11)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /480, 490., 500., 510., 520., 530.,
     +                 540., 550., 560., 570., 580./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_hatchshade_ex')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
c      CALL PSETC ('GRIB_SCALING',         'OFF')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_SHADE',            'ON')      
      call PSETC  ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
      call PSETC  ('CONTOUR_SHADE_METHOD',     'HATCH')
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1', 'Contours with hatch shading')
      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines

      CALL PCOAST


C     ------------------------------------------------------------
C     New page - this time with a smaller area
C     ------------------------------------------------------------

      CALL PNEW ('SUPER_PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    30.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  70.0)


C     Plot everything

      CALL PTEXT
      CALL PCOAST
      CALL PCONT




C     ------------------------------------------------------------
C     New page - this time with a constant hatch index
C     ------------------------------------------------------------

      CALL PNEW ('SUPER_PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PRESET ('SUBPAGE_LOWER_LEFT_LATITUDE')
      CALL PRESET ('SUBPAGE_LOWER_LEFT_LONGITUDE')
      CALL PRESET ('SUBPAGE_UPPER_RIGHT_LATITUDE')
      CALL PRESET ('SUBPAGE_UPPER_RIGHT_LONGITUDE')

      CALL PSETI  ('CONTOUR_SHADE_HATCH_INDEX', 3)
      CALL PSETC ('TEXT_LINE_1', 'Hatch shading with constant index')


C     Plot everything

      CALL PTEXT
      CALL PCOAST
      CALL PCONT


      CALL HATCH_TEST(0, 18.0)
      CALL HATCH_TEST(1, 18.0)
      CALL HATCH_TEST(2, 18.0)
      CALL HATCH_TEST(3, 18.0)
      CALL HATCH_TEST(4, 18.0)
      CALL HATCH_TEST(5, 18.0)
      CALL HATCH_TEST(6, 18.0)
      CALL HATCH_TEST(6,  5.0)
      CALL HATCH_TEST(4, 10.0)


      CALL PCLOSE

      STOP
      END


C    ------------------------------------------------------------
C    HATCH_TEST
C    Plots hatch shaded contours with specified attributes
C    ------------------------------------------------------------

      SUBROUTINE HATCH_TEST (INDEX, RDENSITY)
      
          CHARACTER*80  TITLE


          CALL PNEW  ('SUPER_PAGE')



C         Draw contours

          CALL PSETI  ('CONTOUR_SHADE_HATCH_INDEX', INDEX)
          CALL PSETR  ('CONTOUR_SHADE_HATCH_DENSITY', RDENSITY)
          CALL PCONT
C          CALL PCOAST


C         Plot the title

          WRITE(TITLE,'(A,I1,A,F4.1)') 'INDEX: ', INDEX, 
     X          ' DENSITY: ', RDENSITY
          CALL PSETC ('TEXT_LINE_1', TITLE)

          CALL PTEXT

      RETURN
      END


#include "parse_command_line.h"
