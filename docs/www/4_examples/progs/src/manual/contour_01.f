      PROGRAM CONTOUR01

C     Define our contour levels and colours

      PARAMETER (NLEV=16)
      DIMENSION RLEV (NLEV+1)
      DATA RLEV /-50., 
     +            -8.,  -4.,   0.,   4.,  8.,
     +            12.,  16.,  20.,  24., 30.,
     +            34.,  38., 42., 46., 50., 54./
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'BLUE_PURPLE',
     +                  'GREENISH_BLUE',
     +                  'BLUE_GREEN',
     +                  'BLUISH_GREEN',
     +                  'YELLOW_GREEN',
     +                  'GREENISH_YELLOW',
     +                  'YELLOW',
     +                  'ORANGISH_YELLOW',
     +                  'ORANGE_YELLOW',
     +                  'YELLOWISH_ORANGE',
     +                  'ORANGE',
     +                  'REDDISH_ORANGE',
     +                  'RED_ORANGE',
     +                  'ORANGISH_RED',
     +                  'RED',
     +                  'MAGENTA'/


C     Tables for marker shading

      DIMENSION HTAB(NLEV), MTAB(NLEV)
      DATA MTAB /1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 12, 14, 15, 16/
      DATA HTAB /0.22, 0.23, 0.21, 0.18, 0.17, 0.15, 0.18, 0.18, 0.18,
     +           0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18/



C     Open MAGICS and define our output file

      CALL POPEN()

      CALL PSETC ('OUTPUT_NAME', 'contour_01')

      CALL PSETR ('PAGE_X_LENGTH', 9.3)
      CALL PSETR ('PAGE_Y_LENGTH', 16.0)



C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   51.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)


C     Pass the temperature data to MAGICS

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t850_fc_12.grib')
      CALL PGRIB
      

C     Define the contour settings we'll use for all plots   

c      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST', RLEV,NLEV+1)
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('LEGEND',                       'OFF')
      


C     -----  Plot with solid shading  ------

      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PCONT

      CALL PSETC  ('TEXT_LINE_1', 'Solid Shading')
      CALL PTEXT
      CALL PCOAST


C     -----  Plot with hatch shading  ------

      CALL PNEW  ('PAGE')
      CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC ('CONTOUR_SHADE_METHOD',         'HATCH')
      CALL PCONT

      CALL PSETC  ('TEXT_LINE_1', 'Hatch Shading')
      CALL PTEXT
      CALL PCOAST


C     -----  Plot with dot shading   ------

      CALL PNEW  ('PAGE')
      CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',       'POLYGON_SHADING')
      CALL PSETC ('CONTOUR_SHADE_METHOD',          'DOT')
      CALL PSETR ('CONTOUR_SHADE_DOT_SIZE',             0.5)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL_DENSITY',  100.0)
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL_DENSITY',   30.0)
      CALL PCONT

      CALL PSETC  ('TEXT_LINE_1', 'Dot Shading')
      CALL PTEXT
      CALL PCOAST


C     -----  Plot with marker shading   ------

      CALL PNEW   ('PAGE')
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',   'MARKER')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_TABLE', CTAB, NLEV)
      CALL PSET1R ('CONTOUR_SHADE_HEIGHT_TABLE', HTAB, NLEV)
      CALL PSET1I ('CONTOUR_SHADE_MARKER_TABLE', MTAB, NLEV)
      CALL PCONT

      CALL PSETC  ('TEXT_LINE_1', 'Marker Shading')
      CALL PTEXT
      CALL PCOAST


C     -----  Plot with cell shading, low resolution   ------

      CALL PNEW   ('SUPER_PAGE')
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',       'CELL_SHADING')
      CALL PSETR  ('CONTOUR_SHADE_CELL_RESOLUTION',  5.0)
      CALL PCONT

      CALL PSETC  ('TEXT_LINE_1', 'Cell Shading, low resolution')
      CALL PTEXT
      CALL PCOAST


C     -----  Plot with cell shading, default resolution   ------

      CALL PNEW   ('PAGE')
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',   'CELL_SHADING')
      CALL PRESET ('CONTOUR_SHADE_CELL_RESOLUTION')
      CALL PCONT

      CALL PSETC  ('TEXT_LINE_1', 'Cell Shading, default resolution')
      CALL PTEXT
      CALL PCOAST



      CALL PCLOSE

      STOP
      END

