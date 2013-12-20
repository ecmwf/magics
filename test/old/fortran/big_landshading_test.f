           PROGRAM LANDSEA_SHADING

C     No data is plotted, just the global coastline
C     We switch land and sea shading on and off to see what happens..


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('landsea_shading')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     First page: land and sea shading together:

      CALL PSETC ('TEXT_LINE_1', 'LAND and SEA shading')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE_COLOUR',  'BLUE')
      CALL PCOAST
      CALL PTEXT


C     Second page: land shading only:

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'LAND shading only')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'OFF')
      CALL PCOAST
      CALL PTEXT


C     Third page: sea shading only:

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'SEA shading only')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PCOAST
      CALL PTEXT


C     Fourth page:  land and sea shading together, zoom in:

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'LAND and SEA shading')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',      30.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',     65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',    70.0)
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PCOAST
      CALL PTEXT

      CALL PNEW  ('SUPER_PAGE')
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
      call psetc ('map_coastline_land_shade','on')
      call psetc ('map_coastline_sea_shade','on')
      call psetc ('map_grid','on')   
      call psetr ('subpage_lower_left_latitude',    -50.0)
      call pcoast

      call pnew('PAGE')
      call psetr ('subpage_lower_left_latitude',    -50.0)
      call psetr ('subpage_lower_left_longitude',  100.)
      call psetr ('subpage_upper_right_latitude',   -10.0)
      call psetr ('subpage_upper_right_longitude',  160.0)
      call pcoast

      call pnew('PAGE')
      call psetr ('subpage_lower_left_latitude',    -50.0)
      call psetr ('subpage_lower_left_longitude',  100.)
      call psetr ('subpage_upper_right_latitude',   -20.0)
      call psetr ('subpage_upper_right_longitude',  160.0)
      call pcoast

      call pnew('PAGE')
      call psetr ('subpage_lower_left_latitude',    40.0)
      call psetr ('subpage_lower_left_longitude',  -50.)
      call psetr ('subpage_upper_right_latitude',   90.0)
      call psetr ('subpage_upper_right_longitude',  50.0)
      call pcoast

      CALL PNEW  ('SUPER_PAGE')
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
      call psetc ('map_coastline_land_shade','on')
      call psetc ('map_coastline_land_shade_colour','cream')
      call psetc ('map_coastline_sea_shade','off')
      call psetc ('map_grid','on')   
      call psetr ('subpage_lower_left_latitude',    58.0)
      call psetr ('subpage_lower_left_longitude',  21.)
      call psetr ('subpage_upper_right_latitude',   65.0)
      call psetr ('subpage_upper_right_longitude',  28.0)

      call pcoast

      CALL PCLOSE

      STOP
      END



C --------------------------------------------------------------------
C     PARSE_COMMAND_LINE
C     Checks the command-line for any arguments.
C     Arguments can come in pairs. Currently supported arguments are:
C     PROJECTION 
C     DEVICE 
C     e.g. Run the program with:
C      PROJECTION POLAR_STEREOGRAPHIC
C      PROJECTION CYLINDRICAL   DEVICE SVG
C --------------------------------------------------------------------

      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)

      CHARACTER*32 ARG
      CHARACTER*64 ID_TEXT
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*48 EXENAME
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME
      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      DEVICE_SET = 0

      ID_TEXT = ''

      NUM_ARGS = IARGC()

      I = 1

20    IF (I.LE.NUM_ARGS) THEN
          CALL GETARG ( I, ARG ) 
          

C         Set the projection?

          IF (ARG.EQ.'PROJECTION') THEN
              I = I + 1 
              CALL GETARG ( I, PROJECTION ) 
              CALL PSETC ('SUBPAGE_MAP_PROJECTION', PROJECTION)


C        Set the device?

          ELSEIF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

C             Set the output filename

              IF     (DEVICE.EQ.'PS')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_DEVICE',   'ps_a4')
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
                CALL PSETC ('PS_PDF',      'BOTH')
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('DEVICE',       'GD')
                OUTNAME = OUTROOTNAME //   '.gif'
                CALL PSETC ('GD_FORMAT',   'ANIMATION')
                CALL PSETC ('GD_FILE_NAME', OUTNAME)
                CALL PSETI ('GD_DELAY',     150)
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('DEVICE',       'GD')
                OUTNAME = OUTROOTNAME //   '.png'
                CALL PSETC ('GD_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('DEVICE',       DEVICE)
                OUTNAME = OUTROOTNAME //   '.svg'
                CALL PSETC ('SVG_FILE_NAME', OUTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1


C        Split the PostScript pages into separate files?

          ELSEIF (ARG.EQ.'PS_SPLIT') THEN
                CALL PSETC ('PS_SPLIT',     'ON')


C        Run using linear contouring?

          ELSEIF (ARG.EQ.'LINEAR') THEN
                CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT', 'ON')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'LINEAR')
          ENDIF

          I = I + 1 
          GOTO 20
      ENDIF
      


C     If no device has been set, then use PostScript by default

      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME // '.ps'
        CALL PSETC ('PS_DEVICE',    'ps_a4')
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF

      END
