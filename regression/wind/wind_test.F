      PROGRAM WIND

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_test')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')
	  call psetc ('legend', 'on')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    62.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   0.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  15.0)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'windspeed.grb')
      CALL PGRIB


C     -----------------------------------------------------------
C     General setup, and plotting of everything (all grid points)
C     -----------------------------------------------------------




C     Show the wind speed as grid points

      CALL PSETC ('CONTOUR',                         'OFF')
      CALL PSETC ('CONTOUR_METHOD',                  'LINEAR')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE',    'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',         'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY',  1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY',  1)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',         0.3)
      CALL PSETC ('CONTOUR_GRID_VALUE_COLOUR',        'GREY')
      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT',  0.2)
      CALL PSETC ('CONTOUR_GRID_VALUE_MARKER_COLOUR', 'ROSE')
      CALL PCONT


C     Coastlines...

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PCOAST 


C     Text...

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_LINE_1', 'Wind speed / arrows')
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Now plot the wind arrows on top

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)
      CALL PWIND



C     ------------------------------------------------------------
C     New page - set wind plotting paramters to remove some arrows
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'windspeed.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', -1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', -1)
      CALL PGRIB
      CALL PCONT

      CALL PCOAST 

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETR ('WIND_ARROW_MAX_SPEED',       11.0)
      CALL PSETR ('WIND_ARROW_MIN_SPEED',        3.0)
      CALL PSETC ('WIND_ARROW_CALM_INDICATOR',  'ON')
      CALL PSETR ('WIND_ARROW_CALM_BELOW',       5.0)
      CALL PSETC ('WIND_ARROW_ORIGIN_POSITION', 'CENTRE')

c      CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY', 'OFF')
      CALL PWIND


      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1',    'Wind speed / arrows')
      CALL PSETC ('TEXT_LINE_2',    
     x            'Between 3.0 and 11.0. Calm below 5.0')
      CALL PTEXT




C     ------------------------------------------------------------
C     New page - set wind plotting paramters to remove some flags
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'windspeed.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', -1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', -1)
      CALL PGRIB
      CALL PCONT

      CALL PCOAST 

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETC ('WIND_FIELD_TYPE',     'FLAGS')
      CALL PSETR ('WIND_FLAG_MAX_SPEED', 10.0)
      CALL PSETR ('WIND_FLAG_MIN_SPEED',  4.0)
      CALL PWIND

      CALL PSETC ('TEXT_LINE_1',    'Wind speed / flags')
      CALL PSETC ('TEXT_LINE_2',    
     x            'Between 4.0 and 10.0. Calm at default (0.5)')
      CALL PTEXT









C     ------------------------------------------------------------
C     New page - set wind plotting paramters to remove some arrows
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'windspeed.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', -1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', -1)
      CALL PGRIB
      CALL PCONT

      CALL PCOAST 

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETC ('WIND_FIELD_TYPE',            'ARROWS')
      CALL PSETR ('WIND_ARROW_MAX_SPEED',       11.0)
      CALL PSETR ('WIND_ARROW_MIN_SPEED',        3.0)
      CALL PSETC ('WIND_ARROW_CALM_INDICATOR',  'ON')
      CALL PSETR ('WIND_ARROW_CALM_BELOW',       5.0)
      CALL PSETC ('WIND_ARROW_ORIGIN_POSITION', 'CENTRE')

c      CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY', 'OFF')
      CALL PWIND


      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1',    'Wind speed / arrows')
      CALL PSETC ('TEXT_LINE_2',    
     x            'Between 3.0 and 11.0. Calm below 5.0')
      CALL PTEXT





C     ------------------------------------------------------------
C     New page - start testing the wind flags - do they have the
C     correct number and type of penants?
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'uv500.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB
      CALL PRESET ('GRIB_WIND_POSITION_1')               
      CALL PRESET ('GRIB_WIND_POSITION_2')
      CALL PSETC ('WIND_FIELD_TYPE',      'FLAGS')
      CALL PWIND

      CALL PSETC  ('GRIB_INPUT_FILE_NAME', 'windspeed500.grb')
      CALL PGRIB
      CALL PCONT

      CALL PSETI ('TEXT_LINE_COUNT', 1)
      CALL PSETC ('TEXT_LINE_1',    'Wind speed / flags')
      CALL PTEXT




C      CALL PNEW  ('SUPER_PAGE')
Cc      CALL PSETI ('GRIB_FIELD_POSITION', 2)               
C      CALL PGRIB
C      CALL PSETC ('TEXT_LINE_1', 'V field values')
C      CALL PCONT
C      CALL PCOAST 
C      CALL PTEXT


C
C
C      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'windspeed.grb')
C
C
C
CC     Start a new page, this time showing the wind arrows
C
C      CALL PNEW  ('SUPER_PAGE')
C
C      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
C      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
C      CALL PGRIB
C
C
CC     Plot the wind field using default wind-plotting paramters
C
C      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, no thinning')
C
C      CALL PCOAST 
C      CALL PWIND
C      CALL PTEXT
C
C
C      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'windspeed.grb')
C      CALL PCONT
C
C
C
CC     Start a new page, this time with no thinning
C
C      CALL PNEW  ('SUPER_PAGE')
C      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)
C      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, thinning 1.0')
C      CALL PCOAST 
C      CALL PWIND
C      CALL PTEXT


      CALL PCLOSE
      END


C --------------------------------------------------------------------
C     PARSE_COMMAND_LINE
C     Checks the command-line for any arguments.
C     Arguments can come in pairs. Currently supported arguments are:
C     PROJECTION <CYLINDRICAL | POLAR_STEREOGRAPHIC>
C     DEVICE <PS | SVG | PNG>
C     e.g. Run the program with:
C       <progname> PROJECTION CYLINDRICAL  DEVICE SVG
C --------------------------------------------------------------------

      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)

      CHARACTER*32 ARG
      CHARACTER*64 ID_TEXT
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*48 EXENAME
      CHARACTER*8  MINIMAL
      CHARACTER*8  SEPARATOR
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME

      CHARACTER*16 FORMATS_PS_GIF
      DIMENSION    FORMATS_PS_GIF(2)
      DATA         FORMATS_PS_GIF /'PS', 'GIF_ANIMATION'/

      CHARACTER*16 FORMATS_PS_GIF_PDF
      DIMENSION    FORMATS_PS_GIF_PDF(3)
      DATA         FORMATS_PS_GIF_PDF /'PS', 'GIF_ANIMATION', 'PDF'/

      CHARACTER*16 FORMATS_PS_GIF_SVG
      DIMENSION    FORMATS_PS_GIF_SVG(3)
      DATA         FORMATS_PS_GIF_SVG /'PS', 'GIF_ANIMATION', 'SVG'/

      CHARACTER*16 FORMATS_PS_PNG_SVG
      DIMENSION    FORMATS_PS_PNG_SVG(3)
      DATA         FORMATS_PS_PNG_SVG /'PS', 'PNG', 'SVG'/

      CHARACTER*16 FORMATS_PS_GIF_PNG_SVG
      DIMENSION    FORMATS_PS_GIF_PNG_SVG(4)
      DATA         FORMATS_PS_GIF_PNG_SVG /'PS', 'GIF_ANIMATION', 
     +                                     'PNG', 'SVG'/

      CHARACTER*16 FORMATS_ALL
      DIMENSION    FORMATS_ALL(5)
      DATA         FORMATS_ALL /'PS', 'SVG', 'PDF',
     +                          'KML', 'PNG'/

      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      DEVICE_SET = 0

      ID_TEXT = ''

      NUM_ARGS = IARGC()

      I = 1
C
C  Start GoTo
C
20    IF (I.LE.NUM_ARGS) THEN
          CALL GETARG ( I, ARG )
C
C         Set the projection?
C
          IF (ARG.EQ.'PROJECTION') THEN
              I = I + 1 
              CALL GETARG ( I, PROJECTION ) 
              CALL PSETC ('SUBPAGE_MAP_PROJECTION', PROJECTION)
C
C        Set the device?
C
          ELSEIF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

C             Set the output filename

              IF     (DEVICE.EQ.'PS')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_DEVICE',   'ps_a4')
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'PS_NEW') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'EPS') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'EPS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF_ANIMATION')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
                CALL PSETI ('OUTPUT_GIF_DELAY',     150)
              ELSEIF (DEVICE.EQ.'GIF_MULTI') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PNG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'JPEG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'JPEG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'SVG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'CSVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'CSVG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'KML') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'KML')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)

              ELSEIF (DEVICE.EQ.'BAD') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'BAD')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)

              ELSEIF (DEVICE.EQ.'PS_GIF') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF, 2)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_GIF_PDF') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF_PDF, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_GIF_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF_SVG, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_PNG_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_PNG_SVG, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_GIF_PNG_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS',
     +                               FORMATS_PS_GIF_PNG_SVG, 4)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'ALL') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_ALL, 5)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1
C
C        Split the PostScript pages into separate files?
C
          ELSEIF (ARG.EQ.'PS_SPLIT') THEN
                CALL PSETC ('OUTPUT_PS_SPLIT',     'ON')
C
C        Turn on the numbering for the first page?
C
          ELSEIF (ARG.EQ.'FIRST_PAGE_NUMBER') THEN
                CALL PSETC ('OUTPUT_NAME_FIRST_PAGE_NUMBER', 'ON')
C
C         Set the minimal filename number width?
C
          ELSEIF (ARG.EQ.'MINIMAL') THEN
              I = I + 1 
              CALL GETARG (I, MINIMAL) 
              IF     (MINIMAL.EQ.'0')  THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 0)
              ELSEIF (MINIMAL.EQ.'1')  THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 1)
              ELSEIF (MINIMAL.EQ.'2') THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 2)
              ELSEIF (MINIMAL.EQ.'3') THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 3)
              ENDIF
C
C         Set the file number separator?
C
          ELSEIF (ARG.EQ.'SEP') THEN
              I = I + 1 
              CALL GETARG ( I, SEPARATOR ) 
              CALL PSETC ('OUTPUT_FILE_SEPARATOR', SEPARATOR)
C
C        Run using linear contouring?
C
          ELSEIF (ARG.EQ.'LINEAR') THEN
                CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT', 'ON')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'LINEAR')
          ENDIF
          I = I + 1 
          GOTO 20
      ENDIF
C
C     If no device has been set, then use PostScript by default
C
      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME // '.ps'
        CALL PSETC ('PS_DEVICE',    'ps_a4')
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF
C
      END
