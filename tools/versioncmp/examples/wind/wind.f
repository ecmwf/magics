      PROGRAM TEST

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 145.0)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '500uv.grb')

C     Start a new page, this time showing the wind arrows

      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PSETC ('LEGEND', 'ON')

C     Plot the wind field using default wind-plotting paramters

      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, no thinning')
      CALL PSETC ('TEXT_LINE_2', ' ')

      CALL PCOAST 
      CALL PGRIB
      CALL PWIND
      CALL PSETI ('TEXT_LINE_COUNT',     2)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PTEXT


C     Start a new page, this time with no thinning

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)
      CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY', 'OFF')
      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, thinning 1.0')
      CALL PSETC ('TEXT_LINE_2', 'Arrows not truncated at boundary')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time with thinning

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR',       2.6)
      CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY', 'ON')
      CALL PSETC ('WIND_ARROW_COLOUR',         'RED')
      CALL PSETI ('WIND_ARROW_THICKNESS',       3)
      CALL PSETC ('TEXT_LINE_1', 'Thick RED wind arrows')
      CALL PSETC ('TEXT_LINE_2', 'Thinning 2.6')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time with thinning of 8.0

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR', 8.0)
      CALL PSETI ('WIND_ARROW_THICKNESS',  1)
      CALL PSETI ('WIND_ARROW_HEAD_INDEX', 14)
      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, thinning 8.0')
      CALL PSETC ('TEXT_LINE_2', 'Arrow head: 14')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, and turn the calm indicator on

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR', 3.0)
      CALL PSETC ('WIND_ARROW_CALM_INDICATOR', 'ON')
      CALL PSETI ('WIND_ARROW_HEAD_INDEX', 14)
      CALL PSETC ('TEXT_LINE_1', 'Thinning 3.0, CALM ON')
      CALL PSETC ('TEXT_LINE_2', 'Arrow head: 14')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time with different attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, origin CENTRE')
      CALL PSETC ('TEXT_LINE_2', 'Calm indicator size: 0.6')
      CALL PSETC ('WIND_ARROW_ORIGIN_POSITION',    'CENTRE')
      CALL PSETR ('WIND_ARROW_CALM_INDICATOR_SIZE', 0.6)
      CALL PSETI ('WIND_ARROW_HEAD_INDEX',          22)
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time testing the unit arrow length

      CALL PNEW   ('SUPER_PAGE')
      CALL PRESET ('WIND_ARROW_ORIGIN_POSITION')
      CALL PRESET ('WIND_ARROW_CALM_INDICATOR_SIZE')
      CALL PRESET ('WIND_ARROW_HEAD_INDEX')
      CALL PSETR  ('WIND_ARROW_UNIT_VELOCITY', 10.0)
      CALL PSETI  ('TEXT_LINE_COUNT',     4)
      CALL PSETC  ('TEXT_LINE_1', 'Wind arrows, origin PRESET')
      CALL PSETC  ('TEXT_LINE_2', 'Calm indicator size: PRESET')
      CALL PSETC  ('TEXT_LINE_3', 'WIND_ARROW_HEAD_INDEX: PRESET')
      CALL PSETC  ('TEXT_LINE_4', 'WIND_ARROW_UNIT_VELOCITY: 10.0')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT
      CALL PSETI ('TEXT_LINE_COUNT',     2)

C     Start a new page, this time testing the unit arrow length
C     Setting it to zero should make the maximum wind speed the
C     unit vector.

      CALL PNEW   ('SUPER_PAGE')
      CALL PRESET ('WIND_ARROW_ORIGIN_POSITION')
      CALL PRESET ('WIND_ARROW_CALM_INDICATOR_SIZE')
      CALL PRESET ('WIND_ARROW_HEAD_INDEX')
      CALL PSETR  ('WIND_ARROW_UNIT_VELOCITY', 0.0)
      CALL PSETI  ('TEXT_LINE_COUNT',     4)
      CALL PSETC  ('TEXT_LINE_1', 'Wind arrows, origin PRESET')
      CALL PSETC  ('TEXT_LINE_2', 'Calm indicator size: PRESET')
      CALL PSETC  ('TEXT_LINE_3', 'WIND_ARROW_HEAD_INDEX: PRESET')
      CALL PSETC  ('TEXT_LINE_4', 'WIND_ARROW_UNIT_VELOCITY: 0.0')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT
      CALL PSETI ('TEXT_LINE_COUNT',     2)

C ------------------------ Wind Flags ---------------------------------

C     Start a new page, this time plotting with wind flags

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FIELD_TYPE', 'FLAGS')
      CALL PSETC ('TEXT_LINE_1', 'Wind plotting with flags')
      CALL PSETC ('TEXT_LINE_2', '')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time changing the flags' attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FLAG_COLOUR', 'KELLY_GREEN')
      CALL PSETR ('WIND_FLAG_CALM_INDICATOR_SIZE', 0.7)
      CALL PSETR ('WIND_FLAG_LENGTH',              1.8)
      CALL PSETC ('TEXT_LINE_1', 'Green flags, length: 1.8')
      CALL PSETC ('TEXT_LINE_2', 'Calm indicator size: 0.5')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time changing the flags' attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FLAG_CROSS_BOUNDARY',     'OFF')
      CALL PSETR ('WIND_THINNING_FACTOR',          2.0)
      CALL PSETR ('WIND_FLAG_CALM_INDICATOR_SIZE', 0.3)
      CALL PSETR ('WIND_FLAG_LENGTH',              1.0)
      CALL PSETC ('TEXT_LINE_1', 'Flags, thinning 2.0')
      CALL PSETC ('TEXT_LINE_2', 'not truncated at boundary')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time changing the flags' attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FLAG_CROSS_BOUNDARY',     'ON')
      CALL PSETC ('WIND_FLAG_MODE',               'OFF_LEVEL')
      CALL PSETR ('WIND_THINNING_FACTOR',          4.0)
      CALL PSETR ('WIND_FLAG_CALM_INDICATOR_SIZE', 0.3)
      CALL PSETC ('TEXT_LINE_1', 'Flags, thinning 4.0')
      CALL PSETC ('TEXT_LINE_2', 'Mode: OFF_LEVEL')
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time changing the flags' attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FLAG_CROSS_BOUNDARY',     'ON')
      CALL PSETC ('WIND_FLAG_MODE',               'OFF_TIME')
      CALL PSETR ('WIND_THINNING_FACTOR',          4.0)
      CALL PSETR ('WIND_FLAG_CALM_INDICATOR_SIZE', 0.3)
      CALL PSETC ('TEXT_LINE_1', 'Flags, thinning 4.0')
      CALL PSETC ('TEXT_LINE_2', 'Mode: OFF_TIME')
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time changing the flags' attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FLAG_CROSS_BOUNDARY',     'ON')
      CALL PSETC ('WIND_FLAG_MODE',               'NORMAL')
      CALL PSETC ('WIND_FLAG_ORIGIN_MARKER',      'DOT')
      CALL PSETI ('WIND_FLAG_THICKNESS',           3)
      CALL PSETC ('TEXT_LINE_1', 'Flags, thickness: 3')
      CALL PSETC ('TEXT_LINE_2', 'ORIGIN_MARKER: DOT')
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time changing the flags' attributes

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('WIND_FLAG_ORIGIN_MARKER',      'OFF')
      CALL PSETC ('TEXT_LINE_1', 'Flags, thickness: 3')
      CALL PSETC ('TEXT_LINE_2', 'ORIGIN_MARKER: OFF')
      CALL PWIND
      CALL PTEXT

C     Start a new page, this time with a new area so we can look at
C     stronger winds

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   -40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',  -20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 100.0)

      CALL PSETC ('WIND_FLAG_CROSS_BOUNDARY',     'ON')
      CALL PSETC ('WIND_FLAG_MODE',               'NORMAL')
      CALL PSETC ('WIND_FLAG_ORIGIN_MARKER',      'CIRCLE')
      CALL PSETI ('WIND_FLAG_THICKNESS',           1)
      CALL PSETC ('TEXT_LINE_1', 'Flags, strong winds, thickness: 1')
      CALL PSETC ('TEXT_LINE_2', 'ORIGIN_MARKER: CIRCLE')
      CALL PWIND
      CALL PTEXT

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
      CHARACTER*32 DEVICE
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME

      CHARACTER*16 FORMATS_PS_PNG_SVG
      DIMENSION    FORMATS_PS_PNG_SVG(3)
      DATA         FORMATS_PS_PNG_SVG /'PS', 'PNG', 'SVG'/

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
C        Set the device?
C
          IF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

C             Set the output filename

              IF     (DEVICE.EQ.'OLD')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_DEVICE',   'ps_a4')
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'PS') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'EPS') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'EPS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'AGIF') THEN
                CALL PSETC ('OUTPUT_FORMAT','GIF_ANIMATION')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
                CALL PSETI ('OUTPUT_GIF_DELAY',     150)
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PNG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'SVG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'KML') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'KML')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)

              ELSEIF (DEVICE.EQ.'PS_PNG_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_PNG_SVG, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'ALL') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_ALL, 5)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1
          ENDIF
          I = I + 1 
          GOTO 20
      ENDIF
C
C     If no device has been set, then use PostScript by default
C
      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME //'.ps'
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF
C
      END
