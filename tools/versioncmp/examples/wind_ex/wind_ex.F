C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM WIND

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_ex')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')


C     Area specification (SOUTH, WEST, NORTH, EAST )

C      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    60.0)
C      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -6.0)
C      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   68.0)
C      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  6.0)

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 145.0)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv500.grb')
c      CALL psetc ('GRIB_WIND_MODE',       'SD')
c      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/wave_height.grib')



C     Debug
c      CALL PSETI ('WIND_ARROW_THICKNESS',     1)
c      CALL PSETR ('WIND_ARROW_UNIT_VELOCITY', 25.0)




C     Show the UV fields as grid points
C
C      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE', 'BOTH')
C      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'ON')
C      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 1)
C      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 1)
C      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)
C      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT', 0.2)
C
C
Cc      CALL PSETI ('GRIB_FIELD_POSITION', 1)               
C      CALL PGRIB
C      CALL PSETC ('TEXT_LINE_1', 'U field values')
C      CALL PCONT
C      CALL PCOAST 
C      CALL PTEXT
C
C      CALL PNEW  ('SUPER_PAGE')
Cc      CALL PSETI ('GRIB_FIELD_POSITION', 2)               
C      CALL PGRIB
C      CALL PSETC ('TEXT_LINE_1', 'V field values')
C      CALL PCONT
C      CALL PCOAST 
C      CALL PTEXT
C





C     Start a new page, this time showing the wind arrows

C       CALL PNEW  ('SUPER_PAGE')

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
