         PROGRAM METOPS_MSL_AND_PRECIP


      PARAMETER (NLEV=8)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /0., 1., 4., 8., 20., 50., 100., 200./
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'YELLOW',
     +                  'green',
     +                  'orange',
     +                  'red',
     +                  'purple',
     +                  'magenta',
     +                  'HSL(0,1,0.75)',
     +                   "RED" /
   


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_msl_and_precip')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    50.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  0.0)



C     First, load and plot the temperature data, shaded

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/total_precip.grib')
      CALL PGRIB
      
      
C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     


C     Define the contour     

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR',                      'ON')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('CONTOUR_LABEL',                'ON')
      CALL PSETC  ('contour_hilo_position_write',                'ON')
      CALL PSETC  ('contour_hilo_position_file_name',   'hilo.geo')
      CALL PCONT
      

C     Plot the title text and the coastlines

      CALL PSETR  ('TEXT_REFERENCE_CHARACTER_HEIGHT',  0.35)
      CALL PTEXT
      CALL PCOAST






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


