C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM METOPS_WEATHER_RISK

c
c     open magics
      
      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_weather_risk')
      

C   set up the page

      CALL PSETR ('SUPER_PAGE_X_LENGTH',   42.0)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',   29.7)
      CALL PSETR ('PAGE_X_LENGTH',         42.0)
      CALL PSETR ('PAGE_Y_LENGTH',         29.7)
      CALL PSETC ('SUBPAGE_FRAME_COLOUR', 'TAN')
      CALL PSETC ('PS_DEVICE',            'ps_a3')


C  set up and plot the coastlines

      CALL PSETC ('MAP_COASTLINE_COLOUR',         'RUST')
      CALL PSETC ('MAP_GRID',                     'ON')
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  10.)
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',  2)
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY', 2)
      CALL PSETR ('MAP_LABEL_HEIGHT',              0.3)
      CALL PSETC ('MAP_LABEL_QUALITY',            'HIGH')
      CALL PSETC ('MAP_GRID_COLOUR',              'GREY')
      CALL PSETC ('MAP_GRID_LINE_STYLE',          'DOT')
      CALL PSETC ('MAP_LABEL_COLOUR',             'BLACK')
c     CALL PSETC ('MAP_COASTLINE_LAND_SHADE',     'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')
      CALL PCOAST
      
  
c  generate text for title

      CALL PSETC ('TEXT_LINE_1',  'EFI EXPERIMENTAL MAPS')
      CALL PSETI ('TEXT_LINE_COUNT',                 1)
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.5)
      CALL PSETC ('TEXT_JUSTIFICATION',             'CENTRE')
      CALL PTEXT


  
c  load the Z1000_em data

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/em.grb')
      CALL PGRIB


c  define the contour Z1000_em     

      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',   'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',                4.)
      CALL PSETI ('CONTOUR_LINE_THICKNESS',          1)
      CALL PSETC ('CONTOUR_LINE_COLOUR',            'BLACK')
      CALL PSETI ('CONTOUR_HIGHLIGHT_THICKNESS',     3)
      CALL PSETC ('CONTOUR_LABEL_QUALITY',          'HIGH')
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',            0.25)
      CALL PSETR ('CONTOUR_HILO_SUPPRESS_RADIUS',    40.)
      CALL PSETR ('CONTOUR_HILO_REDUCTION_RADIUS',   40.)
      CALL PSETC ('CONTOUR_LO_COLOUR',              'SKY')
      CALL PSETC ('CONTOUR_HI_COLOUR',              'RED')
      CALL PSETC ('CONTOUR_HILO_QUALITY',           'LOW')
      CALL PSETC ('CONTOUR_HILO',                   'ON')
      CALL PSETR ('CONTOUR_HILO_HEIGHT',             0.5)
c     CALL PSETC ('GRIB_SCALING_OF_DERIVED_FIELDS', 'ON')
      CALL PCONT


c  load the 2m temperature data 

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/efi_2tm.grb')
      CALL PGRIB


C  contour for positive 2mT

c     CALL PSETC ('LEGEND',                         'ON')
      CALL PSETC ('CONTOUR',                        'OFF')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',   'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',                0.25) 
      CALL PSETR ('CONTOUR_REFERENCE_LEVEL',         0.5)
      CALL PSETC ('CONTOUR_SHADE',                  'ON')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',         0.5)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',         1.)	
      CALL PSETR ('CONTOUR_MIN_LEVEL',               0.5)
      CALL PSETR ('CONTOUR_MAX_LEVEL',               1.)	
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'YELLOW')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'ORANGE')
      CALL PSETC ('CONTOUR_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PSETC ('CONTOUR_SHADE_METHOD',           'AREA_FILL')
      CALL PSETC ('CONTOUR_LABEL',                  'OFF')
      CALL PSETC ('CONTOUR_HILO',                   'OFF')
      CALL PCONT 


C  contour for negative 2mT

      CALL PSETR ('CONTOUR_INTERVAL',                0.25) 
      CALL PSETR ('CONTOUR_REFERENCE_LEVEL',        -0.5)
      CALL PSETC ('CONTOUR_SHADE',                  'ON')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',        -1.)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',        -0.5)	
      CALL PSETR ('CONTOUR_MIN_LEVEL',              -1.)
      CALL PSETR ('CONTOUR_MAX_LEVEL',              -0.5)	
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'SKY')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'RGB(0.8,0.9,0.9)')
      CALL PCONT 
	 


	
C  Load the wind data

      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/efi_fx.grb')
      CALL PGRIB


C  Define the contour for the wind events

      CALL PSETC ('CONTOUR',                      'OFF')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',              0.2) 
      CALL PSETC ('CONTOUR_SHADE',                'ON')
      CALL PSETR ('CONTOUR_REFERENCE_LEVEL',       0.6)
      CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',      'MARKER') 
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',       0.6)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',       1.0)	
      CALL PSETR ('CONTOUR_MIN_LEVEL',             0.6)
      CALL PSETR ('CONTOUR_MAX_LEVEL',             1.0)
      CALL PSET1C('CONTOUR_SHADE_COLOUR_TABLE',
     x           ['BLUISH_PURPLE', 'BLUISH_PURPLE'], 2)
      CALL PSET1R('CONTOUR_SHADE_HEIGHT_TABLE', [0.02, 0.2], 2)
      CALL PSET1I('CONTOUR_SHADE_MARKER_TABLE', [5, 17],     2)
      CALL PSETC ('CONTOUR_LABEL',                'OFF')
      CALL PSETC ('CONTOUR_HILO',                 'OFF')
      CALL PCONT 





C  Load the data for the total precipitation index

      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/efi_tp.grb')
      CALL PGRIB
      
C  Define and plot the contours for the total precipitation index

      CALL PSETC  ('CONTOUR',                 'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',         0.2) 
      CALL PSETC  ('CONTOUR_SHADE',           'ON')
      CALL PSETR  ('CONTOUR_REFERENCE_LEVEL',  0.6)
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE', 'MARKER') 
      CALL PSETR  ('CONTOUR_SHADE_MIN_LEVEL',  0.6)
      CALL PSETR  ('CONTOUR_SHADE_MAX_LEVEL',  1.0)	
      CALL PSETR  ('CONTOUR_MIN_LEVEL',        0.6)
      CALL PSETR  ('CONTOUR_MAX_LEVEL',        1.0)
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_TABLE',
     x            ['KELLY_GREEN', 'KELLY_GREEN'] , 2)
      CALL PSET1R ('CONTOUR_SHADE_HEIGHT_TABLE', [0.02,0.2], 2)
      CALL PSET1I ('CONTOUR_SHADE_MARKER_TABLE', [6, 20], 2)
      CALL PSETC  ('CONTOUR_LABEL',           'OFF')
      CALL PSETC  ('CONTOUR_HILO',            'OFF')
      CALL PCONT


      CALL PCLOSE

      STOP
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
