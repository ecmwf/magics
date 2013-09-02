      PROGRAM CONTLABELS
      
C     This program demonstrates contour labels in Magics++.
C     We use the automatic layout facilities to create a set of plots
C     stacked vertically from the top of the page.


      REAL SUPER_W
      REAL SUPER_H
      INTEGER NUM_PLOTS_PER_PAGE



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_labels')


C     Set up the main page dimensions

      SUPER_W = 21.0
      SUPER_H = 29.7
      NUM_PLOTS_PER_PAGE = 4
      

      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETC ('PLOT_START',         'TOP')
      CALL PSETC ('PLOT_DIRECTION',     'VERTICAL')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', SUPER_H)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', SUPER_W)

      CALL PSETR ('PAGE_Y_LENGTH', SUPER_H / (NUM_PLOTS_PER_PAGE*1.1))
      CALL PSETR ('PAGE_X_LENGTH', SUPER_W)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -90.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -120.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   -10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  120.0)

C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.38)
      CALL PSETC ('TEXT_COLOUR',       'BLACK')
      CALL PSETC ('TEXT_FONT_STYLE',   'BOLD')


C     Set up our generic contour attributes

      CALL PSETC ('CONTOUR_HILO', 'OFF')




C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Contour Labels')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_HEIGHT: 0.8')
      CALL PTEXT
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',  0.8)     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_HEIGHT: 0.2')
      CALL PTEXT
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',  0.2)     
      CALL PCONT


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_HEIGHT: 0.5')
      CALL PTEXT
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',  0.5)     
      CALL PCONT


C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label font quality
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1',
     x            'Height 0.5, CONTOUR_LABEL_QUALITY: DEFAULT')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_QUALITY: MEDIUM')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_QUALITY',  'MEDIUM')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_QUALITY: HIGH')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_QUALITY',  'HIGH')     
      CALL PCONT


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_COLOUR: LAVENDER')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_COLOUR',    'LAVENDER')     
      CALL PCONT



C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label fonts
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PRESET ('CONTOUR_LABEL_COLOUR')     
      CALL PRESET ('CONTOUR_LABEL_QUALITY')     


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Label Font')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_FONT: COURIER')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',  'COURIER')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_FONT: TIMES')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',  'TIMES')     
      CALL PCONT



C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label font styles
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')



C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Times, Bold')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'TIMES')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'BOLD')     
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Times, Italic')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'TIMES')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'ITALIC')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Times, Normal')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'TIMES')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'NORMAL')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Courier, Bold')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'COURIER')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'BOLD')     
      CALL PCONT




C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label frequencies
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PRESET ('CONTOUR_LABEL_FONT')
      CALL PRESET ('CONTOUR_LABEL_FONT_STYLE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Blanking OFF')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_BLANKING',  'OFF')     
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Blanking ON, Frequency 1')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_BLANKING',  'ON')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',  1)
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Frequency 3')
      CALL PTEXT
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',  3)
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Frequency 4')
      CALL PTEXT
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',  4)
      CALL PCONT


C     Close

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

