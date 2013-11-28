      PROGRAM LAYOUTAUTO

C     This program demonstrates the automatic layout facilities in MAGICS.
C     We generate two plots on the same page.


      PARAMETER (NLEV=6)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /54500.,55000.,55500.,56000.,56500.,57000./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout_auto_ex')

      CALL PSETC ('TEXT_MODE',     'POSITIONAL')
      CALL PSETC ('TEXT_LINE_1', 'Positional Text 1')



C     Set up the main page dimensions

      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)
      CALL PSETR ('PAGE_X_LENGTH',       12.0)
      CALL PSETR ('PAGE_Y_LENGTH',       15.0)
      CALL PSETR ('PAGE_X_POSITION',      1.5)
      CALL PSETR ('PAGE_Y_POSITION',      1.5)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -18.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',         'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',    'z500.grb')

      CALL PSETC ('GRIB_AUTOMATIC_SCALING', 'OFF')



      CALL PSETC ('GRIB_SPECIFICATION',      'OFF')
      CALL PSETC ('GRIB_SUBAREA_EXTRACTION', 'OFF')
      CALL PGRIB


C     Define the contour     

      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R('CONTOUR_LEVEL_LIST',            RLEV, NLEV)


C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     Page 1

      CALL PSETR ('SUBPAGE_X_LENGTH',       5.0)
      CALL PSETR ('SUBPAGE_Y_LENGTH',       12.0)
      CALL PSETR ('SUBPAGE_X_POSITION',      1.5)
      CALL PSETR ('SUBPAGE_Y_POSITION',      1.5)
      CALL PSETC ('TEXT_LINE_1', 'Page 1')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


      CALL PSETR ('TEXT_BOX_X_POSITION', 2.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 1.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    4.0)
      CALL PSETC ('TEXT_LINE_1', 'text 1')
      CALL PTEXT





C     Page 2

      CALL PNEW  ('PAGE')

      CALL PSETR ('SUBPAGE_X_LENGTH',        6.0)
      CALL PSETR ('SUBPAGE_Y_LENGTH',       12.0)
      CALL PSETR ('SUBPAGE_X_POSITION',      6.5)
      CALL PSETR ('SUBPAGE_Y_POSITION',      1.5)

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    35.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)

      CALL PSETC ('TEXT_LINE_1', 'Page 2')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


      CALL PSETR ('TEXT_BOX_X_POSITION', 2.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 1.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    4.0)
      CALL PSETC ('TEXT_LINE_1', 'text 2')
      CALL PTEXT


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

