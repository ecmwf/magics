      PROGRAM CONTFLATFIELD1

C     A test of Magics++ dot shading, using lots of different areas



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_flat_field_1')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
        


C     Set up the text attributes

      CALL PSETI ('TEXT_LINE_COUNT', 3)


C     Define the contour attributes

      CALL PSETC  ('LEGEND',                   'ON')
      CALL PSETC  ('CONTOUR_SHADE',            'ON')      
c      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
c      CALL PSETC  ('CONTOUR_SHADE_METHOD',     'DOT')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/cloudcover.grib')
      CALL PGRIB
      



C     Start plotting - one page per geographical area.

C                        LEFT , RIGHT, TOP , BOTTOM, NEWPAGE?
C      CALL PSETC ('TEXT_LINE_3', 'Whole Globe')
C      CALL CONTOUR_TEST (-180.,  180.,  90.,  -90.,   0)

C      CALL PSETC ('TEXT_LINE_3', 'Top-left corner')
C      CALL CONTOUR_TEST (-180., -140.,  90.,   60.,   1)
C
      CALL PSETC ('TEXT_LINE_3', 'Top-right corner')
      CALL CONTOUR_TEST ( 140.,  180.,  90.,   60.,   0)
C
C      CALL PSETC ('TEXT_LINE_3', 'Bottom-left corner')
C      CALL CONTOUR_TEST (-180., -140., -60.,  -90.,   1)
C
C      CALL PSETC ('TEXT_LINE_3', 'Bottom-right corner')
C      CALL CONTOUR_TEST ( 140.,  180., -60.,  -90.,   0)
C
C      CALL PSETC ('TEXT_LINE_3', 'Middle section')
C      CALL CONTOUR_TEST ( -30.,  70.,   50.,   30.,   1)
C
C      CALL PSETC ('TEXT_LINE_3', 'Inside the middle')
C      CALL CONTOUR_TEST ( -20.,   0.,   20.,   10.,   1)
C
C      CALL PSETC ('TEXT_LINE_3', 'Inside an enclosed blob')
C      CALL CONTOUR_TEST (  60.,  80.,   76.,   70.,   1)

      CALL PCLOSE

      STOP
      END



C    ------------------------------------------------------------
C    CONTOUR_TEST
C    Plots solid shaded contours with coastlines within the given
C    geographical area.
C    ------------------------------------------------------------

      SUBROUTINE CONTOUR_TEST (RLEFT, RIGHT, RTOP, RBOT, INEW)
      
          CHARACTER*80  TITLE
          REAL          RLATINC, RLONGINC


C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


C         Set up the geographical area

          CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)


C         Calculate the best lat/long label increments

          RLATINC  = (RTOP  - RBOT)  / 10.
          RLONGINC = (RIGHT - RLEFT) / 10.

          CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  RLONGINC)
          CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   RLATINC)
 



C         Draw contours and coastlines

          CALL PCONT
          CALL PCOAST


C         Plot the title

          WRITE(TITLE,'(A,4F6.1)') '(L,R,T,B): ',
     X          RLEFT, RIGHT, RTOP, RBOT
          CALL PSETC ('TEXT_LINE_2', TITLE)

          CALL PTEXT

      RETURN
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
