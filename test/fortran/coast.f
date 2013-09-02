      PROGRAM COAST
C
C     This program is intended to test different resolution coastlines
C     under Magics++ in order to determine how to define the automatic
C     coastline resolution selection algorithm.
C
      PARAMETER (RESLO=1.0)
      PARAMETER (RESHI=0.2)
      PARAMETER (NLEV=2)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /580., 590./
C
C     Open MAGICS and set the output device
C
      CALL POPEN
      CALL PARSE_COMMAND_LINE ('coast_res')
C
C     Set up the coastline attributes
C
      CALL PSETC ('MAP_GRID_COLOUR',              'GREY') 
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   10.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  20.0)
      CALL PSETC ('MAP_COASTLINE',                'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR',         'BLACK')
      CALL PSETC ('MAP_boundaries',         'on')
       CALL PSETC ('map_boundaries_path', 
     +   'boundaries.mapgen')
C
C     Set up the title text
C
      CALL PSETI ('TEXT_LINE_COUNT',     2)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
C
C     First page, this time using the global area
C
C      Misc
      CALL COAST_TEST (-180.0, 180.0, 90.0, -90.0,  '0.4',  0)
      CALL COAST_TEST (-160.0, 160.0, 80.0, -80.0, '0.075', 1)
      CALL COAST_TEST (-135.0, 135.0, 68.0, -68.0, '0.075', 1)
      CALL COAST_TEST (-90.0, 90.0, 45.0, -45.0, '0.075', 1)
      CALL COAST_TEST (-45.0, 45.0, 20.0, -20.0, '0.15', 1)
      CALL COAST_TEST (-45.0, 45.0, 70.0, 30.0, '0.15', 1)
      CALL COAST_TEST (-15.0, 45.0, 70.0, 35.0, '0.15', 1)
      CALL COAST_TEST (-15.0, 5.0, 60.0, 50.0, '0.025', 1)
C     Close
      CALL PCLOSE
      STOP
      END
C
      SUBROUTINE COAST_TEST (RLEFT, RIGHT, RTOP, RBOT, FACTOR, INEW)
C     
          CHARACTER*80  TITLE
          CHARACTER*32  COASTFILE
          CHARACTER*(*) FACTOR
C
C         Optionally start a new page
C
          IF (INEW.eq.1) THEN
            CALL PNEW  ('SUPER_PAGE')
          ENDIF
C
C         Set up the geographical area
C
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)
C
C         Plot the coastlines - one full res, one scaled
C
          COASTFILE = 'coast_' // FACTOR // '.nc'
          CALL PCOAST
C
C         Create and plot the title
C
          WRITE(TITLE,'(A,4F6.1,A,A)') '(L,R,T,B): ',
     X          RLEFT, RIGHT, RTOP, RBOT, '  Factor: ', FACTOR
          CALL PSETC ('TEXT_LINE_1', TITLE)
C
          WRITE(TITLE,'(A,2F6.1)') '(W,H): ',
     X          RIGHT - RLEFT, RTOP - RBOT
          CALL PSETC ('TEXT_LINE_2', TITLE)
c
          CALL PTEXT
      RETURN
      END
C
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
C
      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)

      CHARACTER*32 ARG
      CHARACTER*64 ID_TEXT
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*48 EXENAME
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME
      CHARACTER*10 FORMATS_PS_GIF
      DIMENSION    FORMATS_PS_GIF(2)
      DATA         FORMATS_PS_GIF /'PS', 'GIF'/
      CHARACTER*10 FORMATS_PS_GIF_PDF
      DIMENSION    FORMATS_PS_GIF_PDF(3)
      DATA         FORMATS_PS_GIF_PDF /'PS', 'GIF', 'PDF'/
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
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PNG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'SVG')
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
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1


C        Split the PostScript pages into separate files?

          ELSEIF (ARG.EQ.'PS_SPLIT') THEN
                CALL PSETC ('OUTPUT_PS_SPLIT',     'ON')


C        Turn on the numbering for the first page?

          ELSEIF (ARG.EQ.'FIRST_PAGE_NUMBER') THEN
                CALL PSETC ('OUTPUT_NAME_FIRST_PAGE_NUMBER', 'ON')


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
