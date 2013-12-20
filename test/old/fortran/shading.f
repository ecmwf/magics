         PROGRAM CONTSOLIDSHADETEST1

C     A test of Magics++ solid shading, using lots of different areas


      PARAMETER (NLEV=11)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /48000., 49000., 50000., 51000., 52000., 
     +                  53000.,
     +                 54000., 55000., 56000., 57000., 58000./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('shading')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT', 10.0)
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   5.0)
        


C     Set up the text attributes

      CALL PSETI ('TEXT_LINE_COUNT', 3)


C     Define the contour attributes

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_SHADE',            'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',     'AREA_FILL')


C     Pass the data to MAGICS

      



C     Start plotting - one page per geographical area.

C                        LEFT , RIGHT, TOP , BOTTOM, NEWPAGE?
      

      CALL PSETC ('TEXT_LINE_3', 'Bottom-right corner')
      CALL CONTOUR_TEST ( 140.,  180., -60.,  -70.,   0)


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

        
          

          CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)

          CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
          CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
          CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
          CALL PSETC ('GRIB_SCALING',         'OFF')
          CALL PGRIB
C         Set up the geographical area

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
C     PROJECTION 
C     DEVICE 
C     e.g. Run the program with:
C      PROJECTION POLAR_STEREOGRAPHIC
C      PROJECTION CYLINDRICAL   DEVICE SVG
C --------------------------------------------------------------------

      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)

      CHARACTER*32 ARG
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*48 EXENAME
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME
      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      DEVICE_SET = 0

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
              ELSEIF (DEVICE.EQ.'PDF') THEN
                CALL PSETC ('DEVICE',       'IM')
                OUTNAME = OUTROOTNAME //   '.pdf'
                CALL PSETC ('IM_FILE_NAME', OUTNAME) 
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('DEVICE',       DEVICE)
                OUTNAME = OUTROOTNAME //   '.svg'
                CALL PSETC ('SVG_FILE_NAME', OUTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1

          ELSEIF (ARG.EQ.'PS_SPLIT') THEN
                CALL PSETC ('PS_SPLIT',     'ON')
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


