      PROGRAM SYMBOL_TABLE

C     Plots all MAGICS symbols


      INTEGER ISYMB, MAX_SYMBOLS
      INTEGER NUM_SYMBS_PER_COL, COL, ROW
      INTEGER NUM_ROWS, NUM_COLS, IROW, ICOL
      REAL    X, Y, Y_INC
      CHARACTER STRING*2
      CHARACTER SYMBNAME*5
      CHARACTER C1, C2

      PARAMETER   (N_OTHER_CODES=8)
      CHARACTER*2 CODES
      DIMENSION   CODES (N_OTHER_CODES)
      DATA        CODES /' N', ' W', 'CL', 'CM', 'CH', ' C', ' a', 'DS'/

      PARAMETER   (N_OTHER_NUMBERS=11)
      CHARACTER*1 CNUMBERS
      DIMENSION   CNUMBERS (N_OTHER_NUMBERS)
      DATA        CNUMBERS /'0','1','2','3','4','5','6','7','8',
     x                      '9','/'/



C     Start MAGICS and set the ouput device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('symbol_table')
      

C     Switch off the coastlines

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_GRID',             'OFF')     


C     Generate a plot of all 27 'standard' symbols

      CALL PSETC ('SYMBOL_TYPE', 'MARKER') 
      CALL PSET1R('SYMBOL_INPUT_NUMBER_LIST', (0.), 1)

      MAX_SYMBOLS = 27
      NUM_COLS    = 2
      NUM_SYMBS_PER_COL = (MAX_SYMBOLS + 1) / NUM_COLS
      Y_INC       = 160.0 / NUM_SYMBS_PER_COL
      X_INC       = 20.0


      CALL PSETR ('SYMBOL_HEIGHT', 0.4)


      DO 10 ISYMB = 0, MAX_SYMBOLS

        COL = (ISYMB / NUM_SYMBS_PER_COL)
        ROW = ISYMB - (COL * NUM_SYMBS_PER_COL)
        X   = 10.0 + COL * X_INC
        Y   = 90 - ((ROW + 1) * Y_INC)

        CALL PSET1R ('SYMBOL_INPUT_X_POSITION', X,  1)
        CALL PSET1R ('SYMBOL_INPUT_Y_POSITION', Y,  1)
        CALL PSETI  ('SYMBOL_MARKER', ISYMB)     
        CALL PSYMB

  10  CONTINUE




C     --------------------------------------------------------
C     Start a new page, this time showing the WMO 'ww' symbols
C     --------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PRESET ('SYMBOL_MARKER')


      NUM_ROWS = 10
      NUM_COLS = 10
      Y_INC       = 160.0 / NUM_ROWS
      X_INC       = 320.0 / NUM_COLS

      CALL PSETR ('SYMBOL_HEIGHT', 0.4)

      DO 20 IROW = 0, NUM_ROWS - 1
        DO 40 ICOL = 0, NUM_COLS - 1

          X   = -130 + ICOL * X_INC
          Y   =  90  - ((IROW + 1) * Y_INC)
        
          WRITE(UNIT=STRING, FMT='(I1,I1)') IROW, ICOL
          SYMBNAME = 'ww_' // STRING

c          write (*,*) 'X: ', X
c          write (*,*) 'Y: ', Y
c          write (*,*) 'S: ', SYMBNAME

          CALL PSET1R ('SYMBOL_INPUT_X_POSITION', X,  1)
          CALL PSET1R ('SYMBOL_INPUT_Y_POSITION', Y,  1)
          CALL PSETC  ('SYMBOL_NAME', SYMBNAME)     
          CALL PSYMB

  40    CONTINUE
  20  CONTINUE



C     -----------------------------------------------------
C     Start a new page, this time showing the other symbols
C     -----------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PRESET ('SYMBOL_MARKER')
      write (*,*) '------------------------------------'


      NUM_ROWS = N_OTHER_NUMBERS
      NUM_COLS = N_OTHER_CODES
      Y_INC       = 160.0 / NUM_ROWS
      X_INC       = 320.0 / NUM_COLS

      CALL PSETR ('SYMBOL_HEIGHT', 0.4)

      DO 60 IROW = 0, NUM_ROWS - 1
        DO 70 ICOL = 0, NUM_COLS - 1

          X   = -130 + ICOL * X_INC
          Y   =  90  - ((IROW + 1) * Y_INC)
        
c          WRITE(UNIT=STRING, FMT='(I1,I1)') ICOL, IROW
c          SYMBNAME = CODES(ICOL) // CNUMBERS(CROW)

          C1 = CODES(ICOL + 1)(1:1)
          C2 = CODES(ICOL + 1)(2:2)
          IF (C1.EQ.' ') THEN
            SYMBNAME = C2 // '_' // CNUMBERS(IROW+1)
          ELSE
            SYMBNAME = C1 // C2  // '_' // CNUMBERS(IROW+1)
          ENDIF

c          write (*,*) 'X: ',  X
c          write (*,*) 'Y: ',  Y
c          write (*,*) 'S: .', SYMBNAME, '.'

          CALL PSET1R ('SYMBOL_INPUT_X_POSITION', X,  1)
          CALL PSET1R ('SYMBOL_INPUT_Y_POSITION', Y,  1)
          CALL PSETC  ('SYMBOL_NAME', SYMBNAME)
          CALL PSYMB

  70    CONTINUE
  60  CONTINUE

      

C     Set up and plot the title text
     
      CALL PTEXT
      CALL PCOAST



C     -------------------------------------------
C     Start a new page, this time showing numbers
C     -------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PSETC ('SYMBOL_TYPE', 'NUMBER') 

      NUM_ROWS = 10
      NUM_COLS = 10
      Y_INC       = 160.0 / NUM_ROWS
      X_INC       = 320.0 / NUM_COLS

      CALL PSETR ('SYMBOL_HEIGHT', 0.4)

      DO 90 IROW = 0, NUM_ROWS - 1
        DO 80 ICOL = 0, NUM_COLS - 1

          X   = -130 + ICOL * X_INC
          Y   =  90  - ((IROW + 1) * Y_INC)
        
          CALL PSET1R ('SYMBOL_INPUT_X_POSITION', X,  1)
          CALL PSET1R ('SYMBOL_INPUT_Y_POSITION', Y,  1)
          CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST', X / Y, 1)
          CALL PSYMB

  80    CONTINUE
  90  CONTINUE



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
