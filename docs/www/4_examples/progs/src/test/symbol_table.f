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


#include "parse_command_line.h"
