C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM FONT_TABLES

C     Plots all characters from each font
C     This program was prompted by a request from 
C     Per Kallberg to clarify the font tables in the MAGICS
C     documentation. The problems is that there seemed to be
C     no surviving source code for the program that generated
C     those tables. So here's a new one.


      INTEGER NUM_ROWS, NUM_COLS, IROW, ICOL
      REAL    X, Y, Y_INC


C     Start MAGICS and set the ouput device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('text_font_tables')
      
      CALL PSETR ('SUPER_PAGE_X_LENGTH',  21.0)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',  29.7)
      CALL PSETR ('PAGE_X_LENGTH',        21.0)
      CALL PSETR ('PAGE_Y_LENGTH',        29.7)
c      CALL PSETR ('SUBPAGE_X_LENGTH',     65.0)
c      CALL PSETR ('SUBPAGE_Y_LENGTH',     65.0)
c      CALL PSETR ('SUBPAGE_X_POSITION',   1.05)
c      CALL PSETR ('SUBPAGE_Y_POSITION',   2.08)
      CALL PSETC ('SUBPAGE_FRAME',       'OFF')
c      CALL PSETC ('SUBPAGE_FRAME_COLOUR','RED')
      CALL PSETC ('PAGE_FRAME',          'OFF')
      CALL PSETC ('PAGE_ID_LINE',        'OFF')
c      CALL PSETC ('TEXT_BORDER', 'ON')
      

C     Generate the font tables

      CALL FONT_TABLE ('LOW',    'Times-Roman',    0)
      CALL FONT_TABLE ('MEDIUM', 'Helvetica',      1)
      CALL FONT_TABLE ('HIGH',   'Helvetica-Bold', 1)

      CALL PCLOSE

      STOP
      END




      SUBROUTINE FONT_TABLE (QUALITY, FONT, INEW)
      
          CHARACTER*(*) QUALITY
          CHARACTER*(*) FONT
          CHARACTER*80  TITLE        
          CHARACTER     OCT_STRING*3
          CHARACTER     SYMBNAME*12
          INTEGER       INDEX


C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


C         Set the font

          CALL PSETC ('TEXT_QUALITY', QUALITY)


C         Plot the title
C         Note that #043 is the '#' character itself

          CALL PSETC ('TEXT_MODE', 'POSITIONAL')
          CALL PSETR ('TEXT_BOX_X_POSITION', 2.0)
          CALL PSETR ('TEXT_BOX_Y_POSITION', 28.0)
          CALL PSETR ('TEXT_BOX_X_LENGTH',   18.0)
          CALL PSETR ('TEXT_BOX_Y_LENGTH',   1.2)
          CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.8)
          WRITE(TITLE,'(A,A)') FONT, ' , Format: octal #043nnn'
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PSETC ('TEXT_COLOUR',        'BLACK')
          CALL PTEXT


C         Set up our table parameters

          NUM_ROWS = 32
          NUM_COLS = 7
          Y_INC    = 25.0 / NUM_ROWS
          X_INC    = 20.0 / NUM_COLS
          INDEX    = 32

          CALL PSETR ('TEXT_BOX_X_LENGTH',    2.0)
          CALL PSETR ('TEXT_BOX_Y_LENGTH',    2.0)
          CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.4)


          DO 20 ICOL = 0, NUM_COLS - 1
            DO 40 IROW = 0, NUM_ROWS - 1

              X   = 0.5 + ICOL * X_INC
              Y   = 28.0  - ((IROW + 1) * Y_INC)

              WRITE(UNIT=OCT_STRING, FMT='(O3.3)') INDEX
              SYMBNAME = '' // '#' // OCT_STRING

              IF (OCT_STRING.EQ.'060')  THEN
                SYMBNAME = 'LESSTHAN'
              ENDIF

              CALL PSETC ('TEXT_COLOUR',        'BLACK')
              CALL PSETR ('TEXT_BOX_X_POSITION', X)
              CALL PSETR ('TEXT_BOX_Y_POSITION', Y)
              CALL PSETC ('TEXT_LINE_1', OCT_STRING)
              CALL PTEXT

              CALL PSETC ('TEXT_COLOUR',        'BLUE')
              CALL PSETR ('TEXT_BOX_X_POSITION', X + 0.7)
              CALL PSETC ('TEXT_LINE_1', SYMBNAME)
              CALL PTEXT

              INDEX = INDEX + 1

40         CONTINUE
20       CONTINUE


      RETURN
      END



#include "parse_command_line.h"
