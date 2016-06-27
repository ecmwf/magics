C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM WINDARROWHEADS


      INTEGER ISTYLE, IRATIO, INDEX
      CHARACTER*80  TITLE
      PARAMETER (NRATIOS=5, NSHAPES=4)
      REAL      LAT_STEP,  LON_STEP
      REAL      LAT_FIRST, LON_FIRST
      REAL      LAT_LAST,  LON_LAST
      REAL      PAGE_X_LENGTH, PAGE_Y_LENGTH
      INTEGER   LAT, LON

      CALL POPEN
      CALL PSETC ('OUTPUT_FORMAT', 'PNG')
      CALL PSETC ('OUTPUT_NAME',   'diagram_wind_arrow_heads')
      CALL PSETI ('OUTPUT_WIDTH',   250)


      CALL PSETC ('PAGE_ID_LINE',  'OFF')
      CALL PSETC ('PAGE_FRAME',    'OFF')
      CALL PSETC ('SUBPAGE_FRAME', 'OFF')


C     Set the page dimensions 

      PAGE_X_LENGTH = 8.0
      PAGE_Y_LENGTH = 8.0

      CALL PSETR ('SUPER_PAGE_X_LENGTH', PAGE_X_LENGTH)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH', PAGE_Y_LENGTH)

      CALL PSETR ('PAGE_X_LENGTH', PAGE_X_LENGTH)
      CALL PSETR ('PAGE_Y_LENGTH', PAGE_Y_LENGTH)


C     Set the constant text parameters

      CALL PSETC ('TEXT_MODE',     'POSITIONAL')
      CALL PSETC ('TEXT_COLOUR',   'BLACK')
      CALL PSETC ('TEXT_BORDER',   'OFF')





C     Set the field data

      LAT_STEP  = -10.0
      LON_STEP  =  10.0
      LAT_FIRST =  50.0
      LON_FIRST =   0.0
      LAT_LAST  = LAT_FIRST + (LAT_STEP * NSHAPES)
      LON_LAST  = LON_FIRST + (LON_STEP * NRATIOS)


C     Set the geographical bounds to plot

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    LAT_LAST)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   LON_FIRST -  5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   LAT_FIRST + 10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  LON_LAST)



      CALL PSETR ('INPUT_FIELD_INITIAL_LATITUDE',  LAT_FIRST)             
      CALL PSETR ('INPUT_FIELD_INITIAL_LONGITUDE', LON_FIRST)             
      CALL PSETR ('INPUT_FIELD_LATITUDE_STEP',     LAT_STEP)                
      CALL PSETR ('INPUT_FIELD_LONGITUDE_STEP',    LON_STEP)                

      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)

      DO 10 I = 1, NSHAPES
        DO 20 J = 1, NRATIOS
          CALL DRAW_WIND_ARROW (I, J, NSHAPES, NRATIOS)
  20    CONTINUE
  10  CONTINUE




C     Draw text boxes for the plot title

      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')

      CALL PSETC ('TEXT_LINE_1',
     x         'WIND_ARROW_HEAD_SHAPE \\ RATIO')
      CALL PSETR ('TEXT_FONT_SIZE',      0.4)
      CALL PSETR ('TEXT_BOX_X_POSITION', 0.3)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 7.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',   10.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',   1.0)
      CALL PTEXT





      CALL PCLOSE

      STOP
      END



C    ------------------------------------------------------------
C    DRAW_WIND_ARROW
C    Defines and draws a new wind arrow with the specified shape
C    and ratio. The position of the arrow is automatically
C    fitted into a grid based on these parameters.
C    The supplied NRATIO will be divided by 10.
C    ------------------------------------------------------------

      SUBROUTINE DRAW_WIND_ARROW (NSHAPE, NRATIO, NSHAPES, NRATIOS)
      
          INTEGER   NSHAPE, NRATIO
          REAL      RATIO, TEXT_Y, TEXT_X
          DIMENSION U (NRATIOS,NSHAPES)
          DIMENSION V (NRATIOS,NSHAPES)
          CHARACTER STRING*15

C         Recompute our arrays of wind data. We want all points to have
C         zero wind except for the one we wish to plot now.

          DO 50 I = 1, NSHAPES
            DO 60 J = 1, NRATIOS
              IF (I.EQ.NSHAPE.AND.J.EQ.NRATIO) THEN
                U(J,I) = 20.0
                V(J,I) = 0.0
              ELSE
                U(J,I) = 0.0
                V(J,I) = 0.0
              ENDIF
60          CONTINUE
50        CONTINUE




C         Pass the data to MAGICS and plot it

          CALL PSET2R ('INPUT_WIND_U_COMPONENT', U, NRATIOS, NSHAPES)
          CALL PSET2R ('INPUT_WIND_V_COMPONENT', V, NRATIOS, NSHAPES)


          RATIO  = NRATIO * 0.2
          CALL PSETI ('WIND_ARROW_HEAD_SHAPE', NSHAPE - 1)
          CALL PSETR ('WIND_ARROW_HEAD_RATIO', RATIO)
          CALL PWIND


C         Draw a text box showing the arrow head shape

          TEXT_Y = 5.2 - ((NSHAPE-1) * 1.32)
          WRITE(UNIT=STRING, FMT='(I1)') NSHAPE - 1
          CALL PSETC ('TEXT_LINE_1',         STRING)
          CALL PSETR ('TEXT_FONT_SIZE',      0.4)
          CALL PSETR ('TEXT_BOX_X_POSITION', 0.3)
          CALL PSETR ('TEXT_BOX_Y_POSITION', TEXT_Y)
          CALL PSETR ('TEXT_BOX_X_LENGTH',   1.0)
          CALL PSETR ('TEXT_BOX_Y_LENGTH',   1.0)
          CALL PTEXT


C         Draw a text box showing the arrow head ratio

          TEXT_X = 1.22 + ((NRATIO-1) * 1.32)
          WRITE(UNIT=STRING, FMT='(F4.1)') RATIO
          CALL PSETC ('TEXT_LINE_1',         STRING)
          CALL PSETR ('TEXT_FONT_SIZE',      0.4)
          CALL PSETR ('TEXT_BOX_X_POSITION', TEXT_X)
          CALL PSETR ('TEXT_BOX_Y_POSITION', 6.0)
          CALL PSETR ('TEXT_BOX_X_LENGTH',   1.0)
          CALL PSETR ('TEXT_BOX_Y_LENGTH',   1.0)
          CALL PTEXT

      RETURN
      END


