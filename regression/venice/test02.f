      PROGRAM TSERIES

C ---------------------------------------------------------------------
C
C**** *PLTIMES* - PLOT TIME SERIES OF MODEL AND BUOY DATA.
C
C     LIANA ZAMBRESKY  GKSS/ECMWF   FEBRUARY 1988
C     HEINZ GUNTHER    GKSS/ECMWF   AUGUST   1991 MODIFIED FOR MAGICS.
C     LUIGI CAVALERI   ISDGM        MARCH    1992 MODIFIED FOR INCREASED
C                                            PLOTTING AND IMPROVED TIME AXIS
C
C     PURPOSE
C     -------
C
C         PLOT A MONTHLY TIME SERIES AT SPECTRA OUTPUT POINTS,
C         WHICH ARE GIVEN BY THE USER IN THE INPUT LIST.
C         THE FOLLOWING TIME SERIES ARE PLOTTED..
C
C         1.  MODEL AND BUOY WAVE HEIGHT
C         2.  MODEL AND BUOY WIND SPEED
C         3.  MODEL AND BUOY WIND DIRECTION
C         4.  MODEL MEAN PERIOD
C         5.  MODEL MEAN DIRECTION.
C
C     INTERFACE
C     ---------
C
C       IU05  USER INPUT.
C       IU06  PRINTER OUTPUT.
C       MODEL DATA ARE FETCHED DYNAMICALLY.
C
C     LIBRARIES.
C     ----------
C
C       ECLIB
C       MAGICS
C       WAMCRLIB
C
C     METHOD
C     ------
C
C       A PLOT IS PRODUCED WITH TIME SPAN LIMITED TO THE PERIOD OF THE RUN
C
C     EXTERNALS
C     ---------
C
C     REFERENCES
C     ----------
C
C ----------------------------------------------------------------------
C
C     *PARAMETER* FOR ARRAY DIMENSIONS.
C
      PARAMETER (IDD = 24, IDF = 25, IDS=5, IDT=260)
C
C        *IDD* INTEGER  NUMBER OF DIRECTIONS IN WAVE MODEL SPECTRUM.
C        *IDF* INTEGER  NUMBER OF FREQUENCIES IN WAVE MODEL SPECTRUM.
C        *IDS* INTEGER  NUMBER OF SITES TO BE EVALUATED.
C        *IDT* INTEGER  LENGTH OF TIME SERIES.
C
C ----------------------------------------------------------------------
C
C     YMODEL(,,) = MODEL ANALYSIS
C     YFOR1(,,)  =  "    1-D FORECAST
C     YFOR2(,,)  =  "    2-D    "
C     YFOR3(,,)  =  "    3-D    "
C     YMEAS(,,)  = MEASURED DATA
C
C ----------------------------------------------------------------------

      CHARACTER MONTH(12)*9, NAME(IDS)*20
      CHARACTER USERID*3, FILEID*3, PATH*60
      CHARACTER RUNID*3
      CHARACTER TXT1*40, TXT2*40, TXT3*40, TEXTX*40, TEXTY*40
      CHARACTER RUN*20
      CHARACTER*10 CDATEA, CDATEE, CDATEF, CDATM

      INTEGER DD,HH
      INTEGER DATEMIN, DATEMAX

      REAL MHT, MPER, MSPD, MDIR

      DIMENSION XTIME(IDT), IMON(12)
      DIMENSION YMODEL(IDT,5,IDS), XLONG(IDS), XLAT(IDS),
     1          SPEC(IDD,IDF), FR(IDF), THETA(IDD)
      REAL YFOR1(IDT,5,IDS), YFOR2(IDT,5,IDS), YFOR3(IDT,5,IDS)
      REAL YMEAS(IDT,5,IDS)
      DIMENSION KNTOBS(IDS), IREASON(5), WORK1(IDT), WORK2(IDT)
      LOGICAL IEOF
      CHARACTER*80 LINE

C
C ----------------------------------------------------------------------
C
      DATA MONTH/'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     *           'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     *           'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA IMON/31,28,31,30,31,30,31,31,30,31,30,31/
C
C ----------------------------------------------------------------------
      
C*    *COMMON* *TEXT* - FILE NAME INFORMATION.
      COMMON /TEXT/ USERID, RUNID, PATH
      
      COMMON /IMO/IMON
C
C ----------------------------------------------------------------------
C
C*    1. INITIALIZATION.
C        ---------------
C
 1000 CONTINUE
C
C     1.1 UNITS.
C         ------
C
      IU05 = 10

      open (unit = IU05, file = "input_test02")
      open (unit = 30, file = "WAS_RES.2010")
      open (unit = 21, file = "TOWER.ope_2010")
      IU06 = 6
C
C     1.2 READ USER INPUT.
C         ----------------
C
      CALL USEIN (IU05, IU06, IDS, XLONG, XLAT, NAME, NS,
     1           CDATEA, CDATEE, IDELDO,
     2           USERID, RUNID, FILEID, PATH, CDATEF, IDELFI,RUN)
C
C     LIMIT DATES OF THE PLOT AND ITS DURATION ARE EVALUATED IN SUB PLOTDATE
      CALL PLOTDATE (CDATEA,CDATEE,DATEMIN,DATEMAX,DURATION)
C
C     1.3 LOAD TABLE TO CONVERT U* TO U10.
C         --------------------------------
C
CCCCC      CALL LODTAB                 !     NOT USED
C
C     1.5 FLAG MODEL AND BUOY AND MODEL ARRAYS AS EMPTY.
C         ----------------------------------------------
C
      DO 1501 K=1,IDS
         KNTOBS(K) = 0
         DO 1502 J=1,5
         DO 1503 I=1,IDT
            YMODEL(I,J,K) = -999.
            YFOR1(I,J,K) = -999.
            YFOR2(I,J,K) = -999.
            YFOR3(I,J,K) = -999.
            YMEAS(I,J,K) = -999.
 1503 CONTINUE
 1502 CONTINUE
 1501 CONTINUE
C
C*   1.6 INITIALIZE HORIZONTAL AXIS TIME ARRAY
C        -------------------------------------
C
      ICHECK = IDELDO/3600
      DEL   = REAL(ICHECK)/24.
      XSTRT = 0.
C     NOTE - TIME INCREMENT IS EVALUATED IN FORMAT YYMMDDHH.0
C     NOTE - TIME IS EVALUATED UNDER THE ASSUMPTION THAT THE WHOLE SPAN IS
C            LESS THAN A MONTH - THIS IS BECAUSE WE ARE WORKING IN HOUR -
C            THIS CAN BE IMPROVED -
      DO 1601 I=1,IDT
         NHOURS=I*ICHECK
         XTIME(I)=NHOURS
 1601 CONTINUE
C
C*    1.7 INITIALIZE PLOTTING.
C         --------------------
C
      CALL POPEN
      CALL PSETC ('output_name', 'test02')
C     CALL PSETC ('WORKSTATION_1', 'PS')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 29.7)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 21.0)

      CALL PSETI ('SUBPAGE_FRAME_THICKNESS',5)

      CALL PSETR ('PAGE_X_LENGTH', 18.)
      CALL PSETR ('PAGE_Y_LENGTH', 27.5)

      CALL PSETC ('PAGE_ID_LINE', 'ON')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT', RUN)
      CALL PSETR ('PAGE_ID_LINE_HEIGHT', 0.2)
      CALL PSETC ('SUBPAGE_FRAME', 'OFF')
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'NONE')

      CALL PSETC ('TEXT_QUALITY', 'HIGH')
      CALL PSETI ('GRAPH_LINE_THICKNESS', 2)
      CALL PSETC ('AXIS_TICK_LABEL_QUALITY', 'HIGH')
      CALL PSETC ('LEGEND', 'ON')
      CALL PSETR ('LEGEND_TEXT_MAXIMUM_HEIGHT',0.25)
      CALL PSETC ('LEGEND_TEXT_COMPOSITION', 'USER_TEXT_ONLY')
      CALL PSETC ('LEGEND_BOX_MODE','POSITIONAL')
      CALL PSETR ('LEGEND_BOX_X_POSITION', 3.0)
      CALL PSETR ('LEGEND_BOX_Y_POSITION', 1.5)
      CALL PSETR ('LEGEND_BOX_X_LENGTH', 14.0)
      CALL PSETR ('LEGEND_BOX_Y_LENGTH', 0.5)

      READ(CDATEA,'(5I2.2)') IYY, IMM, IDA, IHO, IMI
      IYYY = IYY
      IF(IYYY.GE.70) IYY = IYY + 1900
      IF(IYYY.LT.70) IYY = IYY + 2000

      TXT2 = MONTH(IMM)
      IL =IECF_LEN(MONTH(IMM))
      WRITE(TXT2(IL+2:IL+5),'(I4.4)') IYY
C
C ----------------------------------------------------------------------
C
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     data are read in sub DATIN
      call DATIN(CDATEA,CDATEE,ymodel,yfor1,yfor2,yfor3,ymeas)

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --------------------------------------------------------------
C
C*    4. PLOT SUMMARY AT EACH BUOY LOCATION.
C        -----------------------------------
C
4000  CONTINUE

ccc   DO 4001 IB=1,NS
      DO 4001 IB=1,1

         ILENN = IECF_LEN(NAME(IB))

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     this part is to write the data on file - not to be used 
c     when plotting

      go to 678

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         IU76 = 76

         WRITE(IU76,*) '  '
         WRITE(IU76,*) ' SIDE: ',NAME(IB)(1:ILENN)
cc       WRITE(IU06,'(3X,''DATE'',9X,''WIND'',16x,''WAVE''/
cc   .                '' YYMMDDHH'',4X,''SPEED'',4X,''DIR'',
cc   .                3X,''HEIGHT'',4X,''DIR'',3X,''PERIOD''/)')
         WRITE(IU76,'(3X,''DATE'',9X,''WAVE'',16x,''WAVE''/
     .                '' YYMMDDHH'',4X,''HEIGHT'',4X,''HT '',
     .                3X,''HEIGHT'',4X,''DIR'',3X,''PERIOD''/)')
      DO 4002 IX=1,260
ccc      WRITE(IU06,44) YMODEL(IX,2,IB), YMODEL(IX,3,IB),
ccc  .                 YMODEL(IX,1,IB), YMODEL(IX,5,IB), YMODEL(IX,4,IB)
         WRITE(IU76,44) IX, YMODEL(IX,1,IB), YFOR1(IX,1,IB),
     .                 YFOR2(IX,1,IB), YFOR3(IX,1,IB), YMEAS(IX,1,IB)
 44      FORMAT(I5,10X,F8.2,F8.2,F8.2,F8.2,F8.2)
 4002 CONTINUE

C
C*    4.1 IF THERE ARE FEWER THAN 10 ENTRIES, DO NOT MAKE A PLOT.
C         -------------------------------------------------------
C
C         IF (KNTOBS(IB).LE.10) THEN
C            WRITE(IU06,*) ' FEWER THAN 10 ENTRIES '
C            WRITE(IU06,*) ' NO PLOT WILL BE MADE'
C            WRITE(IU06,*) '  '
C            GOTO 4001
C         ENDIF
C
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      STOP 1234

 678  continue

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C =============================================================================

C
C*    4.2 PLOT PAGE HEADER.
C         -----------------
C
         CALL PSETC ('TEXT_MODE', 'POSITIONAL')
         CALL PSETR ('TEXT_BOX_X_POSITION', 3.0)
         CALL PSETR ('TEXT_BOX_Y_POSITION', 26.0)
         CALL PSETR ('TEXT_BOX_X_LENGTH', 14.0)
         CALL PSETR ('TEXT_BOX_Y_LENGTH', 1.5)
         CALL PSETI ('TEXT_LINE_COUNT', 2)

         IF (XLAT(IB).LT.0.) THEN
            WRITE(TXT3,'('' ('',F4.1,''S,'')') -XLAT(IB)
         ELSE
            WRITE(TXT3,'('' ('',F4.1,''N,'')') XLAT(IB)
         ENDIF
         IF (XLONG(IB).GT.180.) THEN
            WRITE(TXT3(9:15),'(F5.1,''W)'')') 360.-XLONG(IB)
         ELSE
            WRITE(TXT3(9:15),'(F5.1,''E)'')') XLONG(IB)
         ENDIF
         TXT1 = NAME(IB)(1:ILENN)//TXT3(1:15)

         CALL PSETC ('TEXT_LINE_1', TXT1)
         CALL PSETC ('TEXT_LINE_2', TXT2)
         CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.35)
         CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1', 1.0)
         CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_2', 1.0)
         CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
         CALL PTEXT
C
C ---------------------------------------------------------------------------
C
C        TIME SCALE OF THE PLOT IS ASSIGNED
C
         XMIN=DATEMIN
         XMAX=DURATION
         XUNIT=1.

C ==========================================================================
C
C*    4.3 PLOT WAVE HEIGHTS.
C         ------------------
C
         CALL PNEW  ('SUBPAGE')
         CALL PSETR ('SUBPAGE_X_POSITION', 3.)
         CALL PSETR ('SUBPAGE_Y_POSITION', 21.5)
         CALL PSETR ('SUBPAGE_X_LENGTH', 14.)
         CALL PSETR ('SUBPAGE_Y_LENGTH', 4.)
         CALL PSETC ('SUBPAGE_FRAME', 'ON')
         CALL PSETC ('LEGEND_ENTRY', 'ON')
         CALL PSETC ('LEGEND_USER_TEXT', 'AN')
C
C     DRAW AXIS.
C
         YMIN  = 0.0
         YMAX = 0.
         DO 4301 I=1,IDT
            YMAX = MAX(YMAX, YMODEL(I,1,IB))
            YMAX = MAX(YMAX, YFOR1(I,1,IB))
            YMAX = MAX(YMAX, YFOR2(I,1,IB))
            YMAX = MAX(YMAX, YFOR3(I,1,IB))
            YMAX = MAX(YMAX, YMEAS(I,1,IB))
 4301    CONTINUE
            IF (YMAX.GT. 5.) THEN
            YMAX= 6.
            YUNIT = 2.
            ELSE
               IF (YMAX.GT.3.) THEN
               YMAX= 5.
               YUNIT=1.
               ELSE
               YMAX=3.
               YUNIT=1.
               END IF
            END IF
         TEXTX = ' '
         TEXTY = 'SIG WAVEHT (M)'

         CALL PSETR ('AXIS_MONTHS_LABEL_HEIGHT',0.25)
         CALL PSETC ('AXIS_YEARS_LABEL','OFF')
         CALL PSETC ('AXIS_MONTHS_LABEL','OFF')
         CALL PSETC ('AXIS_DAYS_LABEL','OFF')

         CALL AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT, TEXTX, TEXTY)
C
C     DRAW MODEL TIME SERIES.
C
         ITHICK=2

c     analysis
         ISYM=18
         CALL PSERIE (XTIME, YMODEL(1,1,IB), IDT, IDT,
     1                'SOLID', 'BLACK', 18, ITHICK)
C
c     1-d forecast
         ISYM=5
         CALL PSETC ('LEGEND_USER_TEXT', 'F1')
         CALL PSERIE (XTIME, YFOR1(1,1,IB), IDT, IDT,
     1                'SOLID', 'GREEN', 5, ITHICK)
C
c     2-d forecast
         ISYM=4
         CALL PSETC ('LEGEND_USER_TEXT', 'F2')
         CALL PSERIE (XTIME, YFOR2(1,1,IB), IDT, IDT,
     1                'SOLID', 'BLUE', 4, ITHICK)
C
c     3-d forecast
         ISYM=1
         CALL PSETC ('LEGEND_USER_TEXT', 'F3')
         CALL PSERIE (XTIME, YFOR3(1,1,IB), IDT, IDT,
     1                'SOLID', 'RED', 1, ITHICK)
C
c     measures
         ISYM=15
         CALL PSETC ('LEGEND_USER_TEXT', 'MEAS')
         CALL PSERIE (XTIME, YMEAS(1,1,IB), IDT, IDT,
     1                'DASH', 'MAGENTA', 15, ITHICK)
C
         CALL PSETC ('LEGEND_ENTRY','OFF')

C ==============================================================================
C
C*    4.4 PLOT WINDSPEED.
C         ---------------
C
         CALL PNEW  ('SUBPAGE')
         CALL PSETR ('SUBPAGE_X_POSITION', 3.)
         CALL PSETR ('SUBPAGE_Y_POSITION', 8.0)
         CALL PSETR ('SUBPAGE_X_LENGTH', 14.)
         CALL PSETR ('SUBPAGE_Y_LENGTH', 4.)
         CALL PSETC ('SUBPAGE_FRAME','ON')
         CALL PSETC ('LEGEND','OFF')
C
C     DRAW AXIS.
C
         YMIN  = 0.0
         DO 4401 I=1,IDT
            YMAX = MAX(YMAX, YMODEL(I,2,IB))
            YMAX = MAX(YMAX, YFOR1(I,2,IB))
            YMAX = MAX(YMAX, YFOR2(I,2,IB))
            YMAX = MAX(YMAX, YFOR3(I,2,IB))
 4401    CONTINUE
         YMAX = REAL(INT(YMAX/10.)+1)*10.
         YUNIT = 5.0
         TEXTX = ' '
         TEXTY = 'WIND SPEED (M/S)'
         CALL AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT, TEXTX, TEXTY)
C
C     DRAW MODEL TIME SERIES.
C
c     analysis
         isym=18
         CALL PSERIE (XTIME, YMODEL(1,2,IB), IDT, IDT,
     1                'SOLID', 'BLACK', 18, ITHICK)
C
c     1-d forecast
         isym=5
         CALL PSERIE (XTIME, YFOR1(1,2,IB), IDT, IDT,
     1                'SOLID', 'GREEN', 5, ITHICK)
C
c     2-d forecast
         isym=4
         CALL PSERIE (XTIME, YFOR2(1,2,IB), IDT, IDT,
     1                'SOLID', 'BLUE', 4, ITHICK)
C
c     3-d forecast
         isym=1
         CALL PSERIE (XTIME, YFOR3(1,2,IB), IDT, IDT,
     1                'SOLID', 'RED', 1, ITHICK)
C
C ==============================================================================
C
C     4.5 PLOT WIND DIRECTION.
C         --------------------
C
         CALL PNEW  ('SUBPAGE')
         CALL PSETR ('SUBPAGE_X_POSITION', 3.)
         CALL PSETR ('SUBPAGE_Y_POSITION', 3.5)
         CALL PSETR ('SUBPAGE_X_LENGTH', 14.)
         CALL PSETR ('SUBPAGE_Y_LENGTH', 4.)
         CALL PSETC ('SUBPAGE_FRAME', 'ON')

C      LABEL IS PLOTTED ONLY AT THIS BOTTOM DIAGRAM
         CALL PSETC ('AXIS_YEARS_LABEL','ON')
         CALL PSETC ('AXIS_MONTHS_LABEL','ON')
         CALL PSETC ('AXIS_DAYS_LABEL','ON')
C
C     DRAW AXIS.
C
         YMIN  = 0.0
         YMAX  = 360.
         YUNIT =  45.
         TEXTX = ' '
         TEXTY = 'WIND DIR (DEG)'
         CALL AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT, TEXTX, TEXTY)
C
         CALL PSETC ('AXIS_YEARS_LABEL','OFF')
         CALL PSETC ('AXIS_MONTHS_LABEL','OFF')
         CALL PSETC ('AXIS_DAYS_LABEL','OFF')

C     DRAW MODEL TIME SERIES.
C
C     analysis
         isym=18
         CALL PSERIED (XTIME, YMODEL(1,3,IB), IDT, IDT,
     1                'SOLID', 'BLACK', isym, ITHICK)
C
C     1-d forecast
         isym=5
         CALL PSERIED (XTIME, YFOR1(1,3,IB), IDT, IDT,
     1                'SOLID', 'GREEN', isym, ITHICK)
C
C     2-d forecast
         isym=4
         CALL PSERIED (XTIME, YFOR2(1,3,IB), IDT, IDT,
     1                'SOLID', 'BLUE', isym, ITHICK)
C
C     3-d forecast
         isym=1
         CALL PSERIED (XTIME, YFOR3(1,3,IB), IDT, IDT,
     1                'SOLID', 'RED', isym, ITHICK)
C
C        CALL PSETC ('LEGEND_ENTRY','OFF')

C ============================================================================
C
C     4.6 WAVE PERIOD.
C         -------------
C
         CALL PNEW  ('SUBPAGE')
         CALL PSETR ('SUBPAGE_X_POSITION', 3.)
         CALL PSETR ('SUBPAGE_Y_POSITION', 17.0)
         CALL PSETR ('SUBPAGE_X_LENGTH', 14.)
         CALL PSETR ('SUBPAGE_Y_LENGTH', 4.0)
         CALL PSETC ('SUBPAGE_FRAME', 'ON')
C
C     DRAW AXIS.
C
         YMIN  = 0.0
         YMAX = 0.
         DO 4601 I=1,IDT
            YMAX = MAX(YMAX, YMODEL(I,4,IB))
            YMAX = MAX(YMAX, YFOR1(I,4,IB))
            YMAX = MAX(YMAX, YFOR2(I,4,IB))
            YMAX = MAX(YMAX, YFOR3(I,4,IB))
            YMAX = MAX(YMAX, YMEAS(I,4,IB))
 4601    CONTINUE
            IF (YMAX.GT.12.) THEN
            YMAX=18.
            YUNIT = 3.
            ELSE
               IF (YMAX.GT.6.) THEN
               YMAX=12.
               YUNIT=2.
               ELSE
               YMAX=6.
               YUNIT=1.
               END IF
            END IF
         TEXTX = ' '
         TEXTY = 'MEAN PERIOD (S)'
         CALL AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT, TEXTX, TEXTY)
C
C     DRAW MODEL TIME SERIES.
C
c     analysis
         CALL PSERIE (XTIME, YMODEL(1,4,IB), IDT, IDT,
     1                'SOLID', 'BLACK', 18, ITHICK)
C
c     1-d forecast
         CALL PSERIE (XTIME, YFOR1(1,4,IB), IDT, IDT,
     1                'SOLID', 'GREEN', 5, ITHICK)
C
c     2-d forecast
         CALL PSERIE (XTIME, YFOR2(1,4,IB), IDT, IDT,
     1                'SOLID', 'BLUE', 4, ITHICK)
C
c     3-d forecast
         CALL PSERIE (XTIME, YFOR3(1,4,IB), IDT, IDT,
     1                'SOLID', 'RED', 1, ITHICK)
C
c     measured data
         CALL PSERIE (XTIME, YMEAS(1,4,IB), IDT, IDT,
     1                'DASH', 'MAGENTA', 15, ITHICK)
C
C ==============================================================================
C
C     4.5 PLOT WAVE DIRECTION.
C         --------------------
C
         CALL PNEW  ('SUBPAGE')
         CALL PSETR ('SUBPAGE_X_POSITION', 3.)
         CALL PSETR ('SUBPAGE_Y_POSITION', 12.5)
         CALL PSETR ('SUBPAGE_X_LENGTH', 14.)
         CALL PSETR ('SUBPAGE_Y_LENGTH', 4.)
         CALL PSETC ('SUBPAGE_FRAME', 'ON')
C
C     DRAW AXIS.
C
         YMIN  = 0.0
         YMAX  = 360.
         YUNIT =  45.
         TEXTX = ' '
         TEXTY = 'WAVE DIR (DEG)'
         CALL AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT, TEXTX, TEXTY)
C
C     DRAW MODEL TIME SERIES.
C
c     analysis
         CALL PSERIED (XTIME, YMODEL(1,5,IB), IDT, IDT,
     1                'SOLID', 'BLACK', 18, ITHICK)
C
c     1-d forecast
         CALL PSERIED (XTIME, YFOR1(1,5,IB), IDT, IDT,
     1                'SOLID', 'GREEN', 5, ITHICK)
C
c     2-d forecast
         CALL PSERIED (XTIME, YFOR2(1,5,IB), IDT, IDT,
     1                'SOLID', 'BLUE', 4, ITHICK)
C
c     3-d forecast
         CALL PSERIED (XTIME, YFOR3(1,5,IB), IDT, IDT,
     1                'SOLID', 'RED', 1, ITHICK)
c     measured data
         CALL PSERIED (XTIME, YMEAS(1,5,IB), IDT, IDT,
     1                'DASH', 'MAGENTA', 15, ITHICK)
C
C
C ==============================================================================
C
C*    4.7 END OF PHYSICAL PAGE.
C         ---------------------

      CALL PNEW ('PAGE')
C     CALL PNEW ('SUPER_PAGE')

 4001 CONTINUE

C     4.14   CLOSE GRAPHICS
C     ---------------------

      CALL PCLOSE

      STOP
      END

C ##############################################################################

      SUBROUTINE PSERIE (XVAL, YVAL, IDT, NDT, STYLE, COLOUR, ISYMB,
     *                   ITHICK)
      CHARACTER COLOUR*(*), STYLE*(*)
      DIMENSION XVAL(IDT), YVAL(IDT)

      CALL PSETC ('GRAPH_TYPE', 'CURVE')
      CALL PSETC ('GRAPH_LINE_COLOUR', COLOUR)
      CALL PSETC ('GRAPH_LINE_STYLE', STYLE)
      CALL PSETI ('GRAPH_LINE_THICKNESS', ITHICK)

      CALL PSETC ('GRAPH_SYMBOL', 'ON')
      CALL PSETI ('GRAPH_SYMBOL_MARKER_INDEX', ISYMB)
      CALL PSETR ('GRAPH_SYMBOL_HEIGHT', 0.05*ITHICK)
      CALL PSETC ('GRAPH_SYMBOL_COLOUR', COLOUR)

      CALL PSETc ('graph_curve_x_base_date', "20100306")
      CALL PSETc ('graph_missing_data_mode', "ignore")
      CALL PSETc ('graph_curve_x_base_date', "20100306")
      CALL PSETc ('graph_curve_x_date_offset', "hours")
      CALL PSET1R ('graph_curve_x_values', XVAL, NDT)
      CALL PSET1R ('graph_curve_y_values', YVAL, NDT)
      CALL PSETR ('graph_y_missing_value', -999.)
      CALL PGRAPH

      RETURN
      END

C###############################################################################

      SUBROUTINE PSERIED (XVAL, YVAL, IDT, NDT, STYLE, COLOUR, ISYMB,
     *                   ITHICK)
      CHARACTER COLOUR*(*), STYLE*(*)
      DIMENSION XVAL(IDT), YVAL(IDT)
      REAL X1(IDT*3),Y1(IDT*3),X2(IDT*3),Y2(IDT*3)

c --------------------------------------------------------------------

c     definition of variables

c     XVAL(), YVAL()   input time series
c     X1()  , Y1()     revised time series (including 0. and 360.  values)
c     X2()  , Y2()     sections of X1(), Y1() for plotting of single
c     section (between one 0.-360. crossing and another) -

c -----------------------------------------------------------------------

      CALL PSETC ('GRAPH_TYPE', 'CURVE')
      CALL PSETC ('GRAPH_LINE_COLOUR', COLOUR)
      CALL PSETC ('GRAPH_LINE_STYLE', STYLE)
      CALL PSETI ('GRAPH_LINE_THICKNESS', ITHICK)

      CALL PSETC ('GRAPH_SYMBOL', 'ON')
      CALL PSETI ('GRAPH_SYMBOL_MARKER_INDEX', ISYMB)
      CALL PSETR ('GRAPH_SYMBOL_HEIGHT', 0.05*ITHICK)
      CALL PSETC ('GRAPH_SYMBOL_COLOUR', COLOUR)

c     CALL PSET1R ('GRAPH_CURVE_X_values', XVAL, NDT)
c     CALL PSET1R ('GRAPH_CURVE_Y_VALUES', YVAL, NDT)

c -------------------------------------------------------------------
c     evaluation of X1(), Y1()

      x1(1)=xval(1)
      y1(1)=yval(1)
      ind=1

         do 100 i=2,ndt
         ya=yval(i-1)
         yb=yval(i)
            if(ya.ge.0. .and. yb.ge.0. .and. abs(ya-yb).gt.180.) then
            xa=xval(i-1)
            xb=xval(i)
c     case ya<yb
               if(ya.lt.180.) then
               c=ya+360.-yb
               ind=ind+1
c     for linear interpolation the x (time) coordinates are first changed from
c     DDHH to decimal hours and then back
               ia=xa
               xa=(ia/100)*24.+mod(ia,100)
               ib=xb
               xb=(ib/100)*24.+mod(ib,100)
               xab=xa+(ya/c)*(xb-xa)
               x1(ind)=xab-int(xab/24.)*24.+int(xab/24.)*100.
               y1(ind)=0.
               ind=ind+1
               x1(ind)=x1(ind-1)
               y1(ind)=360.
               end if
c     case ya>yb
               if(ya.gt.180.) then
               c=yb+360.-ya
               ind=ind+1
c     for linear interpolation the x (time) coordinates are first changed from
c     DDHH to decimal hours and then back
               ia=xa
               xa=(ia/100)*24.+mod(ia,100)
               ib=xb
               xb=(ib/100)*24.+mod(ib,100)
               xab=xa+((360.-ya)/c)*(xb-xa)
               x1(ind)=xab-int(xab/24.)*24.+int(xab/24.)*100.
               y1(ind)=360.
               ind=ind+1
               x1(ind)=x1(ind-1)
               y1(ind)=0.
               end if
            end if
         ind=ind+1
         x1(ind)=xval(i)
         y1(ind)=yval(i)
 100     continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc      do 1952 ijk=1,120
ccc      write(54,'(i10,2(f10.2,''    - ''))') ijk,x1(ijk),y1(ijk)
ccc 1952    continue
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     NP number of points in the extended series
      np=ind
c     plot is done in sections
      x2(1)=x1(1)
      y2(1)=y1(1)
      ind=1
         do 200 i=2,np
            if(abs(y1(i)-y1(i-1)).gt.180.) then
            call PSET1R('graph_curve_x_values',x2,ind)
            call PSET1R('graph_curve_y_values',y2,ind)
            call pgraph
            ind=0
            end if
         ind=ind+1
         x2(ind)=x1(i)
         y2(ind)=y1(i)
 200     continue
      call PSET1R('graph_curve_x_values',x2,ind)
      call PSET1R('graph_curve_y_values',y2,ind)
      call pgraph
      
      RETURN
      END

C###############################################################################

      SUBROUTINE AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT,
     1                 TEXTX, TEXTY)
      CHARACTER TEXTX*(*), TEXTY*(*)
      CHARACTER*20 DMAX 
      CHARACTER*20 DMIN
      
	  INTEGER IMIN,IMAX
	  IMIN = XMIN
	  IMAX = XMAX
      
      write(6,*) "MIN", IMIN
      write(6,*) "MAX", IMAX
      

C Date format ti create!
	  DMIN='20100306'
	  DMAX='20100312'

      

C     XMAX IS NOW THE TIME SPAN OF THE PLOT, WRITTEN AS YYMMDDHH.0 
      CALL PSETC ('AXIS_TYPE', 'DATE')
      CALL PSETC ('AXIS_ORIENTATION', 'HORIZONTAL') 
      CALL PSETC ('AXIS_TYPE', 'DATE')
      CALL PSETC ('AXIS_GRID', 'ON')
      CALL PSETC ('AXIS_GRID_LINE_STYLE','DOT')
      CALL PSETI ('AXIS_LINE_THICKNESS',2)
      CALL PSETC ('AXIS_GRID_COLOUR', 'BLACK')

      CALL PSETC ('AXIS_LINE', 'OFF')
      CALL PSETC ('AXIS_ORIENTATION', 'HORIZONTAL')
      CALL PSETC ('AXIS_POSITION','BOTTOM')
      CALL PSETC ('AXIS_DATE_MIN_VALUE', DMIN)
      CALL PSETC ('AXIS_DATE_MAX_VALUE', DMAX)
      

      CALL PSETC ('AXIS_TITLE', 'ON')
      CALL PSETC ('AXIS_TITLE_TEXT', TEXTX)
      CALL PSETR ('AXIS_TITLE_HEIGHT', 0.25)
      CALL PSETC ('AXIS_TICK', 'ON')
      CALL PSETC ('AXIS_TICK_POSITIONING','DAYS')
      CALL PSETC ('AXIS_DAYS_LABEL','NUMBER')
      CALL PSETR ('AXIS_TICK_INTERVAL', XUNIT)
      CALL PSETR ('AXIS_DAYS_LABEL_HEIGHT', 0.18)
      CALL PAXIS

      CALL PSETC ('AXIS_POSITION', 'LEFT')
      CALL PSETC ('AXIS_ORIENTATION', 'VERTICAL')
      CALL PSETC ('AXIS_TYPE', 'REGULAR')
      CALL PSETR ('AXIS_MIN_VALUE', YMIN)
      CALL PSETR ('AXIS_MAX_VALUE', YMAX)
      CALL PSETC ('AXIS_TICK_POSITIONING','REGULAR')

      CALL PSETC ('AXIS_TITLE', 'ON')
      CALL PSETC ('AXIS_TITLE_TEXT', TEXTY)
      CALL PSETR ('AXIS_TITLE_HEIGHT', 0.2)
      CALL PSETC ('AXIS_TICK', 'ON')
      CALL PSETR ('AXIS_TICK_INTERVAL', YUNIT)
      CALL PSETC ('AXIS_TICK_LABEL', 'ON')
      CALL PSETR ('AXIS_TICK_LABEL_HEIGHT', 0.18)
      CALL PAXIS

      RETURN
      END

C###############################################################################


      SUBROUTINE USEIN(IU05, IU06, IDS, XLONG, XLAT, NAME, NS,
     1                 CDATEA, CDATEE, IDELDO,
     2                 USERID, RUNID, FILEID, PATH, CDATEF, IDELFI,RUN)

C -----------------------------------------------------------------------
C
C**** *USEIN* - ROUTINE TO READ USER INPUT
C
C     H. GUNTHER     ECMWF     NOVEMBER 1989
C
C     PURPOSE.
C     --------
C
C        READ USER INPUT CONCERNING PERIOD OF INTEREST,TIMESTEPS AND
C        OPTIONS. A CONSISTENCY CHECK IS DONE TOO.
C
C**   INTERFACE.
C     ----------
C
C      *CALL* *USEIN*
C         *IU05  *  INTEGER    INPUT UNIT.
C         *IU06  *  INTEGER    PRINTER OUTPUT UNIT.
C         *IDS   *  INTEGER    DIMENSION OF ARRAYS
C         *XLONG *  REAL       LONGITUDES OF OUTPUT POINTS (DEGREE)
C         *XLAT  *  REAL       LATITUDES OF OUTPUT POINTS (DEGREE)
C         *NAME  *  CHARACTER  NAMES OF OUTPUT POINTS
C         *NS    *  INTEGER    NUMBER OF OUTPUT POINTS
C         *CDATEA*  CHARACTER  START DATE OF OUTPUT (YYMMDDHHMM)
C         *CDATEE*  CHARACTER  END DATE OF OUTPUT (YYMMDDHHMM)
C         *IDELDO*  INTEGER    OUTPUT TIME INCREMENT (SECONDS)
C         *USERID*  CHARACTER  USERID OF INPUT FILES
C         *FILEID*  CHARACTER  FILE ID OF INPUT FILES
C         *PATH  *  CHARACTER  DIRECTORY OF INPUT FILES
C         *CDATEF*  CHARACTER  FIRST INPUT FILE DATE (YYMMDDHHMM)
C         *IDELFI*  INTEGER    INPUT FILE DATE INCREMENT (SECONDS)
C         *RUN*     CHARACTER  INFORMATION ON THE ACTUAL RUN
C
C     METHOD.
C     -------
C
C        USER INFORMATION IS BEING READ WITH THE PRESUMPTIONS THAT:
C         1. EVERY LINE STARTING WITH 'C' IS A COMMENT LINE
C         2. VALUES ARE PUT IN BELOW POSITIONS INDICATED WITH '-'
C            (RIGHT-JUSTIFIED)
C
C     EXTERNALS.
C     ----------
C
C        ABORT
C
C     REFERENCE.
C     ----------
C
C        *NONE*
C
C ------------------------------------------------------------------
C
      CHARACTER USERID*3, RUNID*3, FILEID*3, NAME(IDS)*20, PATH*60
      DIMENSION XLONG(IDS), XLAT(IDS)

      LOGICAL LERROR
      CHARACTER LINE*72
      CHARACTER RUN*20
      CHARACTER*10 CDATEA, CDATEE, CDATEF
C
C --------------------------------------------------------------------
C
C*    1. READ USER INPUT
C        ---------------
C
      NS = 0
      ICOUNT = 0
 1000 CONTINUE
      READ(IU05, '(A72)',ERR=4000,END=4000,IOSTAT=IOS) LINE
      IF (LINE(1:1).EQ.'C') GOTO 1000
      ICOUNT = ICOUNT + 1
      IF (ICOUNT.EQ.1) THEN
         READ(LINE( 2:11),'(A10)',ERR=4100,IOSTAT=IOS) CDATEA
         READ(LINE(14:23),'(A10)',ERR=4100,IOSTAT=IOS) CDATEE
         READ(LINE(26:32),'(I7 )',ERR=4100,IOSTAT=IOS) IDELDO
         IF (LINE(35:35).EQ.'H') IDELDO = IDELDO*3600
         GOTO 1000
      ELSE IF (ICOUNT.EQ.2) THEN
         READ(LINE( 2:11),'(A10)',ERR=4100,IOSTAT=IOS) CDATEF
         READ(LINE(14:20),'(I7 )',ERR=4100,IOSTAT=IOS) IDELFI
         IF (LINE(23:23).EQ.'H') IDELFI = IDELFI*3600
         USERID = LINE(26:28)
         GOTO 1000
      ELSE IF (ICOUNT.EQ.3) THEN
         RUNID  = LINE( 2: 4)
         FILEID = LINE( 7: 9)
         PATH   = LINE(12:71)
         GOTO 1000
      ELSE IF (ICOUNT.EQ.4) THEN
         READ(LINE( 2:21),'(A20)',ERR=4100,IOSTAT=IOS) RUN
         GOTO 1000
      ELSE IF(LINE(22:24).NE.'END') THEN
         IF (NS.EQ.IDS) THEN
            WRITE(IU06,*) '++++++++++++++++++++++++++++++++++++++++++++'
            WRITE(IU06,*) '+                                          +'
            WRITE(IU06,*) '+      WARNING ERROR IN SUB. USEIN         +'
            WRITE(IU06,*) '+      ===========================         +'
            WRITE(IU06,*) '+ NUMBER OF OUTPUT SIDES EXCEEDS DIMENSION +'
            WRITE(IU06,*) '+ THE FIRST ', NS, 'SIDES ARE EXECUTED'
            WRITE(IU06,*) '+                                          +'
            WRITE(IU06,*) '+ CHECK INPUT OR CHANGE DIMENSION IN MAIN  +'
            WRITE(IU06,*) '+ PROGRAM                                  +'
            WRITE(IU06,*) '++++++++++++++++++++++++++++++++++++++++++++'
            GOTO 2000
         ENDIF
         NS = NS + 1
         READ(LINE( 2: 9),'(F8.0)',ERR=4100,IOSTAT=IOS) XLONG(NS)
         READ(LINE(12:19),'(F8.0)',ERR=4100,IOSTAT=IOS) XLAT(NS)
         NAME(NS) = LINE(22:41)
         GOTO 1000
      ELSE IF (LINE(22:24).EQ.'END') THEN
         GOTO 2000
      ENDIF
C
C     2. PRINT USER INPUT
C        ----------------
 2000 CONTINUE
      WRITE(IU06,'(1H1)')
      WRITE(IU06,*) ' USER INPUT:'
      WRITE(IU06,*) '  '
      WRITE(IU06,*) ' START  DATE (FORMAT:YYMMDDHHMM) : ',CDATEA,
     *           ' END DATE :',CDATEE
      WRITE(IU06,*) '  '
      WRITE(IU06,*) ' OUTPUT EVERY ',IDELDO ,' SECONDS'
      WRITE(IU06,*) '  '
      WRITE(IU06,*) '  '
      WRITE(IU06,*) ' DATA WILL BE FETCHED FROM ECFILE:'
      WRITE(IU06,*) ' USER ID IS ..................... ', USERID
      WRITE(IU06,*) ' FILE ID IS ..................... ', FILEID
      WRITE(IU06,*) ' DIRECTORY NAME IS .............. ', PATH
      WRITE(IU06,*) ' THE FIRST FILE DATE IS ......... ', CDATEF
      WRITE(IU06,*) ' A NEW FILE WILL BE FETCHED EVERY ', IDELFI,
     1              ' SECONDS'
      WRITE(IU06,*) '  '
      WRITE(IU06,*) ' LIST OF OUTPUT SIDE TO BE PROCESSED:'
      WRITE(IU06,*) '  '
      WRITE(IU06,*) ' TOTAL NUMBER OF SIDES IS.........', NS
      WRITE(IU06,*) '  '
      WRITE(IU06,'(''  LONGITUDE | LATITUDE |'',
     1             ''       SIDE NAME      |'')')
      WRITE(IU06,'('' -----------|----------|'',
     1             ''----------------------|'')')
      DO 2100 I=1,NS
         WRITE(IU06,'(1X,F10.3,'' | '',F8.3,'' | '',A20,'' |'')')
     1            XLONG(I),XLAT(I),NAME(I)
 2100 CONTINUE
C
C*    3. CHECK CONSISTENCY OF INPUT DATA
C        -------------------------------
C
      IF (CDATEE.LT.CDATEA) THEN
         WRITE(IU06,*) '*******************************************'
         WRITE(IU06,*) '*                                         *'
         WRITE(IU06,*) '*    FATAL ERROR IN SUB. USEIN            *'
         WRITE(IU06,*) '*    =========================            *'
         WRITE(IU06,*) '* END DATE IS BEFORE START DATE           *'
         WRITE(IU06,*) '* START DATE = ', CDATEA
         WRITE(IU06,*) '* END  DATE  = ', CDATEE
         WRITE(IU06,*) '*                                         *'
         WRITE(IU06,*) '* CORRECT USER INPUT                      *'
         WRITE(IU06,*) '*                                         *'
         WRITE(IU06,*) '* PROGRAM ABORTS.   PROGRAM ABORTS.       *'
         WRITE(IU06,*) '* ---------------   --------------        *'
         WRITE(IU06,*) '*******************************************'
         CALL ABORT
      ENDIF

      RETURN
C
C*    4. READ ERROR MESSAGES.
C        --------------------
C
 4000 CONTINUE
         WRITE(IU06,*) ' ********************************************'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *     FATAL ERROR IN SUB. USEIN            *'
         WRITE(IU06,*) ' *     =========================            *'
         WRITE(IU06,*) ' * READ ERROR ON INPUT FILE:                *'
         WRITE(IU06,*) ' * ERROR IS LATER THAN ICOUNT = ', ICOUNT
         WRITE(IU06,*) ' * LAST LINE READ IS     LINE = ', LINE
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *   PROGRAM ABORTS  PROGRAM ABORTS         *'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' ********************************************'
         CALL ABORT
 4100 CONTINUE
         WRITE(IU06,*) ' ********************************************'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *     FATAL ERROR IN SUB. USEIN            *'
         WRITE(IU06,*) ' *     =========================            *'
         WRITE(IU06,*) ' * READ ERROR ON CHARACTER STRING           *'
         WRITE(IU06,*) ' * ERROR IS IN DATA LINE ICOUNT = ', ICOUNT
         WRITE(IU06,*) ' * CHARACTER STRING IS   LINE = ', LINE
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *   PROGRAM ABORTS  PROGRAM ABORTS         *'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' ********************************************'
         CALL ABORT

      END

C###############################################################################

      SUBROUTINE PLOTDATE (C1,C2,X1,X2,DX)

C     SUB PLOTDATE RECEIVES TWO DATES, I1,I2, WRITTEN AS YYMMDDHHMM,
C     EVALUATES THE TWO EXTREME DATES X1,X2 OF PLOT WRITTEN AS YYMMDD00.0,
C     AND THE DURATION OF THE PLOT DX WRITTEN AS YYMMDDHH.0 -

C     IMON(12) ARE THE NUMBER OF DAYS IN EACH MONTH OF THE YEAR -

C -----------------------------------------------------------------------------

      CHARACTER*10 C1,C2
      INTEGER IMON(12)
      INTEGER Y1,M1,D1,H1, Y2,M2,D2,H2
      INTEGER X1, X2

      COMMON /IMO/IMON

C -----------------------------------------------------------------------------

C     EVALUATION OF X1 -

      READ(C1,'(5I2.2)') Y1,M1,D1,H1,IM1
      D1=D1-1

C     CHECK ON THE EVENTUAL CHANGE OF MONTH AND YEAR
         IF(D1.EQ.0) THEN
         M1=M1-1
            IF(M1.EQ.0) THEN
            Y1=Y1-1
            M1=12
            D1=31
               ELSE
            D1=IMON(M1)
            END IF
         END IF
      X1=Y1*10**6+M1*10**4+D1*10**2

C     EVALUATION OF X2 -

      READ(C2,'(5I2.2)') Y2,M2,D2,H2,IM2
      D2=D2+1

C     CHECK ON THE EVENTUAL CHANGE OF MONTH AND YEAR
         IF(D2.GT.IMON(M2)) THEN
         D2=1
         M2=M2+1
            IF(M2.GT.12) THEN
            M2=1
            Y2=Y2+1
            END IF
         END IF
      X2=Y2*10**6+M2*10**4+D2*10**2

C     DX IS EVALUATED
      DX=0.
 100  IF(Y1.EQ.Y2.AND.M1.EQ.M2.AND.D1.EQ.D2) RETURN
      CALL INCDATE1(Y1,M1,D1)
      DX=DX+100.
      GO TO 100

      END

C############################################################################

      SUBROUTINE INCDATE1(Y,M,D)

C     SUB INCDATE1 INCREMENT THE DATE OF ONE DAY

C --------------------------------------------------------------------------

C     Y = YEAR
C     M = MONTH
C     D = DAY
C     IMON() = NUMBER OF DAYS IN EACH MONTH

      INTEGER Y,M,D
      INTEGER IMON(12)

      COMMON /IMO/IMON

C --------------------------------------------------------------------------

      D=D+1
      IF(D.LE.IMON(M)) RETURN
C     MONTH NUMBER MUST BE INCREASED
      D=1
      M=M+1
      IF(M.LE.12) RETURN
      M=1
      Y=Y+1

      RETURN
      END

C###############################################################################

      SUBROUTINE INCDATE3 (Y2,M2,D2,H2,DATEMIN,IH,IX)

C     SUB INCDATE3 RECEIVES A DATE DEFINED BY Y2,M2,D2,H2, AND REFERENCE
C     DATE DATEMIN, WRITTEN AS YYMMDDHH.0, THE TIME STEP IH IN HOUR, AND
C     IT EVALUATES HOW MANY STEPS ARE REQUIRED TO GO FROM DATEMIN TO
C     Y2,M2,D2,H2 -

C-----------------------------------------------------------------------------

C     Y2,M2,D2,H2 = YEAR, MONTH, DAY, HOUR  OF THE DATE
C      Y, M, D, H = GENERAL YEAR, MONTH, DAY AND HOUR, INITIALLY DERIVED
C                   FROM DATEMIN
C      IMON()     = NUMBER OF DAYS IN EACH MONTH OF THE YEAR

      INTEGER Y,M,D,H
      INTEGER Y2,M2,D2,H2
      INTEGER IMON(12)
      INTEGER DATEMIN

      COMMON /IMO/IMON

C ---------------------------------------------------------------------------

C     Y,M,D,H ARE EVALUATED -
      ID=DATEMIN
      Y=ID/10**6
      ID=ID-Y*10**6
      M=ID/10**4
      ID=ID-M*10**4
      D=ID/10**2
      H=ID-D*10**2

C     CHECK ON THE NUMBER OF DAYS OF FEBRUARY
      IMON(2)=28
      IF(MOD(Y,4).EQ.0) IMON(2)=29

C     CHECK THAT DATEMIN=IY=(Y,M,D,H) COMES BEFORE DATE=(Y2,M2,D2,H2) -
      IF(Y.GT.Y2) GO TO 5000
      IF(Y.LT.Y2) GO TO 100
C     Y=Y2
      IF(M.GT.M2) GO TO 5000
      IF(M.LT.M2) GO TO 100
C     M=M2
      IF(D.GT.D2) GO TO 5000
      IF(D.LT.D2) GO TO 100
C     D=D2
      IF(H.GT.H2) GO TO 5000
      IF(H.EQ.H2) GO TO 4000

 100  CONTINUE
C     DATE DATEMIN=(Y,M,D,H) IS BEFORE (Y2,M2,D2,H2)
C     IX IS EVALUATED

      IX=0
 40   IX=IX+1
C     (Y,M,D,H) IS INCREMENTED OF IH HOURS -
      H=H+IH
         IF(H.GE.24) THEN
         H=H-24
         D=D+1
            IF(D.GT.IMON(M)) THEN
            D=1
            M=M+1
               IF(M.GT.12) THEN
               M=1
               Y=Y+1
               END IF
            END IF
         END IF
      IF(Y.EQ.Y2.AND.M.EQ.M2.AND.D.EQ.D2.AND.H.EQ.H2) RETURN
      GO TO 40

 4000 WRITE(6,4001)
      STOP
 4001 FORMAT(1X//////1X,'IDATM EQ DATEMIN'//////)

 5000 WRITE(6,5001)
      STOP
 5001 FORMAT(1X//////1X,'IDATM LT DATEMIN'//////)

      END

c ====================================================================

      subroutine DATIN(CDATEA,CDATEE,y0,y1,y2,y3,ym)

c     sub DATIN read in data for the time series plot

c -------------------------------------------------------------------

c     variables definition

C     CDATEA       INITIAL DATE OF PLOT
C     CDATEE       END DATE OF PLOT
c     Y0(,,)     = analysis data
c     Y1(,,)     = 1-d forecast data
c     Y2(,,)     = 2-d forecast data
c     Y3(,,)     = 3-d forecast data
c     YM(,,)     = measured data

c     idt        = max number of time steps
c     ids        = max number of stations

c     number of parameters to be plotted = 5
c     1  = wave height
c     2  = wind speed
c     3  = wind direction
c     4  = wave period
c     5  = wave direction

c -------------------------------------------------------------------

      parameter (ids=26, idt=260)
 
      real y0(idt,5,ids),y1(idt,5,ids),y2(idt,5,ids),y3(idt,5,ids)
      real ym(idt,5,ids)
      character*1 a
      character*10 CDATEA, CDATEE

      nt = 48
      it = 7
      nstaz = 1
      READ(CDATEA,'(I8,I2)') idatini, III
      READ(CDATEE,'(I8,I2)') idatfin, III

c -------------------------------------------------------------------

c     model data

      do 250 istat=1,nstaz

      iunit = 20+istat

C     SKIP HEADING LINES
         do 251 i=1,7
         read(iunit,'(a1)')a
 251     continue

 252     CONTINUE
         IT = IT+1

         READ(IUNIT,2001,end=3000) IDATE,
     A       y0(it,1,istat),y0(it,4,istat),y0(it,5,istat),
     1       y1(it,1,istat),y1(it,4,istat),y1(it,5,istat),
     2       y2(it,1,istat),y2(it,4,istat),y2(it,5,istat),
     3       y3(it,1,istat),y3(it,4,istat),y3(it,5,istat)

         y0(it,4,istat) = 1./y0(it,4,istat)
         y1(it,4,istat) = 1./y1(it,4,istat)
         y2(it,4,istat) = 1./y2(it,4,istat)
         y3(it,4,istat) = 1./y3(it,4,istat)


c        A T T E N Z I O N E  ! ! ! ! !
C        PER DISEGNARE SOLO    A N A L I S I
c        y1(it,1,istat) = -999.0
c        y2(it,1,istat) = -999.0
c        y3(it,1,istat) = -999.0
c        y1(it,4,istat) = -999.0
c        y2(it,4,istat) = -999.0
c        y3(it,4,istat) = -999.0
c        y1(it,5,istat) = -999.0
c        y2(it,5,istat) = -999.0
c        y3(it,5,istat) = -999.0

         if(IDATE.lt.idatini)  it = it-1
         if(IDATE.eq.idatfin)  go to 250
         GO TO 252

 250     continue

c     measured data

 3000 CONTINUE

C     AT TOWER LOCATION

C     SKIP HEADING LINES
ccc   do 301 i=1,20
      do 301 i=1,30
      read(30,'(a1)')a
 301  continue

      itt=7

      do 350 istat=1,nstaz

 302  CONTINUE
      itt = itt + 1

ccc   READ(30,1301)IYM,IMM,IDM,IHM,MIN,IND,HS,FP,FM,DIRM,AM
      READ(30,3001)IND, IYM, IMM, IDM, IHM,
     .             WAS1, WAS2, WAS3, WAS4, WAS5, WAS6
ccc    WAS1 = HS
ccc    IF(FM.GT.0.) WAS3 = 1./FM
ccc    WAS4 = DIRM

       IYYM = IYM
       IF(IYYM.LE.1999) IYM = IYYM-1900
       IF(IYYM.GT.1999) IYM = IYYM-2000
       YM(itt,1,istat) = WAS1
       YM(itt,4,istat) = WAS3
       YM(itt,5,istat) = WAS4

       IWASDAT = IYM*10**6+IMM*10**4+IDM*10**2+IHM
       IF(IWASDAT.LT.idatini) itt = itt-1
       IF(IWASDAT.EQ.idatfin) GO TO 350
       GO TO 302
 350   CONTINUE

C     UNREAL DATA ARE DISREGARDED

      DO 360 I=2,ITT-1
      TM1 = YM(I-1,4,1)
      TM2 = YM(I,4,1)
      TM3 = YM(I+1,4,1)
      IF(TM2.LE.0.) GO TO 360
      IF(TM2.GT.10.)GO TO 370
      IF(TM1.LE.0. .AND. TM2.GT.5.) GO TO 370
      IF(TM1.GT.0.) THEN
         IF(TM2.GT.6. .AND. (TM2/TM1).GT.2.) GO TO 370
         END IF
      IF(TM3.EQ.0. .AND. TM2.GT.5.) GO TO 370
      IF(TM3.GT.0.) THEN
         IF(TM2.GT.0. .AND. (TM2/TM3).GT.1.5) GO TO 370
         END IF
      GO TO 360
      
 370  CONTINUE
      YM(I,4,1) = -9.0
 360  CONTINUE


C     AT RON LOCATIONS

C     SKIP HEADING LINES
C     do 402 i=1,3
C     read(31,'(a1)')a
C402  continue

c     do 403 it=8,nt
c     read(25,2501)k1,k2,k3
c     ym(it,1,1)=k1/100.
c     ym(it,4,1)=k2/10.
c     ym(it,5,1)=k3
c403  continue

c     check print
      go to 888

      do 950 istat=1,nstaz
      write(6,6601)istat,((y0(it,kk,istat),kk=1,5),it=1,nt)
 950  continue
      do 951 istat=1,nstaz
      write(6,6601)istat,((y1(it,kk,istat),kk=1,5),it=1,nt)
 951  continue
      do 952 istat=1,nstaz
      write(6,6601)istat,((y2(it,kk,istat),kk=1,5),it=1,nt)
 952  continue
      do 953 istat=1,nstaz
      write(6,6601)istat,((y3(it,kk,istat),kk=1,5),it=1,nt)
 953  continue
      do 954 istat=1,nstaz
      write(6,6601)istat,((ym(it,kk,istat),kk=1,5),it=1,nt)
 954  continue

      stop 1235

 888  continue

      return

c -------------------------------------------------------------------


 4000 continue
      write(6,'(''  TROVATO E O F '')')
      return

c -------------------------------------------------------------------

 2001 format(1X,I8,1X,4(1X,F5.2,F6.3,F5.0))
 6601 format(1x/1x,'station ',i2/(1x,5f10.2))
 2501 format(17x,3i3)
 3001 format(i5,i6,3i3,1x,3f6.2,f6.0,3x,2f6.2)

 1301 FORMAT(6I5,3X,F5.2,2F8.3,F5.0,3F5.2)


      end

C##########################################################################

      FUNCTION IECF_len(y_char)
c
c  Calculate the 'length' of a character string.
c
c  The 'length' is the character-position of the last character in the
c  string which is neither 'BLANK', nor 'NULL'.
c
      CHARACTER*(*) y_char
      CHARACTER*1   null
c
      null = char(0)
      lgth = len(y_char)
c
      DO 10 i = lgth , 1 , -1
      IF ((y_char(i:i) .NE. ' ') .AND. (y_char(i:i) .NE. null)) goto 20
   10 CONTINUE
      i = 0
c
   20 CONTINUE
      IECF_len = i
c
      RETURN
      END
      
C -----------------------------------------------------------------
C    DATETOSTRING
C    Converts a date of number type YYMMDDHH into a string of type
C    YYYY-MM-DD HH:MM:SS
C -----------------------------------------------------------------

      subroutine DATETOSTRING(MDATE,CDATE)
    
       real MDATE
      integer DATE
      character*19 CDATE
      integer YEAR, MONTH, DAY, HOUR
       
      
      DATE  = MDATE
       Write(6,*) "DATETOSTRING", DATE
      YEAR  = DATE/10**6
      DATE  = DATE-YEAR*10**6
      MONTH = DATE/10**4
      DATE  = DATE-MONTH*10**4
      DAY   = DATE/10**2
      HOUR  = DATE-DAY*10**2
      YEAR  = YEAR + 2000
      

      WRITE(CDATE,
     +        '(I4,''-'',I0.2,''-'',I0.2, '' '',I0.2,''\:00\:00'')')
     +             YEAR,MONTH,DAY,HOUR

      END

