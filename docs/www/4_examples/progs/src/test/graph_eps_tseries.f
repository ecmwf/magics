      PROGRAM GRAFEPSTSERIES
    
C--- Copied and adapted from program TSERIES being used by Luciana

C ---------------------------------------------------------------------
C
C**** *PLTIMES* - PLOT TIME SERIES OF MODEL FROM ENSEMBLE FORECAST RUNS.
C
C     LIANA ZAMBRESKY  GKSS/ECMWF   FEBRUARY 1988
C     HEINZ GUNTHER    GKSS/ECMWF   AUGUST   1991 MODIFIED FOR MAGICS.
C     LUIGI CAVALERI   ISDGM        MARCH    1992 MODIFIED FOR INCREASED
C                                            PLOTTING AND IMPROVED TIME AXIS
C
C     PURPOSE
C     -------
C
C         PLOT A TIME SERIES OF WIND OR WAVE OUT OF ENSAMBLE FORECAST
C
C     INTERFACE
C     ---------
C
C       IU05  USER INPUT.
C       IU06  PRINTER OUTPUT.
C
C     METHOD
C     ------
C
C     A PLOT IS PRODUCED WITH TIME SPAN LIMITED TO THE PERIOD OF THE RUN
C
C ----------------------------------------------------------------------
C
C     *PARAMETER* FOR ARRAY DIMENSIONS.
C
ccccccc      PARAMETER ( IDR=52, IDT=56)
      PARAMETER ( IDR=52, IDT=160)
      PARAMETER ( IDS=6)
C
C        *IDR* INTEGER  NUMBER OF RUNS ANALYSIS AND FORECAST
C        *IDT* INTEGER  LENGTH OF TIME SERIES.
C         IDS  INTEGER  NUMBER OF LOCATIIONS.
C
C ----------------------------------------------------------------------
C
C     YMODEL(,) = MODEL RESULTS 
C
      DIMENSION YMODEL(IDR,IDT)

c .....................................................................
C
C      CHARACTER*80  LINE

C      NAME()         : NAME OF LOCATION
       CHARACTER*14 NAME(IDS), NNAME
C      XLON(), XLAT() : GEOGRAPHICAL COORDINATES 
       DIMENSION XLON(IDS), XLAT(IDS)

       COMMON/BOE/NAME,XLON,XLAT
c .....................................................................

       CHARACTER*10 CDATEA, CDATEE, CDATEF, CDATM

       INTEGER DATEMIN, DATEMAX

c .....................................................................

C      IMON()         NUMBER OF DAYS IN EACH MONTH OF THE YEAR

      INTEGER   IMON(12)
      CHARACTER MONTH(12)*9
C
      DATA MONTH/'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     *           'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     *           'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA IMON/31,28,31,30,31,30,31,31,30,31,30,31/

      COMMON /IMO/IMON,MONTH

       INTEGER STEP
       DATA STEP/3/
C .....................................................................
      CHARACTER TXT1*50, TXT2*50, TXT3*50, TEXTX*50, TEXTY*50
      CHARACTER RUN*20
      CHARACTER*80 LINE
C .....................................................................

      LOGICAL IEOF

      DIMENSION XTIME(IDT)
      DIMENSION XSHAD(IDT)
      DIMENSION YPLOT(IDT)
      DIMENSION AVER(IDT)
      DIMENSION RMEDI(IDT), QU25(IDT), QU75(IDT)
      DIMENSION QU25S(IDT), QU75S(IDT)
      DIMENSION RMINI(IDT), RMAXI(IDT)
      DIMENSION ORDIN(IDR)



C
C ----------------------------------------------------------------------
C*    1. INITIALIZATION.
C        ---------------
C
 1000 CONTINUE
C
C     1.1 UNITS.
C         ------
C
C      IU05 = 5
      IU06 = 6
      
      OPEN (5, FILE='data/eps_tseries_stdin.txt')
 
 
      OPEN (11, FILE='data/TIME_EPS_1966110400_CNRTOWER_0')
      OPEN (12, FILE='data/TIME_EPS_1966110400_CNRTOWER_1')
      OPEN (13, FILE='data/TIME_EPS_1966110400_CNRTOWER_2')
      OPEN (14, FILE='data/TIME_EPS_1966110400_CNRTOWER_3')
      OPEN (15, FILE='data/TIME_EPS_1966110400_CNRTOWER_4')
      OPEN (16, FILE='data/TIME_EPS_1966110400_CNRTOWER_5')
      OPEN (17, FILE='data/TIME_EPS_1966110400_CNRTOWER_6')
      OPEN (18, FILE='data/TIME_EPS_1966110400_CNRTOWER_7')
      OPEN (19, FILE='data/TIME_EPS_1966110400_CNRTOWER_8')
      OPEN (20, FILE='data/TIME_EPS_1966110400_CNRTOWER_9')
      OPEN (21, FILE='data/TIME_EPS_1966110400_CNRTOWER_10')
      OPEN (22, FILE='data/TIME_EPS_1966110400_CNRTOWER_11')
      OPEN (23, FILE='data/TIME_EPS_1966110400_CNRTOWER_12')
      OPEN (24, FILE='data/TIME_EPS_1966110400_CNRTOWER_13')
      OPEN (25, FILE='data/TIME_EPS_1966110400_CNRTOWER_14')
      OPEN (26, FILE='data/TIME_EPS_1966110400_CNRTOWER_15')
      OPEN (27, FILE='data/TIME_EPS_1966110400_CNRTOWER_16')
      OPEN (28, FILE='data/TIME_EPS_1966110400_CNRTOWER_17')
      OPEN (29, FILE='data/TIME_EPS_1966110400_CNRTOWER_18')
      OPEN (30, FILE='data/TIME_EPS_1966110400_CNRTOWER_19')
      OPEN (31, FILE='data/TIME_EPS_1966110400_CNRTOWER_20')
      OPEN (32, FILE='data/TIME_EPS_1966110400_CNRTOWER_21')
      OPEN (33, FILE='data/TIME_EPS_1966110400_CNRTOWER_22')
      OPEN (34, FILE='data/TIME_EPS_1966110400_CNRTOWER_23')
      OPEN (35, FILE='data/TIME_EPS_1966110400_CNRTOWER_24')
      OPEN (36, FILE='data/TIME_EPS_1966110400_CNRTOWER_25')
      OPEN (37, FILE='data/TIME_EPS_1966110400_CNRTOWER_26')
      OPEN (38, FILE='data/TIME_EPS_1966110400_CNRTOWER_27')
      OPEN (39, FILE='data/TIME_EPS_1966110400_CNRTOWER_28')
      OPEN (40, FILE='data/TIME_EPS_1966110400_CNRTOWER_29')
      OPEN (41, FILE='data/TIME_EPS_1966110400_CNRTOWER_30')
      OPEN (42, FILE='data/TIME_EPS_1966110400_CNRTOWER_31')
      OPEN (43, FILE='data/TIME_EPS_1966110400_CNRTOWER_32')
      OPEN (44, FILE='data/TIME_EPS_1966110400_CNRTOWER_33')
      OPEN (45, FILE='data/TIME_EPS_1966110400_CNRTOWER_34')
      OPEN (46, FILE='data/TIME_EPS_1966110400_CNRTOWER_35')
      OPEN (47, FILE='data/TIME_EPS_1966110400_CNRTOWER_36')
      OPEN (48, FILE='data/TIME_EPS_1966110400_CNRTOWER_37')
      OPEN (49, FILE='data/TIME_EPS_1966110400_CNRTOWER_38')
      OPEN (50, FILE='data/TIME_EPS_1966110400_CNRTOWER_39')
      OPEN (51, FILE='data/TIME_EPS_1966110400_CNRTOWER_40')
      OPEN (52, FILE='data/TIME_EPS_1966110400_CNRTOWER_41')
      OPEN (53, FILE='data/TIME_EPS_1966110400_CNRTOWER_42')
      OPEN (54, FILE='data/TIME_EPS_1966110400_CNRTOWER_43')
      OPEN (55, FILE='data/TIME_EPS_1966110400_CNRTOWER_44')
      OPEN (56, FILE='data/TIME_EPS_1966110400_CNRTOWER_45')
      OPEN (57, FILE='data/TIME_EPS_1966110400_CNRTOWER_46')
      OPEN (58, FILE='data/TIME_EPS_1966110400_CNRTOWER_47')
      OPEN (59, FILE='data/TIME_EPS_1966110400_CNRTOWER_48')
      OPEN (60, FILE='data/TIME_EPS_1966110400_CNRTOWER_49')
      OPEN (61, FILE='data/TIME_EPS_1966110400_CNRTOWER_50')
      OPEN (62, FILE='data/TIME_ANA_1966_CNRTOWER')
 
      
C
C     1.2 INITIALIZE ARRAYS
C         -----------------
C
      DO 1201 I=1,IDR
            ORDIN(I)    = -999.
        DO 1202 J=1,IDT
            YMODEL(I,J) = -999.
            YPLOT(J)    = -999.
            AVER(J)     = 0.0
            QU25(J)     = 0.0
            QU75(J)     = 0.0
            RMEDI(J)    = 0.0
            RMINI(J)    = 0.0
            RMAXI(J)    = 0.0
 1202 CONTINUE
 1201 CONTINUE

      DO 1205 IS=1,IDS
        NAME(IS) = 'xxxxxxxxxxxxxx'
        XLON(IS) = -999.
        XLAT(IS) = -999.
 1205 CONTINUE

C
C     1.3 READ INITIAL AND FINAL DATES
C         ----------------------------
C
        READ(5,'(A10,2x,A10)') CDATEA, CDATEE 
        READ(CDATEA,'(I8.8,I2.2)') IDATINI,IMI
        READ(CDATEE,'(I8.8,I2.2)') IDATFIN,IMI

       WRITE(6,'('' IDATINI , IDATFIN = '',I8.8,5x,I8.8)')
     .             IDATINI,IDATFIN


C     LIMIT DATES OF THE PLOT AND ITS DURATION ARE EVALUATED IN SUB PLOTDATE
       CALL PLOTDATE (CDATEA,CDATEE,DATEMIN,DATEMAX,DURATION)
C     EVALUATE THE NUMBER OF DATA TO PLOT
      NDT = DURATION/100.*(24/STEP)
      WRITE(6,'('' NUMBER OF DATA TO PLOT ='',I5)') NDT
C
C     1.4 READ LOCATION INFO
C         ------------------
       READ(5,*) ILOC
       DO 1401 I=1,ILOC
         READ(5,1002) IB,RLAT,RLON,NNAME
 1002 format(i6,3x,f9.2,1x,f9.2,1x,A14)
         NAME(IB) = NNAME
         XLAT(IB) = RLAT
         XLON(IB) = RLON
 1401  CONTINUE

        write(6,'(''NUMBER OF STATIONS TO PLOT IS ~~~~~~~>'',I5)')ILOC

      READ(5,*) NLOC
      WRITE(6,'(''THE POINT TO PLOT IS'',I4,A16)')NLOC,NAME(NLOC)

      WRITE(6,'(''DATES FOR PLOT''/
     .          '' INITIAL DATE ='',I13,'' - FINAL DATE ='',I13,
     .          3X,''DURATION ='',F13.0)') DATEMIN,DATEMAX,DURATION

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -




C
C*    1.5 INITIALIZE PLOTTING.
C         --------------------
C
      CALL POPEN
C      CALL PSETC ('WORKSTATION_1', 'PS_COL')
      CALL PARSE_COMMAND_LINE ('graph_eps_tseries')
C     CALL PSETC ('WORKSTATION_1', 'PS')

      CALL PSETC ('SUBPAGE_MAP_PROJECTION','CARTESIAN')  

C
C*   1.6 INITIALIZE HORIZONTAL AXIS TIME ARRAY
C        -------------------------------------
C
      DEL   = REAL(STEP)/24.
      XSTRT = 0.
C     NOTE - TIME INCREMENT IS EVALUATED IN FORMAT YYMMDDHH.0
C     NOTE - TIME IS EVALUATED UNDER THE ASSUMPTION THAT THE WHOLE SPAN IS
C            LESS THAN A MONTH - THIS IS BECAUSE WE ARE WORKING IN HOUR -
C            THIS CAN BE IMPROVED -
      DO 3001 I=1,IDT
         NHOURS=I*STEP
         XTIME(I)=(NHOURS/24)*100.+MOD(NHOURS,24)
 3001 CONTINUE

C ----------------------------------------------------------------------
C
C     2.0 LOOP OVER LOCATIONS
C         -------------------

ccc   DO 2100 IB=1,ILOC
      DO 2100 IB=NLOC,NLOC

C     2.1 READ INPUT DATA
C         ---------------

      call DATIN(CDATEA,CDATEE,DATEMIN,IDR,IDT,ymodel)

c   * * * REMEMBER: YMODEL(52,IDT) IS ANALYSIS  * * *

c     2.2 EVALUATE MEAN VALUE
C         -------------------

      DO 2201 J=1,IDT
      AV = 0.0
      NV = 0
      DO 2202 I=1,IDR-1
      AV = AV+YMODEL(I,J)
      NV = NV+1
 2202 CONTINUE
      AVER(J) = AV/NV
 2201 CONTINUE

C     2.3 EVALUATE 'QUARTILE'
C         -------------------

      DO 2301 J=1,IDT
      DO 2302 I=1,IDR-1
      ORDIN(I) = YMODEL(I,J)
 2302 CONTINUE
      DO 2303 I=1,IDR-1
      XMIN = ORDIN(I)
      IMM  = I
      DO 2304 II=I+1,IDR-1
      IF(ORDIN(II).LT.XMIN) THEN
         XMIN = ORDIN(II)
         IMM  = II
         END IF
 2304  CONTINUE

      AA = ORDIN(IMM)
      ORDIN(IMM) = ORDIN(I)
      ORDIN(I)   = AA
 2303 CONTINUE

      RMINI(J) = ORDIN(1)
      RMAXI(J) = ORDIN(IDR-1)
         IF(RMINI(J).LT.0.0) RMINI(J) =0.0
         IF(RMAXI(J).LT.0.0) RMAXI(J) =0.0
         IF(RMAXI(J).GT.32.) RMAXI(J) =32.
      RMEDI(J) = ORDIN(25)
      QU25(J)  = ORDIN(12)
      QU75(J)  = ORDIN(38)

 2301 CONTINUE

      DO 2305 J=1,IDT-3
      XSHAD(J) = XTIME(J+3)
      RMINI(J) = RMINI(J+3)
      RMAXI(J) = RMAXI(J+3)
      QU25S(J) = QU25(J+3)
      QU75S(J) = QU75(J+3)
 2305 CONTINUE
C ---------------------------------------------------------------------
C
C     3.0 PLOT TIME SERIES
C         ----------------

C
C        TIME SCALE OF THE PLOT IS ASSIGNED
C
         XMIN=DATEMIN
         XMAX=DURATION
         XUNIT=1.

C
C*    3.1 INITIALIZE PLOTTING.
C         --------------------
C
      CALL PSETC ('PAGE_ID_LINE',                       'OFF' )
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',             'OFF' )
      CALL PSETC ('PAGE_ID_LINE_SYSTEM_PLOT',           'OFF' )
      CALL PSETC ('PAGE_ID_LINE_DATE_PLOT',             'OFF' )
      CALL PSETC ('PAGE_ID_LINE_ERRORS_PLOT',           'OFF' )
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',         'ON' )
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT',                '' )
      CALL PSETR ('PAGE_ID_LINE_HEIGHT',                  0.2 )

      CALL PSETR ('SUPER_PAGE_X_LENGTH',                  29.7)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',                  21.0)
      CALL PSETC ('SUPER_PAGE_FRAME',                    'OFF')

      CALL PSETR ('PAGE_X_LENGTH',                        27.5)
      CALL PSETR ('PAGE_Y_LENGTH',                        18.0)
ccc   CALL PSETC ('PAGE_FRAME',                         'OFF' )

      CALL PSETC ('TEXT_QUALITY',                       'HIGH')
      CALL PSETI ('GRAPH_LINE_THICKNESS',                    2)
      CALL PSETC ('AXIS_TICK_LABEL_QUALITY',            'HIGH')
      CALL PSETC ('LEGEND',                               'ON')
      CALL PSETR ('LEGEND_TEXT_MAXIMUM_HEIGHT',           0.25)
      CALL PSETC ('LEGEND_TEXT_COMPOSITION',  'USER_TEXT_ONLY')
      CALL PSETC ('LEGEND_BOX_MODE',              'POSITIONAL')
      CALL PSETR ('LEGEND_BOX_X_POSITION',                 3.0)
ccc   CALL PSETR ('LEGEND_BOX_Y_POSITION',                 1.5)
      CALL PSETR ('LEGEND_BOX_Y_POSITION',                14.0)
      CALL PSETR ('LEGEND_BOX_X_LENGTH',                  20.0)
      CALL PSETR ('LEGEND_BOX_Y_LENGTH',                   0.5)

      READ(CDATEA,'(5I2.2)') IYY, IMM, IDA, IHO, IMI
      IIYY = IYY
      IF(IIYY.GE.50) IYY = IYY + 1900
      IF(IIYY.LT.50) IYY = IYY + 2000

      TXT2 = MONTH(IMM)
      IL =IECF_LEN(MONTH(IMM))
c!!!  TXT2 = MONTH(IMM+1)
c???  IL =IECF_LEN(MONTH(IMM+1))
      WRITE(TXT2(IL+2:IL+5),'(I4.4)') IYY
C
C =============================================================================
C
C*    3.2 PLOT PAGE HEADER.
C         -----------------
C
         CALL PSETC ('TEXT_MODE',       'POSITIONAL')
         CALL PSETR ('TEXT_BOX_X_POSITION',      3.0)
         CALL PSETR ('TEXT_BOX_Y_POSITION',     16.0)
         CALL PSETR ('TEXT_BOX_X_LENGTH',       20.0)
         CALL PSETR ('TEXT_BOX_Y_LENGTH',        2.0)
         CALL PSETI ('TEXT_LINE_COUNT',            2)

         IF (XLAT(IB).LT.0.) THEN
            WRITE(TXT3,'('' ('',F5.2,''S,'')') -XLAT(IB)
         ELSE
            WRITE(TXT3,'('' ('',F5.2,''N,'')') XLAT(IB)
         ENDIF
         IF (XLON(IB).GT.180.) THEN
            WRITE(TXT3(10:17),'(F6.2,''W)'')') 360.-XLON(IB)
         ELSE
            WRITE(TXT3(10:17),'(F6.2,''E)'')') XLON(IB)
         ENDIF
         ILENN = IECF_LEN(NAME(IB))
         TXT1 = NAME(IB)(1:ILENN)//TXT3(1:17)

         CALL PSETC ('TEXT_LINE_1', TXT1)
         CALL PSETC ('TEXT_LINE_2', TXT2)
         CALL PSETC ('TEXT_COLOUR',                  'BLACK')
         CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.45)
         CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1',         1.0)
         CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_2',         1.0)
         CALL PSETC ('TEXT_JUSTIFICATION',          'CENTRE')
         CALL PTEXT
C
C ---------------------------------------------------------------------------
C
C        TIME SCALE OF THE PLOT IS ASSIGNED
C
         XMIN  = DATEMIN
         XMAX  = DURATION
         XUNIT = 1.
         YMIN  = 0.0
         YMAX  = 32.
         YUNIT = 8.
         TEXTX = ' '
         TEXTY = 'WIND SPEED (M/S)'


C*    3.3 PLOT WAVE HEIGHTS.
C         ------------------
C
         CALL PSETR ('SUBPAGE_X_POSITION',                 3.0)
         CALL PSETR ('SUBPAGE_Y_POSITION',                 3.0)
         CALL PSETR ('SUBPAGE_X_LENGTH',                   20.)
         CALL PSETR ('SUBPAGE_Y_LENGTH',                   13.)
         CALL PSETC ('SUBPAGE_FRAME',                     'ON')
cccc         CALL PSETC ('SUBPAGE_MAP_PROJECTION',          'CARTESIAN')

         CALL PSETC ('LEGEND_ENTRY',      'ON')
C
C     DRAW AXIS.
C
ccc      CALL PSETR ('AXIS_MONTHS_LABEL_HEIGHT',0.25)
         CALL PSETC ('AXIS_YEARS_LABEL',       'OFF')
         CALL PSETC ('AXIS_MONTHS_LABEL',      'OFF')
         CALL PSETC ('AXIS_DAYS_LABEL',        'OFF')

         CALL AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT, TEXTX, TEXTY)
C
C    3.4 LOOP OVER DIFFERENT RUNS (AN+FC)
C        --------------------------------

c ....................................................................
c ....................................................................
c ....................................................................
         DO 3400 IR=1,IDR

ccc      IF(IR.EQ.1) CALL PSETC ('LEGEND_USER_TEXT',  'CF')
ccc      IF(IR.EQ.2) CALL PSETC ('LEGEND_USER_TEXT',  'PF')
         IF(IR.GT.2) CALL PSETC ('LEGEND_ENTRY',     'OFF')

C     DRAW MODEL TIME SERIES.

C     PREPARE ARRAY TO PLOT
      DO 3401 ITT=1,IDT
      YPLOT(ITT) = YMODEL(IR,ITT)
 3401 CONTINUE

c     control forecast   <-----------------------------------
ccc   if(IR.EQ.1) THEN
ccc      ITHICK=6
ccc      ISYM=5
ccc      CALL PSERIE2 (XTIME, YPLOT, IDT, IDT,
ccc  1                'SOLID', 'YELLOW', ISYM, ITHICK)
ccc   end if
C
c     perturbed forecast <-----------------------------------
ccc   if(IR.NE.1) THEN
ccc      ITHICK=4
ccc      ISYM=5
ccc      CALL PSERIE2 (XTIME, YPLOT, IDT, IDT,
ccc  1                'SOLID', 'SKY', ISYM, ITHICK)
ccc   end if
C
c     ANALYSIS           <-----------------------------------
ccc   if(IR.EQ.52) THEN
ccc      ITHICK=6
ccc      ISYM=5
ccc      CALL PSETC   ('LEGEND_ENTRY',      'ON')
ccc      CALL PSETC   ('LEGEND_USER_TEXT',  'AN')
ccc      CALL PSERIE2 (XTIME, YPLOT, IDT, IDT,
ccc  1                'SOLID', 'GREEN', ISYM, ITHICK)
ccc   end if

c     END OF LOOP OVER RUNS

 3400 CONTINUE

c ....................................................................
c ....................................................................
c ....................................................................

         ITHICK=9
         ISYM=5
         CALL PSETC   ('LEGEND_ENTRY',      'ON')

C     DRAW SHADED AREAS

C        BETWEEN MAXIMUM AND MINIMUM PERTURBED-FORECAST
C

         CALL PSETR   ('GRAPH_Y_SUPPRESS_BELOW', -1000.0)
         CALL PSETC   ('LEGEND_USER_TEXT',  'PF')
         DEN = 4
         CALL PSERIE3 (XSHAD, RMINI, RMAXI,IDT, NDT,
     1                'SOLID', 'SKY', ISYM, ITHICK, DEN)

C        BETWEEN 25% AND 75% LINES OF PERTURBED-FORECAST
C
         CALL PSETC   ('LEGEND_ENTRY',     'OFF')
         DEN = 6
         CALL PSERIE3 (XSHAD, QU25S, QU75S,IDT, NDT,
     1                'SOLID', 'SKY', ISYM, ITHICK, DEN)

         CALL PSETC   ('LEGEND_ENTRY',      'ON')
C
C     CONTROL FORECAST    <-----------------------------------
         DO 3402 ITT=1,IDT
         YPLOT(ITT) = YMODEL(1,ITT)
 3402    CONTINUE
         CALL PSETC   ('LEGEND_USER_TEXT',  'CF')
         CALL PSERIE2 (XTIME, YPLOT, IDT, IDT,
     1                'SOLID', 'ORANGE', ISYM, ITHICK)
C
C     DRAW LINE OF 25% AND 75%   <-----------------------------
         ITHICK=4
         CALL PSETC   ('LEGEND_USER_TEXT',  '25%')
         CALL PSERIE2 (XTIME, QU25 , IDT, IDT,
     1                'SOLID', 'BLUE', ISYM, ITHICK)
C
C     DRAW 'MEDIAN'       <------------------------------------
         ITHICK=6
         CALL PSETC   ('LEGEND_USER_TEXT',  'median')
         CALL PSERIE2 (XTIME, RMEDI, IDT, IDT,
     1                'DASH', 'BLUE', ISYM, ITHICK)
C
         ITHICK=4
         CALL PSETC   ('LEGEND_USER_TEXT',  '75%')
         CALL PSERIE2 (XTIME, QU75, IDT, IDT,
     1                'SOLID', 'BLUE', ISYM, ITHICK)
C
c     ANALYSIS           <-----------------------------------
         YPLOT(1) = YMODEL(52,1)
         YPLOT(IDT) = YMODEL(52,IDT)
         DO 3403 ITT=2,IDT-1
         YPLOT(ITT) = YMODEL(52,ITT)
         if(YPLOT(ITT).eq.-99.0)
     .   YPLOT(ITT) = (YMODEL(52,ITT-1)+YMODEL(52,ITT+1))/2.
 3403    CONTINUE
         ITHICK=7
         ISYM=5
         CALL PSETC   ('LEGEND_USER_TEXT',  'AN')
         CALL PSERIE2 (XTIME, YPLOT, IDT, IDT,
     1                'SOLID', 'BLACK', ISYM, ITHICK)
C
C     DRAW MEAN VALUES

c        ITHICK=6
c        ISYM=5
c        CALL PSETC   ('LEGEND_ENTRY',      'ON')
c        CALL PSETC   ('LEGEND_USER_TEXT',  'AV')
c        CALL PSERIE2 (XTIME, AVER, IDT, IDT,
c    1                'SOLID', 'MAGENTA', ISYM, ITHICK)

c ....................................................................

C*    4.7 END OF PHYSICAL PAGE.
C         ---------------------

      CALL PNEW ('PAGE')
C     CALL PNEW ('SUPER_PAGE')

 4001 CONTINUE



C --------------------------------------------------------------
C
C*    4. WRITE SUMMARY AT EACH BUOY LOCATION.
C        ------------------------------------
C
4000  CONTINUE

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ccc        go to 678

         WRITE(99,*) '  '
         WRITE(99,*) ' SIDE: ',NAME(1)
         WRITE(99,'(3X,''TIME'',9X,''WAVE'',16x,''WAVE'',/
     .                '' YYMMDDHH'',4X,''ANALY'',4X,''MIN'',
     .                3X,'' AVER '',4X,''MAX'',3X,''25 %  '',
     .                3X,''MEDIAN'',4X,''75 %''/)')
      DO 4002 I=1,IDT
         WRITE(99,43)I,XTIME(I),YMODEL(52,I),
     .               RMINI(I), AVER(I), RMAXI(I),
     .               QU25(I), RMEDI(I),QU75(I)
 43      FORMAT(i2,6X,F5.0,7F7.2)
 4002 CONTINUE

 678  continue

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C =============================================================================

c     END OF LOOP OVER LOCATIONS

 2100 CONTINUE

C     4.14   CLOSE GRAPHICS
C     ---------------------

      CALL PCLOSE

      STOP
      END

      subroutine DATIN(CDATEA,CDATEE,DATEMIN,idr,idt,y0)

c     sub DATIN read in data for the time series plot

c -------------------------------------------------------------------

c     variables definition

C     CDATEA       INITIAL DATE OF PLOT
C     CDATEE       END DATE OF PLOT
c     Y0(,,)     = analysis data

c     idr        = max number of runs
c     idt        = max number of time steps

       INTEGER   DATEMIN
c -------------------------------------------------------------------

      real y0(idr,idt)
      character*1 a
      character*10 CDATEA, CDATEE
      INTEGER STEP
c .............................................................

      READ(CDATEA,'(I8,I2)') idatini, III
      READ(CDATEE,'(I8,I2)') idatfin, III
      DATEMINI = idatini
ccccccccccccccccc      DATEMINI = 66103112
      STEP = 3
c -------------------------------------------------------------------

      nfil = 1
      nfil = IDR
c     model data

      do 250 istat=1,nfil

      iunit = 10 + istat
      it = 0

      WRITE(6,'(''READING UNIT N.'',I5)')IUNIT

C     SKIP HEADING LINES
      DO 100 I=1,50
      READ(IUNIT,*)
 100  CONTINUE

 252     CONTINUE

         READ(iunit,2001,end=3000)
     .     IY,IM,ID,IH, U ,V ,WIND,WDIR, HS,DM,TM,TP
 2001    FORMAT(I4,3I2.2,1X,8F7.2)
         IF(IY.GE.2000) IIY=IY-2000
         IF(IY.LE.1999) IIY=IY-1900
         IDATE=IIY*10**6+IM*10**4+ID*10**2+IH

         if(IDATE.lt.DATEMINI)  go to 252

c     find index
      CALL INCDATE3(IIY,IM,ID,IH,DATEMIN,STEP,IND)
         IT=IND
         y0(istat,it) = WIND

         if(IDATE.eq.idatfin)  go to 250
         GO TO 252

c     END OF FILE FOUND
 3000 CONTINUE
      write(6,'(''  TROVATO E O F  ON UNIT'',i5)')IUNIT

C     END OF LOOP OVER FILES
 250  CONTINUE

      return
      end
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
ccc      write(6,'(''----> SUB. IECF_len'')')

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
c???????????????????????????????????????????????
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
      WRITE(6,5002) Y2,M2,D2,H2 ,  DATEMIN , Y,M,D,H
      IX=1
      return
cccccccccccccccccccc      STOP
 4001 FORMAT(1X//////1X,'IDATM EQ DATEMIN'//////)

 5000 WRITE(6,5001)
      WRITE(6,5002) Y2,M2,D2,H2 ,  DATEMIN , Y,M,D,H
      STOP
 5001 FORMAT(1X//////1X,'IDATM LT DATEMIN'//////)
 5002 format(1x,4i4,5x,i12,5x,4i4)

      END
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
      IF(MOD(Y1,4).EQ.0) IMON(2) = 29
cccccc         D1=D1-1

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
      IF(MOD(Y2,4).EQ.0) IMON(2) = 29
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

      SUBROUTINE AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT,
     1                 TEXTX, TEXTY)
      CHARACTER TEXTX*(*), TEXTY*(*)

C     XMAX IS NOW THE TIME SPAN OF THE PLOT, WRITTEN AS YYMMDDHH.0

      CALL PSETC ('AXIS_GRID', 'ON')
      CALL PSETC ('AXIS_GRID_LINE_STYLE','DOT')
      CALL PSETI ('AXIS_LINE_THICKNESS',2)
      CALL PSETC ('AXIS_GRID_COLOUR', 'BLACK')

      CALL PSETC ('AXIS_LINE', 'OFF')
      CALL PSETC ('AXIS_ORIENTATION', 'HORIZONTAL')
      CALL PSETC ('AXIS_POSITION','BOTTOM')
      CALL PSETR ('AXIS_BASE_DATE',XMIN)
      CALL PSETR ('AXIS_MIN_VALUE', 0.0)
      CALL PSETR ('AXIS_MAX_VALUE', XMAX)

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

      CALL PSET1R ('GRAPH_CURVE_X_VALUES', XVAL, NDT)
      CALL PSET1R ('GRAPH_CURVE_Y_VALUES', YVAL, NDT)
      CALL PGRAPH

      RETURN
      END


      SUBROUTINE PSERIE2 (XVAL, YVAL, IDT, NDT, STYLE, COLOUR, ISYMB,
     *                   ITHICK)
      CHARACTER COLOUR*(*), STYLE*(*)
      DIMENSION XVAL(IDT), YVAL(IDT)

      CALL PSETC ('GRAPH_TYPE', 'CURVE')
      CALL PSETC ('GRAPH_LINE_COLOUR', COLOUR)
      CALL PSETC ('GRAPH_LINE_STYLE', STYLE)
      CALL PSETI ('GRAPH_LINE_THICKNESS', ITHICK)

ccc   CALL PSETC ('GRAPH_SYMBOL', 'ON')
      CALL PSETC ('GRAPH_SYMBOL', 'OFF')
      CALL PSETI ('GRAPH_SYMBOL_MARKER_INDEX', ISYMB)
      CALL PSETR ('GRAPH_SYMBOL_HEIGHT', 0.05*ITHICK)
      CALL PSETC ('GRAPH_SYMBOL_COLOUR', COLOUR)

      CALL PSET1R ('GRAPH_CURVE_X_VALUES', XVAL, NDT)
      CALL PSET1R ('GRAPH_CURVE_Y_VALUES', YVAL, NDT)
      CALL PGRAPH

      RETURN
      END


      SUBROUTINE PSERIE3 (XVAL, YVAL1, YVAL2, IDT, NDT, STYLE, COLOUR,
     *                    ISYMB, ITHICK,DDD)
      CHARACTER COLOUR*(*), STYLE*(*)
      DIMENSION XVAL(IDT), YVAL1(IDT), yval2(IDT)


      DENS = 10.* DDD
      DOTS = 0.01 * DDD

      CALL PSETC ('GRAPH_TYPE',          'AREA')
      CALL PSETC ('GRAPH_SHADE',           'ON')
ccc   CALL PSETC ('GRAPH_SHADE_STYLE',  'SOLID')
      CALL PSETC ('GRAPH_SHADE_STYLE',    'DOT')
      CALL PSETR ('GRAPH_SHADE_DENSITY',   DENS)
      CALL PSETR ('GRAPH_SHADE_DOT_SIZE',  DOTS)
      CALL PSETC ('GRAPH_SHADE_COLOUR',  COLOUR)

      IDP = IDT-4
      idp=idp-3

      IDP = NDT-8


      CALL PSET1R ('GRAPH_CURVE_X_VALUES', XVAL,  IDP)
      CALL PSET1R ('GRAPH_CURVE_Y_VALUES', YVAL2, IDP)

      CALL PSET1R ('GRAPH_CURVE2_X_VALUES', XVAL,  IDP)
      CALL PSET1R ('GRAPH_CURVE2_Y_VALUES', YVAL1, IDP)


      CALL PGRAPH

      RETURN
      END

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

c     CALL PSET1R ('GRAPH_CURVE_X_VALUES', XVAL, NDT)
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



#include "parse_command_line.h"
