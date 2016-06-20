C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

       PROGRAM SCATPLT

C      PROGRAM TO READ WAVE DATA FROM FILES AND TO PLOT THE RESULTING
C      SCATTER -
C      DIFFERENT DATA ARE READ:
C      -  TIME SERIES OF WAM MODEL RESULTS
C      -  MEASUREMENTS  FROM BUOYS
C      -  MEASUREMENTS  FROM TOWER
 
C                              L.BERTOTTI ISDGM      MAR 1995
C                              L.CAVALERI ISDGM      MAR 1995
C                              L.BERTOTTI            MAR 1996 (FOR WS)
C                              L.BERTOTTI            JUL 1998 (SEMPLIFIED)
C                                                          AND MULTIPLE PLOT
C      -  MEASURES vs ANAL-FOR1-FOR2-FOR3 AT FOUR LOCATIONS
C                              L.BERTOTTI ISDGM      MAY 2003
C--------------------------------------------------------------------------
 
C     INPUT UNITS FOR MODEL DATA

C     11      TOWER
C     12      PO-MAESTRA
C     13      ANCONA
C     14      PESCARA
C     15      MONOPOLI
C     ...
C     ...     other locations
C     20

C     INPUT UNITS FOR MEASURED DATA

C     41      RON BUOYS DATA  
C     42      TOWER DATA

C -------------------------------------------------------------------------
       
C      IDS :   MAXIMUM NUMBER OF DIFFERENT RUNS TO BE CONSIDERED
C      IDT :   LENGTH OF TIME SERIES (SUPPOSED MAX ONE YEAR AT 3 HOUR)

       PARAMETER ( IDS = 10, IDT = 3000)
       PARAMETER ( NLOC=5 )

C ------------------------------------------------------------------------

C      NAME         : NAME OF LOCATION

       CHARACTER*10 NAME(NLOC)

C      YMEAS(,N,)   : ARRAY OF MEASUREMENTS
C                     N = 1   WAVE HEIGHT         (M)
C                     N = 2   WIND SPEED          (M/S)
C                     N = 3   WIND DIRECTION      (FLOW - DEGREE CWRGN)
C                     N = 4   WAVE MEAN PERIOD    (S)
C                     N = 5   WAVE MEAN DIRECTION (FLOW - DEGREE CWRGN)

       REAL YMEAS(IDT,5,IDS)

C      YMOD(,N,)    : ARRAY OF WAM MODEL RESULTS
C                     N = 1   WAVE HEIGHT         (M)
C                     N = 2   WIND SPEED          (M/S)
C                     N = 3   WIND DIRECTION      (FLOW - DEGREE CWRGN)
C                     N = 4   WAVE MEAN PERIOD    (S)
C                     N = 5   WAVE MEAN DIRECTION (FLOW - DEGREE CWRGN)

       REAL YMOD0(IDT,5,IDS)
       REAL YMOD1(IDT,5,IDS), YMOD2(IDT,5,IDS), YMOD3(IDT,5,IDS)
C
C      XME()        : COORDINATES ON HORIZONTAL AXIS (MEASUREMENTS)
C      YMO()        : COORDINATES ON VERTICAL AXIS   (MODEL RESULTS)

       REAL XME(IDT), YMO(IDT)

C      DATINI       : INITIAL DATE OF DATA  (GIVEN AS YYMMDDHH)
C      DATFIN       : FINAL   DATE OF DATA  (  "          "   )
C      DATE1        : INITIAL DATE OF PLOT  (  "      YYMMDD00.0)
C      DATE2        : FINAL   DATE OF PLOT  (  "          "     )
C      DX           : DURATION OF PLOT      (  "      YYMMDDHH.0)

       CHARACTER*8   I1, I2
       CHARACTER*8   DATINI, DATFIN

       DOUBLE PRECISION DATE1, DATE2

C      IMON()         NUMBER OF DAYS IN EACH MONTH OF THE YEAR

       INTEGER IMON(12)
       DATA IMON/31,28,31,30,31,30,31,31,30,31,30,31/

       COMMON /IMO/IMON

       INTEGER STEP
       DATA STEP/3/

       DATA NAME /'CNR TOWER ',' VENEZIA  ','   ANCONA ',
     .            ' PESCARA  ','  MONOPOLI'/

       CHARACTER*17 FILPLOT
C ===========================================================================

      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(1)
      DATA         FORMATS /'PS'/
cc    DIMENSION    FORMATS(2)
cc    DATA         FORMATS /'PS', 'PNG'/

C ===========================================================================
       
C       OPEN MAGICS
C       -----------

ccccccc        YEAR = 2010
ccccccc        WRITE(FILPLOT,'("scatbin_MEMO_",i4)') YEAR
        CALL POPEN
        CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 1)
        CALL PSETC  ('OUTPUT_NAME', 'test03')
cccccc        CALL PSETC  ('OUTPUT_NAME',             FILPLOT)  
C       CALL PSETC ('WORKSTATION_1', 'PS_COL')

C       INITIALISE ARRAYS OF MEASURED AND MODEL DATA
        DO 50 IT=1,IDT
        XME(IT) = -999.
        YMO(IT) = -999.
        DO 51 JP=1,5   
        DO 52 IS=1,IDS
        YMEAS(IT,JP,IS) = -999.
        YMOD0(IT,JP,IS) = -999.
        YMOD1(IT,JP,IS) = -999.
        YMOD2(IT,JP,IS) = -999.
        YMOD3(IT,JP,IS) = -999.
 52     CONTINUE
 51     CONTINUE
 50     CONTINUE
       
C       READ INITIAL AND FINAL DATES
       
        open (unit = 5, file = "input03")
        open (unit = 11, file = "fort.11")
        open (unit = 12, file = "fort.12")
        open (unit = 13, file = "fort.13")
        open (unit = 14, file = "fort.14")
        open (unit = 15, file = "fort.15")
        READ(5,'(A8,1x,A8)') DATINI, DATFIN 
        READ(DATINI,'(I8.8)') IDATINI
        READ(DATFIN,'(I8.8)') IDATFIN

      WRITE(6,'('' IDATINI , IDATFIN = '',I8.8,5x,I8.8)')
     .             IDATINI,IDATFIN

C      DATE1, DATE2, DX ARE EVALUATED IN SUB PLOTDATE -
       I1=DATINI
       I2=DATFIN
       CALL PLOTDATE(I1,I2, DATE1,DATE2,DX)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cccc   WRITE(6,'('' DATE1 , DATE2 = '',f20.2,f20.2)') DATE1, DATE2
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

C -------------------------------------------------------------------------

C       READ WAM MODEL RESULTS FOR EACH RUN, IN SEQUENCE
       CALL RDWAMOD(IDATINI,IDATFIN,DATE1,STEP,IDT,IDS,
     .               ymod0,ymod1,ymod2,ymod3)

C       READ MEASUREMENTS FROM BUOYS
c      IUNIT = 41
c      CALL RDBUOYS(IUNIT,IDATINI,IDATFIN,DATE1,STEP,IDT,IDS,YMEAS)
 
C       READ MEASUREMENTS FROM TOWER
c      IUNIT = 42
c      CALL RDTOWER(IUNIT,IDATINI,IDATFIN,DATE1,STEP,IDT,IDS,YMEAS)

C........................................................................

C        PRINT OUT WAVE DATA

        go to 6161
       
        ITT=24
        ITT=60
        
        WRITE(6,'(1X,50(''='')/1X,50(''='')/)')
        WRITE(6,'(''  WAM MODEL RESULTS'')')
        WRITE(6,'(1X,''POINT  NUM   WAVE-HT  WIND-SPD  WIND-DIR'',
     .               ''   WAVE-TM  WAVE-DIR'')')
        DO 100 IL=1,5
        WRITE(6,'(1X,50(''-''))')
        WRITE(6,6011)NAME(IL)
 6011   FORMAT(1X,'LOCATION NAME = ',A10)
        DO 101 IT=1,ITT
        WRITE(6,'(1X,2I5,5F10.2)') IL,IT,(YMOD0(IT,IK,IL),IK=1,5)
 101    CONTINUE
 100    CONTINUE

        WRITE(6,'(//1X,50(''='')/1X,50(''='')/)')
        WRITE(6,'(''  MEASURES FROM BUOYS AND ISDGM TOWER'')')
        WRITE(6,'(1X,''POINT  NUM   WAVE-HT  WIND-INT  WIND-DIR'',
     .               ''   WAVE-TM  WAVE-DIR'')')
        DO 110 IL=1,1
        WRITE(6,'(1X,50(''-''))')
        WRITE(6,6011)NAME(IL)
        DO 111 IT=1,ITT
        WRITE(6,'(1X,2I5,5F10.2)') IL,IT,(YMEAS(IT,IK,IL),IK=1,5)
 111    CONTINUE
 110    CONTINUE

 6161   continue
        
        
        go to 7171
       
        ITT=70
        WRITE(77,'(1X,70(''='')/1X,70(''='')/)')
        WRITE(77,'(''  WAM MODEL RESULTS'')')
        WRITE(77,'(1X,''POINT  NUM   WAVE-HT'')')
        WRITE(77,'(1X,''             MEASURE  ANALYSIS  1D-FOREC'',
     .               ''   2D-FORE  3D-FOREC'')')
        DO 7100 IL=1,1
        WRITE(77,'(1X,70(''-''))')
        WRITE(77,7011)NAME(IL)
 7011   FORMAT(1X,'LOCATION NAME = ',A10)
        DO 7101 IT=264,331
        WRITE(77,'(1X,2I5,5F10.2)') IL,IT, YMEAS(IT,1,IL),
     .   YMOD0(IT,1,IL), YMOD1(IT,1,IL), YMOD2(IT,1,IL), YMOD3(IT,1,IL)
 7101   CONTINUE
 7100   CONTINUE

 7171   continue
        
C........................................................................


C       ISTA = 1   RESULTS AT TOWER    LOCATION
C       ISTA = 2   RESULTS AT PO-MAEST LOCATION
C       ISTA = 3   RESULTS AT ANCONA   LOCATION
C       ISTA = 4   RESULTS AT PESCARA  LOCATION
C       ISTA = 5   RESULTS AT MONOPOLI LOCATION
        ILOC = NLOC
              IYY = IDATINI/10**6
              IF(IYY.GT.50) IYEAR = 1900+IYY
              IF(IYY.LE.50) IYEAR = 2000+IYY
              IF(IYEAR.LT.1999) ILOC = 3
              write(6,'(''NUMBER OF LOCATIONS ------------>>''I5)')ILOC

C       TOTAL NUMBER OF COMPARISONS AT EACH LOCATION
        NRUNS  = 4

        DO 4000 ISTA=1,ILOC

C       SKIP PLOT AT PO-DELTA LOCATION N=2 BECAUSE NO MEASURES
ccc     IF(ISTA.EQ.2)  GO TO 4000

        write(777,7701) NAME(ISTA)
 7701   FORMAT(/1X,' STATISTICS AT :',A12/)
        write(777,7702)
 7702   format('NTOT  XMAX XMEAN STDEVX YMAX'
     .         ' YMEAN STDEVY RMSE  SID'
     .         ' SLOPS SLOP1 SLOPE SLOPEQ'/)

C       MAKE PLOT FOR EACH EXPERIMENTAL RUN AT ONE LOCATION

ccc     DO 4100 IRUN=1,1
        DO 4100 IRUN=1,NRUNS

        ITI = 0

C       PREPARE DATA TO BE PLOTTED
        DO 4200 IT=1,IDT
        
ccc     IF(YMEAS(IT,1,ISTA).LE.0.0) GO TO 4200
        IF(IRUN.EQ.1) THEN
                      IF(YMOD0(IT,1,ISTA).LE.0.0) GO TO 4200
                      ITI = ITI+1
                      AMODEL = YMOD0(IT,1,ISTA)
                      END IF
        IF(IRUN.EQ.2) THEN
                      IF(YMOD1(IT,1,ISTA).LE.0.0) GO TO 4200
                      ITI = ITI+1
                      AMODEL = YMOD1(IT,1,ISTA)
                      END IF
        IF(IRUN.EQ.3) THEN
                      IF(YMOD2(IT,1,ISTA).LE.0.0) GO TO 4200
                      ITI = ITI+1
                      AMODEL = YMOD2(IT,1,ISTA)
                      END IF
        IF(IRUN.EQ.4) THEN
                      IF(YMOD3(IT,1,ISTA).LE.0.0) GO TO 4200
                      ITI = ITI+1
                      AMODEL = YMOD3(IT,1,ISTA)
                      END IF
        XME(ITI) = YMOD0(IT,1,ISTA)
        YMO(ITI) = AMODEL
 4200   CONTINUE


C       PLOTS ARE DONE IN SUB SCATPL -

        CALL SCATPL(IRUN,XME,YMO,ITI)

 4100   CONTINUE

C       PLOT FIGURE CAPTION
        IRUN = 5
        CALL SCATPL(IRUN,XME,YMO,10)

 4000   CONTINUE

C       CLOSE MAGICS
        CALL PCLOSE

        STOP  
        END
C ====================================================================== 
C
        SUBROUTINE SCATPL(IRUN,XME,YMO,ITI)
C
C ----------------------------------------------------------------------
C
C      ROUTINE SCATPL TO PLOT SCATTER DIAGRAM AND STATISTICS
C
C      L. BERTOTTI    ECMWF/ISDGM    JULY 1998
C      J-R BIDLOT     ECMWF          JANUARY 1997 : BINNED DATA PLOTS
C
C     PURPOSE.
C     --------
C        makes plot of scatter diagrams and corresponding statistics
C
C**   INTERFACE.
C     ----------
C     UNIT.        FILE NAME. &  PURPOSE.
C     -----        ----------------------
C     jpuin  = 5  input unit for all MAGICS parameters.
C     jpuso  = 6, printer output.
C
C**   LIBRARIES.
C     ----------
C       $MAGLIB.
C       $ECLIB.
C
C**   EXTERNALS.
C     ----------
C       PCLOSE     TERMINATES MAGICS.
C       PENQ??     MAGICS PARAMETER ENQUIRIES.
C       POPEN      STARTS MAGICS.
C       PNEW       NEW MAGICS PAGE.
C       PSET??     MAGICS PARAMETER SETTINGS.
C       URAOPC     SETS OUTPUT SWITCHES.
C
C     METHOD.
C     -------
C       MODEL FIELDS AND SATELLITE DATA ARE READ AND PLOTTED IN
C       VARIOUS WAYS DEPENDING WHAT IS GIVEN AS USER INPUT.
C
C     REFERENCE.
C     ----------
C       NONE.
C
C ======================================================================
C
C* 0.    DEFINITIONS.
C  ------------------
C
      PARAMETER ( jpuso = 6, 
     .            JPCU  = 2,
     .            JPPAR = 2 )

      LOGICAL lobel

      CHARACTER *32 ccol
      CHARACTER *60 yofc(3)

      REAL XME (iti) ! original data from buoys
      REAL YMO (iti) ! sign. wave height by wam model

      REAL px(jpcu), zx(jpcu)
      REAL py(jpcu), zy(jpcu)

      PARAMETER (nclv= 7)    ! number of marker levels

      real,allocatable :: xsym(:),ysym(:),box(:,:)

      integer icolorlist(nclv), isymt(nclv)
      real clvl(nclv), clvl2(nclv), zsyht(nclv)
      CHARACTER* 20 cl_magcl
      CHARACTER* 20 clscolour(nclv)

      DATA icolorlist / 16, 4, 2, 50,  1, 3, 56 /

C--------------------------------------------------------------------------

C 0.0 CHOOSE BINNED OR NORMAL
C     -----------------------

      ISCATBIN = 0             !  NORMAL
      ISCATBIN = 1             !  BINNED

C 1.0 WRITE FIGURE CAPTION
C     --------------------

      IF(IRUN.EQ.5) GO TO 7000

C 2.0 PREPARE AXIS INFORMATION
C     ------------------------

      ih=0
      XMEMIN=10000.
      XMEMAX=0.
      YMOMIN=10000.
      YMOMAX=0.
      DO 111 IT=1,ITI
          XMEMIN=MIN(XMEMIN,XME(it) )
          XMEMAX=MAX(XMEMAX,XME(it) )
          YMOMIN=MIN(YMOMIN,YMO(it) )
          YMOMAX=MAX(YMOMAX,YMO(it) )
 111  CONTINUE

          zzhsmax=MAX(XMEMAX,YMOMAX )
          zzhsmin=MIN(XMEMIN,YMOMIN )

      plomin = 0.
      plomax = 2.
      if(zzhsmax.gt.2.) plomax = 4.
      if(zzhsmax.gt.4.) plomax = 6.
      if(zzhsmax.gt.6.) plomax = 8.
      if(zzhsmax.gt.8.) plomax = 10.
      if(zzhsmax.gt.10.) plomax = 12.
      if(zzhsmax.gt.12.) plomax = 24.
      plomax = 10.
      plomax = 6.

      nh=iti
      
C 2.1 MAKE PLOT OF X AND Y AXIS
C     -------------------------

      CALL PNEW ('PAGE')
      CALL PLOT_LAYOUT(6,1,IRUN)
      CALL AXIS_PLOTTING(6,1,plomin,plomax,plomin,IRUN)
      CALL PAXIS
      CALL AXIS_PLOTTING(6,2,plomin,plomax,plomin,IRUN)
      CALL PAXIS


C 3.0 SCATTER PLOT WITH BINNED DATA
C     -----------------------------

 3000 CONTINUE
      IF(ISCATBIN.EQ.0) GO TO 3500             !  NORMAL

c **** LEGEND
      NDIM = ITI

c **** find position of the subpage and locate the legend to the right of if
      CALL PENQR('SUBPAGE_X_POSITION',XPOS)
      CALL PENQR('SUBPAGE_Y_POSITION',YPOS)
      CALL PENQR('SUBPAGE_X_LENGTH',XLEN)
      CALL PENQR('SUBPAGE_Y_LENGTH',YLEN)
      XLEGPOS=XPOS+XLEN+0.02
      YLEGPOS=YPOS+3.5
      XLEGLEN=2.3
      YLEGLEN=3.5

      if(ndim.gt.0) then
        CALL psetc ('LEGEND',                          'ON' )
      else
        CALL psetc ('LEGEND',                         'OFF' )
      endif
      CALL psetc ('LEGEND_BORDER',                  'OFF' )
      CALL psetc ('LEGEND_BOX_MODE',         'POSITIONAL' )
      CALL psetr ('LEGEND_BOX_X_POSITION',       XLEGPOS  )
      CALL psetr ('LEGEND_BOX_Y_POSITION',       YLEGPOS  )
      CALL psetr ('LEGEND_BOX_X_LENGTH',         XLEGLEN  )
      CALL psetr ('LEGEND_BOX_Y_LENGTH',         YLEGLEN  )
      CALL psetc ('LEGEND_TITLE',                    'ON' )
      CALL psetc ('LEGEND_TITLE_TEXT',         'ENTRIES:' )
      CALL pseti ('LEGEND_COLUMN_COUNT',               1  )
      CALL psetc ('LEGEND_TEXT_COLOUR',           'BLACK' )
cIR   LEGEND_TEXT_FORMAT: to get the legend boxes spaced correctly with the text
      CALL psetc ('LEGEND_TEXT_FORMAT',            '(I6)' )
c      if(IRUN.le.3) then
c        CALL psetc ('LEGEND_TEXT_FORMAT',     '(AUTOMATIC)' )
c      else
c        CALL psetc ('LEGEND_TEXT_FORMAT',            '(I5)' )
c      end if

c ***** SELECT BIN SIZE
        binsize=.5                       !  FOR WIND
        binsize=.30                      !  FOR WAVE
        binsize=.20                      !  FOR WAVE

c ***** bin the data
        zxmax = plomax
        zxmin = plomin
        zymax = plomax
        zymin = plomin
        nxbox=nint((zxmax-zxmin)/binsize)+1
        nybox=nint((zymax-zymin)/binsize)+1
cccc   allocate(xsym(nxbox*nybox),ysym(nxbox*nybox),box(nxbox,nybox))
cccc    call prep_box(xme,ymo,ndim,zxmin,zymin,binsize,xsym,ysym,
cccc .                box,nxbox,nybox,boxmax)

        nbox=nint(zxmax/binsize)+1
        allocate(xsym(nbox*nbox),ysym(nbox*nbox),box(nbox,nbox))
        call prep_box(xme,ymo,ndim,binsize,xsym,ysym,box,nbox,boxmax)

c ***** determine the boundaries of the bins
        bmax=149
        boxmax=(int(boxmax/bmax)+1)*bmax+int(boxmax/bmax)
        dlog=log10(boxmax)/nclv
           clvl2(nclv)=1+boxmax
        do iclv=1,nclv-1
           clvl2(iclv)= 1+int(10**(iclv*dlog))
        enddo
           clvl(1)= 1
        do iclv=2,nclv
           clvl(iclv)= clvl2(iclv-1)
        enddo

ccc     do iclv=1,nclv
ccc        write(6,*) clvl(iclv),clvl2(iclv)
ccc     enddo

        nin=nxbox*nybox
        CALL psetc ('SYMBOL_POSITION_MODE', 'PAPER' )
c ***** correct for coordinates in cm to graph coord.
        CALL PENQR('SUBPAGE_X_LENGTH',XLEN)
        xsym=xsym*XLEN/zxmax
        ysym=ysym*XLEN/(zymax-zymin)

c ***** define symbol's attributes
        CALL pset1r('SYMBOL_INPUT_X_POSITION', xsym,nin )
        CALL pset1r('SYMBOL_INPUT_Y_POSITION', ysym,nin )
        CALL psetc ('SYMBOL_QUALITY',   'HIGH' )
        CALL psetc ('SYMBOL_TABLE_MODE',  'ON' )
        CALL psetc ('SYMBOL_TYPE',    'MARKER' )
        CALL pset1r('SYMBOL_MIN_TABLE',clvl,nclv)
        CALL pset1r('SYMBOL_MAX_TABLE',clvl2,nclv)
        CALL pset1r('SYMBOL_INPUT_NUMBER_LIST',box,nin)

c ***** find symbol size
        CALL PENQR('SUBPAGE_X_LENGTH',XLEN)
        znu = zxmax/binsize
        zsysi=XLEN/znu

        DO jlev=1,nclv
          clscolour(jlev)=cl_magcl(icolorlist(jlev))
          isymt(jlev)=18
          zsyht(jlev)=zsysi
c          zsyht(jlev)=0.3
        ENDDO
        CALL pset1i('SYMBOL_MARKER_TABLE',isymt,nclv)
        CALL pset1c('SYMBOL_COLOUR_TABLE',clscolour,nclv)
        CALL pset1r('SYMBOL_HEIGHT_TABLE',zsyht,nclv)
c ***** plot box data
        CALL PSYMB
c
c ***** limit legend plotting to box symbols
        CALL psetc ('LEGEND','OFF' )

        deallocate (xsym,ysym,box)

        IF(ISCATBIN.EQ.1) GO TO 4000             !  BINNED

C 3.5 SCATTER PLOTS OF (X,Y) POINTS
C     -----------------------------

 3500 CONTINUE

      CALL GRAPH_PLOTTING(6,1)
      CALL PSET1R('GRAPH_CURVE_X_VALUES',XME,nh)
      CALL PSET1R('GRAPH_CURVE_Y_VALUES',YMO,nh)
      CALL PGRAPH


C 4.0 MAKE PLOT OF 45 DEG AND BEST FIT LINES
C     --------------------------------------
C

 4000 CONTINUE

      PX(1)=0.
      PX(1)=plomin
      PX(2)=plomax
      PY(1)=0.
      PY(1)=plomin
      PY(2)=plomax
      CALL GRAPH_PLOTTING(6,2)
      CALL PSET1R('GRAPH_CURVE_X_VALUES',PX,2)
      CALL PSET1R('GRAPH_CURVE_Y_VALUES',PY,2)
      CALL PGRAPH

      CALL STATS (6, XME, YMO, nh, nh, 1,
     1            XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2            DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3            SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4            SLOPE2, RINTR2, RMSES2, RMSEU2)

c     zx(1)=0.0
c     zy(1)=zx(1)*SLOPE2+RINTR2
c     zx(2)=plomax
c     zy(2)=zx(2)*SLOPE2+RINTR2
c     if(zy(2).GT.plomax) then
c                 zx(2)=(plomax-RINTR2)/SLOPE2
c                 zy(2)=plomax
c                 end if

      zx(1)=0.0
      zx(1)=plomin
      zy(1)=0.0
      zy(1)=plomin
      zx(2)=plomax
cccc      zy(2)=zx(2)*SLOPE1
cccc      if(SLOPE1.gt.1.0) then
cccc                  zx(2)=plomax/SLOPE1
cccc                  zy(2)=plomax
cccc                  end if
      zy(2)=zx(2)*SLOPES
      if(SLOPES.gt.1.0) then
                  zx(2)=plomax/SLOPES
                  zy(2)=plomax
                  end if

      CALL GRAPH_PLOTTING(6,3)
      CALL PSET1R('GRAPH_CURVE_X_VALUES',zx,2)
      CALL PSET1R('GRAPH_CURVE_Y_VALUES',zy,2)
      CALL PGRAPH
 
C 5.0 WRITE TEXT INSIDE THE PLOT.
C     --------------------------

      CALL PSETC ('TEXT_BORDER','OFF')
      CALL PSETC ('TEXT_COLOUR', 'BLACK')
      CALL PSETC ('TEXT_MODE', 'POSITIONAL')
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PSETR ('TEXT_BOX_X_LENGTH', 1.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH', 0.5)
      CALL PSETI ('TEXT_LINE_COUNT', 1)
      CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1', 1.0)
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.25)

      CALL PSETR ('TEXT_BOX_X_POSITION', 1.5)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 7.0)
      IF(IRUN.EQ.1) CALL PSETC ('TEXT_LINE_1', ' AN ')
      IF(IRUN.EQ.2) CALL PSETC ('TEXT_LINE_1', ' F1 ')
      IF(IRUN.EQ.3) CALL PSETC ('TEXT_LINE_1', ' F2 ')
      IF(IRUN.EQ.4) CALL PSETC ('TEXT_LINE_1', ' F3 ')

      CALL PTEXT

      CALL PSETR ('TEXT_BOX_X_POSITION', 6.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 2.0)
      CALL PSETC ('TEXT_LINE_1', ' AN ')

      CALL PTEXT


C 6.0 PREPARE FOR WAVE STATISTICS TO BE PLOTTED
C     -----------------------------------------
      
 6000 CONTINUE

      CALL PNEW ('PAGE')
      CALL PLOT_LAYOUT(6,2,IRUN)
      lobel=.TRUE.
      CALL PSTATS ( 6, LOBEL, NH, 
     1            XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2            DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3            SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4            SLOPE2, RINTR2, RMSES2, RMSEU2)


C 7.0 FIGURE CAPTION.
C     ---------------

 7000 CONTINUE

      IF(IRUN.EQ.5) THEN

               write(yofc(1),'(''WAVE COMPARISON IN ADRIATIC SEA'')')
               READ (5,1006) yofc(2)
               READ (5,1006) yofc(3)
 1006          FORMAT(a60)
c                   write(yofc(3),'(''WIND'',52X,''WAVE'')')

               CALL PNEW ('PAGE')
               CALL PLOT_LAYOUT(6,3,IRUN)

               CALL PSETC ('TEXT_BOX_BLANKING', 'ON' )
               CALL PSETR ('TEXT_BOX_X_POSITION', 0.0)
               CALL PSETR ('TEXT_BOX_Y_POSITION', 0.0)
               CALL PSETR ('TEXT_BOX_X_LENGTH',  20.0)
               CALL PSETR ('TEXT_BOX_Y_LENGTH',   2.0)
               CALL PSETC ('TEXT_COLOUR',     'BLACK')
               CALL PSETC ('TEXT_MODE',  'POSITIONAL')
               CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
               CALL PSETC ('TEXT_QUALITY','HIGH')
               CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.6)
               CALL PSETI ('TEXT_LINE_COUNT', 4)
               CALL PSETC ('TEXT_LINE_1', yofc(1) )
               CALL PSETC ('TEXT_LINE_2', '     ' )
               CALL PSETC ('TEXT_LINE_3', yofc(2) )
               CALL PSETC ('TEXT_LINE_4', yofc(3) )
               CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1', 1.0)
               CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_2', 1.0)
               CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_3', 1.0)
               CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_4', 1.0)
               CALL PSETC ('TEXT_BORDER', 'OFF')
               CALL PTEXT

C
C*    7.1 END OF PHYSICAL PAGE.
C         ---------------------

               CALL PNEW ('SUPER_PAGE')

                    END IF

      RETURN
      END
C ======================================================================
C
      SUBROUTINE axis_plotting ( kuso, kplt, 
     .                           axis_min_value, axis_max_value ,
     .                           axis_base_date,
     .                           IRUN )

C GENERAL AXIS PARAMETERS.
      REAL      axis_min_value
      REAL      axis_max_value
      CHARACTER axis_orientation*10
      CHARACTER axis_position*6
      DATA axis_orientation / 'HORIZONTAL' /
      DATA axis_position    / 'BOTTOM' /
C AXIS LINE PARAMETERS.
      CHARACTER axis_line*3
      CHARACTER axis_line_colour*30
      INTEGER   axis_line_thickness
      DATA axis_line           / 'ON' /
      DATA axis_line_colour    / 'BLACK' /
      DATA axis_line_thickness / 4 /
C AXIS GRID PARAMETERS.
      CHARACTER axis_grid*3
      CHARACTER axis_grid_colour*30
      INTEGER   axis_grid_thickness
      CHARACTER axis_grid_line_style*10
      DATA axis_grid            / 'OFF' /
      DATA axis_grid_colour     / 'YELLOW' /
      DATA axis_grid_thickness  /    2  /
      DATA axis_grid_line_style / 'DOT' /
C AXIS TITLE PARAMETERS.
      CHARACTER axis_title*3
      CHARACTER axis_title_colour*30
      REAL      axis_title_height
      CHARACTER axis_title_orientation*10
      CHARACTER axis_title_quality*6
      CHARACTER axis_title_text*70
      DATA axis_title             / 'ON' /
      DATA axis_title_colour      / 'BLACK' /
c      DATA axis_title_height      /  0.2 /
      DATA axis_title_height      /  0.3 /
      DATA axis_title_orientation / 'PARALLEL' /
      DATA axis_title_quality     / 'HIGH' /
      DATA axis_title_text        / ' ' /
C AXIS TIP TITLE PARAMETERS.
      CHARACTER axis_tip_title*3
      CHARACTER axis_tip_title_colour*30
      REAL      axis_tip_title_height
      CHARACTER axis_tip_title_orientation*10
      CHARACTER axis_tip_title_quality*6
      CHARACTER axis_tip_title_text*70
      DATA axis_tip_title             / 'OFF' /
      DATA axis_tip_title_colour      / 'BLACK' /
      DATA axis_tip_title_height      /  0.4 /
      DATA axis_tip_title_orientation / 'VERTICAL' /
      DATA axis_tip_title_quality     / 'HIGH' /
      DATA axis_tip_title_text        / ' ' /
C AXIS TICK PARAMETERS.
      CHARACTER axis_tick*3
      CHARACTER axis_tick_colour*30
      REAL      axis_tick_interval
      CHARACTER axis_tick_positioning*13
      INTEGER   axis_tick_positioning_list_len
      PARAMETER (axis_tick_positioning_list_len=100)
      REAL axis_tick_positioning_list(axis_tick_positioning_list_len)
      REAL      axis_tick_size
      INTEGER   axis_tick_thickness
      DATA axis_tick                  / 'OFF'/
      DATA axis_tick_colour           / 'BLACK'/
      DATA axis_tick_interval         /  2.0 /
      DATA axis_tick_positioning      / 'REGULAR'/
      DATA axis_tick_positioning_list /  100*0.0 /
      DATA axis_tick_size             /  0.2 /
      DATA axis_tick_thickness        /  4 /
C AXIS MINOR TICK PARAMETERS.
      CHARACTER axis_minor_tick*3
      CHARACTER axis_minor_tick_colour*30
      INTEGER   axis_minor_tick_count
      REAL      axis_minor_tick_min_gap
      REAL      axis_minor_tick_size
      INTEGER   axis_minor_tick_thickness
      DATA axis_minor_tick           / 'OFF'/
      DATA axis_minor_tick_colour    / 'BLACK'/
      DATA axis_minor_tick_interval  /  1.0 /
      DATA axis_minor_tick_count     /  1 /
      DATA axis_minor_tick_min_gap   /  0.1 /
      DATA axis_minor_tick_size      /  0.1 /
      DATA axis_minor_tick_thickness /  1 /
C AXIS LABEL PARAMETERS.
      CHARACTER axis_tick_label*3
      CHARACTER axis_tick_label_colour*30
      CHARACTER axis_tick_label_first*3
      CHARACTER axis_tick_label_format*9
      INTEGER   axis_tick_label_frequency
      REAL      axis_tick_label_height
      CHARACTER axis_tick_label_last*3
      CHARACTER axis_tick_label_list(10)*5
      CHARACTER axis_tick_label_orientation*10
      CHARACTER axis_tick_label_position*10
      CHARACTER axis_tick_label_quality*6
      CHARACTER axis_tick_label_type*10
      DATA axis_tick_label             / 'ON' /
      DATA axis_tick_label_colour      / 'BLACK' /
      DATA axis_tick_label_first       / 'ON' /
      DATA axis_tick_label_format      / 'AUTOMATIC' /
      DATA axis_tick_label_frequency   /  1  /
      DATA axis_tick_label_height      /  0.25 /
      DATA axis_tick_label_last        / 'ON' /
      DATA axis_tick_label_list        / 10*'  ' /
      DATA axis_tick_label_orientation / 'HORIZONTAL' /
      DATA axis_tick_label_position    / 'ON_TICK' /
      DATA axis_tick_label_quality     / 'HIGH' /
      DATA axis_tick_label_type        / 'NUMBER' /
C AXIS TIME PARAMETERS.
      REAL      axis_base_date 
      CHARACTER axis_hours_label*3
      CHARACTER axis_hours_label_colour*30
      CHARACTER axis_hours_label_quality*6
      REAL      axis_hours_label_height
      CHARACTER axis_days_label*6
      CHARACTER axis_days_label_colour*30
      CHARACTER axis_days_label_quality*6
      REAL      axis_days_label_height
      CHARACTER axis_days_label_composition*5
      CHARACTER axis_months_label*3
      CHARACTER axis_months_label_colour*30
      CHARACTER axis_months_label_quality*6
      REAL      axis_months_label_height
      CHARACTER axis_months_label_composition*5
      CHARACTER axis_years_label*3
      CHARACTER axis_years_label_colour*30
      CHARACTER axis_years_label_quality*6
      REAL      axis_years_label_height
C-----DATA axis_base_date ! = axis_min_value
      DATA axis_hours_label               / 'OFF' /
      DATA axis_hours_label_colour        / 'BLACK' /
      DATA axis_hours_label_quality       / 'MEDIUM' /
      DATA axis_hours_label_height        /  0.2 /
      DATA axis_days_label                / 'OFF' /
      DATA axis_days_label_colour         / 'BLACK' /
      DATA axis_days_label_quality        / 'LOW' /
      DATA axis_days_label_height         /  0.2 /
      DATA axis_days_label_composition    / 'ONE' /
      DATA axis_months_label              / 'ON' /
      DATA axis_months_label_colour       / 'BLACK' /
      DATA axis_months_label_quality      / 'LOW' /
      DATA axis_months_label_height       /  0.2 /
      DATA axis_months_label_composition  / 'FULL' /
      DATA axis_years_label               / 'ON' /
      DATA axis_years_label_colour        / 'BLACK' /
      DATA axis_years_label_quality       / 'LOW' /
      DATA axis_years_label_height        /  0.2 /
C ----------------------------------------------------------------------
C
C* 0.    COMMON OUTPUT CONTROL.
C  ----------------------------
C
      PARAMETER ( jpctl = 20 )
      CHARACTER*7 ctest, csuvi
      COMMON /output/ lsub,ltest,lsuvi,ctest(jpctl),csuvi(jpctl)
      LOGICAL lotest, losuvi
      CHARACTER*7 csubna
C
C        *LSUB*  CURRENT SUB-ROUTINE LEVEL
C        *LTEST* MAXIMUM LEVEL WITH TEST OUTPUT
C        *LSUVI* MAXIMUM LEVEL WITH STANDARD OUTPUT
C        *CTEST* ARRAY OF SUB-ROUTINE NAMES GIVING TEST OUTPUT
C        *CSUVI* ARRAY OF SUB-ROUTINE NAMES GIVING STANDARD OUTPUT
C
      DATA csubna /"axis_pl"/
C
C ----------------------------------------------------------------------
C
C*    1. INITIALIZATIONS.
C        ----------------
 1000 CONTINUE
      lsub = lsub + 1
      CALL  uraopc (csubna, lsub, losuvi, lotest)
C
C ----------------------------------------------------------------------
C
      IF ( losuvi .OR. lotest ) THEN
          WRITE(102,1001)
 1001     FORMAT('MPPT2 - Subroutine:  AXIS_PLOTTING ')
      ENDIF

      CALL PENQC ('SUBPAGE_FRAME_COLOUR',axis_line_colour)
      axis_grid_colour=axis_line_colour
      axis_title_colour=axis_line_colour
      axis_tip_title_colour=axis_line_colour
      axis_tick_colour=axis_line_colour
      axis_tick_labeL_colour=axis_line_colour
      axis_minor_tick_colour=axis_line_colour
      IF (axis_orientation.EQ.'HORIZONTAL') THEN
         CALL PENQR ('SUBPAGE_X_LENGTH',axis_length )
      ELSEIF (axis_orientation.EQ.'VERTICAL  ') THEN
         CALL PENQR ('SUBPAGE_Y_LENGTH',axis_length )
      ELSE
         WRITE(102,*)' NO axis_orientation '
         WRITE(102,*)' AXIS_ORIENTATION=',axis_orientation
         CLOSE (102)
         CALL abort
      ENDIF
      IF (lotest) WRITE(kuso,*)' axis_length= ',axis_length
C
C ----------------------------------------------------------------------
C
C*    2. SELECT AXIS ORIENTATION
C        -----------------------
 2000 CONTINUE

C     HORIZONTAL AXIS
      IF(KPLT.EQ.1) THEN
               axis_orientation = 'HORIZONTAL'
               axis_position    = 'BOTTOM'
cccc           axis_title_text  = 'H\SB\S\SBR\ (>m)    <BUOY'
cccc           axis_title_text  = 'H\SB\S\SBR\ (>m)    <MODEL'
cccc           axis_title_text  = 'HS (>m)    <BUOY'
cIR   < and > are not supported in Magics++ for converting character case.
cIR   it is also redundant in this case.
cIR               axis_title_text  = 'HS (>m)    <MODEL'
               axis_title_text  = 'HS (m)    MODEL'
                    END IF

C     VERTICAL AXIS
      IF(KPLT.EQ.2) THEN
               axis_orientation = 'VERTICAL'
               axis_position    = 'LEFT'
cccc           IF(IRUN.EQ.1) THEN
cccc                   axis_title_text  = 'H\SB\S\SBR\ (>m)    <BUOY'
cccc                   axis_title_text  = 'HS (>m)    <BUOY'
cccc                   ELSE
cccc                   axis_title_text  = 'H\SB\S\SBR\ (>m)    <MODEL'
cIR   < and > are not supported in Magics++ for converting character case.
cIR   it is also redundant in this case.
cIR                       axis_title_text  = 'HS (>m)    <MODEL'
                       axis_title_text  = 'HS (m)    MODEL'
cccc                   END IF
                    END IF
C
      i=0
      itick_label = 0
C
C ----------------------------------------------------------------------
C
C*    3. MAGICS PARAMETER SEETINGS.
C        --------------------------
 3000 CONTINUE

      CALL PSETC ('AXIS_ORIENTATION',axis_orientation)
      axis_tick_interval= 0.5
      if(axis_max_value.gt. 2.) axis_tick_interval=1.0
      if(axis_max_value.gt. 5.) axis_tick_interval=2.0
      if(axis_max_value.gt.12.) axis_tick_interval=4.0
      CALL PSETR ('AXIS_MIN_VALUE',axis_min_value)
      CALL PSETR ('AXIS_MAX_VALUE',axis_max_value)
      CALL PSETR ('AXIS_TITLE_HEIGHT',axis_title_height)
      CALL PSETR ('AXIS_TIP_TITLE_HEIGHT',axis_tip_title_height)
      CALL PSETR ('AXIS_TICK_INTERVAL',axis_tick_interval)
      IF ( i .GT. 0 )
     .CALL PSET1R ('AXIS_TICK_POSITIONING_LIST',
     .             axis_tick_positioning_list(i))
      CALL PSETR ('AXIS_TICK_SIZE',axis_tick_size)
      CALL PSETR ('AXIS_MINOR_TICK_MIN_GAP',axis_minor_tick_min_gap)
      CALL PSETR ('AXIS_MINOR_TICK_SIZE',axis_minor_tick_size)
      CALL PSETC ('AXIS_TICK_LABEL', axis_tick_label)
      IF (lotest) 
     .WRITE(102,*) ' GRAPH PLOTTING atlh= ',axis_tick_label_height
      CALL PSETR ('AXIS_TICK_LABEL_HEIGHT',axis_tick_label_height)
      IF (lotest) THEN
         CALL PENQR ('AXIS_TICK_LABEL_HEIGHT',atlh)
         WRITE(102,*) ' GRAPH PLOTTING atlh= ',atlh
      ENDIF
      IF (lotest) THEN
         WRITE(kuso,*) ' AXIS_BASE_DATE: ',axis_base_date
         WRITE(kuso,*) ' AXIS_TICK_POSITIONING: >',
     .                   axis_tick_positioning,'<'
      ENDIF
      IF (axis_tick_positioning.eq.'HOURS        ' .OR.
     .    axis_tick_positioning.eq.'DAYS         ' .OR.
     .    axis_tick_positioning.eq.'MONTHS       ' .OR.
     .    axis_tick_positioning.eq.'YEARS        ' ) THEN
         CALL PSETR ('AXIS_BASE_DATE',axis_base_date)
         IF (lotest) 
     .   WRITE(kuso,*) ' TIME AXIS TIME AXIS TIME AXIS TIME AXIS TIME '
      ENDIF
      CALL PSETR ('AXIS_HOURS_LABEL_HEIGHT',axis_hours_label_height)
      CALL PSETR ('AXIS_DAYS_LABEL_HEIGHT',axis_days_label_height)
      CALL PSETR ('AXIS_MONTHS_LABEL_HEIGHT',axis_months_label_height)
      CALL PSETR ('AXIS_YEARS_LABEL_HEIGHT',axis_years_label_height)
      CALL PSETI ('AXIS_LINE_THICKNESS',axis_line_thickness)
      CALL PSETI ('AXIS_GRID_THICKNESS',axis_grid_thickness)
      CALL PSETI ('AXIS_TICK_THICKNESS',axis_tick_thickness)
      CALL PSETI ('AXIS_MINOR_TICK_COUNT',axis_minor_tick_count)
      CALL PSETI ('AXIS_MINOR_TICK_THICKNESS',axis_minor_tick_thickness)
      CALL PSETI ('AXIS_TICK_LABEL_FREQUENCY',axis_tick_label_frequency)
      CALL PSETC ('AXIS_POSITION',axis_position)
      CALL PSETC ('AXIS_LINE',axis_line)
      CALL PSETC ('AXIS_LINE_COLOUR', axis_line_colour)
      CALL PSETC ('AXIS_GRID', axis_grid)
      CALL PSETC ('AXIS_GRID_COLOUR', axis_grid_colour)
      CALL PSETC ('AXIS_GRID_LINE_STYLE', axis_grid_line_style)
      CALL PSETC ('AXIS_TITLE', axis_title)
      CALL PSETC ('AXIS_TITLE_COLOUR', axis_title_colour)
      CALL PSETC ('AXIS_TITLE_ORIENTATION', axis_title_orientation)
      CALL PSETC ('AXIS_TITLE_QUALITY', axis_title_quality)
      CALL PSETC ('AXIS_TITLE_TEXT', axis_title_text)
      CALL PSETC ('AXIS_TIP_TITLE', axis_tip_title)
      CALL PSETC ('AXIS_TIP_TITLE_COLOUR', axis_tip_title_colour)
      CALL PSETC ('AXIS_TIP_TITLE_ORIENTATION',
     .             axis_tip_title_orientation)
      CALL PSETC ('AXIS_TIP_TITLE_QUALITY', axis_tip_title_quality)
      CALL PSETC ('AXIS_TIP_TITLE_TEXT', axis_tip_title_text)
      CALL PSETC ('AXIS_TICK', axis_tick)
      CALL PSETC ('AXIS_TICK_COLOUR', axis_tick_colour)
      CALL PSETC ('AXIS_TICK_POSITIONING', axis_tick_positioning)
      CALL PSETC ('AXIS_MINOR_TICK', axis_minor_tick)
      CALL PSETC ('AXIS_MINOR_TICK_COLOUR', axis_minor_tick_colour)
      CALL PSETC ('AXIS_TICK_LABEL_COLOUR', axis_tick_label_colour)
      CALL PSETC ('AXIS_TICK_LABEL_FIRST', axis_tick_label_first)
cIR      Magics++ does not understand AXIS_TICK_LABEL_FORMAT=AUTOMATIC
cIR      CALL PSETC ('AXIS_TICK_LABEL_FORMAT', axis_tick_label_format)
      CALL PSETC ('AXIS_TICK_LABEL_LAST', axis_tick_label_last)
      IF ( itick_label .GT. 0 )
     .CALL PSETC ('AXIS_TICK_LABEL_LIST',   
     .             axis_tick_label_list(itick_label))          
      CALL PSETC ('AXIS_TICK_LABEL_ORIENTATION',
     .             axis_tick_label_orientation)
      CALL PSETC ('AXIS_TICK_LABEL_POSITION', axis_tick_label_position)
      CALL PSETC ('AXIS_TICK_LABEL_QUALITY', axis_tick_label_quality)
      CALL PSETC ('AXIS_TICK_LABEL_TYPE', axis_tick_label_type)
      CALL PSETC ('AXIS_HOURS_LABEL', axis_hours_label)
      CALL PSETC ('AXIS_HOURS_LABEL_COLOUR', axis_hours_label_colour)
      CALL PSETC ('AXIS_HOURS_LABEL_QUALITY', axis_hours_label_quality)
      CALL PSETC ('AXIS_DAYS_LABEL', axis_days_label)
      CALL PSETC ('AXIS_DAYS_LABEL_COLOUR', axis_days_label_colour)
      CALL PSETC ('AXIS_DAYS_LABEL_QUALITY', axis_days_label_quality)
      CALL PSETC ('AXIS_DAYS_LABEL_COMPOSITION',
     .             axis_days_label_composition)
      CALL PSETC ('AXIS_MONTHS_LABEL', axis_months_label)
      CALL PSETC ('AXIS_MONTHS_LABEL_COLOUR', axis_months_label_colour)
      CALL PSETC ('AXIS_MONTHS_LABEL_QUALITY',
     .             axis_months_label_quality)
      CALL PSETC ('AXIS_MONTHS_LABEL_COMPOSITION', 
     .             axis_months_label_composition)
      CALL PSETC ('AXIS_YEARS_LABEL', axis_years_label)
      CALL PSETC ('AXIS_YEARS_LABEL_COLOUR', axis_years_label_colour)
      CALL PSETC ('AXIS_YEARS_LABEL_QUALITY', axis_years_label_quality)
      lsub = lsub - 1
      RETURN
      END
C ======================================================================
C
C
      SUBROUTINE graph_plotting (kuso, kplt)
C    .                           kdim, k2dim, kbar,
C    .                           graph_curve_x_value,
C    .                           graph_curve_y_value,
C    .                           graph_curve2_x_value,
C    .                           graph_curve2_y_value,
C    .                           graph_bar_x_values,
C    .                           graph_bar_y_lower_values,
C    .                           graph_bar_y_upper_values)
      CHARACTER graph_type*5
C     REAL      graph_curve_x_value(kdim)
C     REAL      graph_curve_y_value(kdim)
C     REAL      graph_curve2_x_value(k2dim)
C     REAL      graph_curve2_y_value(k2dim)
C     REAL      graph_bar_x_values(kbar)
C     REAL      graph_bar_y_lower_values(kbar)
C     REAL      graph_bar_y_upper_values(kbar)
      CHARACTER graph_curve_method*8
      CHARACTER graph_curve_interpolation*6
      INTEGER   graph_smoothing_factor
      CHARACTER graph_line*3
      CHARACTER graph_line_style*10
      CHARACTER graph_line_colour*30
      INTEGER   graph_line_thickness
      CHARACTER graph_symbol*12
      INTEGER   graph_symbol_marker_index
      REAL      graph_symbol_height
      CHARACTER graph_symbol_colour*30
      CHARACTER graph_missing_data_mode*6
      CHARACTER graph_missing_data_style*10
      CHARACTER graph_missing_data_colour*30
      INTEGER   graph_missing_data_thickness
      REAL      graph_bar_width
      CHARACTER graph_bar_line_style*10
      INTEGER   graph_bar_line_thickness
      CHARACTER graph_bar_line_colour*30
      CHARACTER graph_shade*3
      CHARACTER graph_shade_style*10
      CHARACTER graph_shade_colour*30
      INTEGER   graph_shade_density
      CHARACTER graph_bar_annotation_position*6
      CHARACTER graph_blanking*3
      REAL      graph_blanking_gap
      REAL      graph_x_suppress_above
      REAL      graph_x_suppress_below
      REAL      graph_y_suppress_above
      REAL      graph_y_suppress_below
      
      DATA graph_type                    / 'CURVE' /
      DATA graph_curve_method            / 'STRAIGHT' /
      DATA graph_curve_interpolation     / 'SPLINE' /
      DATA graph_smoothing_factor        /  3 /
      DATA graph_line                    / 'ON ' /
      DATA graph_line_style              / 'SOLID' /
      DATA graph_line_colour             / 'BLUE' /
      DATA graph_line_thickness          /  1 /
      DATA graph_symbol                  / 'OFF' /
      DATA graph_symbol_marker_index     /  3 /
      DATA graph_symbol_height           /  0.2 /
      DATA graph_symbol_colour           / 'RED' /
      DATA graph_missing_data_mode       / 'IGNORE' /
      DATA graph_missing_data_style      / 'DASH' /
      DATA graph_missing_data_colour     / 'RED' /
      DATA graph_missing_data_thickness  /  1 /
      DATA graph_bar_width               /  0.0 /
      DATA graph_bar_line_style          / 'SOLID' /
      DATA graph_bar_line_thickness      /  1 /
      DATA graph_bar_line_colour         / 'BLUE' /
      DATA graph_shade                   / 'ON' /
      DATA graph_shade_style             / 'DOT' /
      DATA graph_shade_colour            / 'BLUE' /
      DATA graph_shade_density           /  20 /
      DATA graph_bar_annotation_position / 'TOP' /
      DATA graph_blanking                / 'ON ' /
      DATA graph_blanking_gap            /  0.2  /
      DATA graph_x_suppress_above        /  21.E6 /
      DATA graph_x_suppress_below        / -21.E6 /
      DATA graph_y_suppress_above        /  21.E6 /
      DATA graph_y_suppress_below        / -21.E6 /
C USER INPUT.
      CHARACTER youser*120
C ----------------------------------------------------------------------
C
C* 0.    COMMON OUTPUT CONTROL.
C  ----------------------------
C
      PARAMETER ( jpctl = 20 )
      CHARACTER*7 ctest, csuvi
      COMMON /output/ lsub,ltest,lsuvi,ctest(jpctl),csuvi(jpctl)
      LOGICAL lotest, losuvi
      CHARACTER*7 csubna
C
C        *LSUB*  CURRENT SUB-ROUTINE LEVEL
C        *LTEST* MAXIMUM LEVEL WITH TEST OUTPUT
C        *LSUVI* MAXIMUM LEVEL WITH STANDARD OUTPUT
C        *CTEST* ARRAY OF SUB-ROUTINE NAMES GIVING TEST OUTPUT
C        *CSUVI* ARRAY OF SUB-ROUTINE NAMES GIVING STANDARD OUTPUT
C
      DATA csubna /"graph_p"/
C
C ----------------------------------------------------------------------
C
C*    1. INITIALIZATIONS.
C        ----------------
 1000 CONTINUE
      lsub = lsub + 1
      CALL  uraopc (csubna, lsub, losuvi, lotest)
      IF (losuvi .OR. lotest) THEN
        WRITE(102,1001)
 1001   FORMAT('MPPT2 - Subroutine:  GRAPH_PLOTTING ')
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    2. LOOP OVER USER INPUT.
C        ---------------------
 2000 CONTINUE
C
      CALL PENQR ('GRAPH_BAR_WIDTH',graph_bar_width_m)
      graph_bar_width=graph_bar_width_m

C     SELECT GRAPH-PLOTTING

      IF(KPLT.EQ.1) THEN
                    graph_symbol = 'SYMBOLS_ONLY'
                    END IF
      IF(KPLT.EQ.2) THEN
                    graph_symbol       = 'OFF'
                    graph_line_colour  = 'BLACK'
                    graph_line_style   = 'DASH'
                    graph_blanking_gap =  0.01
                    END IF
      IF(KPLT.EQ.3) THEN
                    graph_line_style   = 'SOLID'
                    END IF
C
C ----------------------------------------------------------------------
C
C*    3. MAGICS PARAMETER SEETINGS IF A MAP IS TO BE PLOTTED.
C        ----------------------------------------------------
 3000 CONTINUE
      CALL PSETC ('GRAPH_LINE_STYLE',graph_line_style)
      CALL PSETC ('GRAPH_LINE_COLOUR',graph_line_colour)
      CALL PSETI ('GRAPH_LINE_THICKNESS',graph_line_thickness)
C
C     CALL PSETR ('GRAPH_X_SUPPRESS_ABOVE',graph_x_suppress_above)
C     CALL PSETR ('GRAPH_X_SUPPRESS_BELOW',graph_x_suppress_below)
C     CALL PSETR ('GRAPH_Y_SUPPRESS_ABOVE',graph_y_suppress_above)
C     CALL PSETR ('GRAPH_Y_SUPPRESS_BELOW',graph_y_suppress_below)

      CALL PENQR ('GRAPH_X_SUPPRESS_ABOVE',x_suppress_above)
      CALL PENQR ('GRAPH_X_SUPPRESS_BELOW',x_suppress_below)
      CALL PENQR ('GRAPH_Y_SUPPRESS_ABOVE',y_suppress_above)
      CALL PENQR ('GRAPH_Y_SUPPRESS_BELOW',y_suppress_below)

      IF (losuvi.OR.lotest) THEN
         WRITE(102,*)'GRAPH_X_SUPPRESS_ABOVE: ',x_suppress_above 
         WRITE(102,*)'GRAPH_X_SUPPRESS_BELOW: ',x_suppress_below 
         WRITE(102,*)'GRAPH_Y_SUPPRESS_ABOVE: ',y_suppress_above 
         WRITE(102,*)'GRAPH_Y_SUPPRESS_BELOW: ',y_suppress_below 
      ENDIF

C
      CALL PSETC ('GRAPH_TYPE',graph_type)
      IF (graph_type(1:5).EQ.'CURVE') THEN
C
C     GRAPH TYPE = CURVE.
C     -------------------
         CALL PSETC ('GRAPH_LINE',graph_line)
C
         CALL PSETC ('GRAPH_CURVE_METHOD',graph_curve_method)
         CALL PSETC ('GRAPH_CURVE_INTERPOLATION'
     .               ,graph_curve_interpolation)
         CALL PSETI ('GRAPH_SMOOTHING_FACTOR',graph_smoothing_factor)
C
         CALL PSETC ('GRAPH_SYMBOL',graph_symbol)
         CALL PSETI ('GRAPH_SYMBOL_MARKER_INDEX'
     .               ,graph_symbol_marker_index)
         CALL PSETC ('GRAPH_SYMBOL_COLOUR',graph_symbol_colour)
         CALL PSETR ('GRAPH_SYMBOL_HEIGHT',graph_symbol_height)
C
         CALL PSETC ('GRAPH_MISSING_DATA_MODE',graph_missing_data_mode)
         CALL PSETC ('GRAPH_MISSING_DATA_STYLE',
     .                graph_missing_data_style)
         CALL PSETC ('GRAPH_MISSING_DATA_COLOUR'
     .               ,graph_missing_data_colour)
         CALL PSETI ('GRAPH_MISSING_DATA_THICKNESS'
     .               ,graph_missing_data_thickness)
C
         CALL PSETC ('GRAPH_BLANKING',graph_blanking)
         CALL PSETR ('GRAPH_BLANKING_GAP',graph_blanking_gap)
C
      ELSEIF (graph_type(1:3).EQ.'BAR') THEN
C
C     GRAPH TYPE = BAR                                  .
C     ---------------------------------------------------
         IF ( graph_bar_width_m .NE. graph_bar_width )
     .   CALL PSETR ('GRAPH_BAR_WIDTH',graph_bar_width)
         CALL PSETC ('GRAPH_BAR_LINE_STYLE',graph_bar_line_style)
         CALL PSETI ('GRAPH_BAR_LINE_THICKNESS',
     .                graph_bar_line_thickness)
         CALL PSETC ('GRAPH_BAR_LINE_COLOUR',graph_bar_line_colour)
C
         CALL PSETC ('GRAPH_SHADE',graph_shade)
         CALL PSETC ('GRAPH_SHADE_STYLE',graph_shade_style)
         CALL PSETC ('GRAPH_SHADE_COLOUR',graph_shade_colour)
         CALL PSETI ('GRAPH_SHADE_DENSITY',graph_shade_density)
         CALL PSETC ('GRAPH_BAR_ANNOTATION_POSITION'
     .               ,graph_bar_annotation_position)
C
      ELSEIF (graph_type(1:4).EQ.'AREA' ) THEN
C     GRAPH TYPE = AREA                                 .
C     ---------------------------------------------------
         CALL PSETC ('GRAPH_SHADE',graph_shade)
         CALL PSETC ('GRAPH_CURVE_METHOD',graph_curve_method)
         CALL PSETC ('GRAPH_CURVE_INTERPOLATION'
     .               ,graph_curve_interpolation)
         CALL PSETI ('GRAPH_SMOOTHING_FACTOR',graph_smoothing_factor)
         CALL PSETC ('GRAPH_SHADE_STYLE',graph_shade_style)
         CALL PSETC ('GRAPH_SHADE_COLOUR',graph_shade_colour)
         CALL PSETI ('GRAPH_SHADE_DENSITY',graph_shade_density)
      
      ELSE
         WRITE(102,*)' GRAPH_TYPE: ',graph_type
         WRITE(102,*)' THIS TYPE IS UNKNOWN '
         CLOSE(102)
         CALL abort
      ENDIF
      lsub = lsub - 1
      RETURN
      END
C ======================================================================
C
      SUBROUTINE PLOT_LAYOUT (kuso, kplt,IRUN)


      REAL super_page_x_length
      REAL super_page_y_length
      REAL page_x_gap
      REAL page_y_gap
      REAL page_x_position
      REAL page_y_position
      REAL page_x_length
      REAL page_y_length
      REAL page_id_line_height
      INTEGER page_frame_thickness
      REAL subpage_x_position
      REAL subpage_y_position
      REAL subpage_x_length
      REAL subpage_y_length
      INTEGER subpage_frame_thickness
      REAL subpage_lower_left_longitude
      REAL subpage_lower_left_latitude
      REAL subpage_upper_right_longitude
      REAL subpage_upper_right_latitude
      REAL subpage_map_vertical_longitude
      REAL subpage_map_centre_longitude
      REAL subpage_map_centre_latitude
      REAL subpage_map_x_polar_vector
      REAL subpage_map_y_polar_vector
      REAL subpage_map_scale
      INTEGER map_grid_thickness
      REAL map_grid_latitude_reference
      REAL map_grid_longitude_reference
      REAL map_grid_latitude_increment
      REAL map_grid_longitude_increment
      INTEGER map_label_longitude_frequency
      INTEGER map_label_latitude_frequency
      REAL map_label_height

      DATA super_page_x_length            / 21.0 /
      DATA super_page_y_length            / 29.7 /
      DATA page_x_gap                     / 0.0 /
      DATA page_y_gap                     / 0.0 /
      DATA page_x_position                / 0.0 /
      DATA page_y_position                / 0.0 /
      DATA page_x_length                  / 21.0 /
      DATA page_y_length                  / 29.7 /
      DATA page_id_line_height            / 0.2 /
      DATA page_frame_thickness           / 1   /
      DATA subpage_x_position             / 2.0 /
      DATA subpage_y_position             / 1.0 /
      DATA subpage_x_length               / 24.5 /
      DATA subpage_y_length               / 17.75 /
      DATA subpage_frame_thickness        / 1   /
      DATA subpage_lower_left_longitude   / 0.0 /
      DATA subpage_lower_left_latitude    / 0.0 /
      DATA subpage_upper_right_longitude  / 25.0 /
      DATA subpage_upper_right_latitude   / 25.0 /
      DATA subpage_map_vertical_longitude / 0.0 /
      DATA subpage_map_centre_longitude   / 0.0 /
      DATA subpage_map_centre_latitude    / 90.0 /
      DATA subpage_map_x_polar_vector     / 0.0 /
      DATA subpage_map_y_polar_vector     / 0.0 /
      DATA subpage_map_scale              / 50.0E6 /
      DATA map_grid_thickness             / 1   /
      DATA map_grid_latitude_reference    / 0.0 /
      DATA map_grid_longitude_reference   / 0.0 /
      DATA map_grid_latitude_increment    / 2.0 /
      DATA map_grid_longitude_increment   / 2.0 /
      DATA map_label_longitude_frequency  / 1   /
      DATA map_label_latitude_frequency   / 1   /
      DATA map_label_height               / 0.2 /

      CHARACTER youser*120

      CHARACTER super_page_frame*3
      CHARACTER layout*10
      CHARACTER plot_start*6
      CHARACTER plot_direction*10
      CHARACTER page_frame*3
      CHARACTER page_frame_colour*30
      CHARACTER page_frame_line_style*10
      CHARACTER page_id_line*3
      CHARACTER page_id_line_quality*6
      CHARACTER page_id_line_user_text*80
      CHARACTER subpage_frame*3
      CHARACTER subpage_frame_colour*30
      CHARACTER subpage_frame_line_style*10
      CHARACTER subpage_map_projection*19
      CHARACTER subpage_map_area_definition*12
      CHARACTER subpage_map_hemisphere*5
      CHARACTER map_coastline*3
      CHARACTER map_coastline_resolution*10
      CHARACTER map_coastline_style*10
      CHARACTER map_coastline_colour*30
      CHARACTER map_grid*3
      CHARACTER map_grid_line_style*10
      CHARACTER map_grid_colour*30
      CHARACTER map_label*3
      CHARACTER map_label_quality*6

      DATA super_page_frame             / 'OFF' /
      DATA layout                       / 'POSITIONAL' /
      DATA plot_start                   / 'TOP' /
      DATA plot_direction               / 'HORIZONTAL' /
      DATA page_frame                   / 'OFF' /
      DATA page_frame_colour            / 'BLACK' /
      DATA page_frame_line_style        / 'SOLID' /
      DATA page_id_line                 / 'OFF' /
      DATA page_id_line_quality         / 'HIGH' /
      DATA page_id_line_user_text       / 'B. HANSEN' /
      DATA subpage_frame                / 'ON' /
      DATA subpage_frame_colour         / 'BLACK' /
      DATA subpage_frame_line_style     / 'SOLID' /
C      DATA subpage_map_projection       / 'CYLINDRICAL' /
      DATA subpage_map_projection       / 'NONE' /
      DATA subpage_map_area_definition  / 'CORNERS' /
      DATA subpage_map_hemisphere       / 'NORTH' /
      DATA map_coastline                / 'OFF' /
      DATA map_coastline_resolution     / 'MEDIUM' /
      DATA map_coastline_style          / 'SOLID' /
      DATA map_coastline_colour         / 'GREEN' /
      DATA map_grid                     / 'OFF' /
      DATA map_grid_line_style          / 'DOT' /
      DATA map_grid_colour              / 'BLACK' /
      DATA map_label                    / 'OFF' /
      DATA map_label_quality            / 'HIGH' /
C ----------------------------------------------------------------------
C
C* 0.    COMMON OUTPUT CONTROL.
C  ----------------------------
C
      PARAMETER ( jpctl = 20 )
      CHARACTER*7 ctest, csuvi
      COMMON /output/ lsub,ltest,lsuvi,ctest(jpctl),csuvi(jpctl)
      LOGICAL lotest, losuvi
      CHARACTER*7 csubna
C
C        *LSUB*  CURRENT SUB-ROUTINE LEVEL
C        *LTEST* MAXIMUM LEVEL WITH TEST OUTPUT
C        *LSUVI* MAXIMUM LEVEL WITH STANDARD OUTPUT
C        *CTEST* ARRAY OF SUB-ROUTINE NAMES GIVING TEST OUTPUT
C        *CSUVI* ARRAY OF SUB-ROUTINE NAMES GIVING STANDARD OUTPUT
C
      DATA csubna /"plot_la"/
C
C ----------------------------------------------------------------------
C
C*    1. INITIALIZATIONS.
C        ----------------
 1000 CONTINUE
      lsub = lsub + 1
      CALL  uraopc (csubna, lsub, losuvi, lotest)
      IF (losuvi .OR. lotest) THEN
        WRITE(102,1001)
 1001   FORMAT('MPPT2 - Subroutine:  PLOT_LAYOUT ')
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    2. LOOP OVER USER INPUT.
C        ---------------------
 2000 CONTINUE
C
C     DEFINE PAGE ORIGIN ACCORDING TO IRUN

      IF(IRUN.EQ.1) THEN
                    XPORG = 0.0
                    YPORG = 0.0
                    END IF
      IF(IRUN.EQ.2) THEN
                    XPORG = 10.2
                    YPORG = 0.0
                    END IF
      IF(IRUN.EQ.3) THEN
                    XPORG = 0.0
                    YPORG = 9.2
                    END IF
      IF(IRUN.EQ.4) THEN
                    XPORG = 10.2
                    YPORG =  9.2
                    END IF
      IF(IRUN.EQ.5) THEN
                    XPORG =  0.0
                    YPORG = 18.4
                    END IF
      IF(IRUN.EQ.6) THEN
                    XPORG = 10.2
                    YPORG = 18.4
                    END IF

      IF(KPLT.EQ.1) THEN
                    page_x_position =  XPORG
                    page_y_position =  YPORG
                    page_x_length   = 10.2
                    page_y_length   =  9.2
                    page_frame      = 'ON'
                    subpage_x_position =  1.0
                    subpage_y_position =  1.5
                    subpage_x_length   =  6.5
                    subpage_y_length   =  6.5
                    subpage_frame      = 'ON'
                    if(IRUN.EQ.6)subpage_frame      = 'OFF'
                    END IF

      IF(KPLT.EQ.2) THEN
                    page_x_position =  XPORG + 7.5
                    page_y_position =  YPORG
                    page_x_length   =  2.7
                    page_y_length   =  9.2
                    page_frame      = 'OFF'
                    subpage_x_position =  0.5
                    subpage_y_position =  0.0
                    subpage_x_length   =  2.2
                    subpage_y_length   =  6.5
                    subpage_frame      = 'OFF'
                    END IF

      IF(KPLT.EQ.3) THEN
                    page_x_position =  XPORG
                    page_y_position =  YPORG
                    page_x_length   = 20.4
                    page_y_length   =  2.1
                    page_frame      = 'ON'
                    subpage_x_position =  0.0
                    subpage_y_position =  0.0
                    subpage_x_length   = 20.4
                    subpage_y_length   =  2.1
                    subpage_frame      = 'OFF'
                    END IF

           IF (lotest) THEN
           WRITE(kuso,*)'SUPER_PAGE_X_LENGTH      :',super_page_x_length
           WRITE(kuso,*)'SUPER_PAGE_Y_LENGTH      :',super_page_y_length
           WRITE(kuso,*)'PAGE_X_GAP               :',page_x_gap
           WRITE(kuso,*)'PAGE_Y_GAP               :',page_y_gap
           WRITE(kuso,*)'PAGE_X_POSITION          :',page_x_position
           WRITE(kuso,*)'PAGE_Y_POSITION          :',page_y_position
           WRITE(kuso,*)'PAGE_X_LENGTH            :',page_x_length
           WRITE(kuso,*)'PAGE_Y_LENGTH            :',page_y_length
           WRITE(kuso,*)'PAGE_ID_LINE_HEIGHT      :',page_id_line_height
           WRITE(kuso,*)'PAGE_FRAME_THICKNESS     :',
     .                   page_frame_thickness
           WRITE(kuso,*)'SUBPAGE_X_POSITION       :',subpage_x_position
           WRITE(kuso,*)'SUBPAGE_Y_POSITION       :',subpage_y_position
           WRITE(kuso,*)'SUBPAGE_X_LENGTH         :',subpage_x_length
           WRITE(kuso,*)'SUBPAGE_Y_LENGTH         :',subpage_y_length
           WRITE(kuso,*)'SUBPAGE_FRAME_THICKNESS  :',
     .                   subpage_frame_thickness
           WRITE(kuso,*)'SUBPAGE_LOWER_LEFT_LONGITUDE  :',
     .                   subpage_lower_left_longitude
           WRITE(kuso,*)'SUBPAGE_LOWER_LEFT_LATITUDE   :',
     .                   subpage_lower_left_latitude
           WRITE(kuso,*)'SUBPAGE_UPPER_RIGHT_LONGITUDE :',
     .                   subpage_upper_right_longitude
           WRITE(kuso,*)'SUBPAGE_UPPER_RIGHT_LATITUDE  :',
     .                   subpage_upper_right_latitude
           WRITE(kuso,*)'SUBPAGE_MAP_VERTICAL_LONGITUDE:',
     .                   subpage_map_vertical_longitude
           WRITE(kuso,*)'SUBPAGE_MAP_CENTRE_LONGITUDE  :',
     .                   subpage_map_centre_longitude
           WRITE(kuso,*)'SUBPAGE_MAP_CENTRE_LATITUDE   :',
     .                   subpage_map_centre_latitude
           WRITE(kuso,*)'SUBPAGE_MAP_X_POLAR_VECTOR    :',
     .                   subpage_map_x_polar_vector
           WRITE(kuso,*)'SUBPAGE_MAP_Y_POLAR_VECTOR    :',
     .                   subpage_map_y_polar_vector
           WRITE(kuso,*)'SUBPAGE_MAP_SCALE        :',subpage_map_scale
           WRITE(kuso,*)'MAP_GRID_THICKNESS       :',map_grid_thickness
           WRITE(kuso,*)'MAP_GRID_LATITUDE_REFERENCE   :',
     .                   map_grid_latitude_reference
           WRITE(kuso,*)'MAP_GRID_LONGITUDE_REFERENCE  :',
     .                   map_grid_longitude_reference
           WRITE(kuso,*)'MAP_GRID_LATITUDE_INCREMENT   :',
     .                   map_grid_latitude_increment
           WRITE(kuso,*)'MAP_GRID_LONGITUDE_INCREMENT  :',
     .                   map_grid_longitude_increment
           WRITE(kuso,*)'MAP_LABEL_LONGITUDE_FREQUENCY :',
     .                   map_label_longitude_frequency
           WRITE(kuso,*)'MAP_LABEL_LATITUDE_FREQUENCY  :',
     .                   map_label_latitude_frequency
           WRITE(kuso,*)'MAP_LABEL_HEIGHT         :',map_label_height
           WRITE(kuso,*)'SUPER_PAGE_FRAME         :',super_page_frame
           WRITE(kuso,*)'LAYOUT                   :',layout
           WRITE(kuso,*)'PLOT_START               :',plot_start
           WRITE(kuso,*)'PLOT_DIRECTION           :',plot_direction
           WRITE(kuso,*)'PAGE_FRAME               :',page_frame
           WRITE(kuso,*)'PAGE_FRAME_COLOUR        :',page_frame_colour
           WRITE(kuso,*)'PAGE_FRAME_LINE_STYLE    :',
     .                   page_frame_line_style
           WRITE(kuso,*)'PAGE_ID_LINE             :',page_id_line
           WRITE(kuso,*)'PAGE_ID_LINE_QUALITY     :',
     .                   page_id_line_quality
           WRITE(kuso,*)'PAGE_ID_LINE_USER_TEXT   :',
     .                   page_id_line_user_text
           WRITE(kuso,*)'SUBPAGE_FRAME            :',subpage_frame
           WRITE(kuso,*)'SUBPAGE_FRAME_COLOUR     :',
     .                   subpage_frame_colour
           WRITE(kuso,*)'SUBPAGE_FRAME_LINE_STYLE :',
     .                   subpage_frame_line_style
           WRITE(kuso,*)'SUBPAGE_MAP_PROJECTION   :',
     .                   subpage_map_projection
           WRITE(kuso,*)'SUBPAGE_MAP_AREA_DEFINITION   :',
     .                   subpage_map_area_definition
           WRITE(kuso,*)'SUBPAGE_MAP_HEMISPHERE   :',
     .                   subpage_map_hemisphere
           WRITE(kuso,*)'MAP_COASTLINE            :',
     .                   map_coastline
           WRITE(kuso,*)'MAP_COASTLINE_RESOLUTION :',
     .                   map_coastline_resolution
           WRITE(kuso,*)'MAP_COASTLINE_STYLE      :',
     .                   map_coastline_style
           WRITE(kuso,*)'MAP_COASTLINE_COLOUR     :',
     .                   map_coastline_colour
           WRITE(kuso,*)'MAP_GRID                 :',map_grid
           WRITE(kuso,*)'MAP_GRID_LINE_STYLE      :',map_grid_line_style
           WRITE(kuso,*)'MAP_GRID_COLOUR          :',map_grid_colour
           WRITE(kuso,*)'MAP_LABEL                :',map_label
           WRITE(kuso,*)'MAP_LABEL_QUALITY        :',map_label_quality
     
          WRITE(kuso,*)'                                 '
          ENDIF
          GOTO 3000
C
C ----------------------------------------------------------------------
C
C*    3. MAGICS PARAMETER SEETINGS IF A MAP IS TO BE PLOTTED.
C        ----------------------------------------------------
 3000 CONTINUE
      CALL PSETC ('SUBPAGE_MAP_PROJECTION' ,subpage_map_projection)
      CALL PSETR ('SUPER_PAGE_X_LENGTH' ,super_page_x_length)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH' ,super_page_y_length)
      CALL PSETC ('SUPER_PAGE_FRAME' ,super_page_frame)
      CALL PSETC ('LAYOUT' ,layout)
      CALL PSETC ('PLOT_START' ,plot_start)
      CALL PSETC ('PLOT_DIRECTION' ,plot_direction)
      CALL PSETR ('PAGE_X_GAP' ,page_x_gap)
      CALL PSETR ('PAGE_Y_GAP' ,page_y_gap)

      CALL PSETR ('PAGE_X_POSITION' ,page_x_position)
      CALL PSETR ('PAGE_Y_POSITION' ,page_y_position)
      CALL PSETR ('PAGE_X_LENGTH' ,page_x_length)
      CALL PSETR ('PAGE_Y_LENGTH' ,page_y_length)
      CALL PSETC ('PAGE_FRAME' ,page_frame)
      CALL PSETC ('PAGE_FRAME_COLOUR' ,page_frame_colour)
      CALL PSETC ('PAGE_FRAME_LINE_STYLE' ,page_frame_line_style)
      CALL PSETI ('PAGE_FRAME_THICKNESS' ,page_frame_thickness)
      CALL PSETC ('PAGE_ID_LINE', page_id_line)
      CALL PSETR ('PAGE_ID_LINE_HEIGHT' ,page_id_line_height)
      CALL PSETC ('PAGE_ID_LINE_QUALITY', page_id_line_quality)
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT' ,page_id_line_user_text)

      CALL PSETR ('SUBPAGE_X_POSITION' ,subpage_x_position)
      CALL PSETR ('SUBPAGE_Y_POSITION' ,subpage_y_position)
      CALL PSETR ('SUBPAGE_X_LENGTH' ,subpage_x_length)
      CALL PSETR ('SUBPAGE_Y_LENGTH' ,subpage_y_length)
      CALL PSETC ('SUBPAGE_FRAME' ,subpage_frame)
      CALL PSETC ('SUBPAGE_FRAME_COLOUR' ,subpage_frame_colour)
      CALL PSETC ('SUBPAGE_FRAME_LINE_STYLE'
     .            ,subpage_frame_line_style)
      CALL PSETI ('SUBPAGE_FRAME_THICKNESS' ,subpage_frame_thickness)
cIR settings not required for graph plotting
C      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE'
C     .            ,subpage_lower_left_longitude)
C      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE'
C     .            ,subpage_lower_left_latitude)
C      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE'
C     .            ,subpage_upper_right_longitude)
C      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE'
C     .            ,subpage_upper_right_latitude)
C
C      CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION'
C     .            ,subpage_map_area_definition)
C      CALL PSETC ('SUBPAGE_MAP_HEMISPHERE' ,subpage_map_hemisphere)
C      CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE'
C     .            ,subpage_map_vertical_longitude)
C      CALL PSETR ('SUBPAGE_MAP_CENTRE_LONGITUDE'
C     .            ,subpage_map_centre_longitude)
C      CALL PSETR ('SUBPAGE_MAP_CENTRE_LATITUDE'
C     .            ,subpage_map_centre_latitude)
C      CALL PSETR ('SUBPAGE_MAP_X_POLAR_VECTOR'
C     .            ,subpage_map_x_polar_vector)
C      CALL PSETR ('SUBPAGE_MAP_Y_POLAR_VECTOR'
C     .            ,subpage_map_y_polar_vector)
C      CALL PSETR ('SUBPAGE_MAP_SCALE'
C     .            ,subpage_map_scale)
C      CALL PSETC ('MAP_COASTLINE' ,map_coastline)
C      CALL PSETC ('MAP_COASTLINE_RESOLUTION'
C     .            ,map_coastline_resolution)
C      CALL PSETC ('MAP_COASTLINE_STYLE' ,map_coastline_style)
C      CALL PSETC ('MAP_COASTLINE_COLOUR' ,map_coastline_colour)
C
C      CALL PSETC ('MAP_GRID' ,map_grid)
C      CALL PSETC ('MAP_GRID_LINE_STYLE' ,map_grid_line_style)
C      CALL PSETI ('MAP_GRID_THICKNESS' ,map_grid_thickness)
C      CALL PSETC ('MAP_GRID_COLOUR' ,map_grid_colour)
C      CALL PSETR ('MAP_GRID_LATITUDE_REFERENCE'
C     .            ,map_grid_latitude_reference)
C      CALL PSETR ('MAP_GRID_LONGITUDE_REFERENCE'
C     .            ,map_grid_longitude_reference)
C      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT'
C     .            ,map_grid_latitude_increment)
C      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT'
C     .            ,map_grid_longitude_increment)
C      
C      CALL PSETC ('MAP_LABEL' ,map_label)
C      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY'
C     .            ,map_label_longitude_frequency)
C      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY'
C     .            ,map_label_latitude_frequency)
C      CALL PSETR ('MAP_LABEL_HEIGHT' ,map_label_height)
C      CALL PSETC ('MAP_LABEL_QUALITY' ,map_label_quality)
      lsub = lsub - 1
      RETURN
      END
C
C ======================================================================
C
      FUNCTION cl_magcl(kc)
C
C ======================================================================
C
      CHARACTER cl_magcl*20,cl_colt*20
      DIMENSION cl_colt(56)
C
      DATA cl_colt( 1) / 'RED'                /
      DATA cl_colt( 2) / 'GREEN'              /
      DATA cl_colt( 3) / 'BLUE'               /
      DATA cl_colt( 4) / 'YELLOW'             /
      DATA cl_colt( 5) / 'CYAN'               /
      DATA cl_colt( 6) / 'MAGENTA'            /
      DATA cl_colt( 7) / 'BACKGROUND'         /
      DATA cl_colt( 8) / 'AVOCADO'            /
      DATA cl_colt( 9) / 'BEIGE'              /
      DATA cl_colt(10) / 'BRICK'              /
      DATA cl_colt(11) / 'BROWN'              /
      DATA cl_colt(12) / 'BURGUNDY'           /
      DATA cl_colt(13) / 'CHARCOAL'           /
      DATA cl_colt(14) / 'CHESTNUT'           /
      DATA cl_colt(15) / 'CORAL'              /
      DATA cl_colt(16) / 'CREAM'              /
      DATA cl_colt(17) / 'EVERGREEN'          /
      DATA cl_colt(18) / 'GOLD'               /
      DATA cl_colt(19) / 'GREY'               /
      DATA cl_colt(20) / 'KHAKI'              /
      DATA cl_colt(21) / 'KELLY_GREEN'        /
      DATA cl_colt(22) / 'LAVENDER'           /
      DATA cl_colt(23) / 'MUSTARD'            /
      DATA cl_colt(24) / 'NAVY'               /
      DATA cl_colt(25) / 'OCHRE'              /
      DATA cl_colt(26) / 'OLIVE'              /
      DATA cl_colt(27) / 'PEACH'              /
      DATA cl_colt(28) / 'PINK'               /
      DATA cl_colt(29) / 'ROSE'               /
      DATA cl_colt(30) / 'RUST'               /
      DATA cl_colt(31) / 'SKY'                /
      DATA cl_colt(32) / 'TAN'                /
      DATA cl_colt(33) / 'TANGERINE'          /
      DATA cl_colt(34) / 'TURQUOISE'          /
      DATA cl_colt(35) / 'VIOLET'             /
      DATA cl_colt(36) / 'REDDISH_PURPLE'     /
      DATA cl_colt(37) / 'PURPLE_RED'         /
      DATA cl_colt(38) / 'PURPLISH_RED'       /
      DATA cl_colt(39) / 'ORANGISH_RED'       /
      DATA cl_colt(40) / 'RED_ORANGE'         /
      DATA cl_colt(41) / 'REDDISH_ORANGE'     /
      DATA cl_colt(42) / 'ORANGE'             /
      DATA cl_colt(43) / 'YELLOWISH_ORANGE'   /
      DATA cl_colt(44) / 'ORANGE_YELLOW'      /
      DATA cl_colt(45) / 'ORANGISH_YELLOW'    /
      DATA cl_colt(46) / 'GREENISH_YELLOW'    /
      DATA cl_colt(47) / 'YELLOW_GREEN'       /
      DATA cl_colt(48) / 'YELLOWISH_GREEN'    /
      DATA cl_colt(49) / 'BLUISH_GREEN'       /
      DATA cl_colt(50) / 'BLUE_GREEN'         /
      DATA cl_colt(51) / 'GREENISH_BLUE'      /
      DATA cl_colt(52) / 'PURPLISH_BLUE'      /
      DATA cl_colt(53) / 'BLUE_PURPLE'        /
      DATA cl_colt(54) / 'BLUISH_PURPLE'      /
      DATA cl_colt(55) / 'PURPLE'             /
      DATA cl_colt(56) / 'BLACK'/
C
      cl_magcl='                    '
      cl_magcl=cl_colt(MOD(kc-1,56)+1)
      RETURN
      END
      SUBROUTINE prep_box(x,y,ndim,binsize,xsym,ysym,box,nbox,boxmax)
c
c prepares x,y data for box (binned) data plot
c
c                           J.BIDLOT  sept. 1998
c --------------------------------------------------------------------
c
      real x(ndim),y(ndim)
      real xsym(nbox*nbox),ysym(nbox*nbox),box(nbox,nbox)
c
      nin=0
      do j=1,nbox
        do i=1,nbox
           nin=nin+1
           xsym(nin)=(i-1)*binsize
           ysym(nin)=(j-1)*binsize
        enddo
      enddo

      box=0.

      do ij=1,ndim
         i=nint((x(ij))/binsize)+1
         j=nint((y(ij))/binsize)+1
         box(i,j)=box(i,j)+1.
      enddo

      boxmax=0.
      do j=1,nbox
        do i=1,nbox
        boxmax=max(boxmax,box(i,j))
        enddo
      enddo

      return
      end
C ======================================================================
C
C
C
      SUBROUTINE PSTATS (IU06,  LABEL, NPT,
     1                   XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2                   DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3                   SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4                   SLOPE2, RINTR2, RMSES2, RMSEU2)
C
C ======================================================================
C
C****  *PSTATS* PLOT STATISTIC RESULTS.
C
C     B. HANSEN           ECMWF     JUNE 1994
C
C*    PURPOSE.
C     --------
C
C       THIS SUB PLOTS THE FOLLOWING STATISTICS
C
C       1.  MAXIMUM VALUES OF X AND Y
C       2.  MEAN AND STANDARD DEVIATION OF BOTH X AND Y VALUES
C       3.  MAXIMUM VALUE OF ABSOLUTE DIFFERNCE
C       4.  MEAN VALUE OF DIFFERENCESM (BIAS)
C       5.  ROOT-MEAN-SQUARE ERROR (RMSE)
C       6.  STANDARD DEVIATION OF DIFFERENCES
C       7.  SCATTER INDEX (SI)
C       8.  REDUCTION OF VARIACE.
C       9.  CORRELATION COEFFICIENT
C      10.  THE SYMMETRIC SLOPE AND THE UNSYMMETRIC AND SYMMETRIC RMSE
C           OF THIS REGESSION LINE. (SLOPES, RSMEUS, RSMESS)
C      11.  THE SLOPE OF THE REGRESSION LINE THROUGH THE ORIGAN AND
C           THE UNSYMMETRIC AND SYMMETRIC RMSE. (SLOPE1, RSMEU1, RSMES1)
C      12.  SLOPE AND INTERCEPT OF THE 2 PARAMETER REGRESSION LINE AND
C           THE UNSYMMETRIC AND SYMMETRIC RMSE. (SLOPE2, RINTS2, RSMEU2,
C           RSMES2)
C
C***  INTERFACE.
C     ----------
C
C      *CALL* *PSTATS (IU06, LABEL, NPT,
C                      XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
C                      DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
C                      SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
C                      SLOPE2, RINTR2, RMSES2, RMSEU2)*
C
C*    VARIABLE.    TYPE.     PURPOSE.
C     ---------    ------    --------
C       *IU06*     INTEGER   PRINTER OUTPUT UNIT. IF EQ 0 NO OUTPUT.
C       *LABEL*    L         TRUE MEANS X/Y VALUES ARE WAVEHEIGHTS
C                            FALSE MEANS X/Y VALUES ARE WIND SPEEDS.
C       *NPT*      INTEGER   NUMBER OF POINTS
C       *XMAX*     REAL      MAX VALUE OF INDEPENDENT VARIABLE
C       *YMAX*     REAL      MAX VALUE OF DEPENDENT VARIABLE
C       *XMEAN*    REAL      MEAN VALUE OF INDEPENDENT VARIABLE
C       *YMEAN*    REAL      MEAN VALUE OF DEPENDENT VARIABLE
C       *STDEVX*   REAL      STANDARD DEVIATION OF INDEPENDENT VARIABLE
C       *STDEVY*   REAL      STANDARD DEVIATION OF DEPENDENT VARIABLE
C       *DMAX*     REAL      MAX ABSOLUTE VALUE OF DIFFERENCE
C       *BIAS*     REAL      MEAN VALUE OF DIFFERENCES
C       *RMSE*     REAL      ROOT-MEAN-SQUARE ERROR
C       *STDEVD*   REAL      STANDARD DEVIATION OF DIFFERENCE
C       *SID*      REAL      SCATTER INDEX
C       *RV*       REAL      REDUCTION OF VARIACE.
C       *CORR*     REAL      CORRELATION COEFFICIENT
C       *SLOPES*   REAL      SLOPE OF SYMMETRIC REGRESSION LINE.
C       *RMSEUS*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *RMSESS*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *SLOPE1*   REAL      SLOPE OF REGRESSION LINE THROUGH ORIGIAN.
C       *RMSEU1*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *RMSES1*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *SLOPE2*   REAL      SLOPE OF 2 PARAMETER LEAST SQUARES FIT.
C       *RINTR2*   REAL      INTERCEPT OF 2 PARAMETER LEAST SQUARES FIT
C       *RMSEU2*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C       *RMSES2*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C
C     METHOD.
C     -------
C
C       THE COMPUTATION OF THESE STATISTICS IS STRAIGHTFORWARD.
C
C
C     EXTERNALS.
C     ---------
C
C        *STATSI* - INITIALISE SUMS FOR STATISTICS.
C        *STATSC* - COMPUTE    SUMS FOR STATISTICS.
C        *STATSE* - EVALUATE   STATISTISC FROM SUMS.
C
C     REFERENCES.
C     ----------
C
C          ZAMBRESKY,L.F.,  ECMWF TECH REPORT NUMBER 63
C
C ----------------------------------------------------------------------
C
      LOGICAL LABEL
      CHARACTER* 90 YOFILE
      CHARACTER* 80 TXT3
      CHARACTER* 40 TXT1, TXT2, TEXTX, TEXTY
      CHARACTER*  5 YOBUOY
C
C ----------------------------------------------------------------------
C
C*    1. PLOT STATISTICS.
C        ----------------
C
 1000 CONTINUE
      CALL PSETC ('TEXT_COLOUR', 'BLACK')
      CALL PSETC ('TEXT_MODE', 'POSITIONAL')
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETC ('TEXT_BORDER','OFF')

C     2. BOX POSITION - PLOT TEXTS.
C        --------------------------

      XPOS1 = 0.2
      YPOS1 = 2.0
      NLINE = 1
      XPOS3 = XPOS1
      DECR  = NLINE * 1.67*0.22
      YPOS3 = YPOS1 - DECR
      CALL PSETR ('TEXT_BOX_X_POSITION', XPOS3)
      CALL PSETR ('TEXT_BOX_Y_POSITION', YPOS3)
      CALL PSETR ('TEXT_BOX_X_LENGTH', 2.25)
      CALL PSETR ('TEXT_BOX_Y_LENGTH', 3.30)

c      CALL PSETC ('TEXT_INSTRUCTION_SHIFT_CHARACTER','#')
c     CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 1.5)
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETI ('TEXT_LINE_COUNT', 10)

      CALL PSETI ('TEXT_INTEGER_1',NPT)
      CALL PSETR ('TEXT_REAL_1', YMEAN)
      CALL PSETR ('TEXT_REAL_2', STDEVY)
      CALL PSETR ('TEXT_REAL_3', XMEAN)
      CALL PSETR ('TEXT_REAL_4', STDEVX)
c>>>      CALL PSETR ('TEXT_REAL_5', SLOPE2)
c>>>      CALL PSETR ('TEXT_REAL_5', SLOPE1)
      CALL PSETR ('TEXT_REAL_5', SLOPES)
      CALL PSETR ('TEXT_REAL_6', RMSE)
      CALL PSETR ('TEXT_REAL_7', BIAS)
      CALL PSETR ('TEXT_REAL_8', CORR)
      CALL PSETR ('TEXT_REAL_9', SID)

C      CALL PSETC ('TEXT_LINE_1', '#CW1.0#NUM  = @(I5)TEXT_INTEGER_1@')
C      CALL PSETC ('TEXT_LINE_2', '#CW1.0#YM   = @(F5.2)TEXT_REAL_1@')
C      CALL PSETC ('TEXT_LINE_3', '#CW1.0#STDY = @(F5.2)TEXT_REAL_2@')
C      CALL PSETC ('TEXT_LINE_4', '#CW1.0#XM   = @(F5.2)TEXT_REAL_3@')
C      CALL PSETC ('TEXT_LINE_5', '#CW1.0#STDX = @(F5.2)TEXT_REAL_4@')
C      CALL PSETC ('TEXT_LINE_6', '#CW1.0#SSLO = @(F5.2)TEXT_REAL_5@')
C      CALL PSETC ('TEXT_LINE_7', '#CW1.0#RMSE = @(F5.2)TEXT_REAL_6@')
C      CALL PSETC ('TEXT_LINE_8', '#CW1.0#BIAS = @(F5.2)TEXT_REAL_7@')
C      CALL PSETC ('TEXT_LINE_9', '#CW1.0#CORR = @(F5.2)TEXT_REAL_8@')
C      CALL PSETC ('TEXT_LINE_10','#CW1.0#SI   = @(F5.2)TEXT_REAL_9@')
      CALL PSETC ('TEXT_LINE_1', 'NUM  = @TEXT_INTEGER_1@')
      CALL PSETC ('TEXT_LINE_2', 'YM   = @TEXT_REAL_1@')
      CALL PSETC ('TEXT_LINE_3', 'STDY = @TEXT_REAL_2@')
      CALL PSETC ('TEXT_LINE_4', 'XM   = @TEXT_REAL_3@')
      CALL PSETC ('TEXT_LINE_5', 'STDX = @TEXT_REAL_4@')
      CALL PSETC ('TEXT_LINE_6', 'SSLO = @TEXT_REAL_5@')
      CALL PSETC ('TEXT_LINE_7', 'RMSE = @TEXT_REAL_6@')
      CALL PSETC ('TEXT_LINE_8', 'BIAS = @TEXT_REAL_7@')
      CALL PSETC ('TEXT_LINE_9', 'CORR = @TEXT_REAL_8@')
      CALL PSETC ('TEXT_LINE_10','SI   = @TEXT_REAL_9@')
      CALL PTEXT


      RETURN
      END
C ======================================================================
C
      SUBROUTINE STATS (IU06, X, Y, NDIM, NPT, kcirc,
     1                  XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2                  DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3                  SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4                  SLOPE2, RINTR2, RMSES2, RMSEU2)

C ----------------------------------------------------------------------
C
C****  *STATS* COMPUTE STATISTICS.
C
C     LIANA ZAMBRESKY      GKSS/ECMWF     AUGUST 1987
C     H. GUNTHER           GKSS/ECMWF     AUGUST 1990
C
C*    PURPOSE.
C     --------
C
C       THIS SUB COMPUTES THE FOLLOWING STATISTICS
C
C       1.  MAXIMUM VALUES OF X AND Y
C       2.  MEAN AND STANDARD DEVIATION OF BOTH X AND Y VALUES
C       3.  MAXIMUM VALUE OF ABSOLUTE DIFFERNCE
C       4.  MEAN VALUE OF DIFFERENCESM (BIAS)
C       5.  ROOT-MEAN-SQUARE ERROR (RMSE)
C       6.  STANDARD DEVIATION OF DIFFERENCES
C       7.  SCATTER INDEX (SI)
C       8.  REDUCTION OF VARIACE.
C       9.  CORRELATION COEFFICIENT
C      10.  THE SYMMETRIC SLOPE AND THE UNSYMMETRIC AND SYMMETRIC RMSE
C           OF THIS REGESSION LINE. (SLOPES, RSMEUS, RSMESS)
C      11.  THE SLOPE OF THE REGRESSION LINE THROUGH THE ORIGAN AND
C           THE UNSYMMETRIC AND SYMMETRIC RMSE. (SLOPE1, RSMEU1, RSMES1)
C      12.  SLOPE AND INTERCEPT OF THE 2 PARAMETER REGRESSION LINE AND
C           THE UNSYMMETRIC AND SYMMETRIC RMSE. (SLOPE2, RINTS2, RSMEU2,
C           RSMES2)
C
C***  INTERFACE.
C     ----------
C
C       *CALL* *STATS (IU06, X, Y, NDIM, NPT,
C                      XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
C                      DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
C                      SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
C                      SLOPE2, RINTR2, RMSES2, RMSEU2)*
C
C*    VARIABLE.    TYPE.     PURPOSE.
C     ---------    ------    --------
C       *IU06*     INTEGER   PRINTER OUTPUT UNIT. IF EQ 0 NO OUTPUT.
C       *X*        REAL      INDEPENDENT ARRAY
C       *Y*        REAL      DEPENDENT ARRAY
C       *NDIM*     INTEGER   ARRAY DIMENSION
C       *NPT*      INTEGER   NUMBER OF POINTS
c       *kcirc*    integer   input 2 if x and y values are in degree else 1
C       *XMAX*     REAL      MAX VALUE OF INDEPENDENT VARIABLE
C       *YMAX*     REAL      MAX VALUE OF DEPENDENT VARIABLE
C       *XMEAN*    REAL      MEAN VALUE OF INDEPENDENT VARIABLE
C       *YMEAN*    REAL      MEAN VALUE OF DEPENDENT VARIABLE
C       *STDEVX*   REAL      STANDARD DEVIATION OF INDEPENDENT VARIABLE
C       *STDEVY*   REAL      STANDARD DEVIATION OF DEPENDENT VARIABLE
C       *DMAX*     REAL      MAX ABSOLUTE VALUE OF DIFFERENCE
C       *BIAS*     REAL      MEAN VALUE OF DIFFERENCES
C       *RMSE*     REAL      ROOT-MEAN-SQUARE ERROR
C       *STDEVD*   REAL      STANDARD DEVIATION OF DIFFERENCE
C       *SID*      REAL      SCATTER INDEX
C       *RV*       REAL      REDUCTION OF VARIACE.
C       *CORR*     REAL      CORRELATION COEFFICIENT
C       *SLOPES*   REAL      SLOPE OF SYMMETRIC REGRESSION LINE.
C       *RMSEUS*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *RMSESS*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *SLOPE1*   REAL      SLOPE OF REGRESSION LINE THROUGH ORIGIAN.
C       *RMSEU1*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *RMSES1*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *SLOPE2*   REAL      SLOPE OF 2 PARAMETER LEAST SQUARES FIT.
C       *RINTR2*   REAL      INTERCEPT OF 2 PARAMETER LEAST SQUARES FIT
C       *RMSEU2*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C       *RMSES2*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C
C     METHOD.
C     -------
C
C       THE COMPUTATION OF THESE STATISTICS IS STRAIGHTFORWARD.
C
C
C     EXTERNALS.
C     ---------
C
C        *STATSI* - INITIALISE SUMS FOR STATISTICS.
C        *STATSC* - COMPUTE    SUMS FOR STATISTICS.
C        *STATSE* - EVALUATE   STATISTISC FROM SUMS.
C        *STATSP* - PRINT      STATISTICS.
C
C     REFERENCES.
C     ----------
C
C          ZAMBRESKY,L.F.,  ECMWF TECH REPORT NUMBER 63
C
C ----------------------------------------------------------------------
C
      DIMENSION X(NDIM),Y(NDIM)
C
C ----------------------------------------------------------------------
C
C*    1. INITIALIZATION OF SUMS.
C        -----------------------
C
 1000 CONTINUE
      CALL STATSI (NTOT, kcirc, XMAX, YMAX, DMAX,
     1             SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
     &             sumxc, sumyc)
C
C ----------------------------------------------------------------------
C
C*    2. COMPUTE SUMS FOR STATISTICS.
C        -----------------------------
C
 2000 CONTINUE
      CALL STATSC (X, Y, NDIM, NPT, NTOT, kcirc,  XMAX, YMAX, DMAX,
     1             SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
     &             sumxc, sumyc)
C
C ----------------------------------------------------------------------
C
C*    3. EVALUATE STATISTICS.
C        --------------------
C
 3000 CONTINUE
      CALL STATSE (NTOT, kcirc, SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
     &             sumxc, sumyc,
     1             XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2             DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3             SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4             SLOPE2, RINTR2, RMSES2, RMSEU2)
C
C ----------------------------------------------------------------------
C
C*    4. PRINT STATISTICS.
C        -----------------
C
 4000 CONTINUE
      IF (IU06.NE.0) THEN
         CALL STATSP (IU06, NTOT, kcirc,
     1                XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2                DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3                SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4                SLOPE2, RINTR2, RMSES2, RMSEU2)
      ENDIF

      RETURN
      END
C ======================================================================
C
      SUBROUTINE STATSP (IU06, NTOT, kcirc,
     1                   XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2                   DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3                   SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4                   SLOPE2, RINTR2, RMSES2, RMSEU2)

C ----------------------------------------------------------------------
C
C**** *STATSP*- PRINT STATISTICS.
C
C     LIANA ZAMBRESKY      GKSS/ECMWF     AUGUST 1987
C     H. GUNTHER           GKSS/ECMWF     AUGUST 1990
C
C*    PURPOSE.
C     --------
C
C       TO PRINT THE STATISTIC PARAMETER EVALUATED IN SUB STATSE.
C
C***  INTERFACE.
C     ----------
C
C       *CALL* *STATSP (IU06, NTOT,
C                      XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
C                      DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
C                      SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
C                      SLOPE2, RINTR2, RMSES2, RMSEU2)*
C
C*    VARIABLE.    TYPE.     PURPOSE.
C     ---------    -------   --------
C       *IU06*     INTEGER   PRINTER OUTPUT UNIT. IF EQ 0 NO OUTPUT.
C       *NTOT*     INTEGER   NUMBER OF POINTS.
c       *kcirc*    integer   input 2 if x and y values are in degree else 1
C       *XMAX*     REAL      MAX VALUE OF INDEPENDENT VARIABLE
C       *YMAX*     REAL      MAX VALUE OF DEPENDENT VARIABLE
C       *XMEAN*    REAL      MEAN VALUE OF INDEPENDENT VARIABLE
C       *YMEAN*    REAL      MEAN VALUE OF DEPENDENT VARIABLE
C       *STDEVX*   REAL      STANDARD DEVIATION OF INDEPENDENT VARIABLE
C       *STDEVY*   REAL      STANDARD DEVIATION OF DEPENDENT VARIABLE
C       *DMAX*     REAL      MAX ABSOLUTE VALUE OF DIFFERENCE
C       *BIAS*     REAL      MEAN VALUE OF DIFFERENCES
C       *RMSE*     REAL      ROOT-MEAN-SQUARE ERROR
C       *STDEVD*   REAL      STANDARD DEVIATION OF DIFFERENCE
C       *SID*      REAL      SCATTER INDEX
C       *RV*       REAL      REDUCTION OF VARIACE.
C       *CORR*     REAL      CORRELATION COEFFICIENT
C       *SLOPES*   REAL      SLOPE OF SYMMETRIC REGRESSION LINE.
C       *RMSEUS*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *RMSESS*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *SLOPE1*   REAL      SLOPE OF REGRESSION LINE THROUGH ORIGIAN.
C       *RMSEU1*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *RMSES1*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *SLOPE2*   REAL      SLOPE OF 2 PARAMETER LEAST SQUARES FIT.
C       *RINTR2*   REAL      INTERCEPT OF 2 PARAMETER LEAST SQUARES FIT
C       *RMSEU2*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C       *RMSES2*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C
C     METHOD.
C     -------
C
C       NONE.
C
C
C     EXTERNALS.
C     ----------
C
C       NONE.
C
C     REFERENCES.
C     -----------
C
C       ZAMBRESKY,L.F.,  ECMWF TECH REPORT NUMBER 63
C
C ----------------------------------------------------------------------
C
C*    1. CHECK PRINTER UNIT.
C        ------------------
C
 1000 CONTINUE
      IF (IU06.EQ.0) RETURN
C
C ----------------------------------------------------------------------
C
C*    2. PRINT RESULTS.
C        --------------
C
 2000 CONTINUE
      WRITE(IU06,500) NTOT
 500  FORMAT(/1X,'TOTAL NUMBER OF DATA : ',I9)

      IF (NTOT.LT.2) THEN
         WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++'
         WRITE(IU06,*) ' + WARNING: NUMBER OF ENTRIES IS LESS THAN 2 +'
         WRITE(IU06,*) ' + SOME VALUES COULD NOT BE EVALUATED.       +'
         WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++'
      ENDIF

      WRITE(IU06,501) XMAX, XMEAN, STDEVX
 501  FORMAT(' INDEPENDENT VARIABLE X :',/,'       MAXIMUM VALUE',
     1  F8.3,'  MEAN VALUE',F8.3,'  STANDARD DEVIATION',F8.3)

      WRITE(IU06,502) YMAX, YMEAN, STDEVY
 502  FORMAT(' DEPENDENT VARIABLE Y :',/,'       MAXIMUM VALUE',
     1  F8.3,'  MEAN VALUE',F8.3,'  STANDARD DEVIATION',F8.3)

      WRITE(IU06,503) DMAX, BIAS , STDEVD
 503  FORMAT( 1X,'DIFFERENCE   Y - X :',/,'  ABS. MAXIMUM VALUE',
     1  F8.3,'        BIAS',F8.3,'  STANDARD DEVIATION',F8.3)

      WRITE(IU06,504) RMSE, CORR, RV, SID
 504  FORMAT( 1X,' ROOT-MEAN-SQUARE ERROR ',F9.4,
     1           '  CORRELATION COEFFICIENT ',F9.4,/,
     2           '  REDUCTION OF VARIANCE  ',F9.4,
     3           '  SCATTER INDEX ',F9.4)

      WRITE(IU06,506)
 506  FORMAT(' SYMMETRIC REGRESSION LINE : ')
      WRITE(IU06,507) SLOPES, RMSESS, RMSEUS
 507  FORMAT( 1X,' SLOPE', F7.3,'           ',6X,
     1           ' SYMMETRIC RMSE', F7.3,' UNSYMMETRIC RMSE',F7.3)

      WRITE(IU06,508)
 508  FORMAT(' 1 PARAMETER REGRESSION LINE : ')
      WRITE(IU06,507) SLOPE1, RMSES1, RMSEU1
      WRITE(IU06,509)

 509  FORMAT(' 2 PARAMETER REGRESSION LINE : ')
      WRITE(IU06,510) SLOPE2, RINTR2, RMSES2, RMSEU2
 510  FORMAT( 1X,' SLOPE', F7.3,' INTERCEPT',F7.3,
     1           ' SYMMETRIC RMSE', F7.3,' UNSYMMETRIC RMSE',F7.3)

      RETURN
      END
C ======================================================================
C
      SUBROUTINE STATSC (X, Y, NDIM, NPT, NTOT, kcirc, XMAX, YMAX, DMAX,
     1                   SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
     &                   sumxc, sumyc)

C ----------------------------------------------------------------------
C
C**** *STATSC* - COMPUTE SUMS FOR STATISTICS.
C
C     LIANA ZAMBRESKY        GKSS/ECMWF       AUGUST 1987
C     H. GUNTHER             GKSS/ECMWF       AUGUST 1990
C
C*    PURPOSE.
C     --------
C
C       THIS SUB ADDS TO THE STATISTIC SUMS THE VALUES
C       FROM THE INPUT FIELDS.
C
C       1.  MAXIMUM VALUES OF X AND Y
C       2.  MAXIMUM VALUE OF ABSOLUTE DIFFERNCE
C       3.  SUM OF X VALUES.
C       4.  SUM OF Y VALUES.
C       5.  SUM OF X*X VALUES.
C       6.  SUM OF Y*Y VALUES.
C       7.  SUM OF X*Y VALUES.
C       8.  SUM OF (Y-X)*(Y-X) VALUES.
C       9.  TOTAL NUMBER OF DATA PAIRS.
C
C
C***  INTERFACE.
C     ----------
C
C       *CALL* *STATSC (X, Y, NDIM, NPT, NTOT, XMAX, YMAX, DMAX,
C                       SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2)*
C
C*    VARIABLE.    TYPE.     PURPOSE.
C     ---------    -------   --------
C       *X*        REAL      INDEPENDENT ARRAY.
C       *Y*        REAL      DEPENDENT ARRAY.
C       *NDIM*     INTEGER   ARRAY DIMENSION.
C       *NPT*      INTEGER   NUMBER OF POINTS IN INPUT.
C       *NTOT*     INTEGER   NUMBER OF POINTS IN STATISTIC SUMS.
c       *kcirc*    integer   input 2 if x and y values are in degree else 1
C       *XMAX*     REAL      MAX VALUE OF INDEPENDENT VARIABLE.
C       *YMAX*     REAL      MAX VALUE OF DEPENDENT VARIABLE.
C       *DMAX*     REAL      MAX ABSOLUTE VALUE OF DIFFERENCE.
C       *SUMX*     REAL      SUM OF X VALUES.
C       *SUMY*     REAL      SUM OF Y VALUES.
C       *SUMXX*    REAL      SUM OF X*X VALUES.
C       *SUMYY*    REAL      SUM OF Y*Y VALUES.
C       *SUMXY*    REAL      SUM OF X*Y VALUES.
C       *SUMXY2*   REAL      SUM OF (Y-X)*(Y-X) VALUES.
C       *sumxc*    REAL      cos SUM OF X VALUES if values are in degree.
C       *sumyc*    REAL      cos SUM OF Y VALUES if values are in degree.
C
C     METHOD.
C     -------
C
C       THE COMPUTATION IS STRAIGHTFORWARD.
C
C     EXTERNALS.
C     ----------
C
C       NONE.
C
C     REFERENCES.
C     -----------
C
C       ZAMBRESKY,L.F.,  ECMWF TECH REPORT NUMBER 63
C
C ----------------------------------------------------------------------
C
      PARAMETER (PI  = 3.141592, DEG = 180./PI, RAD = PI/180.)
c
      DIMENSION x (NDIM), Y (NDIM)
      DIMENSION xs(NDIM), Ys(NDIM)
      DIMENSION Xc(NDIM), Yc(NDIM)
C
C ----------------------------------------------------------------------
C
C*    1. MAXIMA AND TOTAL NUMBER.
C        ------------------------
C
 1000 CONTINUE
      NTOT = NTOT+NPT
      DO 1001 I=1,NPT
         XMAX = MAX(X(I),XMAX)
         YMAX = MAX(Y(I),YMAX)
         DMAX = MAX(ABS(Y(I)-X(I)),DMAX)
 1001 CONTINUE
C
C ----------------------------------------------------------------------
C
C*    2. FORM A VARIETY OF SUMS.
C        -----------------------
C
 2000 CONTINUE
      if (kcirc .eq. 1 ) then
        DO 2001 I=1,NPT
          SUMX    = SUMX+X(I)
          SUMY    = SUMY+Y(I)
          SUMXX   = SUMXX+X(I)*X(I)
          SUMYY   = SUMYY+Y(I)*Y(I)
          SUMXY   = SUMXY+X(I)*Y(I)
          SUMXY2  = SUMXY2+(Y(I)-X(I))*(Y(I)-X(I))
 2001   CONTINUE
      elseif (kcirc .eq. 2 ) then
        DO 2002 I=1,NPT
          xs(i) = sin(x(i) *rad)
          ys(i) = sin(y(i) *rad)
          xc(i) = cos(x(i) *rad)
          yc(i) = cos(y(i) *rad)
          SUMX  = SUMX+Xs(I)
          SUMY  = SUMY+Ys(I)
          SUMXc = sumxc+Xc(I)
          SUMYc = sumyc+Yc(I)
 2002   CONTINUE
      endif


      RETURN
      END
C ======================================================================
C
      SUBROUTINE STATSE (NTOT, kcirc,
     &                   SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
     &                   sumxc, sumyc,
     1                   XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
     2                   DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
     3                   SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
     4                   SLOPE2, RINTR2, RMSES2, RMSEU2)

C ----------------------------------------------------------------------
C
C**** *STATSE* - EVALUATE STATISTICS FORM CUMULATED SUMS.
C
C     LIANA ZAMBRESKY      GKSS/ECMWF     AUGUST 1987
C     H. GUNTHER           GKSS/ECMWF     AUGUST 1990
C
C*    PURPOSE.
C     --------
C
C       THIS SUB COMPUTES THE FOLLOWING STATISTICS
C
C       1.  MAXIMUM VALUES OF X AND Y
C       2.  MEAN AND STANDARD DEVIATION OF BOTH X AND Y VALUES
C       3.  MAXIMUM VALUE OF ABSOLUTE DIFFERNCE
C       4.  MEAN VALUE OF DIFFERENCESM (BIAS)
C       5.  ROOT-MEAN-SQUARE ERROR (RMSE)
C       6.  STANDARD DEVIATION OF DIFFERENCES
C       7.  SCATTER INDEX (SI)
C       8.  REDUCTION OF VARIACE.
C       9.  CORRELATION COEFFICIENT
C      10.  THE SYMMETRIC SLOPE AND THE UNSYMMETRIC AND SYMMETRIC RMSE
C           OF THIS REGESSION LINE. (SLOPES, RSMEUS, RSMESS)
C      11.  THE SLOPE OF THE REGRESSION LINE THROUGH THE ORIGAN AND
C           THE UNSYMMETRIC AND SYMMETRIC RMSE. (SLOPE1, RSMEU1, RSMES1)
C      12.  SLOPE AND INTERCEPT OF THE 2 PARAMETER REGRESSION LINE AND
C           THE UNSYMMETRIC AND SYMMETRIC RMSE. (SLOPE2, RINTS2, RSMEU2,
C           RSMES2)
C
C***  INTERFACE.
C     ----------
C
C       *CALL* *STATSE (NTOT, SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
C                      XMAX, YMAX, XMEAN, YMEAN, STDEVX, STDEVY,
C                      DMAX, BIAS, RMSE, STDEVD, SID, RV, CORR,
C                      SLOPES, RMSESS, RMSEUS, SLOPE1, RMSES1, RMSEU1,
C                      SLOPE2, RINTR2, RMSES2, RMSEU2)*
C
C*    VARIABLE.    TYPE.     PURPOSE.
C     ---------    -------   --------
C       *NTOT*     INTEGER   NUMBER OF POINTS.
c       *kcirc*    integer   input 2 if x and y values are in degree else 1
C       *SUMX*     INTEGER   SUM OF X VALUES.
C       *SUMY*     INTEGER   SUM OF Y VALUES.
C       *SUMXX*    INTEGER   SUM OF X*X VALUES.
C       *SUMYY*    INTEGER   SUM OF Y*Y VALUES.
C       *SUMXY*    INTEGER   SUM OF X*Y VALUES.
C       *SUMXY2*   INTEGER   SUM OF (Y-X)*(Y-X) VALUES.
C       *sumxc*    REAL      cos SUM OF X VALUES if values are in degree.
C       *sumyc*    REAL      cos SUM OF Y VALUES if values are in degree.
C       *XMAX*     REAL      MAX VALUE OF INDEPENDENT VARIABLE
C       *YMAX*     REAL      MAX VALUE OF DEPENDENT VARIABLE
C       *XMEAN*    REAL      MEAN VALUE OF INDEPENDENT VARIABLE
C       *YMEAN*    REAL      MEAN VALUE OF DEPENDENT VARIABLE
C       *STDEVX*   REAL      STANDARD DEVIATION OF INDEPENDENT VARIABLE
C       *STDEVY*   REAL      STANDARD DEVIATION OF DEPENDENT VARIABLE
C       *DMAX*     REAL      MAX ABSOLUTE VALUE OF DIFFERENCE
C       *BIAS*     REAL      MEAN VALUE OF DIFFERENCES
C       *RMSE*     REAL      ROOT-MEAN-SQUARE ERROR
C       *STDEVD*   REAL      STANDARD DEVIATION OF DIFFERENCE
C       *SID*      REAL      SCATTER INDEX
C       *RV*       REAL      REDUCTION OF VARIACE.
C       *CORR*     REAL      CORRELATION COEFFICIENT
C       *SLOPES*   REAL      SLOPE OF SYMMETRIC REGRESSION LINE.
C       *RMSEUS*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *RMSESS*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF SYMMETRIC REGESSION LINE.
C       *SLOPE1*   REAL      SLOPE OF REGRESSION LINE THROUGH ORIGIAN.
C       *RMSEU1*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *RMSES1*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 1 PARAMETER REGRESSION LINE.
C       *SLOPE2*   REAL      SLOPE OF 2 PARAMETER LEAST SQUARES FIT.
C       *RINTR2*   REAL      INTERCEPT OF 2 PARAMETER LEAST SQUARES FIT
C       *RMSEU2*   REAL      UNSYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C       *RMSES2*   REAL      SYSTEMATIC ROOT MEAN SQUARE ERROR
C                            OF 2 PARAMETER REGRESSION LINE.
C
C     METHOD.
C     -------
C
C       THE COMPUTATION OF THESE STATISTICS IS STRAIGHTFORWARD.
C
C
C     EXTERNALS.
C     ----------
C
C       NONE.
C
C     REFERENCES.
C     -----------
C
C       ZAMBRESKY,L.F.,  ECMWF TECH REPORT NUMBER 63
C
C ----------------------------------------------------------------------
C
      PARAMETER (PI  = 3.141592, DEG = 180./PI, RAD = PI/180.)
c
C ----------------------------------------------------------------------
C
C*    1.0 INITIALISE VALUES.
C         ------------------
C
 1000 CONTINUE
      XMEAN  = 0.
      YMEAN  = 0.
      STDEVX = 0.
      STDEVY = 0.
      BIAS   = 0.
      RMSE   = 0.
      STDEVD = 0.
      SID    = 0.
      RV     = 0.
      CORR   = 0.
      SLOPES = 0.
      RMSEUS = 0.
      RMSESS = 0.
      SLOPE1 = 0.
      RMSEU1 = 0.
      RMSES1 = 0.
      SLOPE2 = 0.
      RINTR2 = 0.
      RMSEU2 = 0.
      RMSES2 = 0.
C
C ----------------------------------------------------------------------
C
C*    2. ENOUGH DATA?
C        ------------
C
 2000 CONTINUE
      IF (NTOT.LT.1) RETURN
      XPT  = REAL(NTOT)
      XPTD = 1./XPT
C
C ----------------------------------------------------------------------
C
C*    3. COMPUTE MEANS, LEAST SQUARES FIT AND SCATTER INDEX.
C        ---------------------------------------------------
C
 3000 CONTINUE
      if ( kcirc .eq. 1 ) then
        XMEAN = SUMX*XPTD
        YMEAN = SUMY*XPTD
        BIAS  = YMEAN-XMEAN
      elseif ( kcirc .eq. 2 ) then
        xmean = ATAN2(sumx,sumxc)*DEG
        ymean = ATAN2(sumy,sumyc)*DEG
        if (xmean.lt.0.) xmean = xmean +360.
        if (ymean.lt.0.) ymean = ymean +360.
        bias  = abs(xmean-ymean)
        if(bias.gt.180.)bias=360.-bias
      endif
      if (kcirc .eq. 2) return

      RMSE = SQRT(SUMXY2*XPTD)

      IF (NTOT.GT.1) THEN
         XHELP = XPT*SUMXX-SUMX*SUMX
         YHELP = XPT*SUMYY-SUMY*SUMY

         STDEVX = ABS(XHELP/(XPT*(XPT-1.)))
         STDEVX = SQRT(STDEVX)
         STDEVY = ABS(YHELP/(XPT*(XPT-1.)))
         STDEVY = SQRT(STDEVY)
         STDEVD = ABS((SUMXY2-XPT*BIAS**2)/(XPT-1.))
         STDEVD = SQRT(STDEVD)

         IF (XMEAN.NE.0.) SID    = STDEVD/XMEAN
         IF (XHELP.GT.0.) RV     = 1. - XPT*SUMXY2/XHELP
         IF (STDEVX.NE.0. .AND. STDEVY.NE.0.)
     1      CORR   = (SUMXY-XPT*XMEAN*YMEAN)/(STDEVX*STDEVY*(XPT-1.))
         IF (SUMXX.NE.0.) THEN
            SLOPES = SQRT(SUMYY/SUMXX)
            RMSEUS = ABS((SUMYY-2*SLOPES*SUMXY+SLOPES**2*SUMXX)*XPTD)
            RMSEUS = SQRT(RMSEUS)
            RMSESS = ABS((1.-SLOPES)**2*SUMXX*XPTD)
            RMSESS = SQRT(RMSESS)
            SLOPE1 = SUMXY/SUMXX
            RMSEU1 = ABS((SUMYY-2*SLOPE1*SUMXY+SLOPE1**2*SUMXX)*XPTD)
            RMSEU1 = SQRT(RMSEU1)
            RMSES1 = ABS((1.-SLOPE1)**2*SUMXX*XPTD)
            RMSES1 = SQRT(RMSES1)
         ENDIF
         IF (XHELP.GT.0.) THEN
            SLOPE2 = (XPT*SUMXY-SUMX*SUMY)/XHELP
            RINTR2 = YMEAN-SLOPE2*XMEAN
            RMSEU2 = ABS((SUMYY-2*SLOPE2*SUMXY+SLOPE2**2*SUMXX)*XPTD
     1             - RINTR2**2)
            RMSEU2 = SQRT(RMSEU2)
            RMSES2 = ABS((1.-SLOPE2)**2*SUMXX*XPTD + RINTR2**2
     1             - 2.*(1.-SLOPE2)*RINTR2*XMEAN)
            RMSES2 = SQRT(RMSES2)
         ENDIF
      ENDIF

      RETURN
      END
C ======================================================================
C
      SUBROUTINE STATSI (NTOT, kcirc, XMAX, YMAX, DMAX,
     1                   SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2,
     &                   sumxc, sumyc)

C ----------------------------------------------------------------------
C
C**** *STATSI* - INITIALIZES STATISTICS.
C
C      LIANA ZAMBRESKY        GKSS/ECMWF       AUGUST 1987
C
C*    PURPOSE.
C     --------
C
C          PRESET SUMMATION AND MAXIMA VALUES FOR STATISTICS.
C
C***  INTERFACE.
C     ----------
C
C     *CALL* *STATSI (NTOT, XMAX, YMAX, DMAX,
C                     SUMX, SUMY, SUMXX, SUMYY, SUMXY, SUMXY2)*
C
C*    VARIABLE.    TYPE.     PURPOSE.
C     ---------    -------   --------
C       *NTOT*     INTEGER   NUMBER OF POINTS.
c       *kcirc*    integer   input 2 if x and y values are in degree else 1
C       *XMAX*     REAL      MAX VALUE OF INDEPENDENT VARIABLE.
C       *YMAX*     REAL      MAX VALUE OF DEPENDENT VARIABLE.
C       *DMAX*     REAL      MAX ABSOLUTE VALUE OF DIFFERENCE.
C       *SUMX*     REAL      SUM OF X VALUES.
C       *SUMY*     REAL      SUM OF Y VALUES.
C       *SUMXX*    REAL      SUM OF X*X VALUES.
C       *SUMYY*    REAL      SUM OF Y*Y VALUES.
C       *SUMXY*    REAL      SUM OF X*Y VALUES.
C       *SUMXY2*   REAL      SUM OF (Y-X)*(Y-X) VALUES.
C       *sumxc*    REAL      cos SUM OF X VALUES if values are in degree.
C       *sumyc*    REAL      cos SUM OF Y VALUES if values are in degree.
C
C     METHOD.
C     -------
C
C       NONE.
C
C     EXTERNALS.
C     ----------
C
C       NONE
C
C     REFERENCES.
C     -----------
C
C       NONE.
C
C ----------------------------------------------------------------------
C
C*    1. MAXIMA AND TOTAL NUMBER.
C        ------------------------
C
 1000 CONTINUE
      NTOT = 0
      XMAX = -999999.
      YMAX = -999999.
      DMAX = -999999.
C
C ----------------------------------------------------------------------
C
C*    2. A VARIETY OF SUMS.
C        ------------------
C
 2000 CONTINUE
      SUMX    = 0.
      SUMY    = 0.
      sumxc   = 0.
      sumyc   = 0.
      SUMXX   = 0.
      SUMYY   = 0.
      SUMXY   = 0.
      SUMXY2  = 0.

      RETURN
      END
C ======================================================================
C
      SUBROUTINE URAOPC (CSUBNA, KSUB, LOSUVI, LOTEST)
C
C ======================================================================
C
C****  *URAOPC*  - SET OUTPUT CONTROL SWITCHES.
C
C     B. HANSEN           ECMWF       MARCH   1992
C
C**   PURPOSE.
C     --------
C       SETTING OF OUTPUT CONTROL LOGICALS.
C
C**   INTERFACE.
C     ----------
C       CALL  URAOPC (CSUBNA, KSUB, LASUVI, LATEST)
C
C       I/   *CSUBNA*  NAME OF CALLING SUB-ROUTINE.
C       I/   *KSUB*    SUB-ROUTINE LEVEL OF CALLLING SUB-ROUTINE.
C        /O  *LASUVI*  LOGICAL SWITCH (TRUE: SV-OUTPUT,
C                                      FALSE: NO SV_OUTPUT).
C        /O  *LATEST*  LOGICAL SWITCH (TRU: TEST-OUTPUT,
C                                      FALSE: NO TEST_OUTPUT).
C
C     EXTERNALS.
C     ----------
C       NONE.
C
C     METHOD.
C     -------
C       NONE.
C
C     REFERENCES.
C     -----------
C        NONE
C
C ----------------------------------------------------------------------
C
C* 0.    COMMON OUTPUT CONTROL.
C  ----------------------------
C
      PARAMETER ( JPCTL = 20 )
      CHARACTER*7 CTEST, CSUVI
      COMMON /OUTPUT/ LSUB,LTEST,LSUVI,CTEST(JPCTL),CSUVI(JPCTL)
      LOGICAL LOTEST, LOSUVI
      CHARACTER*7 CSUBNA
C
C        *LSUB*  CURRENT SUB-ROUTINE LEVEL
C        *LTEST* MAXIMUM LEVEL WITH TEST OUTPUT
C        *LSUVI* MAXIMUM LEVEL WITH STANDARD OUTPUT
C        *CTEST* ARRAY OF SUB-ROUTINE NAMES GIVING TEST OUTPUT
C        *CSUVI* ARRAY OF SUB-ROUTINE NAMES GIVING STANDARD OUTPUT
C
C ----------------------------------------------------------------------
C
C* 1.0   INITIALIZATION.
C  ---------------------
C
 1000 CONTINUE
CCCCCCCCCC      LOTEST = .TRUE.
      LOTEST = .FALSE.
      LOSUVI = .FALSE.
CCCCCC      WRITE('flow',9000)' IN ROUTINE >',CSUBNA,'< KSUB IS >',KSUB,'<'
      IF (KSUB .LE. LSUVI) THEN
        LOSUVI = .TRUE.
      ELSE
        DO 1001 J=1,JPCTL
          IF (CSUVI(J) .EQ. CSUBNA ) THEN
              LOSUVI = .TRUE.
              GOTO 1002
          ENDIF
 1001   CONTINUE
 1002   CONTINUE
      ENDIF
C
      IF (KSUB .LE. LTEST) THEN
        LOTEST = .TRUE.
      ELSE
        DO 1003 J=1,JPCTL
          IF (CTEST(J) .EQ. CSUBNA ) THEN
              LOTEST = .TRUE.
              GOTO 1004
          ENDIF
 1003   CONTINUE
 1004   CONTINUE
      ENDIF
      RETURN
 9000 FORMAT(A13,A7,A11,I2,A1)
      END
C===========================================================================

       SUBROUTINE RDBUOYS(IUNIT,IDATINI,IDATFIN,DATEMIN,STEP,
     .                    IDT,IDS,YMEAS)

C***   *RDBUOYS* - READ THE MEASUREMENTS DATA FROM ITALIAN NETWORK (TEI)   
C
C -------------------------------------------------------------------------

C      DEFINITION OF VARIABLES -

C      DATINI        INITIAL DATE (YYMMDDHH) OF THE PERIOD OF INTEREST
C      DATFIN        FINAL    "   (   "    )    "      "         "
C      DATEMIN       REFERENCE STARTING DATE FOR PLOT (YYMMDD00.0)
C      STEP          TIME STEP, IN HOUR, WITH WHICH DATA ARE CONSIDERED
C      IDT           MAX LENGTH OF THE SERIES
C      IDS           NUMBER OF STATIONS
C      YMEAS(I,J,K)  DATA RETRIEVED FOR I-TIME, J-PARAMETER, K-STATION -
C                    J=1-5 -

C      IND           PROGRESSIVE INDEX OF RETRIEVED FIELD WITH RESPECT TO
C                    DATEMIN
C      KOUNT         COUNTER OF RETRIEVED FIELDS
C      GLO(), GLA()  GEOGRAPHICAL LONGITUDE, LATITUDE OF THE BUOY LOCATION
C      HS( )         SIGNIFICANT WAVE HEIGHT
C      DM( )         WAVE MEAN DIRECTION
C      TM( )         WAVE MEAN PERIOD

C      IUNIT         LOGICAL INPUT UNIT FOR BUOYS DATA

C---------------------------------------------------------------------
C      
       CHARACTER*80 LINE
       CHARACTER*5 NAME(15), DUMMY
       REAL GLO(15), GLA(15)
       REAL YMEAS(IDT,5,IDS)
       INTEGER   HS(15) , DM(15) , TM(15)
       INTEGER STEP

       DOUBLE PRECISION DATEMIN
C      
C-------------------------------------------------------------------
C      FORMATS
C      -------

 700  FORMAT(8X,15(2X,A5,2X))
 710  FORMAT(8X,15(1X,2F4.1))
 720  FORMAT(4I2.2,15(3I3))

C----------------------------------------------------------------------
       
C     THE BUOYS FILE CONTAINS DATA FOR 8 ITALIAN STATIONS -
C     THE LIST IS :
C     N  1   LA SPEZIA      (SPEZI)
C     N  2   ALGHERO        (ALGHE)
C     N  3   PESCARA        (PESCA)
C     N  4   MONOPOLI       (MONOP)
C     N  5   PONZA          (PONZA)
C     N  6   CROTONE        (CROTO)
C     N  7   CATANIA        (CATAN)
C     N  8   MAZARA         (MAZAR) 

C     N  9   ANCONA         (ANCON)
C     N 10   CETRARO        (CETRA)

C     N 11   PO-MAESTRA     (POMAE)
C     N 12   CIVITAVECCHIA  (CIVIT)
C     N 13   COMINO         (COMIN)
C     N 14   PALERMO        (PALER)
C     N 15   CAGLIARI       (CAGLI)


C     DEFINE THE NUMBER OF LOCATIONS IN THE BUOYS FILE
C     ACCORDING TO YEAR
      IYY = IDATINI/10**6
      IF(IYY.GT.70) IYEAR = IYY+1900
      IF(IYY.LT.70) IYEAR = IYY+2000
      IF(IYEAR.LE.1998) NLOC = 8
      IF(IYEAR.GT.1998) NLOC = 10
      IF(IYEAR.GT.2001) NLOC = 15

C     INITIALIZE COUNTER OF MEASUREMENT
      KOUNTER=0
       
C     READ HEADER LINES 
       
      READ(IUNIT,700) (NAME(I),I=1,NLOC)
      READ(IUNIT,710) (GLO(I),GLA(I),I=1,NLOC)
      READ(IUNIT,'(A80)') LINE

ccccc   do 3 is=1,nloc
ccccc     write(6,'(10x,a5,2f10.2)') name(is),glo(is),gla(is)
ccccc  3    continue

C     MEASURED DATA ARE READ FROM FILE

 3300 CONTINUE
 
      READ(IUNIT,720,END=4000) JY,JM,JD,JH,
     .                        (HS(I),TM(I),DM(I),I=1,NLOC)

      JDATE = JY*10**6+JM*10**4+JD*10**2+JH
cccc      WRITE(JDATE,'(4I2.2)') JY, JM, JD, JH
      IF(JDATE.LT.IDATINI) GO TO 3300
      IF(JDATE.GT.IDATFIN) GO TO 4000

      DO 3500 I=1,NLOC
      IF(HS(I).EQ.0.0) HS(I) = 1.0
 3500 CONTINUE

C     CHECK ON COUNTER

      KOUNTER=KOUNTER+1

         IF(KOUNTER.GT.IDT) THEN
         WRITE(6,'(1X//1X,''LENGTH OF SERIES EXCEEDED'')')
         STOP 1357
         END IF

C     THE SEQUENTIAL NUMBER OF THIS FIELD WITH RESPECT TO DATEMIN
C     IS EVALUATED IN SUB INCDATE3
       
ccccc      CALL INCDATE3(JY,JM,JD,JH,DATEMIN,STEP,IND)
      IND = KOUNTER
C     DATA ARE ASSIGNED TO THE REFERENCE LOCATIONS  
 
C     PO-MAESTRA
      YMEAS(IND,1,2) = HS(11)/100.
      YMEAS(IND,4,2) = TM(11)/10.
      YMEAS(IND,5,2) = DM(11)
C     ANCONA
      YMEAS(IND,1,3) = HS(9)/100.
      YMEAS(IND,4,3) = TM(9)/10.
      YMEAS(IND,5,3) = DM(9)
C     PESCARA 
      YMEAS(IND,1,4) = HS(3)/100.
      YMEAS(IND,4,4) = TM(3)/10.
      YMEAS(IND,5,4) = DM(3)
C     MONOPOLI
      YMEAS(IND,1,5) = HS(4)/100.
      YMEAS(IND,4,5) = TM(4)/10.
      YMEAS(IND,5,5) = DM(4)


C     GO TO READ THE NEXT DATE
      GO TO 3300
 
 4000 CONTINUE

      RETURN
      END
C===========================================================================
       
       SUBROUTINE RDTOWER(IUNIT,IDATINI,IDATFIN,DATEMIN,STEP,
     .                    IDT,IDS,YMEAS)

C***   *RDTOWER* - READ THE MEASUREMENTS DATA FROM ISDGM TOWER
C
C -------------------------------------------------------------------------
         
C      DEFINITION OF VARIABLES -

C      IDATINI       INITIAL DATE (YYMMDDHH) OF THE PERIOD OF INTEREST
C      IDATFIN       FINAL    "   (   "    )    "      "         "
C      DATEMIN       REFERENCE STARTING DATE FOR PLOT (YYMMDD00.0)
C      STEP          TIME STEP, IN HOUR, WITH WHICH DATA ARE CONSIDERED
C      IDT           MAX LENGTH OF THE SERIES
C      IDS           NUMBER OF STATIONS
C      YMEAS(I,J,K)  DATA RETRIEVED FOR I-TIME, J-PARAMETER, K-STATION -
C                    J=1-5 -

C      IND           PROGRESSIVE INDEX OF RETRIEVED FIELD WITH RESPECT TO
C                    DATEMIN
C      KOUNT         COUNTER OF RETRIEVED FIELDS
C      GLO(), GLA()  GEOGRAPHICAL LONGITUDE, LATITUDE OF THE BUOY LOCATIONS
C      HS            SIGNIFICANT WAVE HEIGHT (M)
C      DM            WAVE MEAN DIRECTION     (FLOW - DEGREE CWRGN)
C      TM            WAVE MEAN PERIOD        (S)
C      FM            WAVE MEAN FREQUENCY     (HZ)
C      FP            WAVE PEAK FREQUENCY     (HZ)

C      IUNIT         LOGICAL INPUT UNIT FOR TOWER

C      JX, XX        DUMMY VARIABLES
      
C---------------------------------------------------------------------
C      
       CHARACTER*80 LINE
       CHARACTER*5 NAME(IDS)
       REAL GLO(IDS), GLA(IDS)
       REAL YMEAS(IDT,5,IDS)
       CHARACTER*8   DATINI, DATFIN
       INTEGER STEP

       DOUBLE PRECISION DATEMIN
C
C-------------------------------------------------------------------  
C      FORMATS
C      -------
 710  FORMAT(i5,i6,3i3,1x,3f6.2,f6.0,3x,2f6.2)
 720  FORMAT(4I5,3X,F5.2,2F8.3,F5.0)   

C----------------------------------------------------------------------

C     THE TOWER IS CONSIDERED POSITION #1 IN THE LIST OF THE LOCATIONS
C     OF INTEREST

      NAME(1) = 'TOWER'
      write(6,'(10x,a5)') name(1)

C     INITIALIZE COUNTER OF MEASUREMENT
      KOUNTER=0

C     READ HEADER LINES
      DO 100 I=1,30
      READ(IUNIT,'(A80)') LINE
 100  CONTINUE
       
C     DATA MEASUREMENTS ARE READ FROM FILE
       
 3300 CONTINUE
       
      READ(IUNIT,710,END=4000) IND, JY, JM, JD, JH,
     .                        HS, FM, TM, DM, FP, TP

      JJY = JY
      IF(JJY.LE.1999) JY = JJY-1900
      IF(JJY.GT.1999) JY = JJY-2000
      JDATE = JY*10**6+JM*10**4+JD*10**2+JH
cccc      WRITE(JDATE,'(4I2.2)') JY, JM, JD, JH
      IF(JDATE.LT.IDATINI) GO TO 3300
      IF(JDATE.GT.IDATFIN) GO TO 4000

cccc      write(6,720) JY,JM,JD,JH, HS, TM, FM, DM
 
C     CHECK ON COUNTER

      KOUNTER=KOUNTER+1
         IF(KOUNTER.GT.IDT) THEN
         WRITE(6,'(1X//1X,''LENGTH OF SERIES EXCEEDED'')')
         STOP 1358
         END IF

C     THE SEQUENTIAL NUMBER OF THIS FIELD WITH RESPECT TO DATEMIN
C     IS EVALUATED IN SUB INCDATE3

ccc   CALL INCDATE3(JY,JM,JD,JH,DATEMIN,STEP,IND)
      IND = KOUNTER

      YMEAS(IND,1,1) = HS
C     BECAUSE OF LOW SIGNAL FROM TRANSDUCERS
C     WAVE HEIGHT IS INCREASED DURING THE PERIOD: SEPT 2002 - MAY 2003
cccc  IF(JJY.EQ.2002 .AND. JM.GE.9) YMEAS(IND,1,1) = HS/0.63
cccc  IF(JJY.EQ.2003 .AND. JM.LE.5) YMEAS(IND,1,1) = HS/0.63
      YMEAS(IND,4,1) = TM
      YMEAS(IND,5,1) = DM

C     GO TO READ THE NEXT DATE
      GO TO 3300
 
 4000 CONTINUE
      
C     UNREAL DATA ARE DISREGARDED

      ITT = IDT
      DO 4360 I=2,ITT-1
      TM1 = YMEAS(I-1,4,1)
      TM2 = YMEAS(I,4,1)
      TM3 = YMEAS(I+1,4,1)
      IF(TM2.LE.0.) GO TO 4360
      IF(TM2.GT.10.)GO TO 4370
      IF(TM1.LE.0. .AND. TM2.GT.5.) GO TO 4370
      IF(TM1.GT.0.) THEN
         IF(TM2.GT.6. .AND. (TM2/TM1).GT.2.) GO TO 4370
         END IF
      IF(TM3.EQ.0. .AND. TM2.GT.5.) GO TO 4370
      IF(TM3.GT.0.) THEN
         IF(TM2.GT.0. .AND. (TM2/TM3).GT.1.5) GO TO 4370
         END IF 
      GO TO 4360
      
 4370  CONTINUE
      YMEAS(I,4,1) = -999.0
 4360  CONTINUE


      RETURN
      END
C===========================================================================
      
      SUBROUTINE RDWAMOD(IDATINI,IDATFIN,DATEMIN,STEP,
     .                   IDT,IDS,y0,y1,y2,y3)

C***   *RDWAMOD* - READ THE WAM MODEL RESULTS
C     
C -------------------------------------------------------------------------
      
C      DEFINITION OF VARIABLES -
 
C      IDATINI       INITIAL DATE (YYMMDDHH) OF THE PERIOD OF INTEREST
C      IDATFIN       FINAL    "   (   "    )    "      "         "
C      DATEMIN       REFERENCE STARTING DATE FOR PLOT (YYMMDD00.0)
C      IDT           MAX LENGTH OF THE SERIES
C      IDS           NUMBER OF POINTS
C      YMOD(I,J,K)   DATA RETRIEVED FOR I-TIME, J-PARAMETER, K-POINT -
C                    J=1-5 -

C      IND           PROGRESSIVE INDEX OF RETRIEVED FIELD WITH RESPECT TO
C                    DATEMIN
C      KOUNT         COUNTER OF RETRIEVED FIELDS
C      HS            SIGNIFICANT WAVE HEIGHT (M)
C      DM            WAVE MEAN DIRECTION     (FLOW - DEGREE CWRGN)
C      TM            WAVE MEAN PERIOD        (S)
C      WW            WIND SPEED              (M/S)
C      WD            WIND DIRECTION          (FLOW - DEGREE CWRGN)
C     
C---------------------------------------------------------------------
C     
       CHARACTER*80 LINE

       INTEGER STEP
       REAL y0(idt,5,ids),y1(idt,5,ids),y2(idt,5,ids),y3(idt,5,ids)

       DOUBLE PRECISION DATEMIN
         
C-------------------------------------------------------------------
C      FORMATS
C      -------

 2001 format(1X,I8.8,1X,4(1X,F5.2,F6.3,F5.0))

C----------------------------------------------------------------------



C     INITIALIZE COUNTER OF WAM MODEL DATA
      KOUNTER=0
      IUNIT = 10
      NSTAZ = 5
c      NSTAZ = 1
              IYY = IDATINI/10**6
              IF(IYY.GT.50) IYEAR = 1900+IYY
              IF(IYY.LE.50) IYEAR = 2000+IYY
              IF(IYEAR.LT.1999) NSTAZ = 3
              write(6,'(''NUMBER OF STATIONS ------------>>''I5)')NSTAZ


C     READ HEADER LINES
      DO 1100 IS=1,NSTAZ
      IUNIT = 10+IS
      DO 1000 I=1,7
      READ(IUNIT,'(A80)') LINE
 1000 CONTINUE
 1100 CONTINUE
       
      
C     WAM MODEL DATA ARE READ FROM FILE
C        
         do 2000 istat=1,nstaz
         IUNIT = 10 + istat
         IT = 0

 2400    CONTINUE
         IT = IT+1

         READ(IUNIT,2001,end=2500) IDATE,
     A       y0(it,1,istat),y0(it,4,istat),y0(it,5,istat),
     1       y1(it,1,istat),y1(it,4,istat),y1(it,5,istat),
     2       y2(it,1,istat),y2(it,4,istat),y2(it,5,istat),
     3       y3(it,1,istat),y3(it,4,istat),y3(it,5,istat)

         y0(it,4,istat) = 1./y0(it,4,istat)
         y1(it,4,istat) = 1./y1(it,4,istat)
         y2(it,4,istat) = 1./y2(it,4,istat)
         y3(it,4,istat) = 1./y3(it,4,istat)

ccccc      write(IDATE,'(4i2.2)')JY,JM,JD,JH
      IF(IDATE.LT.IDATINI) IT = IT-1
      IF(IDATE.GT.IDATFIN) GO TO 2500
      GO TO 2400

 2500 CONTINUE
      
C     CHECK ON COUNTER

      KOUNTER=IT
         IF(KOUNTER.GT.IDT) THEN
         WRITE(6,'(1X//1X,''LENGTH OF SERIES EXCEEDED'')')
         STOP 1359
         END IF  
      
C     THE SEQUENTIAL NUMBER OF THIS FIELD WITH RESPECT TO DATEMIN
C     IS EVALUATED IN SUB INCDATE3
C     CALL INCDATE3(JY,JM,JD,JH,DATEMIN,STEP,IND)

 2000 CONTINUE

      RETURN
      END
C===========================================================================
       
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
C===========================================================================

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

      DOUBLE PRECISION DATEMIN

      COMMON /IMO/IMON
      
C ---------------------------------------------------------------------------

C     Y,M,D,H ARE EVALUATED -
      ID=DATEMIN+.01
      Y=ID/10**6
      ID=ID-Y*10**6
      M=ID/10**4
      ID=ID-M*10**4
      D=ID/10**2
      H=ID-D*10**2

C     CHECK ON THE NUMBER OF DAYS OF FEBRUARY
      IMON(2)=28
      IF(MOD(Y2,4).EQ.0) IMON(2)=29

C     CHECK THAT H2 IS A MULTIPLE OF IH -
C     IF NOT SO, H2 IS SET EQUAL TO THE NEAREST MULTIPLE OF IH (0-23) -
         IF(MOD(H2,IH).NE.0)THEN
         DH=IH/2.
         L1=(H2/IH)*IH
            IF((H2-L1).LE.DH)THEN
            H2=L1
            ELSE
            H2=L1+IH
            END IF
C     CHECK IF NOW H2=24. IF SO, DATE IS ADJUSTED -
            IF(H2.GE.24)THEN
            H2=0.
            D2=D2+1
               IF(D2.GT.IMON(M2))THEN
               D2=1
               M2=M2+1
                  IF(M2.GT.12)THEN
                  M2=1
                  Y2=Y2+1
                  END IF
               END IF
            END IF
         END IF

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
C===========================================================================
       
      SUBROUTINE PLOTDATE (I1,I2,X1,X2,DX) 

C     SUB PLOTDATE RECEIVES TWO DATES, I1,I2, WRITTEN AS YYMMDDHH,
C     EVALUATES THE TWO EXTREME DATES X1,X2 OF PLOT WRITTEN AS YYMMDD00.0,
C     AND THE DURATION OF THE PLOT DX WRITTEN AS YYMMDDHH.0 -

C     IMON(12) ARE THE NUMBER OF DAYS IN EACH MONTH OF THE YEAR -

C -----------------------------------------------------------------------------

      INTEGER IMON(12)
      INTEGER Y1,M1,D1,H1, Y2,M2,D2,H2
      INTEGER YY1

      CHARACTER*8      I1, I2
      DOUBLE PRECISION X1, X2
        
      COMMON /IMO/IMON

C -----------------------------------------------------------------------------
           
C     EVALUATION OF X1 -
      
      READ(I1,'(4I2.2)') Y1, M1, D1, H1

      YY1 = Y1
      IF(YY1.GE.70) Y1 = 1900+YY1
      IF(YY1.LT.70) Y1 = 2000+YY1

CCC   D1=D1-1

C     CHECK ON THE NUMBER OF DAYS IN FEBRUARY
      IMON(2)=28
      IF(MOD(Y1,4).EQ.0) IMON(2)=29
      
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

      READ(I2,'(4I2.2)') Y2, M2, D2, H2

      YY2 = Y2
      IF(YY2.GE.70) Y2 = 1900+YY2
      IF(YY2.LT.70) Y2 = 2000+YY2

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
