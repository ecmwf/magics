      PROGRAM OPEPLOT

C====================================================================
C
C----  ADRIOPER _ Y E A R P L . F O R  ----
C
C                   Luciana Bertotti - ISDGM-CNR - Venice - 8-Jul-1994
C                   Luigi Cavaleri   - ISDGM-CNR - Venice - 23 Jun 1997
C                   Luciana Bertotti - ISDGM-CNR - Venice - 25 Jun 1999
C                                      migration to WS and MAGICS
C                   Luciana Bertotti - ISDGM-CNR - Venice - 3-OCT- 2002
C                                      modified for 'ADRIOPER'
C======================================================================
C
C.... program OPEYEARP.FOR plots results from ADRIOPER data of one year -
C.... 4 plots per page, 1 for each quarter  -
C.... each plot shows Hs, Tm, Dm -
C
C======================================================================

      PARAMETER ( NST = 5)

      CHARACTER*5  NAMEST(NST)
ccc   CHARACTER*47 FILIN
      CHARACTER*51 FILIN

      DATA NAMEST /'TOWER','POMAE','ANCON','PESCA','MONOP'/

C ----------------------------------------------------------------------

      IUNIT = 12
ccc   FILIN = '/usr/users/luciana/work/oper/res/xxxxx.ope_yyyy'
ccc   FILIN = '/home/luciana/work/oper/res_OLD/yyyy/xxxxx.ope_yyyy'
ccc   FILIN = '/home/luciana/work/oper/res_NEW/yyyy/xxxxx.ope_yyyy'


C     1.1 READ YEAR AND OPEN INPUT FILE
C         -----------------------------

      

      IYEAR = 2010

C     1.2 OPEN PLOT PAGE
C         --------------

      CALL POPEN
      CALL PSETC ('output_name', 'test01')
c     CALL PSETC ('WORKSTATION_1', 'PS')

C     2.0 LOOP OVER EACH LOCATION
C         -----------------------

      DO 2100 IST=1,1

C     2.1 UPDATE AND OPEN INPUT FILE
C         --------------------------

      IF(IYEAR.LE.2004) WRITE(FILIN,1001) IYEAR,NAMEST(IST), IYEAR
 1001 FORMAT('/home/luciana/work/oper/res_OLD/',i4,'/',A5,'.ope_',i4)

      IF(IYEAR.GE.2005) WRITE(FILIN,1002) NAMEST(IST), IYEAR
 1002 FORMAT('./',A5,'.ope_',i4)

      WRITE(6,'(''  FILE  =   '',a55)') FILIN
      
      flush(6)

      OPEN(IUNIT,FILE=FILIN,STATUS='OLD')

C     2.2 DATA ARE READ FROM INPUT FILE
C         -----------------------------

clllllllllllllllllllllllllllllllllllllllllllllllllllll
ccc   write(57,'(1x,16(''+''),i5,3x,16(''+''))')ist
clllllllllllllllllllllllllllllllllllllllllllllllllllll

          CALL DATIN(IUNIT)

C     2.3 DATA ARE PLOTTED
C         ----------------

          CALL TIMEPLT(NAMEST(IST))

 2100  CONTINUE

C     3.0 CLOSE PLOT PAGE
C         ---------------
          CALL PCLOSE
 

      STOP
      END

C##########################################################################

       SUBROUTINE DATIN(IUNIT)

C     sub DATIN reads date and data to plot from input file

C====================================================================
C	---  definition of variables  ---
C
C.... HS()  = significant wave height (meter) -
C.... TM()  = mean spectral period (second) -
C.... DM()  = mean flow direction (degrees - cwrgn) -
C.... Y()   = year      !  of each single datum - given at synoptic
C.... M()   = month     !  times, at 3-hour interval - their maximum
C.... D()   = day       !  number is 366*8+1 = 2928+1 = 2929 -
C.... R()   =           !  UTC time - 0 to 21 - 3 by 3
C.... FM    = mean frequency -
C.... FP    = peak frequency -
C.... TP    = peak period -
C.... W     = dummy logical variable -
C
C====================================================================

        real hs(2929),tm(2929),dm(2929)
        integer y(2929), m(2929), d(2929), r(2929)
        logical*1 W
C
C
C================================================================

        COMMON /DATE/Y,M,D,R
        COMMON /WAVE/HS,TM,DM
        COMMON /NREC/NREC
C................................................................

C.... data y(),m(),d(),r(),hs(),tm(),dm() are read from file -
C....    fm, fp and tp are read but neglected -
C....    k is progressive number of records -
C....    nrec is overall number of records -

        do 101 i=1,7           ! this is to skip the 30 headers lines
 101    read(IUNIT,1001)w      ! on the input file

        do 102 i=1,3000
        read(IUNIT,1002,end=10) y(i), m(i), d(i), r(i),
     A                         hs(i), fm, dm(i),
     1                         hs1,  fm1, dm1,
     2                         hs2,  fm2, dm2,
     3                         hs3,  fm3, dm3
     4                         hs4,  fm4, dm4
     5                         hs5,  fm5, dm5

        tm(i) = 1./fm
        if(hs(i).eq.0.) then
                        hs(i) = 0.1
                        tm(i) = -99.0
                        dm(i) = -99.0
                        end if
        nrec=i
 102    continue
C
 10	continue
C       NREC is decreased of 1 as last record refers to first date of next year
        nrec=nrec-1
        print*, ' END OF FILE REACHED'
        PRINT*, ' NREC-1 =',NREC

        RETURN

C       ---  FORMAT  ---
 1001	format(1A1)
 1002   format(1X,4I2.2,1X,6(1X,F5.2,F6.3,F5.0))
C
        END

c
C##########################################################################

        SUBROUTINE TIMEPLT(NAME)

C       sub TIMEPLT is to plot WAS data for one year
c....   4 plots per page, 1 for each quarter  -
c....   each plot shows Hs, Tm, Dm -

c======================================================================

c	---  definition of variables  ---
c
c.... XT()  = normalized time coordinates -
c.... HS()  = significant wave height (meter) -
c.... TM()  = mean spectral period (second) -
c.... DM()  = mean flow direction (degrees - cwrgn) -

c.... NN()  = number of synoptic times in each quarter -
c.... N     = number of synoptic times in the actual quarter -
c.... DAY() = number of days in each month -
c
c====================================================================

        real XT(800)
        real hs(2929),tm(2929),dm(2929)
        real wh(800),wt(800),wdir(800)
        real XS(800), YS(800)
        real XV(2), YV(2)

        integer yy(2929), mm(2929), dd(2929), r(2929)
        integer nn(4)
        integer day(12)
        integer year

        character*40 labelx,labely, labela
        CHARACTER*5 NAME

c====================================================================

        COMMON /DATE/YY,MM,DD,R
        COMMON /WAVE/HS,TM,DM
        COMMON /NREC/NREC
        COMMON /OUT/WH,WT,WDIR

C................................................................

        data day/31,28,31,30,31,30,31,31,30,31,30,31/
        common /day/day

C====================================================================

C       1.0 NUMBER OF SYNOPTIC TIMES NN() IN EACH QUARTER IS EVALUATED
C           ----------------------------------------------------------

        if(yy(1).lt.50) year = 2000+yy(1)
        if(yy(1).ge.50) year = 1900+yy(1)
        if(mod(year,4).eq.0)day(2)=29
        nn(1)=(day(1)+day(2)+day(3))*8
        nn(2)=(day(4)+day(5)+day(6))*8
        nn(3)=(day(7)+day(8)+day(9))*8
        nn(4)=(day(10)+day(11)+day(12))*8

        NTOT = nn(1)+nn(2)+nn(3)+nn(4)


C       2.0 PREPARE LABELS FOR X AND Y AXIS
C           -------------------------------
        write(labela,'(''YEAR'',i5)') year
        labelx = '  '
        labely = '  QUARTER'


C       3.0 HORIZONTAL COORDINATES XT() ARE DEFINED
C           ---------------------------------------

C           having 8 data per day, 800 data correspond to 100 days -

        DO 301 i=1,800    

cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
ccc   if(i.gt.320 .and. i.lt.400)
ccc  .   write(57,'(4i5,f8.2)') yy(i), mm(i), dd(i), r(i),hs(i)
clllllllllllllllllllllllllllllllllllllllllllllllllllll

 301    XT(i)=i               

C       4.0 DEFINE MAX AND MIN
C           ------------------

        XMIN = 1.
        XMAX = 800.
        YMIN = 0.0
        YMAX = 5.0
        XUNIT = 80.
        YUNIT = 1.0

C       5.0 DEFINE SIZE AND POSITION OF GRAPHS
C           --------------------------------

        XP = 2.0
        YP = 1.0
        XL =25.0
        YL = 4.0


C     6.0 PLOT IS DONE HERE
C         -----------------

C     6.1.1 DEFINE SIZE OF PLOT
C           -------------------

      CALL PSETC ('PAGE_ID_LINE',                       'OFF' )
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',             'OFF' )
      CALL PSETC ('PAGE_ID_LINE_SYSTEM_PLOT',           'OFF' )
      CALL PSETC ('PAGE_ID_LINE_DATE_PLOT',             'OFF' )
      CALL PSETC ('PAGE_ID_LINE_ERRORS_PLOT',           'OFF' )
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',         'ON' )
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT',        'ADRIOPER' )
      CALL PSETR ('PAGE_ID_LINE_HEIGHT',                  0.2 )

      CALL PSETR ('SUPER_PAGE_X_LENGTH',                 29.7 )
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',                 21.0 )

      CALL PSETR ('PAGE_X_LENGTH',                       29.7 )
      CALL PSETR ('PAGE_Y_LENGTH',                       21.0 )
      CALL PSETC ('PAGE_FRAME',                         'OFF' )

      CALL PSETC ('SUBPAGE_FRAME',                      'OFF' )
      CALL PSETC ('SUBPAGE_MAP_PROJECTION',            'NONE' )

C     6.1.2 PLOT LOGO
C           ---------

         CALL PSETC ('TEXT_MODE',                'POSITIONAL' )
         CALL PSETC ('TEXT_BORDER',                      'ON' )
         CALL PSETC ('TEXT_BORDER_COLOUR',            'BLACK' )
         CALL PSETR ('TEXT_BOX_X_POSITION',              24.0 )
         CALL PSETR ('TEXT_BOX_Y_POSITION',              19.0 )
         CALL PSETR ('TEXT_BOX_X_LENGTH',                 3.0 )
         CALL PSETR ('TEXT_BOX_Y_LENGTH',                 1.0 )
         CALL PSETC ('TEXT_COLOUR',                   'BLACK' )
         CALL PSETI ('TEXT_LINE_COUNT',                     1 )
         CALL PSETC ('TEXT_LINE_1',                      NAME )
         CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',   0.6 )
         CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1',          1.0 )
         CALL PSETC ('TEXT_JUSTIFICATION',           'CENTRE' )
         CALL PSETC ('TEXT_QUALITY',                 'MEDIUM' )
         CALL PTEXT


C     6.1.3 PLOT YEAR
C           ---------

         CALL PSETC ('TEXT_BORDER',                      'ON' )
         CALL PSETC ('TEXT_BORDER_COLOUR',            'BLACK' )
         CALL PSETR ('TEXT_BOX_X_POSITION',              13.0 )
         CALL PSETR ('TEXT_BOX_Y_POSITION',              19.0 )
         CALL PSETR ('TEXT_BOX_X_LENGTH',                 4.0 )
         CALL PSETR ('TEXT_BOX_Y_LENGTH',                 1.0 )
         CALL PSETC ('TEXT_COLOUR',                   'BLACK' )
         CALL PSETI ('TEXT_LINE_COUNT',                     1 )
         CALL PSETC ('TEXT_LINE_1',                    LABELA )
         CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',   1.0 )
         CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1',          1.0 )
         CALL PSETC ('TEXT_JUSTIFICATION',           'CENTRE' )
         CALL PSETC ('TEXT_QUALITY',                 'MEDIUM' )
         CALL PTEXT

C     6.1.4 PREPARE LEGEND
C           --------------

      CALL PSETC ('LEGEND',                              'ON' )
      CALL PSETR ('LEGEND_TEXT_MAXIMUM_HEIGHT',           1.5 )
      CALL PSETC ('LEGEND_TEXT_COMPOSITION', 'USER_TEXT_ONLY' )
      CALL PSETC ('LEGEND_ENTRY_PLOT_DIRECTION', 'COLUMN' )
      CALL PSETC ('LEGEND_TEXT_COLOUR',               'BLACK' )
      CALL PSETC ('LEGEND_BORDER',                       'ON' )
      CALL PSETC ('LEGEND_BORDER_COLOUR',             'BLACK' )
      CALL PSETC ('LEGEND_BOX_MODE',             'POSITIONAL' )
      CALL PSETR ('LEGEND_BOX_X_POSITION',                2.0 )
      CALL PSETR ('LEGEND_BOX_Y_POSITION',                19. )
      CALL PSETR ('LEGEND_BOX_X_LENGTH',                  5.0 )
      CALL PSETR ('LEGEND_BOX_Y_LENGTH',                  1.5 )
      CALL PSETI ('LEGEND_COLUMN_COUNT',                  1   )
      CALL PSETR ('LEGEND_ENTRY_MAXIMUM_HEIGHT',          1.0 )

c
C     6.2 PLOT IS SPLITTED IN FOUR SEASONS
C         --------------------------------

        DO 6200 LL=1,4

C     6.2.1 UPDATE VERTICAL AXIS LABEL
C           --------------------------
        WRITE(LABELY(1:1),'(I1)') LL

C     6.2.2 DEFINE PAGE SIZE
C           ----------------

        YYP = YP + (4-LL)*4.5

         CALL PNEW  ('SUBPAGE')

         CALL PSETR ('SUBPAGE_X_POSITION', XP)
         CALL PSETR ('SUBPAGE_Y_POSITION', YYP)
         CALL PSETR ('SUBPAGE_X_LENGTH', XL)
         CALL PSETR ('SUBPAGE_Y_LENGTH', YL)
         CALL PSETC ('SUBPAGE_FRAME', 'ON')
         CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'NONE')

         CALL PSETC ('LEGEND', 'OFF')
         IF(LL.EQ.1) CALL PSETC ('LEGEND', 'ON')


C     6.2.3 DROW AXIS
C           ---------

        CALL AXIS(XMIN,XMAX,XUNIT, YMIN,YMAX,YUNIT, LABELX,LABELY)


C     6.2.5 PREPARE DATA TO PLOT
C           --------------------

C     N IS NUMBER OF SYNOPTIC TIMES IN THIS QUARTER -
      N=NN(LL)

C     ACTUAL SEQUENCE OF DATA FOR THIS PLOT  IS BUILT IS SUB QUARTER -
C     MISSING DATA ARE FILLED UP WITH -999. -
        CALL QUARTER(ll,n)

ccccccccccccccccccccccccccccccccccccccccccccccc
c     ----- PRINT DATA FOR CHECK -----
CC      print*,' TRIMESTRE --->',LL
CC      print*,' N=',N
CC      PRINT*,' ALTEZZA ALTEZZA ALTEZZA ALTEZZA ALTEZZA ALTEZZA'
CC      WRITE(6,6666)(WH(I),I=1,N)
CC      PRINT*,' PERIODO PERIODO PERIODO PERIODO PERIODO PERIODO'
CC      WRITE(6,6666)(WT(I),I=1,N)
CC 6666 FORMAT(1X,8F7.2)
ccccccccccccccccccccccccccccccccccccccccccccccc

C     6.2.6 DATA ARE NOW READY FOR PLOTTING
C           -------------------------------

C     6.2.6.1 PLOT WAVE HEIGHT
C             ----------------


        ITHICK=4
        ISYM=18
c        IF(LL.EQ.1)
         CALL PSETC ('LEGEND_USER_TEXT_1', 'WAVE HEIGHT (M)   .')
         CALL PSETC ('LEGEND_USER_TEXT_2', 'MEAN PERIOD (S)   /2')
         CALL PSETC ('LEGEND_USER_TEXT_3', 'MEAN FLOW DIR (D) 0-360')
        CALL PSERIE (XT, WH, N, N, 'SOLID', 'BLACK', ISYM , ITHICK)


C     6.2.6.2 PLOT MEAN PERIOD
C             ----------------

        ITHICK=2
        ISYM=4
c        IF(LL.EQ.1)
        CALL PSETC ('LEGEND_USER_TEXT_2', 'MEAN PERIOD (S)   /2')
        CALL PSERIE (XT, WT, N, N, 'DASH', 'RED', ISYM , ITHICK)


C     6.2.6.3 PLOT MEAN DIRECTION
C             -------------------

        ITHICK=6
        ISYM=3
C        IF(LL.EQ.1)
        CALL PSETC ('LEGEND_USER_TEXT_3', 'MEAN FLOW DIR (D) 0-360')
        CALL PSERIED (XT, WDIR, N, N, 'DOT','BLUE',ISYM ,ITHICK)

C            END OF LOOP OVER EACH QUARTER
C            -----------------------------

      CALL PSETC ('LEGEND', 'OFF')
 6200 CONTINUE

C
C*    7.0 END OF PHYSICAL PAGE.
C         ---------------------

      CALL PNEW ('PAGE')
C     CALL PNEW ('SUPER_PAGE')


        RETURN
c
        END
c
c#######################################################################
c
         subroutine QUARTER(ll,n)
c
c======================================================================
c
c.... sub QUARTER receives from MAIN the sequences y(),m(),d(),r(),
c....    hs(),tm(),dm() - see MAIN for meaning of variables -
c....    sequences provide data at synoptic times for a full year -
c.... sub QUARTER extracts the n data of the ll-th quarter placing them
c....    in vectors h(),t(),dir() -
c.... note: input sequences can be discontinuous - output sequences
c....    must be continuous - missing results are filled up with -999. -
c
c======================================================================
c
      integer y(2929), m(2929), d(2929), r(2929)
      real hs(2929),tm(2929),dm(2929)
      real h(800),t(800),dir(800)
      integer day(12)
C................................................................

        COMMON /DATE/Y,M,D,R
        COMMON /WAVE/HS,TM,DM
        COMMON /NREC/NREC
        COMMON /OUT/H,T,DIR

        common /day/day
c
c====================================================================
c.... tecnique followed in the subroutine -
c....    h(),t(),dir() are first filled up with -999. - then input 
c....    results are explored in sequence - one by one, if correct, they
c....    are placed at the right h(),t(),dir() positions - these are
c....    estimated by evaluating the sequential index in the actual
c....    quarter -
c
c.... filling with -999. -
      do 101 i=1,n
      h(i)=-999.
      t(i)=-999.
      dir(i)=-999.
 101  continue
c
c.... np is overall number of synoptic times in the quarters preceeding
c....    the ll-th one -
      np=0
      if(ll.eq.1)go to 10
c.... nm is number of months in the previous quarters -
      nm=(ll-1)*3
c.... nd is number of days in the previous quarters -
      nd=0
      do 102 k=1,nm
 102  nd=nd+day(k)
      np=nd*8
c
 10   continue
c
c.... exploration of the whole year -
      do 103 i=1,nrec
c
c.... ns is sequential synoptic time of the actual record in the year -
c....    evaluated in sub SYNOPT -
      call SYNOPT(y(i),m(i),d(i),r(i),ns)
c.... nq is sequential synoptic time in this quarter -
      nq=ns-np

c.... if(0.lt.nq.le.n) and original results are correct (positive)
c....    results are assigned to h(),t(),dir() -
c....    n is number of synoptic times in this quarter -
      if(nq.gt.0.and.nq.le.n)then
                   if(hs(i).ge.0.)h(nq)=hs(i)
                   if(tm(i).ge.0.)t(nq)=tm(i)/2.
                   if(dm(i).ge.0.)dir(nq)=dm(i)/72.
                   end if
c
 103  continue
c
      return
      end
c
c#######################################################################
c
      subroutine SYNOPT(y,m,d,r,ns)
c
c======================================================================
c
c.... sub SYNOPT receives year Y, month M, day D, UTC time R (0 to 21,
c....   3 by 3) - it evaluates the sequential index NS of the actual 
c....    synoptic time in the year -
c.... DAY() is number of days in each month -
c
c======================================================================
c
      integer y,m,d,r
      integer day(12)
c
      common /day/day
c
c=====================================================================
c
c.... nd is number of elapsed days previously to the actual one -
      nd=0
      if(m.gt.1)then
          do 101 i=1,(m-1)
 101      nd=nd+day(i)
          end if
      nd=nd+(d-1)
c
      ns=(nd*8)+r/3+1
c
      return
      end
c
C##########################################################################

      SUBROUTINE AXIS (XMIN, XMAX, XUNIT, YMIN, YMAX, YUNIT,
     1                 TEXTX, TEXTY)
      CHARACTER TEXTX*(*), TEXTY*(*)

C   --------------------------------------------------------------


C     ---   X - AXIS  ---

      CALL PSETC ('AXIS_GRID',                    'ON' )
      CALL PSETC ('AXIS_GRID_LINE_STYLE',      'SOLID' )
      CALL PSETC ('AXIS_GRID_COLOUR',          'BLACK' )
      CALL PSETI ('AXIS_LINE_THICKNESS',             2 )
      CALL PSETC ('AXIS_LINE',                   'OFF' )

      CALL PSETC ('AXIS_ORIENTATION',     'HORIZONTAL' )
      CALL PSETC ('AXIS_POSITION',            'BOTTOM' )
      CALL PSETR ('AXIS_MIN_VALUE',               XMIN )
      CALL PSETR ('AXIS_MAX_VALUE',               XMAX )

      CALL PSETC ('AXIS_TITLE',                  'OFF' )
      CALL PSETC ('AXIS_TITLE_TEXT',             TEXTX )
      CALL PSETR ('AXIS_TITLE_HEIGHT',            0.25 )
      CALL PSETC ('AXIS_TICK',                    'ON' )
      CALL PSETC ('AXIS_TICK_POSITIONING',   'REGULAR' )
      CALL PSETR ('AXIS_TICK_INTERVAL',          XUNIT )
      CALL PSETC ('AXIS_TICK_LABEL',             'OFF' )
      CALL PAXIS

C     ---   Y - AXIS  ---

      CALL PSETC ('AXIS_GRID',                   'OFF' )
      CALL PSETC ('AXIS_LINE',                   'OFF' )

      CALL PSETC ('AXIS_ORIENTATION',       'VERTICAL' )
      CALL PSETC ('AXIS_POSITION',              'LEFT' )
      CALL PSETR ('AXIS_MIN_VALUE',               YMIN )
      CALL PSETR ('AXIS_MAX_VALUE',               YMAX )

      CALL PSETC ('AXIS_TITLE',                   'ON' )
      CALL PSETC ('AXIS_TITLE_TEXT',             TEXTY )
      CALL PSETR ('AXIS_TITLE_HEIGHT',             0.3 )
      CALL PSETC ('AXIS_TICK',                    'ON' )
      CALL PSETC ('AXIS_TICK_POSITIONING',   'REGULAR' )
      CALL PSETR ('AXIS_TICK_INTERVAL',          YUNIT )
      CALL PSETC ('AXIS_TICK_LABEL',              'ON' )
      CALL PSETR ('AXIS_TICK_LABEL_HEIGHT',        0.3 )
      CALL PSETC ('AXIS_TICK_LABEL_QUALITY',    'HIGH' )
      CALL PAXIS

      RETURN
      END

C##########################################################################

      SUBROUTINE PSERIE (XVAL, YVAL, IDT, NDT, STYLE, COLOUR, ISYMB,
     *                   ITHICK)
      CHARACTER COLOUR*(*), STYLE*(*)
      DIMENSION XVAL(IDT), YVAL(IDT)

      CALL PSETC ('GRAPH_TYPE', 'CURVE')
      CALL PSETC ('GRAPH_LINE_COLOUR', COLOUR)
      CALL PSETC ('GRAPH_LINE_STYLE', STYLE)
      CALL PSETI ('GRAPH_LINE_THICKNESS', ITHICK)

      CALL PSETC ('GRAPH_SYMBOL', 'OFF')
      CALL PSETI ('GRAPH_SYMBOL_MARKER_INDEX', ISYMB)
      CALL PSETR ('GRAPH_SYMBOL_HEIGHT', 0.05*ITHICK)
      CALL PSETC ('GRAPH_SYMBOL_COLOUR', COLOUR)

      CALL PSET1R ('GRAPH_CURVE_X_VALUES', XVAL, NDT)
      CALL PSET1R ('GRAPH_CURVE_Y_VALUES', YVAL, NDT)
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
C
C     NOTE:  VALUES ARE DIVIDED BY 72. FOR PLOTTING PUROSES
C
c -----------------------------------------------------------------------

      CALL PSETC ('GRAPH_TYPE', 'CURVE')
      CALL PSETC ('GRAPH_LINE_COLOUR', COLOUR)
      CALL PSETC ('GRAPH_LINE_STYLE', STYLE)
      CALL PSETI ('GRAPH_LINE_THICKNESS', ITHICK)

ccc   CALL PSETC ('GRAPH_SYMBOL', 'ON')
      CALL PSETC ('GRAPH_SYMBOL', 'OFF')
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
      if(ya.ge.0. .and. yb.ge.0. .and. abs(ya-yb).gt.(180./72.)) then
            xa=xval(i-1)
            xb=xval(i)

c     case ya<yb
               if(ya.lt.180.) then
               c=ya+(360./72.)-yb
               ind=ind+1
               xab=xa+(ya/c)*(xb-xa)
               x1(ind)=xab
               y1(ind)=0.
               ind=ind+1
               x1(ind)=x1(ind-1)
               y1(ind)=360./72.
               end if

c     case ya>yb
               if(ya.gt.(180./72.)) then
               c=yb+(360./72.)-ya
               ind=ind+1
               xab=xa+(((360./72.)-ya)/c)*(xb-xa)
               x1(ind)=xab
               y1(ind)=360./72.
               ind=ind+1
               x1(ind)=x1(ind-1)
               y1(ind)=0.
               end if

            end if
         ind=ind+1
         x1(ind)=xval(i)
         y1(ind)=yval(i)
 100     continue

c     NP number of points in the extended series
      np=ind
c     plot is done in sections
      x2(1)=x1(1)
      y2(1)=y1(1)
      ind=1
         do 200 i=2,np
            if(abs(y1(i)-y1(i-1)).gt.(180./72.)) then
            call PSET1R('graph_curve_x_values',x2,ind)
            call PSET1R('graph_curve_y_values',y2,ind)
            call pgraph
            CALL PSETC ('LEGEND', 'OFF')
            ind=0
            end if
         ind=ind+1
         x2(ind)=x1(i)
         y2(ind)=y1(i)
 200     continue
      call PSET1R('graph_curve_x_values',x2,ind)
      call PSET1R('graph_curve_y_values',y2,ind)
      call pgraph
      CALL PSETC ('LEGEND', 'OFF')
      
      RETURN
      END


