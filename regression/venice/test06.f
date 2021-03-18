C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CUSTPLT

C          THIS PROGRAM TAKES THE MEDITERRANEAN WAM MODEL GRID AS
C          INPUT AND MAKES A PLOT OF A FIELD                        
C          USING MAGICS

C---------------------------------------------------------------------------

C     GRID DIMENSIONS ARE CHOSEN                        

      PARAMETER (ID1=85,ID2=33)

      DIMENSION WAVEHT(ID1,ID2),WAVEDIR(ID1,ID2)
      LOGICAL IEOF

      COMMON /DATEP/IY,IM,ID,IH
      COMMON /GRID/ DLON,DLAT, XLATS,XLATN,XLONW,XLONE

C----------------------------------------------------

      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(1)
      DATA         FORMATS /'PS'/
cc    DIMENSION    FORMATS(2)
cc    DATA         FORMATS /'PS', 'PNG'/
C---------------------------------------------------------------------------
C
      IEOF = .FALSE.
        open(unit=61, file='fort.61')

c     read heading of the file

         do 101 i=1,50
         read(61,*)
 101     continue

C       LOOP OVER  WIND AND/OR WAVE FIELDS

 10   continue

C          OPEN MAGICS

       CALL POPEN
       CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 1)
cc     CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 2)
       CALL PSETC  ('OUTPUT_NAME',         'test06')

c Enables the output to be split into different (single page) PostScript files
c Defines the PostScript scale between 0.1 and 1.0 - def.=1.0
c      CALL PSETR  ('OUTPUT_ps_scale' ,            0.5)

ccc   CALL PSETC('WORKSTATION_1','PS_COL')
C     CALL PSETC('WORKSTATION_1','PS')

c     read the field in sub FLDS

      CALL FLDS (IDATE,ID1,ID2,WAVEHT,WAVEDIR,NGX,NGY,IEOF)
      IF(IEOF) GO TO 1000

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=

C          PLOT  FIELD

      write(6,'(''sto disegnando:'',i12)') IDATE
c<><><><><><><><><><><><><><><><><><><><><><><><><><><><>

c     definition of the field -
c     if wave, itype=1 ,          if wind, itype=2

      itype=1
      CALL FLDPLT (WAVEHT ,WAVEDIR,NGX,NGY,ITYPE,IDATE)

c     GO TO READ THE NEXT FIELD
ccccccccccccccccc      GO TO 10

 1000 CONTINUE

C          CLOSE MAGICS

      CALL PCLOSE

      STOP 1111
      END

C()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

      subroutine FILLAND(x,nlon,nlat)

c sub FILLAND fills land point with values derived from sea points -
c   for each land point the weighted average (with distance) is taken
c   from all the surrounding 8 points, if sea points - the procedure
c   is repeated twice -

c========================================================================

c X(,)       matrix to be changed
c Y(,)       matrix for operation
c NLON,NLAT  grid dimensions
c DLAT       half a degree step in latitude, in km
c DLON       half a degree step in longitude, in km
c DIAG       half a degree diagonal step, in km
c LAT        latitude, in degrees

c DIST       sum of distance of bordering points from one grid point
c SUM        weighted sum (with distance) of their values, if not "land"
c A          dummy variable

c KOUNT      counter of the correction loop (to be done twice - if more
c              act on the if at the end of the sub)

c=======================================================================

      real x(nlon,nlat)
      real y(nlon,nlat)
      real lat,dlat,dlon,diag
      integer nlon,nlat
      real sum,dist
      real a
      integer kount

c=======================================================================

      dlat=55.6
      conv=asin(1.)/90.
      kount=0

 10   continue

c information of X(,) is transfered to Y(,) -
      do 100 i=1,nlon
      do 110 j=1,nlat
         y(i,j)=x(i,j)
 110  continue
 100  continue

c we act first on the inner points of the grid - bord is dealt with later

      do 200 j=2,nlat-1
         lat=(30.+(j-1)*0.5)*conv
         dlon=dlat*cos(lat)
       do 210 i=2,nlon-1
         if(y(i,j).gt.0.) go to 210
c land point - search for eventual close by sea points
         sum=0.
         dist=0.
c scan the bordering points horizontally, from the lowest row
         a=y(i-1,j-1)        ! S-W
         if(a.gt.0.) then
            sum=sum+a*diag
            dist=dist+diag
         end if
         a=y(i,j-1)          ! S
         if(a.gt.0.) then
            sum=sum+a*dlat
            dist=dist+dlat
         end if
         a=y(i+1,j-1)        ! S-E
         if(a.gt.0.) then
            sum=sum+a*diag
            dist=dist+diag
         end if
         a=y(i-1,j)          ! W
         if(a.gt.0.) then
            sum=sum+a*dlon
            dist=dist+dlon
         end if
         a=y(i+1,j)          ! E
         if(a.gt.0.) then
            sum=sum+a*dlon
            dist=dist+dlon
         end if
         a=y(i-1,j+1)        ! N-W
         if(a.gt.0.) then
            sum=sum+a*diag
            dist=dist+diag
         end if
         a=y(i,j+1)          ! N
         if(a.gt.0.) then
            sum=sum+a*dlat
            dist=dist+dlat
         end if
         a=y(i+1,j+1)        ! N-E
         if(a.gt.0.) then
            sum=sum+a*diag
            dist=dist+diag
         end if
         if(dist.gt.0.)x(i,j)=sum/dist
 210  continue
 200  continue

      kount=kount+1
      if(kount.lt.2) go to 10

c end of looping
c border values are set = to the closest non-border point -
c horizontal borders
      do 300 i=2,nlon-1   
         x(i,1)=x(i,2)
         x(i,nlat)=x(i,nlat-1)
 300  continue
c vertical borders
      do 400 j=2,nlat-1   
         x(1,j)=x(2,j)
         x(nlon,j)=x(nlon-1,j)
 400  continue
c corner points
      x(1,1)=x(2,2)
      x(nlon,1)=x(nlon-1,2)
      x(nlon,nlat)=x(nlon-1,nlat-1)
      x(1,nlat)=x(2,nlat-1)
         
      return
      end

C()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

      SUBROUTINE FLDPLT(XMAG,XDIR,ID1,ID2,ITYPE,JDATE)

C          THIS ROUTINE PLOTS GRIDDED WIND AND WAVE HEIGHT
C          FIELDS USING MAGICS.  MAGNITUDES ARE PLOTTED AS
C          ISOLINES AND DIRECTIONS AS ARROWS.

C          LIANA ZAMBRESKY
C          GKSS/ECMWF
C          FEBRUARY 1989 -
C          MODIFIED FOR THE MEDITERRANEAN SEA
C          LUIGI CAVALERI
C          APRIL 1989 -
C          NOVEMBER 1989 -
c          DECEMBER 1994

C
C          - USE OF NEW-VERSION OF MAGICS++
C          - MODIFIED FOR NEW COLOUR SCALE AND OUT-LOOK
C          - NEW COAST-LINE ????????????
C          LUCIANA BERTOTTI - JULY 2011

C----------------------------------------------------------------------------

C          VARIABLES DEFINITION

C          XMAG(,)    = 2-DIM VECTOR MODULI
C          XDIR(,)    = 2-DIM VECTOR DIRECTIONS
C          CONTI()    = CONTOUR INTERVAL FOR WAVES AND WIND RESPLY
C          CONTHF()   = HEAVY CONTOUR FREQUENCY FOR WAVES AND WIND RESPLY

C----------------------------------------------------------------------------

      DIMENSION CONLIS (21)
      CHARACTER*20 CLIST(21)
      CHARACTER*20 labels(2)

      DATA labels /'0-2', '>30'/

C     THIS LIST FOR WAVE
      DATA CONLIS /0.0,  0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0,
     x             2.5,  3.0,  3.5, 4.0,  4.5, 5.0,  6.0, 7.0,  8.0,
     x             9.0, 10.0, 11.0/
      DATA CLIST /'RGB(0,0.3,1)', 'RGB(0,0.44,1)', 'RGB(0,0.55,1)',
     x            'RGB(0,0.66,1)','RGB(0,0.78,1)', 'RGB(0,0.89,1)',
     x            'RGB(0,1,1)',   'RGB(0.1,1,0.6)','RGB(0.4,1,0.1)',
     x            'RGB(0.6,1,0)', 'RGB(0.8,1,0)',  'RGB(1,1,0)',
     x            'RGB(1,0.86,0)','RGB(1,0.7,0)',  'RGB(1,0.53,0)',
     x            'RGB(1,0.38,0)','RGB(1,0.22,0)', 'RGB(1,0,0)',
     x            'RGB(1,0,0.55)','RGB(1,0,0.75)', 'RGB(1,0,0.75)'/

C     THIS LIST FOR WIND
c     DATA CONLIS /0.0,  2.0, 4.0, 6.0, 8.0,10.0,12.0,14.0,16.0,18.0,
c    x             20.0,22.0,24.0,26.0,28.0,30.0/
c     DATA CLIST /'RGB(0,0.4,1)', 'RGB(0,0.6,1)',  'RGB(0,0.78,1)',
c    x            'RGB(0,0.89,1)','RGB(0.1,1,0.6)','RGB(0.4,1,0.1)',
c    x            'RGB(0.6,1,0)', 'RGB(0.8,1,0)',  'RGB(1,1,0)',
c    x            'RGB(1,0.86,0)','RGB(1,0.7,0)',  'RGB(1,0.53,0)',
c    x            'RGB(1,0.38,0)',
c    x            'RGB(1,0,0)',   'RGB(1,0,0.55)', 'RGB(1,0,0.75)'/

C----------------------------------------------------------------------------

      PARAMETER(IP1=85, IP2=33)

      CHARACTER TITLE1*58,TITLE2*50
      CHARACTER RUN*53

      INTEGER IY,IM,ID,IH

C     XMAG(,) , XDIR(,) : ORIGINAL MATRICES FROM INPUT
      DIMENSION XMAG(ID1,ID2),XDIR(ID1,ID2)
C     XMAG2(,)          : INTERPOLTED VALUES OVER LAND 
      DIMENSION XMAG2(IP1,IP2)
C     SCALE WAVE MATRIX TO PLOT 'BIGGER' ARROWS
      DIMENSION XMAG3(IP1,IP2)

      DIMENSION CONTI(2)
      INTEGER CONTHF(2)

C          CONTOUR INTERVAL FOR WAVES AND WINDS

      DATA CONTI/1.0,4.0/
      DATA CONTHF/5,5/

      COMMON /DATEP/IY,IM,ID,IH
      COMMON /GRID/ DLON,DLAT, XLATS,XLATN,XLONW,XLONE

C----------------------------------------------------------------------------

C     PREPARE MATRIX TO EXTEND WAVE VALUES OVER LAND
      DO I= 1,ID1
         DO J= 1,ID2
            XMAG2(I,J) = XMAG(I,J)
            XMAG3(I,J) = SQRT(XMAG(I,J)/7.0)*7.0
            IF(XMAG(I,J) < 0.0) XMAG3(I,J) = -0.1
         END DO
      END DO

C          IDENTIFICATION TEXT OF ACTUAL RUN
      RUN = 'WAVE ANALYSIS  ECMWF'
c            -----------------------------------------------------

cc    CALL PSETC ('PAGE_ID_LINE',                        'ON')
      CALL PSETC ('PAGE_ID_LINE',                       'OFF')
cc    CALL PSETC ('PAGE_ID_LINE_COLOUR',              'WHITE')
      CALL PSETC ('PAGE_ID_LINE_COLOUR',              'BLACK')
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',             'OFF')
      CALL PSETC ('PAGE_ID_LINE_SYSTEM_PLOT',           'OFF')
cc    CALL PSETC ('PAGE_ID_LINE_DATE_PLOT',             'OFF')
      CALL PSETC ('PAGE_ID_LINE_ERRORS_PLOT',           'OFF')
cc    CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',        'OFF')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',         'ON')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT',               RUN)
      CALL PSETR ('PAGE_ID_LINE_HEIGHT',                 0.20)

C          DEFINE SUPERPAGE AND PAGE

      CALL PSETR ('SUPER_PAGE_X_LENGTH',                 29.7)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',                 21.0)
      CALL PSETR ('PAGE_X_LENGTH',                       29.7)
      CALL PSETR ('PAGE_Y_LENGTH',                       21.0)
      CALL PSETC ('PAGE_FRAME',                         'OFF')
 
C           DEFINE SUBPAGE
      CALL PSETR ('SUBPAGE_X_LENGTH',                    25.0)       ! MED SEA
      CALL PSETR ('SUBPAGE_Y_LENGTH',                    12.5)       ! MED SEA
      CALL PSETC ('SUBPAGE_FRAME_COLOUR',             'BLACK')


C...........................................................................
C   4.0   CHOOSE THE PROJECTION
C         ---------------------
 4000  CONTINUE

C     CHOOSE THE PROJECTION
C     111  =  POLAR STEREOGRAPHIC
C     222  =  MERCATOR
C     333  =  POLAR STEREOGRAPHIC - NEW DEFINITION

      GO TO 333

c====++++====++++====++++====++++====++++====++++====++++====++++====
 111  CONTINUE
C     STEREOGRAPHIC PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION',        'CENTRE')
      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',           15.0)
      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',            38.0)
      CALL PSETR('SUBPAGE_MAP_VERTICAL_LONGITUDE',         15.0)
      CALL PSETR('SUBPAGE_MAP_SCALE',                    18.0E6)
      GO TO 4100
c====++++====++++====++++====++++====++++====++++====++++====++++====
 222  CONTINUE
C     MERCATOR    PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION',         'MERCATOR')
      CALL PSETR('SUBPAGE_LOWER_LEFT_LONGITUDE',        XLONW) 
      CALL PSETR('SUBPAGE_LOWER_LEFT_LATITUDE',         XLATS) 
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LONGITUDE',       XLONE) 
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LATITUDE',        XLATN) 
      GO TO 4100

c====++++====++++====++++====++++====++++====++++====++++====++++====
 333  CONTINUE
C     STEREOGRAPHIC PROJECTION  -  NEW DEFINITION

      CALL PSETR ('SUBPAGE_Y_POSITION',                       1.0)
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION',        'CORNERS')
      CALL PSETC ('SUBPAGE_MAP_HEMISPHERE',               'NORTH')
      CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',          16.0)    ! MED SEA
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',             27.5)    ! MED SEA
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',            -2.0)    ! MED SEA
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',            42.5)    ! MED SEA
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',           40.0)    ! MED SEA

c     CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',          13.0)
c     CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',             44.7)     ! VENICE
c     CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',            12.0)     ! VENICE
c     CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',            45.8)     ! VENICE
c     CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',           14.0)     ! VENICE
ccc   CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',             39.5)      ! ADRIA
ccc   CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',            12.0)      ! ADRIA
ccc   CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',            46.0)      ! ADRIA
ccc   CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',           20.5)      ! ADRIA
      GO TO 4100

c====++++====++++====++++====++++====++++====++++====++++====++++====

 4100  CONTINUE

C                  MAP_GRID  NEW DEFINITION
c     CALL PSETC ('MAP_GRID',                                'ON')
cc    CALL PSETC ('MAP_BOUNDARIES',                         'OFF')
c     CALL PSETC ('MAP_BOUNDARIES',                          'ON')
c     CALL PSETI ('MAP_BOUNDARIES_THICKNESS',                   3)
c     CALL PSETC ('MAP_BOUNDARIES_PATH',  'med_boundaries.mapgen')
c     CALL PSETC ('map_boundaries_colour',                 'GOLD')
cc    CALL PSETC ('map_boundaries_colour',                'white')
c     CALL PSETC ('MAP_GRID_COLOUR',                      'KHAKI')
c     CALL PSETC ('MAP_GRID_LINE_STYLE',                   'DASH')
c     CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',              4.0)
c     CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',             4.0)
c     CALL PSETC ('MAP_LABEL',                               'ON')
c     CALL PSETC ('MAP_LABEL_COLOUR',                     'BLACK')
c     CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY',              1)
c     CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',               1)
c     CALL PSETC ('MAP_COASTLINE_LAND_SHADE',                'ON')
c     CALL PSETC ('MAP_COASTLINE_COLOUR',                  'GREY')
c     CALL PSETI ('MAP_COASTLINE_THICKNESS',                    3)
c     CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR',
c    x                                      'RGB(0.80,0.80,0.80)')


C                  MAP_GRID  OLD DEFINITION
      CALL PSETR ('MAP_GRID_LONGITUDE_REFERENCE',             0.0)
      CALL PSETR ('MAP_GRID_LATITUDE_REFERENCE',             30.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',             1.0)
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',              1.0)
      CALL PSETC ('MAP_GRID_COLOUR',                      'BLACK')
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY',              4)
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',               4)
      CALL PSETC ('MAP_GRID_LINE_STYLE',                    'DOT')
      CALL PSETC ('MAP_COASTLINE_COLOUR',                 'BLACK')
      CALL PSETC ('MAP_COASTLINE_RESOLUTION',              'HIGH')
      CALL PSETI ('MAP_COASTLINE_THICKNESS',                    3)

C  ...............................................................

C          PLOT TITLE


      IF(ITYPE.EQ.1) THEN
         WRITE(TITLE1,100) IY,IM,ID,IH
 100     FORMAT('MEDITERRANEAN SEA - WAMS WAVE HEIGHT AT   ',
     *           I4,'.',I2.2,'.',I2.2,' ',I2.2,' UT')
         CALL PSETC ('TEXT_LINE_1',TITLE1)
      ENDIF
      IF(ITYPE.EQ.2) THEN
         WRITE(TITLE2,101) IY,IM,ID,IH
 101     FORMAT('MEDITERRANEAN SEA - 10M WIND  AT  ',
     *           I4,'.',I2.2,'.',I2.2,' ',I2.2,' UT')
         CALL PSETC ('TEXT_LINE_1',TITLE2)
      ENDIF

c     CALL PSETC ('TEXT_QUALITY',                          'HIGH')
      CALL PSETI ('TEXT_LINE_COUNT',                            1)
ccc   CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',          0.3)
      CALL PSETC ('TEXT_JUSTIFICATION',                  'CENTRE')
      CALL PSETC ('TEXT_COLOUR',                          'BLACK')
c     CALL PSETR ('TEXT_FONT_SIZE',                          1.25)
      CALL PSETR ('TEXT_FONT_SIZE',                           0.5)

      CALL PTEXT

C  ...............................................................

C          DEFINE WAVE OR WIND FIELDS

cc    CALL PSET2R ('INPUT_WIND_SPEED',              XMAG,ID1,ID2)
cc    CALL PSET2R ('INPUT_WIND_DIRECTION',          XDIR,ID1,ID2)

      CALL PSET2R ('INPUT_WIND_SPEED',             XMAG3,ID1,ID2)
      CALL PSET2R ('INPUT_WIND_DIRECTION',          XDIR,ID1,ID2)
      CALL PSET2R ('INPUT_FIELD',                  XMAG2,ID1,ID2)


      CALL PSETR  ('INPUT_FIELD_INITIAL_LONGITUDE',        XLONW)
      CALL PSETR  ('INPUT_FIELD_INITIAL_LATITUDE',         XLATS)
      CALL PSETR  ('INPUT_FIELD_LONGITUDE_STEP',            DLON)
      CALL PSETR  ('INPUT_FIELD_LATITUDE_STEP',             DLAT)
C  ...............................................................

c  fill the field also on land for plotting contours

ccccccccccccc      call FILLAND (XMAG,id1,id2)
ccccccccccccc      CALL PSET2R ('INPUT_FIELD',XMAG,ID1,ID2)

C          PLOT CONTOURS

C          OLD CONTOURING    <****************
      CALL PSETC ('CONTOUR_SHADE',                           'ON')      
      CALL PSETC ('CONTOUR_HILO',                           'OFF')
      CALL PSETC ('CONTOUR_LABEL',                           'ON')
      CALL PSETC ('CONTOUR_LABEL_QUALITY',                 'HIGH')
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',                      .3)
      CALL PSETC ('CONTOUR_LABEL_COLOUR',                 'BLACK')
      CALL PSETC ('CONTOUR_LINE_COLOUR',                  'BLACK')
      CALL PSETR ('CONTOUR_MIN_LEVEL',                        1.0)
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',      'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',                CONTI(ITYPE))
      CALL PSETI ('CONTOUR_HIGHLIGHT_FREQUENCY',    CONTHF(ITYPE))
      CALL PSETI ('CONTOUR_HIGHLIGHT_THICKNESS',                2)

C          NEW CONTOURING    <****************
c     CALL PSETC ('CONTOUR_HIGHLIGHT',                      'OFF')
c     CALL PSETC ('CONTOUR_LABEL',                          'OFF')
c     CALL PSETC ('CONTOUR_LINE_COLOUR',                   'GREY')
c     CALL PSETC ('CONTOUR_LINE_STYLE',                     'DOT')
c     CALL PSETI ('CONTOUR_LINE_THICKNESS',                     1)
c     CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',    'LEVEL_LIST')
c     CALL PSET1R ('CONTOUR_LEVEL_LIST',               CONLIS, 21)!<+++++++++

c     CALL PSETC ('CONTOUR_SHADE_COLOUR_METHOD',           'LIST')
c     CALL PSET1C('CONTOUR_SHADE_COLOUR_LIST',           CLIST,21)!<=========
c     CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',    'POLYGON_SHADING')  
c     CALL PSETC ('CONTOUR_SHADE_METHOD',             'AREA_FILL')  

      CALL PCONT

C          PLOT ARROWS

ccc   CALL PSETR ('WIND_THINNING_FACTOR',                     0.0)
      IF(ITYPE.EQ.1) CALL PSETR ('WIND_ARROW_UNIT_VELOCITY',  15.)     ! WAVE
      IF(ITYPE.EQ.2) CALL PSETR ('WIND_ARROW_UNIT_VELOCITY',  40.)     ! WIND
      CALL PSETC ('WIND_ARROW_COLOUR',                    'BLACK')
      CALL PSETI ('WIND_ARROW_HEAD_SHAPE',                      1)
      CALL PSETR ('WIND_ARROW_HEAD_RATIO',                    0.3)
      CALL PSETI ('WIND_ARROW_THICKNESS',                       1)
ccc   CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY',                'ON')
      CALL PWIND

      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',                'ON')     
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR',      'WHITE')     
      CALL PCOAST
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',               'OFF')     
      CALL PCOAST

      CALL PNEW ('PAGE')

      RETURN
      END

C==============================================================================

      SUBROUTINE FLDS (IDATE,ID1,ID2,WAVEHT,WAVEDIR,NGX,NGY,IEOF)

C------------------------------------------------------------------
C
C     *FLDS* - READ GRIDDED FIELDS FROM MAP FILES
C
C     LIANA ZAMBRESKY
C     GKSS/ECMWF
C     AUGUST 1988
C     MODIFIED  LUIGI CAVALERI
C     APRIL 1989
C     SEPTEMBER 1990
c     DECEMBER 1994
C
C----------------------------------------------------------------------
C
C     PURPOSE.
C     --------
C
C                 POST PROCESSING ROUTINE FOR WAVE MODEL
C
C
C*    INTERFACE.
C     ----------
C
C                               WAVE HEIGHT
C                               MEAN WAVE DIRECTION
C                               MEAN FREQUENCY
C
C-----------------------------------------------------------------------
 
      DIMENSION WAVEHT(ID1,ID2), WAVEDIR(ID1,ID2)
      real a(85,33), b(85,33)
      LOGICAL IEOF

      COMMON /DATEP/IY,IM,ID,IH
      COMMON /GRID/ DLON,DLAT, XLATS,XLATN,XLONW,XLONE


C-----------------------------------------------------------------------
C
C*     VARIABLE     TYPE     PURPOSE.
C      --------     ----     --------
C
C      *XLATS*      REAL     LOWEST LATITUDE OF PLOT.
C      *XLATN*      REAL     HIGHEST LATITUDE OF PLOT.
C      *XLONW*      REAL     MOST WESTERN LONGITUDE OF PLOT.
C      *XLONE*      REAL     MOST EASTERN LONGITUDE OF PLOT.
C
C----------------------------------------------------------------------
C
c      grid characteristics are assigned - 

       XLATS   =  30.
       XLATN   =  46.
       XLONW   =  -6.
       XLONE   =  36.

       DLON = 0.5
       DLAT = 0.5

C*     1.     - READ WAVE AND WIND FIELDS -
C             -----------------------------
C
      DO 1100 I=1,ID1
      DO 1100 J=1,ID2
         WAVEHT(I,J)  = 0.
         WAVEDIR(I,J) = 0.
 1100 CONTINUE
C
c     characteristics of the field and date are read from file -
c     note - NP, KPAR, IY, IM, ID, IH are given for each parameter;
c            besides, there are two dummy readings after the first two
c            fields (peak period and mean period) -

      read(61,1101,end=10) np,kpar
      read(61,1102,end=10) iy,im,id,ih
 1101 format(25x,i6,10x,i5)
 1102 format(45x,4(1x,i2))
         IDATE=iy*10**6+im*10**4+id*100+ih
         NGX=ID1           
         NGY=ID2          
         do 241 j=1,ngy          
         READ(61,1103,end=10) (WAVEHT (I,J),I=1,NGX)
 1103 format(12f6.2)
 241     continue
         read(61,1101,end=10) np,kpar
         read(61,1102,end=10) iy,im,id,ih
         do 242 j=1,ngy    
         READ(61,1103,end=10) (WAVEDIR(I,J),I=1,NGX)
 242     continue
         read(61,1101,end=10) np, kpar
         read(61,1102,end=10) iy,im,id,ih
         do 243 j=1,ngy          
         read(61,1103,end=10) (a(i,j),i=1,ngx)
 243     continue
         read(61,1101,end=10) np,kpar
         read(61,1102,end=10) iy,im,id,ih
         do 244 j=1,ngy         
         read(61,1103,end=10) (b(i,j),i=1,ngx)
 244     continue

c        negative (land) points are set = 0. 

            do 202 i=1,NGX
            do 203 j=1,NGY
            if(waveht(i,j).lt.0.) then
               waveht(i,j)=0.
               wavedir(i,j)=0.
               a(i,j)=0.
               b(i,j)=0.
            end if
 203        continue
 202        continue

C           WAVE DIRECTION IS INVERTED FROM FLOW TO INCOMING DIRECTION 
C           FOR PLOTTING WITH MAGICS CONVENTION
            DO 1200 I=1,NGX
            DO 1300 J=1,NGY
            WAVEDIR(I,J) = 180.+ WAVEDIR(I,J)
            WAVEDIR(I,J)=MOD(WAVEDIR(I,J),360.)
 1300       CONTINUE
 1200       CONTINUE

      RETURN

c     EOF encountered - 

 10   continue
      IEOF = .true.
      return

      END
