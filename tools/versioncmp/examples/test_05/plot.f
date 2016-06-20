C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM SEAPLT

C          THIS PROGRAM PLOTS THE MEDITERRANEAN SEA POINTS
C                                      L.BERTOTTI   JAN. 1998

C---------------------------------------------------------------------------

C     GRID DIMENIONS

C     PARAMETER (ID1= 4,ID2= 3)                      !  ORKNEY 1/10
      PARAMETER (ID1=16,ID2=11)                      !  ORKNEY 1/50
C     PARAMETER (ID1=31,ID2=21)                      !  ORKNEY 1/100

      LOGICAL IEOF
      CHARACTER AX(ID1)*1
      CHARACTER RUN*17
      CHARACTER LINE*72

      DIMENSION TOPO(ID1,ID2)
      INTEGER   IBAT(ID1,ID2)

C-----------------------------------------------------------------------
C
C*    *COMMON* *BOUNDS* SCALING OF PLOTS.
C
      COMMON/BOUNDS/XLATS,XLATN,XLONW,XLONE

C*    *GRID*  GRID STEPS
      COMMON/GRID/ XDELO,XDELA
C
C_________________________________________________________________________

      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(1)
      DATA         FORMATS /'PS'/
cc    DIMENSION    FORMATS(2)
cc    DATA         FORMATS /'PS', 'PNG'/

C_________________________________________________________________________
C

C 1.0      INFORMATION ON ACTUAL RUN IS READ
C          ---------------------------------

      RUN = 'ORKNEY ISLAND WAM'

C 2.0      OPEN MAGICS
C          -----------

      CALL POPEN

      CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 1)
      CALL PSETC  ('OUTPUT_NAME',            'ORKNEY')
ccc   CALL PSETC('WORKSTATION_1','PS_COL')
c     CALL PSETC('WORKSTATION_1','PS')

C_____________________________________________________

C  3.0  READ FILE OF BATHIMETRY TO BE PLOTTED
C       -------------------------------------

      READ(11,'(6F10.5)') XDELA, XDELO, XLATS, XLATN, XLONW, XLONE
      NGX = ID1
      NGY = ID2
      DO 10 J=1,NGY
      READ(11,'(12(I5,A1))') (IBAT(I,J),AX(I),I=1,NGX)
      DO 20 I=1,NGX
      TOPO(I,J) = IBAT(I,J)
 20   CONTINUE
 10   CONTINUE



C          PLOT SEA POINTS

      CALL BATPLT (TOPO,NGX,NGY,RUN)


C          CLOSE MAGICS

      CALL PCLOSE

      STOP
      END

C========================================================================

      SUBROUTINE BATPLT(XMAG,ID1,ID2,RUN)

C          THIS ROUTINE PLOTS SYMBOLS AT SEA POINT LOCATION
C          OF THE MEDITERRANEAN SEA.

C----------------------------------------------------------------------------

C          VARIABLES DEFINITION

C          XMAG(,)    = MATRIX OF BATHIMETRY

C          RLON()     = ARRAY OF LONGITUDES
C          RLAT()     = ARRAY OF LATITUDES
C          DUMM()     = DUMMY ARRAY
C          ISYM()     = SYMBOL TO BE DRAWN

C----------------------------------------------------------------------------

      CHARACTER TITLE1*58,TITLE2*50
      CHARACTER RUN*17

      REAL XMAG(ID1,ID2)
      REAL RLON(5000), RLAT(5000), DUMM(5000)
      INTEGER ISYM(5000)

C-----------------------------------------------------------------------
C
C*    *COMMON* *BOUNDS* SCALING OF PLOTS.
C
      COMMON/BOUNDS/XLATS,XLATN,XLONW,XLONE
C
C*    *GRID*  GRID STEPS
      COMMON/GRID/ XDELO,XDELA
C
C-----------------------------------------------------------------------
C     buoys location

      real blat(2), blon(2)
      real bdum(2)
c     integer bsym(2)

      data blat /58.97, 58.98/
      data blon /-3.39, -3.40/
      data bdum / 3.,    3./
C                BOA-E, BOA-F
C
C-----------------------------------------------------------------------


C          GRID STEP
      DLON = XDELO
      DLAT = XDELA

C          IDENTIFICATION TEXT OF PLOT

cc    CALL PSETC ('PAGE_ID_LINE',                        'ON')
      CALL PSETC ('PAGE_ID_LINE',                       'OFF')
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',             'OFF')
      CALL PSETC ('PAGE_ID_LINE_SYSTEM_PLOT',           'OFF')
      CALL PSETC ('PAGE_ID_LINE_DATE_PLOT',             'OFF')
      CALL PSETC ('PAGE_ID_LINE_ERRORS_PLOT',           'OFF')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',         'ON')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT',               RUN)
      CALL PSETR ('PAGE_ID_LINE_HEIGHT',                  0.2)

C
C          THESE ARE THE PAGE DEFINITIONS FOR SIZE A4

ccc   CALL PSETR('SUPER_PAGE_X_LENGTH',                  29.7)
ccc   CALL PSETR('SUPER_PAGE_Y_LENGTH',                  21.0)
      CALL PSETR('SUPER_PAGE_X_LENGTH',                  17.0)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',                  21.0)
      CALL PSETR('PAGE_X_LENGTH',                        15.7)
      CALL PSETR('PAGE_Y_LENGTH',                        18.0)
      CALL PSETC('PAGE_FRAME',                          'OFF')
C

C          DEFINE SUBPAGE

      CALL PSETR('SUBPAGE_X_LENGTH',                     15.0)
      CALL PSETR('SUBPAGE_Y_LENGTH',                     18.0)

C...........................................................................
C          CHOOSE THE PROJECTION

C     CHOOSE THE PROJECTION
C     111  =  POLAR STEREOGRAPHIC
C     222  =  MERCATOR
C     333  =  POLAR STEREOGRAPHIC - NEW DEFINITION

      GO TO 333

c====++++====++++====++++====++++====++++====++++====++++====++++====
 111  CONTINUE
C     STEREOGRAPHIC PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION',         'CENTRE')
      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',            15.0)
      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',             38.0)
      CALL PSETR('SUBPAGE_MAP_VERTICAL_LONGITUDE',          15.0)
      CALL PSETR('SUBPAGE_MAP_SCALE',                     18.0E6) 
      GO TO 1100

c====++++====++++====++++====++++====++++====++++====++++====++++====
 222  CONTINUE
C     MERCATOR    PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION',            'MERCATOR')
      CALL PSETR('SUBPAGE_LOWER_LEFT_LONGITUDE',            -6.0) 
      CALL PSETR('SUBPAGE_LOWER_LEFT_LATITUDE',             30.0) 
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LONGITUDE',           36.0) 
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LATITUDE',            46.0) 
      GO TO 1100

c====++++====++++====++++====++++====++++====++++====++++====++++====
 333  CONTINUE
C     STEREOGRAPHIC PROJECTION  -  NEW DEFINITION

      CALL PSETR ('SUBPAGE_Y_POSITION',                       1.0)
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION',        'CORNERS')
      CALL PSETC ('SUBPAGE_MAP_HEMISPHERE',               'NORTH')
      CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',         356.5)     ! ORKNEY
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',             58.8)     ! ORKNEY
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',           356.3)     ! ORKNEY
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',            59.2)     ! ORKNEY
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',          356.9)     ! ORKNEY
      GO TO 1100

c ===++++====++++====++++====++++====++++====++++====++++====++++====

 1100  CONTINUE

      CALL PSETR('MAP_GRID_LONGITUDE_REFERENCE',           180.0) 
      CALL PSETR('MAP_GRID_LATITUDE_REFERENCE',              0.0) 
      CALL PSETR('MAP_GRID_LONGITUDE_INCREMENT',             0.1)
      CALL PSETR('MAP_GRID_LATITUDE_INCREMENT',              0.1)
      CALL PSETC('MAP_GRID_COLOUR',                      'BLACK')
      CALL PSETC('MAP_GRID_LINE_STYLE',                    'DOT')
      CALL PSETI('MAP_LABEL_LONGITUDE_FREQUENCY',              1)
      CALL PSETI('MAP_LABEL_LATITUDE_FREQUENCY',               1)

      CALL PSETC('MAP_COASTLINE_COLOUR',                 'BLACK')
      CALL PSETC('MAP_COASTLINE_RESOLUTION',              'HIGH')
      CALL PSETI('MAP_COASTLINE_THICKNESS',                    6)
      CALL PSETC('MAP_COASTLINE_LAND_SHADE',                'ON')
ccc   CALL PSETC('MAP_COASTLINE_LAND_SHADE_COLOUR',       'GOLD')
      CALL PSETC('MAP_COASTLINE_LAND_SHADE_COLOUR',       'GREY')
      CALL PCOAST

C  ...............................................................


C          PLOT TITLE

         WRITE(TITLE1,100)
 100     FORMAT('ORKNEY ISLANDS 1/50  - SEA POINTS')

         CALL PSETC ('TEXT_QUALITY',                'HIGH')
         CALL PSETC ('TEXT_COLOUR',                'BLACK')
         CALL PSETI ('TEXT_LINE_COUNT',                  1)
         CALL PSETR ('TEXT_FONT_SIZE',                 0.5)
         CALL PSETC ('TEXT_LINE_1',                 TITLE1)
         CALL PTEXT

C  ...............................................................

C          DEFINE THE SEA POINTS LONGITUDE , LATITUDE AND SYMBOL

      NPT = 0
      DO 1300 I=1,ID1
      DO 1300 J=1,ID2
      IF(XMAG(I,J).EQ.0.) GO TO 1300
      NPT  = NPT + 1
      RLON(NPT) = XLONW + (I-1)*DLON
      RLAT(NPT) = XLATS + (J-1)*DLAT
      DUMM(NPT) = 3.
      ISYM(NPT) = 15
 1300 CONTINUE

      WRITE(6,'('' I PUNTI DI MARE SONO --->'',I10)') NPT

C     DEFINE ARRAYS FOR LONG AND LAT
      CALL PSET1R ('SYMBOL_INPUT_X_POSITION',       RLON,NPT)
      CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',       RLAT,NPT)
      CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',      DUMM,NPT)
      CALL PSETC  ('SYMBOL_TABLE_MODE',                'OFF')
      CALL PSETC  ('SYMBOL_TYPE',                   'MARKER')
ccc   CALL PSET1I ('SYMBOL_INPUT_MARKER_LIST',      ISYM,NPT)
      CALL PSETI  ('SYMBOL_MARKER',                       15)  
      CALL PSETC  ('SYMBOL_COLOUR',                  'BLACK')
c     CALL PSETC  ('SYMBOL_QUALITY',                'MEDIUM')
      CALL PSETR  ('SYMBOL_HEIGHT',                      0.3)

      CALL PSYMB
C  ...............................................................

C          DEFINE THE SEA POINTS LONGITUDE , LATITUDE AND SYMBOL

      NPT = 0
      DO 1400 I=1,ID1,5
      DO 1400 J=1,ID2,5
      IF(XMAG(I,J).EQ.0.) GO TO 1400
      NPT  = NPT + 1
      RLON(NPT) = XLONW + (I-1)*DLON
      RLAT(NPT) = XLATS + (J-1)*DLAT
      DUMM(NPT) = 3.
      ISYM(NPT) = 15
 1400 CONTINUE

      WRITE(6,'('' I PUNTI DI MARE SONO --->'',I10)') NPT

C     DEFINE ARRAYS FOR LONG AND LAT
      CALL PSET1R ('SYMBOL_INPUT_X_POSITION',       RLON,NPT)
      CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',       RLAT,NPT)
      CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',      DUMM,NPT)
      CALL PSETC  ('SYMBOL_TABLE_MODE',                'OFF')
      CALL PSETC  ('SYMBOL_TYPE',                   'MARKER')
ccc   CALL PSET1I ('SYMBOL_INPUT_MARKER_LIST',      ISYM,NPT)
      CALL PSETI  ('SYMBOL_MARKER',                       15)  
      CALL PSETC  ('SYMBOL_COLOUR',                  'BLACK')
c     CALL PSETC  ('SYMBOL_QUALITY',                'MEDIUM')
      CALL PSETR  ('SYMBOL_HEIGHT',                      0.6)

      CALL PSYMB

C  ...............................................................

C     PLOT NESTED GRID BOUNDARIES
ccc   CALL GRIDS

C      8.1 PLOT MEASUREMENT POSITION
C          ------------------------- 

C       DEFINE ARROWS FOR LONG AND LAT
        CALL PSET1R ('SYMBOL_INPUT_X_POSITION',         BLON,2)   
        CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',         BLAT,2)   
        CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',        BDUM,2)  

        CALL PSETC  ('SYMBOL_TABLE_MODE',                'OFF')
        CALL PSETC  ('SYMBOL_TYPE',                   'MARKER')
        CALL PSETI  ('SYMBOL_MARKER',                       17)  
        CALL PSETC  ('SYMBOL_COLOUR',                  'BLACK')  
        CALL PSETR  ('SYMBOL_HEIGHT',                      1.1)

        CALL PSYMB

C      8.2 PLOT NAME OF BUOYS
C          ------------------

c     data blat /58.97, 58.98/
c     data blon /-3.39, -3.40/
C                BOA-E, BOA-F

      CALL PSETC ('TEXT_MODE',           'POSITIONAL')
      CALL PSETC ('TEXT_JUSTIFICATION',      'CENTRE')
      CALL PSETR ('TEXT_FONT_SIZE',               0.7)
      CALL PSETC ('TEXT_COLOUR',              'BLACK')
      CALL PSETC ('TEXT_QUALITY',              'HIGH')
      CALL PSETI ('TEXT_LINE_COUNT',                1)

      CALL PSETR ('TEXT_BOX_X_POSITION',          3.8)
      CALL PSETR ('TEXT_BOX_Y_POSITION',          9.0)
      CALL PSETC ('TEXT_LINE_1',                  'E')
      CALL PTEXT
      CALL PSETR ('TEXT_BOX_X_POSITION',          5.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',          8.1)
      CALL PSETC ('TEXT_LINE_1',                  'F')
      CALL PTEXT



        CALL PSYMB

      END


C========================================================================

      SUBROUTINE GRIDS

C     THIS ROUTINE IS USED TO PLOT GRID A-B-C IN THE IRANIAN ZONE

      PARAMETER (NPG=5)

      REAL XC1(5), YC1(5)

C     JONIAN AREA
C     DATA XC1/ 15.0, 15.0, 21.0, 21.0, 15.0/
C     DATA YC1/ 37.0, 42.5, 42.5, 37.0, 37.0/
C     AEGEAN AREA
      DATA XC1/ 19.0, 19.0, 29.0, 29.0, 19.0/
      DATA YC1/ 34.0, 41.0, 41.0, 34.0, 34.0/

C ......................................................................

c     dimensions of the page, XLEN and YLEN, are extracted from MAGICS
c     and graph limits accordingly defined

      call PENQR ('$SUBPAGE_X_LENGTH', xlen)
      call PENQR ('$SUBPAGE_Y_LENGTH', ylen)

c     first X-axis
      call PSETR ('AXIS_MIN_VALUE',0.)
      call PSETR ('AXIS_MAX_VALUE',xlen)
      call PSETC ('AXIS_TICK','OFF')
      call PSETC ('AXIS_TICK_LABEL','OFF')
      call PAXIS

c     Y-axis
      CALL PSETC ('AXIS_POSITION', 'LEFT')
      CALL PSETC ('AXIS_ORIENTATION', 'VERTICAL')
      call PSETR ('AXIS_MIN_VALUE',0.)
      call PSETR ('AXIS_MAX_VALUE',ylen)
      call PAXIS

      call PSETI ('GRAPH_LINE_THICKNESS',10)
      call PSETC ('GRAPH_LINE_STYLE','SOLID')
      call PSETC ('GRAPH_LINE_COLOUR','RED')

C     ***   GRID  'A'  ***
C     geographical coordinates XC.(), YC.() are transformed to page coordinates
ccc   CALL PAPROJ (XC1,YC1,NPG)
c     PLOTTING LIMITS ARE SET
      CALL PSET1R ('GRAPH_CURVE_X_VALUES',XC1,NPG)
      CALL PSET1R ('GRAPH_CURVE_Y_VALUES',YC1,NPG)
C     PLOT IS DONE
      call PGRAPH

      RETURN 
      END
