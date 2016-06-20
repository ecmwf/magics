C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM SEAPLT

C --------------------------------------------------------------------------
C          THIS PROGRAM PLOTS THE MEDITERRANEAN COAST-LINE
C          LUCIANA  BERTOTTI   AUG. 2009
C
C --------------------------------------------------------------------------

C     GRID DIMENIONS

      PARAMETER (ID1=169, ID2=65)

      LOGICAL IEOF
      CHARACTER AX(ID1)*1

      CHARACTER RUN*11
      CHARACTER TITLE1*50, TITLE2*50, TITLE3*50, TITLE4*50
      CHARACTER TITLE5*50, TITLE6*50, TITLE7*50, TITLE8*50

      DIMENSION TOPO(ID1,ID2)
      INTEGER   IBAT(ID1,ID2)

      REAL RLON(5000), RLAT(5000), DUMM(5000)
      INTEGER ISYM(5000)

C----------------------------------------------------

      CHARACTER*10 FNAME
      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(1)
c     DATA         FORMATS /'GIF'/
      DATA         FORMATS /'PS'/
ccc      DIMENSION    FORMATS(2)
ccc      DATA         FORMATS /'PS', 'JPEG'/
c     DIMENSION    FORMATS(2)
c     DATA         FORMATS /'PS', 'PNG'/

C ________________________________________________________________________
C

C 1.0      INFORMATION ON ACTUAL RUN IS READ
C          ---------------------------------

      RUN = '           '

C 2.0      OPEN MAGICS
C          -----------

      write(FNAME,110) 
 110  format('BOX_ON_MAP')

       CALL POPEN
       CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 1)
c      CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 2)
       CALL PSETC  ('OUTPUT_NAME',               FNAME)
c      CALL PSETC  ('OUTPUT_NAME',        'BOX_ON_MAP')

c Enables the output to be split into different (single page) PostScript files
c      CALL PSETC  ('OUTPUT_ps_split',            'ON')
c Defines the PostScript scale between 0.1 and 1.0 - def.=1.0
c      CALL PSETR  ('OUTPUT_ps_scale' ,            0.5)

C =======================================================================

C          IDENTIFICATION TEXT OF PLOT

      CALL PSETC ('PAGE_ID_LINE',                'OFF')
CCC   CALL PSETC ('PAGE_ID_LINE',                 'ON')
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',      'OFF')
      CALL PSETC ('PAGE_ID_LINE_SYSTEM_PLOT',    'OFF')
      CALL PSETC ('PAGE_ID_LINE_DATE_PLOT',      'OFF')
      CALL PSETC ('PAGE_ID_LINE_ERRORS_PLOT',    'OFF')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',  'ON')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT',        RUN)
      CALL PSETR ('PAGE_ID_LINE_HEIGHT',           0.2)

C
C          THESE ARE THE PAGE DEFINITIONS FOR SIZE A4

      CALL PSETR('SUPER_PAGE_X_LENGTH',          29.7)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',          21.0)
      CALL PSETR('PAGE_X_LENGTH',                29.7)
      CALL PSETR('PAGE_Y_LENGTH',                21.0)
      CALL PSETC('PAGE_FRAME',                  'OFF')

C          DEFINE SUBPAGE

      CALL PSETR('SUBPAGE_X_LENGTH',             26.0)
      CALL PSETR('SUBPAGE_Y_LENGTH',             14.0)

C ..........................................................................
C          CHOOSE THE PROJECTION

C     CHOOSE THE PROJECTION
C     111  =  POLAR STEREOGRAPHIC
C     222  =  MERCATOR
C     333  =  AITOFF - the whole globe in one map

      GO TO 111

c ===++++====++++====++++====++++====++++====++++====++++====++++====
 111  CONTINUE
C     STEREOGRAPHIC PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION', 'CENTRE')
      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',    15.0)
      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',     38.0)
      CALL PSETR('SUBPAGE_MAP_VERTICAL_LONGITUDE',  15.0)
      CALL PSETR('SUBPAGE_MAP_SCALE',             18.0E6) 
      GO TO 1100

c ===++++====++++====++++====++++====++++====++++====++++====++++====
 222  CONTINUE
C     MERCATOR    PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION',   'MERCATOR')
cc      CALL PSETR('SUBPAGE_LOWER_LEFT_LONGITUDE',   69.0) 
cc      CALL PSETR('SUBPAGE_LOWER_LEFT_LATITUDE',    11.0) 
cc      CALL PSETR('SUBPAGE_UPPER_RIGHT_LONGITUDE',  76.0)
cc      CALL PSETR('SUBPAGE_UPPER_RIGHT_LATITUDE',   19.0) 
      CALL PSETR('SUBPAGE_LOWER_LEFT_LONGITUDE',   44.0) 
      CALL PSETR('SUBPAGE_LOWER_LEFT_LATITUDE',     4.0) 
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LONGITUDE',  86.0)
      CALL PSETR('SUBPAGE_UPPER_RIGHT_LATITUDE',   26.0) 
      GO TO 1100

c ===++++====++++====++++====++++====++++====++++====++++====++++====
 333  CONTINUE
C     AITOFF   PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION',      'AITOFF')
      CALL PSETR('SUBPAGE_MAP_VERTICAL_LONGITUDE',-100.0)
      GO TO 1100

c ===++++====++++====++++====++++====++++====++++====++++====++++====

 1100  CONTINUE

      CALL PSETR('MAP_GRID_LONGITUDE_REFERENCE',    0.0) 
      CALL PSETR('MAP_GRID_LATITUDE_REFERENCE',    30.0) 
      CALL PSETR('MAP_GRID_LONGITUDE_INCREMENT',    1.0)
      CALL PSETR('MAP_GRID_LATITUDE_INCREMENT',     1.0)
      CALL PSETC('MAP_GRID_COLOUR',             'BLACK')
      CALL PSETC('MAP_GRID_LINE_STYLE',           'DOT')
      CALL PSETI('MAP_LABEL_LONGITUDE_FREQUENCY',     2)
      CALL PSETI('MAP_LABEL_LATITUDE_FREQUENCY',      2)

      CALL PSETC('MAP_COASTLINE_COLOUR',        'BLACK')
      CALL PSETC('MAP_COASTLINE_RESOLUTION',     'HIGH')
      CALL PSETI('MAP_COASTLINE_THICKNESS',           6)
      CALL PSETC('MAP_COASTLINE_LAND_SHADE',       'ON')
      CALL PSETC('MAP_COASTLINE_LAND_SHADE_COLOUR','CREAM')
      CALL PCOAST
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',          'OFF')
      CALL PCOAST


c    plot symbol at buoy location

      RLON(1) =  4.70
      RLAT(1) = 42.10
      DUMM(1) =  3.0
      ISYM(1) =  15
C
      CALL PSET1R ('SYMBOL_INPUT_X_POSITION',     RLON,1)
      CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',     RLAT,1)
      CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',    DUMM,1)
      CALL PSETC  ('SYMBOL_TABLE_MODE',            'OFF')
      CALL PSETC  ('SYMBOL_TYPE',               'MARKER')
      CALL PSETI ('SYMBOL_MARKER_INDEX',    15)
      CALL PSETC  ('SYMBOL_COLOUR',              'BLACK')
c     CALL PSETC  ('SYMBOL_QUALITY',            'MEDIUM')
      CALL PSETR  ('SYMBOL_HEIGHT',                  0.5)

      CALL PSYMB


C ..........................................................................
C          PLOT TITLE

c        WRITE(TITLE1,100)
c100     FORMAT('MEDITERRANEAN - GRIDS')
c        CALL PTEXT

C ..........................................................................
C     PLOT GRID BOUNDARIES

C     WHOLE MED: ECMWF
c     WHOLE MED: NETTUNO
C     WHOLE MED: SHOM
C     WHOLE MED: UKMO

C     BLACK: METEO-FRANCE
C     RED  : PROTEZ.CIVILE
C     GREEN: PUERTOS DEL ESTADO

C     GRAY:  COMMON AREA

      CALL GRIDS

C =======================================================================

C 3.0      CLOSE MAGICS
C          ------------

      CALL PCLOSE

      STOP
      END


C========================================================================

      SUBROUTINE GRIDS

C     THIS ROUTINE IS USED TO PLOT GRIDS over the Mediterranean

c -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      REAL XLOP1(65), YLAP1(65)             !  PROT. CIVILE - WIND

      REAL XLOP2(65), YLAP2(65)             !  PROT. CIVILE - WAVE

      REAL XLOP3(70), YLAP3(70)             !  PUERTOS D ESTADO

      REAL XLOMF(70), YLAMF(70)             !  METEO FRANCE

      REAL XLOINT(50), YLAINT(50)           !  AREA FO INTEREST

      REAL XLOMED(121), YLAMED(121)         !  WHOLE MED SEA


c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for PROTEZIONE-CIVILE  -  WIND

      ind = 1
      DO 110 I=2,20
        XLOP1(ind) = i
        YLAP1(ind) = 46.
        ind = ind+1
 110  continue
      DO 120 I=46,34,-1
        XLOP1(ind) = 20.
        YLAP1(ind) = i
        ind = ind+1
 120  continue
      DO 130 I=20,2,-1
        XLOP1(ind) = i
        YLAP1(ind) = 34.
        ind = ind+1
 130  continue
      DO 140 I=34,46
        XLOP1(ind) = 2.
        YLAP1(ind) = i
        ind = ind+1
 140  continue

      ITP1 = ind-1
      write(6,'(" i punti di PROT.CIV. WIND  sono",i5)') ITP1

c      for PROTEZIONE-CIVILE  -  WAVE

      ind = 1
      DO 210 I=6,20
        XLOP2(ind) = i
        YLAP2(ind) = 45.9
        ind = ind+1
 210  continue
      DO 220 I=46,34,-1
        XLOP2(ind) = 19.9
        YLAP2(ind) = i
        ind = ind+1
 220  continue
      DO 230 I=20,6,-1
        XLOP2(ind) = i
        YLAP2(ind) = 34.1
        ind = ind+1
 230  continue
      DO 240 I=34,46
        XLOP2(ind) = 6.
        YLAP2(ind) = i
        ind = ind+1
 240  continue

      ITP2 = ind-1
      write(6,'(" i punti di PROT.CIV. WAVE sono",i5)') ITP2

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for PUERTOS del ESTADO

c     XO3(1) = -7.
c     YO3(1) = 35.
c     XO4(1) = -7.
c     YO4(1) = 45.

      ind = 1
      DO 310 I=-7,15
        XLOP3(ind) = i
        YLAP3(ind) = 45.0
        ind = ind+1
 310  continue
      DO 320 I=45,35,-1
        XLOP3(ind) = 15.0
        YLAP3(ind) = i
        ind = ind+1
 320  continue
      DO 330 I=15,-7,-1
        XLOP3(ind) = i
        YLAP3(ind) = 35.1
        ind = ind+1
 330  continue
      DO 340 I=35,45
        XLOP3(ind) = -7.
        YLAP3(ind) = i
        ind = ind+1
 340  continue

      ITP3 = ind-1
      write(6,'(" i punti di PUERTO. ESTADO sono",i5)') ITP3

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for METEO - FRANCE

c     XO5(1) = -9.
c     YO5(1) = 34.9

      ind = 1
      DO 410 I=-9,17
        XLOMF(ind) = i
        YLAMF(ind) = 35.0
        ind = ind+1
 410  continue
      DO 420 I=35,48
        XLOMF(ind) = 17.0
        YLAMF(ind) = i
        ind = ind+1
 420  continue

      ITMF = ind-1
      write(6,'(" i punti di METEO.FR.  sono",i5)') ITMF

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA OF INTEREST

      ind = 1
      DO 510 I= 3,10
        XLOINT(ind) = i
        YLAINT(ind) = 44.
        ind = ind+1
 510  continue
      DO 520 I=44,37,-1
        XLOINT(ind) = 10.
        YLAINT(ind) = i
        ind = ind+1
 520  continue
      DO 530 I=10,3 ,-1
        XLOINT(ind) = i
        YLAINT(ind) = 37.
        ind = ind+1
 530  continue
      DO 540 I=37,44
        XLOINT(ind) = 3.
        YLAINT(ind) = i
        ind = ind+1
 540  continue

      ITZO = ind-1
      write(6,'(" i punti di ZONA INT sono",i5)') ITZO

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for THE WHOLE MED SEA

      ind = 1
      DO 610 I=-6,36
        XLOMED(ind) = i
        YLAMED(ind) = 46.
        ind = ind+1
 610  continue
      DO 620 I=46,30,-1
        XLOMED(ind) = 36.
        YLAMED(ind) = i
        ind = ind+1
 620  continue
      DO 630 I=36,-6,-1
        XLOMED(ind) = i
        YLAMED(ind) = 30.
        ind = ind+1
 630  continue
      DO 640 I=30,46
        XLOMED(ind) = -6.
        YLAMED(ind) = i
        ind = ind+1
 640  continue

      ITME = ind-1
      write(6,'(" i punti di MED sono",i5)') ITME

C ......................................................................
c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for PROTEZIONE-CIVILE  -  WIND

c     call PSETC ('GRAPH_LINE_COLOUR','RED')
      call pset1r ("polyline_input_latitudes",  YLAP1,ITP1)
      call psetc  ("polyline_line_colour",           "RED")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP1,ITP1)
      call pset1r ("polyline_input_values",     XLOP1,ITP1)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for PROTEZIONE-CIVILE  -  WAVE

      call pset1r ("polyline_input_latitudes",  YLAP2,ITP2)
      call psetc  ("polyline_line_colour",           "RED")
      call psetc  ("polyline_line_style",            "DOT")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP2,ITP2)
      call pset1r ("polyline_input_values",     XLOP2,ITP2)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for PUERTOS del ESTADO

      call pset1r ("polyline_input_latitudes",  YLAP3,ITP3)
      call psetc  ("polyline_line_colour",         "GREEN")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP3,ITP3)
      call pset1r ("polyline_input_values",     XLOP3,ITP3)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for METEO - FRANCE

c     call PSETC ('GRAPH_LINE_COLOUR','BLUE')
      call pset1r ("polyline_input_latitudes",  YLAMF,ITMF)
      call psetc  ("polyline_line_colour",          "BLUE")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOMF,ITMF)
      call pset1r ("polyline_input_values",     XLOMF,ITMF)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA OF INTEREST

      call pset1r ("polyline_input_latitudes", YLAINT,ITZO)
      call psetc  ("polyline_line_colour","rgb(.4,.4,.4)")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes",XLOINT,ITZO)
      call pset1r ("polyline_input_values",    XLOINT,ITZO)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for WHOLE MED SEA

      call pset1r ("polyline_input_latitudes", YLAMED,ITME)
      call psetc  ("polyline_line_colour",         "BLACK")
      call psetc  ("polyline_line_style",           "DASH")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes",XLOMED,ITME)
      call pset1r ("polyline_input_values",    XLOMED,ITME)
      call pline
      

      RETURN 
      END

