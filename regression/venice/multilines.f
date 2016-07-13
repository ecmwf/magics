      PROGRAM SEAPLT

C --------------------------------------------------------------------------
C          THIS PROGRAM PLOTS THE NORTH ATIANTIC COAST-LINE
C          LUCIANA  BERTOTTI   NOV. 2013
C
C --------------------------------------------------------------------------

      CHARACTER RUN*11
      CHARACTER TITLE1*50, TITLE2*50, TITLE3*50, TITLE4*50
      CHARACTER TITLE5*50, TITLE6*50, TITLE7*50, TITLE8*50

C----------------------------------------------------

      CHARACTER*10 FNAME
      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(1)
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
       CALL PSETC  ('OUTPUT_NAME',               FNAME)
       CALL PSETR  ('OUTPUT_ps_scale' ,           0.93)

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
      CALL PSETC ('SUPER_PAGE_FRAME',           'OFF')
      CALL PSETC('PAGE_FRAME',                  'OFF')

C          DEFINE SUBPAGE

      CALL PSETR('SUBPAGE_X_LENGTH',             26.0)
      CALL PSETR('SUBPAGE_Y_LENGTH',             18.0)

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
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION',        'CENTRE')
      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',          -30.0)
      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',            55.0)
      CALL PSETR('SUBPAGE_MAP_VERTICAL_LONGITUDE',        -30.0)
      CALL PSETR('SUBPAGE_MAP_SCALE',                    35.0E6) 
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

      CALL PSETR('MAP_GRID_LONGITUDE_REFERENCE',  180.0) 
      CALL PSETR('MAP_GRID_LATITUDE_REFERENCE',     0.0) 
      CALL PSETR('MAP_GRID_LONGITUDE_INCREMENT',    5.0)
      CALL PSETR('MAP_GRID_LATITUDE_INCREMENT',     5.0)
      CALL PSETC('MAP_GRID_COLOUR',             'BLACK')
      CALL PSETC('MAP_GRID_LINE_STYLE',           'DOT')
      CALL PSETI('MAP_LABEL_LONGITUDE_FREQUENCY',     2)
      CALL PSETI('MAP_LABEL_LATITUDE_FREQUENCY',      2)

      CALL PSETC('MAP_COASTLINE_COLOUR',        'BLACK')
ccc      CALL PSETC('MAP_COASTLINE_RESOLUTION',     'HIGH')
      CALL PSETI('MAP_COASTLINE_THICKNESS',           3)
      CALL PSETC('MAP_COASTLINE_LAND_SHADE',       'ON')
      CALL PSETC('MAP_COASTLINE_LAND_SHADE_COLOUR','CREAM')
      CALL PCOAST
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',       'OFF')
      CALL PCOAST

C ..........................................................................
C          PLOT TITLE

c        WRITE(TITLE1,100)
c100     FORMAT('MEDITERRANEAN - GRIDS')
       TITLE1 =  'GRIDS:'
       TITLE2 = 'A:  ECMWF'
       TITLE3 = 'B:  METEO-FRANCE'
       TITLE4 = 'C:  NETTUNO'
       TITLE5 = 'D:  PROT. CIV.  ITALIA'
       TITLE6 = 'E:  PUERTOS DEL ESTADO'
       TITLE7 = 'F:  SHOM'
       TITLE8 = 'G:  UKMO'

         CALL PSETC ('TEXT_MODE',               'POSITIONAL')
         CALL PSETC ('TEXT_JUSTIFICATION',            'LEFT')
         CALL PSETC ('TEXT_COLOUR',                  'BLACK')
         CALL PSETC ('TEXT_BOX_BLANKING',               'ON')
         CALL PSETR ('TEXT_BOX_X_POSITION',              2.5)
         CALL PSETR ('TEXT_BOX_Y_POSITION',              3.4)
         CALL PSETR ('TEXT_BOX_X_LENGTH',                5.3)
         CALL PSETR ('TEXT_BOX_Y_LENGTH',                4.0)
         CALL PSETC ('TEXT_QUALITY',                  'HIGH')
         CALL PSETI ('TEXT_FIRST_LINE',                    1)
         CALL PSETI ('TEXT_LINE_COUNT',                    1)
         CALL PSETI ('TEXT_LINE_COUNT',                    2)
         CALL PSETI ('TEXT_LINE_COUNT',                    3)
         CALL PSETI ('TEXT_LINE_COUNT',                    4)
         CALL PSETI ('TEXT_LINE_COUNT',                    5)
         CALL PSETI ('TEXT_LINE_COUNT',                    6)
         CALL PSETI ('TEXT_LINE_COUNT',                    7)
         CALL PSETI ('TEXT_LINE_COUNT',                    8)
         CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',  0.4)
         CALL PSETC ('TEXT_LINE_1',                   TITLE1)
         CALL PSETC ('TEXT_LINE_2',                   TITLE2)
         CALL PSETC ('TEXT_LINE_3',                   TITLE3)
         CALL PSETC ('TEXT_LINE_4',                   TITLE4)
         CALL PSETC ('TEXT_LINE_5',                   TITLE5)
         CALL PSETC ('TEXT_LINE_6',                   TITLE6)
         CALL PSETC ('TEXT_LINE_7',                   TITLE7)
         CALL PSETC ('TEXT_LINE_8',                   TITLE8)
c        note: the text is written after the plot of the grids
c              at the end of the plot
ccc      CALL PTEXT

C ..........................................................................
C          DEFINE THE SEA POINTS LONGITUDE , LATITUDE AND SYMBOL

c     define area of interest
      RLIMW =  3.0
      RLIME = 10.0
      RLIMS = 37.0
      RLIMN = 44.0

      NBOUW = ((RLIMW-XLONW)/DLON)+1.01
      NBOUE = ((RLIME-XLONW)/DLON)+1.01
      NBOUS = ((RLIMS-XLATS)/DLAT)+1.01
      NBOUN = ((RLIMN-XLATS)/DLAT)+1.01


cc       write(6,'(20('' .''))')
cc       write(6,'(''LIMITI:        WEST -  EAST - SOUTH - NORTH'')')  
cc       write(6,'(''LIMITI GRID:'',4f7.2)') XLONW,XLONE,XLATS,XLATN
cc       write(6,'('' STEP      :'', 2f7.2)') DLON,DLAT
cc       write(6,'(''LIMITI AREA:'',4f7.2)') RLIMW,RLIME,RLIMS,RLIMN
cc       write(6,'(''LIMITI NUM :'',4i7)')   NBOUW,NBOUE,NBOUS,NBOUN
cc       write(6,'(20('' .''))')

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

c    here is the plot of the grids name
cccccccccc         CALL PTEXT

c    plot symbol at buoy location

c     RLON(1) =  4.70
c     RLAT(1) = 42.10
c     DUMM(1) =  3.0
C
c     CALL PSET1R ('SYMBOL_INPUT_X_POSITION',     RLON,1)
c     CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',     RLAT,1)
c     CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',    DUMM,1)
c     CALL PSETC  ('SYMBOL_TABLE_MODE',            'OFF')
c     CALL PSETC  ('SYMBOL_TYPE',               'MARKER')
ccc   CALL PSET1I ('SYMBOL_INPUT_MARKER_LIST',    ISYM,1)
c     CALL PSETC  ('SYMBOL_COLOUR',              'BLACK')
c     CALL PSETC  ('SYMBOL_QUALITY',            'MEDIUM')
c     CALL PSETR  ('SYMBOL_HEIGHT',                  0.5)
c     CALL PSETI  ('SYMBOL_MARKER_INDEX',             15)

c     CALL PSYMB
C =======================================================================

C 3.0      CLOSE MAGICS
C          ------------

      CALL PCLOSE

      STOP
      END


C========================================================================

      SUBROUTINE GRIDS

C     THIS ROUTINE IS USED TO PLOT GRIDS BOUNDARIES ON MED SEA

      CHARACTER*1 A,B,C
      DIMENSION A(1),B(1),C(1)


c -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      REAL XLOAT(244), YLAAT(244)           !  WHOLE N ATLANTIC
      REAL XLOP1(44), YLAP1(44)             !  AREA N 1
      REAL XLOP2(44), YLAP2(44)             !  AREA N 2
      REAL XLOP3(38), YLAP3(38)             !  AREA N 3
      REAL XLOP4(38), YLAP4(38)             !  AREA N 4
      REAL XLOP56(48), YLAP56(48)           !  AREA N 5 and 6
      REAL XLOP7(54), YLAP7(54)             !  AREA N 7


c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA N. 1

      ind = 1
      DO 110 I=-50,-40
        XLOP1(ind) = i
        YLAP1(ind) = 40.
        ind = ind+1
 110  continue
      DO 120 I=40,50
        XLOP1(ind) = -40.
        YLAP1(ind) = i
        ind = ind+1
 120  continue
      DO 130 I=-40,-50,-1
        XLOP1(ind) = i
        YLAP1(ind) = 50.
        ind = ind+1
 130  continue
      DO 140 I=50,40,-1
        XLOP1(ind) = -50.
        YLAP1(ind) = i
        ind = ind+1
 140  continue

      ITP1 = ind-1
      write(6,'(" i punti di AREA N. 1   sono",i5)') ITP1

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA N. 2

      ind = 1
      DO 210 I=-40,-30
        XLOP2(ind) = i
        YLAP2(ind) = 40.
        ind = ind+1
 210  continue
      DO 220 I=40,50
        XLOP2(ind) = -30.
        YLAP2(ind) = i
        ind = ind+1
 220  continue
      DO 230 I=-30,-40,-1
        XLOP2(ind) = i
        YLAP2(ind) = 50.
        ind = ind+1
 230  continue
      DO 240 I=50,40,-1
        XLOP2(ind) =-40.
        YLAP2(ind) = i
        ind = ind+1
 240  continue

      ITP2 = ind-1
      write(6,'(" i punti di AREA n. 2   sono",i5)') ITP2

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA N. 3

      ind = 1
      DO 310 I=-65,-55
        XLOP3(ind) = i
        YLAP3(ind) = 35.0
        ind = ind+1
 310  continue
      DO 320 I=35,42
        XLOP3(ind) = -55.-0.2
        YLAP3(ind) = i
        ind = ind+1
 320  continue
      DO 330 I=-55,-65,-1
        XLOP3(ind) = i
        YLAP3(ind) = 42.
        ind = ind+1
 330  continue
      DO 340 I=42,35,-1
        XLOP3(ind) = -65.+0.3
        YLAP3(ind) = i
        ind = ind+1
 340  continue

      ITP3 = ind-1
      write(6,'(" i punti di AREA n. 3   sono",i5)') ITP3

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA N. 4

      ind = 1
      DO 410 I=-65,-55
        XLOP4(ind) = i
        YLAP4(ind) = 35.0
        ind = ind+1
 410  continue
      DO 420 I=35,42
        XLOP4(ind) = -55.
        YLAP4(ind) = i
        ind = ind+1
 420  continue
      DO 430 I=-55,-65,-1
        XLOP4(ind) = i
        YLAP4(ind) = 45.-0.2
        ind = ind+1
 430  continue
      DO 440 I=42,35,-1
        XLOP4(ind) = -65.
        YLAP4(ind) = i
        ind = ind+1
 440  continue

      ITP4 = ind-1
      write(6,'(" i punti di AREA N. 4   sono",i5)') ITP4

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA N. 5 and 6

      ind = 1
      DO 510 I= -25,-10
        XLOP56 (ind) = i
        YLAP56 (ind) = 35.
        ind = ind+1
 510  continue
      DO 520 I=35,42
        XLOP56 (ind) = -10.
        YLAP56 (ind) = i
        ind = ind+1
 520  continue
      DO 530 I=-10,-25,-1
        XLOP56 (ind) = i
        YLAP56 (ind) = 42.
        ind = ind+1
 530  continue
      DO 540 I=42,35,-1
        XLOP56 (ind) = -25.
        YLAP56 (ind) = i
        ind = ind+1
 540  continue

      ITP56 = ind-1
      write(6,'(" i punti di AREA N. 5&6 sono",i5)') ITP56

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
c      for AREA N. 7

      ind = 1
      DO 710 I= -65,-50
        XLOP7  (ind) = i
        YLAP7  (ind) = 35.
        ind = ind+1
 710  continue
      DO 720 I=35,45
        XLOP7  (ind) = -50.
        YLAP7  (ind) = i
        ind = ind+1
 720  continue
      DO 730 I=-50,-65,-1
        XLOP7  (ind) = i
        YLAP7  (ind) = 45.
        ind = ind+1
 730  continue
      DO 740 I=45,35,-1
        XLOP7  (ind) = -65.-0.3
        YLAP7  (ind) = i
        ind = ind+1
 740  continue

      ITP7  = ind-1
      write(6,'(" i punti di AREA N. 7   sono",i5)') ITP7 



c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for THE WHOLE NORTH ATLANTIC

      ind = 1
      DO 610 I=-70,15
        XLOAT(ind) = i
        YLAAT(ind) = 35.
        ind = ind+1
 610  continue
      DO 620 I=35,70
        XLOAT(ind) = 15.
        YLAAT(ind) = i
        ind = ind+1
 620  continue
      DO 630 I=15,-70,-1
        XLOAT(ind) = i
        YLAAT(ind) = 70.
        ind = ind+1
 630  continue
      DO 640 I=70,35,-1
        XLOAT(ind) = -70.
        YLAAT(ind) = i
        ind = ind+1
 640  continue

      ITPAT = ind-1
      write(6,'(" i punti di N ATLANTIC  sono",i5)') ITPAT


c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA N.1

c     call PSETC ('GRAPH_LINE_COLOUR','RED')
      call pset1r ("polyline_input_latitudes",  YLAP1,ITP1)
      call psetc  ("polyline_line_colour",           "RED")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP1,ITP1)
      call pset1r ("polyline_input_values",     XLOP1,ITP1)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA N.2

      call pset1r ("polyline_input_latitudes",  YLAP2,ITP2)
      call psetc  ("polyline_line_colour",           "RED")
      call psetc  ("polyline_line_style",            "DOT")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP2,ITP2)
      call pset1r ("polyline_input_values",     XLOP2,ITP2)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA N.7

      call PSETC ('GRAPH_LINE_COLOUR','BLUE')
      call pset1r ("polyline_input_latitudes",  YLAP7,ITP7)
      call psetc  ("polyline_line_colour",          "BLUE")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP7,ITP7)
      call pset1r ("polyline_input_values",     XLOP7,ITP7)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA N.4

      call PSETC ('GRAPH_LINE_COLOUR','BLUE')
      call pset1r ("polyline_input_latitudes",  YLAP4,ITP4)
      call psetc  ("polyline_line_colour",        "ORANGE")
      call psetc  ("polyline_line_style",           "DASH")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP4,ITP4)
      call pset1r ("polyline_input_values",     XLOP4,ITP4)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA N.3

      call pset1r ("polyline_input_latitudes",  YLAP3,ITP3)
      call psetc  ("polyline_line_colour",         "GREEN")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP3,ITP3)
      call pset1r ("polyline_input_values",     XLOP3,ITP3)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for AREA n. 5 & 6

      call pset1r ("polyline_input_latitudes",  YLAP56,ITP56)
      call psetc  ("polyline_line_colour",          "CYAN")
      call psetc  ("polyline_line_style",          "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes", XLOP56,ITP56)
      call pset1r ("polyline_input_values",     XLOP56,ITP56)
      call pline

c -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

c      for WHOLE NORTH ATLANTIC

      call pset1r ("polyline_input_latitudes", YLAAT ,ITPAT)
      call psetc  ("polyline_line_colour",         "BLACK")
      call psetc  ("polyline_line_style",         "SOLID")
      call pseti  ("polyline_line_thickness",            8)
      call pset1r ("polyline_input_longitudes",XLOAT ,ITPAT)
      call pset1r ("polyline_input_values",    XLOAT ,ITPAT)
      call pline

C ......................................................................

C     plot NUMBERS  1 to 7
      CALL PSETC ('TEXT_MODE',           'POSITIONAL')
      CALL PSETC ('TEXT_JUSTIFICATION',      'CENTRE')
      CALL PSETR ('TEXT_FONT_SIZE',               0.7)
      CALL PSETC ('TEXT_COLOUR',              'BLACK')
      CALL PSETC ('TEXT_QUALITY',              'HIGH')
      CALL PSETI ('TEXT_LINE_COUNT',                1)
      CALL PSETR ('TEXT_BOX_X_length',             .5)
      CALL PSETR ('TEXT_BOX_Y_length',             .5)

      CALL PSETR ('TEXT_BOX_X_POSITION',     10.1)
      CALL PSETR ('TEXT_BOX_Y_POSITION',      8.0)
      CALL PSETC ('TEXT_LINE_1',              '1')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_X_POSITION',     12.7)
      CALL PSETR ('TEXT_BOX_Y_POSITION',      7.6)
      CALL PSETC ('TEXT_LINE_1',              '2')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_X_POSITION',      5.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',      6.0)
      CALL PSETC ('TEXT_LINE_1',              '3')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_X_POSITION',      6.1)
      CALL PSETR ('TEXT_BOX_Y_POSITION',      8.2)
      CALL PSETC ('TEXT_LINE_1',              '4')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_X_POSITION',     16.3)
      CALL PSETR ('TEXT_BOX_Y_POSITION',      4.9)
      CALL PSETC ('TEXT_LINE_1',            '5-6')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_X_POSITION',      8.1)
      CALL PSETR ('TEXT_BOX_Y_POSITION',      6.0)
      CALL PSETC ('TEXT_LINE_1',              '7')
      CALL PTEXT

C ......................................................................
      RETURN 
      END

