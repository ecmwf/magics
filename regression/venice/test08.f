      PROGRAM CUSTPLT

C          THIS PROGRAM TAKES THE MEDITERRANEAN WAM MODEL GRID AS
C          INPUT AND MAKES A PLOT OF WAVE HEIGHT AND WINDS
C          USING MAGICS.  THE FIRST FILE TO BE FETCHED WILL
C          HAVE THE NAME MAPYYMMDDHHMMII.  THE FIRST DATE
C          FOR THIS FILE WILL BE JDAT1 READ FROM WAMINFO.
C          PLOTS WILL BE MADE EVERY 6 HOURS AT 06,12,18,00 UT
C          UP TO AND INCLUDING JDAT2.

C          LIANA ZAMBRESKY
C          GKSS/ECMWF
C          FEBRUARY 1989 -
C          MODIFIED FOR THE MEDITERRANEAN SEA
C          LUIGI CAVALERI
C          APRIL 1989 -
C          NOVEMBER 1989 -
C          SEPTEMBER 1990 -

C---------------------------------------------------------------------------

C     GRID DIMENSIONS ID1, ID2 ARE CHOSEN LARGE ENOUGH FOR ANY CASE -
C     ACTUAL DIMENSIONS NGX, NGY ARE LATER READ FROM WAM MAP... OUTPUT FILE -

      PARAMETER (ID1=97,ID2=73)

C     PATH IS A 40 CHARACTER WORD TO IDENTIFY THE PATH WHERE TO GET THE FILES
C        FROM -

      LOGICAL IEOF
      CHARACTER LINE*72
      CHARACTER PATH*60
      CHARACTER USERID*3, RUNID*3

      CHARACTER*10 JDATE,JDAT1,JDAT2
      INTEGER(8)   IDT1, IDT2

      DIMENSION ISTAT(3)
      DIMENSION WAVEHT(ID1,ID2),WAVEDIR(ID1,ID2)
      DIMENSION WINDSPD(ID1,ID2),WINDDIR(ID1,ID2)

cccc  DATA PATH/'work/adrioper/results             '/
ccc   DATA PATH/'results             '/
casa  DATA PATH/'/home/luciana/work/adrioper/results             '/
casa  DATA PATH/'/home/luciana/work/adrioper/postpro             '/
      DATA PATH/'.'/
      DATA USERID/'XXX'/, RUNID/'XXX'/

      COMMON /TEXT/ USERID, RUNID, PATH

       COMMON/MAX/ WAMAX,WIMAX,STMAX

      COMMON /DATEA/ IYA,IMA,IDA,IHA
      COMMON /DATFF/ IHFF
      COMMON /DATEF/ IYF,IMF,IDF,IHF

C----------------------------------------------------

C     NPC:   NUMBER OF POINTS IN EACH COASTLINE-PART
C     NIS:   NUMBER OF PARTS
      PARAMETER (NPC=1500, NIS=873)

C     COASTAL POINTS INFO
c.... XA(),YA()    geographical coordinates of the Adriatic coast -
c.... XJ(,),YJ(,)  geographical coordinates of jugo islands -
c
        real xa(NPC),ya(NPC)
        real xj(NIS,NPC),yj(NIS,NPC)
        integer NPIS(NIS)

      COMMON /COASTA/ XA, YA
      COMMON /COASTJ/ XJ, YJ 
      COMMON /NPOINT/ NPIS

C----------------------------------------------------

      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(1)
      DATA         FORMATS /'PS'/
cc    DIMENSION    FORMATS(2)
cc    DATA         FORMATS /'PS', 'PNG'/

C-------------------------------------------------------------------------

C      IFORM IS TO READ UNFORMATTED RESULTS FROM  'MAP'  FILES IFORM=0
C                   OR    FORMATTED               'OUT'  FILES IFORM=1
       IFORM=0
       IFORM=1

C 1.1    READ ADRIATIC COASTLINE (equatorial grid)
C        -----------------------------------------

      CALL  READCOS

C          GET BEGIN AND END DATES FROM WAMINFO

cccc  CALL READSTA (60,IBGNDT,IENDDT,IANALPD,IFOREPD,ISHIFT,
cccc *              JDAT1,JDAT2,IASS,NF,ISTAT)
C_________________________________________________________________________
C          JDAT1, JDAT2 ARE READ IN SUB READSTA, BUT, PARTICULARLY FOR TEST,
C          THEY CAN BE ASSIGNED - THIS SAVES STORING OF THE REQUIRED
C          WAMINFO IN ECFILE -
ccc   JDAT1 = '9910240000'
ccc   JDAT2 = '9910240000'
cc    JDAT1 = '0102020000'
cc    JDAT2 = '0102030000'
cc      JDAT1 = '0210230000'
cc      JDAT2 = '0210230000'
ccc      JDAT1 = '0205020000'
ccc      JDAT2 = '0205030000'
      JDAT1 = '0904281200'
      JDAT2 = '0904281200'
C_________________________________________________________________________
      READ(JDAT1,'(I10.10)') IDT1
      IDT1 = IDT1*100
      IDT2 = IDT1+24
      IDT2 = IDT1+72
      JDAT1=JDAT1
      JDAT2=JDAT1
      CALL INCDATE(JDAT2,3600*72)
C_______________________________________
C
c     WRITE(6,*) ' CUSTPLT - BEGIN DATE = ',JDAT1,'  END DATE = ',
c    *             JDAT2
      WRITE(6,*) ' CUSTPLT - BEGIN DATE = ',IDT1,'  END DATE = ',
     *             IDT2
      JDATE = JDAT1
      IEOF = .FALSE.

C          OPEN MAGICS

       CALL POPEN
       CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 1)
cc     CALL PSET1C ('OUTPUT_FORMATS',       FORMATS, 2)
       CALL PSETC  ('OUTPUT_NAME',         'test08')

c Enables the output to be split into different (single page) PostScript files
c Defines the PostScript scale between 0.1 and 1.0 - def.=1.0
c      CALL PSETR  ('OUTPUT_ps_scale' ,            0.5)

ccc   CALL PSETC('WORKSTATION_1','PS_COL')
C     CALL PSETC('WORKSTATION_1','PS')

C          FETCH FILE TO BE PLOTTED

      IUNIT=83
       open(unit=83, file='fort.83')
       open(unit=82, file='fort.82')
c5    CALL GSFILE (6, IUNIT, JDATE, JDATE, 'MAP', 'G')         ! ANALYSIS ONLY
 5    CALL GSFILE (6, IUNIT, JDATE, JDATE, 'OPE', 'G')         ! ANALYSIS ONLY
c5    CALL GSFILE (6, IUNIT, JDATE, JDAT1, 'MAP', 'G')
c5    CALL GSFILE (6, IUNIT, JDATE, JDAT1, 'OPE', 'G')

C          READ WIND AND WAVE FIELDS

 10   CALL FLDS (IUNIT,IDATE,ID1,ID2,WAVEHT,WAVEDIR,WINDSPD,WINDDIR,
     *           NGX,NGY,IEOF,IFORM)
      IF(IEOF) GO TO 1000

C          PLOT AT  00, 06, 12, 18  UT
c             or other times according to commented statements

      IHR=MOD(IDATE,100)
ccc   IF(MOD(IHR,12).NE.0) GO TO 10
CCC   IF(MOD(IHR,6).NE.0) GO TO 10
CCC   IF(MOD(IHR,3).NE.0) GO TO 10
C_________________________________________
C    PLOT ONLY AT 12 UT  _________________
ccccccccccccc      IF(IHR.NE.12) GO TO 10
C_________________________________________

c <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
ccc    write(6,'(''date-wamax-wimax-stmax'',i10,3e10.2)')
ccc  .             IDATE,WAMAX,WIMAX,STMAX

      write(6,'(''sto disegnando:'',i12)') IDATE
c<><><><><><><><><><><><><><><><><><><><><><><><><><><><>

C          PLOT WAVE AND WIND  FIELDS

      CALL FLDPLT (WAVEHT ,WAVEDIR,NGX,NGY,1,IDATE)
cc      CALL FLDPLT (WINDSPD,WINDDIR,NGX,NGY,2,IDATE)

cccccccccccccccccccc      GO TO 10

 1000 CONTINUE

       CLOSE (IUNIT,STATUS='KEEP')

C          INCREMENT DATE FOR THE NEXT FILE

      IEOF = .FALSE.
      CALL INCDATE (JDATE,3600*24)
cccccccccccccccccccc      IF(JDATE.LE.JDAT2) GO TO 5

C          CLOSE MAGICS

      CALL PCLOSE

      STOP
      END

C========================================================================

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
C          MODIFIED TO THE ADRIATIC SEA 1/12 DEG
C          LUCIANA BERTOTTI  - NOVEMBER 1996
C          MODIFIED FOR NEW COLOUR SCALE AND OUT-LOOK
C          LUCIANA BERTOTTI - MAY 2009

C----------------------------------------------------------------------------

C          VARIABLES DEFINITION

C          XMAG(,)    = 2-DIM VECTOR MODULI
C          XDIR(,)    = 2-DIM VECTOR DIRECTIONS

C----------------------------------------------------------------------------

C          MODIFIED FOR NEW COLOUR SCALE AND OUT-LOOK
C          LUCIANA BERTOTTI - MARCH 2009
C
C          - USE OF NEW-VERSION OF MAGICS++
C          - NEW COAST-LINE
C          - SHADING OF VENITIAN AND GRADO LAGOONS
C          - POSITION AND NAME OF LOCATIONS: VENEZIA, TRIESTE
C          LUCIANA BERTOTTI - NOVEMBER 2009

C     CHOOSE THE COLOURS AND THE LIMITS

      DIMENSION CONLIS (16)
      CHARACTER*20 CLIST(16)
      CHARACTER*20 labels(2)

      DATA CONLIS /0.0, 0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.50,
     x             3.00,3.50,4.00,5.00,6.00,7.00/

      DATA CLIST /'RGB(0,0.4,1)', 'RGB(0,0.6,1)',  'RGB(0,0.78,1)',
     x            'RGB(0,0.89,1)','RGB(0.1,1,0.6)','RGB(0.4,1,0.1)',
     x            'RGB(0.6,1,0)', 'RGB(0.8,1,0)',  'RGB(1,1,0)',
     x            'RGB(1,0.86,0)','RGB(1,0.7,0)',  'RGB(1,0.53,0)',
     x            'RGB(1,0.38,0)',
     x            'RGB(1,0,0)',   'RGB(1,0,0.55)', 'RGB(1,0,0.75)'/

      CHARACTER TITLE1*150,TITLE2*150, TITLE3*150
      character*9 MESE(12)
      data  MESE /' Gennaio ', ' Febbraio', '  Marzo  ', '  Aprile ',
     .            '  Maggio ', '  Giugno ', ' Luglio  ', '  Agosto ',
     .            'Settembre', ' Ottobre ', 'Novembre ', 'Dicembre '/

C     XMAG(,) , XDIR(,) : ORIGINAL MATRICES FROM INPUT
      DIMENSION XMAG(ID1,ID2),XDIR(ID1,ID2)
C     XMAG2(,)          : INTERPOLTED VALUES OVER LAND 
      DIMENSION XMAG2(ID1,ID2)
C     SCALE WAVE MATRIX TO PLOT 'BIGGER' ARROWS
      DIMENSION XMAG3(ID1,ID2)

      CHARACTER RUN*53

      INTEGER IY,IM,ID,IH

      COMMON /DATEA/ IYA,IMA,IDA,IHA
      COMMON /DATFF/ IHFF
      COMMON /DATEF/ IYF,IMF,IDF,IHF
C-----------------------------------------------------------------------
C
C*    *COMMON* *BOUNDS* SCALING OF PLOTS.
C
       COMMON/BOUNDS/XLATS,XLATN,XLONW,XLONE

C          GRID STEP
C     COMMON /GRID/ DLON, DLAT
C
C-----------------------------------------------------------------------

C          GRID STEP
      DLON = 1./12.
      DLAT = 1./12.

C     PREPARE MATRIX TO EXTEND WAVE VALUES OVER LAND
      DO I= 1,ID1
         DO J= 1,ID2
            XMAG2(I,J) = XMAG(I,J)
            XMAG3(I,J) = SQRT(XMAG(I,J)/7.0)*7.0
            IF(XMAG(I,J) < 0.0) XMAG3(I,J) = -0.1
         END DO
      END DO

C          IDENTIFICATION TEXT OF ACTUAL RUN
      RUN = 'wave forecast  ISMAR-CNR  ICPSM - meteo data   CNMCA '
c            -----------------------------------------------------

      CALL PSETC ('PAGE_ID_LINE',                        'ON')
ccc   CALL PSETC ('PAGE_ID_LINE',                       'OFF')
cc    CALL PSETC ('PAGE_ID_LINE_COLOUR',              'WHITE')
      CALL PSETC ('PAGE_ID_LINE_COLOUR',              'BLACK')
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',             'OFF')
      CALL PSETC ('PAGE_ID_LINE_SYSTEM_PLOT',           'OFF')
cc    CALL PSETC ('PAGE_ID_LINE_DATE_PLOT',             'OFF')
      CALL PSETC ('PAGE_ID_LINE_ERRORS_PLOT',           'OFF')
cc    CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',        'OFF')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT',         'ON')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT',               RUN)
      CALL PSETR ('PAGE_ID_LINE_HEIGHT',                 0.50)
C          DEFINE SUPERPAGE AND PAGE

      CALL PSETR ('SUPER_PAGE_X_LENGTH',                 29.7)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',                 21.0)
      CALL PSETR ('PAGE_X_LENGTH',                       29.7)
      CALL PSETR ('PAGE_Y_LENGTH',                       21.0)
      CALL PSETC ('PAGE_FRAME',                         'OFF')
C

C          DEFINE SUBPAGE
      CALL PSETR ('SUBPAGE_X_LENGTH',                    24.0)
      CALL PSETR ('SUBPAGE_Y_LENGTH',                    17.0)

C...........................................................................
C   4.0   CHOOSE THE PROJECTION
C         ---------------------
 4000  CONTINUE

C     CHOOSE THE PROJECTION
C     111  =  POLAR STEREOGRAPHIC
C     222  =  MERCATOR
C     333  =  POLAR STEREOGRAPHIC - NEW DEFINITION

      GO TO 333
ccc   GO TO 222

c====++++====++++====++++====++++====++++====++++====++++====++++====
 111  CONTINUE
C     STEREOGRAPHIC PROJECTION

      CALL PSETC('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION',        'CENTRE')
      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',           13.0)
      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',            45.2)
      CALL PSETR('SUBPAGE_MAP_VERTICAL_LONGITUDE',         13.0)
      CALL PSETR('SUBPAGE_MAP_SCALE',                     9.0E5)
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
      CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',          13.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',             44.7)     ! VENICE
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',            12.0)     ! VENICE
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',            45.8)     ! VENICE
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',           14.0)     ! VENICE
ccc   CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',             39.5)      ! ADRIA
ccc   CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',            12.0)      ! ADRIA
ccc   CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',            46.0)      ! ADRIA
ccc   CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',           20.5)      ! ADRIA
      GO TO 4100

c====++++====++++====++++====++++====++++====++++====++++====++++====

 4100  CONTINUE

C     PREPARE LOGOS
ccccccccc      CALL PSETC ('PAGE_ID_LINE',                'ON')
ccccccccc      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT',    'USER')

C     PREPARE FIRST LOGO
c     CALL PSETC ('import_file_name', 'Logo_ISMAR_ICPSM-E.eps')
c     CALL PSETC ('import_format',           'EPS')
      CALL PSETC ('import_file_name',       'Logo_Ismar.png')   
      CALL PSETC ('import_format',           'PNG')
      CALL PSETR ('import_x_position',         2.2)
      CALL PSETR ('import_Y_POSITION',        18.3)
      CALL PSETR ('import_width',              2.2)
      CALL PSETR ('import_height',             2.2)
      CALL PIMPORT

C
C     PREPARE SECOND LOGO
C
      CALL PSETC ('import_file_name', 'Logo_ICPSM.png')
      CALL PSETC ('import_format',          'PNG')
      CALL PSETR ('import_x_position',      22.6)
      CALL PSETR ('import_y_position',      18.2)
      CALL PSETR ('import_width',            4.5)
      CALL PSETR ('import_height',           2.5)
      call PIMPORT




cc    CALL PSETC ('USER_LOGO_FILENAME', 'logoISMARgrande.jpg')
cc    CALL PSETC ('USER_LOGO_FORMAT',          'JPEG')
ccc   CALL PSETC ('USER_LOGO_FILENAME', 'LogoIsmar.eps')
c     CALL PSETC ('USER_LOGO_FILENAME', 'Logo_ISMAR_ICPSM-E.eps')
c     CALL PSETC ('USER_LOGO_FORMAT',           'EPS')
c     CALL PSETC ('user_logo_position_units',    'cm')
c     CALL PSETR ('USER_LOGO_X_POSITION',        22.0)
c     CALL PSETR ('USER_LOGO_Y_POSITION',        18.3)
cc    CALL PSETR ('USER_LOGO_X_POSITION',        22.3)
cc    CALL PSETR ('USER_LOGO_Y_POSITION',        18.5)
c     CALL PSETR ('USER_LOGO_width',              6.2)
c     CALL PSETR ('USER_LOGO_height',             2.7)
cc    CALL PSETR ('USER_LOGO_width',             16.4)
cc    CALL PSETR ('USER_LOGO_height',             9.2)

c land if filled for plotting contours
      call FILLAND (XMAG2,id1,id2)

c  nonlinear scaling  XMAG for arrows plotting
       do i=id1,id2
         do j=1,id2
           XMAG(i,j) = SQRT(XMAG(I,J))*7./SQRT(7.)
         end do
       end do

C          DEFINE WAVE OR WIND FIELDS

ccc   CALL PSET2R ('INPUT_WIND_SPEED',           XMAG,ID1,ID2)
      CALL PSET2R ('INPUT_FIELD',               XMAG2,ID1,ID2)

      CALL PSETR  ('INPUT_FIELD_INITIAL_LONGITUDE',     XLONW) 
      CALL PSETR  ('INPUT_FIELD_INITIAL_LATITUDE',      XLATS) 
      CALL PSETR  ('INPUT_FIELD_LONGITUDE_STEP',         DLON)
      CALL PSETR  ('INPUT_FIELD_LATITUDE_STEP',          DLAT)
C
C     SET GENERAL MAGICS PARAMETERS
C
      CALL PSETC ('CONTOUR_HILO',                           'OFF')
      CALL PSETC ('CONTOUR',                                'OFF')
      CALL PSETC ('CONTOUR_SHADE',                           'ON')

      CALL PSETC ('LEGEND',                                  'ON')
      CALL PSETC ('LEGEND_DISPLAY_TYPE',             'CONTINUOUS')
      CALL PSETC ('LEGEND_ENTRY_PLOT_DIRECTION',         'COLUMN')
      CALL PSETC ('LEGEND_BOX_MODE',                 'POSITIONAL')
      CALL PSETR ('LEGEND_BOX_X_POSITION',                   24.0)
      CALL PSETR ('LEGEND_BOX_Y_POSITION',                    0.0)
      CALL PSETR ('LEGEND_BOX_X_LENGTH',                      3.)
      CALL PSETR ('LEGEND_BOX_Y_LENGTH',                     20.)
      CALL PSETR ('LEGEND_TEXT_HEIGHT',                       0.4)
      CALL PSETC ('LEGEND_TEXT_COLOUR',                   'BLACK')

      CALL PSETC ('CONTOUR_HIGHLIGHT',                      'OFF')
ccc   CALL PSETC ('CONTOUR_LABEL',                          'OFF')
      CALL PSETC ('CONTOUR_LABEL',                           'ON')
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',                     0.5)
      CALL PSETC ('CONTOUR_LABEL_COLOUR',                 'BLACK')
      CALL PSETC ('CONTOUR_LINE_COLOUR',                   'GREY')
      CALL PSETC ('CONTOUR_LINE_STYLE',                     'DOT')
      CALL PSETI ('CONTOUR_LINE_THICKNESS',                     1)
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',    'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',               CONLIS, 16)!<+++++++++

      CALL PSETC ('CONTOUR_SHADE_COLOUR_METHOD',           'LIST')
      CALL PSET1C('CONTOUR_SHADE_COLOUR_LIST',           CLIST,16)!<=========
      CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',    'POLYGON_SHADING')  
      CALL PSETC ('CONTOUR_SHADE_METHOD',             'AREA_FILL')  

      CALL PSETC ('MAP_GRID',                                'ON')
c     CALL PSETC ('MAP_BOUNDARIES',                         'OFF')
      CALL PSETC ('MAP_BOUNDARIES',                          'ON')
      CALL PSETI ('MAP_BOUNDARIES_THICKNESS',                   3)
cccccc      CALL PSETC ('MAP_BOUNDARIES_PATH',  'med_boundaries.mapgen')
ccc   CALL PSETC ('map_boundaries_colour',                'white')
      CALL PSETC ('map_boundaries_colour',                 'GOLD')
      CALL PSETC ('MAP_GRID_COLOUR',                      'KHAKI')
      CALL PSETC ('MAP_GRID_LINE_STYLE',                   'DASH')
ccc   CALL PSETC ('MAP_GRID_LINE_STYLE',                 'DOT')
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',             0.25)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',            0.25)
ccc   CALL PSETC ('MAP_LABEL',                               'ON')
      CALL PSETC ('MAP_LABEL',                              'OFF')
      CALL PSETC ('MAP_LABEL_COLOUR',                     'BLACK')
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY',              1)
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',               1)
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',                'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR',                  'GREY')
      CALL PSETI ('MAP_COASTLINE_THICKNESS',                    1)
ccc   CALL PSETI ('MAP_COASTLINE_THICKNESS',                    3)
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR',
     x                                      'RGB(0.80,0.80,0.80)')

      CALL PSETC ('TEXT_JUSTIFICATION',                  'CENTRE')
      CALL PSETR ('TEXT_FONT_SIZE',                          1.25)

      CALL PCONT



      CALL LAGUNA

C  ...............................................................

C          PLOT ARROWS
      CALL PSET2R ('INPUT_WIND_SPEED',          XMAG3,ID1,ID2)
      CALL PSET2R ('INPUT_WIND_DIRECTION',       XDIR,ID1,ID2)

      CALL PSETR ('WIND_THINNING_FACTOR',                   1.0)         - 1/12
cc    CALL PSETR ('WIND_THINNING_FACTOR',                   8.0)         - 1/10
c     CALL PSETR ('WIND_THINNING_FACTOR',                  15.0)         - 1/20
      IF(ITYPE.EQ.1) CALL PSETR ('WIND_ARROW_UNIT_VELOCITY', 7.)         - WAVE
      IF(ITYPE.EQ.2) CALL PSETR ('WIND_ARROW_UNIT_VELOCITY',40.)         - WIND
      CALL PSETC ('WIND_ARROW_LEGEND',                    'OFF')
      CALL PSETC ('WIND_ARROW_COLOUR',                  'BLACK')
      CALL PSETI ('WIND_ARROW_HEAD_SHAPE',                    1)
      CALL PSETR ('WIND_ARROW_HEAD_RATIO',                  0.3)
      CALL PSETI ('WIND_ARROW_THICKNESS',                     2)
ccc   CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY',             'ON')
      CALL PWIND
C  ...............................................................
C   NEW   NEW   NEW   NEW   NEW   NEW   NEW   NEW   NEW   NEW   NEW   NEW

C          PLOT COASTLINE


      CALL PSETC ('MAP_COASTLINE_STYLE',                 'SOLID')
      CALL PSETC ('MAP_COASTLINE_COLOUR',                 'GREY')
      CALL PSETC ('MAP_COASTLINE_RESOLUTION',             'HIGH')
      CALL PSETI ('MAP_COASTLINE_THICKNESS',                   1)
      CALL PCOAST

C  ...............................................................

C          PLOT TITLE

C          THE DATE IS TRANSFORMED FOR PLOTTING PURPOSE

      IIYY = JDATE/10**6
      IF(IIYY.LT.70) IY=2000+IIYY
      IF(IIYY.GE.70) IY=1900+IIYY
      IR=MOD(JDATE,10**6)
      IM=IR/10**4
      IR=MOD(IR,10**4)
      ID=IR/10**2
      IH=MOD(IR,10**2)

c     IF(ITYPE.EQ.1) WRITE(TITLE1,100) IY,IM,ID,IH
c100     FORMAT('ADRIATIC SEA 1/12  - WAM WAVE HEIGHT AT   ',
c    *           I4,'.',I2.2,'.',I2.2,' ',I2.2,' UT')

      IF(ITYPE.EQ.1) then
         IF(kont.lt.9) WRITE(TITLE1,100) ID,MESE(IM),IY,IH
 100     FORMAT('Analisi del giorno :  ',
     .           I2.2,A10,I5,'   ore',1X,I2.2,' UTC')
         IF(kont.ge.9) WRITE(TITLE1,102) ID,MESE(IM),IY,IH
 102     FORMAT('Previsione per il  :  ',
     .           I2.2,A10,I5,'   ore',1X,I2.2,' UTC')
      end if

      TITLE2='Altezza d''onda significativa (m) + direzione media'
      TITLE3='<br/>                                              '


      IF(ITYPE.EQ.2) WRITE(TITLE1,101) IY,IM,ID,IH
 101     FORMAT('ADRIATIC SEA 1/12 - 10M WIND  AT  ',
     *           I4,'.',I2.2,'.',I2.2,' ',I2.2,' UT')

      CALL PSETC ('TEXT_HTML', 'ON')
      CALL PSETC ('TEXT_MODE',                    'POSITIONAL')
ccc   CALL PSETR ('TEXT_BOX_X_POSITION',                   5.5)
      CALL PSETR ('TEXT_BOX_X_POSITION',                   5.9)
      CALL PSETR ('TEXT_BOX_Y_POSITION',                  18.5)
      CALL PSETC ('TEXT_QUALITY',                       'HIGH')
ccc   CALL PSETC ('TEXT_QUALITY',                        'LOW')
      CALL PSETI ('TEXT_LINE_COUNT',                         2)
      CALL PSETC ('TEXT_LINE_1',                        TITLE1)
      CALL PSETC ('TEXT_LINE_2',                        TITLE2)
ccc   CALL PSETI ('TEXT_LINE_COUNT',                         4)
ccc   CALL PSETC ('TEXT_LINE_1',                        TITLE1)
ccc   CALL PSETC ('TEXT_LINE_2',                        TITLE3)
ccc   CALL PSETC ('TEXT_LINE_3',                        TITLE3)
ccc   CALL PSETC ('TEXT_LINE_4',                        TITLE2)
      CALL PSETR ('TEXT_FONT_SIZE',                       0.75)
      CALL PSETC ('TEXT_COLOUR',                       'BLACK')
      CALL PTEXT

C          PLOT DIFFERENT COASTLINE
      CALL COSTA

C          PLOT VENEZIA AND TRIESTE LOCATION
      CALL CITTA

C  ...............................................................

      CALL PNEW ('PAGE')

      RETURN
      END

C()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

      SUBROUTINE CITTA

C    THIS ROUTINE IS TO WRITE OUT THE LOCATION AND NAME
C    'VENEZIA'  AND 'TRIESTE'

      REAL XV1(2), YV1(2)
      REAL DUMM(2)
      INTEGER ISIM(2)
          CHARACTER*16 VENE,TRIE

      DATA XV1/ 12.32, 13.78/, YV1/ 45.46,45.65/
      DATA DUMM/3.,3./,  ISIM/15,15/
      DATA VENE/'VENEZIA'/, TRIE/'TRIESTE'/

C ......................................................................

C     plot location of towns
      call PSETC  ('SYMBOL_COLOUR',           'BLACK')
      call PSETR  ('SYMBOL_HEIGHT',               0.8)
      call PSETC  ('SYMBOL_TABLE_MODE',         'OFF')
      call PSETC  ('SYMBOL_TYPE',              'MARKER')
 
      CALL PSET1R ('SYMBOL_INPUT_X_POSITION',   XV1,2)
      CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',   YV1,2)
      CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST', DUMM,2)

      CALL PSETI  ('SYMBOL_MARKER',                15)
      CALL PSETC  ('legend',     'off')

      CALL PSYMB

c      plot names
C ..........................................
      CALL PSETC ('TEXT_MODE',           'POSITIONAL')
      CALL PSETC ('TEXT_JUSTIFICATION',        'LEFT')
      CALL PSETR ('TEXT_FONT_SIZE',              0.55)
      CALL PSETC ('TEXT_COLOUR',              'BLACK')
      CALL PSETC ('TEXT_QUALITY',              'HIGH')
      CALL PSETI ('TEXT_LINE_COUNT',                1)
      xv =  3.7
      yv = 13.0
      CALL PSETR ('TEXT_BOX_X_POSITION',           XV)
      CALL PSETR ('TEXT_BOX_Y_POSITION',           YV)
      CALL PSETC ('TEXT_LINE_1',                 VENE)
      CALL PTEXT
C
      xv = 21.0
      yv = 15.8
      CALL PSETR ('TEXT_BOX_X_POSITION',           XV)
      CALL PSETR ('TEXT_BOX_Y_POSITION',           YV)
      CALL PSETC ('TEXT_LINE_1',                 TRIE)
      CALL PTEXT
C ..........................................

c     xv = xv1(1)-0.3
c     yv = yv1(1)+0.3
c     du = dumm(1)
c     CALL PSET1R ('SYMBOL_INPUT_X_POSITION',    XV,1)
c     CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',    YV,1)
c     CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST',   DU,1)
c     CALL PSET1C ('symbol_texts',             VENE,1)
c     call PSETC  ('SYMBOL_texts_COLOUR',     'WHITE')
c     call PSETR  ('SYMBOL_texts_HEIGHT',         2.0)

c     CALL PSYMB

      RETURN
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

C==============================================================================

      SUBROUTINE FLDS (IUNIT,IDATE,ID1,ID2,WAVEHT,WAVEDIR,WINDSPD,
     *                 WINDDIR,NGX,NGY,IEOF,IFORM)
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
C
C                 *IUNIT*  - LOGICAL UNIT FOR INPUT FROM WAVE MODEL:
C                               WAVE HEIGHT
C                               MEAN WAVE DIRECTION
C                               MEAN FREQUENCY
C                               FRICTION VELOCITY
C                               WIND DIRECTION
C                               PEAK FREQUENCY
C
C     METHOD.
C     -------
C
C                               NONE
C
C     EXTERNALS.
C     ----------
C
C                 *USTRU10*  CONVERT FRICTION VELOCITY TO WIND AT 10M
C
C
C-----------------------------------------------------------------------
C
      LOGICAL IEOF

      LOGICAL FFLAG(8)
      DIMENSION WAVEHT(ID1,ID2), WAVEDIR(ID1,ID2)
      DIMENSION WINDSPD(ID1,ID2), WINDDIR(ID1,ID2)
      DIMENSION WINDSTR(ID1,ID2)
      DIMENSION CDG(ID1,ID2), TAUWG(ID1,ID2)
      REAL WAVEMF(ID1,ID2),WAVEPKF(ID1,ID2)

      CHARACTER*10 CDATE
C
C-----------------------------------------------------------------------
C
C*    *COMMON* *BOUNDS* SCALING OF PLOTS.
C
       COMMON/BOUNDS/XLATS,XLATN,XLONW,XLONE


       COMMON/MAX/ WAMAX,WIMAX,STMAX

       COMMON /DATEA/ IYA,IMA,IDA,IHA
       COMMON /DATFF/ IHFF
       COMMON /DATEF/ IYF,IMF,IDF,IHF

C
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
C*     1.     - READ WAVE AND WIND FIELDS -
C             -----------------------------
C
      IEOF = .FALSE.
      DO 1100 I=1,ID1
      DO 1100 J=1,ID2
         WAVEHT(I,J)  = 0.
         WAVEDIR(I,J) = 0.
         WINDSPD(I,J) = 0.
         WINDDIR(I,J) = 0.
 1100 CONTINUE
C
C ++++++++++++++  READ UNFORMATTED FROM  'MAP'  FILES  +++++++++++++++++++

      IF(IFORM .eq. 0) THEN

         READ(IUNIT,END=20) CDATE,DNGX,DNGY,XLONW,XLATS,XLONE,XLATN
C        MINUTES IN THE DATE ARE DISREGARDED
ccc      IDATE=NINT(XDATE/100.)
         READ(CDATE,'(I8.8,I2)') IDATE,IMINU

      IIYY = IDATE/10**6
      IF(IIYY.LT.70) IYA=2000+IIYY
      IF(IIYY.GE.70) IYA=1900+IIYY
      IR=MOD(IDATE,10**6)
      IMA=IR/10**4
      IR=MOD(IR,10**4)
      IDA=IR/10**2
      IHA=MOD(IR,10**2)

      IHFF = 03
      IYF  = IYA
      IMF  = IMA
      IDF  = IDA
      IHF  = IHA

         NGX=NINT(DNGX)
         NGY=NINT(DNGY)
         READ(IUNIT, END=20) (FFLAG(J),J=1,8)
      IF (FFLAG(1))
     1   READ(IUNIT,END=20) ((WAVEHT (I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(2))
     1   READ(IUNIT,END=20) ((WAVEDIR(I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(3))
     1   READ(IUNIT,END=20) ((WAVEMF (I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(4))
     1   READ(IUNIT,END=20) ((WINDSTR(I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(5))
     1   READ(IUNIT,END=20) ((WINDDIR(I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(6))
     1   READ(IUNIT,END=20) ((WAVEPKF(I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(7))
     1   READ(IUNIT, END=20) ((CDG(I,J),I=1,NGX),J=NGY,1,-1)
      IF (FFLAG(8))
     1   READ(IUNIT, END=20) ((TAUWG(I,J),I=1,NGX),J=NGY,1,-1)


C           WIND AND WAVE DIRECTION IS ROTATED FOR PLOTTING PURPOSE
            DO 1200 I=1,NGX
            DO 1200 J=1,NGY

            WINDDIR(I,J) = MOD(180.+WINDDIR(I,J),360.)
            WAVEDIR(I,J) = MOD(180.+WAVEDIR(I,J),360.)

 1200       CONTINUE

      END IF
C
C ++++++++++++++  READ FORMATTED FROM  'OPE'  FILES  +++++++++++++++++++

      IF(IFORM .eq. 1) THEN
C
        READ(IUNIT,1,END=20) CDATE,NGX,NGY,XLONW,XLATS,XLONE,XLATN
C       MINUTES IN THE DATE ARE DISREGARDED
C       IDATE=NINT(IDATE/100.)
        READ(CDATE,'(I8.8,I2)') IDATE,IMINU

      IIYY = IDATE/10**6
      IF(IIYY.LT.70) IYA=2000+IIYY
      IF(IIYY.GE.70) IYA=1900+IIYY
      IR=MOD(IDATE,10**6)
      IMA=IR/10**4
      IR=MOD(IR,10**4)
      IDA=IR/10**2
      IHA=MOD(IR,10**2)

      IHFF = 03
      IYF  = IYA
      IMF  = IMA
      IDF  = IDA
      IHF  = IHA


        READ(IUNIT,2,END=20) ((WAVEHT (I,J),I=1,NGX),J=NGY,1,-1)

        READ(IUNIT,3,END=20) ((WAVEDIR(I,J),I=1,NGX),J=NGY,1,-1)

        READ(IUNIT,4,END=20) ((WAVEMF (I,J),I=1,NGX),J=NGY,1,-1)

        READ(IUNIT,4,END=20) ((WAVEPKF(I,J),I=1,NGX),J=NGY,1,-1)

 1       FORMAT(A10,2I10,4F6.1)
 2       FORMAT(16F5.2)
 3       FORMAT(16F5.0)
 4       FORMAT(16F5.3)

C           WAVE DIRECTION IS ROTATED FOR PLOTTING PURPOSE
            DO 1300 I=1,NGX
            DO 1300 J=1,NGY

            WAVEDIR(I,J) = MOD(180.+WAVEDIR(I,J),360.)

 1300       CONTINUE

      END IF


      DO 2100 I=1,ID1
      DO 2200 J=1,ID2
         IF(WAVEHT(I,J).eq.0.0) WAVEHT(I,J) = -0.1
 2200 CONTINUE
 2100 CONTINUE
c <><><><><><>
         wamax=maxval(WAVEHT)
ccc      wimax=maxval(WINDSPD)
ccc      stmax=maxval(WINDSTR)
c <><><><><><><><>

C.......................................................................

      GO TO 10
 20   IEOF = .TRUE.

   10 CONTINUE
      RETURN
      END

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


      SUBROUTINE INCDATE(DATE,ISHIFT)

C ----------------------------------------------------------------------
C
C**** *INCDATE* - TO UPDATE DATE TIME GROUP
C
C     L. BERTOTTI, P.JANSSEN.
C
C     H. GUNTHER   ECMWF  NOVEMBER 1989    NEGATIVE INCREMENTS.
C
C*    PURPOSE.
C     --------
C
C       UPDATING DATE TIME GROUP.
C
C**   INTERFACE.
C     ----------
C
C       *CALL* *INCDATE (IDATE,ISHIFT)*
C         *DATE*   CHARACTER - DATE TIME GROUP (YYMMDDHHMM)
C         *ISHIFT* INTEGER   - TIME INCREMENT IN SECONDS, WHERE
C                              ABS (ISHIFT) HAS TO BE LESS THEN 1 YEAR
C
C     METHOD.
C     -------
C
C       NONE.
C
C     EXTERNALS.
C     ----------
C
C       NONE.
C
C     REFERENCES.
C     -----------
C
C       NONE.
C
C ----------------------------------------------------------------------
C
      CHARACTER*10 DATE
      DIMENSION MON(12)
C
      DATA MON /31,28,31,30,31,30,31,31,30,31,30,31/
C
C ----------------------------------------------------------------------
C
C*    1.0 SPLITE DATE TIME GROUP INTO MINUTE, HOUR, DAY, MONTH, YEAR.
C         -----------------------------------------------------------
C
      READ(DATE,5) IYEAR,MONTH,IDAY,IHOUR,MINUT

      IF (MOD(IYEAR,4).EQ.0) THEN
         MON(2)=29
      ELSE
         MON(2)=28
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    2.0 ADD SECONDS AND UPDATE DATE AND TIME.
C         -------------------------------------
C
 2000 CONTINUE
      MINUT=MINUT+ISHIFT/60
C
C     2.1 POSITIVE SHIFT GREATER THAN 1 MINUTE.
C
      IF (MINUT.GE.60) THEN
         IHOUR = IHOUR + MINUT/60
         MINUT = MINUT - (MINUT/60)*60
         IF (IHOUR.GE.24) THEN
            IDAY = IDAY + IHOUR/24
            IHOUR = IHOUR - (IHOUR/24)*24
            IF (IDAY.GT.MON(MONTH)) THEN
 1300          CONTINUE
               IDAY=IDAY-MON(MONTH)
               MONTH=MONTH+1
               IF(MONTH.EQ.13) THEN
                  MONTH = 1
                  IYEAR=MOD(IYEAR+1,100)
                  IF (MOD(IYEAR,4).EQ.0) THEN
                     MON(2)=29
                  ELSE
                     MON(2)=28
                  ENDIF
               END IF
               IF(IDAY.GT.MON(MONTH)) GO TO 1300
            END IF
         END IF
      ELSE IF (MINUT.LT.0) THEN
C
C     2.2 NEGATIVE SHIFT.
C
         IHOUR = IHOUR + (MINUT-59)/60
         MINUT = MINUT - ((MINUT-59)/60)*60
         IF (IHOUR.LT.0) THEN
            IDAY = IDAY + (IHOUR-23)/24
            IHOUR = IHOUR - ((IHOUR-23)/24)*24
            IF (IDAY.LT.1) THEN
 1400          CONTINUE
               MONTH=MONTH-1
               IF(MONTH.EQ.0) THEN
                  MONTH = 12
                  IYEAR=MOD(IYEAR+99,100)
                  IF (MOD(IYEAR,4).EQ.0) THEN
                     MON(2)=29
                  ELSE
                     MON(2)=28
                  ENDIF
               END IF
               IDAY=IDAY+MON(MONTH)
               IF(IDAY.LT.1) GO TO 1400
            END IF
         END IF
      END IF
C
C ----------------------------------------------------------------------
C
C*    3.0 COMPOSE NEW DATE TIME GROUP.
C         ----------------------------
C
 3000 CONTINUE
      WRITE(DATE,5) IYEAR,MONTH,IDAY,IHOUR,MINUT
C
    5 FORMAT(5I2.2)
C
      RETURN
      END

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE GSFILE (IU06, IUNIT, DATE, DATEF, FILEID, OPT)

C ----------------------------------------------------------------------
C
C**** *GSFILE* - GETS/SAVES FILES FROM/TO MASS STORAGE.
C
C     H. GUNTHER    GKSS/ECMWF    OCTOBER 1989
C     P. JANSSEN    KNMI          OCTOBER 1990   YMP-MODIFICATION
C     H. GUNTHER    GKSS/ECMWF    OCTOBER 1990   NEW FILE NAMES.
C     H. GUNTHER    GKSS/ECMWF    MARCH   1991   CYCLE_4 MODIFICATIONS.
C     L. BERTOTTI   ISDGM         FEBRUARY1994   YMP-EL VENICE MODIFICATIONS.
C                                 DECEMBER1995   COPY INPUT FILE ( NO ASSIGN)
C*    PURPOSE.
C     --------
C
C       GETS OR SAVES FILES FROM / TO MASS STORAGE.
C
C**   INTERFACE.
C     ----------
C
C       *CALL* *GSFILE (IU06, IUNIT, IDATE, IDATEF, FILEID, OPT)*
C
C         *IU06*   INTEGER    UNIT FOR PRINTER MESSAGES.
C         *IUNIT*  INTEGER    FORTRAN UNIT.
C         *DATE*   CHARACTER  DATE (YYMMDDHHMM) OF FILE TO BE SAVED
C                             OR < 0  RESTART FILE INDEX.
C         *DATEF*  CHARACTER  DATE TIME GROUP OF FORECAST START.
C         *FILEID* CHARACTER  FILE ID.
C         *OPT*    CHARACTER  OPTION
C                             = 'S' SAVE FILE CONNECTED TO IUNIT
C                             = 'G' GET FILE AND CONNECT FILE TO IUNIT
C
C     EXTERNALS.
C     ----------
C
C       *ABORT*     - TERMINATES PROCESSING.
C       *DIFDATE*   - COMPUTE TIME DIFFERENCES.
C       *IECF_LEN*  -  LENGTH OF A CHARATER VARIABLE.
C       *SYSTEM*    -  FORTRAN INTERFACE TO WS SYSTEM.
C
C     METHOD.
C     -------
C
C        THE METHOD USED IN THIS SUB DEPENDS UPON THE COMPUTER
C        ENVIROMENT. IN ITS PRESENT FORM THE ROUTINE IS ADOPTED TO
C        THE VENICE CRAY SYSTEM.
C
C        IF IDATE CONTAINS A DATE TIME GROUP (IDATE.GE.0) THE
C        FILE NAME IS BUILT FORM THE 3 CHARACTER FILEID FOLLOWED
C        BY YYMMDDHHFFFF WHERE YYMMDDHH IS THE LAST ANALYSIS TIME AND
C        FFFF IS THE FORCECAST PERIOD IN HOURS.
C        THESE NUMBERS ARE CONSTRUCTED FROM IDATE AND IDATEF.
C
C        A NEGATIVE IDATE INDICATES THAT THE FILE NAME HAS TO BE
C        TAKE OUT OF ARRAY NAME(-IDATE)
C
C        IN PARTICULAR THESE SUB DOES THE FOLLOWING STEPS:
C               - CLOSES THE UNIT.
C               - CONSTRUCTS THE FILE NAME.
C               - CONSTRUCTS THE UNIT NAME.
C               - OPENS THE FILE.
C
C     REFERENCES.
C      -----------
C
C       NONE.
C
C
C
C ----------------------------------------------------------------------
C
C*    *COMMON* *TESTO* - PRINTER OUTPUT UNIT AND TEST FLAGS.
C
      COMMON /TESTO/ IUOUT, ITEST, ITESTB
C
C*     VARIABLE.   TYPE.     PURPOSE.
C      ---------   -------   --------
C      *IUOUT*     INTEGER   UNIT FOR PRINTER OUTPUT.
C      *ITEST*     INTEGER   TEST OUTPUT LEVEL:
C                             .LE. 0  NO OUTPUT
C                             .GE. I  OUTPUT TO SUB. LEVEL I
C      *ITESTB*    INTEGER   MAX BLOCK NUMBER FOR OUTPUT IN BLOCK LOOPS
C
C ----------------------------------------------------------------------
C
C*    *COMMON* *TEXT* - FILE NAME INFORMATION.
C
      CHARACTER USERID*3, RUNID*3, PATH*60
      COMMON /TEXT/ USERID, RUNID, PATH
C
C*     VARIABLE.   TYPE.     PURPOSE.
C      ---------   -------   --------
C      *USERID*    CHARACTER USERID FOR FILE NAMES. NOT USED
C      *RUNID*     CHARACTER RUN IDENTIFIER FOR FILE NAMES. NOT USED
C      *PATH*      CHARACTER PATH NAME FOR FILES.
C
C ----------------------------------------------------------------------
      INTEGER IU06, IUNIT
      INTEGER(8) IDATE, IDATEF, IDATEH
      CHARACTER*10 DATE, DATEF
      CHARACTER FILEID*3, OPT*1

      INTEGER QQNAME
      PARAMETER (QQNAME =  7)
      CHARACTER NAME(QQNAME)*16
      CHARACTER FILENA*16, IU*8, PLIST*70
      CHARACTER ATTR*70
C
C ----------------------------------------------------------------------
C
cccc     write(6,'(''----> SUB. GSFILE'')')

C*    1. DEFINE RESTART FILENAMES AND OPTIONS.
C        -------------------------------------
C
C*    1.1 FILE NAMES FOR RESTART FIELDS FROM AN ANALYSIS RUN.
C         ---------------------------------------------------
C
      DATA NAME( 1) / 'BLSPANAL' / ,
     1     NAME( 2) / 'SLATANAL' / ,
     2     NAME( 3) / 'LAWIANAL' /
C
C*    1.2 FILE NAMES FOR RESTART FIELDS FROM A FORECAST RUN.
C         --------------------------------------------------
C
      DATA NAME( 4) / 'BLSPFORC' / ,
     1     NAME( 5) / 'SLATFORC' / ,
     2     NAME( 6) / 'LAWIFORC' /
C
C*    1.3 MANAGEMENT FILE.
C         ----------------
C
      DATA NAME( 7) / 'WAMINFO' /
C
C ----------------------------------------------------------------------
C
C         TRASFORM DATE TO INTEGER
C         ------------------------
      READ(DATE(1:10),'(I10)') IDATE
      READ(DATEF(1:10),'(I10)') IDATEF
C
C ----------------------------------------------------------------------
C
C*    2. CLOSE UNIT.
C        -----------
C
 2000 CONTINUE
      CLOSE (UNIT=IUNIT, STATUS='KEEP')
C
C ----------------------------------------------------------------------
C
C*    3.0  CONSTRUCT FILE NAME.
C          --------------------
C
 3000 CONTINUE
      IF (IDATE.GE.0) THEN
         FILENA = FILEID
         IF (IDATE.LE.IDATEF) THEN
            IDATEH = IDATE/100
            ISHIFT = 0
         ELSE
            IDATEH = IDATEF/100
            CALL DIFDATE (DATEF, DATE, ISHIFT)
            ISHIFT = ISHIFT/3600
         ENDIF
         WRITE (FILENA(4:15),'(I8.8,I4.4)') IDATEH,ISHIFT
      ELSE
         IF (IDATE.LT.-QQNAME) THEN
            WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++'
            WRITE(IU06,*) ' +                                         +'
            WRITE(IU06,*) ' +      WARNING ERROR IN --GSFILE--        +'
            WRITE(IU06,*) ' +      ===========================        +'
            WRITE(IU06,*) ' + FILE NAME INDEX REQUESTED IS ',-IDATE
            WRITE(IU06,*) ' + MAXIMUM INDEX ALLOWED IS QQNAME =',QQNAME
            WRITE(IU06,*) ' + NO GET OR SAVE PROCESSED                +'
            WRITE(IU06,*) ' + EXECUTION IS CONTINUED                  +'
            WRITE(IU06,*) ' +                                         +'
            WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++'
            RETURN
         ENDIF
         FILENA = NAME(-IDATE)
      ENDIF
      LNAME = IECF_LEN(FILENA)
C
C ----------------------------------------------------------------------
C*    4.0 CONSTRUCT UNIT NAME
C         -------------------
C
 4000 CONTINUE
      IF ((IUNIT.GT.0) .AND. (IUNIT.LT.10)) THEN
         WRITE(IU,'(A,I1)') 'fort.',IUNIT
      ELSE IF ((IUNIT.GT.9) .AND. (IUNIT.LT.100)) THEN
         WRITE(IU,'(A,I2)') 'fort.',IUNIT
      ELSE
         WRITE(IU,'(A8)') IUNIT
      ENDIF
      LIU   = IECF_LEN(IU)
C
C ----------------------------------------------------------------------
C*    5.0 CONSTRUCT FILE NAME INCLUDING PATH.
C         -----------------------------------
C
 5000 CONTINUE
      LIP   = IECF_LEN(PATH)
cray
cray change PATH to be a relative one instead of absolute
cray
      PLIST = PATH(1:LIP)//'/'//FILENA(1:LNAME)
      LLIST = IECF_LEN(PLIST)
C
C ----------------------------------------------------------------------
C
C*    6.0 EXECUTE COPY OR ASSIGN COMMAND.
C         -------------------------------
C
 6000 CONTINUE
C
C*    6.1 SAVE FILE.
C         ----------
C
      IF (OPT.EQ.'S') THEN
C
          WRITE(IU06,*) '  '
          WRITE(IU06,*) ' --COPY COMMAND IN GSFILE--'
          WRITE(IU06,*) ' COPY UNIT ',IU
          WRITE(IU06,*) ' ON FILE   ',PLIST

          IFAIL = 0
          CALL SYSTEM ('chmod go+rw '// IU(1:LIU))
          CALL SYSTEM ('cp '// IU(1:LIU) //' ' // PLIST(1:LLIST))
          ENDIF
C
C*    6.2 GET FILE.
C         ---------
C
      IF (OPT.EQ.'G') THEN
C
          WRITE(IU06,*) '  '
          WRITE(IU06,*) ' --COPY COMMAND IN GSFILE--'
          WRITE(IU06,*) ' ASSIGN(COPY) FILE ',PLIST
          WRITE(IU06,*) ' TO UNIT     ',IU

          IFAIL = 0
          CALL SYSTEM ('cp ' // PLIST(1:LLIST) //' ' // IU(1:LIU))
          ENDIF
C
C ----------------------------------------------------------------------
C
C*    7.0 CHECK OPEN RETURN CODE.
C         -----------------------
C
 7000 CONTINUE
      IF(IFAIL.NE.0) THEN
         WRITE(IU06,*) ' *******************************************'
         WRITE(IU06,*) ' *                                         *'
         WRITE(IU06,*) ' *        FATAL ERROR IN --GSFILE--        *'
         WRITE(IU06,*) ' *        =========================        *'
         WRITE(IU06,*) ' * OPEN ERROR                              *'
         WRITE(IU06,*) ' * IFAIL = ',IFAIL
         WRITE(IU06,*) ' * PROCESSING WILL BE ABORTED              *'
         WRITE(IU06,*) ' *                                         *'
         WRITE(IU06,*) ' *******************************************'
         CALL ABORT
      ENDIF

      RETURN
      END

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE USTRU10 (USTR, U10, CD, ID1, ID2, NGX, NGY, CDFLAG)

C ---------------------------------------------------------------------
C
C***  *USTRU10* - CONVERTS FRICTION VELOCITIES TO U10 WU'S FORMULA.
C
C     LIANA ZAMBRESKY   GKSS/ECMWF        JULY 1987
C     H. GUNTHER        ECMWF/GKSS        JANUARY 1990
C                                         MODIFIED FOR YMP.
C     H. GUNTHER        ECMWF/GKSS        NOVEMBER 1990
C                                         MODIFIED FOR CYCLE_4.0
C
C*    PURPOSE
C     -------
C
C       THIS SUB. CONVERTS FRICTION VELOCITY  TO THE WIND AT 10M.
C
C
C**   INTERFACE
C     ---------
C
C       *CALL* *USTRU10 (USTR, U10, CD, ID1, ID2, NGX, NGY, CDFLAG)*
C          USTR    ARRAY OF FRICTION VELOCITIES
C          U10     FRICTION VELOCITIES CONVERTED TO WIND AT 10M
C          CD      DRAG COEFFICIENT.
C          NGX     FIRST ARRAY DIMENSION
C          NGY     SECOND ARRAY DIMENSION
C          ID1     FIRST ARRAY DIMENSION USED
C          ID2     SECOND ARRAY DIMENSION USED
C          CDFLAG  = .TRUE. IF DRAG OUT OF ARRAY CD IS USED.
C                    .FALSE. USE LARGE AND POND FORMULA.
C
C     ENTRY.
C     ------
C
C       *CALL* *LODTAB*
C
C     METHOD
C     ------
C
C       IF CDFLAG IS .TRUE. THE DRAG MUST BE GIVEN IN ARRAY CD.
C       OTHERWISE NEWTON'S METHOD IS USED TO CONVERT USTR TO U10
C       WITH THE HELP OF A TABLE, WHICH HAS TO BE GENERATED
C       WITH THE ENTRY LODTAB.
C
C     EXTERNALS
C     ---------
C
C       NONE
C
C     REFERENCES
C     ----------
C
C       NONE
C
C ----------------------------------------------------------------------

      LOGICAL CDFLAG
      DIMENSION USTR(ID1,ID2), U10(ID1,ID2), CD(ID1,ID2)
      DIMENSION USTRTAB(50), U10TAB(50)
      SAVE USTRTAB, U10TAB

      DATA TLRNCE/.05/

      FU(U10H,USTAR)=U10H*U10H*(.065*U10H+.8)-1000.*USTAR*USTAR
      FPU(U10H)=U10H*(.195*U10H+1.6)
C
C ----------------------------------------------------------------------
C
C*    1. USE DRAG GIVEN AT ALL POINTS.
C        -----------------------------
C
 1000 CONTINUE
      IF (CDFLAG) THEN
         DO 1001 I2 = 1,NGY
         DO 1001 I1 = 1,NGX
            IF (CD(I1,I2).GT.0.) THEN
               U10(I1,I2) = USTR(I1,I2)/SQRT(CD(I1,I2))
            ELSE
               U10(I1,I2) = 0.
            ENDIF
 1001    CONTINUE
         RETURN
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    2. USE LARGE AND POND.
C        -------------------
C
 2000 CONTINUE
      DO 2001 I2 = 1,NGY
      DO 2001 I1 = 1,NGX
         IF(USTR(I1,I2).LE..26911) THEN
C
C*    2.1 WINDSPEED INDEPENDENT CD FOR LOW SPEEDS
C         27.86932057=1./SQRT(.0012875)
C         ---------------------------------------
C
            U10(I1,I2) = 27.86932057*USTR(I1,I2)
         ELSE
C
C*    2.2 WINDSPEED DEPENDENT CD USE NEWTONS METHOD
C         ---------------------------------------
C
C*    2.2.1 FIND FIRST VALUE FROM TABLE GE USTAR AS START
C*          VALUE FOR NEWTONS METHOD
C           ---------------------------------------------
C
            IT=0
            DO 2210 I=1,50
               IF (USTR(I1,I2).GE.USTRTAB(I)) THEN
                 U1 = U10TAB(I)
                 GO TO 2220
               ENDIF
 2210       CONTINUE
            WRITE(6,*) ' '
            WRITE(6,*) ' FRICTION VELOCITY IS OUT OF RANGE OF TABLE --'
            WRITE(6,*) ' USTR = ',USTR(I1,I2)
            WRITE(6,*) ' U10 SET TO ZERO. '
            U10(I1,I2) = 0.
            GO TO 2001
C
C*    2.2.2  SOLVE FOR U10 USING NEWTON'S METHOD
C            -----------------------------------
C
 2220       CONTINUE
            U2 = FU(U1,USTR(I1,I2))/FPU(U1)
            IF(ABS(U2).GT.TLRNCE) THEN
               IT=IT+1
               IF(IT.GT.25) THEN
                  WRITE(6,*) ' '
                  WRITE(6,*) ' NO CONVERGENCE AFTER 25 ITERATIONS -- '
                  WRITE(6,*) ' USTR(I1,I2)  = ',USTR(I1,I2)
                  WRITE(6,*) ' I1 = ', I1, '  I2 = ', I2
                  WRITE(6,*) ' LAST APPROXIMATION IS USED'
               ELSE
                  U1 = U1-U2
                  GOTO 2220
               ENDIF
            ENDIF
            U10(I1,I2) = U1-U2
         ENDIF
 2001 CONTINUE

      RETURN
C
C ----------------------------------------------------------------
C
C*    3. COMPUTE A TABLE OF USTAR AND U10
C        --------------------------------
C
 3000 CONTINUE
      ENTRY LODTAB

      U = 0.
      DO 3001 I=1,50
         U = U+2.
         U10TAB(I) = U
         USTRTAB(I) = U*SQRT(.001*(.8+.065*U))
 3001 CONTINUE

      RETURN
      END


c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DIFDATE (IDATE1, IDATE2, ISHIFT)
C ----------------------------------------------------------------------
C
C**** *DIFDATE* - TO COMPUTE TIME DIFFERENCE.
C
C     H. GUNTHER   GKSS/ECMWF  NOVEMBER 1989
C
C*    PURPOSE.
C     --------
C
C       COMPUTE THE SECONDS BETWEEN THE INPUT DATES.
C       DATES HAVE TO BE IN CONSECUTIVE YEARS.
C
C**   INTERFACE.
C     ----------
C
C       *CALL* *DIFDATE (IDATE1, IDATE2, ISHIFT)*
C
C         *IDATE1* - DATE TIME GROUP (YYMMDDHHMM).
C         *IDATE2* - DATE TIME GROUP (YYMMDDHHMM).
C         *ISHIFT* - DIFFERENCE IN SECONDS (IDATE2-IDATE1).
C
C     METHOD.
C     -------
C
C       NONE.
C
C     EXTERNALS.
C     ----------
C
C       NONE.
C
C      REFERENCES.
C      -----------
C
C       NONE.
C
C ----------------------------------------------------------------------
C
      DIMENSION MON(12)
C
      CHARACTER*10 IDATE1, IDATE2, IDT1, IDT2, CDAT
C
      DATA MON /31,28,31,30,31,30,31,31,30,31,30,31/
C
C ----------------------------------------------------------------------
C
C*    1.0 CHANGE DATE TIME GROUPS TO ENSURE THAT THE SECOND IS LARGER.
C         ------------------------------------------------------------
C
C
C
      IDT1=IDATE1
      IDT2=IDATE2
      ISI = 60
C
C         END OF CENTURY CORRECTION.
C
      READ(IDT1(1:4),'(I4)') IYM1
      READ(IDT2(1:4),'(I4)') IYM2
      IF (IYM1.LT. 100 .AND. IYM2.GT.9900)
     1    IYM1 = 10000 + IYM1
      IF (IYM2.LT. 100 .AND. IYM1.GT.9900)
     1    IYM2 = 10000 + IYM2

      IF (IDT1.GT.IDT2.OR.IYM1.GT.IYM2) THEN
         IDT1 = IDATE2
         IDT2 = IDATE1
         ISI  = -60
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    2.0 SPLIT DATE TIME GROUPS INTO MINUTE, HOUR, DAY, MONTH, YEAR.
C ----------------------------------------------------------------------
C
      READ(IDT1,5) IYEAR1,MONTH1,IDAY1,IHOUR1,MINUT1
      IF (IYM1.GT.10000) IYEAR1=IYEAR1+100
C
      READ(IDT2,5) IYEAR2,MONTH2,IDAY2,IHOUR2,MINUT2
      IF (IYM2.GT.10000) IYEAR2=IYEAR2+100
C
C ----------------------------------------------------------------------
C
C*    3.0 CORRECT DAYS IN FEBRUARY OF FIRST YEAR FOR LEAP-YEAR.
C         -----------------------------------------------------
C
C      IF (MOD(IYEAR1,4).EQ.0) MON(2) = 29   ! corrected as below!
      IF (MOD(IYEAR1,4).EQ.0) THEN
         MON(2) = 29
      ELSE 
         MON(2) = 28
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    4.0 COMPUTE TIME DIFFERENCE IN MINUTES.
C         -----------------------------------
C
C     4.1 DIFFERENCE BETWEEN DAY, HOUR ,MINITE.
C
      ISHIFT = ((IDAY2-IDAY1)*24+IHOUR2-IHOUR1)*60+MINUT2-MINUT1
C
C      4.2 ADD DIFFERENCE FROM MONTH.
C
      IF (IYEAR2.GT.IYEAR1) THEN
C
C      4.2.1 START AND END MONTH ARE IN DIFFERENT YEARS.
C
         DO 4211 M=MONTH1,12
            ISHIFT = ISHIFT + INT(MON(M)*24*60)
 4211    CONTINUE
         IF (MONTH2.GT.1) THEN
            IF (MOD(IYEAR2,4).EQ.0) THEN
               MON(2) = 29
            ELSE
               MON(2) = 28
            ENDIF
            DO 4212 M=1,MONTH2-1
               ISHIFT = ISHIFT + INT(MON(M)*24*60)
 4212       CONTINUE
         ENDIF
      ELSE
C
C      4.2.2 START AND END MONTH ARE IN THE SAME YEAR.
C
         IF (MONTH2.GT.MONTH1) THEN
            DO 4221 M=MONTH1,MONTH2-1
               ISHIFT = ISHIFT + INT(MON(M)*24*60)
 4221       CONTINUE
         ENDIF
      ENDIF
C
C ----------------------------------------------------------------------
C
C*    5.0 CHANGE SIGN OF DIFFERENCE IF DATES HAVE BEEN EXCHANGED
C*        AND CONVERT TO SECONDS.
C         ------------------------------------------------------
C
      ISHIFT = ISHIFT*ISI
C
    5 FORMAT(5I2.2)
C
      RETURN
      END


c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        SUBROUTINE READCOS

C       read adriatic coastline points coordinates (equatorial grid)

C------------------------------------------------------------------------

C     NPC:   NUMBER OF POINTS IN EACH COASTLINE-PART
C     NIS:   NUMBER OF PARTS
      PARAMETER (NPC=1500, NIS=873)

C     COASTAL POINTS INFO
c.... XA(),YA()      geographical coordinates of the Adriatic coast -
c.... XJ(,),YJ(,)    geographical coordinates of jugo islands -
c.... XJ(,),YJ(,)    geographical coordinates of differents parts of coast
c
        real xa(NPC),ya(NPC)
        real xj(NPC,NIS),yj(NPC,NIS)
        integer NPIS(NIS)

      COMMON /COASTA/ XA, YA
      COMMON /COASTJ/ XJ, YJ
      COMMON /NPOINT/ NPIS

C------------------------------------------------------------------------

C     READ ADRIATIC CAOSTAL POINTS
ccc   READ(82,*)
c     READ(82,'(I5)') NPA
c     DO 101 I=1,NPA
ccc   the following line is for 'rotated' coastline
ccc   READ(82,*,END=999) II, GLON,GLAT, XA(I),YA(I)
c     READ(82,*,END=999)  XA(I),YA(I)
ccc    XA(I) = GLON
ccc    YA(I) = GLAT
c101  CONTINUE

C     READ JUGO ISLANDS COASTAL POINTS

C     LOOP OVER ISLANDS
c     DO 201 ISL=1,NIS
c     READ(82,'(I5)',END=999) NPP
c     NPIS(ISL)=NPP
c     DO 202 I=1,NPP
ccc   READ(82,*,END=999) II,GLON,GLAT,XJ(I,ISL),YJ(I,ISL)
c     READ(82,*,END=999) XJ(I,ISL),YJ(I,ISL)
ccc   XJ(I,ISL) = GLON
ccc   YJ(I,ISL) = GLAT
c202  CONTINUE
c201  CONTINUE

c - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C     READ ADRIATIC CAOSTAL POINTS

C     LOOP OVER EACH PART
      DO 301 ISL=1,NIS
      READ(82,'(I5)',END=999) NPP
      NPIS(ISL)=NPP
      DO 302 I=1,NPP
      READ(82,*,END=999) XJ(I,ISL),YJ(I,ISL)
 302  CONTINUE
 301  CONTINUE


 999  CONTINUE
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     write(6,'(''LETTA COSTA'')')
c     WRITE(6,'('' PUNTI ADRIATICO '',I10)') NPA
c     do 300 i=1,nis
c     WRITE(6,'('' PUNTI ISOLA N.'',i3,I10)') I,NPIS(I)
c300  CONTINUE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      RETURN
      END

C==============================================================================

      SUBROUTINE COSTA

C     THIS ROUTINE IS USED TO PLOT ADRIATIC SEA COASTLINE
C     FROM A DIFFERENT SOURCE

C     NPC:   NUMBER OF POINTS IN EACH COASTLINE-PART
C     NIS:   NUMBER OF PARTS
ccc   PARAMETER (NPC=500, NIS=873)
      PARAMETER (NPC=1500, NIS=873)

C     COASTAL POINTS INFO
c.... XA(),YA()      geographical coordinates of the Adriatic coast -
c.... XJ(,),YJ(,)    geographical coordinates of jugo islands -
c.... XJ(,),YJ(,)    geographical coordinates of differents parts of coast
c
c===============================================================================
        real xa(NPC),ya(NPC)
        real xj(NPC,NIS),yj(NPC,NIS)
        integer NPIS(NIS)

      COMMON /COASTA/ XA, YA
      COMMON /COASTJ/ XJ, YJ
      COMMON /NPOINT/ NPIS
c
c============================================================================

      CALL PSETC ('LEGEND',                                  'OFF')

      call PSETI ('polyline_line_thickness',          5)
      call PSETC ('polyline_line_style',        'SOLID')
      call PSETC ('polyline_line_colour',       'BLACK')


C     PLOT ADRIATIC COASTLINE

ccc   NPA=500
ccc   CALL PSET1R ('polyline_input_longitudes',  XA,NPA)
ccc   CALL PSET1R ('polyline_input_latitudes',   YA,NPA)
ccc   CALL PSET1R ('polyline_input_values',        XA,1)
C     PLOT IS DONE
ccc   call PLINE

C     PLOT JUGO ISLANDS
C     PLOT ADRIATIC COASTLINE

c     DO 100 ISO=1,50
      DO 100 ISO=1,NIS

         NNN=NPIS(ISO)

c     disregard small islands
      if(NNN.lt.12) go to 100

ccc   write(6,'(''DISEGNO LINEA N'',I5,i10,'' punti'')') ISO,NNN

c     PLOTTING LIMITS ARE SET

      CALL PSET1R ('polyline_input_longitudes',  XJ(1,ISO),NNN)
      CALL PSET1R ('polyline_input_latitudes',   YJ(1,ISO),NNN)
      CALL PSET1R ('polyline_input_values',        XJ(1,ISO),1)

C     PLOT IS DONE
      call PLINE

 100  CONTINUE

      CALL PSETC ('LEGEND',                                  'OFF')


      RETURN 
      END

C==============================================================================

      SUBROUTINE LAGUNA

C    THIS ROUTINE IS TO HIDE THE VENITIAN LGOON WITH UNIFORM COLOUR

      PARAMETER( NPS=10, NPOL=2)

      REAL RX1(NPS), RY1(NPS)
      REAL RLAG(NPOL)

      DATA RX1/ 12.30, 12.40, 12.55, 12.60, 12.10, 12.10,
     .          12.25, 12.30, 12.30, 12.30/
      DATA RY1/ 45.32, 45.44, 45.48, 45.55, 45.55, 45.20,
     .          45.20, 45.20, 45.30, 45.32/

      PARAMETER( NPM=9)
      REAL RX2(NPM), RY2(NPM)
      DATA RX2/13.08, 13.15, 13.30, 13.38, 13.45, 13.50, 13.50,
     .         13.00,13.08/
      DATA RY2/45.66, 45.71, 45.72, 45.68, 45.68, 45.80, 45.85,
     .         45.85,45.66/

      DATA RLAG/1.,2./
C ......................................................................


ccc   RLAG(1) = 1.0
ccc   RLAG(2) = 2.0

      call PSETC   ('legend',                          'off')
      call PSET1R  ('polyline_input_longitudes',     RX1,NPS)
      call PSET1R  ('polyline_input_latitudes',      RY1,NPS)
      call PSET1R  ('polyline_input_values',          RLAG,1)
ccc   CALL PSETR   ('polyline_input_break_indicator',  -999.)

      CALL PSETC   ('polyline_line_colour',           'grey')
      CALL PSETC   ('polyline_shade',                   'ON')
      CALL PSETC   ('polyline_shade_level_selection_type',  'list')
      CALL PSET1R  ('polyline_level_list',             RLAG,2)
      CALL PSETC   ('polyline_shade_min_level_colour', 'grey')
      CALL PSETC   ('polyline_shade_max_level_colour', 'grey')
      CALL PLINE

ccc   CALL PSETC   ('polyline_line_colour',           'grey')
      call PSET1R  ('polyline_input_longitudes',     RX2,NPM)
      call PSET1R  ('polyline_input_latitudes',      RY2,NPM)
      call PSET1R  ('polyline_input_values',          RLAG,2)
c     CALL PSETC   ('polyline_shade_min_level_colour', 'grey')
c     CALL PSETC   ('polyline_shade_max_level_colour', 'grey')
      CALL PLINE
      call PSETC  ('legend',                             'on')

      RETURN
      END
