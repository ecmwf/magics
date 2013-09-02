      Subroutine makeplot (X,kbasetime,kfcdate,khour,kstep,LLAT,LLON,RLAT,RLON,RLATPF,RLONPF,X5PBOX,X5PBOX2,XMXFF,KCSTORM,K5P,KINC)
      IMPLICIT NONE
      INTEGER K,K1,KDIM,JDIM,NENS,kbasetime,kfcdate,khour,kstep,NCONT,III,I,NPF,K5P,IC,IS,IL,KINC
      PARAMETER(KDIM=720,JDIM=361,NENS=52)
      INTEGER ibasetime,ihour,istep1,istep2,KTHICK,IFAIL
      INTEGER TYPE,ILEN,KCSTORM(K5P,5)
      INTEGER, allocatable :: IRANK(:)
      REAL X(KDIM,JDIM),RLATPF(80,NENS),RLONPF(80,NENS),X5PBOX(K5P,8),X5PBOX2(K5P,8),XMXFF(KINC,NENS),xboxpos(80)
      REAL CLARRAY(11),zpmax,zpmin,zpmin1,zpmin2,zpmin3,raux,rmin,rmax,raxismax,raxismin

      REAL LLON,RLON,LLAT,RLAT
      REAL XLON0,XLAT0,XLONSTEP,XLATSTEP
      REAL, allocatable ::  ZBARVAL(:,:)
      CHARACTER    CPF2*9,TITLE_1*30,TITLE_2*120,TITLE_3*120,CCOLOUR*20,FLGCOLOR*4,CAUX1*20,CAUX2*20,CAUX3*80,ctitle*2100,CINCOLOR*13
      CHARACTER*3  cstep1,cstep2
      CHARACTER*2  chour,CPF
      CHARACTER*8  cbasetime
      CHARACTER*20  boxpos(K5P)
      CHARACTER*20 CLIST(10)
      CHARACTER*10 YLTEXT(10)
      CHARACTER*3 CLIM0,CLIM1,CLIM2,CLIM3,CLIM4
      CHARACTER*3 CLIM5,CLIM6,CLIM7,CLIM8,CLIM9,CLIM10
      CHARACTER*10 YSUNIT,CSTYLE
      CHARACTER , allocatable ::  CBARCOL(:,:)*15
      CHARACTER CBARC(5)*15/'GREEN','ORANGE_YELLOW','RED','MAGENTA','BLACK'/





      character*2100, allocatable ::  title(:)
      !dimension title(6)
! define the data for the 2 graphs
      real ymin(3), ymax(3)
      character*22 xtime(3)

! define the text
      character*20 lines(1)


      data ymin /0.,0.,0./
      data ymax /22.,21.,17./

      data xtime /"2011-03-31 00:00:00", &
             "2011-04-01 00:00:00", &
             "2011-04-02 00:00:00"/

      data lines /"My other Graph"/

! Set the contours levels

      NCONT=11
      CLARRAY(1)=5
      CLARRAY(2)=10
      CLARRAY(3)=20
      CLARRAY(4)=30
      CLARRAY(5)=40
      CLARRAY(6)=50
      CLARRAY(7)=60
      CLARRAY(8)=70
      CLARRAY(9)=80
      CLARRAY(10)=90
      CLARRAY(11)=110

      YSUNIT="%"

      write(cbasetime,'(I8)') kbasetime
      write(chour,'(I2)') khour
      write(cstep1,'(I3)') kstep
      !write(cstep2,'(I3)') istep2
      TITLE_1="Date "//cbasetime//" "//chour//" UTC"
      TITLE_2=TITLE_1//"Probability that --- will pass within 120 km radius during the next "//cstep1//"  hours"
      TITLE_3="tracks: black=OPER; dot black=CTRL; blue=EPS members"
  
      call popen
    
      call psetr('super_page_x_length', 29.7)
      call psetr('super_page_y_length', 21.)
      call psetc('page_id_line', 'off')
      call psetc('layout', 'positional')
    
      call psetr('page_x_length', 17.)
      call psetr('subpage_y_position', 21./3)
      call psetr('subpage_x_length', 15.)

! Setting the coordinates of the geographical area

      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'CYLINDRICAL')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    LLON)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     LLAT)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   RLON)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    RLAT)


      CALL PSETC('MAP_COASTLINE',                      'ON')
      CALL PSETC('MAP_COASTLINE_COLOUR',            'CREAM') !#Original BLACK
      CALL PSETC('MAP_COASTLINE_LAND_SHADE',        'ON') !Prates
      CALL PSETC('MAP_COASTLINE_LAND_SHADE_COLOUR',     'CREAM') !Prates
      CALL PSETC('MAP_GRID',                        'ON')
      CALL PSETC('MAP_GRID_COLOUR',                 'GREY')
      CALL PSETR('MAP_GRID_LONGITUDE_INCREMENT',                 20.)
      CALL PSETC('MAP_GRID_LINE_STYLE',               'DOT')
      CALL PSETC('MAP_LABEL_COLOUR',               'BLACK')

      call pcoast

!
! Data input - ????? the data is assumed to be in lat/lon regular grid 0.5 degrees ??????
!
      xlon0=0.
      xlat0=90.
      xlonstep=0.5
      xlatstep=-0.5

      CALL PSETC ('INPUT_FIELD_ORGANIZATION',     'REGULAR')
      CALL PSETR ('INPUT_FIELD_INITIAL_LONGITUDE',    xlon0)
      CALL PSETR ('INPUT_FIELD_INITIAL_LATITUDE',     xlat0)
      CALL PSETR ('INPUT_FIELD_LONGITUDE_STEP',    xlonstep)
      CALL PSETR ('INPUT_FIELD_LATITUDE_STEP',     xlatstep)
      CALL PSET2R('INPUT_FIELD',                X,KDIM,JDIM)

      FLGCOLOR='RGB'

      IF (FLGCOLOR.EQ.'RGB') THEN
        CLIST(1) ='RGB(.7969,.3633,.5664)'
        CLIST(2) ='RGB(.7656,.1602,.1797)'
        CLIST(3) ='RGB(.8438,.5312,.2226)'
        CLIST(4) ='RGB(.9570,.8398,.3672)'
        CLIST(5) ='RGB(.6875,.7773,.3671)'
        CLIST(6) ='RGB(.4062,.6445,.2930)'
        CLIST(7) ='RGB(0.,.5156,.3906)'
        CLIST(8) ='RGB(0.,.5781,.8359)'
        CLIST(9) ='RGB(0.,.3281,.5976)'
        CLIST(10)='RGB(.3750,.2226,.5234)'
      END IF
!
! Contour options 

      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
!      CALL PSETC ('INPUT_FIELD_GRADIENT_CONTROL',    'LEAST' )
      CALL PSETC ('CONTOUR_SHADE',                        'ON')
      CALL PSETC ('CONTOUR_SHADE_METHOD',          'AREA_FILL')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',        CLARRAY(1))
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',    CLARRAY(NCONT))

      CALL PSETC ('CONTOUR_SHADE_COLOUR_METHOD',        'LIST')
      CALL PSET1C('CONTOUR_SHADE_COLOUR_LIST',  CLIST, NCONT-1)
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR',        CLIST(1))
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR',        CLIST(NCONT-1))

      CALL PSET1R('CONTOUR_LEVEL_LIST',         CLARRAY, NCONT)
      CALL PSETC ('CONTOUR',                             'OFF')
      CALL PSETR ('CONTOUR_MIN_LEVEL',              CLARRAY(1))
      CALL PSETR ('CONTOUR_MAX_LEVEL',          CLARRAY(NCONT))
      CALL PSETC('LEGEND','ON')
      CALL PSETC('CONTOUR_LEGEND','ON')

      CALL PCONT

!
! Title 

      CALL PSETI ('TEXT_LINE_COUNT',      2)
!      CALL PSETC ('TEXT_JUSTIFICATION',           'LEFT')
      CALL PSETI ('TEXT_FIRST_LINE',      1)
!      CALL PSETC ('TEXT_LINE_1',TITLE_1)
      CALL PSETC ('TEXT_LINE_1',    TITLE_2)
      CALL PSETC ('TEXT_LINE_2',    TITLE_3)
      CALL PSETC ('TEXT_BORDER',      "OFF")
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',  .5)
!c      CALL PSETR ('TEXT_LINE_SPACE_RATIO',  1.)
      CALL PSETC('TEXT_FONT_STYLE',  'BOLD')
!      CALL PSET1C ('LEGEND_TEXT_COLOUR','BLACK'     )

!      CALL PTEXT




! legend

      CALL PSETC('LEGEND','ON')
      CALL PSETR ('LEGEND_TEXT_font_size',  0.2)
      CALL PSET1C ('legend_user_lines',  (/'entry1', 'entry2', 'entry3'/), 3)
	  call psetc("legend_text_composition", "user_text_only")

! plot individual tracks for EPS, CTRL and HiRes

      DO I=1, NENS

        NPF= count(RLATPF(1:80,I) /= -999)

!        CCOLOUR='BLUE'
!        CCOLOUR='RGB(.5586,.5781,.5938)'  ! GREY WOOL
!        CCOLOUR='RGB(.4570,.4062,.3398)'  ! COCOA
!        CCOLOUR='RGB(.6758,.3516,.9961)'  ! PETAL
        CCOLOUR='RGB(.5352,.4062,.3476)'  ! CHOCOLATE
        KTHICK=1
        CSTYLE='SOLID'
        IF (I.EQ.NENS-1) THEN
             CCOLOUR='BLACK'
             CSTYLE='DOT'
             KTHICK=5
        ENDIF
        IF ( I.EQ.NENS) THEN
             CCOLOUR='BLACK'
             KTHICK=5
        ENDIF

        CALL PSETI ('GRAPH_LINE_THICKNESS',             KTHICK)
!        CALL PSET1R('GRAPH_CURVE_X_VALUES',RLONPF(1:NPF,I),NPF)
!        CALL PSET1R('GRAPH_CURVE_Y_VALUES',RLATPF(1:NPF,I),NPF)
        CALL PSETC ('GRAPH_LINE_COLOUR',               CCOLOUR)
        CALL PSET1R('GRAPH_CURVE_X_VALUES',PACK(RLONPF(1:80,I),RLONPF(1:80,I) /= -999),NPF)
        CALL PSET1R('GRAPH_CURVE_Y_VALUES',PACK(RLATPF(1:80,I),RLATPF(1:80,I) /= -999),NPF)
        CALL PSETC ('GRAPH_LINE_STYLE',                 CSTYLE)
        CALL PSETC ('LEGEND', 'OFF')

        CALL PGRAPH
      END DO
 
      CALL PRESET ('GRAPH_CURVE_X_VALUES')
      CALL PRESET ('GRAPH_CURVE_Y_VALUES')
!      CALL PRESET ('GRAPH_TYPE')
      
! add a legend below the strike prob map

      ALLOCATE (TITLE(KINC+1)) 
      title(1) ='<font size=".33" > </font> List of ensemble members numbers (intensity in colour)'
      !title(2)=''
      CAUX1='<font colour='
      IS=scan(caux1,'=',.TRUE.)
      CAUX2='</font>'
      
      DO I=1,KINC
       ctitle=''
     
       DO K=1,NENS
         IL=scan(ctitle,'>',.TRUE.)
         
         IF (XMXFF(I,K).NE.-999.) THEN
            
            IF (XMXFF(I,K).LT.17) THEN
               CINCOLOR='GREEN'
               IC=len_trim (CINCOLOR)
            END IF
            IF (XMXFF(I,K).GE.17.AND.XMXFF(I,K).LE.32) THEN
               CINCOLOR='ORANGE_YELLOW'
               IC=len_trim (CINCOLOR)
            ENDIF   
            IF (XMXFF(I,K).GE.33.AND.XMXFF(I,K).LE.42) THEN
               CINCOLOR='RED'
               IC=len_trim (CINCOLOR)
            END IF
            IF (XMXFF(I,K).GE.43.AND.XMXFF(I,K).LE.48) THEN
               CINCOLOR='MAGENTA'
               IC=len_trim (CINCOLOR)
            END IF
            IF (XMXFF(I,K).GE.49.) THEN
               CINCOLOR='BLACK'
               IC=len_trim (CINCOLOR)
            END IF
            WRITE(CPF,'(I2)') K
            caux3=caux1(1:IS)//'"'//CINCOLOR(1:IC)//'">'//CPF//" "//CAUX2(1:scan(caux2,'>',.TRUE.))
            IF (K>=NENS-1) THEN 
               IF (K==NENS-1) WRITE(CPF,'(A2)') 'ct'
               IF (K==NENS  ) WRITE(CPF,'(A2)') 'hr'
               caux3=caux1(1:IS)//'"'//CINCOLOR(1:IC)//'">'//CPF//" "//CAUX2(1:scan(caux2,'>',.TRUE.))
               ctitle=caux3//ctitle(1:IL+1) 
            ELSE
               ctitle=ctitle(1:IL+1)//caux3 
            END IF
         ELSE
 
         END IF
         
       END DO 
       IL=scan(ctitle,'>',.TRUE.)  
       
       WRITE(CPF2,'(A1,I3,A5)') '+',24*(I),' h : ' 
       title(I+1)='<font colour="rgb(.56,.58,.59)"> '//CPF2//'</font> '//ctitle(1:IL)
      END DO
! 
! text settings 
!
      call psetc('text_mode', 'positional')
      call psetr('text_box_y_length', 2.500000)
      call psetc('text_justification', 'left')
      call psetc('text_border', 'on')
      call pset1c('text_lines', title, KINC+1)
      call psetr('text_box_x_length', 15.000)
      call psetr('text_box_x_position', .30000)
      call psetc('text_html', 'true')
      call psetc('text_box_blanking', 'on')
      call psetc('text_border_colour', 'black')
      call psetr('text_box_y_position', 4.000000)
      call psetc('text_colour', 'black')
      call psetr('text_font_size', 0.21)
      call ptext

      CALL PRESET ('text_mode') 
      call PRESET ('text_lines')

! new page

      call pnew('page')

      
      call psetr('page_y_length', 5.)
      call psetr('page_x_position', 17.)
      call psetr('page_x_length', 9.)
      call psetr('subpage_x_length', 11.)
      call psetr('page_y_position', 3.)
      call psetc("legend", "off")

! define the cartesian projection

      call psetc("subpage_map_projection", "none") !cartesian
      call psetr('subpage_y_position', 1.)
      call psetr('subpage_y_length', 3.5)
      call psetr('subpage_vertical_axis_width', 0.25)      
      CALL PSETC ('TEXT_LINE_1', 'Mean Sea Level Pressure in Tropical Cyclone Centre (HPa)')
      CALL PSETI ('TEXT_LINE_COUNT', 1)
      call psetr('subpage_horizontal_axis_height', 0.25) 
      call psetc('text_border', 'off')
      call psetc('text_box_blanking', 'on')
      CALL PTEXT 

!      do i=1,K5P
!         xoxpos(i)=float(i-1)*6
!      end do 

      boxpos(1)="2011-03-31 00:00:00"
      boxpos(2)="2011-03-31 12:00:00"
      boxpos(3)="2011-04-01 00:00:00"
      boxpos(4)="2011-04-01 12:00:00"
      boxpos(5)="2011-04-02 00:00:00"
      boxpos(6)="2011-04-02 12:00:00"
      boxpos(7)="2011-04-03 00:00:00"
      boxpos(8)="2011-04-03 12:00:00"
      boxpos(9)="2011-04-04 00:00:00"
      boxpos(10)="2011-04-04 12:00:00"
      boxpos(11)="2011-04-05 00:00:00"
      boxpos(12)="2011-04-05 12:00:00"
      boxpos(13)="2011-04-06 00:00:00"
      boxpos(14)="2011-04-06 12:00:00"
      boxpos(15)="2011-04-07 00:00:00"
      boxpos(16)="2011-04-07 12:00:00"
      boxpos(17)="2011-04-08 00:00:00"
      boxpos(19)="2011-04-08 12:00:00"
      boxpos(20)="2011-04-09 00:00:00"
      boxpos(21)="2011-04-09 12:00:00"
      boxpos(22)="2011-04-10 00:00:00"
      boxpos(23)="2011-04-10 12:00:00"
      boxpos(24)="2011-04-11 00:00:00"
      boxpos(25)="2011-04-11 12:00:00"


! define horizontal axis

      call psetc("axis_orientation","horizontal")
      call psetc("axis_type","date")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_date_min_value", "2011-03-31 00:00:00")
      call psetc("axis_date_max_value", "2011-04-10 00:00:00")
      call psetc("axis_grid_line_style", "dot")
      !call psetr("axis_tick_interval", 12.)
      call paxis

! define vertical axis
! compute the max val for y-axis
      zpmax=maxval(x5pbox(1:K5P,5:7))
      if (zpmax.gt.1000.) then
         raux=zpmax
         rmin=1000.
         rmax=1005.
      else
         raux=zpmax
         rmin=905.
         rmax=910.
      end if 
      do while ((raux.ge.rmin).and.(raux.ge.rmax))
         rmin=rmax
         rmax=rmax+5.
      end do
      raxismax=rmax+5

! compute the min val for y-axis
      zpmin1=minval(x5pbox(1:K5P,1))
      zpmin2=minval(x5pbox(1:K5P,6))
      zpmin3=minval(x5pbox(1:K5P,7))
      zpmin =min(zpmin1,zpmin2,zpmin3)
      if (zpmin.gt.1000.) then
         raux=zpmin
         rmin=1000.
         rmax=1005.
      else
         raux=zpmin
         rmin=905.
         rmax=910.
      end if 
      do while ((raux.ge.rmin).and.(raux.ge.rmax))
         rmin=rmax
         rmax=rmax+5.
      end do
      raxismin=rmin-5

      call psetc("axis_orientation", "vertical")
      call psetc("axis_type","regular")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_grid_line_style", "dot")
      call psetr("axis_min_value", raxismin)
      call psetr("axis_max_value", raxismax)
      call psetr("axis_tick_interval", 10.)
      call paxis

      CALL PSET1C('BOXPLOT_DATE_POSITIONS',boxpos,K5P)
      CALL PSET1R('BOXPLOT_MINIMUM_VALUES',x5pbox(1:K5P,1),K5P)
      CALL PSET1R('BOXPLOT_MAXIMUM_VALUES',x5pbox(1:K5P,5),K5P)
      CALL PSET1R('BOXPLOT_MEDIAN_VALUES', x5pbox(1:K5P,3),K5P)
      CALL PSET1R('BOXPLOT_BOX_UPPER_VALUES',x5pbox(1:K5P,4),K5P)
      CALL PSET1R('BOXPLOT_BOX_LOWER_VALUES', x5pbox(1:K5P,2),K5P)
      CALL PSETR ('BOXPLOT_BOX_WIDTH', .35)
      CALL PSETC ('BOXPLOT_BOX_COLOUR', "BLUE_GREEN")

      CALL PBOXPLOT

! Define the blue gurve with the min values
      !call psetc("graph_line_colour", "blue")
      call pseti("graph_line_thickness", 5)
      call psetc("graph_line_style", "dot")
      !call psetc("graph_symbol", "on")
      !call psetc("legend_user_text",&
      !"<font colour='blue'> Min </font>")
      !call pseti("graph_symbol_marker_index", 1)
      !call psetr("graph_symbol_height", 0.5)
      !call psetc("graph_symbol_colour", "black")
      !call pset1r("graph_curve_date_x_values", xtime, 3)
      !call pset1r("graph_curve_y_values", ymin, 3)
      call pset1c("x_date_values", boxpos,K5P)!xtime, 3)
      call pset1r("y_values", x5pbox(1:K5P,6),K5P)
      call pgraph

      call psetc("graph_line_style", "solid")
      call pset1r("y_values", x5pbox(1:K5P,7),K5P)
      call pgraph
!
! new page

      call pnew('page')

      CALL PSETC ('TEXT_MODE','TITLE')
      CALL PSETC ('TEXT_LINE_1', 'Mean Sea Level Pressure in Tropical Cyclone Centre (HPa)')
      CALL PTEXT 

! define the cartesian projection 
      call psetr('page_y_position', 8.)
      call psetc("subpage_map_projection", "cartesian")
     


! define horizontal axis
      call psetc("axis_orientation","horizontal")
      call psetc("axis_type","date")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_date_min_value", "2011-03-31 00:00:00")
      call psetc("axis_date_max_value", "2011-04-10 00:00:00")
      call psetc("axis_grid_line_style", "dot")
      call psetr("axis_tick_interval", 1.)
      call paxis

! define vertical axis
! compute the max val for y-axis
      zpmax=maxval(x5pbox2(1:K5P,5:7))
      !if (zpmax.gt.1000.) then
         raux=zpmax
         rmin=0.
         rmax=5.
      !end if 
      do while ((raux.ge.rmin).and.(raux.ge.rmax))
         rmin=rmax
         rmax=rmax+5.
      end do
      raxismax=rmax+5

! compute the min val for y-axis
      zpmin1=minval(x5pbox2(1:K5P,1))
      zpmin2=minval(x5pbox2(1:K5P,6))
      zpmin3=minval(x5pbox2(1:K5P,7))
      zpmin =min(zpmin1,zpmin2,zpmin3)
      !if (zpmin.gt.1000.) then
         raux=zpmin
         rmin=0.
         rmax=5.
      !end if 
      do while ((raux.ge.rmin).and.(raux.ge.rmax))
         rmin=rmax
         rmax=rmax+5.
      end do
      raxismin=rmin-5
      call psetc("axis_orientation", "vertical")
      call psetc("axis_type","regular")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_grid_line_style", "dot")
      call psetr("axis_min_value", raxismin) 
      call psetr("axis_max_value", raxismax)
      call psetr("axis_tick_interval", 10.)
      !call psetr("axis_min_value", 10.)
      !call psetr("axis_max_value", 30.)
      call paxis

      CALL PSET1C('BOXPLOT_DATE_POSITIONS',boxpos,K5P)
      CALL PSET1R('BOXPLOT_MINIMUM_VALUES',x5pbox2(1:K5P,1),K5P)
      CALL PSET1R('BOXPLOT_MAXIMUM_VALUES',x5pbox2(1:K5P,5),K5P)
      CALL PSET1R('BOXPLOT_MEDIAN_VALUES', x5pbox2(1:K5P,3),K5P)
      CALL PSET1R('BOXPLOT_BOX_UPPER_VALUES',x5pbox2(1:K5P,4),K5P)
      CALL PSET1R('BOXPLOT_BOX_LOWER_VALUES', x5pbox2(1:K5P,2),K5P)
      CALL PSETR ('BOXPLOT_BOX_WIDTH', .35)


      CALL PBOXPLOT

! Define the blue gurve with the min values
      !call psetc("graph_line_colour", "red")
      call psetc("graph_line_style", "dot")
      !call pseti("graph_line_thickness", 8)
      !call psetc("graph_symbol", "off")
      !call psetc("legend_user_text",&
      !"<font colour='red'> Max </font>")
      !call pseti("graph_symbol_marker_index", 1)
      !call psetr("graph_symbol_height", 0.5)
      !call psetc("graph_symbol_colour", "black")
!  call preset("x_values")
      call pset1c("x_date_values", boxpos,K5P)
      call pset1r("y_values", x5pbox2(1:K5P,6),K5P) 

      call pgraph
      call psetc("graph_line_style", "solid")
      call pset1r("graph_curve_y_values", x5pbox2(1:K5P,7),K5P) 
      call pgraph

	 

!
! new page
!
      call pnew('page')
      CALL PTEXT 

! define the cartesian projection
      call psetr('page_y_position', 13.)
      call psetc("subpage_map_projection", "cartesian")
     


! define horizontal axis
      call psetc("axis_orientation","horizontal")
      call psetc("axis_type","date")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_date_min_value", "2011-03-31 00:00:00")
      call psetc("axis_date_max_value", "2011-04-10 00:00:00")
      call psetc("axis_grid_line_style", "dot")
      call psetr("axis_tick_interval", 1.)
      call paxis

! define vertical axis
      call psetc("axis_orientation", "vertical")
      call psetc("axis_type","regular")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_grid_line_style", "dot")
      call psetr("axis_min_value", 0.)
      call psetr("axis_max_value", 100.)
      call psetr("axis_tick_interval", 15.)
      call paxis

! prepare the data for the next plot

      call psetc("graph_type","bar")
      call psetr ("graph_bar_width",4.0*3600)

      ALLOCATE (IRANK      (5))
      ALLOCATE (ZBARVAL(K5P,5))
      ALLOCATE (CBARCOL(K5P,5))
      DO I=1,K5P 

         IFAIL=0

         CALL M01DBF (KCSTORM(I,1:5),1,5,'D',IRANK,IFAIL) ! variable IRANK MUST have the same dimension as KCSTORM(I,1:5)
       
         DO K=1,5
           DO K1=1,5
             IF (K1 == IRANK(K)) THEN
               ZBARVAL(I,K1)=100.*FLOAT(KCSTORM(I,K))/x5pbox2(I,8)
               CBARCOL(I,K1)=CBARC(K)
             END IF
           END DO
         END DO
         call pset1c("x_date_values",boxpos(i), 1)
         call preset("y_values")
         DO K=1,5
           call pset1r("y_upper_values", ZBARVAL(I,K),1)
           call psetc ("graph_bar_colour",       CBARCOL(I,K))
           call psetc ("graph_bar_line_colour",    CBARCOL(I,K))
           call pgraph
           print*,ZBARVAL(I,1)+ZBARVAL(I,2)+ZBARVAL(I,3)+ZBARVAL(I,4)+ZBARVAL(I,5)  
         END DO 
      END DO
! Define the blue gurve with the min values
      !call psetc("graph_line_colour", "red")
      !call pseti("graph_line_thickness", 8)
      !call psetc("graph_symbol", "on")
      !call psetc("legend_user_text",&
      !"<font colour='blue'> Max </font>")
      !call pseti("graph_symbol_marker_index", 1)
      !call psetr("graph_symbol_height", 0.5)
      !call psetc("graph_symbol_colour", "black")
      !call pset1c("graph_curve_date_x_values", xtime, 3)
      !call pset1r("graph_bar_y_lower_values", ymin, 3)
      !call pset1r("graph_bar_x_values",xboxpos(5), 1)
      !call pset1r("graph_bar_y_upper_values", ZBARVAL(5,1),1)
      !call psetc ("graph_shade_colour", "red")
      !call pgraph
      call pclose
      end
