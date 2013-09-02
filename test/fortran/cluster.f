           PROGRAM GRAPH_EPS_STAT
		
	  CHARACTER AREA*10,STEPC*4,STEPT*4,CLASS*10
	  CHARACTER YD*8
      CHARACTER*72 TEXTH,TEXTHH
      DIMENSION   ID(400),CLUST(400),TUBES(400),
     &			CTHR(400),CSEAS(400),TRAD(400),RLOW(400),STDC(400),
     &			STDT(400),CCERR(400),TSEAS(400),TCERR(400)
	  CHARACTER*19 XD(400),FDATEI,FDATEF
	  
	  OPEN (UNIT=10,FILE='/scratch/mo/moz/eps.dat',
     &	  			STATUS='OLD', FORM='FORMATTED')
	  
	  READ(10,'(A10,I2)')CLASS,IEXP
	  READ(10,'(A10)')AREA
	  READ(10,*)NSTEPC,NSTEPT

	  WRITE(STEPC,'(I3)')NSTEPC
	  WRITE(STEPT,'(I3)')NSTEPT

10	  CONTINUE
	
	  ND=ND+1	     
 	 
	  READ(10,*,END=20) IYMD,CLUST(ND),TUBES(ND),STDC(ND),STDT(ND),
     &	       CSEAS(ND),TSEAS(ND),CTHR(ND),TRAD(ND),CCERR(ND),TCERR(ND)    

      write (yd,'(i8)') iymd
      XD(ND)=YD(1:4)//'-'//YD(5:6)//'-'//YD(7:8)//' 00:00:00'
      print *, XD(ND),  "st", STDC(ND)
      GOTO 10
    
20    CONTINUE

	  ND=ND-1
	  FDATEI=XD(1)
	  FDATEF=XD(ND)
      WRITE (*,*) FDATEI,'  ',FDATEF

C*** OPEN MAGICS AND SET PAGE PARAMETERS

C     Open MAGICS and set the output device

      CALL POPEN
   

c     Note: Replaced the following line with the next one for the
C     Magics++ test suite
c	  CALL PSETC('WORKSTATION_1','PS_COL_A3')
      CALL PSETC ('output_format',    'ps')
      CALL PSETC ('output_ps_device',    'ps_a3')
      CALL PSETC ('output_name', 'metops_cluster_tube_new')
      PX=29.7
      PY=42.0
      ppx=29.6
      ppy=20.9

      CALL PSETR('SUPER_PAGE_X_LENGTH',PX)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',PY)
      CALL PSETR('PAGE_X_LENGTH',PPX)
      CALL PSETR('PAGE_Y_LENGTH',PPY)
c      CALL PSETC('layout','positional')
    
      CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')  
      CALL PSETI('SUBPAGE_FRAME_THICKNESS',2)  
      CALL PSETC('PAGE_ID_LINE','ON')
      
            CALL PSETR('SUBPAGE_Y_LENGTH',4.)
            CALL PSETR('SUBPAGE_X_LENGTH',5.5)
            CALL PSETR('SUBPAGE_Y_POSITION',15.8)  
            CALL PSETR('SUBPAGE_x_POSITION',1.) 
            CALL PSETI('SUBPAGE_FRAME_THICKNESS',2) 
            CALL PSETC('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')  
            CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    30.0)
            CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -20.0)
            CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   75.0)
            CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   45.0)
            call psetc ('map_coastline_land_shade','on')
            call psetc ('map_coastline_land_shade_colour','cream')
            call psetc ('map_coastline_sea_shade','off') 
            call psetc ('map_coastline_sea_shade_colour','none')   
            CALL PSETC ('MAP_COASTLINE_COLOUR', 'tan')
            cALL PSETC ('MAP_label_COLOUR',      'tan')
            CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
            CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT', 10.0)
            CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   5.0)

            call pcoast
            call pnew('subpage')  
200              CALL PSETR('SUBPAGE_Y_LENGTH',12.6225)
            CALL PSETR('SUBPAGE_X_LENGTH',25.245)
             CALL PSETR('SUBPAGE_Y_POSITION',2.5)   
             CALL PSETR('SUBPAGE_X_POSITION',2.)  
             CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')  
      
CCCC		CLUSTERS

C---------- heading legend
C---------- plot options

       TEXTH='CLUSTERS'//'     '//'500hPa'
       TEXTHH='Area :'//area//' step:+'//stepc
       
       
       CALL PSETC('TEXT_COLOUR','BLACK')
       CALL PSETI('TEXT_LINE_COUNT',2)
       call psetc('TEXT_LINE_1',TEXTH)
       CALL PSETC('TEXT_LINE_2',TEXTHH)
       CALL PSETR('TEXT_REFERENCE_CHARACTER_HEIGHT',1.)

       call psetc('LEGEND','ON')
      
       call psetc('LEGEND_BORDER','ON')
  
       CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
       CALL PSETR('LEGEND_BOX_X_LENGTH',4.8)
       CALL PSETR('LEGEND_BOX_Y_LENGTH',3.5)
       call psetr('legend_box_x_position', 21.5)
       call psetr('legend_box_y_position', 13.3)
       call psetc('legend_box_mode', 'positional')
       call psetc('legend_box_blanking', 'on')
       call psetc('legend_box_blanking', 'on')
        
       call pseti('LEGEND_BORDER_THICKNESS',2)
       call psetc('LEGEND_BORDER_COLOUR','black')
       call psetc('legend_entry_plot_direction', 'column')
       
       CALL PSETC('LEGEND_TEXT_COLOUR','BLACK')
       

c       call psetc('LEGEND_TEXT_COMPOSITION','USER_TEXT_ONLY')
       call psetc('LEGEND_USER_TEXT','clusters')
   	  call psetr('LEGEND_text_height',0.4)
	  
	   CALL PSETI('GRAPH_LINE_THICKNESS',3)
       CALL PSETR('axis_days_label_height', 0.3)
       CALL PSETR('axis_months_label_height', 0.3)
       CALL PSETR('axis_years_label_height', 0.3)

C--------------- Axis ----------------------------------
C-------- PLOT HORIZONTAL AXIS ------------------------
 
           
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
            CALL PSETC('AXIS_POSITION','BOTTOM')
            CALL PSETC('AXIS_TYPE','DATE')
            CALL PSETC('AXIS_GRID','OFF')
            CALL PSETC('AXIS_LINE_COLOUR','BLACK')
            CALL PSETC('AXIS_DATE_TYPE','DAYS')
            CALL PSETC('AXIS_DATE_MIN_VALUE',FDATEI)
            CALL PSETC('AXIS_DATE_MAX_VALUE',FDATEF)
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETR('AXIS_TICK_INTERVAL',1.0)
c            CALL PSETC('AXIS_TICK_LABEL_fTY','HIGH')
            CALL PSETC('AXIS_DAYS_LABEL_COLOUR','BLACK')
            CALL PSETC('AXIS_DAYS_LABEL','NUMBER')
            CALL PSETC('AXIS_LINE','ON')
            CALL PSETR('axis_tick_interval', 2.)
            CALL PRESET('AXIS_TITLE_TEXT')
            CALL PAXIS


C-------- PLOT VERTICAL AXIS ----------------------------
            
C##### Bar vertical axis 

            CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_POSITION','right')
            CALL PSETR('AXIS_MIN_VALUE',0.0)
            CALL PSETR('AXIS_MAX_VALUE',12.0)
           
            CALL PSETR('AXIS_TICK_INTERVAL',1.0)
      
c            CALL PSETC('AXIS_TICK_LABEL_FORMAT','(I2)')
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',2)
            CALL PSETC('AXIS_TICK_LABEL_QUALITY','HIGH')
           
       
            CALL PSETC('AXIS_TICK_LABEL_COLOUR','black')

            CALL PSETC('AXIS_LINE','ON')
            
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETR('AXIS_TITLE_height',0.6)
            CALL PSETC('AXIS_TITLE_COLOUR','black')
            CALL PSETC('AXIS_TITLE_ORIENTATION','PARALLEL')
c            CALL PSETC('AXIS_TITLE_QUALITY','HIGH')
c            CALL PSETC('AXIS_TITLE_TEXT','n. of clusters')
C---- GRIDDING LINES

            CALL PSETC('AXIS_GRID','ON')
            CALL PSETC('AXIS_GRID_COLOUR','BLACK')
            CALL PSETI('AXIS_GRID_THICKNESS',2)
            CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')

 
      		CALL PAXIS

C-------- SETTING THE GRAPH ----------------------------

cc### Bar graph ---CLUSTERS--

      CALL PSETC('GRAPH_TYPE','BAR')
c      CALL PSETC('GRAPH_SHADE','ON')
c      CALL PSETC('GRAPH_SHADE_STYLE','AREA_FILL')
c      CALL PSETC('GRAPH_SHADE_COLOUR','GREY')
      CALL PSETC('GRAPH_BAR_COLOUR','GREY')
c      CALL PSETR('GRAPH_BAR_WIDTH',0.2)
      CALL PSET1C('GRAPH_BAR_DATE_X_VALUES',XD,ND)
C      CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES',RLOW,ND)
      CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES', CLUST,ND)
      call psetc('LEGEND_USER_TEXT','clusters')
      CALL PGRAPH
      call pnew('subpage')
      
      

      
      
   	      CALL PSETC('legend','ON')
            
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_GRID','OFF')
            CALL PSETC('AXIS_TYPE','DATE')
            CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
            CALL PSETC('AXIS_POSITION','BOTTOM')
            CALL PSETC('AXIS_TYPE','DATE')
            CALL PSETC('AXIS_DATE_TYPE','DAYS')
            CALL PSETC('AXIS_DATE_MIN_VALUE',FDATEI)
            CALL PSETC('AXIS_DATE_MAX_VALUE',FDATEF)
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETR('AXIS_TICK_INTERVAL',2.0)
c            CALL PSETC('AXIS_TICK_LABEL_QUALITY','HIGH')
            CALL PSETC('AXIS_DAYS_LABEL_COLOUR','BLACK')
            CALL PSETC('AXIS_DAYS_LABEL','NUMBER')
            CALL PSETC('AXIS_LINE','ON')
            CALL PRESET('AXIS_TITLE')            
            CALL PRESET('AXIS_TITLE_TEXT')
            CALL PAXIS

   
cc# Curve
            CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_POSITION','LEFT')
            CALL PSETR('AXIS_MIN_VALUE',0.0)
            CALL PSETR('AXIS_MAX_VALUE',180.0)
            
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETR('AXIS_TICK_INTERVAL',10.0)
            
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',2)
           
            CALL PSETC('AXIS_MINOR_TICK','OFF')
            CALL PSETC('AXIS_TITLE_TEXT','metres')

C---- GRIDDING LINES

            CALL PSETC('AXIS_GRID','OFF')
            CALL PSETC('AXIS_GRID_COLOUR','BLACK')
           
            CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')

			CALL PAXIS

			
			CALL PSETC('GRAPH_LINE_STYLE','SOLID')
			CALL PSETC('GRAPH_LINE_COLOUR','BLUE')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',STDC,ND)
            call psetc('LEGEND_USER_TEXT','eps_std')
	   
			CALL PGRAPH

		
			CALL PSETC('GRAPH_LINE_STYLE','SOLID')
			CALL PSETC('GRAPH_LINE_COLOUR','BROWN')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSETC('GRAPH_MISSING_DATA_MODE','IGNORE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',CCERR,ND)
            call psetc('LEGEND_USER_TEXT','control error')
       
			CALL PGRAPH

		
			CALL PSETC('GRAPH_LINE_STYLE','SOLID')
			CALL PSETC('GRAPH_LINE_COLOUR','RED')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',CSEAS,ND)
            call psetc('LEGEND_USER_TEXT','calib. std')
	        

			CALL PGRAPH

			
			CALL PSETC('GRAPH_LINE_STYLE','DASH')
			CALL PSETC('GRAPH_LINE_COLOUR','BLUE')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',CTHR,ND)
            call psetc('LEGEND_USER_TEXT','clust. thr.')
			CALL PGRAPH
            call PTEXT
            
            
			CALL PNEW('PAGE')
            call psetc('legend', 'off')
            
            
             CALL PSETR('SUBPAGE_Y_LENGTH',4.)
            CALL PSETR('SUBPAGE_X_LENGTH',5.5)
            CALL PSETR('SUBPAGE_Y_POSITION',15.8)  
            CALL PSETR('SUBPAGE_x_POSITION',1.) 
            CALL PSETI('SUBPAGE_FRAME_THICKNESS',2) 
            CALL PSETC('SUBPAGE_MAP_PROJECTION','CYLINDRICAL')  
           
            call psetc ('map_coastline_land_shade','on')
            call psetc ('map_coastline_land_shade_colour','cream')
            call psetc ('map_coastline_sea_shade','off') 
            call psetc ('map_coastline_sea_shade_colour','none')   
            CALL PSETC ('MAP_COASTLINE_COLOUR', 'tan')
            cALL PSETC ('MAP_label_COLOUR',      'tan')
            CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
            CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT', 10.0)
            CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   5.0)

            call pcoast
            call pnew('subpage')  
              CALL PSETR('SUBPAGE_Y_LENGTH',12.6225)
            CALL PSETR('SUBPAGE_X_LENGTH',25.245)
             CALL PSETR('SUBPAGE_Y_POSITION',2.5) 
             CALL PSETR('SUBPAGE_X_POSITION',2.)  
             CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')  

cc###########	--TUBES--
       		TEXTH='TUBES'//'     '//'500hPa'
            	TEXTHH='Area :'//area//'  step:+'//stept
		    
		    
       		CALL PSETC('TEXT_COLOUR','BLACK')
       		CALL PSETI('TEXT_LINE_COUNT',2)
       		call psetc('TEXT_LINE_1',TEXTH)
       		CALL PSETC('TEXT_LINE_2',TEXTHH)

       		call psetc('LEGEND','ON')
       		

 			
	   		
	  	  
	  	   
C--------------- Axis ----------------------------------
C-------- PLOT HORIZONTAL AXIS ------------------------
 
          
            CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
            CALL PSETC('AXIS_POSITION','BOTTOM')
            CALL PSETC('AXIS_TYPE','DATE')
            CALL PSETC('AXIS_DATE_TYPE','DAYS')
            CALL PSETC('AXIS_DATE_MIN_VALUE',FDATEI)
            CALL PSETC('AXIS_DATE_MAX_VALUE',FDATEF)
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETC('AXIS_GRID','OFF')
            CALL PSETC('AXIS_TITLE','OFF')
            CALL PSETR('AXIS_TICK_INTERVAL',2.0)
c            CALL PSETC('AXIS_TICK_LABEL_QUALITY','HIGH')
            CALL PSETC('AXIS_DAYS_LABEL','NUMBER')
            
            CALL PSETC('AXIS_DAYS_LABEL_COLOUR','BLACK')
            CALL PSETC('AXIS_LINE','ON')
            
            CALL PRESET('AXIS_TITLE_TEXT')
            
            CALL PAXIS


C-------- PLOT VERTICAL AXIS ----------------------------
            
C##### Bar vertical axis 

            CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_TYPE','REGULAR')
            CALL PSETC('AXIS_POSITION','right')
            CALL PSETR('AXIS_MIN_VALUE',0.0)
            CALL PSETR('AXIS_MAX_VALUE',12.0)
            
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETR('AXIS_TICK_INTERVAL',1.0)
            CALL PSETC('AXIS_TICK_LABEL_COLOUR','black')
            
c            CALL PSETC('AXIS_TICK_LABEL_FORMAT','(I2)')
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',2)
            
            

			
            CALL PSETC('AXIS_LINE','ON')
           
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_TITLE_ORIENTATION','PARALLEL')
            
            CALL PSETC('AXIS_TITLE_TEXT','n. of tubes')
C---- GRIDDING LINES


            CALL PSETC('AXIS_GRID','ON')
            CALL PSETC('AXIS_GRID_COLOUR','BLACK')
            CALL PSETI('AXIS_GRID_THICKNESS',2)
            CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')
 
      		CALL PAXIS
	  	     
 	       CALL PSETC('GRAPH_TYPE','BAR')         
c 	       CALL PSETC('GRAPH_SHADE_COLOUR','GREY')
           CALL PSETC('GRAPH_BAR_COLOUR','GREY')
   	       CALL PSET1C('GRAPH_BAR_DATE_X_VALUES',XD,ND)
C    	   CALL PSET1R('GRAPH_BAR_Y_LOWER_VALUES', RLOW,ND)
    	   CALL PSET1R('GRAPH_BAR_Y_UPPER_VALUES', TUBES,ND)
           call psetc('LEGEND_USER_TEXT','tubes')
      	   CALL PGRAPH
           
           call pnew('subpage')
            
            
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_ORIENTATION','HORIZONTAL')
            CALL PSETC('AXIS_POSITION','BOTTOM')
            CALL PSETC('AXIS_TYPE','DATE')
            CALL PSETC('AXIS_DATE_TYPE','DAYS')
            CALL PSETC('AXIS_DATE_MIN_VALUE',FDATEI)
            CALL PSETC('AXIS_DATE_MAX_VALUE',FDATEF)
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETR('AXIS_TICK_INTERVAL',2.0)
c            CALL PSETC('AXIS_TICK_LABEL_QUALITY','HIGH')
            CALL PSETC('AXIS_DAYS_LABEL_COLOUR','BLACK')
            CALL PSETC('AXIS_DAYS_LABEL','NUMBER')
            CALL PSETC('AXIS_LINE','ON')
            CALL PSETC('AXIS_GRID','OFF')
            
            CALL PRESET('AXIS_TITLE_TEXT')
            CALL PAXIS

ccc# Curve
		    CALL PSETC('AXIS_ORIENTATION','VERTICAL')
            CALL PSETC('AXIS_POSITION','LEFT')
            CALL PSETR('AXIS_MIN_VALUE',0.0)
            CALL PSETR('AXIS_MAX_VALUE',180.0)
            CALL PSETC('AXIS_TYPE','REGULAR')
          
            CALL PSETC('AXIS_TICK','ON')
            CALL PSETR('AXIS_TICK_INTERVAL',10.0)
            
           
            CALL PSETI('AXIS_TICK_LABEL_FREQUENCY',2)
c            CALL PSETC('AXIS_TICK_LABEL_QUALITY','HIGH')
            CALL PSETC('AXIS_MINOR_TICK','OFF')
            CALL PSETC('AXIS_TITLE','ON')
            CALL PSETC('AXIS_TITLE_TEXT','metres')

C---- GRIDDING LINES

            CALL PSETC('AXIS_GRID','OFF')
            CALL PSETC('AXIS_GRID_COLOUR','BLACK')
            CALL PSETI('AXIS_GRID_THICKNESS',2)
            CALL PSETC('AXIS_GRID_LINE_STYLE','DOT')
			CALL PAXIS
			
			       	          	         	   
			
			CALL PSETC('GRAPH_LINE_STYLE','SOLID')
			CALL PSETC('GRAPH_LINE_COLOUR','BLUE')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',STDT,ND)
            call psetc('LEGEND_USER_TEXT','eps_std')
			CALL PGRAPH

			
			CALL PSETC('GRAPH_LINE_STYLE','SOLID')
			CALL PSETC('GRAPH_LINE_COLOUR','BROWN')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSETC('GRAPH_MISSING_DATA_MODE','IGNORE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',TCERR,ND)
			call psetc('LEGEND_USER_TEXT','control error')
			CALL PGRAPH
			
			
			CALL PSETC('GRAPH_LINE_STYLE','SOLID')
			CALL PSETC('GRAPH_LINE_COLOUR','RED')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',TSEAS,ND)
            call psetc('LEGEND_USER_TEXT','calib.std')

			CALL PGRAPH

			
			CALL PSETC('GRAPH_LINE_STYLE','DASH')
			CALL PSETC('GRAPH_LINE_COLOUR','MAGENTA')
			CALL PSETC('GRAPH_TYPE','CURVE')
			CALL PSET1C('GRAPH_CURVE_DATE_X_VALUES',XD,ND)
			CALL PSET1R('GRAPH_CURVE_Y_VALUES',TRAD,ND)
            call psetc('LEGEND_USER_TEXT','tubing rad')

			CALL PGRAPH
            call PTEXT
            
						
C*** CLOSE MAGICS

100       CALL PCLOSE

       END






