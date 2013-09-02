      PROGRAM TEXT02
*
*     THIS PROGRAM DEMONSTRATES MAGICS TEXT AND LEGEND FACILITIES
*     WHEN USED WITHIN METVIEW.
*
*     OPEN MAGICS
*
      DIMENSION JLIS(100)
      DIMENSION IILIS(100),IILEG(400),JLEG(400)
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','text02.ps')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/Text02')
      CALL PSETC ('METVIEW','ON')                   
*
*     Define Map Area and Projection
*     ------------------------------
      CALL PSETR ('SUBPAGE_X_LENGTH',27.6)              
      CALL PSETR ('SUBPAGE_Y_LENGTH',17.0)
      CALL PSETR ('SUBPAGE_X_POSITION',2.0)              
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',-20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',35.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',60.0)
      CALL PSETC('MAP_COASTLINE_COLOUR','BLACK')
      CALL PSETC('MAP_GRID_COLOUR','BLACK')
      CALL PSETC('TEXT_TITLES_TABLE_USE','ON')       
*
*     INITIALISE THE PACKAGES (This is Necessary)
*     -------------------------------------------
      CALL PSET1I('$METVIEW_TITLE_PACKAGE',IILIS,100)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE',IILEG,400)    
*
*     First Page - Contour Shading - Title - Legend
*     ---------------------------------------------
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PGRIB
*
*     Save the text package
*     ---------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',JLIS,100,N)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg1')
*
*     Plot Contours and Save the Legend Package
*     -----------------------------------------
      CALL TEXT02_ONE
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)      

      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)

      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg1')
*
*     Plot the Title and Legend
*
*     PMVTIT is the ACTION routine for TITLEs. PLEGND is the ACTION
*     routine for legends.  
*     It is important that the legend information as well as the
*     text information be passed to MAGICS before PMVTIT is called.
*     PMVTIT must be called before PLEGND
*     ------------------------------------------------------------
      CALL PMVTIT 
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')                   
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')         
      CALL PSETC('TEXT_BOX_BLANKING','ON')   
      CALL PSETR('TEXT_BOX_X_POSITION',2.0)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking etc')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)                    
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Second Page - LEGEND POSITIONING = OUTSIDE     
*     ------------------------------------------
      CALL PNEW('PAGE')
*
*     Plot Contours and Save the Legend Package
*     -----------------------------------------
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg2')
      CALL TEXT02_ONE
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)      
      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)

      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC('TEXT_LEGEND_BOX_BLANKING','OFF')
      CALL PSETC('LEGEND_POSITIONING','OUTSIDE')
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg2')
*
*     Plot the Title and Legend
*     -------------------------
      CALL PMVTIT 
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')                   
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')         
      CALL PSETC('TEXT_BOX_BLANKING','ON')   
      CALL PSETR('TEXT_BOX_X_POSITION',2.0)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'TEXT_LEGEND_BOX_BLANKING = OFF')
      CALL PSETC('TEXT_LINE_5',
     x       'LEGEND_POSITIONING = OUTSIDE')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)                    
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Third Page - LEGEND MODE = MAGICS             
*     ----------------------------------
      CALL PNEW('PAGE')
*
*     Plot Contours and Save the Legend Package
*     -----------------------------------------
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg3')
      CALL TEXT02_ONE
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)      
      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
    
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC('TEXT_LEGEND_MAGICS_STYLE','ON')
      CALL PSETC('TEXT_LEGEND_BOX_BLANKING','ON')
      CALL PSETC('LEGEND_POSITIONING','INSIDE')
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg3')
      CALL PMVTIT 
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')                   
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')         
      CALL PSETC('TEXT_BOX_BLANKING','ON')   
      CALL PSETR('TEXT_BOX_X_POSITION',2.0)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_5','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'TEXT_LEGEND_MAGICS_STYLE = ON')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)                    
      CALL PSETC ('METVIEW','ON')                   
      CALL PCLOSE
      END
      SUBROUTINE TEXT02_ONE 
*
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')
      CALL PSETC ('CONTOUR_SHADE','ON')     
      CALL PSETC ('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PSETC('LEGEND','ON')
*
*     Plot the Contours
*     -----------------
      CALL PCONT
      RETURN
      END
