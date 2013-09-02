      PROGRAM TEXT04
*
*     THIS PROGRAM DEMONSTRATES MAGICS TEXT AND LEGEND FACILITIES
*     WHEN USED WITHIN METVIEW.
*
      DIMENSION ILIS(100),JLIS(100),KLIS(100),IORD(3)
      DIMENSION IILIS(100),IILEG(400),JLEG(400),KLEG(400)
      DATA IORD/3,1,2/
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','text04.ps')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/Text04')
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('LEGEND','ON')
*
*     Define Map Area and Projection
*     ------------------------------
      CALL PSETC ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',-40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',95.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',35.0)
      CALL PSETC('MAP_COASTLINE_COLOUR','BLACK')
      CALL PSETC('MAP_GRID_COLOUR','BLACK')
      CALL PSETC('TEXT_TITLES_TABLE_USE','ON')
*
*     Initialise the Title and Legend Packages
*     (This is necessary)
*     ----------------------------------------
      CALL PSET1I('$METVIEW_TITLE_PACKAGE',IILIS,100)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE',IILEG,400)    
*
*     First Page - Contour Shading - Title - Legend
*     ---------------------------------------------
      CALL PCOAST
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z_AN.grb')
      CALL PGRIB
*
*     Collect First Text Package (Set by PGRIB)
*     ------------------------------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',ILIS,100,N)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg1')
      CALL TEXT04_CONTX
*
*     Collect First Legend Package
*     ----------------------------
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)      
*
*     Pass Text and Legend Information for Page 1
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PSETC('TEXT_LEGEND_BOX_BLANKING','ON')  
      CALL PSETC('TEXT_JUSTIFICATION','CENTRE')
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
      CALL PSETR('TEXT_BOX_X_POSITION',2.25)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.05)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend PLotting')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking/Reduction etc')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Second Page - Contour Shading - Title - Legend
*     ----------------------------------------------
      CALL PNEW('PAGE')
      CALL PCOAST
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PGRIB
*
*     Collect Second Text Package
*     ---------------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',JLIS,100,N)
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR','RED')
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg2')
      CALL TEXT04_CONTX
*
*     Collect Second Legend Package
*     ----------------------------
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',KLEG,400,N)      
*
*     Pass Text and Legend Information for Page 2
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',KLEG,400)
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
      CALL PSETR('TEXT_BOX_X_POSITION',2.25)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.05)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend PLotting')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Contour and Highlights coloured RED')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)                    
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Third Page - Observations - Title
*     ---------------------------------
      CALL PNEW('PAGE')
      CALL PCOAST
      CALL PSETC ('OBS_INPUT_FILE_NAME','data/oldts/obs990506.bfr')
      CALL POBS
*
*     Collect Second Text Package (From POBS)
*     ----------------------------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',KLIS,100,N)
*
*     Pass Text Information for Page 3 (No Legend for Obs Plotting)
*     --------------------------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',KLIS,100)
*
*     Plot the Title
*     --------------
      CALL PMVTIT 
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')                   
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')         
      CALL PSETC('TEXT_BOX_BLANKING','ON')   
      CALL PSETR('TEXT_BOX_X_POSITION',2.25)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.05)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend Plotting')
      CALL PSETC('TEXT_LINE_3',
     x       'Upper Air Observations with Automatic Text ')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking/Reduction etc')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)                    
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Fourth Page - Obs + Title for First 3 Pages + 2 Legends
*     -------------------------------------------------------
      CALL PNEW('PAGE')
      CALL PCOAST
*
*     Firstly - Plot the Fields and Observations
*     ------------------------------------------
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z_AN.grb')
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR','BLUE')
      CALL PSETC('LEGEND_ENTRY','OFF')          
      CALL TEXT04_CONTX
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PSETC ('OBS_INPUT_FILE_NAME','data/oldts/obs990506.bfr')
      CALL POBS
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR','RED')
      CALL PSETC('LEGEND_ENTRY','OFF')          
      CALL TEXT04_CONTX
*
*     Pass Text and Legend Information for Page 4
*     3 Text Packages from Pages 1,2 and 3
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',3)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_2',JLIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_3',KLIS,100)
*
*     Two Legend Packages form Pages 1,2
*     ----------------------------------
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',2)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_2',KLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_2','leg2')
      CALL PSETC('TEXT_ORDER_MODE','USER')
      CALL PSET1I('TEXT_ORDER',IORD,3)
*
*     Plot the Title and Legend
*     -------------------------
      CALL PMVTIT
      CALL PLEGND
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')         
      CALL PSETC('TEXT_BOX_BLANKING','ON')   
      CALL PSETR('TEXT_BOX_X_POSITION',2.25)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.05)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETC ('METVIEW','OFF')                   
      CALL PSETI('TEXT_LINE_COUNT',5)                    
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend Plotting')
      CALL PSETC('TEXT_LINE_3',
     x       'Fields and Observations ')
      CALL PSETC('TEXT_LINE_4',
     x       'TEXT Lines USER Ordered - Observations Text First')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')         
      CALL PSETI('TEXT_LINE_COUNT',1)                    
      CALL PSETC ('METVIEW','ON')                   
      CALL PSETC('TEXT_MODE','TITLE')
      CALL PCLOSE
      END
      SUBROUTINE TEXT04_CONTX
*
*     Draw Shaded Contours
*     --------------------
      CALL PSETC ('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PSETC ('CONTOUR_HILO_QUALITY','MEDIUM')
      CALL PSETC ('CONTOUR_LABEL_QUALITY','MEDIUM')
      CALL PSETC('LEGEND','ON')
      CALL PCONT
      RETURN
      END
