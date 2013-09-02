      PROGRAM TEXT08
*
*     THIS PROGRAM DEMONSTRATES HOW METVIEW SHOULD USE MAGICS TO
*     PLOT TEXT AND LEGENDS.
*
      DIMENSION JLIS(100)
      DIMENSION IILIS(100),IILEG(400),JLEG(400)
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','text08.ps')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/Text08')
      CALL PSETC('METVIEW','ON')
      CALL PSETC('LEGEND','ON')
*
*     Define Map Area and Projection
*     ------------------------------
      CALL PSETC ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',-40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',95.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',35.0)
      CALL PSETC('MAP_COASTLINE_COLOUR','GREEN')
      CALL PSETC('MAP_GRID_COLOUR','GREEN')
*
*     Initialise the Title and Legend Packages
*     (This is necessary)
*     ----------------------------------------
      CALL PSET1I('$METVIEW_TITLE_PACKAGE',IILIS,100)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE',IILEG,400)    
*
*     First Page - Contour Shading - Title - Legend
*     ---------------------------------------------
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PGRIB
*
*     Collect First Text Package (Set by PGRIB)
*     ------------------------------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',JLIS,100,N)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE','leg1')
      CALL TEXT08_ONE
      CALL PCOAST
*
*     Collect First Legend Package
*     ----------------------------
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)      
*
*     Pass Text and Legend Information for Page 1
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_1','leg1')
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
      CALL PSETR('TEXT_BOX_X_POSITION',2.2)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking/Reduction etc')
      CALL PSETC('TEXT_LINE_5','')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
      CALL PSETC('LEGEND_ENTRY','OFF')
*
*     Second Page - Legend INSIDE
*     ---------------------------
      CALL PNEW('PAGE')
      CALL TEXT08_ONE
      CALL PCOAST
*
*     Pass Text and Legend Information for Page 2
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PSETC('LEGEND_POSITIONING','INSIDE')
      CALL PMVTIT 
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')
      CALL PSETC('TEXT_BOX_BLANKING','ON')
      CALL PSETR('TEXT_BOX_X_POSITION',2.2)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'one Field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'LEGEND_POSITIONING = INSIDE')
      CALL PSETC('TEXT_LINE_5','')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Third Page - Reduction =1 Blanking OFF
*     --------------------------------------
      CALL PNEW('PAGE')
      CALL PCOAST
      CALL TEXT08_ONE
*
*     Pass Text and Legend Information for Page 3
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
      CALL PSETI('TEXT_REDUCTION_LEVEL',1)
      CALL PSETC('TEXT_LEGEND_BOX_BLANKING','OFF')
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PMVTIT 
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')
      CALL PSETC('TEXT_BOX_BLANKING','ON')
      CALL PSETR('TEXT_BOX_X_POSITION',2.2)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',7)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'LEGEND_POSITIONING = INSIDE')
      CALL PSETC('TEXT_LINE_5',
     x       'TEXT_LEGEND_BOX_BLANKING = OFF')
      CALL PSETC('TEXT_LINE_6',
     x       'TEXT_REDUCTION_LEVEL = 1')
      CALL PSETC('TEXT_LINE_7','')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Fourth Page - LEGEND_MODE = MAGICS
*     --------------------------------------
      CALL PNEW('PAGE')
      CALL PCOAST
      CALL TEXT08_ONE
*
*     Pass Text and Legend Information for Page 4
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
      CALL PSETC('TEXT_LEGEND_MAGICS_STYLE','ON')
      CALL PSETI('TEXT_REDUCTION_LEVEL',2)
      CALL PSETC('LEGEND_POSITIONING','AUTOMATIC')
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PMVTIT 
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')
      CALL PSETC('TEXT_BOX_BLANKING','ON')
      CALL PSETR('TEXT_BOX_X_POSITION',2.2)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',6)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'TEXT_LEGEND_MAGICS_STYLE= ON')
      CALL PSETC('TEXT_LINE_5',
     x       'TEXT_REDUCTION_LEVEL = 2')
      CALL PSETC('TEXT_LINE_6','')
      CALL PTEXT
      CALL PCLOSE
      END
      SUBROUTINE TEXT08_ONE
*
*     Draw Shaded Contours
*     --------------------
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')     
      CALL PSETC ('CONTOUR_SHADE','ON')
      CALL PSETC ('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PCONT
      RETURN
      END
