      PROGRAM TEXT06
*
*     THIS PROGRAM DEMONSTRATES HOW METVIEW SHOULD USE MAGICS TO  
*     PLOT TEXT AND LEGENDS.
*
      DIMENSION ILIS(100),JLIS(100)
      DIMENSION IILIS(100),IILEG(400),JLEG(400),KLEG(400)
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','text06.ps')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/Text06')
      CALL PSETC('METVIEW','ON')
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
      CALL PSETC('TEXT_JUSTIFICATION','LEFT')
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
      CALL PSETC ('LEGEND_TEXT_COMPOSITION','USER_TEXT_ONLY')
      CALL PSETC ('LEGEND_USER_TEXT','Analysis')
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE','leg1')
      CALL TEXT06_CONTX
*
*     Collect First Legend Package
*     ----------------------------
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)      
*
*     Pass Text and Legend Information for Page 1
*     Text is set by User
*     --------------------------------------------
      CALL PSETC('TEXT_ORIGIN','USER')   
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC('TEXT_LINE_1',
     X  '!level! !param! !title! for !an_date!')
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
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
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
      CALL PSETR('TEXT_BOX_X_LENGTH',15.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.0)
      CALL PSETI('TEXT_LINE_COUNT',4)
      CALL PSETC('TEXT_LINE_1','Plotting Text in USER Mode')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','TEXT_ORIGIN = USER')
      CALL PSETC('TEXT_LINE_3',
     x       'TEXT_LINE_1 = " !level! !param! !title! for !an_date! "')
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
*     Collect Second Text Package (Set by PGRIB)
*     ------------------------------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',JLIS,100,N)
      CALL PSETC ('LEGEND_TEXT_COLOUR','RED')
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR','RED')
      CALL PSETC ('LEGEND_TEXT_COMPOSITION','USER_TEXT_ONLY')
      CALL PSETC ('LEGEND_USER_TEXT','Forecast')
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE','leg2')
      CALL TEXT06_CONTX
*
*     Collect Second Legend Package
*     -----------------------------
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',KLEG,400,N)      
*
*     Pass Text and Legend Information for Page 2
*     --------------------------------------------
      CALL PSETC('TEXT_ORIGIN','USER')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC('TEXT_LINE_1',
     X'!level! !param! t+!fc_step! !title! from !an_date! - '//
     X'valid !fc_date!')
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',JLIS,100)
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',KLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_1','leg2')
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
      CALL PSETR('TEXT_BOX_X_LENGTH',15.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.0)
      CALL PSETI('TEXT_LINE_COUNT',4)
      CALL PSETC('TEXT_LINE_1','Plotting Text in USER Mode')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','TEXT_ORIGIN = USER')
      CALL PSETC('TEXT_LINE_3',
     x       'TEXT_LINE_1 = " !level! !param! t+!fc_step! '//
     X'!title! from !an_date! - valid !fc_date! "')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Third Page -  Title for First 2 Pages + 2 Legends
*     -------------------------------------------------------
      CALL PNEW('PAGE')
      CALL PSETC('LEGEND_ENTRY','OFF')
      CALL PCOAST
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z_AN.grb')
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR','BLUE')
      CALL TEXT06_CONTX
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL TEXT06_CONTX
*
*     Pass Text and Legend Information for Page 3
*     2 Text Packages from Pages 1 and 2
*     Text is set by User
*     --------------------------------------------
      CALL PSETC('TEXT_ORIGIN','USER')
      CALL PSETI('TEXT_LINE_COUNT',2)
      CALL PSETC('TEXT_LINE_1',
     X  '!level! !param.d1.r1! !title.d1! from !an_date.d1!')
      CALL PSETC('TEXT_LINE_2',
     X'!level.d2.r2!!param.d2.r2! t+!fc_step.d2! !title.d2!'//
     X ' from !an_date.d2! - valid !fc_date.d2!')
      CALL PSETC('TEXT_JUSTIFICATION','LEFT')

      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',2)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_2',JLIS,100)
*
*     Two Legend Packages from Pages 1,2
*     ----------------------------------
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',2)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_2',KLEG,400)
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE_2','leg2')
      CALL PMVTIT
      CALL PLEGND
*
*     Plot Explanatory Text (Not Metview)
*     -----------------------------------
      CALL PSETC ('METVIEW','OFF')
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')
      CALL PSETC('TEXT_JUSTIFICATION','CENTRE')
      CALL PSETC('TEXT_BOX_BLANKING','ON')
      CALL PSETR('TEXT_BOX_X_POSITION',2.2)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',15.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.0)
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_1','PLotting Text in USER Mode')
      CALL PSETC('TEXT_LINE_5','')
      CALL PSETC('TEXT_LINE_2','TEXT_ORIGIN = USER')
      CALL PSETC('TEXT_LINE_3',
     X  'TEXT_LINE_1 = " !level! !title.data1! from !an_date.d1! "')
      CALL PSETC('TEXT_LINE_4',
     X'TEXT_LINE_2 = " !level.d2.r2!!param.d2.r2! t+!fc_step!'//
     X ' !title.d2! from !an_date.d2! - valid !fc_date.d2! "')
      CALL PTEXT
      CALL PCLOSE
      END
      SUBROUTINE TEXT06_CONTX
*
*     DRAW SHADED CONTOURS
*     --------------------
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')     
      CALL PSETC('LEGEND','ON')
      CALL PCONT
      RETURN
      END
