      PROGRAM TEXT03
*
*
*     THIS PROGRAM DEMONSTRATES MAGICS TEXT AND LEGEND FACILITIES
*     WHEN USED WITHIN METVIEW.
*
*     OPEN MAGICS
*
      DIMENSION ILIS(100),JLIS(100),KLIS(100)
      DIMENSION IILIS(100),ILEG(400),JLEG(400),KLEG(400),LLEG(400)
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','text03.ps')
      CALL PSETC ('METVIEW','ON')
*
*     Define Map Area and Projection
*     ------------------------------
      CALL PSETC ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',-40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',95.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',35.0)
      CALL PSETR ('SUBPAGE_X_LENGTH',27.6)
      CALL PSETR ('SUBPAGE_X_POSITION',2.0)
      CALL PSETC('MAP_COASTLINE_COLOUR','BLACK')
      CALL PSETC('MAP_GRID_COLOUR','BLACK')
      CALL PSETC('TEXT_TITLES_TABLE_USE','ON')
*
*     DEFINE TITLE AND LEGEND PACKAGES
*     --------------------------------
      CALL PSET1I('$METVIEW_TITLE_PACKAGE',IILIS,100)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE',ILEG,400)
*
*     First Page - Contour Shading - Title - Legend
*     ---------------------------------------------
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
*
*     Save the text package
*     ---------------------
      CALL PGRIB
*
*     Retrieve TITLE TEXT for FIELD (Does fall all four pages)
*
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',ILIS,100,N)
      CALL PSETC('LEGEND','ON')
*
*     Plot Contours and Save the Legend Package
*     -----------------------------------------
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg1')
      CALL TEXT03_ONE
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',JLEG,400,N)
      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)

      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg1')
      CALL PMVTIT
      CALL PLEGND
*
*     Descriptive text (Nothing to do with Metview)
*
      CALL PSETC('TEXT_MODE','POSITIONAL')
      CALL PSETC('TEXT_BORDER','ON')
      CALL PSETC('TEXT_BOX_BLANKING','ON')
      CALL PSETR('TEXT_BOX_X_POSITION',2.0)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETC ('METVIEW','OFF')
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'One field with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Selected Contour Shading with DOTS')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Second Page - Contour Shading - Title - Legend
*     ----------------------------------------------
      CALL PNEW('PAGE')
*
*     Plot Contours and Save the Legend Package
*     -----------------------------------------
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg2')       
      CALL TEXT03_TWO
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',KLEG,400,I)      
      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)

      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',KLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg2')       
      CALL PMVTIT 
      CALL PLEGND
*
*     Descriptive text (Nothing to do with Metview)
*     ---------------------------------------------
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
     x       'Selected Contour Shading with AREA_FILL')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Third Page  - Contour Shading - Title - Legend
*     ----------------------------------------------
      CALL PNEW('PAGE')
*
*     Plot Contours and Save the Legend Package
*     -----------------------------------------
      CALL PSETC('METVIEW_LEGEND_STORE_FILE','leg3')       
      CALL TEXT03_THREE
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',LLEG,400,N)      
      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
 
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',1)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',LLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg3')       
      CALL PMVTIT 
      CALL PLEGND
*
*     Descriptive text (Nothing to do with Metview)
*     ---------------------------------------------
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
     x       'Selected Contour Shading with HATCHING')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Fourth Page - Re-drop all Three Shadings
*     ----------------------------------------
      CALL PNEW('PAGE')
      CALL PSETC('LEGEND_ENTRY','OFF')
      CALL TEXT03_ONE
      CALL TEXT03_TWO
      CALL TEXT03_THREE
      CALL PCOAST
*
*     Pass the Title and Legend Packages to MAGICS
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
   
      CALL PSETI('$METVIEW_LEGEND_PACKAGE_COUNT',3)        
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_1',JLEG,400)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_2',KLEG,400)
      CALL PSET1I('$METVIEW_LEGEND_PACKAGE_3',LLEG,400)
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_1','leg1')       
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_2','leg2')       
      CALL PSETC('METVIEW_LEGEND_STORE_FILE_3','leg3')       
      CALL PMVTIT
      CALL PLEGND
*
*     Descriptive text (Nothing to do with Metview)
*     ---------------------------------------------
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
     x       '3 Previously Selected Contour Shading Plots')
      CALL PSETC('TEXT_LINE_4',
     x       'with MERGED Legend')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
      CALL PCLOSE
      END
      SUBROUTINE TEXT03_ONE
C
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')
      CALL PSETC ('CONTOUR_SHADE','ON')     
      CALL PSETR ('CONTOUR_MIN_LEVEL',512.0)
      CALL PSETR ('CONTOUR_MAX_LEVEL',536.0)
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',512.0)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',536.0)
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR','RED')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR','YELLOW')
      CALL PSETC ('CONTOUR_SHADE_METHOD','DOT')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL_DENSITY',50.0)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL_DENSITY',50.0)
      CALL PSETR ('CONTOUR_SHADE_DOT_SIZE',0.04)          
      CALL PCONT
      RETURN
      END
      SUBROUTINE TEXT03_TWO
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')
      CALL PSETC ('CONTOUR_SHADE','ON')     
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',536.0)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',560.0)
      CALL PSETR ('CONTOUR_MIN_LEVEL',536.0)
      CALL PSETR ('CONTOUR_MAX_LEVEL',560.0)
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR','YELLOWISH_GREEN')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR','CYAN')
      CALL PSETC ('CONTOUR_SHADE_METHOD','AREA_FILL')
      CALL PCONT
      RETURN
      END
      SUBROUTINE TEXT03_THREE 
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')
      CALL PSETC ('CONTOUR_SHADE','ON')     
      CALL PSETC ('CONTOUR_SHADE_METHOD','HATCH')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',560.0)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',592.0)
      CALL PSETR ('CONTOUR_MIN_LEVEL',560.0)
      CALL PSETR ('CONTOUR_MAX_LEVEL',592.0)
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR','BLUISH_GREEN')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR','BLUE')
      CALL PCONT
      RETURN
      END
