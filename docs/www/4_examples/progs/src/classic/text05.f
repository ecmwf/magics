C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM TEXT05
*
*     THIS PROGRAM DEMONSTRATES HOW METVIEW SHOULD USE MAGICS TO  
*     PLOT TEXT AND LEGENDS.
*
      DIMENSION ILIS(100),JLIS(100),KLIS(100),IORD(3)
      DIMENSION IILIS(100),IILEG(400),JLEG(400),KLEG(400)
      DATA IORD /3,1,2/
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','text05.ps')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/Text05')
*
*     Define Map Area and Projection
*     ------------------------------
      CALL PSETC('METVIEW','ON')
      CALL PSETC ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',-40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',95.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',35.0)
      CALL PSETC('MAP_COASTLINE_COLOUR','GREEN')
      CALL PSETC('MAP_GRID_COLOUR','GREEN')
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
      CALL PSETC ('LEGEND_TEXT_COMPOSITION','USER_TEXT_ONLY')
      CALL PSETC ('LEGEND_USER_TEXT','Analysis')
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE','leg1')
      CALL TEXT05_CONTX
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
      CALL PSETC('TEXT_LEGEND_BOX_BLANKING','ON')
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
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'Analysis with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking etc')
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
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL PSETC ('LEGEND_TEXT_COMPOSITION','USER_TEXT_ONLY')
      CALL PSETC ('LEGEND_USER_TEXT','Forecast')
      CALL PSETC ('METVIEW_LEGEND_STORE_FILE','leg2')
      CALL TEXT05_CONTX
*
*     Collect Second Legend Package
*     -----------------------------
      CALL PENQ1I('$METVIEW_LEGEND_PACKAGE',KLEG,400,N)      
*
*     Pass Text and Legend Information for Page 2
*     --------------------------------------------
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
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'Forecast with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking etc')
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
*     Collect Third Text Package (Set by POBS)
*     ----------------------------------------
      CALL PENQ1I('$METVIEW_TITLE_PACKAGE',KLIS,100,N)
*
*     Pass Text nformation for Page 3 (No Legend for OBS Plotting)
*     ------------------------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',1)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',KLIS,100)
      CALL PMVTIT 
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
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'Observations with Automatic Text (No Legend)')
      CALL PSETC('TEXT_LINE_4',
     x       'Default Values for Position/Blanking etc')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Fourth Page - Obs + Title for First 3 Pages + 2 Legends
*     -------------------------------------------------------
      CALL PNEW('PAGE')
      CALL PSETC('LEGEND_ENTRY','OFF')
      CALL PCOAST
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z_AN.grb')
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','BLUE')
      CALL TEXT05_CONTX
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb')
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL TEXT05_CONTX
      CALL PSETC ('OBS_INPUT_FILE_NAME','data/oldts/obs990506.bfr')
      CALL POBS
*
*     Pass Text and Legend Information for Page 4
*     3 Text Packages from Pages 1,2 and 3
*     NOTE: Input order of Text Packages is 3,1,2
*     Automatic Ordering by MAGICS gives 1,2,3
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',3)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_3',KLIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_2',JLIS,100)
      CALL PSETC('TEXT_JUSTIFICATION','LEFT')
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
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'An/Fc/Obs with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Title TEXT Sorted by MAGICS (Default)')
      CALL PTEXT
      CALL PSETC('TEXT_BORDER','OFF')
      CALL PSETI('TEXT_LINE_COUNT',1)
      CALL PSETC ('METVIEW','ON')
      CALL PSETC('TEXT_MODE','TITLE')
*
*     Fifth Page - Obs + Title for First 3 Pages + 2 Legends
*     Specify Order of Title Text 
*     -------------------------------------------------------
      CALL PNEW('PAGE')
      CALL PSETC('LEGEND_ENTRY','OFF')
      CALL PCOAST
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z_AN.grb')
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','BLUE')
      CALL TEXT05_CONTX
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/oldts/500Z.grb') 
      CALL PGRIB
      CALL PSETC ('CONTOUR_LINE_COLOUR','RED')
      CALL TEXT05_CONTX
      CALL PSETC ('OBS_INPUT_FILE_NAME','data/oldts/obs990506.bfr')
      CALL POBS
*
*     Pass Text and Legend Information for Page 5
*     3 Text Packages from Pages 1,2 and 3
*     NOTE: Input order of Text Packages is 1,2,3
*     Speific Ordering by Metview gives 1,2,3
*     --------------------------------------------
      CALL PSETI('$METVIEW_TITLE_PACKAGE_COUNT',3)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_1',ILIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_2',JLIS,100)
      CALL PSET1I('$METVIEW_TITLE_PACKAGE_3',KLIS,100)
      CALL PSETC('TEXT_JUSTIFICATION','LEFT')
      CALL PSETC('TEXT_ORDER_MODE','USER')
      CALL PSET1I('TEXT_ORDER',IORD,3)         
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
      CALL PSETC('TEXT_JUSTIFICATION','CENTRE')
      CALL PSETC('TEXT_BORDER','ON')
      CALL PSETC('TEXT_BOX_BLANKING','ON')
      CALL PSETR('TEXT_BOX_X_POSITION',2.2)
      CALL PSETR('TEXT_BOX_Y_POSITION',1.0)
      CALL PSETR('TEXT_BOX_X_LENGTH',10.0)
      CALL PSETR('TEXT_BOX_Y_LENGTH',3.5)
      CALL PSETI('TEXT_LINE_COUNT',5)
      CALL PSETC('TEXT_LINE_1','')
      CALL PSETC('TEXT_LINE_4','')
      CALL PSETC('TEXT_LINE_2','Metview Text and Legend')
      CALL PSETC('TEXT_LINE_3',
     x       'An/Fc/Obs with Automatic Text and Legend')
      CALL PSETC('TEXT_LINE_4',
     x       'Title TEXT Sorted by Metview  (Ordered 3-1-2)')
      CALL PTEXT

      CALL PCLOSE
      END
      SUBROUTINE TEXT05_CONTX
*
*     DRAW SHADED CONTOURS
*     --------------------
      CALL PSETC ('CONTOUR_HIGHLIGHT','OFF')     
      CALL PSETC('LEGEND','ON')
      CALL PCONT
      RETURN
      END
