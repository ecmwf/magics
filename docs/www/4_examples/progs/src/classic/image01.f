C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

	PROGRAM IMAG01
C
C	PLOT IMAGES IN THE SAME PROJECTION AS THE INPUT IMAGE
C
	CHARACTER*30 PATH(7)

	DATA PATH / 'data/oldts/Met_5_FIB.image','data/oldts/Met_7_WVB.image',
     X   'data/oldts/Goes_9_FVB.image','data/oldts/Goes_12_FIB.image',
     X   'data/oldts/cptec.image','gribsim_00_6_WV.grb',
     X   'sat3.grb'/

	CALL POPEN
C
C	PASS GRIB FILE WITH IMAGE
C
	CALL PSETC ('PS_DEVICE','ps_a4')
	CALL PSETC ('PS_FILE_NAME','image01.ps')
	CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Tests/image01')
	CALL PSETC ('GRIB_INPUT_TYPE','FILE')

	DO 100 I = 1,6

	   CALL PSETC ('GRIB_INPUT_FILE_NAME',PATH(I))
C
C	   SELECT THE PROJECTION (MUST BE DONE BEFORE CALLING PGRIB)
C
	   CALL PSETC ('SUBPAGE_MAP_PROJECTION','SATELLITE')
	   CALL PGRIB
C
C	SET UP IMAGE COLOUR TABLE - BLACK AND WHITE ONLY
C
	   CALL PSETC ('IMAGE_MIN_LEVEL_COLOUR','WHITE')
	   CALL PSETC ('IMAGE_MAX_LEVEL_COLOUR','BLACK')

	   CALL PSETC ('IMAGE_COLOUR_TABLE_CREATION_MODE','EQUIDISTANT')
       CALL PSETC ('IMAGE_LEGEND','ON')

C      NOTE THAT WE COULD INCREASE THE RESOLUTION BY SETTING THE NEXT LINE
C	   CALL PSETI ('IMAGE_PIXEL_SELECTION_FREQUENCY',20)

C	   PLOT THE IMAGE
C
	   CALL PIMAGE
C
C	   PLOT COASTLINES AND TITLE
C	
	   CALL PSETC ('SUBPAGE_FRAME','OFF')
	   CALL PSETC ('MAP_COASTLINE_COLOUR','BLACK')
	   CALL PSETC ('MAP_GRID_COLOUR','BLACK')
	   CALL PCOAST

       CALL PTEXT

       CALL PNEW('PAGE')

100	CONTINUE
C
C	CLOSE MAGICS
C
	CALL PCLOSE

	END

