C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM DATA03
      CHARACTER*6 ACT
C
C     DATA ON FILE IS A MARS TARGET FILE CONTAINING 
C     (1) 500 HPA STRETCHED/ROTATED FIELD ON LAT/LONG GRID
C     (2) 500 HPA STRETCHED/ROTATED FIELD IN SPHERICAL HARMONICS
C
C
C     OPEN MAGICS               
C
      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
c     CALL PSETR ('PS_SCALE',0.76)
      CALL PSETC ('PS_FILE_NAME','data03.ps')
      CALL PSETC('PAGE_ID_LINE_USER_TEXT','TESTS/DATA03')
C
C     SET UP MAPPING PARAMETERS
C
      CALL PSETC('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')
      CALL PSETC('SUBPAGE_MAP_AREA_DEFINITION','CENTRE')
      CALL PSETR('SUBPAGE_MAP_CENTRE_LATITUDE',55.0)
      CALL PSETR('SUBPAGE_MAP_CENTRE_LONGITUDE',-20.0)
      CALL PSETR('SUBPAGE_MAP_SCALE',35.E6)
      CALL PSETC('MAP_COASTLINE_COLOUR','GREEN')        
      CALL PSETC('MAP_GRID_COLOUR','GREEN')        
C
C     PLOT THE FIELDS - 1 FIELD PER PAGE
C
      CALL PSETC('GRIB_INPUT_TYPE','FILE')
      DO 1000 I = 1,2
C
C       THE INPUT FILE IS TO BE READ SEQUENTIALLY BY PGRIB
C
        IF(I.EQ.1)THEN
        CALL PSETC('GRIB_INPUT_FILE_NAME',
     x             'data/oldts/z500.rot_latlon')
        ELSE
        CALL PSETC('GRIB_INPUT_FILE_NAME',
     x             'data/oldts/z500.rot_spectral')
        ENDIF
        CALL PGRIB
C
C       CHECK PGRIB ACTION CODE; IF NO ERRORS THEN PLOT FIELD
C       HERE USER MAY RESET ANY MAGICS PARAMETERS THAT ARE SET
C       BY PGRIB - I.E. CONTOUR, WIND AND TEXT PARAMETERS
C
        IF(I.EQ.1)THEN
        CALL PSETC('TEXT_LINE_2','500z Stretched LAT/LON Field')
        ELSE
        CALL PSETC('TEXT_LINE_2','500z Stretched SPECTRAL Field')
        ENDIF
        CALL PSETC('TEXT_JUSTIFICATION','CENTRE')
        CALL PSETI('TEXT_LINE_COUNT',2)
        CALL PENQC('GRIB_ACTION',ACT)
        IF (ACT.EQ.'PCONT') THEN
            CALL PCOAST
            CALL PCONT
            CALL PTEXT
            CALL PNEW('PAGE')
        ELSE IF (ACT.EQ.'ERROR') THEN
            CALL PENQI('GRIB_ERROR_CODE',IERR)
            WRITE (*,*) ' ERROR IN PGRIB = ',IERR
        ENDIF
1000  CONTINUE
      CALL PCLOSE
      END
