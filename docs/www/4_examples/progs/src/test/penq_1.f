      PROGRAM PENQ1

C     This program is intended to test the PENQ functions.


      CHARACTER*90  TITLE1
      CHARACTER*90  TITLE2
      CHARACTER*32  COL
      REAL          LOWLEFTLAT      
      INTEGER       LABFREQ


      LABFREQ=23
      LOWLEFTLAT=15.16


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('penq_1')



C     Area specification

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    10.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    0.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   40.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  60.0)


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',     'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/rh850.grib')
      CALL PGRIB
      

C     Define contour parameters

      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',   1)
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'ORANGE')


c     Get some values back and put them into the title

      CALL PENQR ('SUBPAGE_LOWER_LEFT_LATITUDE', LOWLEFTLAT)
      CALL PENQI ('CONTOUR_LABEL_FREQUENCY',     LABFREQ)
      CALL PENQC ('CONTOUR_LINE_COLOUR',         COL)

      WRITE (TITLE1,'(A,F5.2,A,I)')
     + 'SUBPAGE_LOWER_LEFT_LATITUDE (10.0?): ',
     + LOWLEFTLAT, ' ; CONTOUR_LABEL_FREQUENCY (1?): ', LABFREQ

      WRITE (TITLE2,'(A,,A)') 'CONTOUR_LINE_COLOUR (ORANGE?): ', COL



C     Set up the title text

      CALL PSETI ('TEXT_LINE_COUNT',2)
      CALL PSETC ('TEXT_LINE_1', TITLE1)
      CALL PSETC ('TEXT_LINE_2', TITLE2)
      CALL PTEXT



C     Close

      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"
