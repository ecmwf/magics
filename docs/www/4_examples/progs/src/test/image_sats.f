      PROGRAM IMAGE_BASIC

      PARAMETER (NUM_FILES=8)
      CHARACTER*30 PATHS(NUM_FILES)

      DATA PATHS
     X  / 'Met_7_WVB.image',
     X    'Met_5_52.image',
     X    'Goes_9_253.image',
     X    'Goes_10_254.image',
     X    'Goes_12_256.image',
c     X    'Met_9_VIS.image',
     X    'cptec.image',
C XXX temorarily remove cptec because it crashes m++.
     X    'Sim_Met_8_55.image',
     X    'Met_8_55.image'/
        

      INTEGER NUM_PAGES_PER_FILE(NUM_FILES)
      DATA NUM_PAGES_PER_FILE
     X  / 1, 2, 2, 2, 2, 1, 3, 7 /	


      CALL POPEN
      CALL PARSE_COMMAND_LINE ('image_sats')


C	Say that we'll be passing files to the grib handler

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')


C	Set up image colour table - black and white only

      CALL PSETC ('IMAGE_MIN_LEVEL_COLOUR','WHITE')
      CALL PSETC ('IMAGE_MAX_LEVEL_COLOUR','BLACK')

      CALL PSETC ('IMAGE_COLOUR_TABLE_CREATION_MODE','EQUIDISTANT')
      CALL PSETC ('IMAGE_LEGEND','ON')


C       Select the projection (must be done before calling PGRIB)

      CALL PSETC ('SUBPAGE_MAP_PROJECTION','SATELLITE')


C     Note that we could change the resolution by setting the next line
      CALL PSETI ('IMAGE_PIXEL_SELECTION_FREQUENCY', 10)



C     Plot the data from each GRIB file

      DO 100 I = 6,6

          CALL PSETC ('GRIB_INPUT_FILE_NAME', 
     x     '/usr/local/share/magics/data/' // PATHS(I))

          WRITE (*,*), 'File: ', PATHS(I)

C         Each file can contain multiple fields - plot them

          DO 200 IPAGE = 1,NUM_PAGES_PER_FILE(I)

C           Load the next field and plot the image

            CALL PGRIB

            CALL PIMAGE


C           Plot coastlines and title
	
            CALL PSETC ('SUBPAGE_FRAME',        'OFF')
            CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
            CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')
            CALL PCOAST
            CALL PTEXT

            CALL PNEW('PAGE')

200       CONTINUE

100   CONTINUE




      CALL PCLOSE
      END


#include "parse_command_line.h"
