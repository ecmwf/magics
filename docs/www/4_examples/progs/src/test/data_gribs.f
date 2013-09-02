      PROGRAM DATAGRIBS

C     Tests GRIB files from various sources, paying particular
C     attention to the automatic titles

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_gribs')


C     Set up the coastline attributes and plot

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     
      CALL PCOAST


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/EPS_27.grib')
      CALL PGRIB
      

C     Draw the contour     

      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT


C     Set up and plot a text box describing the data

      CALL PSETC ('TEXT_MODE',           'POSITIONAL')
      CALL PSETC ('TEXT_BOX_BLANKING',   'ON')
      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1', 'Ensemble Mean')
      CALL PSETC ('TEXT_LINE_2', 'Uses GRIB extension 27')
      CALL PTEXT


C     --------- Potential Temperature from System 3 ------------

      CALL PNEW ('SUPER_PAGE')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            'data/pot_temp_system3.grib')
      CALL PGRIB
      

C     Draw the contour     

      CALL PCONT


C     Set up and plot the title text

      CALL PRESET ('TEXT_MODE')
      CALL PRESET ('TEXT_BOX_BLANKING')
      CALL PRESET ('TEXT_LINE_COUNT')
      CALL PRESET ('TEXT_LINE_1')
      CALL PRESET ('TEXT_LINE_2')
      CALL PTEXT


C     Set up and plot a text box describing the data

      CALL PSETC ('TEXT_MODE',           'POSITIONAL')
      CALL PSETC ('TEXT_BOX_BLANKING',   'ON')
      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1', 'Potential Temperature')
      CALL PSETC ('TEXT_LINE_2', '[lev type: pot vor ]From System 3')
      CALL PTEXT



C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
