      PROGRAM TEXT_GRIBTITLES

C     This program demonstrates extraction of data from the GRIB
C     header for the title.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('text_grib_titles_ex')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB
      

C     Define the contour     

      CALL PCONT


C     Set up and plot the title text

      CALL PSETC('TEXT_LINE_1', "Level: <grib_info key='level'/>")
      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST
      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
