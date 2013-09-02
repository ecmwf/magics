      PROGRAM NETCDF

C     ------------------------------------------------------
C     This program demonstrates Magics++ netcdf data reading
C     facilities. Relative Humidity at 850hPa is plotted.
C     ------------------------------------------------------


C TEMPORARY - REMOVE ALL NETCDF STUFF BECAUSE THE TEST
C SUITE IS TAKING FAR TOO LONG!
C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('ukmo_netcdf_02')



C     Area specification (SOUTH, WEST, NORTH, EAST )

c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -10.0)
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -40.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    70.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   50.0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

C     CALL PSETC ('NETCDF_FILENAME',                
C     + 'data/precip.HC.adehc.1960-1990.nc')
C      CALL PSETC ('NETCDF_TYPE',                    'GEOMATRIX') 
C      CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'rlat')
C      CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'rlon')
C      CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',     'precip')
Cc      CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     2.0)
Cc      CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',        40.0)
Cc     CALL PSETC ('NETCDF_Y_TRANSFORMATION',        'logarithmic')
C      CALL PNETCDF
C      
C
CC     Define the contour     
C
C      CALL PSETC ('CONTOUR_LABEL_COLOUR', 'RED')
C      CALL PSETC ('CONTOUR_HILO',         'OFF')
C      CALL PCONT
C

C     Set up and plot the title text

c      CALL PSETC ('TEXT_LINE_1', 'Relative Humidity at 850hPa')
c      CALL PSETC ('TEXT_LINE_2', 'Loaded from Netcdf file')
c      CALL PSETI ('TEXT_LINE_COUNT', 2)
c      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST
      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
