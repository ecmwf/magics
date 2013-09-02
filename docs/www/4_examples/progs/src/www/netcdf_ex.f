      PROGRAM NETCDF

C     ------------------------------------------------------
C     This program demonstrates Magics++ netcdf data reading
C     facilities. Relative Humidity at 850hPa is plotted.
C     ------------------------------------------------------


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('netcdf_ex')



C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',      5.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -25.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    35.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   25.0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('NETCDF_FILENAME',                'data/rh850.nc')
      CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',     'r')
      CALL PSETC ('NETCDF_TYPE',                    'GEOMATRIX') 
      CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'latitude')
      CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'longitude')
      CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     2.0)
      CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',        40.0)
      CALL PNETCDF
      

C     Define the contour     

      CALL PSETC ('CONTOUR_LABEL_COLOUR', 'CHARCOAL')
      CALL PCONT


C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1', 'Relative Humidity at 850hPa')
      CALL PSETC ('TEXT_LINE_2', 'Loaded from Netcdf file')
      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST
      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
