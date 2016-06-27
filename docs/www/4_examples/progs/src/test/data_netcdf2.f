C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM NETCDF

C     ------------------------------------------------------
C     This program demonstrates Magics++ netcdf data reading
C     facilities. Relative Humidity at 850hPa is plotted.
C     ------------------------------------------------------


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_netcdf2')



C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('NETCDF_FILENAME',
     x            'data/MOZART3_vmr3_2_2003_12_30.nc')


C     Plot the data for several variables over various areas
C                        left, right, top,  bottom, new page
      CALL PLOT_NETCDF (-180.0, 180.0,  90.0, -90.0, 0)
      CALL PLOT_NETCDF ( -40.0,  50.0,  70.0, -10.0, 1)


      CALL PCLOSE

      STOP
      END



      SUBROUTINE PLOT_NETCDF (RLEFT, RIGHT, RTOP, RBOT, INEW)

C       Optionally start a new page

        IF (INEW == 1) THEN
          CALL PNEW  ('SUPER_PAGE')
        END IF


C       Set up the geographical area

        CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
        CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
        CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
        CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)



C       Plot all the NetCDF variables we are interested in



          CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',     'T')
          CALL PSETC ('NETCDF_TYPE',                    'GEOMATRIX') 
          CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'lat')
          CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'lon')
          CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     1.0)
          CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',        -273.0)
c     CALL PSETC ('NETCDF_Y_TRANSFORMATION',        'logarithmic')
          CALL PNETCDF


C     Set up and plot the title text and everything else

          CALL PSETC ('TEXT_LINE_1', 'T (Air temperature)')
          CALL PSETI ('CONTOUR_LABEL_FREQUENCY', 1)
          CALL PCONT
          CALL PCOAST
          CALL PTEXT


C     ----------------------------  New page -------------------

          CALL PNEW ('SUPER_PAGE')

          CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',     'Q')
          CALL PSETC ('NETCDF_TYPE',                    'GEOMATRIX') 
          CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'lat')
          CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'lon')
          CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     1000.0)
          CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',            0.0)
c     CALL PSETC ('NETCDF_Y_TRANSFORMATION',        'logarithmic')
          CALL PNETCDF


C     Set up and plot the title text and everything else

          CALL PSETC ('TEXT_LINE_1',  'Q (Specific Humidity)')
          CALL PSETC ('CONTOUR_HILO', 'OFF')
          CALL PCONT
          CALL PCOAST
          CALL PTEXT


C     ----------------------------  New page -------------------

          CALL PNEW ('SUPER_PAGE')

          CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',     'PS')
          CALL PSETC ('NETCDF_TYPE',                    'GEOMATRIX') 
          CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'lat')
          CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'lon')
          CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     0.01)
          CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',         0.0)
c     CALL PSETC ('NETCDF_Y_TRANSFORMATION',        'logarithmic')
          CALL PNETCDF


C     Set up and plot the title text and everything else

          CALL PSETC ('TEXT_LINE_1',  'PS (Surface Air Pressure)')
          CALL PSETC ('CONTOUR_HILO', 'OFF')
          CALL PCONT
          CALL PCOAST
          CALL PTEXT

C     ----------------------------  New page -------------------

          CALL PNEW ('SUPER_PAGE')

          CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',     'C3H8_VMR_inst')
          CALL PSETC ('NETCDF_TYPE',                    'GEOMATRIX') 
          CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'lat')
          CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'lon')
          CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     1E25)
          CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',         0.0)
c     CALL PSETC ('NETCDF_Y_TRANSFORMATION',        'logarithmic')
          CALL PNETCDF


C     Set up and plot the title text and everything else

          CALL PSETC ('TEXT_LINE_1', 'C3H8_VMR_inst (propane)')
          CALL PCONT
          CALL PCOAST
          CALL PTEXT



      RETURN
      END







#include "parse_command_line.h"
