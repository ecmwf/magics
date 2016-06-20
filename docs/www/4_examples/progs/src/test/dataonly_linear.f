C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM DATALINEAR_ONLY

C     This program demonstrates magics contouring facilities. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours are drawn only - no coastlines or grid.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('dataonly_linear')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
c      CALL PSETC ('GRIB_SCALING',         'OFF')
      CALL PGRIB
      

C     Define and plot the contour     

      CALL PSETC  ('CONTOUR_METHOD',           'LINEAR')
      CALL PCONT


      CALL PCLOSE

      STOP
      END

#include "parse_command_line.h"
