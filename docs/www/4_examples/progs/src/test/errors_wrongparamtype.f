C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM ERRORS_WRONGPARAMTYPE

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('errors_wrongparamtype')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETR ('MAP_GRID_COLOUR',      'GREY')     



C     Plot everything

      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

