C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM OBS

C     -------------------------------------------------------
C     This program demonstrates Magics++ observation plotting
C     facilities.
C     -------------------------------------------------------

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('obs_ex')

C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     50.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    -5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   25.0)

      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',      5.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',     5.0)

C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'RGB(0.85, 0.90, 0.85)')
      CALL PSETC ('MAP_GRID_COLOUR',      'RGB(0.85, 0.85, 0.85)') 
      CALL PSETC ('MAP_GRID_LINE_STYLE',  'DASH')    
      CALL PCOAST

C     Pass the data to MAGICS

      CALL PSETC ('OBS_INPUT_TYPE',      'FILE')
      CALL PSETC ('OBS_INPUT_FILE_NAME', 'data/obs_synop_cold.bufr')
      CALL POBS

C     Set up and plot the title text

      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1', 'Observation Plotting')
      CALL PSETC ('TEXT_LINE_2', 'Synoptic from 13 June 2005')
      CALL PTEXT

C     -------------- New superpage ----------------------

      CALL PNEW  ('SUPER_PAGE')

C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     52.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    15.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    56.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   20.0)
      
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',      1.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',     1.0)

C     Plotting

      CALL PCOAST
      CALL POBS
      CALL PTEXT

C     ----------------- Close ----------------------------

      CALL PCLOSE

      STOP
      END

#include "parse_command_line.h"
