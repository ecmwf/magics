C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM LOGOLAYOUT

C     This program demonstrates the automatic layout facilities in MAGICS.
C     We generate two plots on the same page.



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('logo_layout')


C     Set up the main page dimensions

      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETC ('PLOT_START',         'LEFT')
      CALL PSETC ('PLOT_DIRECTION',     'HORIZONTAL')
      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)
      CALL PSETR ('PAGE_X_LENGTH',       12.0)
      CALL PSETR ('PAGE_Y_LENGTH',       15.0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -18.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default logo settings (p1)')
      CALL PTEXT
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    35.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)

      CALL PSETC ('TEXT_LINE_1', 'Default logo settings (p2)')
      CALL PTEXT
      CALL PCOAST




C     Next superpage

      CALL PNEW ('SUPER_PAGE')



C     Page 1

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -18.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)

      CALL PSETR('USER_LOGO_X_POSITION',   1.7)
      CALL PSETR('USER_LOGO_Y_POSITION',  10.0)

      CALL PSETC ('TEXT_LINE_1', 'Default logo, user position (p1)')
      CALL PTEXT
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    35.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)

      CALL PSETC ('TEXT_LINE_1', 'Default logo, user position (p2)')
      CALL PTEXT
      CALL PCOAST



C     Next superpage

      CALL PNEW ('SUPER_PAGE')



C     Page 1

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -18.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)

      CALL PSETC('PAGE_ID_LINE_LOGO_PLOT', 'USER')
      CALL PSETC('USER_LOGO_FILENAME',     'data/logo_mf_vertical.gif')
      CALL PSETC('USER_LOGO_FORMAT',       'gif')
      CALL PSETR('USER_LOGO_X_POSITION', 23.7)
      CALL PSETR('USER_LOGO_WIDTH',       1.0)
      CALL PSETR('USER_LOGO_Y_POSITION',  0.2)
      CALL PSETR('USER_LOGO_HEIGHT',      2.0)

      CALL PSETC ('TEXT_LINE_1', 'User logo (p1)')
      CALL PTEXT
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    35.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)

      CALL PSETC ('TEXT_LINE_1', 'Default logo settings (p2)')
      CALL PTEXT
      CALL PCOAST


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

