C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program logowithdata
	
C     This example tests the user logo when data is loaded
    
C     Open MAGICS and set the output device

      call POPEN
      CALL PARSE_COMMAND_LINE ('logo_with_data')

      call psetc('grib_input_type', 'file')
      call psetc('grib_input_file_name', 'data/z500.grb')
      call pgrib

c      CALL PSETC ('OBS_INPUT_TYPE',      'FILE')
c      CALL PSETC ('OBS_INPUT_FILE_NAME', 'data/obs_synop_cold.bufr')

C     New page, this time with a new logo

c      call pnew ('PAGE')
      CALL PSETC('PAGE_ID_LINE_LOGO_PLOT', 'USER')
      CALL PSETC('USER_LOGO_FILENAME', 'data/logo_mf_vertical.gif')
      CALL PSETC('USER_LOGO_FORMAT', 'gif')
      CALL PSETR('USER_LOGO_X_POSITION', 0.5)
      CALL PSETR('USER_LOGO_WIDTH', 3.0)
      CALL PSETR('USER_LOGO_Y_POSITION', 1.0)
      CALL PSETR('USER_LOGO_HEIGHT', 2.0)
      call pcoast

      call psetc ('text_line_1', 'Logo: user, pos: (0.5, 12.0)')
      call ptext

C     New page, setting the logo to a new position

      call pnew ('PAGE')
      call psetr('user_logo_x_position', 23.7)
      call psetr('user_logo_width',       1.0)
      call psetr('user_logo_y_position',  0.2)
      call psetr('user_logo_height',      2.0)
      call pcoast
	  
      call psetc ('text_line_1', 'Logo: user, pos: (23.7, 0.2)')
      call ptext

C     New page, setting the logo to a new size

      call pnew ('PAGE')
      call psetr('user_logo_width',       0.75)
      call psetr('user_logo_height',      1.5)
      call pcoast
	  
      call psetc ('text_line_1', 'Logo: user, 3/4 size')
      call ptext

      call pclose
      end

#include "parse_command_line.h"

