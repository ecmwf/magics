      program logo

C     This program demonstrates the new logo customisations that
C     Magics++ can perform.
	
  
C     Open MAGICS and set the output device

      call popen
      call parse_command_line ('logo')
     

C     Set up and plot the coastlines using default logo

      call psetc ('map_coastline_colour',  'tan')
      call psetc ('map_grid_colour',       'tan')
      call psetc ('map_label_colour',      'tan')
      call psetc ('map_coastline',         'off')
      call pcoast

      call psetc ('text_line_1', 'Default logo')
      call ptext



C     New page, this time no logo

      call pnew ('PAGE')
      call psetc('page_id_line_logo_plot', 'off')
      call pcoast

      call psetc ('text_line_1', 'Logo: OFF')
      call ptext



C     New page, this time with a new logo

      call pnew ('PAGE')
      call psetc('page_id_line_logo_plot', 'user')
      call psetc('user_logo_filename',     'data/logo_met.gif')
      call psetc('user_logo_format',       'gif')
      call psetr('user_logo_x_position',    0.5)
      call psetr('user_logo_width',         1.0)
      call psetr('user_logo_y_position',   12.0)
      call psetr('user_logo_height',        2.0)
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
