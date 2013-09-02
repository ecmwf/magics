   	program axis01
c
c	this program demonstrates axis plotting
c
c	open magics
c
	dimension xval(2), yval(2)
	call popen
	
	call parse_command_line ('graph_lines')
	call psetc ('page_id_line_user_text','line thickness test')
	call psetc ('subpage_frame','on')
	call psetc ('subpage_map_projection','none')
c
c	set page layout values
c
	call psetr ('subpage_x_position',1.0)
	call psetr ('subpage_y_position',2.0)
	call psetr ('subpage_x_length',28.0)
	call psetr ('subpage_y_length',17.0)
    
c	draw regular axis with mostly default values
c
	call psetr ('axis_min_value',1.0)
	call psetr ('axis_max_value',16.0)	
	call psetr ('axis_tick_interval',1.0)	
	call paxis
c
c	draw logarithmic axis with mostly default values
c
	call psetc ('axis_orientation','vertical')

	call psetr ('axis_min_value',1.)
	call psetr ('axis_max_value',0.)
	call psetc ('axis_tick', 'off')
	call psetc ('axis_tick_label', 'off')
	call psetc ('text_line_1','Thickness and line style')
	call pseti ('text_line_count', 1)
	call paxis
      
      yval(1) = 0
      yval(2) = 1
      
      do i = 1, 15
        
        xval(1) = i
        xval(2) = i
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'solid')
        call pgraph
        xval(1) = i + 0.2
        xval(2) = i + 0.2
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'dash')
        call pgraph
        xval(1) = i + 0.4
        xval(2) = i + 0.4
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'dot')
        call pgraph
        xval(1) = i + 0.6
        xval(2) = i + 0.6
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'CHAIN_DOT')
        call pgraph
        xval(1) = i + 0.8
        xval(2) = i + 0.8
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'CHAIN_DASH')
        call pgraph
        
     
        
        
      end do
c
c	plot default regular and logarithmic axis
c
c
        call ptext
	call pclose
c
	end



#include "parse_command_line.h"
