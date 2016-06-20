C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program graf04
c
c	this routine demonstrates the plotting of curves on date/time aixs
c
      call popen
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','curves.ps')
c
c	curve with date axis
c
      call curve

      call pclose
      end
      subroutine curve
c
c	this routine demonstrates the plotting of curves on date/time aixs
c
c	the x axis is a date/time axis from 25/6/91 to 10/7/91
c

      dimension xval(10), xval1(2)
      dimension yval1(10)
      data yval1 /5.0,8.,10.,21.,25.,33.,23.,12.,3.,10./
      dimension yval2(2)
      data yval2 /30.,30./
      dimension yval3(10)
      data yval3 /7.0,10.,12.,23.,27.,35.,25.,14.,5.,12./
      dimension yval4(10)
      data yval4 /8.0,11.,13.,24.,28.,36.,26.,15.,6.,13./
      dimension yval5(10)
      data yval5 /9.0,12.,14.,25.,29.,37.,27.,16.,7.,14./
      dimension yval6(10)
      data yval6 /10.0,13.,15.,26.,30.,38.,28.,17.,8.,15./
      dimension yval7(10)
      data yval7 /11.0,14.,16.,27.,31.,39.,29.,18.,9.,16./
      
      dimension yval8(10)
      data yval8 /12.0,15.,17.,28.,32.,40.,30.,19.,10.,17./     
      
      dimension yval9(10)
      data yval9 /13.0,16.,18.,29.,33.,41.,31.,20.,11.,18./
      
      dimension yval10(10)
      data yval10 /14.0,17.,19.,30.,34.,42.,32.,21.,12.,19./
      
      dimension yval11(10)
      data yval11 /15.0,18.,20.,31.,35.,43.,33.,22.,13.,20./
      
      data xval /1., 2., 3.,4., 5., 6., 7., 8., 9., 10./  
      data xval1 /1., 10./ 
      
      

      call psetc ('subpage_map_projection','none')
      call psetr ('subpage_y_position',3.0)
      call psetr ('subpage_y_length',15.0)
      call psetr ('axis_min_value',1.0)
      call psetr ('axis_max_value',10.0)

      call psetc('legend_box_mode', 'positional')
      call psetr('legend_box_x_position', 25.)
      call psetr('legend_box_y_position', 7.)
      call psetr('legend_box_x_length', 5.)
      call psetr('legend_box_y_length', 10.)
      call psetc('legend_border', 'on')
      call psetc('legend_box_blanking', 'off')
      call pseti('legend_column_count', 2) 

      call paxis

      call psetc ('axis_orientation','vertical')
      call psetc ('axis_position','left')
      call psetc ('axis_type','regular')
      call psetr ('axis_min_value',0.0)
      call psetr ('axis_max_value',50.0)
      call psetc ('axis_grid','on')
      call psetc ('axis_grid_colour','black')
      call psetc ('axis_grid_line_style', 'dash')
      call paxis
c
c	set curve values
c
      call psetc ('legend','on')
      call psetc ('graph_line_colour','black')
      call psetc ('legend_user_text_2','blue')      
      call pset1r ('y_values',yval2,2)
      call pset1r ('graph_curve_y_values',yval2,2)
      call pset1r ('graph_curve_x_values',xval1,2)
      call pseti ('graph_line_thickness',4)
      call pgraph
      
      call psetc ('legend','on')
      call psetc ('graph_type','curve')
      call pset1r ('x_values',xval,10)
      call pset1r ('graph_curve_x_values',xval,10)
      call pset1r ('y_values',yval1,10)
      call pset1r ('graph_curve_y_values',yval1,10)
	  call pseti ('graph_line_thickness',2)
      call psetc ('graph_line_colour','red')
      call psetc ('legend_user_text_1','red')
      call pgraph
      
      call psetc ('graph_line_colour','green')
      call psetc ('legend_user_text_2','green')
      call pset1r ('y_values',yval3,10)
      call pset1r ('graph_curve_y_values',yval3,10)
      call pgraph
      call psetc ('graph_line_colour','magenta')
      call psetc ('legend_user_text_3','magenta')
      call pset1r ('y_values',yval4,10)
      call pset1r ('graph_curve_y_values',yval4,10)
      call pgraph
      call psetc ('graph_line_colour','pink')
      call psetc ('legend_user_text_4','pink')
      call pset1r ('y_values',yval5,10)
      call pset1r ('graph_curve_y_values',yval5,10)
      call pgraph
      call psetc ('graph_line_colour','orange')
      call psetc ('legend_user_text_5','orange')
      call pset1r ('y_values',yval6,10)
      call pset1r ('graph_curve_y_values',yval6,10)
      call pgraph
      call psetc ('graph_line_colour','cyan')
      call psetc ('legend_user_text_6','cyan')
      call pset1r ('y_values',yval7,10)
      call pset1r ('graph_curve_y_values',yval7,10)
      call pgraph
      call psetc ('graph_line_colour','evergreen')
      call psetc ('legend_user_text_7','evergreen')
      call pset1r ('y_values',yval8,10)
      call pset1r ('graph_curve_y_values',yval8,10)
      call pgraph
      call psetc ('graph_line_colour','yellow')
      call psetc ('legend_user_text_8','yellow')
      call pset1r ('y_values',yval9,10)
      call pset1r ('graph_curve_y_values',yval9,10)
      call pgraph
      call psetc ('graph_line_colour','tan')
      call psetc ('legend_user_text_9','tan')

      call pset1r ('y_values',yval10,10)
      call pset1r ('graph_curve_y_values',yval10,10)
      call pgraph
      call psetc ('graph_line_colour','cream')
      call psetc ('legend_user_text_10','cream')
      call pset1r ('y_values',yval11,10)
      call pset1r ('graph_curve_y_values',yval11,10)
      call pgraph
      
      

c
c	generate text for title
c
      call psetc ('text_justification','centre')
      call psetc ('text_line_1',
     1 'graph plotting - example of curve on date/time axis')
      call ptext

      end


