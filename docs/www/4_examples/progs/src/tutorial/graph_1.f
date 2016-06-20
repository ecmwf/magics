C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program graf01

      dimension x(11),y(11)
      data x /0.0,1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
      data y /0.0,4.,6.,8.,10.,15.,20.,30.,20.,10.,5./
c
c     open magics and define printer
c
        call popen
        call psetc ('ps_device',    'ps_a4')
        call psetc ('ps_file_name', 'graph_1.ps')

        call psetc ('page_id_line_user_text','My first graph')

      call psetc ('legend','on')
      call psetc ('legend_user_text','My red curve')

c
c	set up axis system
c
      call psetc ('subpage_map_projection','none')  
      call psetc ('axis_orientation','horizontal')
      call psetc ('axis_position','bottom')
      call psetr ('axis_min_value',0.0)
      call psetr ('axis_max_value',10.0)
      call psetr ('axis_tick_interval',1.0)
      call paxis
      call psetc ('axis_orientation','vertical')
      call psetc ('axis_position','left')
      call psetr ('axis_min_value',0.0)
      call psetr ('axis_max_value',40.0)
      call psetr ('axis_tick_interval',5.0)
      call paxis
      
c
c	plot simple curve with symbols
c

c
c	set graph parameters
c
      call psetc ('graph_type','curve')
      call psetc ('graph_symbol','on')
      call pseti ('graph_symbol_marker_index',18)
      call psetc ('graph_line_colour','red')
      call pset1r ('graph_curve_x_values',x,11)
      call pset1r ('graph_curve_y_values',y,11)
      call pgraph
c
c	plot text
c
      call psetc ('text_line_1','Graph plotting in ')
      call psetc ('text_line_2','curve mode with symbols')
      call psetc ('text_line_colour','navy')
      call pseti ('text_line_count',2)
      call ptext
      call psetc ('graph_symbol','off')
      call pclose

      end
