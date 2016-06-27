C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      program taylor
      
      dimension x(10),y(10),rar (10) 
      data x/0.2,0.4,0.6,0.7,0.6,0.1,0.45,0.5,0.65,0.7/ 
	  data y/0.7,0.4,0.1,0.45,0.5,0.65,0.1,0.2,0.6,0.7/ 
	  data rar/2.5,4.,6.,8.,10.,12.,14.,16.,18.,20./ 
      
      call popen()
      call psetc ('ps_device', 'ps_a4')
      call psetc ('ps_file_name', 'taylor.ps')
      
      call psetc ('subpage_map_projection', 'taylor')
      call psetc ('symbol_position_mode', 'graph')
      call psetc ('subpage_frame', 'off')
      call psetr ('axis_min_value',0.) 
      call psetr ('axis_max_value',1.) 
      call paxis
      call psetc ('axis_orientation','vertical') 
      call paxis

	  call psetc ('symbol_type','number') 
	  call pset1r ('symbol_input_number_list',rar,10) 
	  call pset1r ('symbol_input_x_position',x,10) 
	  call pset1r ('symbol_input_y_position',y,10) 

	  call psetc ('symbol_colour','magenta') 
      call pseti ('symbol_marker', 15)
	  call psetr ('symbol_height',0.5) 
	  call psetc ('symbol_table_mode','off') 
      call pset1c ('symbol_texts',(/'a', 'b', 'c', 'd', 'e', 'f', 
     +                         'g', 'h', 'i', 'j', 'k'/), 10) 
      call psetc ('symbol_text_font_colour', 'black') 
	  call psetc ('text_line_1',
     x  'Example of Taylor Diagram') 
 
	  
      call ptaylor
	  call psymb 
	  call ptext 
    
      
      
 
    
      call pclose()
    
      end
