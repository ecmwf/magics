
      program contour2
      
      real levels
      dimension levels (6)
      data levels /480., 520.,530.,560.,575.,590./
c
c     open magics and define plotter  
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'contour_2.ps')

c
c      global cylindrical projection
c      
      call pseti ('map_coastline_thickness', 2)
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
      call psetc ('map_grid_line_style','dash')
      call psetc ('map_coastline_land_shade','on') 
      call psetc ('map_coastline_land_shade_colour','grey')
    
      call pcoast

c
c     pass the data to magics
c
     
      call psetc ('grib_input_type','file')
      call psetc ('grib_input_file_name', 'data/z500_tc.grib')
      call pgrib
c
c     solid brown contours defined by list of levels
c
      call psetc ('contour_level_selection_type','level_list')
      call pset1r ('contour_level_list', levels, 6)
      call psetc ('contour_line_colour','navy')
      call psetc ('contour_highlight_colour', 'navy')
      call psetc ('contour_line_style','solid')     
      call pseti ('contour_label_frequency',1)     
      call pseti ('contour_line_thickness', 1)
      call psetc ('contour_highlight_colour', 'navy')
      call psetc ('contour_hi_colour', 'navy')
      call psetc ('contour_lo_colour', 'navy')  
      call pcont

C
C   Add a text
C
      call psetc('text_line_1', 'Geopotential on a global map')
      call pseti('text_line_count', 1)
      call psetc('text_colour', 'navy')
      
C
C Positionning of the box 
C
      
      call psetc('text_mode', 'positional')
      call psetr('text_box_x_position', 5.)
      call psetr('text_box_y_position', 16.)
      call psetr('text_box_x_length', 10.)
      call psetr('text_box_y_length', 3.)
      
      call ptext
            
      call pclose
      
      end

      

