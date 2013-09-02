      program coast_2
c
c     open magics and define printer
c
      call popen
      call psetc ('ps_device',    'ps_a4')
      call psetc ('ps_file_name', 'coast_2.ps')
c
c     2nd map - polar stereographic defined by map centre
c     and map scale                   
c
c
      call psetc ('subpage_map_projection','polar_stereographic')
      call psetc ('subpage_map_area_definition','centre')       
      call psetr ('subpage_map_centre_longitude',-95.)            
      call psetr ('subpage_map_centre_latitude',40.)            
      call psetr ('subpage_map_vertical_longitude',-95.)            
      call psetr ('subpage_map_scale', 27.e6 )            
      
      call psetc ('map_coastline_colour','mustard')                       
      call psetc ('map_grid_colour','grey')                       
      call pseti ('map_coastline_thickness',3)          
      call psetc ('map_grid_line_style','dot')                       
      call psetr ('map_grid_latitude_increment',5.0)            
      call psetr ('map_grid_longitude_increment',5.0)            
      call pcoast
 
      call psetc ('text_line_1', 'North America in polar stereographic')
      call psetc ('text_line_2', 'This is My Magics Plot')
      call pseti ('text_line_count', 2)
c
c    add the text 
c
      call ptext 
      
      call pclose

      end
