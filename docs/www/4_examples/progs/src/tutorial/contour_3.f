
      program contour3
      
      real levels
      dimension levels (6)
      
      
      character*6 colours
      dimension colours(5)
      
      data levels /480., 520.,530.,560.,575.,590./
      data colours /'navy', 'blue', 'green', 'yellow', 'orange'/
c
c     open magics and define plotter  
c
      call popen
      call psetc ('ps_device','ps_oa_c')                   
      call psetc ('ps_file_name', 'contour_3.ps')
      
      
c
c     polar stereographic projection - north american area
c
      call psetc ('subpage_map_projection','polar_stereographic')
      call psetc ('subpage_map_area_definition','centre')
      call psetr ('subpage_map_centre_longitude',-95.)
      call psetr ('subpage_map_centre_latitude',47.)
      call psetr ('subpage_map_vertical_longitude',-95.)
      call psetr ('subpage_map_scale',32.e6)
      call psetc ('map_coastline_colour','tan')

      
      call psetc ('map_grid_colour', 'grey') 
      call pseti ('map_coastline_thickness',3)
      call psetr ('map_grid_latitude_increment',5.0)
      call psetr ('map_grid_longitude_increment',5.0)
      call psetc ('legend','on')                   
      call pcoast
       
c
c     pass the data to magics
c
      
      call psetc ('grib_input_type','file')
      call psetc ('grib_input_file_name', 'data/z500_tc.grib')
      call pgrib
      
      
      call psetc ('contour_level_selection_type','level_list')
      call pset1r ('contour_level_list', levels, 6)
      
c      call pcont
            
c
c     default shaded (dot) contours - 
C     List of colours! 
c
      call psetc ('contour_shade','on')             
      call psetc ('contour_hilo','off')  
      call psetc ('contour_shade_colour_method', 'list')
      call pset1c ('contour_shade_colour_list', colours, 5) 
      
C play with the density to increase the contraste 
      call psetr ('contour_shade_dot_size', 0.05)
      call psetr ('contour_shade_max_level_density', 70.)
      call psetr ('contour_shade_min_level_density', 20.)
      call pcont
    
      

      
      call pclose
      end


