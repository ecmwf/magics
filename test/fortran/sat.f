
      program coast
c
     
      
      call popen
      call psetc ('device','ps')
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','sat.ps')
      call psetc ('subpage_map_projection', 'satellite')
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
      call psetc ('map_coastline_land_shade','off')
      call psetc ('map_coastline_land_shade_colour','cream')
      call psetc ('map_coastline_sea_shade','off')
      call psetc ('map_grid','on')   
      


      call psetc ('grib_input_type',      'file')
      call psetc ('grib_input_file_name', 'data/z500.grb')
     
      call pgrib 
      call psetc ('contour_shade',            'on')      
      call psetc ('contour_shade_technique',  'polygon_shading')
      call psetr ('contour_shade_cell_resolution',  25.)
      call psetc ('contour_shade_method',     'area_fill')
      call pcont

c      call psetc ('grib_input_type','file')
c      call psetc ('grib_input_file_name','../data/met7fib.grb')
c      call pgrib


c      call psetc('image_colour_table_creation_mode', 'linear')
c      call psetc('image_colour_table_creation_mode', 'equidistant')
c      call psetr ('image_outlayer_rejection',0.0)
c      call psetc('my_param', 'fernando')
c      call psetc('image_min_level_colour','white')
c      call psetc('image_max_level_colour','black')
c      call pseti('image_level_count',20)
c      call pseti('image_pixel_selection_frequency',90)
c      call pimage


      call pcoast  
	  call pclose
      stop
      end
