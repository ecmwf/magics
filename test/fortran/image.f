      program image

      call popen
      call psetc ('output_format','ps')
c      call psetc ('output_format','gif')
      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','image.ps')
     
c      call psetc('map_coastline', 'off')
      call psetc('map_coastline_colour', 'red')
      call psetc('map_grid_colour', 'blue')
      call psetc('map_label_colour', 'blue')

      call psetc('subpage_map_projection','satellite')
C BEGIN: only for ppm
cf      call psetr ('ppm_min_latitude',-60.)
cf      call psetr ('ppm_max_latitude',10.)
cf      call psetr ('ppm_min_longitude',-80.)
cf      call psetr ('ppm_max_longitude',-30.)

c      call psetr ('subpage_lower_left_latitude',    -60.0)
c      call psetr ('subpage_upper_right_latitude',   16.0)
c      call psetr ('subpage_lower_left_longitude',  -84.0)
c      call psetr ('subpage_upper_right_longitude',   -32.0)

c      call psetr ('subpage_lower_left_latitude',    -30.0)
c      call psetr ('subpage_upper_right_latitude',   0.0)
c      call psetr ('subpage_lower_left_longitude',  -60.0)
c      call psetr ('subpage_upper_right_longitude',   -30.0)

      call psetr ('subpage_lower_left_latitude',     -60.0)
      call psetr ('subpage_upper_right_latitude',     60.0)
      call psetr ('subpage_lower_left_longitude',    -60.0)
      call psetr ('subpage_upper_right_longitude',    60.0)

cc
cc      call psetr ('input_subarea_selection', 'off')
cc      call psetr ('input_image_columns', 2500)
cc      call psetr ('input_image_rows', 2500)
cc      call psetr ('subpage_map_initial_column',    1)
cc      call psetr ('subpage_map_initial_row',    1)
cc      call psetr ('subpage_map_sub_sat_x',    1250)
cc      call psetr ('subpage_map_sub_sat_y',    1250)
cc      call psetr ('subpage_map_x_earth_diameter',    2417)
cc      call psetr ('subpage_map_y_earth_diameter',    2417)
cc      call psetr ('subpage_map_grid_orientation',    180000.0)
cc      call psetr ('subpage_map_camera_altitude',    6610840.0)
cc
cc      call psetc ('legend',    'on')
cc      call psetc ('legend_display_type',    'continuous')
cc      call psetc('legend_box_mode', 'positional')
cc      call psetc('LEGEND_ENTRY_PLOT_DIRECTION','column')
cc      call psetr('legend_box_x_position', 20.)
cc      call psetr('legend_box_y_position',  1.5)
cc      call psetr('legend_box_x_length',    3.)
cc      call psetr('legend_box_y_length',    17.0)

cf      call pcoast

cf      call psetc ('ppm_input_file_name','../data/brazil.pgm')
cf      call praw
C END

C BEGIN: only for image
c      call pcoast

      call psetc ('grib_input_type','file')
      
      call psetc ('grib_input_file_name','../data/met7fib.grb')
c      call psetc ('grib_input_file_name','../data/met8.grb')
c      call psetc ('grib_input_file_name','../data/met8_sim.grb')
c      call psetc ('grib_input_file_name','../data/meteo.grb')
c      call psetc ('grib_input_file_name','../data/Met_8_FIB')

      call pgrib
C END

      call psetc('image_colour_table_creation_mode', 'equidistant')
c      call psetc('image_colour_table_creation_mode', 'normal')
c      call psetc('image_colour_table_creation_mode', 'default')
      call psetr ('image_outlayer_rejection',0.0)
c      call psetc('my_param', 'fernando')
      call psetc('image_min_level_colour','white')
      call psetc('image_max_level_colour','black')
      call pseti('image_level_count',20)
c      call pseti('image_pixel_selection_frequency',1)      !latlong projection
c      call pseti('image_pixel_selection_frequency',20000) !satellite projection
      call pseti('image_pixel_selection_frequency',30)
      call pimage
      call pcoast

      call pclose
      end
