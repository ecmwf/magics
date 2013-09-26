      program contour
c
c     this program demonstrates magics contour shading techniques and methods.
c     there are three techniques - polygon_shading, marker and cell_shading.
c     polygon shading has three methods - dot, hatch and area_fill (solid).
c     the area selected is a polar stereographic atlantic/european area and the
c     data to be contoured/shaded is a msl field in grib code.
c
c     open magics
c
      dimension conlis (7)
      data conlis /450.0, 490.0, 520., 550., 565., 580., 620.0/
      call popen
      call psetc('output_name', 'contour')
      call psetc ('page_id_line_user_text', 'basic/contour')
c
c     select the projection and area and pass data to magics
c
      call psetc ('subpage_map_projection', 'polar_stereographic')
      call psetc ('subpage_map_area_definition', 'centre')
      call psetr ('subpage_map_centre_latitude', 40.0)
      call psetr ('subpage_map_centre_longitude', -20.0)
      call psetr ('subpage_map_scale', 35.e6)
      call psetc ('grib_input_type', 'file')
      call psetc ('grib_input_file_name', 'z500.grb')
      call pgrib
c
c     set general magics parameters
c
      call psetc ('contour_hilo', 'off')
      call psetc ('contour_shade', 'on')
      call psetc ('legend', 'on')
      call psetc ('contour_highlight', 'off')
      call psetc ('contour_line_colour', 'black')
      call psetc ('contour_level_selection_type', 'level_list')
      call pset1r ('contour_level_list', conlis, 7)
      call pseti ('contour_label_frequency', 1)
      call psetc ('map_grid', 'off')
      call psetc ('map_label', 'off')
      call psetc ('map_coastline_colour', 'black')
      call psetc ('text_justification', 'centre')
      call psetr ('text_reference_character_height', 1.25)
c
c     first map - technique: polygon, method: area_fill
c
      call cont08_one
      call pnew ('page')
c
c     second map - technique: polygon, method: dot
c
      call cont08_two
      call pnew ('page')
c
c     third map - technique: polygon, method: hatch
c
      call cont08_three
      call pnew ('page')
c
c     fourth map - technique: cell_shading (default)
c
      call cont08_four
      call pnew ('page')
c
c     fivth map - technique: cell_shading (different method and resolution)
c
      call cont08_five
      call pnew ('page')    
c
c     sixth map - technique: marker
c
      call cont08_six

      call pclose
      stop
      end    
    






      subroutine cont08_one
c
c     this routine uses polygon shading technique with 'dot' method
c
c     plot the shaded contours
c
      call psetc ('contour_shade_technique', 'polygon_shading')   
      call psetc ('contour_shade_method', 'dot')
      call psetr ('contour_shade_dot_size', 0.045)     
      call pcont
c
c     plot the coastlines
c
      call pcoast
c
c     plot title text
c
      call pseti ('text_line_count', 1)
      call psetc ('text_line_1',
     x'contour_shade_technique= polygon '//
     x    '    shade_method= dot')
      call ptext
      return
      end
      subroutine cont08_two
c
c     this routine uses polygon shading technique with 'hatch' method
c
c     plot the shaded contours
c
      call psetc ('contour_shade_technique', 'polygon_shading')   
      call psetc ('contour_shade_method', 'hatch') 
      call pcont
c
c     plot the coastlines
c
      call pcoast
c
c     plot title text
c
      call psetc ('text_line_1',
     x'contour_shade_technique= polygon '//
     x    '    shade_method= hatch')
      call ptext
      return
      end
      subroutine cont08_three

c
c     this routine uses polygon shading technique with 'rea_fill' method
c
c     plot the shaded contours
c
      call psetc ('contour_shade_technique', 'polygon_shading')   
      call psetc ('contour_shade_method', 'area_fill')   
      call pcont
c
c     plot the coastlines
c
      call pcoast
c
c     plot title text
c
      call psetc ('text_line_1',
     x'contour_shade_technique= polygon '//   
     x   '    shade_method = area_fill')
      call ptext
      return
      end

      subroutine cont08_four
c
c     this routine uses cell shading technique,  default cell_method = 'interpolate'
c     default cell_resolution = 10.0 
c
c     plot the shaded contours - default cell_method = 'interpolate'
c     default cell_resolution = 10.0
c
      call psetc ('contour_shade_technique', 'cell_shading')
      call psetr ('contour_shade_cell_resolution', 1.0)
      call pcont
c
c     plot the coastlines
c
      call pcoast
c
c     plot title text
c
      call psetc ('text_line_1',
     x'contour_shade_technique = cell_shading                ')
      call psetc ('text_line_2',
     x'cell_method = interpolate , cell resoloution = 10.0  ')
      call pseti ('text_line_count', 2)
      call ptext
      return
      end
      subroutine  cont08_five
c
c     this routine uses cell shading technique,  cell_method = 'nearest'
c     default cell_resolution = 20.0
c
c     plot the shaded contours  
c
      call psetc ('contour_shade_technique', 'cell_shading')  
      call psetc ('contour_shade_cell_method', 'nearest')
      call psetr ('contour_shade_cell_resolution', 20.0) 
      call pcont
c
c     plot the coastlines
c
      call pcoast
c
c     plot title text
c
      call psetc ('text_line_1',
     x'contour_shade_technique = cell_shading                ')
      call psetc ('text_line_2',
     x'cell_method = nearest, cell resoloution = 20.0  ')
      call pseti ('text_line_count', 2)
      call ptext
      return
      end
      subroutine cont08_six
      character*32 coltab
      dimension  hgtab(6), martab(6)
      data martab /3, 5, 7, 9, 15, 18/
      data hgtab /0.22, 0.23, 0.21, 0.18, 0.17, 0.15/
c
c     cylindical area for marker shading to avoid congestion
c
      call psetc ('subpage_map_projection', 'cylindrical')
      call psetr ('subpage_lower_left_latitude', 20.0)    
      call psetr ('subpage_lower_left_longitude', -30.0)    
      call psetr ('subpage_upper_right_latitude', 70.0)    
      call psetr ('subpage_upper_right_longitude', 40.0)    
c
c     this routine uses marker shading technique 
c
c     each shading level will have a different marker symbol, height and colour
c     - defined by the tables martab, hgtab and coltab
c     6 shading levels required defined previously
c
      call pset1i ('contour_shade_marker_table', martab, 6)
      call pset1r ('contour_shade_height_table', hgtab, 6)
c
c     plot the shaded contours
c
      call psetc ('contour_shade_technique', 'marker')   
      call pcont
c
c     plot the coastlines
c
      call pcoast
c
c     plot title text
c
      call psetc ('text_line_1',
     x'contour_shade_technique = marker -  various markers and heights')
      call pseti ('text_line_count', 1)
      call ptext
      return
      end


