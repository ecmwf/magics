      program odkhis
      implicit none
      real x(6), y(6)
      integer i
      !real,dimension(:),pointer :: rsym
      real rsym

      do i=1, 6
      print*,i
      x(i)=i
      y(i)=3*cos(real(i)/8*3.14)

      enddo

      print*, x
      print*, y

      call popen
      call psetc("output_name","fy3b")
      call psetc("page_id_line","off")
!    call psetc("layout","positional")
       call psetr("subpage_x_position",1.)
        call psetr("subpage_y_position", 3.)

!     CALL PSETC('odb_database',  &
!                 '/scratch/rd/stu/todb/ECMA.amsua')

!     CALL PSETC ('odb_query', &
!               'SELECT lat, lon, obsvalue &
!                FROM   hdr, body          &
!                WHERE  satellite_identifier=4 and &
!                       vertco_reference_1=5 and obsvalue IS NOT null') 
!                       vertco_reference_1=5 and obsvalue IS NOT null') 

      

      
      CALL pcoast
      !CALL PSETC ('odb_filename', "/home/graphics/cgs/public/test.odb")
      CALL PSETC ('odb_filename', "test.odb")
      CALL PSETC ('odb_latitude_variable',    'lat')
      CALL PSETC ('odb_longitude_variable',   'lon')
     !CALL PSETC ('odb_value_variable', 'obsvalue')
      CALL PSETC ('odb_value_variable', 'fg_depar')
      CALL PODB
 
   


      call psetc('legend', 'on')
      call psetc('legend_box_mode', 'positional')
      call psetc('legend_box_blanking', 'off')
      call psetr('legend_box_y_position', 1.)
      call psetr('legend_box_y_length', 1.5)
      call psetr('legend_box_x_length', 28.)
      call psetr('legend_box_x_position', 1)
      call pseti('legend_label_frequency', 20)

      call psetc('legend', 'on')
      call psetc('legend_display_type', 'histogram')     
      call psetc('legend_histogram_border', 'off')     
	  call psetc('legend_text_format', "(F3.2)")
	  call psetc('legend_histogram_mean_value', "on")
	  call psetc('legend_histogram_mean_value_marker_colour', "evergreen")
	  call pseti('legend_histogram_mean_value_marker', 15)
	  call psetr('legend_histogram_mean_value_marker_size', 0.4)
      call psetc('legend_display_type', 'histogram')     
!       call psetc('symbol_advanced_table_selection_type', 'count')
      call psetc('symbol_advanced_table_selection_type', 'count')
      call pseti('symbol_advanced_table_level_count', 300)
      call psetc('symbol_advanced_table_min_level_colour', 'blue')
      call psetc('symbol_table_mode', 'advanced')
      call psetc('symbol_type', 'marker')
      call psetr('symbol_advanced_table_interval', 1.5000000)
      call psetr('symbol_advanced_table_interval', 1.5000000)
      call psetc('symbol_advanced_table_colour_direction', 'clockwise')
      call pseti('symbol_marker', 15)
      call psetc('symbol_advanced_table_max_level_colour', 'red')
      call psetr('symbol_advanced_table_min_value',-3.0)
      call psetr('symbol_advanced_table_max_value',3.0)

      call psymb

      print*, 'hello qifneg '
      print*,rsym
      print*, 'I am passed'
!     CALL PENQR('SYMBOL_INPUT_NUMBER_LIST',rsym)
      print*, rsym
!      call psymb


      call pclose
      



      end program odkhis



      subroutine calhist()


      end subroutine calhist
