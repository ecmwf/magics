! (C) Copyright 1996-2016 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.
!

      program obs

!     -------------------------------------------------------
!     this program demonstrates magics++ observation plotting
!     facilities.
!     -------------------------------------------------------

!     open magics and set the output device

      call popen
     

!     area specification (south, west, north, east )

      call psetr ('subpage_lower_left_latitude',     50.0)
      call psetr ('subpage_lower_left_longitude',    -5.0)
      call psetr ('subpage_upper_right_latitude',    70.0)
      call psetr ('subpage_upper_right_longitude',   25.0)

      call psetr ('map_grid_latitude_increment',      5.0)
      call psetr ('map_grid_longitude_increment',     5.0)

!     set up the coastline attributes

      call psetc ('map_coastline_colour', 'rgb(0.25, 0.25, 0.25)')
      call psetc ('map_grid_colour',      'rgb(0.45, 0.45, 0.45)') 
      call psetc ('map_grid_line_style',  'dash')    
      call pcoast

!     pass the data to magics

      call psetc ('obs_input_type',      'file')
      call psetc ('obs_input_file_name', 'obs.bufr')
      call pobs
! 
!     set up and plot the title text

      call pseti ('text_line_count', 2)
      call psetc ('text_line_1', 'observation plotting')
      call psetc ('text_line_2', 'synoptic from 13 february 2005')
      call ptext

!     -------------- new superpage ----------------------

      call pnew  ('super_page')

!     area specification (south, west, north, east )

      call psetr ('subpage_lower_left_latitude',     52.0)
      call psetr ('subpage_lower_left_longitude',    15.0)
      call psetr ('subpage_upper_right_latitude',    56.0)
      call psetr ('subpage_upper_right_longitude',   20.0)
      
      call psetr ('map_grid_latitude_increment',      1.0)
      call psetr ('map_grid_longitude_increment',     1.0)

!     plotting

      call pcoast
      call pobs
      call ptext

!     ----------------- close ----------------------------

      call pclose

      stop
      end

! 
