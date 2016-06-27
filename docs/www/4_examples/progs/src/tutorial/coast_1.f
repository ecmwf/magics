C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program coast_1
c
c     open magics and define printer
c
      call popen
      call psetc ('ps_device',    'ps_a4')
      call psetc ('ps_file_name', 'coast_1.ps')
c
c     1st map - cylindrical - defined by lower left and
c     upper right latitude/longitude 
C     European Area
c
      call psetr ('subpage_lower_left_latitude',   30.)
      call psetr ('subpage_lower_left_longitude', -15.)
      call psetr ('subpage_upper_right_latitude',  70.)
      call psetr ('subpage_upper_right_longitude', 40.)
      
      call psetc ('map_coastline_colour',       'tan')
      call psetc ('map_grid_colour',            'tan')
      call psetc ('map_grid_line_style',            'dot')

      call pcoast

      call pclose

      end
