C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics

C open Magics
      call popen

C Define the name of the output 
      call ???('???', 'projection_fortran')

C define the coordinates of the geographical area 
      call psetc('subpage_map_projection', ???)
      call psetr('subpage_lower_left_longitude', ???)
      call ???('subpage_upper_right_longitude', ???)
      call psetr('???', ???)
      call psetr('???', ???)

C define the coastlines attributes 
      call psetc('colour of the line', '???')
      call psetc('land shading on???', '???')
      call psetc('sea shading on???', 'on')
      call ???('which colour for the land shading?', '???')
      call psetc('which colour for the See shading?', '???')
      call psetc('We want the grid', '??')
      call psetc('We want a grey grid', '???')
      call psetc('We do not want label', 'off')
	
C Call the action routine to plot the coastlines
      call ???

C Setting of the title 
      call pset1c('???', (/'Global Map!'/), 1)

      call ???

      call pclose

      end
