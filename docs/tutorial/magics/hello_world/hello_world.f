C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics



C Open Magics
      call ??? 

C define the name of the output 
      call psetc('output_name', 'hello_world_fortran')

C find and call the action routine to plot the coastlines
      call ???

C define the text 
      call psetc('???', 'Hello World!')
      call pseti('text_lines_count', 1)
C Alternative setting : using anarray of string instead 
	  call pset1c('???', (/'Hello World!'/), 1)

C find and call the action routine to display the text
      call ???

C Close magics
      call ???

      end
