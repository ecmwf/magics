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
      call psetc('output_name', 'hello_world')

C Call the action routine to plot the coastlines
      call pcoast

C define the text to plot 
      call psetc('text_line_1', 'Hello World!')
	  call pseti('text_line_count', 1)

C Alternative setting : using an array of string instead
c     call pset1c('text_lines', (/'Hello World!'/), 1)

      call ptext

      call pclose

      end
