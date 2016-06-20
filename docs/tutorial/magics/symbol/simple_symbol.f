C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics

C Define the title
      character*25 var
      dimension var(1)
      data var / +    'My first symbol plotting' /

C Open Magics
      call popen

C set the outut information
      call psetc(???)

C Define the background coastlines
      call pset?(???)
      call pcoast

C Load the data 
      call psetc('geo_input_file_name', '../airep.geo')
      call pgeo

c Define the attributes of the symbol plotting
      call psetc('symbol_type', ???)
      call pseti('symbol_marker', ???)
      call psetc('symbol_colour', ???)
      call psetr('symbol_height', ???)
      call psymb

C Add a text
      call pset?(???)
      call ptext


C Close magics : do the plot
      call ???

      end
