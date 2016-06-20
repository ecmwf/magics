C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

        program shading
c
c     this program demonstrates magics contouring facilities. 
c     the meteorological data field is a standard global 500 hpa
c     model output field on a regular 1.5 degree grid. 
c     contours and coastlines are projected onto a 
c     polar stereographic map.


c     open magics
     
   
      
      call popen

      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','geo.ps')
      



      call psetc ("legend", 'on')
      
   
      call psetc ('geo_input_file_name',
     +     '../data/line.geo')

      call pgeo
      
      call psetc('polyline_shade_colour', 'green')
      call pline
      
      call pcoast

      call pclose

      stop
      end
