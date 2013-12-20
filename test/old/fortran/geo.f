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
