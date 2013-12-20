        program shading
c
c     this program demonstrates magics contouring facilities. 
c     the meteorological data field is a standard global 500 hpa
c     model output field on a regular 1.5 degree grid. 
c     contours and coastlines are projected onto a 
c     polar stereographic map.


c     open magics
     
      real lat
      real lon
      real latg
      real long
      dimension rlat(1), rlon(1)
      dimension rlatg(1), rlong(1)
      
      lat = 51.
      lon = -1.
      rlat(1) = lat
      rlon(1) = lon
      latg =51.7
      long =-1.6
      rlatg(1) = latg
      rlong(1) = long
      
      call popen

      call psetc ('ps_device','ps_a4')
      call psetc ('ps_file_name','akima761.ps')
      
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     40.0)
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -7.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    65.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   2.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     0.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    0.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   25.0)

      call psetc ("legend", 'on')
      
   
      call psetc ('geo_input_file_name',
     +     '../data/akima.geo')
c     +     '../data/2mt.geo')
      call pgeo
      
      call psetr("akima_resolution_x",5.)
      call psetr("akima_resolution_y",5.)
      call psetc ('contour_method', 'akima761')
  
c      CALL PSETC("CONTOUR","off")
      CALL PSETC("CONTOUR_GRID_VALUE_PLOT","ON");
      call pcont
      
      call pcoast

      call pclose

      stop
      end
