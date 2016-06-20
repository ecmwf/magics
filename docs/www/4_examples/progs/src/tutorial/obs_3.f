C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

 
      program obs3
      
      character*5 obtyp(1)
      data obtyp /'temp'/
c
c     open magics and define postscript device  
c     output file name: output.ps
c
      call popen
      call psetc ('ps_device','ps_a4')                   
      call psetc ('ps_file_name', 'obs_3.ps')  
      call pseti ('map_coastline_thickness', 2)
      call psetc ('map_coastline_colour','grey')
      call psetc ('map_grid_colour','grey')
      call psetc ('map_grid_line_style','dash')
      
      call pcoast     
      
c
c     plot all code types"
c
      call psetc('obs_input_file_name',
     +'data/obs_tc.bfr')

c
c     turn off all parameters except wind and pressure
c
      call psetc('obs_thickness','off')                         
      call psetc('obs_pressure','on')                         
      call psetc('obs_temperature','off')                         
      call psetc('obs_cloud','off')                         
      call psetc('obs_present_weather','off')                         
      call psetc('obs_past_weather','off')                         
      call psetc('obs_visibility','off')                         
      call psetc('obs_dewpoint','off')                         
      call psetc('obs_pressure_tendency','off')                         
      call psetc('obs_ship_direction','off')                         
      call psetc('obs_ship_speed','off')                         
      call psetc('obs_sea_surface_temperature','off')  
      
      call psetr('obs_distance_apart',0.5)     
      call pobs        

c
c     Add some text
c     
      call psetc ('text_line_1', 'All observations...')
      call psetc ('text_colour', 'navy')
      call pseti ('text_line_count', 1)
      
      
      call ptext


      call pclose
      
      end
      
