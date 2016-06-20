C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program obs

c      real      hlist(1)
c      character ctable*20
c      integer   mtable(1)
c      dimension ctable(8)
      
c      real      min(8), max(8)
c      real      mine(8), maxe(8)
      
c     data min    /220., 233., 245., 258., 270., 283., 
c     +             295., 308./
c      data mine  / 250., 280., 283., 286., 289., 292.,
c     +             295., 298./
c      data maxe  / 280., 283., 286., 289., 292.,
c     +             295., 298., 350./
               
c      data max    /233., 245., 258., 270., 283., 295.,
c     +             308., 320./

c      data hlist  /0.2/

c      data ctable /'NAVY',   'BLUE',   'CYAN', 'GREEN',
c     +             'YELLOW', 'ORANGE', 'RED',  'PURPLE'/
     
c      data mtable /15/


      CALL POPEN
     
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     50.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    -5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   25.0)

      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',      5.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',     5.0)

C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'RGB(0.85, 0.90, 0.85)')
      CALL PSETC ('MAP_GRID_COLOUR',      'RGB(0.85, 0.85, 0.85)') 
      CALL PSETC ('MAP_GRID_LINE_STYLE',  'DASH')    
      CALL PCOAST

C     Pass the data to MAGICS

      CALL PSETC ('OBS_INPUT_TYPE',      'FILE')
      CALL PSETC ('OBS_INPUT_FILE_NAME', 'data/obs_synop_cold.bufr')
      CALL POBS

C     Set up and plot the title text

      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1', 'Observation Plotting')
      CALL PSETC ('TEXT_LINE_2', 'Synoptic from 13 June 2005')
      CALL PTEXT

C     -------------- New superpage ----------------------

      CALL PNEW  ('SUPER_PAGE')

C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     52.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    15.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    56.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   20.0)
      
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',      1.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',     1.0)

 
c	  call psetc('map_coastline_colour', 'tan')
c	  call psetc('map_grid_colour', 'tan')
c	  call psetc('map_label_colour', 'tan')
c	  call psetc('map_coastline_land_shade', 'off')
c	  call psetc('map_coastline_land_shade_colour', 'cream')
c	  call pcoast 
c	  call psetc('legend', 'on')
c      call psetc('obs_input_file_name', '../data/obs.bufr')

C     Define the symbols  
      
c      call psetc  ('symbol_table_mode',  'on')
c      call pset1r ('symbol_min_table',    min, 8)
c      call pset1r ('symbol_max_table',    max, 8)
c      call pset1r ('symbol_height_table', hlist, 1)    
c      call pset1i ('symbol_marker_table', mtable, 1)     
c      call pset1c ('symbol_colour_table', ctable, 8)  

      call pobs
C     Set up and plot the title text
      call ptext
      call pcoast
      call pclose
      end
