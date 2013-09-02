      PROGRAM odb

      real clist(11), hlist(10)
      character ctable*20
      integer mtable(1)
      dimension ctable(10)
      real min(10), max(10)
      
      
      data min /200., 210., 220., 240., 250., 260., 
     +           270., 280., 290., 300./ 
    
               
      data max /210., 220., 240., 250., 260., 
     +           270., 280., 290., 300., 370./
      data hlist /0.1, 0.2, 0.3, 0.1, 0.2,
     + 0.3, 0.1, 0.2, 0.3, 0.4/
      data ctable /'NAVY', 'BLUE', 'GREENISH_BLUE', 
     +             'CYAN', 'GREEN',
     +            'YELLOW', 'ORANGE', 'RED_ORANGE', 
     +            'RED', 'PURPLE'/
     
      data mtable /4/
      


      CALL POPEN
      
       CALL PSETC ('automatic_title',        'ON')
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','amsub.ps')




C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     

C     Pass the data to MAGICS
      call psetc('odb_database',
     +     'odb://freki:/bigtmp/odb_data/ECMA.amsub/ECMA')
      call psetc ('odb_query',
     +     'select lat, lon, obsvalue from hdr, body
     + where  obstype=7 AND sensor=4 AND press=1
     + and obsvalue is not null')
      
      call psetc ('odb_latitude', 'lat@hdr')
      call psetc ('odb_longitude', 'lon@hdr')
      call psetc ('odb_observation', 'obsvalue@body')
      call pseti ('odb_nb_rows', 200000)
      call podb
      
C     Define the contour  
      
      call psetc('symbol_table_mode', 'on')
      call pset1r('symbol_min_table', min, 8)
      call pset1r('symbol_max_table', max, 8)
      call pset1r('symbol_height_table', hlist, 1)    
      call pset1i('symbol_marker_table', mtable, 1)     
      call pset1c('symbol_colour_table', ctable, 8)  

      call psymb
      

C     Set up and plot the title text
     
      CALL PTEXT
      CALL PCOAST





      CALL PCLOSE

      STOP
      END
