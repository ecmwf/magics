C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM odbt2mex

      real      clist(11), hlist(8)
      character ctable*20
      character stable*20
      integer   mtable(1)
      dimension ctable(8), stable(4)
      real      min(8), max(8)
      
      
      data min    /220., 233., 245., 258., 270., 283., 
     +             295., 308./
               
      data max    /233., 245., 258., 270., 283., 295.,
     +             308., 320./

      data hlist  /0.2, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2/

      data ctable /'NAVY',   'BLUE',   'CYAN', 'GREEN',
     +             'YELLOW', 'ORANGE', 'RED',  'PURPLE'/
     
      data mtable /4/
      
      data stable /'magics_1', 'magics_2', 
     +'magics_3', 'magics_3'/


C     Start MAGICS and set the ouput device

      call popen
      CALL PARSE_COMMAND_LINE ('odb_t2m_ex')
      
      call psetc ('automatic_title', 'on')
        call psetc ('legend', 'on')


C     Set up the coastline attributes

      call psetc ('map_coastline',        'on')
      call psetc ('map_coastline_colour', 'grey')
      call psetc ('map_grid_colour',      'grey')     


C     Access the ODB and pass the data to MAGICS

      call psetc('odb_database',
     +             'odb://njord/tmp/odb_data/ECMA.conv/ECMA')
      call psetc('odb_database_option',
     +             'clean')
      call psetc ('odb_query',
     +              'select lat, lon, obsvalue 
     +               from hdr, body 
     +               where varno=$t2m and obsvalue is not null')
      
      call psetc ('odb_latitude',    'lat@hdr')
      call psetc ('odb_longitude',   'lon@hdr')
      call psetc ('odb_observation', 'obsvalue@body')
      call pseti ('odb_nb_rows',      200000)
      call podb
      

C     Define the symbols  
      
      call psetc  ('symbol_table_mode',  'on')
      call pset1r ('symbol_min_table',    min, 8)
      call pset1r ('symbol_max_table',    max, 8)
      call pset1r ('symbol_height_table', hlist, 8)    
c      call pset1c ('symbol_name_table',   stable, 4)     
       call pset1i ('symbol_marker_table', mtable, 1) 
      call pset1c ('symbol_colour_table', ctable, 8)  

      call psymb
      

C     Set up and plot the title text
     
      call ptext
      call pcoast



C     ------------------------------------------------------------
C     New page - this time with a smaller request and an area zoom
C     ------------------------------------------------------------

      call pnew  ('super_page')


      call psetr ('subpage_lower_left_longitude',  -20.0)
      call psetr ('subpage_upper_right_longitude',  20.0)
      call psetr ('subpage_upper_right_latitude',   50.0)
      call psetr ('subpage_lower_left_latitude',    30.0)

      call psetc('odb_database',
     +             'odb://njord/tmp/odb_data/ECMA.conv/ECMA')

      call psetc ('odb_query',
     +              'select lat, lon, obsvalue 
     +               from hdr, body 
     +               where varno=$t2m and 
     +               degrees(lat) < 48 and degrees(lat) >  32 and
     +               degrees(lon) < 18 and degrees(lon) > -18 and
     +               obsvalue is not null')
      
      call psetc ('odb_latitude',    'lat@hdr')
      call psetc ('odb_longitude',   'lon@hdr')
      call psetc ('odb_observation', 'obsvalue@body')
      call pseti ('odb_nb_rows',      200000)
      call podb


      call psymb
      call pcoast

      call pclose

      STOP
      END



C --------------------------------------------------------------------
C     PARSE_COMMAND_LINE
C     Checks the command-line for any arguments.
C     Arguments can come in pairs. Currently supported arguments are:
C     PROJECTION 
C     DEVICE 
C     e.g. Run the program with:
C      PROJECTION POLAR_STEREOGRAPHIC
C      PROJECTION CYLINDRICAL   DEVICE SVG
C --------------------------------------------------------------------

      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)

      CHARACTER*32 ARG
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*48 EXENAME
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME
      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      DEVICE_SET = 0

      NUM_ARGS = IARGC()

      I = 1

20    IF (I.LE.NUM_ARGS) THEN
          CALL GETARG ( I, ARG ) 
          

C         Set the projection?

          IF (ARG.EQ.'PROJECTION') THEN
              I = I + 1 
              CALL GETARG ( I, PROJECTION ) 
              CALL PSETC ('SUBPAGE_MAP_PROJECTION', PROJECTION)


C        Set the device?

          ELSEIF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

C             Set the output filename

              IF     (DEVICE.EQ.'PS')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_DEVICE',   'ps_a4')
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('DEVICE',       'GD')
                OUTNAME = OUTROOTNAME //   '.gif'
                CALL PSETC ('GD_FORMAT',   'ANIMATION')
                CALL PSETC ('GD_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('DEVICE',       DEVICE)
                OUTNAME = OUTROOTNAME //   '.svg'
                CALL PSETC ('SVG_FILE_NAME', OUTNAME)
              ELSE
                WRITE(*,*), 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1

          ENDIF

          I = I + 1 
          GOTO 20
      ENDIF


C     If no device has been set, then use PostScript by default

      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME // '.ps'
        CALL PSETC ('PS_DEVICE',    'ps_a4')
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF

      END



