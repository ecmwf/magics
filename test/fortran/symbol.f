      PROGRAM odbt2mex

      real      clist(11), hlist(8)
      real      x(10), y(10), z(10)
      character ctable*20
      character stable*20
      integer   mtable(1)
      dimension ctable(8), stable(4)
      real      min(8), max(8)
      
      
      data min    /0, 1., 2., 4., 5., 6., 
     +             8., 9./
               
      data max    /1., 2., 4., 5., 6., 
     +             8., 9., 10./

      data hlist  /0.2, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2/
      
      data x /10., 20., 30., 40., 50., 60., 
     +        70., 80., 90., 100. /
       data y /5., 10., 15., 20., 25., 30., 
     +        35., 40., 45., 50. /
       data z /1., 2., 3., 4., 5., 6., 
     +        7., 8., 9., 10. /

      data ctable /'NAVY',   'BLUE',   'CYAN', 'GREEN',
     +             'YELLOW', 'ORANGE', 'RED',  'PURPLE'/
     
      data mtable /4/
      
      data stable /'ww_05', 'magics_2', 
     +'magics_3', 'ww_10'/


C     Start MAGICS and set the ouput device

      call popen
      CALL PARSE_COMMAND_LINE ('symbol')
      
      call psetc ('automatic_title', 'on')
        call psetc ('legend', 'on')


C     Set up the coastline attributes

      call psetc ('map_coastline',        'on')
      call psetc ('map_coastline_colour', 'grey')
      call psetc ('map_grid_colour',      'grey')     


C     Setting of input positions.

      call pset1r('symbol_input_x_position', x, 10)
      call pset1r('symbol_input_y_position', y, 10)
      call pset1r('symbol_input_number_list', z, 10)

     
      

C     Define the symbols  
      
      call psetc  ('symbol_table_mode',  'on')
      call pset1r ('symbol_min_table',    min, 8)
      call pset1r ('symbol_max_table',    max, 8)
      call pset1r ('symbol_height_table', hlist, 8)    
      call pset1c ('symbol_name_table',   stable, 4)     
c      call pset1i ('symbol_marker_table', mtable, 1) 
      call pset1c ('symbol_colour_table', ctable, 8)  

      call psymb
      

C     Set up and plot the title text
     
      call ptext
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



