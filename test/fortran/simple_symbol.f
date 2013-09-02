      PROGRAM odbt2mex

    

C     Start MAGICS and set the ouput device

      call popen
      CALL PARSE_COMMAND_LINE ('simple_symbol')
      
      call psetc ('automatic_title', 'on')
      call psetc ('legend', 'on')


C     Set up the coastline attributes

      call psetc ('map_coastline',        'off')
      call psetc ('map_coastline_colour', 'grey')
      call psetc ('map_grid',      'off')     


C     Setting of input positions.

      call pset1r('symbol_input_x_position', (0.), 1)
      call pset1r('symbol_input_y_position', (10.), 1)
      call pset1r('symbol_input_number_list', (0.), 1)
     
      call pseti ('symbol_marker',   4 )     

      call psymb
      
      call pset1r('symbol_input_x_position', (0.), 1)
      call pset1r('symbol_input_y_position', (20.), 1)
      call pset1r('symbol_input_number_list', (0.), 1)
      call preset ('symbol_marker')
      call psetc ('symbol_name',   "ww_12" )     

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



