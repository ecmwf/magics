      PROGRAM TEST

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout')


C     Define the superpage size to be 29.7 x 21.0 cm

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 29.7)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 21.0)


C     The page size within the superpage will be half the superpage's
C     height, but its full width.

      CALL PSETR ('PAGE_Y_LENGTH',       14.85)
      CALL PSETR ('PAGE_X_LENGTH',       21.0)

      CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'layout_manual_ex')
      CALL PSETC ('MAP_COASTLINE_COLOUR',   'OLIVE')
      CALL PSETC ('MAP_GRID_COLOUR',        'OLIVE')


C     -----------------
C     FIRST SUPER-PAGE.
C     -----------------

      CALL PSETC ('TEXT_LINE_1','Plot Layout Example')
      CALL PCOAST
      CALL PTEXT

      CALL PSETC ('TEXT_LINE_1',        'Page One')
      CALL PSETC ('TEXT_MODE',          'POSITIONAL')    
      CALL PSETC ('TEXT_BORDER',        'ON')    
      CALL PSETC ('TEXT_BOX_BLANKING',  'ON')    
      CALL PSETR ('TEXT_BOX_X_POSITION',  8.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION', 13.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',    5.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    1.5)     
      CALL PTEXT

      CALL PSETC ('TEXT_LINE_1',        'Subpage One')
      CALL PSETR ('TEXT_BOX_X_POSITION', 9.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION', 8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',   3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',   1.0)     
      CALL PTEXT
      CALL PSETC ('SUPER_PAGE_FRAME','ON')

      CALL PNEW  ('PAGE')
      CALL PSETC ('SUBPAGE_FRAME',      'OFF')
      CALL PSETC ('PAGE_ID_LINE',       'OFF')
      CALL PSETC ('TEXT_LINE_1',        'Super-Page One')
      CALL PSETR ('TEXT_BOX_X_POSITION', 4.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION', 12.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',   13.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',   2.5)     
      CALL PTEXT      



C     --------------------------------------
C     SECOND SUPER-PAGE.
C     --------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('MAP_COASTLINE_COLOUR',   'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',        'TAN')


C     PLOT FIRST PAGE, WHICH HAS ONE SUBPAGE

      CALL PSETC ('PAGE_ID_LINE', 'ON')
      CALL PSETC ('SUBPAGE_FRAME', 'ON')
      CALL PSETC ('TEXT_MODE','TITLE')    
      CALL PSETC ('TEXT_BORDER','OFF')    
      CALL PCOAST
      CALL PSETC ('TEXT_LINE_1','Plot Layout Example')
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Page One')
      CALL PSETC ('TEXT_MODE','POSITIONAL')    
      CALL PSETC ('TEXT_BOX_BLANKING','ON')    
      CALL PSETR ('TEXT_BOX_X_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',13.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',5.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.2)     
      CALL PSETC ('TEXT_BORDER','ON')    
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Subpage One')
      CALL PSETR ('TEXT_BOX_X_POSITION',9.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.0)     
      CALL PTEXT
      CALL PNEW  ('PAGE')
C
C     PLOT SECOND PAGE, WHICH HAS TWO SUBPAGES
C
      CALL PSETR ('SUBPAGE_X_LENGTH',8.25)
      CALL PSETR ('SUBPAGE_Y_LENGTH',8.25)
      CALL PSETR ('SUBPAGE_X_POSITION',1.5)
      CALL PSETR ('SUBPAGE_Y_POSITION',1.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',0.0)
      CALL PSETC ('TEXT_MODE','TITLE')    
      CALL PSETC ('TEXT_BORDER','OFF')    
      CALL PSETC ('TEXT_LINE_1','Plot Layout Example')
      CALL PCOAST
      CALL PTEXT
      CALL PNEW  ('SUBPAGE')
C
      CALL PSETR ('SUBPAGE_X_POSITION',11.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',180.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',0.0)
      CALL PCOAST
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Page Two')
      CALL PSETC ('TEXT_MODE','POSITIONAL')    
      CALL PSETC ('TEXT_BORDER','ON')    
      CALL PSETC ('TEXT_BOX_BLANKING','ON')    
      CALL PSETR ('TEXT_BOX_X_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',11.3)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',5.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.2)     
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Super-Page Two')
      CALL PSETR ('TEXT_BOX_X_POSITION',4.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',12.8)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',13.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.9)     
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Subpage One')
      CALL PSETR ('TEXT_BOX_X_POSITION',3.9)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.0)     
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Subpage two')
      CALL PSETR ('TEXT_BOX_X_POSITION',13.2)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.0)     
      CALL PTEXT

      CALL PCLOSE
      END


C --------------------------------------------------------------------
C     PARSE_COMMAND_LINE
C     Checks the command-line for any arguments.
C     Arguments can come in pairs. Currently supported arguments are:
C     PROJECTION <CYLINDRICAL | POLAR_STEREOGRAPHIC>
C     DEVICE <PS | SVG | PNG>
C     e.g. Run the program with:
C       <progname> PROJECTION CYLINDRICAL  DEVICE SVG
C --------------------------------------------------------------------

      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)

      CHARACTER*32 ARG
      CHARACTER*32 DEVICE
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME

      CHARACTER*16 FORMATS_PS_PNG_SVG
      DIMENSION    FORMATS_PS_PNG_SVG(3)
      DATA         FORMATS_PS_PNG_SVG /'PS', 'PNG', 'SVG'/

      CHARACTER*16 FORMATS_ALL
      DIMENSION    FORMATS_ALL(5)
      DATA         FORMATS_ALL /'PS', 'SVG', 'PDF',
     +                          'KML', 'PNG'/

      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      DEVICE_SET = 0

      ID_TEXT = ''

      NUM_ARGS = IARGC()

      I = 1
C
C  Start GoTo
C
20    IF (I.LE.NUM_ARGS) THEN
          CALL GETARG ( I, ARG )
C
C        Set the device?
C
          IF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

C             Set the output filename

              IF     (DEVICE.EQ.'OLD')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_DEVICE',   'ps_a4')
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'PS') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'EPS') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'EPS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'AGIF') THEN
                CALL PSETC ('OUTPUT_FORMAT','GIF_ANIMATION')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
                CALL PSETI ('OUTPUT_GIF_DELAY',     150)
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PNG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'SVG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'KML') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'KML')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)

              ELSEIF (DEVICE.EQ.'PS_PNG_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_PNG_SVG, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'ALL') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_ALL, 5)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1
          ENDIF
          I = I + 1 
          GOTO 20
      ENDIF
C
C     If no device has been set, then use PostScript by default
C
      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME //'.ps'
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF
C
      END
