      PROGRAM LAYOUTMANUAL

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout_manual_ex')


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

      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.4)
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
C
C     PLOT SECOND PAGE, WHICH HAS TWO SUBPAGES
C
      CALL PNEW  ('PAGE')
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
C
C     CLOSE MAGICS
C
      CALL PCLOSE
C
      END


#include "parse_command_line.h"
