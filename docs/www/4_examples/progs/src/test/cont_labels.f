      PROGRAM CONTLABELS
      
C     This program demonstrates contour labels in Magics++.
C     We use the automatic layout facilities to create a set of plots
C     stacked vertically from the top of the page.


      REAL SUPER_W
      REAL SUPER_H
      INTEGER NUM_PLOTS_PER_PAGE



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_labels')


C     Set up the main page dimensions

      SUPER_W = 21.0
      SUPER_H = 29.7
      NUM_PLOTS_PER_PAGE = 4
      

      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETC ('PLOT_START',         'TOP')
      CALL PSETC ('PLOT_DIRECTION',     'VERTICAL')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', SUPER_H)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', SUPER_W)

      CALL PSETR ('PAGE_Y_LENGTH', SUPER_H / (NUM_PLOTS_PER_PAGE*1.1))
      CALL PSETR ('PAGE_X_LENGTH', SUPER_W)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -90.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -120.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   -10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  120.0)

C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.38)
      CALL PSETC ('TEXT_COLOUR',       'BLACK')
      CALL PSETC ('TEXT_FONT_STYLE',   'BOLD')


C     Set up our generic contour attributes

      CALL PSETC ('CONTOUR_HILO', 'OFF')




C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Contour Labels')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_HEIGHT: 0.8')
      CALL PTEXT
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',  0.8)     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_HEIGHT: 0.2')
      CALL PTEXT
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',  0.2)     
      CALL PCONT


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_HEIGHT: 0.5')
      CALL PTEXT
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',  0.5)     
      CALL PCONT


C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label font quality
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1',
     x            'Height 0.5, CONTOUR_LABEL_QUALITY: DEFAULT')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_QUALITY: MEDIUM')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_QUALITY',  'MEDIUM')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_QUALITY: HIGH')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_QUALITY',  'HIGH')     
      CALL PCONT


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_COLOUR: LAVENDER')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_COLOUR',    'LAVENDER')     
      CALL PCONT



C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label fonts
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PRESET ('CONTOUR_LABEL_COLOUR')     
      CALL PRESET ('CONTOUR_LABEL_QUALITY')     


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Label Font')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_FONT: COURIER')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',  'COURIER')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'CONTOUR_LABEL_FONT: TIMES')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',  'TIMES')     
      CALL PCONT



C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label font styles
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')



C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Times, Bold')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'TIMES')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'BOLD')     
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Times, Italic')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'TIMES')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'ITALIC')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Times, Normal')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'TIMES')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'NORMAL')     
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Courier, Bold')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_FONT',       'COURIER')     
      CALL PSETC ('CONTOUR_LABEL_FONT_STYLE', 'BOLD')     
      CALL PCONT




C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour label frequencies
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PRESET ('CONTOUR_LABEL_FONT')
      CALL PRESET ('CONTOUR_LABEL_FONT_STYLE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Blanking OFF')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_BLANKING',  'OFF')     
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Blanking ON, Frequency 1')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LABEL_BLANKING',  'ON')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',  1)
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Frequency 3')
      CALL PTEXT
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',  3)
      CALL PCONT


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Frequency 4')
      CALL PTEXT
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',  4)
      CALL PCONT


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

