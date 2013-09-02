      PROGRAM LAYOUTMANUAL

C     This program demonstrates the manual layout facilities in MAGICS.
C     We generate two plots on the same page.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','layout_manual_ex.ps')


C     Set up the main page dimensions

      CALL PSETC ('LAYOUT',             'POSITIONAL')
      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)
      CALL PSETR ('PAGE_X_LENGTH',        8.25)
      CALL PSETR ('PAGE_Y_LENGTH',        8.25)
      CALL PSETR ('PAGE_X_POSITION',      1.5)
      CALL PSETR ('PAGE_Y_POSITION',      1.5)



C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/z500.grb')
      CALL PGRIB


C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')


C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Page 1')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', 11.5)
      CALL PSETC ('TEXT_LINE_1', 'Page 2')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END
