      PROGRAM PLOT_LOGO
C
C     SET MULTIPLE OUTPUT FORMATS
C
      CHARACTER*10 FORMATS
      DIMENSION    FORMATS(3)
      DATA         FORMATS /'PS', 'PNG', 'PDF'/

      CALL POPEN
      CALL PSET1C ('OUTPUT_FORMATS', FORMATS, 3)
      CALL PSETC  ('OUTPUT_NAME',    'logo_test')
C
C     PREPARE FIRST LOGO
C
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT', 'USER')
      CALL PSETC ('import_file_name',       'logo.png')
      CALL PSETC ('import_format',          'PNG')
      CALL PSETR ('import_y_position',      13.5)
      CALL PSETR ('import_x_position',       2.4)
      CALL PSETR ('import_width',            3.8)
      CALL PSETR ('import_height',           2.0)
      call PIMPORT
C
C     PREPARE SECOND LOGO
C
      CALL PSETC ('import_file_name',       'logo2.png')
      CALL PSETC ('import_format',          'PNG')
      CALL PSETR ('import_y_position',      13.5)
      CALL PSETR ('import_x_position',      23.4)
      CALL PSETR ('import_width',            3.8)
      CALL PSETR ('import_height',           2.0)
      call PIMPORT
C
      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
C
C     SELECT THE PROJECTION AND AREA AND PASS DATA TO MAGICS
C
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION', 'CORNERS')
      CALL PSETC ('SUBPAGE_MAP_HEMISPHERE', 'NORTH')
      CALL PSETR ('SUBPAGE_MAP_VERTICAL_LONGITUDE',16.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE', 27.5)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE', -2.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE', 42.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 40.0)
C
C     SET GENERAL MAGICS PARAMETERS
C
      CALL PSETC ('MAP_GRID', 'OFF')
C
C     PLOT THE COASTLINES
C
      CALL PCOAST
C
C     PLOT TITLE TEXT
C
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PSETC ('TEXT_LINE_1','Vento a 10 metri (m/s)')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT',0.4)
      CALL PSETC ('TEXT_COLOUR','black')
      CALL PTEXT
C
      CALL PCLOSE
C
      STOP
C
      END 
