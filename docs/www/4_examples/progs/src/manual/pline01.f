      PROGRAM PLINE01

C     This example demonstrates the use of PLINE to draw
C     shaded polygons over a geographic area. In this case,
C     the polygons each represent a different catchement area.


      CALL POPEN
      CALL PSETC ('OUTPUT_NAME', 'pline01')



C     Set up our page / projection

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  77.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   75.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)


C     Set up the legend

      CALL PSETC ('LEGEND', 'ON')
      CALL PSETC ('LEGEND_TITLE',        'on')
      CALL PSETC ('LEGEND_TITLE_TEXT',   'Catchement areas')
      CALL PSETC ('LEGEND_DISPLAY_TYPE', 'continuous')
      CALL PSETC ('LEGEND_TEXT_FORMAT',  '(f5.2)')


C     Tell MAGICS where to find the coordinate and values files

      CALL PSETC ('POLYLINE_INPUT_POSITIONS_FILENAME', 
     +            'data/catchment.pos')
      CALL PSETC ('POLYLINE_INPUT_VALUES_FILENAME', 
     +            'data/catchment.val')


C     Tell MAGICS which number will separate the polygons

      CALL PSETR('POLYLINE_INPUT_BREAK_INDICATOR', -999.)


C     Set up the polyline plotting attributes - we will have
C     MAGICS automatically calculate the colours to shade with

      CALL PSETC ('POLYLINE_SHADE', 'ON')
      CALL PSETC ('POLYLINE_SHADE_COLOUR_METHOD',    'CALCULATE')
      CALL PSETC ('POLYLINE_SHADE_MAX_LEVEL_COLOUR', 'RED')
      CALL PSETC ('POLYLINE_SHADE_MIN_LEVEL_COLOUR', 'BLUE')
      CALL PSETC ('POLYLINE_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PLINE
      CALL PCOAST



C     New plot - this time with a vertical legend and
C     restricted values for shading

      CALL PNEW  ('PAGE')
      CALL PSETC ('LEGEND_BOX_MODE', 'POSITIONAL')
      CALL PSETR ('LEGEND_BOX_X_POSITION', 25.)
      CALL PSETR ('LEGEND_BOX_Y_POSITION', 7.)
      CALL PSETR ('LEGEND_BOX_X_LENGTH', 5.)
      CALL PSETR ('LEGEND_BOX_Y_LENGTH', 10.)
      CALL PSETC ('LEGEND_BOX_BLANKING', 'ON')
      CALL PSETC ('LEGEND_BOX_BORDER', 'ON')
      CALL PSETR ('POLYLINE_SHADE_MAX_LEVEL', 8.)
      CALL PSETR ('POLYLINE_SHADE_MIN_LEVEL', 4.)
      CALL PSETC ('POLYLINE_SHADE_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR ('POLYLINE_INTERVAL', 0.5)
      CALL PLINE
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END




