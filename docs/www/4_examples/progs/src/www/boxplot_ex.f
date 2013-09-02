      PROGRAM GRAFBOXPLOT
    
      CALL POPEN()

      CALL PARSE_COMMAND_LINE ('boxplot_ex')
      
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'NONE')


C     Set up our axes. We will always use a 10x10 axis for
C     these examples. We must set up the axes before plotting
C     our boxplots.

      CALL DRAW_AXES ()


C     Set up our data for the boxplots

      CALL PSET1R('BOXPLOT_POSITIONS', (/2., 4., 6.5, 8. /), 4 )
      CALL PSET1R('BOXPLOT_MINIMUM_VALUES', 
     +                 (/1., 3., 5.2, 4./), 4)
      CALL PSET1R('BOXPLOT_MAXIMUM_VALUES', 
     +                 (/5., 7.2, 9.4, 8./), 4)
      CALL PSET1R('BOXPLOT_MEDIAN_VALUES', 
     +                 (/3., 5., 7.7, 6./), 4)
      CALL PSET1R('BOXPLOT_BOX_UPPER_VALUES', 
     +                 (/4., 6., 8.5, 7./), 4)
      CALL PSET1R('BOXPLOT_BOX_LOWER_VALUES', 
     +                 (/2.3, 4., 6.1, 5./), 4)
     


C     Draw the boxplots using the default plotting attributes

      CALL PBOXPLOT

      CALL PSETC ('TEXT_LINE_1',
     x            'Boxplot - default plotting attributes')
      CALL PTEXT



C     New page - use different boxplot plotting attributes

      CALL PNEW ('PAGE')

      CALL DRAW_AXES ()

      CALL PSETR ('BOXPLOT_BOX_WIDTH', 1.)
      CALL PSETC ('BOXPLOT_BOX_COLOUR', 'TURQUOISE')
      CALL PSETI ('BOXPLOT_BOX_BORDER_THICKNESS', 1)
      CALL PSETC ('BOXPLOT_BOX_BORDER_LINE_STYLE', 'DASH')
      CALL PSETC ('BOXPLOT_WHISKER', 'BOX')
      CALL PSETR ('BOXPLOT_WHISKER_BOX_WIDTH', 0.3)
      CALL PSETC ('BOXPLOT_WHISKER_BOX_COLOUR', 'BLUE_PURPLE')
      CALL PSETC ('BOXPLOT_WHISKER_BOX_BORDER_COLOUR', 'RED')
      CALL PBOXPLOT

      CALL PSETC ('TEXT_LINE_1',
     x            'Thick dashed border, box colour, whiskers as boxes')
      CALL PTEXT




C     New page - use different boxplot plotting attributes

      CALL PNEW ('PAGE')

      CALL DRAW_AXES ()

      CALL PSETC  ('BOXPLOT_WHISKER',            'LINE')
      CALL PSETC  ('BOXPLOT_WHISKER_LINE_STYLE', 'DOT')
      CALL PRESET ('BOXPLOT_BOX_COLOUR')
      CALL PRESET ('BOXPLOT_BOX_BORDER_THICKNESS')
      CALL PRESET ('BOXPLOT_BOX_BORDER_LINE_STYLE')
      CALL PBOXPLOT

      CALL PSETC ('TEXT_LINE_1',
     x            'Whiskers as lines')
      CALL PTEXT


C     New page - use different median plotting attributes

      CALL PNEW ('PAGE')

      CALL DRAW_AXES ()

      CALL PSETC  ('BOXPLOT_MEDIAN_COLOUR',     'NAVY')
      CALL PSETI  ('BOXPLOT_MEDIAN_THICKNESS',   4)
      CALL PSETC  ('BOXPLOT_MEDIAN_LINE_STYLE', 'DOT')
      CALL PRESET ('BOXPLOT_WHISKER_LINE_STYLE')
      CALL PBOXPLOT

      CALL PSETC ('TEXT_LINE_1',
     x            'Median styles - thick, navy, dotted')
      CALL PTEXT




C     New page - plot plain black & white boxplots

      CALL PNEW ('PAGE')

      CALL DRAW_AXES ()

      CALL PSETC  ('BOXPLOT_BOX_COLOUR',          'WHITE')
      CALL PSETC  ('BOXPLOT_MEDIAN_COLOUR',       'BLACK')
      CALL PSETC  ('BOXPLOT_BOX_BORDER_COLOUR',   'BLACK')
      CALL PSETC  ('BOXPLOT_WHISKER_LINE_COLOUR', 'BLACK')
      CALL PRESET ('BOXPLOT_MEDIAN_THICKNESS')
      CALL PRESET ('BOXPLOT_MEDIAN_LINE_STYLE')
      CALL PBOXPLOT

      CALL PSETC ('TEXT_LINE_1', 'Plain boxplot')
      CALL PTEXT



C     Shutdown

      CALL PCLOSE()
    
      END




C     ------------------------------------------------------------
C     SUBROUTINE DRAW_AXES
C     Sets up and draws the axes. We will need to do this for each
C     page, to it's better to have it as a subroutine.
C     ------------------------------------------------------------

      SUBROUTINE DRAW_AXES ()

      CALL PSETC ('AXIS_LINE_COLOUR', 'BLUE')
      CALL PSETC ('AXIS_GRID', 'ON')
      CALL PSETC ('AXIS_GRID', 'ON')
      CALL PSETC ('AXIS_GRID_COLOUR', 'GREY')
      CALL PSETC ('AXIS_GRID_LINE_STYLE', 'DASH')
      CALL PSETC ('AXIS_ORIENTATION', 'VERTICAL')
      CALL PSETR ('AXIS_MIN_VALUE', 0.)
      CALL PSETR ('AXIS_MAX_VALUE', 10.)
      CALL PAXIS


      CALL PSETC ('AXIS_ORIENTATION', 'HORIZONTAL')
      CALL PSETR ('AXIS_MIN_VALUE', 0.)
      CALL PSETR ('AXIS_MAX_VALUE', 10.)
      CALL PAXIS

      RETURN
      END







#include "parse_command_line.h"
