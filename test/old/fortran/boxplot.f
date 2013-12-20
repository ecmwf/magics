C     ****************** LICENSE ****************
C
C     Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)
C
C     Licensed under the Apache License, Version 2.0 (the "License");
C     you may not use this file except in compliance with the License.
C     You may obtain a copy of the License at 
C
C        http://www.apache.org/licenses/LICENSE-2.0
C
C     Unless required by applicable law or agreed to in writing, software
C     distributed under the License is distributed on an "AS IS" BASIS,
C     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
C     See the License for the specific language governing permissions and
C     limitations under the License.
C
C     ****************** LICENSE ****************
C
C     This program demonstrates a new feature of Magics++: boxplots.
C
      PROGRAM BOXPLOT

      DIMENSION RPOS(4),RMIN(4),RMAX(4),RMED(4),RUPP(4),RLOW(4)
      DATA RPOS/2.0, 4.0, 6.5, 8.0/
      DATA RMIN/1.0, 3.0, 5.2, 4.0/
      DATA RMAX/5.0, 7.2, 9.4, 8.0/
      DATA RMED/3.0, 5.0, 7.7, 6.0/
      DATA RUPP/4.0, 6.0, 8.5, 7.0/
      DATA RLOW/2.3, 4.0, 6.1, 5.0/
C
C     Open MAGICS and set the output file types/names
C
      CALL POPEN()
C
      CALL PSETC ('OUTPUT_FORMAT',  'PS')
      CALL PSETC ('OUTPUT_NAME',    'boxplot')
C
C     Boxplots work in paper coordinates, not map projection
C
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'NONE')
C
C     Set up our axes. We will always use a 10x10 axis for
C     these examples. We must set up the axes before plotting
C     our boxplots.
C
      CALL DRAW_AXES ()
C
C     Set up our data for the boxplots
C
      CALL PSET1R('BOXPLOT_POSITIONS',        RPOS, 4)
      CALL PSET1R('BOXPLOT_MINIMUM_VALUES',   RMIN, 4)
      CALL PSET1R('BOXPLOT_MAXIMUM_VALUES',   RMAX, 4)
      CALL PSET1R('BOXPLOT_MEDIAN_VALUES',    RMED, 4)
      CALL PSET1R('BOXPLOT_BOX_UPPER_VALUES', RUPP, 4)
      CALL PSET1R('BOXPLOT_BOX_LOWER_VALUES', RLOW, 4)
C
C     Draw the boxplots using the default plotting attributes
C
      CALL PBOXPLOT
C
      CALL PSETC ('TEXT_LINE_1',
     x            'Boxplot - default plotting attributes')
      CALL PTEXT
C
C     New page - use different boxplot plotting attributes
C
      CALL PNEW ('PAGE')
C
      CALL DRAW_AXES ()
C
      CALL PSETR ('BOXPLOT_BOX_WIDTH', 1.)
      CALL PSETC ('BOXPLOT_BOX_COLOUR', 'TURQUOISE')
      CALL PSETI ('BOXPLOT_BOX_BORDER_THICKNESS', 1)
      CALL PSETC ('BOXPLOT_BOX_BORDER_LINE_STYLE', 'DASH')
      CALL PSETC ('BOXPLOT_WHISKER', 'BOX')
      CALL PSETR ('BOXPLOT_WHISKER_BOX_WIDTH', 0.3)
      CALL PSETC ('BOXPLOT_WHISKER_BOX_COLOUR', 'BLUE_PURPLE')
      CALL PSETC ('BOXPLOT_WHISKER_BOX_BORDER_COLOUR', 'RED')
      CALL PBOXPLOT
C
      CALL PSETC ('TEXT_LINE_1',
     x            'Thick dashed border, box colour, whiskers as boxes')
      CALL PTEXT
C
C
C     New page - use different boxplot plotting attributes
C
      CALL PNEW ('PAGE')
C
      CALL DRAW_AXES ()
C
      CALL PSETC  ('BOXPLOT_WHISKER',            'LINE')
      CALL PSETC  ('BOXPLOT_WHISKER_LINE_STYLE', 'DOT')
      CALL PRESET ('BOXPLOT_BOX_COLOUR')
      CALL PRESET ('BOXPLOT_BOX_BORDER_THICKNESS')
      CALL PRESET ('BOXPLOT_BOX_BORDER_LINE_STYLE')
      CALL PBOXPLOT
C
      CALL PSETC ('TEXT_LINE_1',
     x            'Whiskers as lines')
      CALL PTEXT
C
C     New page - use different median plotting attributes
C
      CALL PNEW ('PAGE')
C
      CALL DRAW_AXES ()
C
      CALL PSETC  ('BOXPLOT_MEDIAN_COLOUR',     'NAVY')
      CALL PSETI  ('BOXPLOT_MEDIAN_THICKNESS',   4)
      CALL PSETC  ('BOXPLOT_MEDIAN_LINE_STYLE', 'DOT')
      CALL PRESET ('BOXPLOT_WHISKER_LINE_STYLE')
      CALL PBOXPLOT
C
      CALL PSETC ('TEXT_LINE_1',
     x            'Median styles - thick, navy, dotted')
      CALL PTEXT
C
C
C     New page - plot plain black & white boxplots
C
      CALL PNEW ('PAGE')
C
      CALL DRAW_AXES ()
C
      CALL PSETC  ('BOXPLOT_BOX_COLOUR',          'WHITE')
      CALL PSETC  ('BOXPLOT_MEDIAN_COLOUR',       'BLACK')
      CALL PSETC  ('BOXPLOT_BOX_BORDER_COLOUR',   'BLACK')
      CALL PSETC  ('BOXPLOT_WHISKER_LINE_COLOUR', 'BLACK')
      CALL PRESET ('BOXPLOT_MEDIAN_THICKNESS')
      CALL PRESET ('BOXPLOT_MEDIAN_LINE_STYLE')
      CALL PBOXPLOT

      CALL PSETC ('TEXT_LINE_1', 'Plain boxplot')
      CALL PTEXT
C
C     closing
C
      CALL PCLOSE()
C
      END
C
C
C     ------------------------------------------------------------
C     SUBROUTINE DRAW_AXES
C     Sets up and draws the axes. We will need to do this for each
C     page, to it's better to have it as a subroutine.
C     ------------------------------------------------------------
C
      SUBROUTINE DRAW_AXES ()
C
      CALL PSETC ('AXIS_LINE_COLOUR', 'BLUE')
      CALL PSETC ('AXIS_GRID', 'ON')
      CALL PSETC ('AXIS_GRID', 'ON')
      CALL PSETC ('AXIS_GRID_COLOUR', 'GREY')
      CALL PSETC ('AXIS_GRID_LINE_STYLE', 'DASH')
      CALL PSETC ('AXIS_ORIENTATION', 'VERTICAL')
      CALL PSETR ('AXIS_MIN_VALUE', 0.)
      CALL PSETR ('AXIS_MAX_VALUE', 10.)
      CALL PAXIS
C
      CALL PSETC ('AXIS_ORIENTATION', 'HORIZONTAL')
      CALL PSETR ('AXIS_MIN_VALUE', 0.)
      CALL PSETR ('AXIS_MAX_VALUE', 10.)
      CALL PAXIS
C
      RETURN
      END
