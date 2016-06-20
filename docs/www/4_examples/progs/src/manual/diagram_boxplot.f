C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM BOXPLOT01
    
      CALL POPEN()

      CALL PSETC ('OUTPUT_NAME', 'diagram_boxplot')

      
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'NONE')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 10.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 10.0)

      CALL PSETR ('PAGE_Y_LENGTH', 10.0)
      CALL PSETR ('PAGE_X_LENGTH', 10.0)

      CALL PSETC ('PAGE_ID_LINE', 'OFF')
      CALL PSETC ('PAGE_FRAME',   'OFF')


C     Set up our axes. We will always use a 10x10 axis for
C     these examples. We must set up the axes before plotting
C     our boxplots.

      CALL DRAW_AXES ()


C     Set up our data for the boxplots

      CALL PSET1R('BOXPLOT_POSITIONS',        (/0.5/), 1)
      CALL PSET1R('BOXPLOT_MINIMUM_VALUES',   (/1./),  1)
      CALL PSET1R('BOXPLOT_MAXIMUM_VALUES',   (/5./),  1)
      CALL PSET1R('BOXPLOT_MEDIAN_VALUES',    (/3./),  1)
      CALL PSET1R('BOXPLOT_BOX_UPPER_VALUES', (/4./),  1)
      CALL PSET1R('BOXPLOT_BOX_LOWER_VALUES', (/2.3/), 1)
     


C     Draw the boxplots using the default plotting attributes

      CALL PBOXPLOT



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
      CALL PSETC ('AXIS_GRID', 'OFF')
      CALL PSETC ('AXIS_ORIENTATION', 'VERTICAL')
      CALL PSETR ('AXIS_MIN_VALUE', 0.)
      CALL PSETR ('AXIS_MAX_VALUE', 6.)
      CALL PAXIS


      CALL PSETC ('AXIS_ORIENTATION', 'HORIZONTAL')
      CALL PSETR ('AXIS_MIN_VALUE', 0.)
      CALL PSETR ('AXIS_MAX_VALUE', 1.)
      CALL PAXIS

      RETURN
      END



