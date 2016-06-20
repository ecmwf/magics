C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

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




C     New page - plot plain black & white boxplots with an additional
C     curve added to the plot

      CALL PNEW ('PAGE')

      CALL DRAW_AXES ()

      CALL PSETC  ('BOXPLOT_BOX_COLOUR',          'WHITE')
      CALL PSETC  ('BOXPLOT_MEDIAN_COLOUR',       'BLACK')
      CALL PSETC  ('BOXPLOT_BOX_BORDER_COLOUR',   'BLACK')
      CALL PSETC  ('BOXPLOT_WHISKER_LINE_COLOUR', 'BLACK')
      CALL PRESET ('BOXPLOT_MEDIAN_THICKNESS')
      CALL PRESET ('BOXPLOT_MEDIAN_LINE_STYLE')
      CALL PBOXPLOT

      CALL PSETC  ('GRAPH_TYPE',          'CURVE')
      CALL PSETC  ('GRAPH_SYMBOL',        'ON')
      CALL PSETC  ('GRAPH_LINE',          'ON')
      CALL PSETC  ('GRAPH_SYMBOL_COLOUR', 'RED_ORANGE')
      CALL PSETI  ('GRAPH_SYMBOL_MARKER_INDEX',18)
      CALL PSETR  ('GRAPH_SYMBOL_HEIGHT',  0.3)
      CALL PSET1R ('GRAPH_CURVE_X_VALUES', (/2.4, 4.4, 6., 8.5 /), 4)
      CALL PSET1R ('GRAPH_CURVE_Y_VALUES', (/1., 3., 5.2, 4./), 4)
      CALL PGRAPH 

      CALL PSETC ('TEXT_LINE_1', 'Plain boxplot with added curve')
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
      CHARACTER*64 ID_TEXT
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*48 EXENAME
      CHARACTER*8  MINIMAL
      CHARACTER*8  SEPARATOR
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME

      CHARACTER*16 FORMATS_PS_GIF
      DIMENSION    FORMATS_PS_GIF(2)
      DATA         FORMATS_PS_GIF /'PS', 'GIF_ANIMATION'/

      CHARACTER*16 FORMATS_PS_GIF_PDF
      DIMENSION    FORMATS_PS_GIF_PDF(3)
      DATA         FORMATS_PS_GIF_PDF /'PS', 'GIF_ANIMATION', 'PDF'/

      CHARACTER*16 FORMATS_PS_GIF_SVG
      DIMENSION    FORMATS_PS_GIF_SVG(3)
      DATA         FORMATS_PS_GIF_SVG /'PS', 'GIF_ANIMATION', 'SVG'/

      CHARACTER*16 FORMATS_PS_PNG_SVG
      DIMENSION    FORMATS_PS_PNG_SVG(3)
      DATA         FORMATS_PS_PNG_SVG /'PS', 'PNG', 'SVG'/

      CHARACTER*16 FORMATS_PS_GIF_PNG_SVG
      DIMENSION    FORMATS_PS_GIF_PNG_SVG(4)
      DATA         FORMATS_PS_GIF_PNG_SVG /'PS', 'GIF_ANIMATION', 
     +                                     'PNG', 'SVG'/

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
C         Set the projection?
C
          IF (ARG.EQ.'PROJECTION') THEN
              I = I + 1 
              CALL GETARG ( I, PROJECTION ) 
              CALL PSETC ('SUBPAGE_MAP_PROJECTION', PROJECTION)
C
C        Set the device?
C
          ELSEIF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

C             Set the output filename

              IF     (DEVICE.EQ.'PS')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_DEVICE',   'ps_a4')
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'PS_NEW') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'EPS') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'EPS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF_ANIMATION')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
                CALL PSETI ('OUTPUT_GIF_DELAY',     150)
              ELSEIF (DEVICE.EQ.'GIF_MULTI') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PNG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'JPEG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'JPEG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'SVG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'CSVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'CSVG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'KML') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'KML')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)

              ELSEIF (DEVICE.EQ.'BAD') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'BAD')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)

              ELSEIF (DEVICE.EQ.'PS_GIF') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF, 2)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_GIF_PDF') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF_PDF, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_GIF_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF_SVG, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_PNG_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_PNG_SVG, 3)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'PS_GIF_PNG_SVG') THEN
                CALL PSET1C ('OUTPUT_FORMATS',
     +                               FORMATS_PS_GIF_PNG_SVG, 4)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'ALL') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_ALL, 5)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1
C
C        Split the PostScript pages into separate files?
C
          ELSEIF (ARG.EQ.'PS_SPLIT') THEN
                CALL PSETC ('OUTPUT_PS_SPLIT',     'ON')
C
C        Turn on the numbering for the first page?
C
          ELSEIF (ARG.EQ.'FIRST_PAGE_NUMBER') THEN
                CALL PSETC ('OUTPUT_NAME_FIRST_PAGE_NUMBER', 'ON')
C
C         Set the minimal filename number width?
C
          ELSEIF (ARG.EQ.'MINIMAL') THEN
              I = I + 1 
              CALL GETARG (I, MINIMAL) 
              IF     (MINIMAL.EQ.'0')  THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 0)
              ELSEIF (MINIMAL.EQ.'1')  THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 1)
              ELSEIF (MINIMAL.EQ.'2') THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 2)
              ELSEIF (MINIMAL.EQ.'3') THEN
                CALL PSETI ('OUTPUT_FILE_MINIMAL_WIDTH', 3)
              ENDIF
C
C         Set the file number separator?
C
          ELSEIF (ARG.EQ.'SEP') THEN
              I = I + 1 
              CALL GETARG ( I, SEPARATOR ) 
              CALL PSETC ('OUTPUT_FILE_SEPARATOR', SEPARATOR)
C
C        Run using linear contouring?
C
          ELSEIF (ARG.EQ.'LINEAR') THEN
                CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT', 'ON')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'LINEAR')
          ENDIF
          I = I + 1 
          GOTO 20
      ENDIF
C
C     If no device has been set, then use PostScript by default
C
      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME // '.ps'
        CALL PSETC ('PS_DEVICE',    'ps_a4')
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF
C
      END
