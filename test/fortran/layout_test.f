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
C     This program demonstrates a new feature of Magics++: layout
C
      PROGRAM LAYOUT_TEST
C
      DIMENSION X(11),Y(11)
      DATA X/0.0,1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
      DATA Y/0.0,4.,6.,8.,10.,15.,20.,30.,20.,10.,5./
C
C     Define our array of output formats
C
      CHARACTER*16 FORMATS_PS_SVG
      DIMENSION    FORMATS_PS_SVG(2)
      DATA         FORMATS_PS_SVG /'PS','SVG'/
C
C     OPEN MAGICS
C 
      CALL POPEN
      CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_SVG,2)
      CALL PSETC  ('OUTPUT_NAME',    'layout_test')
      CALL PARSE_COMMAND_LINE ('layout_test')
C
C   Set ID line text
C
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'Layout test')
C      
C     dimensions for A3      
C
      PX = 42.0
      PY = 29.7
      X1 =  0.5
      X2 = 21.5
      Y1 =  0.5
      Y2 = 15.5
C
C     Page locations. We have a 2x3 grid of plots
C
      CALL PSETR('SUPER_PAGE_X_LENGTH',PX)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',PY)
C
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    23.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   51.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  72.0)
C
C     Setup of subpages
C      
      CALL PSETC ('LAYOUT', 'POSITIONAL')
      CALL PSETR ('PAGE_X_LENGTH', 20.0)
      CALL PSETR ('PAGE_Y_LENGTH', 15.0)
      CALL PSETR ('PAGE_X_POSITION', X1)
      CALL PSETR ('PAGE_Y_POSITION', Y2)
C
      CALL MAP_SHADING
C
      CALL PCOAST
C
      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X2)
      CALL PSETR ('PAGE_Y_POSITION', Y2)
      CALL MAP_SHADING_2
C
      CALL PCOAST
C
      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X1)
      CALL PSETR ('PAGE_Y_POSITION', Y1)
      CALL MAP_THREE
C
      CALL PNEW  ('PAGE')
      CALL PSETC ('PAGE_ID_LINE', 'OFF')
      CALL PSETR ('PAGE_X_POSITION', X2)
      CALL PSETR ('PAGE_Y_POSITION', Y1)
      CALL GRAPH_ONE
C
C     Close
C
      CALL PCLOSE
C
      STOP
      END
C
C
      SUBROUTINE MAP_SHADING
C
      PARAMETER (NLEV=18)
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'BLUE_PURPLE',
     +                  'BLUE',
     +                  'GREENISH_BLUE',
     +                  'BLUE_GREEN',
     +                  'BLUISH_GREEN',
     +                  'YELLOW_GREEN',
     +                  'GREENISH_YELLOW',
     +                  'YELLOW',
     +                  'ORANGISH_YELLOW',
     +                  'ORANGE_YELLOW',
     +                  'YELLOWISH_ORANGE',
     +                  'ORANGE',
     +                  'REDDISH_ORANGE',
     +                  'RED_ORANGE',
     +                  'ORANGISH_RED',
     +                  'RED',
     +                  'REDDISH_PURPLE',
     +                  'MAGENTA'/
C
C     Pass the data to MAGICS
C
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/t2m_fc12.grib')
      CALL PGRIB
      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETR  ('CONTOUR_SHADE_MIN_LEVEL',       -50.)
      CALL PSETR  ('CONTOUR_SHADE_MAX_LEVEL',        40.)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',              5.0)
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PCONT
C
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     x  '../data/wind30m_fc12.grib')
      CALL PSETI ('GRIB_WIND_POSITION_1',1)                 
      CALL PSETI ('GRIB_WIND_POSITION_2',2) 
      CALL PGRIB
      CALL PWIND
C
      CALL PRESET('CONTOUR_SHADE_MIN_LEVEL')
      CALL PRESET('CONTOUR_SHADE_MAX_LEVEL')
C
      RETURN
      END
C
C
C
      SUBROUTINE MAP_SHADING_2
C
      PARAMETER (NLEV=21)
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'BLUISH_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'PURPLISH_BLUE',
     +                  'BLUE',
     +                  'GREENISH_BLUE',
     +                  'BLUE_GREEN',
     +                  'BLUISH_GREEN',
     +                  'YELLOW_GREEN',
     +                  'GREENISH_YELLOW',
     +                  'YELLOW',
     +                  'ORANGISH_YELLOW',
     +                  'ORANGE_YELLOW',
     +                  'YELLOWISH_ORANGE',
     +                  'ORANGE',
     +                  'REDDISH_ORANGE',
     +                  'RED_ORANGE',
     +                  'ORANGISH_RED',
     +                  'RED',
     +                  'REDDISH_PURPLE',
     +                  'PURPLE_RED',
     +                  'MAGENTA'/
C
C     Pass the data to MAGICS
C 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/t850_fc_12.grib')
      CALL PGRIB
C
      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETR  ('CONTOUR_SHADE_MIN_LEVEL',       -40.)
      CALL PSETR  ('CONTOUR_SHADE_MAX_LEVEL',        44.)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',              4.0)
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PCONT
C
C     Now load the Z500 data
C
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/z500_fc_12.grib')
      CALL PGRIB
C     Redefine the contouring parameters for the Z500
      CALL PSETC  ('CONTOUR',                      'ON')
      CALL PSETR  ('CONTOUR_INTERVAL',              6.0)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETC  ('CONTOUR_LINE_COLOUR',          'BLACK')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR',     'BLACK')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   4)
      CALL PSETC  ('CONTOUR_HILO',                 'ON')
      CALL PSETR  ('CONTOUR_HILO_HEIGHT',           0.25)
      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  40.0)
      CALL PSETC  ('CONTOUR_SHADE',                'OFF')      
      CALL PCONT
C
      CALL PRESET('CONTOUR_SHADE_MIN_LEVEL')
      CALL PRESET('CONTOUR_SHADE_MAX_LEVEL')
C
      RETURN
      END
C
C
C
      SUBROUTINE MAP_THREE
C
      PARAMETER (NCOLS=64)
       CHARACTER*25 CTAB
       DIMENSION  CTAB (NCOLS)
       DATA       CTAB /'HSL(0,0,1)',
     +                  'HSL(29,0.14,0.92)',
     +                  'HSL(29,0.29,0.83)',
     +                  'HSL(29,0.43,0.75)',
     +                  'HSL(300,0.08,0.92)',
     +                  'HSL(360,0.16,0.84)',
     +                  'HSL(13,0.3,0.75)',
     +                  'HSL(18,0.44,0.67)',
     +                  'HSL(300,0.16,0.83)',
     +                  'HSL(340,0.22,0.75)',
     +                  'HSL(360,0.34,0.67)',
     +                  'HSL(8,0.47,0.58)',
     +                  'HSL(300,0.24,0.75)',
     +                  'HSL(330,0.28,0.67)',
     +                  'HSL(349,0.38,0.58)',
     +                  'HSL(360,0.5,0.5)',
     +                  'HSL(180,0.17,0.92)',
     +                  'HSL(120,0.08,0.84)',
     +                  'HSL(57,0.17,0.75)',
     +                  'HSL(44,0.3,0.67)',
     +                  'HSL(209,0.14,0.84)',
     +                  'HSL(187,0,0.75)',
     +                  'HSL(29,0.15,0.67)',
     +                  'HSL(29,0.29,0.59)',
     +                  'HSL(239,0.16,0.75)',
     +                  'HSL(299,0.08,0.67)',
     +                  'HSL(360,0.17,0.58)',
     +                  'HSL(13,0.3,0.5)',
     +                  'HSL(258,0.21,0.67)',
     +                  'HSL(299,0.16,0.59)',
     +                  'HSL(341,0.22,0.5)',
     +                  'HSL(360,0.33,0.42)',
     +                  'HSL(180,0.34,0.83)',
     +                  'HSL(161,0.22,0.75)',
     +                  'HSL(120,0.16,0.67)',
     +                  'HSL(78,0.21,0.58)',
     +                  'HSL(193,0.3,0.75)',
     +                  'HSL(180,0.17,0.67)',
     +                  'HSL(120,0.08,0.58)',
     +                  'HSL(59,0.16,0.5)',
     +                  'HSL(209,0.29,0.67)',
     +                  'HSL(209,0.15,0.58)',
     +                  'HSL(217,0,0.5)',
     +                  'HSL(29,0.14,0.42)',
     +                  'HSL(224,0.3,0.58)',
     +                  'HSL(237,0.17,0.5)',
     +                  'HSL(299,0.08,0.42)',
     +                  'HSL(360,0.16,0.33)',
     +                  'HSL(180,0.5,0.75)',
     +                  'HSL(169,0.38,0.67)',
     +                  'HSL(150,0.28,0.58)',
     +                  'HSL(120,0.24,0.5)',
     +                  'HSL(188,0.47,0.67)',
     +                  'HSL(180,0.34,0.59)',
     +                  'HSL(160,0.22,0.5)',
     +                  'HSL(120,0.16,0.42)',
     +                  'HSL(198,0.44,0.58)',
     +                  'HSL(193,0.3,0.5)',
     +                  'HSL(180,0.17,0.42)',
     +                  'HSL(120,0.08,0.33)',
     +                  'HSL(209,0.43,0.5)',
     +                  'HSL(209,0.29,0.42)',
     +                  'HSL(209,0.14,0.33)',
     +                  'HSL(191,0,0.25)'/
       PARAMETER (NLEV=65)
       DIMENSION  RLEV (NLEV)
       DATA       RLEV /-0.5,
     +                  0.5,
     +                  1.5,
     +                  2.5,
     +                  3.5,
     +                  4.5,
     +                  5.5,
     +                  6.5,
     +                  7.5,
     +                  8.5,
     +                  9.5,
     +                  10.5,
     +                  11.5,
     +                  12.5,
     +                  13.5,
     +                  14.5,
     +                  15.5,
     +                  16.5,
     +                  17.5,
     +                  18.5,
     +                  19.5,
     +                  20.5,
     +                  21.5,
     +                  22.5,
     +                  23.5,
     +                  24.5,
     +                  25.5,
     +                  26.5,
     +                  27.5,
     +                  28.5,
     +                  29.5,
     +                  30.5,
     +                  31.5,
     +                  32.5,
     +                  33.5,
     +                  34.5,
     +                  35.5,
     +                  36.5,
     +                  37.5,
     +                  38.5,
     +                  39.5,
     +                  40.5,
     +                  41.5,
     +                  42.5,
     +                  43.5,
     +                  44.5,
     +                  45.5,
     +                  46.5,
     +                  47.5,
     +                  48.5,
     +                  49.5,
     +                  50.5,
     +                  51.5,
     +                  52.5,
     +                  53.5,
     +                  54.5,
     +                  55.5,
     +                  56.5,
     +                  57.5,
     +                  58.5,
     +                  59.5,
     +                  60.5,
     +                  61.5,
     +                  62.5,
     +                  63.5/
      CHARACTER TITLE*256

C     First, load and plot the temperature data, shaded
C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/cloudcover.grib')
      CALL PGRIB

C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     

C     Define the contour     

      CALL PSETC  ('LEGEND',                       'OFF')
      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'CELL_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NCOLS)
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PCONT
      
C     Plot the title text and the coastlines

      TITLE = 'Z700 \\\CL' //  
     +        CTAB(4)  // '\\\Low,\\\CL' //
     +        CTAB(16) // '\\\ L+M,\\\CL' //
     +        CTAB(13) // '\\\ Medium,\\\CL' //
     +        CTAB(61) // '\\\ M+H,\\\CL' //
     +        CTAB(49) // '\\\ High,\\\CL' //
     +        CTAB(52) // '\\\ H+L,\\\CLBLACK\\\ H+M+L' //
     +        '\\\CLR\\\ clouds'
C
      CALL PSETR  ('TEXT_FONT_SIZE', 0.4)
      CALL PSETC  ('TEXT_LINE_1',TITLE)
      CALL PTEXT
      CALL PCOAST
C
      RETURN
      END
C
C
C
	SUBROUTINE GRAPH_ONE
C
C	PLOT SIMPLE CURVE WITH SYMBOLS
C
	DIMENSION X(11),Y(11),Y2(11),Y3(11),X2(10),YL(10),YU(10),
     + YL2(10),YU2(10)
	DATA X/0.0,1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
	DATA  Y/0.0,15.,6.,18.,10.,15.,27.,37.,20.,10.,15./
	DATA Y2/3.0,14.,2.,8.,13.,75.,57.,45.,26.,52.,58./
	DATA Y3/2.0,10.,22.,12.,32.,12.,24.,38.,23.,34.,45./
	DATA X2/1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
	DATA YL /10*0.0/
	DATA YU/3.0,5.,9.,1.,2.,10.,19.,18.,10.,18./
	DATA YU2 /13.0,15.,19.,11.,22.,20.,29.,28.,20.,21./
C
C	SET UP AXIS SYSTEM
C
	CALL PSETC ('SUBPAGE_MAP_PROJECTION','NONE')  
	CALL PSETC ('AXIS_ORIENTATION','HORIZONTAL')
	CALL PSETC ('AXIS_POSITION','BOTTOM')
	CALL PSETR ('AXIS_MIN_VALUE',0.0)
	CALL PSETR ('AXIS_MAX_VALUE',10.5)
	CALL PSETR ('AXIS_TICK_INTERVAL',1.0)
	CALL PAXIS
	CALL PSETC ('AXIS_ORIENTATION','VERTICAL')
	CALL PSETC ('AXIS_POSITION','LEFT')
	CALL PSETR ('AXIS_MIN_VALUE',0.0)
	CALL PSETR ('AXIS_MAX_VALUE',100.0)
	CALL PSETR ('AXIS_TICK_INTERVAL',5.0)
	CALL PAXIS
C
C	SET GRAPH PARAMETERS
C 
C     Set up our data for the boxplots
c
      CALL PSETC ('GRAPH_SHADE_COLOUR','SKY')
      CALL PSETR ('boxplot_box_width',0.5)
      CALL PSET1R('BOXPLOT_POSITIONS',
     + (/2., 4., 6., 8. /), 4 )
      CALL PSET1R('BOXPLOT_MINIMUM_VALUES', 
     + (/10., 30., 52., 40./), 4)
      CALL PSET1R('BOXPLOT_MAXIMUM_VALUES', 
     + (/50., 72., 87., 81./), 4)
      CALL PSET1R('BOXPLOT_MEDIAN_VALUES', 
     + (/25., 42., 77., 46./), 4)
      CALL PSET1R('BOXPLOT_BOX_UPPER_VALUES', 
     + (/35., 56., 82.5, 67./), 4)
      CALL PSET1R('BOXPLOT_BOX_LOWER_VALUES', 
     + (/21.3, 34., 66.1, 45./), 4)
C
C     Draw the boxplots using the default plotting attributes
C
	CALL PBOXPLOT
C      
	CALL PSETC ('GRAPH_TYPE','BAR')
	CALL PSETC ('GRAPH_SHADE','ON')
	CALL PSETC ('GRAPH_SHADE_STYLE','HATCH')
	CALL PSETI ('graph_shade_hatch_index',0)
C
	CALL PSETC ('GRAPH_SHADE_COLOUR','ORANGE')
	CALL PSET1R ('GRAPH_BAR_X_VALUES',X2,10)
	CALL PSET1R ('GRAPH_BAR_Y_LOWER_VALUES',YL,10)
	CALL PSET1R ('GRAPH_BAR_Y_UPPER_VALUES',YU,10)
	CALL PGRAPH
C
	CALL PSETC ('GRAPH_SHADE_STYLE','DOT')
	CALL PSETC ('GRAPH_SHADE_COLOUR','BLUE')
	CALL PSET1R ('GRAPH_BAR_X_VALUES',X2,10)
	CALL PSET1R ('GRAPH_BAR_Y_LOWER_VALUES',YU,10)
	CALL PSET1R ('GRAPH_BAR_Y_UPPER_VALUES',YU2,10)
	CALL PGRAPH
C
	CALL PSETC ('GRAPH_TYPE','CURVE')
	CALL PSETC ('GRAPH_SYMBOL','ON')
	CALL PSETI ('GRAPH_SYMBOL_MARKER_INDEX',18)
	CALL PSETC ('GRAPH_LINE_COLOUR','RED')
	CALL PSET1R ('GRAPH_CURVE_X_VALUES',X,11)
	CALL PSET1R ('GRAPH_CURVE_Y_VALUES',Y,11)
	CALL PGRAPH

	CALL PSETC ('GRAPH_LINE_COLOUR','BLUE')
	CALL PSET1R ('GRAPH_CURVE_X_VALUES',X,11)
	CALL PSET1R ('GRAPH_CURVE_Y_VALUES',Y2,11)
	CALL PGRAPH

	CALL PSETC ('GRAPH_LINE_COLOUR','GREEN')
	CALL PSET1R ('GRAPH_CURVE_X_VALUES',X,11)
	CALL PSET1R ('GRAPH_CURVE_Y_VALUES',Y3,11)
	CALL PGRAPH

	CALL PSETC ('TEXT_LINE_1','Graph Plotting in '//
     x             'bar and line mode')
	CALL PTEXT

	RETURN
	END

C --------------------------------------------------------------------
C     PARSE_COMMAND_LINE
C     Checks the command-line for any arguments.
C     Arguments can come in pairs. Currently supported arguments are:
C     PROJECTION 
C     DEVICE 
C     e.g. Run the program with:
C      PROJECTION POLAR_STEREOGRAPHIC
C      PROJECTION CYLINDRICAL   DEVICE SVG
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

      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      ID_TEXT = ''

      NUM_ARGS = IARGC()

      I = 1

20    IF (I.LE.NUM_ARGS) THEN
          CALL GETARG ( I, ARG ) 
 
C         Set the projection?
          IF (ARG.EQ.'PROJECTION') THEN
              I = I + 1 
              CALL GETARG ( I, PROJECTION ) 
              CALL PSETC ('SUBPAGE_MAP_PROJECTION', PROJECTION)
	      
C        Set the device?
          ELSEIF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 

              CALL PSETC ('OUTPUT_FORMAT', DEVICE )
              CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
	      CALL PSETC ('OUTPUT_SVG_COMPRESS', 'ON')

C        Run using linear contouring?

          ELSEIF (ARG.EQ.'LINEAR') THEN
                CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT', 'ON')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'LINEAR')
          ENDIF

          I = I + 1 
          GOTO 20
      ENDIF
      END
