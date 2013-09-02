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
C     This example plots a shaded temperature field, then overlays
C     z500 isolines.
C
      PROGRAM T_SHADED_Z_ISOLINES
C
C     Define our colour palette for the shading
C
      PARAMETER (NLEV=21)
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
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
C     Define our array of output formats
C
      CHARACTER*16 FORMATS_PS_SVG
      DIMENSION    FORMATS_PS_SVG(2)
      DATA         FORMATS_PS_SVG /'PS','SVG'/
C
C     Open MAGICS and set the output file type/name
C     Note that 'PS' is the default so we don't need to 
C     specify it here.
C
      CALL POPEN
      CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_SVG,2)
      CALL PSETC  ('OUTPUT_NAME',    'shaded_isolines')
      CALL PSETC  ('PAGE_ID_LINE_USER_TEXT', 'shading test')
C
C     Area specification (SOUTH, WEST, NORTH, EAST )
C
      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'CYLINDRICAL')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   75.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  70.0)
C
C     First, load and plot the temperature data, shaded
C
C     Pass the data to MAGICS
C
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/t850_fc_12.grib')
      CALL PGRIB
C
C     Set up the coastline attributes
C
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     
C
C     Define and plot the contour     
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
C     Plot the title text and the coastlines
C
      CALL PCOAST
C
C     Now load the Z500 data
C
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/z500_fc_12.grib')
      CALL PGRIB
C
C     Redefine the contouring parameters for the Z500
C
      CALL PSETC  ('CONTOUR',                      'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',              6.0)
      CALL PSETC  ('CONTOUR_LINE_COLOUR',          'BLACK')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR',     'BLACK')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   4)
      CALL PSETC  ('CONTOUR_HILO',                 'ON')
      CALL PSETR  ('CONTOUR_HILO_HEIGHT',           0.25)
      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  40.0)
      CALL PSETC  ('CONTOUR_SHADE',                'OFF')      
      CALL PCONT
C
      CALL PSETC  ('TEXT_LINE_1','T850 (shaded) & Z500')
      CALL PTEXT
      CALL PCLOSE
C
      STOP
      END
