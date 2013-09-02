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
C     This example demonstrates the plotting of wind arrows
C     and flags.
C
      PROGRAM WIND
C
C     Open MAGICS and set the output file type/name
C     Note that 'PS' is the default so we don't need to 
C     specify it here.
C
      CALL POPEN
      CALL PSETC ('OUTPUT_FORMAT',  'PS')
      CALL PSETC ('OUTPUT_NAME',    'wind')
C
C     Set up all the parameters we'll use in all the examples
C
      CALL PSETC ('MAP_COASTLINE_COLOUR',            'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',                 'GREY')
      CALL PSETC ('MAP_LABEL_COLOUR',                'GREY')
C
C     Area specification (SOUTH, WEST, NORTH, EAST)
C
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   75.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  70.0)
C
C     Tell MAGICS where the U/V fields are in the file
C     Note that these values are the defaults and so are not
C     actually necessary in this example
C
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
C
C     Load the data file
C
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/uv500.grb')
      CALL PGRIB
C
C     --- Plot wind arrows with default settings ---
C
      CALL PSETI ('TEXT_LINE_COUNT',  2)
      CALL PSETC ('TEXT_LINE_1',     'Wind arrows, default settings')
      CALL PSETC ('TEXT_LINE_2',     '')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT
C
C     Close MAGICS
C
      CALL PCLOSE
      END
