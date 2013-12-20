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
C     No data is plotted, just the global coastline
C     We also demonstrate how to specify multiple output
C     formats. This increases efficiency compared to either running
C     the program multiple times or using an external conversion
C     tool.
C
      PROGRAM COAST_ONLY
C
C     Define our array of output formats
C
      CHARACTER*16 FORMATS_PS_SVG
      DIMENSION    FORMATS_PS_SVG(2)
      DATA         FORMATS_PS_SVG /'PS','SVG'/
C
C     Open MAGICS and set the output file types/names
C     Here we specify three output formats.
C
      CALL POPEN
      CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_SVG,2)
      CALL PSETC  ('OUTPUT_NAME',    'coast_only')
C
C     Set up the coastline attributes
C
      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     
C
C     Plot the coastlines and then close
C
      CALL PCOAST
      CALL PCLOSE
C
      STOP
      END
