C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      PROGRAM TAYLOR

      DIMENSION X(10),Y(10),RAR (10) 
      DATA X/0.2,0.4,0.6,0.7,0.6,0.1,0.45,0.5,0.65,0.7/ 
      DATA Y/0.7,0.4,0.1,0.45,0.5,0.65,0.1,0.2,0.6,0.7/ 
      DATA RAR/2.5,4.,6.,8.,10.,12.,14.,16.,18.,20./ 

      CALL POPEN()
      CALL PARSE_COMMAND_LINE ('taylor_ex')

      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'TAYLOR')
      CALL PSETC ('SYMBOL_POSITION_MODE',   'GRAPH')
      CALL PSETC ('SUBPAGE_FRAME',          'OFF')
      CALL PSETC ('PAGE_ID_LINE',           'OFF')

      CALL PSETC  ('SYMBOL_TYPE','NUMBER') 
      CALL PSET1R ('SYMBOL_INPUT_NUMBER_LIST', RAR, 10) 
      CALL PSET1R ('SYMBOL_INPUT_X_POSITION',  X,   10) 
      CALL PSET1R ('SYMBOL_INPUT_Y_POSITION',  Y,   10) 

      CALL PSETC ('SYMBOL_COLOUR',     'MAGENTA') 
      CALL PSETI ('SYMBOL_MARKER',      15)
      CALL PSETR ('SYMBOL_HEIGHT',      0.5) 
      CALL PSETC ('SYMBOL_TABLE_MODE', 'OFF') 

      CALL PTAYLOR
      CALL PSYMB 

      CALL PSETC ('TEXT_LINE_1', 'Taylor Diagram')
      CALL PTEXT


      CALL PCLOSE()

      END


#include "parse_command_line.h"


