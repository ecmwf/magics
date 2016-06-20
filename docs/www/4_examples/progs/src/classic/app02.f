C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM app02 
       call popen
      CALL PSETC ('PS_DEVICE','ps_a4')
      CALL PSETC ('PS_FILE_NAME','app02.ps')
      CALL PSETR('SUPER_PAGE_X_LENGTH',21.0)
      CALL PSETR('SUPER_PAGE_Y_LENGTH',29.7)
      CALL PSETR('PAGE_X_LENGTH',21.0)
      CALL PSETR('PAGE_Y_LENGTH',29.7)
      CALL PSETR('SUBPAGE_X_LENGTH',21.0)
      CALL PSETR('SUBPAGE_Y_LENGTH',29.7)
      CALL PSETR('SUBPAGE_X_POSITION',0.0)
      CALL PSETR('SUBPAGE_Y_POSITION',0.0)
      CALL PSETC('SYMBOL_POSITION_MODE','PAPER')
      CALL PSETC('SUBPAGE_MAP_PROJECTION','NONE')
      CALL PSETC('PAGE_ID_LINE','OFF')              
      CALL PSETC('SUBPAGE_FRAME','OFF')             
      CALL PSETC('PAGE_FRAME','OFF')             
      CALL ONE
      CALL FIVE
      CALL PCLOSE
      STOP
      END
      SUBROUTINE ONE
      DIMENSION X(28),Y(28),RAR(28)
      DATA X/14*13.0,14*3.0/                                      
c     DATA Y/2.0,3.25,4.5,5.75,7.,8.25,9.5,10.75,12.,13.25,14.5,
c    x 15.75,17.,18.25,19.5,20.75,22.,23.25,24.5,25.75,27./
      DATA Y/2.0,3.75,5.5,7.25,9.,10.75,12.5,14.25,16.,17.75,19.5,
     x 21.25,23.,24.75,24.75,23.,21.25,19.5,17.75,16.,14.25,
     x 12.5,10.75,9.,7.25,5.5,3.75,2.0/

      DATA RAR/27.,26.,25.,24.,23.,22.,21.,
     X20.,19.,18.,17.,16.,15.,14.,13.,12.,11.,10.,9.,
     x 8.,7.,6.,5.,4.,3.,2.,1.,0./
      CALL PSETC('SYMBOL_TYPE','NUMBER')
      CALL PSET1R('SYMBOL_INPUT_NUMBER_LIST',RAR,28)
      CALL PSET1R('SYMBOL_INPUT_X_POSITION',X,28)
      CALL PSET1R('SYMBOL_INPUT_Y_POSITION',Y,28)
      CALL PSETC('SYMBOL_COLOUR','MAGENTA')
      CALL PSETR('SYMBOL_HEIGHT',0.5)
      CALL PSETC('SYMBOL_TABLE_MODE','OFF')
      CALL PSETC('PAGE_ID_LINE_USER_TEXT','TESTS/SYM01')
      CALL PSETC('TEXT_LINE_1','S>YMBOL <NUMBER> PLOTTING IN '//
     X' <INDIVIDUAL> MODE')
      CALL PSYMB
c     CALL PTEXT
      RETURN
      END
      SUBROUTINE FIVE
      DIMENSION XX(28),YY(28)
      DIMENSION IMAR(28)
      DATA IMAR/27,26,25,24,23,22,21,
     x20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,
     x    2,1,0/
      DATA XX/14*16.0,14*6.0/
c     DATA YY/2.0,3.25,4.5,5.75,7.,8.25,9.5,10.75,12.,13.25,14.5,
c    x 15.75,17.,18.25,19.5,20.75,22.,23.25,24.5,25.75,27./
      DATA YY/2.0,3.75,5.5,7.25,9.,10.75,12.5,14.25,16.,17.75,19.5,
     x 21.25,23.,24.75,24.75,23.,21.25,19.5,17.75,16.,14.25,
     x 12.5,10.75,9.,7.25,5.5,3.75,2.0/

      CALL PSET1R('SYMBOL_INPUT_X_POSITION',XX,28)
      CALL PSET1R('SYMBOL_INPUT_Y_POSITION',YY,28)
      CALL PSETC('SYMBOL_TABLE_MODE','OFF')
      CALL PSETC('LEGEND','OFF')
      CALL PSETC('LEGEND_ENTRY','OFF')
      CALL PSETC('SYMBOL_TYPE','MARKER')
      CALL PSET1I('SYMBOL_INPUT_MARKER_LIST',IMAR,28)
      CALL PSETC('SYMBOL_COLOUR','GREEN')
      CALL PSETR('SYMBOL_HEIGHT',0.4)
      CALL PSETC('TEXT_LINE_1','S>YMBOL <MARKER> PLOTTING IN '//
     X' <INDIVIDUAL> MODE')
c     CALL PTEXT
      CALL PSYMB
      RETURN
      END
