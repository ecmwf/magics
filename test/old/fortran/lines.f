C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

   	program axis01
c
c	this program demonstrates axis plotting
c
c	open magics
c
	dimension xval(2), yval(2)
	call popen
	
	call parse_command_line ('lines')
	call psetc ('page_id_line_user_text','line thickness test')
	call psetc ('subpage_frame','on')
	call psetc ('subpage_map_projection','none')
c
c	set page layout values
c
	call psetr ('subpage_x_position',1.0)
	call psetr ('subpage_y_position',2.0)
	call psetr ('subpage_x_length',28.0)
	call psetr ('subpage_y_length',17.0)
    
c	draw regular axis with mostly default values
c
	call psetr ('axis_min_value',1.0)
	call psetr ('axis_max_value',16.0)	
	call psetr ('axis_tick_interval',1.0)	
	call paxis
c
c	draw logarithmic axis with mostly default values
c
	call psetc ('axis_orientation','vertical')

	call psetr ('axis_min_value',1.)
	call psetr ('axis_max_value',0.)
	call psetc ('axis_tick', 'off')
	call psetc ('axis_tick_label', 'off')
	call psetc ('text_line_1','Thickness and line style')
	call pseti ('text_line_count', 1)
	call paxis
      
      yval(1) = 0
      yval(2) = 1
      
      do i = 1, 15
        
        xval(1) = i
        xval(2) = i
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'solid')
        call pgraph
        xval(1) = i + 0.2
        xval(2) = i + 0.2
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'dash')
        call pgraph
        xval(1) = i + 0.4
        xval(2) = i + 0.4
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'dot')
        call pgraph
        xval(1) = i + 0.6
        xval(2) = i + 0.6
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'CHAIN_DOT')
        call pgraph
        xval(1) = i + 0.8
        xval(2) = i + 0.8
        call pset1r ('graph_curve_x_values', xval, 2)
        call pset1r ('graph_curve_y_values',yval, 2)
        call pseti ('graph_line_thickness', i)
        call psetc ('graph_line_style', 'CHAIN_DASH')
        call pgraph
        
     
        
        
      end do
c
c	plot default regular and logarithmic axis
c
c
        call ptext
	call pclose
c
	end

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
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME

      CHARACTER*10 FORMATS_PS_GIF
      DIMENSION    FORMATS_PS_GIF(2)
      DATA         FORMATS_PS_GIF /'PS', 'GIF'/

      CHARACTER*10 FORMATS_PS_GIF_PDF
      DIMENSION    FORMATS_PS_GIF_PDF(3)
      DATA         FORMATS_PS_GIF_PDF /'PS', 'GIF', 'PDF'/

      INTEGER NUM_ARGS
      INTEGER DEVICE_SET

      DEVICE_SET = 0

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
              ELSEIF (DEVICE.EQ.'PNG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PNG')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'SVG') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'SVG')
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
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
            
            DEVICE_SET = 1


C        Split the PostScript pages into separate files?

          ELSEIF (ARG.EQ.'PS_SPLIT') THEN
                CALL PSETC ('OUTPUT_PS_SPLIT',     'ON')


C        Turn on the numbering for the first page?

          ELSEIF (ARG.EQ.'FIRST_PAGE_NUMBER') THEN
                CALL PSETC ('OUTPUT_NAME_FIRST_PAGE_NUMBER', 'ON')


C        Run using linear contouring?

          ELSEIF (ARG.EQ.'LINEAR') THEN
                CALL PSETC ('CONTOUR_METHOD', 'LINEAR')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT_PLOT', 'ON')
                CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'LINEAR')
          ENDIF

          I = I + 1 
          GOTO 20
      ENDIF
      


C     If no device has been set, then use PostScript by default

      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME // '.ps'
        CALL PSETC ('PS_DEVICE',    'ps_a4')
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF

      END
      
