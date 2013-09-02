      PROGRAM MAGICS_EXAMPLE_FORTRAN

C     This program shows an example of a simple MAGICS plot.
C     We start MAGICS, load some data, set some plotting
C     attributes then generate the plot. MAGICS is closed at
C     the end.



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PSETC ('OUTPUT_FORMAT',  'PS')
      CALL PSETC ('OUTPUT_NAME',    'using_fortran1')
      CALL PSETC ('PAGE_ID_LINE_USER_TEXT','Using FORTRAN Interface')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Set up and plot the coastlines

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
      CALL PCOAST
      

C     Define and plot the contour   

      CALL PSETC ('CONTOUR_LINE_STYLE',  'DASH')
      CALL PCONT


C     Set up and plot the title text. We just use the default setting
C     which generates an automatic title from the data.

      CALL PTEXT


C     Close MAGICS. It is this command that actually initiates the
C     plotting.

      CALL PCLOSE

      STOP
      END


