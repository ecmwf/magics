          PROGRAM CONTAUTO

C     This program demonstrates magics contouring facilities.
C     Set to 'AUTOMATIC', Magics++ attempts to find the 'best'
C     contouring parameters for the trade-off between quality and speed. 


      PARAMETER (NLEV=8)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /-7., -5., -3., -1., 1., 3., 5., 9./

C     Open MAGICS and set the output device
      CALL POPEN
      call psetc ('device','ps')
      call psetc ('ps_device','ps_a4')
      CALL PSETC ('PS_FILE_NAME', 'toto1.ps')

C     Set up the coastline attributes
c      CALL PSETC ('MAP_COASTLINE_RESOLUTION', 'HIGH')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID', 'OFF')
      CALL PSETC ('MAP_LABEL', 'OFF')

C     Pass the data to MAGICS
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/mark.grb')
c     x            '/tmp/cgk/data/MetErrors/MarkRodwell/s.grb')
      CALL PSETC ('grib_scaling', 'off')
      CALL PGRIB

C     Define the contour
      CALL PSETC ('CONTOUR',               'OFF')
      CALL PSETC ('contour_level_selection_type','level_list')
      CALL PSET1R ('contour_level_list', RLEV, NLEV)
      CALL PSETC ('contour_shade', 'on')
      CALL PSETC ('contour_shade_method', 'area_fill')
c      CALL PSETC ('contour_shade_colour_method', 'list')
c      CALL PSETC ('contour_shade_colour_list', shade_colours)
      CALL PSETC ('contour_shade_label_blanking', 'on')
      CALL PSETC ('contour_label', 'off')
      CALL PSETC ('contour_highlight', 'off')
      CALL PSETC ('contour_hilo', 'off')
      CALL PCONT

C     Plot the coastlines
      CALL PCOAST

      CALL PCLOSE

      STOP
      END
