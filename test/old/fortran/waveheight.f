C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

          PROGRAM fc_plot

!     Program written by Bjorn Hansen  Wed May 12 13:10:29 BST 1999

      IMPLICIT NONE !

      INTEGER            :: jf=0, ib=1, il=2
      CHARACTER (LEN=19) :: cl_map_projection = 'CYLINDRICAL'
      CHARACTER (LEN=19) :: cl_device = 'ps_vax_c'

c      READ *, ib, il, cl_map_projection, cl_device
      ib = 1
      il = 1
      cl_map_projection = 'CYLINDRICAL'
      cl_device = 'ps_vax_a3_c'
      
      IF ( il == 0 ) il = ib

      WRITE(*,'("Device....: ", A)') cl_device(:LEN_TRIM(cl_device))
      WRITE(*,'("Projection: ", A)')
     x         cl_map_projection(:LEN_TRIM(cl_map_projection))
      WRITE(*,'("Field ", I2, " to ", I2 )') ib, il

      CALL layout(cl_device, cl_map_projection )
      fields: DO jf=ib, il
        CALL draw(jf)
      ENDDO fields
      CALL pclose

      END PROGRAM fc_plot



      SUBROUTINE layout (cd_device, cd_map_projection)
      IMPLICIT NONE!

      CHARACTER (LEN=*) ::  cd_map_projection
      CHARACTER (LEN=*) ::  cd_device

      CHARACTER (LEN=2), DIMENSION(2) ::  cla = (/'a3', 'a4'/)

      REAL, DIMENSION(2) :: zs_pxl = (/42.0,  21.0 /)
      REAL, DIMENSION(2) :: zs_pyl = (/29.7,  29.7 /)

      REAL, DIMENSION(2) :: z__pxl = (/21.0,  29.7 /)
      REAL, DIMENSION(2) :: z__pyl = (/14.84, 29.7 /)

      INTEGER           ::  isp = 2

      IF (index(cd_device, 'a3') == 7 .OR.
     x    index(cd_device, 'a3') == 8 ) isp = 1

      CALL popen
      CALL PARSE_COMMAND_LINE ('metops_wave_height')
c      CALL psetc ('PS_DEVICE',                     cd_device         )
c      CALL pseti ('PS_RESOLUTION',                   400             )
c      CALL psetc ('PS_ACTION',                     'SHOWPS'          )
        
      CALL psetc ('PLOT_START',                    'TOP'             )
      CALL psetc ('PLOT_DIRECTION',                'HORIZONTAL'      )

      CALL psetc ('SUPER_PAGE_FRAME',              'ON'              )
      CALL psetc ('SUPER_PAGE_FRAME_COLOUR',       'RED'             )
      CALL psetr ('SUPER_PAGE_X_LENGTH',            zs_pxl(isp)      )
      CALL psetr ('SUPER_PAGE_Y_LENGTH',            zs_pyl(isp)      )

      CALL psetc ('PAGE_ID_LINE_USER_TEXT',        'bjorn hansen'    )
      CALL psetc ('PAGE_FRAME',                    'ON'              )
      CALL psetc ('PAGE_FRAME_COLOUR',             'BLACK'           )
      CALL psetr ('PAGE_X_LENGTH',                   21.0            )
      CALL psetr ('PAGE_Y_LENGTH',                   14.84           )

      CALL psetr ('SUBPAGE_Y_LENGTH',                12.8            )
      CALL psetr ('SUBPAGE_X_POSITION',               1.0            )
      CALL psetr ('SUBPAGE_Y_POSITION',               0.8            )
c      CALL psetr ('SUBPAGE_LOWER_LEFT_LONGITUDE',   120.0            )
c      CALL psetr ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  120.0            )
c      CALL psetr ('SUBPAGE_LOWER_LEFT_LATITUDE',    -90.0            )
c      CALL psetr ('SUBPAGE_UPPER_RIGHT_LATITUDE',    90.0            )
c      CALL psetc ('SUBPAGE_MAP_PROJECTION',        cd_map_projection )
c      CALL psetc ('SUBPAGE_MAP_AREA_DEFINITION',   'CENTRE'          )
c      CALL psetr ('SUBPAGE_MAP_CENTRE_LONGITUDE',   -35.0            )
c      CALL psetr ('SUBPAGE_MAP_CENTRE_LATITUDE',     37.5            )
c      CALL psetr ('SUBPAGE_MAP_VERTICAL_LONGITUDE', -12.5            )
c      CALL psetr ('SUBPAGE_MAP_SCALE',               80.0 * 10.0 ** 6)

      CALL psetc ('MAP_COASTLINE',                 'ON'           )
      CALL psetc ('MAP_COASTLINE_COLOUR',          'magenta'        )
      CALL psetc ('MAP_COASTLINE_RESOLUTION',      'HIGH'         )
      CALL pseti ('MAP_COASTLINE_THICKNESS',          2           )
      CALL psetc ('MAP_COASTLINE_RESOLUTION',      'HIGH'         )
      CALL psetc ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM'      )
      CALL psetr ('MAP_LABEL_HEIGHT',                 0.19        )
      CALL psetc ('MAP_GRID_COLOUR',               'BLACK'        )
      CALL psetc ('MAP_GRID_LINE_STYLE',           'DOT'          )
      CALL psetr ('MAP_GRID_LONGITUDE_REFERENCE',     0.0         )
      CALL psetr ('MAP_GRID_LATITUDE_INCREMENT',     10.0         )
      CALL psetc ('MAP_GRID_LINE_STYLE',           'DOT'          )

      CALL psetc ('GRIB_INPUT_TYPE',               'FILE'         )
      CALL psetc ('GRIB_INPUT_FILE_NAME',   'data/wave_height.grib')
c      CALL pseti ('GRIB_INPUT_FILE_UNIT',            23           )
      CALL psetc ('GRIB_TEXT_EXPERIMENT',          'ON'           )
c      CALL psetc ('PS_FILE_NAME',                  'out.ps'       )

      IF (cd_map_projection == 'POLAR_STEREOGRAPHIC') THEN
c    !WRITE(*, '(" psfile: ",a10)') 'law.'//cla(isp)//'.ps'
c    !CALL psetc ('PS_FILE_NAME',             'law.'//cla(isp)//'.ps')
        CALL psetr ('SUBPAGE_X_LENGTH',                17.75       )
        CALL pseti ('MAP_LABEL_LONGITUDE_FREQUENCY',    1          )
        CALL pseti ('MAP_LABEL_LATITUDE_FREQUENCY',     1          )
        CALL psetr ('MAP_GRID_LATITUDE_REFERENCE',     30.0        )
        CALL psetr ('MAP_GRID_LONGITUDE_INCREMENT',    10.0        )
        CALL psetr ('WIND_THINNING_FACTOR',            6.0         )
      ELSE
c    !WRITE(*, '(" psfile: ",a10)') 'o.'//cla(isp)//'.ps'
c    !CALL psetc ('PS_FILE_NAME',             'o.'//cla(isp)//'.ps'  )
        CALL psetr ('SUBPAGE_X_LENGTH',                19.00        )
        CALL pseti ('MAP_LABEL_LONGITUDE_FREQUENCY',    2           )
        CALL pseti ('MAP_LABEL_LATITUDE_FREQUENCY',     2           )
        CALL psetr ('MAP_GRID_LATITUDE_REFERENCE',      0.0         )
        CALL psetr ('MAP_GRID_LONGITUDE_INCREMENT',    20.0         )
        CALL psetr ('WIND_THINNING_FACTOR',            4.0          )
      ENDIF
      RETURN
      END SUBROUTINE layout



      SUBROUTINE draw (kf)
      IMPLICIT NONE!
  
      INTEGER           :: kf, jl, jj1, jj2

      INTEGER, PARAMETER:: jpl=9, jpr=512

      CHARACTER (LEN=10), DIMENSION(6)::  colo

      REAL                    :: zhight = 0.25
      REAL,    DIMENSION(jpl) :: zfcl, zden, zdos=0.02, zang=45.0
      INTEGER, DIMENSION(jpl) :: icol
      INTEGER, DIMENSION(jpr) :: ib1par, ib2par

      DATA zfcl   /  0., 0.5,  1., 1.5,  2., 3., 4., 8., 25./
c  ! DATA zden   /100., 300., 600., 300., 600., 300., 600., 600./
c  ! DATA icol   /  4,    4,    4,    5,    5,    3,    3,    6 /
c  ! Fix for Linux cluster, Cihan 11/2005
      DATA zden   /100., 300., 600., 300., 600., 300., 600., 600., 600./
      DATA icol   /  4,    4,    4,    5,    5,    3,    3,    6, 6 /
  
      colo = ' '
      colo(3) = 'BLUE'
      colo(4) = 'YELLOW'
      colo(5) = 'CYAN'
      colo(6) = 'MAGENTA'

        WRITE(*,'(" Field ", I2, " ", $)') kf ; CALL flush(6)
        jj1=(kf-1)*2+1
        jj2=kf*2
        CALL psetc ('MAP_COASTLINE_LAND_SHADE',       'ON'     )
        CALL pcoast
        CALL psetr ('GRIB_MISSING_VALUE_INDICATOR',    0.0     )
        CALL pset1i ('GRIB_PRODUCT_BLOCK',IB1PAR,      jpr     )
        CALL pset1i ('GRIB_GRID_BLOCK',IB2PAR,         jpr     )
        CALL pseti ('GRIB_FIELD_POSITION',             jj1     )
c        CALL pgrib

c    ! Draw the shadings.
c    ! ------------------
        CALL psetc ('LEGEND',                         'ON'          )
        CALL psetc ('LEGEND_BORDER',                  'OFF'         )
        CALL psetc ('LEGEND_BORDER_COLOUR',           'BLACK'       )
        CALL psetc ('LEGEND_ENTRY',                   'ON'          )
        CALL psetc ('LEGEND_TITLE',                   'ON'          )
        CALL psetc ('LEGEND_DISPLAY_TYPE',            'CONTINUOUS'  )
        CALL psetc ('LEGEND_USER_TEXT',               'cllutxt'     )
        CALL psetc ('LEGEND_TITLE_TEXT',              'm'           )
        CALL psetc ('LEGEND_TEXT_QUALITY',            'HIGH'        )
        CALL psetc ('LEGEND_TEXT_COLOUR',             'BLACK'       )
        CALL psetc ('LEGEND_AUTOMATIC_TEXT_EXTREMA',  'BOTH'        )
        CALL psetr ('LEGEND_BOX_Y_LENGTH',             0.3          )
        CALL psetr ('LEGEND_TEXT_MAXIMUM_HEIGHT',      0.25         )
        CALL psetr ('LEGEND_ENTRY_MAXIMUM_HEIGHT',     0.25         )
        CALL psetc ('CONTOUR',                        'OFF'         )
        CALL psetc ('CONTOUR_SHADE',                  'ON'          )
        CALL psetc ('CONTOUR_LABEL',                  'OFF'         )
        CALL psetc ('CONTOUR_HILO',                   'OFF'         )
c        shades: DO jl = 2, jpl - 1
         shades: DO jl = 2, 1
          CALL psetr ('CONTOUR_REFERENCE_LEVEL',         zfcl(jl)      )
          CALL psetr ('CONTOUR_SHADE_MIN_LEVEL_DENSITY', zden(jl)      )
          CALL psetr ('CONTOUR_SHADE_MAX_LEVEL_DENSITY', zden(jl)      )
          CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
          CALL PSETC ('CONTOUR_SHADE_METHOD',     'AREA_FILL')
          CALL psetr ('CONTOUR_SHADE_DOT_SIZE',          zdos(jl)      )
          CALL psetr ('CONTOUR_SHADE_ANGLE',             zang(jl)      )
          CALL psetr ('CONTOUR_MIN_LEVEL',               zfcl(jl)      )
          CALL psetr ('CONTOUR_MAX_LEVEL',               zfcl(jl+1)    )
          CALL psetr ('CONTOUR_SHADE_MIN_LEVEL',         zfcl(jl)      )
          CALL psetr ('CONTOUR_SHADE_MAX_LEVEL',         zfcl(jl+1)    )
          CALL PSETC('CONTOUR_SHADE_MIN_LEVEL_COLOUR',   colo(icol(jl)))
          CALL PSETC('CONTOUR_SHADE_MAX_LEVEL_COLOUR',   colo(icol(jl)))

          CALL psetr ('CONTOUR_INTERVAL',               
     x                 zfcl(jl+1)-zfcl(jl) )
          CALL pcont
          WRITE(*, '("l.",$)') ; CALL flush(6)
        ENDDO shades
        WRITE(*, '("-")')

c    ! Draw the contour lines.
c    ! -----------------------
        CALL psetc ('LEGEND_ENTRY',                   'OFF'           )
        CALL psetc ('CONTOUR',                        'ON'            )
        CALL psetc ('CONTOUR_SHADE',                  'OFF'           )
        CALL psetc ('CONTOUR_HIGHLIGHT',              'OFF'           )
        CALL psetc ('CONTOUR_LABEL',                  'ON'            )
        CALL psetc ('CONTOUR_LABEL_QUALITY',          'HIGH'          )
        CALL psetr ('CONTOUR_LABEL_HEIGHT',            zhight         )
        CALL pseti ('CONTOUR_LABEL_FREQUENCY',         1              )
        CALL psetr ('CONTOUR_MIN_LEVEL',               zfcl(2)        )
        CALL psetc ('CONTOUR_LEVEL_SELECTION_TYPE',   'LEVEL_LIST'    )
        CALL pset1r('CONTOUR_LEVEL_LIST',              zfcl(2),jpl-1  )
        CALL psetc ('CONTOUR_LINE_COLOUR',            'BLUE'          )
        CALL pseti ('CONTOUR_HIGHLIGHT_FREQUENCY',     5              )
        CALL pseti ('CONTOUR_HIGHLIGHT_THICKNESS',     2              )
c        CALL psetc ('CONTOUR_METHOD',                 'C3'            )
c        CALL pcont

c    ! Draw the arrows.
c    ! ----------------
        CALL psetc ('LEGEND',                         'ON'         )
        CALL psetc ('LEGEND_ENTRY',                   'ON'         )
        CALL psetc ('LEGEND_USER_TEXT',               '10m'        )
        CALL psetc ('LEGEND_TEXT_COMPOSITION',        'BOTH'       )
        CALL psetc ('WIND_ARROW_LEGEND',              'OFF'        )
        CALL psetr ('WIND_ARROW_UNIT_VELOCITY',       10.0         )
        CALL psetc ('WIND_ARROW_COLOUR',              'BLACK'      )
        CALL pseti ('GRIB_WIND_POSITION_1',           jj1          )
        CALL pseti ('GRIB_WIND_POSITION_2',           jj2          )
        CALL psetc ('GRIB_WIND_MODE',                 'SD')
        WRITE(*,'("UV ", I2,I2 )') jj1, jj2
c        CALL pgrib
c        CALL pwind
        CALL pseti ('TEXT_LINE_COUNT',                 2           )
        CALL psetc ('TEXT_COLOUR',                    'BLACK'      )
        CALL ptext
        CALL psetc ('MAP_COASTLINE_LAND_SHADE',       'OFF'        )
        CALL pcoast
        CALL pnew('PAGE')

        RETURN

      END SUBROUTINE draw



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
