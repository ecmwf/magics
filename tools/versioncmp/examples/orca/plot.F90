      PROGRAM DATA_NETCDF

      IMPLICIT NONE
      
      CHARACTER(LEN=60) :: CFILEIN, CVAR, CFOUT, CFRM, CNAME
      REAL :: RMIN, RMAX, RINCR

      CALL GETARG(1, CFILEIN)
      CALL GETARG(2, CVAR)
      CALL GETARG(3, CFOUT)

      READ(16,*) RMIN, RMAX, RINCR

!     CALL SETOUT( TRIM(CFOUT), CFRM, CNAME)

!     Open MAGICS and set the output device

      CALL POPEN
      CALL PSET1C ('OUTPUT_FORMATS',(/'PS'/),1)
      CALL PSETC ('OUTPUT_NAME', TRIM(CFOUT))
      CALL PSETC ('PAGE_ID_LINE','OFF')

!      CALL PSETC ('SUBPAGE_MAP_PROJECTION','POLAR_STEREOGRAPHIC')


!     Set up the geographical area
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -75.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -180.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    75.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  180.0)

!     Plot all the NetCDF variables we are interested in, starting
!     with Temperature

      CALL PSETC ('NETCDF_FILENAME', TRIM(CFILEIN) )
      CALL PSETC ('NETCDF_FIELD_VARIABLE_NAME',TRIM(CVAR))
      CALL PSETC ('NETCDF_TYPE',                    'COMPLEX_MATRIX') 
      CALL PSETC ('netcdf_missing_attribute',       '_FillValue') 
      CALL PSETC ('NETCDF_LATITUDE_VARIABLE_NAME',  'nav_lat')
      CALL PSETC ('NETCDF_LONGITUDE_VARIABLE_NAME', 'nav_lon')
      CALL PSETR ('NETCDF_FIELD_SCALING_FACTOR',     1.0)
      CALL PSETR ('NETCDF_FIELD_ADD_OFFSET',        0.0)
      CALL PNETCDF

!     Set up and plot the title text and everything else

      CALL PSETC ('CONTOUR', 'OFF')
      CALL PSETC ('LEGEND', 'OFF')
      CALL PSETC ('CONTOUR_LABEL', 'OFF')
      CALL PSETC ('CONTOUR_HIGHLIGHT', 'OFF')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY', 1)
      CALL PSETC ('CONTOUR_SHADE', 'ON')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR ('CONTOUR_MIN_LEVEL', RMIN)
      CALL PSETR ('CONTOUR_MAX_LEVEL', RMAX)
      CALL PSETR ('CONTOUR_INTERVAL', RINCR)
      CALL PSETC ('CONTOUR_SHADE_METHOD', 'AREA_FILL')
      CALL PSETC ('CONTOUR_HILO', 'OFF')
      CALL PSETC ('CONTOUR_LABEL', 'OFF')
      call psetc('contour_shade_min_level_colour','blue')
      call psetc('contour_shade_max_level_colour','red')
      call psetc('contour_shade_colour_direction','clockwise')
!      CALL PCONT

      CALL PSETC ('MAP_COASTLINE_LAND_SHADE', 'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'RGB(0.95,0.95,0.95)')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_LABEL', 'ON')
      CALL PSETC ('MAP_LABEL_QUALITY', 'HIGH')
      CALL PSETR ('MAP_LABEL_HEIGHT', 0.4)
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY', 2)
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY', 2)
      CALL PSETC ('MAP_GRID', 'OFF')
      CALL PSETC ('MAP_GRID_LINE_STYLE',  'DASHED')
      CALL PSETR ('MAP_GRID_LATITUDE_REFERENCE', REAL(0))
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT', REAL(15))
      CALL PSETR ('MAP_GRID_LONGITUDE_REFERENCE', REAL(0))
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT', REAL(20))
      CALL PSETC ('MAP_GRID_COLOUR', 'RGB(0.85,0.85,0.85)')
      CALL PSETC ('MAP_GRID_COLOUR', 'BLACK')

      call psetc('legend', 'on')
      call psetc('legend_display_type', "continuous")
      call psetc('symbol_type', 'marker')
      call psetc('symbol_table_mode', 'advanced')
     ! CALL psymb
      CALL pcont
      CALL pcoast

      CALL PCLOSE

CONTAINS 

SUBROUTINE SETOUT( CIN, CF, CN)
IMPLICIT NONE
CHARACTER(LEN=*) :: CIN
CHARACTER(LEN=60) :: CF, CN
INTEGER :: IP, I, NP
CF=' '
CN=' '
NP = LEN_TRIM(CIN)
IF( NP .GT. 0 ) THEN
  IP=0
  DO I=1,NP
    IF( CIN(I:I) .EQ. '.' ) IP=I
  ENDDO 
  IF( IP .GT. 0 ) THEN
      CF(1:NP-IP) = CIN(IP+1:NP)
      CALL TOLOWER( CF )
      IF( CF(1:3).NE.'png' .AND. CF(1:3).NE.'ps ' .AND. CF(1:3).NE.'eps' .AND. &
          CF(1:3).NE.'pdf' .AND. CF(1:3).NE.'svg') CF(1:3) = 'ps'
      CN(1:IP-1) = CIN(1:IP-1)
   ELSE
      CN(1:NP) = CIN(1:NP)  ;   CF(1:3) = 'ps'
   ENDIF
ELSE
CF='output'
CN='ps'
ENDIF
END SUBROUTINE SETOUT

SUBROUTINE TOLOWER( STR )
IMPLICIT NONE
CHARACTER(LEN=*) :: str
  INTEGER :: i,ic
  DO i=1,LEN_TRIM(str)
    ic = IACHAR(str(i:i))
    IF ( (ic >= 65).AND.(ic <= 90) )  str(i:i) = ACHAR(ic+32)
  ENDDO
END SUBROUTINE TOLOWER

END PROGRAM DATA_NETCDF
