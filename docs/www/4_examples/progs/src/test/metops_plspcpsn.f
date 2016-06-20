C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM METOPS_PLSPCPSN


      PARAMETER (NLEV=6)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /1., 5., 10., 20., 50., 100./


C     Note (15/02/2006) This example is still in progress.
C     We need to plot 4 of these onto a page!



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_plspcpsn')



C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  70.0)



C      Load and plot the Large-scale precipitation (LSP)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/plspcpsn_lspacc.grib')
      CALL PGRIB


C     Define the contour for the LSP

#if defined (MAGPLUS)
      CALL PSETC ('grib_automatic_scaling', 'off')
#else
      CALL PSETC ('grib_scaling', 'off')
#endif
      CALL PSETC ('contour_line_style',                'solid')
      CALL PSETC ('contour_label',                     'on')
      CALL PSETI ('contour_label_frequency',           1)
      CALL PSETR ('contour_min_level',                 1.0)
      CALL PSETR ('contour_max_level',                 100.0)
      CALL PSETC ('contour_level_selection_type',      'level_list')
      CALL PSET1R ('contour_level_list', RLEV, NLEV)
      CALL PSETC ('contour_highlight',                 'on')
      CALL PSETR ('contour_hilo_min_value',            5.0)
      CALL PSETC ('contour_hilo_quality',              'high')
      CALL PSETR ('contour_hilo_suppress_radius',      50.0)
      CALL PSETR ('contour_hilo_height',               0.2)
      CALL PSETC ('contour_hilo',                      'hi')
      CALL PSETC ('contour_hilo_type',                 'number')
      CALL PSETC ('contour_hilo_format',               '(I4)')
      CALL PSETC ('contour_shade',                     'on')
      CALL PSETR ('contour_shade_min_level',           1.)
      CALL PSETR ('contour_shade_max_level',           100.)
      CALL PSETR ('contour_shade_min_level_density',   100.)
      CALL PSETR ('contour_shade_max_level_density',   400.)
      CALL PSETC ('contour_shade_min_level_colour',    'red')
      CALL PSETC ('contour_shade_max_level_colour',    'red')
      CALL PSETR ('contour_shade_dot_size',            0.03)
      CALL PSETC ('contour_line_colour',               'red')
      CALL PSETC ('contour_highlight_colour',          'red')
      CALL PSETC ('contour_label_colour',              'red')
      CALL PSETC ('contour_hi_colour',                 'red')
      CALL PCONT



C      Load and plot the convective precipitation (CP)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/plspcpsn_cpacc.grib')
      CALL PGRIB


C     Define the contour for the CP

#if defined (MAGPLUS)
      CALL PSETC ('grib_automatic_scaling', 'on')
#else
      CALL PSETC ('grib_scaling', 'on')
#endif
      CALL PSETC ('contour_line_style',                'solid')
      CALL PSETC ('contour_label',                     'on')
      CALL PSETI ('contour_label_frequency',           1)
      CALL PSETR ('contour_min_level',                 1.0)
      CALL PSETR ('contour_max_level',                 100.0)
      CALL PSETC ('contour_level_selection_type',      'level_list')
      CALL PSET1R ('contour_level_list', RLEV, NLEV)
      CALL PSETC ('contour_highlight',                 'on')
      CALL PSETR ('contour_hilo_min_value',            5.0)
      CALL PSETC ('contour_hilo_quality',              'high')
      CALL PSETR ('contour_hilo_suppress_radius',      50.0)
      CALL PSETR ('contour_hilo_height',               0.2)
      CALL PSETC ('contour_hilo',                      'hi')
      CALL PSETC ('contour_hilo_type',                 'number')
      CALL PSETC ('contour_hilo_format',               '(I4)')
      CALL PSETC ('contour_shade',                     'on')
      CALL PSETR ('contour_shade_min_level',           1.)
      CALL PSETR ('contour_shade_max_level',           100.)
      CALL PSETR ('contour_shade_min_level_density',   100.)
      CALL PSETR ('contour_shade_max_level_density',   400.)
      CALL PSETC ('contour_shade_min_level_colour',    'blue')
      CALL PSETC ('contour_shade_max_level_colour',    'blue')
      CALL PSETR ('contour_shade_dot_size',            0.03)
      CALL PSETC ('contour_line_colour',               'blue')
      CALL PSETC ('contour_highlight_colour',          'blue')
      CALL PSETC ('contour_label_colour',              'blue')
      CALL PSETC ('contour_hi_colour',                 'blue')
      CALL PCONT



C      Load and plot the snowfall precipitation (SFP)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/plspcpsn_sfacc.grib')
      CALL PGRIB


C     Define the contour for the SFP

#if defined (MAGPLUS)
      CALL PSETC ('grib_automatic_scaling', 'on')
#else
      CALL PSETC ('grib_scaling', 'on')
#endif
      CALL PSETC ('contour',                        'off')
      CALL PSETR ('contour_min_level',               1.)
      CALL PSETR ('contour_max_level',               100.)
      CALL PSETC ('contour_level_selection_type',   'interval')
      CALL PSETR ('contour_interval',                99.)
      CALL PSETR ('contour_reference_level',         1.)
      CALL PSETC ('contour_hilo',                   'off')
      CALL PSETC ('contour_shade',                  'on')
      CALL PSETR ('contour_shade_min_level',         1.)
      CALL PSETR ('contour_shade_max_level',         100.)
      CALL PSETR ('contour_shade_min_level_density', 150.)
      CALL PSETR ('contour_shade_max_level_density', 150.)
      CALL PSETC ('contour_shade_min_level_colour', 'green')
      CALL PSETC ('contour_shade_max_level_colour', 'green')
      CALL PSETR ('contour_shade_dot_size',          0.04)
      CALL PSETR ('contour_shade_angle',             60.)
      CALL PCONT


C     Plot the coast

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')
      CALL PCOAST


C     Plot the title

      CALL PSETC ('text_justification',     'left')
      CALL PSETC ('text_titles_table_use',  'off')
      CALL PSETI ('text_line_count',         3)
      CALL PSETI ('text_first_line',         2)
      CALL PSETC ('text_line_2',  '\\CLRED\\Large-Scale precipitation 
     x \\CLBLUE\\Convective precipitation 
     x \\CLBLACK\\and \\CLGREEN\\Snowfall')
      CALL PSETC ('text_line_3',  'Accumulated over last 12 hours')
      CALL PSETC ('text_quality',           'high')
      CALL PSETC ('text_colour',            'black')
      CALL PTEXT


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
