C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C


      program eps
      	DIMENSION X1(9),X2(9),Y1(9),Y2(9)
	   DATA X1/1.,2.,3.,4.,5.,6.,7.,8.,9./
	   DATA Y1/4.5,5.,6.,7.,9.,9.5,9.8,8.,7./
	   DATA X2/1.,2.,3.,4.,5.,6.,7.,8.,9./
	   DATA Y2/2.5,2.,3.,2.,1.,2.5,3.8,6.,2./
      CHARACTER label*8
      DIMENSION   label(6)
      DATA       label /'', 'march', 'avril', 'mai', 'june', ''/
      call popen()
       call psetc ('ps_device', 'ps_a4')
      call psetc ('ps_file_name', 'boxplot.ps')
      
      call psetc ('subpage_map_projection', 'cartesian')
      
      
      call pset1r('boxplot_positions', (/2., 4., 6., 8. /), 4 )
      call pset1r('boxplot_minimum_values', 
     +                 (/1., 3., 5., 4./), 4)
      call pset1r('boxplot_maximum_values', 
     +                 (/5., 7., 9., 8./), 4)
       call pset1r('boxplot_median_values', 
     +                 (/3., 5., 7., 6./), 4)
      call pset1r('boxplot_box_upper_values', 
     +                 (/4., 6., 8., 7./), 4)
      call pset1r('boxplot_box_lower_values', 
     +                 (/2., 4., 6., 5./), 4)
     
      call psetr ('boxplot_box_width', 1.)
      call psetc ('boxplot_box_colour', "grey")
      call pseti ('boxplot_box_border_thickness', 3)
      call psetc ('boxplot_box_border_line_style', "dash")
      call pseti ('boxplot_median_thickness', 15)
      call psetc ('axis_line_colour', 'blue')
      call psetc ('axis_grid', 'on')
      call psetc ('axis_grid', 'on')
      call psetc ('axis_grid_colour', 'grey')
      call psetc ('axis_grid_line_style', 'dash')
      
        
      call psetc ('axis_orientation', 'horizontal')
      call psetr ('axis_min_value', 0.)
      call psetr ('axis_max_value', 10.)
      call psetc ('AXIS_TICK_LABEL_type', 'label_list')
      call pset1c ('AXIS_TICK_LABEL_LIST', label, 6)
     
      call paxis
      
      call psetc ('axis_orientation', 'vertical')
      call psetc ('AXIS_TICK_LABEL_type', 'number')
      call psetr ('axis_min_value', 0.)
      call psetr ('axis_max_value', 10.)
      
      call psetc ('AXIS_TICK_LABEL_format', "(f8.3)")
      call paxis
  
     
          

      
      CALL PSETC ('GRAPH_TYPE','AREA')
	  CALL PSETC ('GRAPH_SHADE','ON')
	   
	  CALL PSETC ('GRAPH_SHADE_COLOUR','yellow')
	  CALL PSET1R ('GRAPH_CURVE_X_VALUES',X1,9)
	  CALL PSET1R ('GRAPH_CURVE_Y_VALUES',Y1,9)
	  CALL PSET1R ('GRAPH_CURVE2_X_VALUES',X2,9)
	  CALL PSET1R ('GRAPH_CURVE2_Y_VALUES',Y2,9)
	  
      CALL PGRAPH
      
      call pboxplot
      
      
      
      
    
      call pclose()
    
      end
