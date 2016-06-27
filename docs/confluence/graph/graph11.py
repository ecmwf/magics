# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.



from Magics.macro import *
ref = "graph11"
output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name=ref)



# Setting the cartesian view
projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_y_axis_type='regular',
    subpage_x_date_min='2012-07-23 18:00:00',
    subpage_x_date_max='2012-08-05 21:00:00',
    subpage_y_min=90.,
    subpage_y_max=150.,
    )

# Vertical axis
vertical = maxis(
    axis_orientation='vertical',
    axis_type='regular',
    axis_tick_label_height=0.40,
    axis_tick_label_colour='navy',
    axis_grid='on',
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    axis_title='on',
	axis_title_text='Discharge (m3/s)',
    axis_title_font_style='bold',
    )

# Horizontal axis
horizontal = maxis(
    axis_orientation='horizontal',
    axis_type='date',
    axis_grid='on',
    axis_days_label_height=0.40,
    axis_months_label_height=0.40,
    axis_years_label_height=0.50,
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    axis_title='on',
	axis_title_text='Date (days)',
    axis_title_font_style='bold',
    )

# eud red curve
eud_input = minput(input_x_type='date',
                   input_date_x_values=['2012-07-27 03:00:00', '2012-07-27 09:00:00', '2012-07-27 15:00:00', '2012-07-27 21:00:00', '2012-07-28 03:00:00', '2012-07-28 09:00:00', '2012-07-28 15:00:00', '2012-07-28 21:00:00', '2012-07-29 03:00:00', '2012-07-29 09:00:00', '2012-07-29 15:00:00', '2012-07-29 21:00:00', '2012-07-30 03:00:00', '2012-07-30 09:00:00', '2012-07-30 15:00:00', '2012-07-30 21:00:00', '2012-07-31 03:00:00', '2012-07-31 09:00:00', '2012-07-31 15:00:00', '2012-07-31 21:00:00', '2012-08-01 03:00:00', '2012-08-01 09:00:00', '2012-08-01 15:00:00', '2012-08-01 21:00:00', '2012-08-02 03:00:00', '2012-08-02 09:00:00', '2012-08-02 15:00:00', '2012-08-02 21:00:00', '2012-08-03 03:00:00', '2012-08-03 09:00:00', '2012-08-03 15:00:00', '2012-08-03 21:00:00', '2012-08-04 03:00:00', '2012-08-04 09:00:00', '2012-08-04 15:00:00', '2012-08-04 21:00:00', '2012-08-05 03:00:00', '2012-08-05 09:00:00', '2012-08-05 15:00:00', '2012-08-05 21:00:00'],
                   input_y_values=[113.466766357422, 111.895797729492, 110.403533935547, 109.061645507813, 107.865753173828, 106.79020690918, 105.812911987305, 104.920349121094, 104.107482910156, 103.749725341797, 106.829193115234, 113.006500244141, 116.862976074219, 118.159683227539, 117.838897705078, 116.482269287109, 114.627899169922, 112.763122558594, 111.083953857422, 109.466613769531, 107.888000488281, 106.374496459961, 104.959259033203, 103.663635253906, 102.487380981445, 101.425109863281, 100.484100341797, 99.7120361328125, 99.2962493896484, 99.5143585205078, 100.130401611328, 100.480865478516, 100.340515136719, 99.79931640625, 99.0550842285156, 98.3815612792969, 98.3650817871094, 100.041290283203, 102.635131835938, 103.883102416992]
                   )

eud_graph = mgraph(graph_line_colour='red',
                   graph_line_thickness=4, 
				   legend='on',
				   legend_user_text="ECMWF")




# box plot
boxplot = mboxplot(boxplot_date_positions=['2012-07-27 12:00:00', '2012-07-28 12:00:00', '2012-07-29 12:00:00', '2012-07-30 12:00:00', '2012-07-31 12:00:00', '2012-08-01 12:00:00', '2012-08-02 12:00:00', '2012-08-03 12:00:00', '2012-08-04 12:00:00', '2012-08-05 12:00:00'],
        boxplot_minimum_values = [110.876510620117, 105.943267822266, 102.94677734375, 101.112152099609, 99.1953125, 97.2408752441406, 95.3248138427734, 93.7392883300781, 93.7828674316406, 91.7444458007813],
        boxplot_maximum_values = [110.898223876953, 106.121658325195, 128.880126953125, 141.421569824219, 123.516998291016, 125.518188476563, 134.954742431641, 146.920013427734, 129.118087768555, 135.248733520508],
        boxplot_box_upper_values = [110.890480041504, 106.034225463867, 110.814819335938, 114.95743560791, 112.672882080078, 109.317665100098, 108.987083435059, 106.87133026123, 107.71662902832, 111.642501831055],
        boxplot_box_lower_values =[110.881286621094, 105.976150512695, 104.097366333008, 103.589248657227, 103.79369354248, 103.230926513672, 101.188011169434, 99.6595077514648, 97.6234970092773, 95.7449645996094],
        boxplot_median_values =[110.885986328125, 106.006408691406, 105.950744628906, 107.602294921875, 106.956008911133, 106.728393554688, 104.66259765625, 103.394897460938, 101.11589050293, 99.4046325683594],
		boxplot_box_colour = "rgb(0.65,0.58,0.92)")
	
 
plot(output, projection, vertical, horizontal, 
	 	eud_input, eud_graph,
        boxplot,
	)	
    
#For documentation only
tofortran(ref,output, projection, vertical, horizontal, 
	 	eud_input, eud_graph,
        boxplot, )
tomv4(ref, eud_input, eud_graph,)
tohtml(ref, boxplot, )


