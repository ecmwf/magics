# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# importing Magics module

from Magics.macro import *

# Setting of the output file name

ref = 'plumes'
output = output(
    output_formats=['ps'],
    output_name_first_page_number='off',
    output_name=ref,
    super_page_y_length=29.7,
    super_page_x_length=21.,
    )

# define the cartesian projection
top = page(
        layout='positional', 
        page_x_length=21.,
        page_y_length=12.,
        page_id_line='off',
        page_x_position=0.,
        page_y_position=18.)
        
projection = mmap(
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_x_automatic='on',
    subpage_y_axis_type='regular',
    subpage_y_automatic='on',
    subpage_y_length = 6.5,
    subpage_y_position = 0.5
    )

# define horizontal axis

horizontal = maxis(
	axis_orientation = "horizontal",
    axis_date_type = "days",
    axis_days_label = "both",
    axis_days_label_colour=  "#0b265a",
    axis_days_label_height = 0.4,
    axis_grid =  "on",
    axis_grid_colour = "Rgb(0.5, 0.5, 0.5)",
    axis_grid_line_style= "dash",
    axis_line_colour= "#0b265a",
    axis_minor_tick= "on",
    axis_minor_tick_colour= "#0b265a",
    axis_months_label= "off",                 
    axis_tick_colour= "#0b265a",
    axis_type =  "date",                  
    axis_years_label = "off"
    )

# define vertical axis

vertical = maxis(
         axis_orientation = "vertical",
         axis_grid = "on",
         axis_grid_colour =" grey",
         axis_grid_line_style = "dash",
         axis_grid_reference_level =  0.,
         axis_grid_reference_thickness = 1,
         axis_line =  "on",
         axis_line_colour = "#0b265a",
         axis_tick_colour = "#0b265a",
         axis_tick_label_colour = "#0b265a",
         axis_tick_label_height =  0.4
    )

tempe = mwrepjson( wrepjson_family =  "eps",
                    wrepjson_input_filename =  "t850.json",
                    wrepjson_parameter =  "130",
                    wrepjson_parameter_offset_factor= -273.15,
                    wrepjson_temperature_correction= "on",
                    wrepjson_plumes_interval =  1.,
                    wrepjson_product_information= "ECMWF Ensemble Forecasts " ,
                    wrepjson_parameter_information= "Temperature 850hPa",
                    wrepjson_y_percentage = 15.)


tempe_contour = mcont( legend = "on",
                    contour_hilo =  "off",
                    contour_label =  "off", 
                    contour_highlight =  "off",
                    contour_level_list =  [0.5, 10., 30., 50., 100.],
                    contour_level_selection_type = "list",
                    contour_line_colour =  "grey",
                    contour_line_thickness =  2,
                    contour_method =  "linear",
                    contour_shade =  "on",
                    contour_shade_colour_list =  ["#d0fd74", "#9fee01", "#789837", "#4f7600"],
                    contour_shade_colour_method =  "list",
                    contour_shade_method = "area_fill")

tempe_plumes =    mepsplumes (
                        eps_plume_line_colour = "#fa73bf",
                        eps_plume_forecast_line_colour = "#cd0174",
                        eps_plume_forecast_line_thickness = 5,
                        eps_plume_control_line_thickness = 5,
                        eps_plume_control_line_colour = "#cd0174",
                        eps_plume_line_style = "dot",
                        eps_plume_line_thickness = 2
                    )
 # definition of the title

lines = [ "ECMWF Ensemble forecasts <json_info key='station_name'/>", 
          "Location: <json_info key='location'/>", 
          "Base Time: <json_info key='date'/>", 
          "<font size='0.1'> .  </font>", 
          "<font size='0.5'><json_info key='parameter_info'/></font><font size='0.5'> - Probability for </font><font size='0.5'><json_info key='plumes_interval'/></font><font size='0.5'>&deg;</font><font size='0.5'>C intervals</font>",       
         ]

title = mtext(
    text_lines=lines,
    text_html='true',
    text_colour='#0b265a',
    text_font_size=0.6,
    text_mode='positional',
    text_box_x_position=1.,
    text_box_y_position=7.,
    text_box_x_length=10.,
    text_box_y_length=2.5,
    text_border='off',
    text_justification='left',
    )
legend = mlegend(
                   legend_box_mode='positional',
    legend_box_x_position=12.,
    legend_box_y_position=8.5,
    legend_box_x_length=11.,
    legend_box_y_length=2.5,
    legend_border='off',
                    legend_column_count =  5, 
                    legend_entry_text_width =  75., 
                    legend_text_colour =  "#0b265a", 
                    legend_text_composition =  "user_text_only", 
                    legend_text_font_size =  0.35,
                    legend_user_lines =  ["0.5-10%" , "1-30%", "30-50%", "50-100%",  "Oper", "Ctr", "EMem"   ]                 
                ) 
# To the plot
middle = page(
        layout='positional', 
        page_x_length=21.,
        page_y_length=9.,
        page_id_line='off',
        page_x_position=0.,
        page_y_position=9.5)
        

precip = mwrepjson( wrepjson_family =  "eps",
                    wrepjson_input_filename =  "precip.json",
                    wrepjson_parameter =  "143",
                    wrepjson_parameter_scaling_factor= 1000.,                  
                    wrepjson_parameter_information= "Ensemble members of Total Precipitation",
                    wrepjson_y_percentage = 15.)



precip_plumes =    mepsplumes (
                        eps_plume_line_colour = "#4a80e8",
                        eps_plume_forecast_line_colour = "#cd0174",
                        eps_plume_forecast_line_thickness = 5,
                        eps_plume_control_line_thickness = 5,
                        eps_plume_control_line_colour = "#cd0174",
                        eps_plume_line_style = "dot",
                        eps_plume_line_thickness = 2
                    )
short_title = mtext(
    text_lines=["<json_info key='parameter_info'/>"],
    text_html='true',
    text_colour='#0b265a',
    text_font_size=0.5,
    text_mode='positional',
    text_box_x_position=1.,
    text_box_y_position=7.,
    text_box_x_length=10.,
    text_box_y_length=2.5,
    text_border='off',
    text_justification='left',
    )


# To the plot
bottom = page(
        layout='positional', 
        page_x_length=21.,
        page_y_length=9.,
        page_id_line='off',
        page_x_position=0.,
        page_y_position=1.)
        

geop = mwrepjson( wrepjson_family =  "eps",
                    wrepjson_input_filename =  "z500.json",
                    wrepjson_parameter =  "129", 
                    wrepjson_parameter_information =  "Geopotential at 500 hPa", 
                    wrepjson_parameter_scaling_factor = 0.01019, 
                    wrepjson_plumes_interval = 2.5,               
                    wrepjson_y_percentage = 15.)


geop_contour = mcont(   legend="off", 
                        contour_highlight= "off", 
                        contour_hilo= "off", 
                        contour_line_colour = "grey",
                        contour_label = "off",
                        contour_line_thickness =  2,
                        contour_level_list =  [0.5, 10, 30, 50, 120],
                        contour_level_selection_type =  "list",
                        contour_shade =  "on",
                        contour_shade_colour_list =  ["#a4bff3", "#4a80e8", "#174cb4", "#0b265a" ],
                        contour_shade_colour_method =  "list",
                        contour_shade_method =  "area_fill"
                    )

geop_plumes =    mepsplumes (
                        eps_plume_line_colour = "#e86349",
                        eps_plume_forecast_line_colour = "#cee849",
                        eps_plume_forecast_line_thickness = 5,
                        eps_plume_control_line_thickness = 5,
                        eps_plume_control_line_colour = "#cee849",
                        eps_plume_line_style = "dot",
                        eps_plume_line_thickness = 2
                    )
                    

    
plot(
    output,
    top,
    projection,
    horizontal,
    vertical,
    tempe, tempe_contour, tempe_plumes,
    title,
    legend,
    middle, 
    projection, horizontal, vertical,
    precip, precip_plumes, short_title, 
    bottom, 
    projection, horizontal, vertical,
    geop, geop_contour, geop_plumes,
    short_title
    )

