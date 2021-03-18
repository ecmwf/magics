
from Magics.macro import *
import datetime
import sys

def bar(param, legend, colour, first="off", info="none") :


    data =  mwrepjson(
                wrepjson_family =  "eps",
                wrepjson_parameter=  "260015",
                wrepjson_key=  param,
                wrepjson_input_filename =  "meteogram-ptype.json",
                wrepjson_position_information =  first,
                wrepjson_parameter_information =  info,
                )

    graph =  mgraph (
                        legend             = "on",
                        legend_user_text   = legend,
                        graph_type         = "bar",
                        graph_bar_line_colour   = colour,
                        graph_shade_colour = colour,
                        graph_bar_line_thickness= 1,
                        graph_bar_justification = "centre",
                        graph_bar_width    = 3000.
                    )
    return data, graph


input = ""
out = output(
        output_formats=["png"],
        output_name_first_page_number='off',
        output_name="ptypegram" ,
        )

projection = mmap(
        subpage_map_projection='cartesian',
        subpage_x_axis_type='date',
        subpage_x_automatic='on',
        subpage_y_axis_type='regular',
        subpage_y_min = 0.,
        subpage_y_max = 100.,
    )


horizontal = maxis(
        axis_orientation = "horizontal",
        axis_date_type = "days",
        axis_days_label = "both",
        axis_days_label_colour=  "navy",
        axis_days_label_height = 0.35,
        axis_grid =  "on",
        axis_grid_colour = "navy",
        axis_grid_line_style= "dash",
        axis_line_colour= "navy",
        axis_minor_tick= "on",
        axis_minor_tick_colour= "navy",
        axis_months_label= "off",
        axis_tick_colour= "navy",
        axis_type =  "date",
        axis_years_label = "off"
        )

vertical = maxis(
        axis_orientation = "vertical",
        axis_grid = "on",
        axis_grid_colour ="navy",
        axis_grid_line_style = "dash",
        axis_grid_reference_level =  0.,
        axis_grid_reference_thickness = 1,
        axis_line =  "on",
        axis_line_colour = "navy",
        axis_tick_colour = "navy",
        axis_tick_label_colour = "navy",
        axis_tick_label_type   = "label_list",
        axis_tick_label_height = 0.4,
        axis_tick_label_list   = ["0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"]
        )



legend = mlegend(
   legend_column_count           = 6,
   legend_entry_plot_direction   = "column",
   legend_entry_plot_orientation = "bottom_top",
   legend_text_font_size       = 0.4,
   legend_text_colour       = "black",
   legend_text_composition     = "user_text_only",
   legend_entry_border         = "off"
   )

text = []

text.append("Probability of precipitation type (%) depending on mean precipitation rates (mm/h)")
text.append("<json_info key='station_name'/><json_info key='location'/><json_info key='grid_point'/><json_info key='height'/>")
text.append("<json_info key='date'/>")
text.append("Rain (mm/h)------------------Sleet(mm/h)----------------Wetsnow (mm/h)--------------Snow (mm/h)-------------Icepellets (mm/h)-------------------Freezing")
                #"text_line_3": "<font size='0.6'><json_info key='product_info'/></font><font size='0.6'><json_info key='date'/></font>",
                #"text_line_4": "<font size='0.5' colour='white'>.</font>",
                #text_line_5": "<font size='0.5'><json_info key='parameter_info'/></font>",


title = mtext(
        text_lines         =  text,
        text_font_size      = 0.4,
        text_colour         = "black",
        text_mode = "positional",
        text_box_x_position = 4.50,
        text_box_y_position = 18.00,
        text_box_x_length = 20.00,
        text_box_y_length = 4.00
        )

plot(out, projection, horizontal, vertical,
    bar("rain_low", "0.1-0.2", "rgb(201,239,191)", "on", "on"),
    bar("rain_med", "0.2-1", "rgb(58,226,20)"),
    bar("rain_high", ">1", "rgb(25,163,7)"),
    bar("sleet_low", "0.1-0.2", "rgb(122,234,221)"),
    bar("sleet_med", "0.2-1", "rgb(28,183,165)"),
    bar("sleet_high", ">1", "rgb(10,112,101)"),
    bar("wetsnow_low", "0.1-0.2", "rgb(114,163,254)"),
    bar("wetsnow_med", "0.2-1", "rgb(68,68,249)"),
    bar("swetsnow_high", ">1", "rgb(0,0,102)"),
    bar("snow_low", "0.1-0.2", "rgb(211,193,242)"),
    bar("snow_med", "0.2-1", "rgb(158,104,239)"),
    bar("snow_high", ">1", "rgb(86,28,178)"),
    bar("icepellets_low", "0.1-0.2", "rgb(234,186,137)"),
    bar("icepellets_med", "0.2-1", "rgb(254,127,0)"),
    bar("icepellets_high", ">1", "rgb(188,99,5)"),
    bar("freezing_low", "0.1-0.2", "rgb(244,130,137)"),
    bar("frezing_med", "0.2-1", "rgb(254,2,73)"),
    bar("freezing_high", ">1", "rgb(191,5,76)"),
    legend, title
    )


