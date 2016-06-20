# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


from Magics.macro import *
import datetime
import sys





def bufrgram(data):


    out = output(
        output_formats="ps",
        output_name_first_page_number='off',
        output_name="bufrgram" ,
        super_page_y_length=29.7,
        super_page_x_length=21.,
        )

    y = 4.5
    last = 22.5
    suby = 3.
    # define the cartesian projection
    frame1 = page(
        layout='positional',
        page_x_length= 21.,
        page_y_length= y+3.,
        page_id_line='off',
        page_x_position=0.,
        page_y_position= last
        )
    last = last - y;
    projection = mmap(
        subpage_map_projection='cartesian',
        subpage_x_axis_type='date',
        subpage_x_automatic='on',
        subpage_y_axis_type='regular',
        subpage_y_automatic='on',
        subpage_y_length = suby,
        subpage_y_position = 0.5
        )
    font_size = 0.35
    # define horizontal axis

    horizontal = maxis(
        axis_orientation = "horizontal",
        axis_date_type = "days",
        axis_days_label = "both",
        axis_days_label_colour=  "navy",
        axis_days_label_height = font_size,
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


    # define vertical axis

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
        axis_tick_label_height =  font_size
        )

    cc_data =    mmetbufr(
                        epsbufr_parameter_title = "Cloud Amount (%)",
                        epsbufr_parameter_descriptor =  20010,
                        epsbufr_information =  "on",
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                        epsbufr_station_name =  data["epsbufr_station_name"],


                    )
    cc_graph = mmetgraph( eps_box_border_thickness = 2,
                          eps_box_width = 1.5,
                          metgram_plot_style = 'bar')
    cc_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.5,
                text_justification =  "left",
                text_lines =  ["<json_info key='station_info'/>",
                    "<font size='0.5'> <json_info key='date'/></font>",
                    "<font size='0.5' colour='white'> . </font>",
                    "<font size='0.4'> <json_info key='parameter_info'/></font> "]
            )

    frame2 = page(
            layout='positional',
            page_x_length=21.,
            page_y_length=y,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=last
            )

    last  = last -y;
    rh_data =    mmetbufr(
                        epsbufr_parameter_title = "850 hPa Relative Humidity  (%)",
                        epsbufr_parameter_descriptor =  13003,
                        epsbufr_information =  "on",
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                    )
    rh_graph = mmetgraph( )
    rh_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_lines =  [
                    "<font size='0.4'> <json_info key='parameter_info'/></font> "]
            )

    frame3 = page(
            layout='positional',
            page_x_length=21.,
            page_y_length=y,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=last
            )

    last  = last -y;
    precip_data =    mmetbufr(

                        epsbufr_accumulated_parameter = "on",
                        epsbufr_parameter_title = "Precipitation",
                        epsbufr_parameter_descriptor =  13011,
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                    )
    precip_graph = mmetgraph(
                          metgram_plot_style = 'bar'
                        )
    precip_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_lines =  [
                    "<font size='0.4'> <json_info key='parameter_info'/></font> "]
            )
    frame4 = page(
            layout='positional',
            page_x_length=21.,
            page_y_length=y,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=last
            )

    last  = last - (y/2) -0.5;
    msl_data =    mmetbufr(
                        epsbufr_parameter_scaling_factor = 0.01,
                        epsbufr_parameter_title = "MSL Pressure (hPa)",
                        epsbufr_parameter_descriptor =  10051,
                        epsbufr_information =  "on",
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                    )
    msl_graph = mmetgraph(
                        )
    msl_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_lines =  [
                    "<font size='0.4'> <json_info key='parameter_info'/></font> "]
            )


    frame5 = page(
            layout='positional',
            page_x_length=21.,
            page_y_length= y/2,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=last
            )
    wind_projection = mmap(
        subpage_map_projection='cartesian',
        subpage_x_axis_type='date',
        subpage_x_automatic='on',
        subpage_y_axis_type='regular',
        subpage_y_min = -1.,
        subpage_y_max = 1.,
        subpage_y_length = suby/2,
        subpage_y_position = 0.5
        )
    wind_vertical = maxis(
        axis_orientation = "vertical",
        axis_grid = "off",
        axis_tick = "off",
        axis_tick_label = "off",
        )


    last  = last -y;
    wind_data =    mmetbufr(
                        epsbufr_parameter_title = "10m Wind (m/s)",
                        epsbufr_parameter_descriptor = 11003,
                        epsbufr_parameter_2_descriptor =  11004,
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                    )
    wind_graph = mmetgraph(
                          metgram_plot_style = 'flags'
                        )
    wind_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_lines =  [
                    "<font size='0.4'> <json_info key='parameter_info'/></font> "]
            )
    frame6 = page(
            layout='positional',
            page_x_length=21.,
            page_y_length=y,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=last
            )

    last  = last - y;

    min_data =    mmetbufr(
                        epsbufr_parameter_title = "Temperature (C)",
                        epsbufr_parameter_descriptor =  12001,
                        epsbufr_information =  "on",
                        epsbufr_parameter_offset_factor =  -273.15,
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                    )
    min_graph = mmetgraph( metgram_curve_colour = "blue"
                        )
    max_data =    mmetbufr(
                        epsbufr_parameter_title = "",
                        epsbufr_parameter_descriptor =  12004,
                        epsbufr_information =  "on",
                        epsbufr_parameter_offset_factor =  -273.15,
                        epsbufr_input_filename = data["epsbufr_input_filename"],
                        epsbufr_station_latitude = data["epsbufr_station_latitude"],
                        epsbufr_station_longitude = data["epsbufr_station_longitude"],
                    )
    max_graph = mmetgraph( metgram_curve_colour = "red"
                        )
    tempe_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_lines =  [
                    "<font size='0.4'> <json_info key='parameter_info'/> </font> "]
            )

    plot(
        out,
        frame1,
        projection,
        horizontal,
        vertical,
        cc_data, cc_graph, cc_text,
        frame2,
        projection,
        horizontal,
        vertical,
        rh_data, rh_graph, rh_text,
        frame3,
        projection,
        horizontal,
        vertical,
        precip_data, precip_graph, precip_text,
        frame4,
        projection,
        horizontal,
        vertical,
        msl_data, msl_graph, msl_text,
        frame5,
        wind_projection,
        horizontal,
        wind_vertical,
        wind_data, wind_graph, wind_text,
        frame6,
        projection,
        horizontal,
        vertical,
        min_data, min_graph, max_data, max_graph, tempe_text,
        )



def main():
    data = {
        "epsbufr_input_filename" : "GRA01130000012300001",
        "epsbufr_station_latitude" : 38.72,
        "epsbufr_station_longitude" :  30.51,
        "epsbufr_station_name" :  "My Station",
        }
    bufrgram(data)


if __name__ == "__main__":
    sys.exit(main())
