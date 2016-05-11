
from Magics.macro import *
import datetime
import sys





def metgram(data):


    out = output(
        output_formats=["ps"],
        output_name_first_page_number='off',
        output_name="metgram" ,
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

    cc_data =  mwrepjson(
                wrepjson_family =  "eps",
                wrepjson_input_filename =  data["cloud_cover"],
                wrepjson_parameter =  "tcc",
                wrepjson_product_information= "Classic metgram" ,
                wrepjson_parameter_information= "Cloud Amount (%)",
                wrepjson_parameter_scaling_factor= 100.,
                )

    cc_graph = mmetgraph( metgram_plot_style = 'bar',
                    metgram_bar_colour = 'green',
                    metgram_bar_keyword = "forecast")
    cc_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_lines =  ["<font size='0.5'>Classic Metgram </font>",
                    "<font size='0.5'> <json_info key='station_name'/></font> <font size='0.5'> <json_info key='location'/></font>",
                    "<font size='0.5'> <json_info key='date'/></font>",
                    "<font size='0.5'> . </font>",
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
    rh_data =mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = data["humidity"],
                        wrepjson_parameter=  "r",
                        wrepjson_parameter_information = "850 hPa Relative Humidity  (%)",

                    )
    rh_graph = mmetgraph( metgram_curve_keyword = "forecast")
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
    precip_data =    mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = data["precipitation"],
                        wrepjson_parameter=  "cp",
                        wrepjson_parameter_information = "Precipitation",
                    )

    precip_graph = mmetgraph(
                          metgram_plot_style = 'bar',
                          metgram_bar_keyword = "forecast"
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
    msl_data =    mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = data["pressure"],
                        wrepjson_parameter=  "msl",
                        wrepjson_parameter_information = "MSL Pressure (hPa)",
                        wrepjson_parameter_scaling_factor = 0.01,
                    )
    msl_graph = mmetgraph(
                    metgram_curve_keyword = "forecast"
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
    wind_data =   mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = data["wind"],
                        wrepjson_parameter=  "10uv",
                        wrepjson_parameter_information = "10m Wind (m/s)",
                    )

    wind_graph = mmetgraph(
                          metgram_plot_style = 'flags',
                          metgram_flag_component1 = "10u",
                          metgram_flag_component2 = "10v",
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

    t850_data =    mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = data["temperature"],
                        wrepjson_parameter=  "t",
                        wrepjson_parameter_information = "Temperature (C)",
                        wrepjson_parameter_offset_factor =  -273.15,
                    )
    min_graph = mmetgraph( metgram_curve_colour = "blue",
                            metgram_curve_keyword = "forecast"
                        )
    t2m_data =    mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = data["2m_temperature"],
                        wrepjson_parameter=  "2t",
                        wrepjson_parameter_offset_factor =  -273.15,
                    )
    max_graph = mmetgraph( metgram_curve_colour = "red",
                        metgram_curve_keyword = "forecast"
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
       #Relative humidity
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

        # wind Flags
        frame5,
        wind_projection,
        horizontal,
        wind_vertical,
        wind_data, wind_graph, wind_text,

        #Temperatures
        frame6,
        projection,
        horizontal,
        vertical,
        t850_data, min_graph, t2m_data, max_graph, tempe_text,
        )





def main():
    lat = "37.00"
    lon = "35.32"
    data = {
        "cloud_cover" : "input/%s_%s_tcc.json" % (lat, lon),
        "precipitation" : "input/%s_%s_cp.json" % (lat, lon),
        "pressure" : "input/%s_%s_msl.json" % (lat, lon),
        "wind" : "input/%s_%s_10uv.json" % (lat, lon),
        "temperature" : "input/%s_%s_t.json" % (lat, lon),
        "2m_temperature" : "input/%s_%s_2t.json" % (lat, lon),
        "humidity" : "input/%s_%s_r.json" % (lat, lon),
        }
    metgram(data)


if __name__ == "__main__":
    sys.exit(main())
