import traceback
import sys

from gribapi import *
import json
from Magics.macro import *
import numpy
import datetime

configuration = {
    "z500" : {
        "ensemble" : "ensemble_z500.grib",
        "control" : "control_z500.grib",
        "deterministic" : "deterministic_z500.grib",
        "param" : "z500",
        "factor":"0.01019",
        "offset" : "0",
        "d-1" : "deterministic_d-1_z500.grib",
        "d-2" : "deterministic_d-2_z500.grib"

    },
    "t850" : {
        "ensemble" : "ensemble_t850.grib",
        "control" : "control_t850.grib",
        "deterministic" : "deterministic_t850.grib",
        "param" : "t850",
        "factor": "1",
        "offset" : "-273.15",
        "d-1" : "deterministic_d-1_t850.grib",
        "d-2" : "deterministic_d-2_t850.grib"
        },
    "precip" : {
        "ensemble" : "ensemble_pr.grib",
        "control" : "control_pr.grib",
        "deterministic" : "deterministic_pr.grib",
        "param" : "precip",
        "factor": "1000.",
        "offset" : "0",
        "d-1" : "deterministic_d-1_pr.grib",
        "d-2" : "deterministic_d-2_pr.grib"
        }
}

reference = datetime.datetime.now()
def ensemble(files, index, out):
    f = open(files["ensemble"])

    factor = float(files["factor"])
    offset = float(files["offset"])

    mcount = grib_count_in_file(f)


    for i in range(mcount):
        gid = grib_new_from_file(f)
        step = str(grib_get(gid, "step"))

        number = str(grib_get(gid, "number"))
        param =  str(grib_get(gid, "parameter"))
        date =  str(grib_get(gid, "date"))
        time =   "%02d" % grib_get(gid, "time")

        values = grib_get_elements(gid,"values",index)
        mean = []

        for l in range(len(index)):
            if not (param in out[l]) :
                out[l][param] = {}
                out[l][param]["steps"] = [-1]
                out[l]["%s_mean" % param] = {}


            if not (number in  out[l][param] ) :
                out[l][param][number]=[]
            if not (step in  out[l]["%s_mean" % param] ) :
                out[l]["%s_mean" % param][step]=[]

            if out[l][param]["steps"][-1] != step :
                if out[l][param]["steps"][-1] == -1:
                    out[l][param]["steps"][-1] = step
                else :
                    out[l][param]["steps"].append(step)




            out[l][param][number].append(values[l]*factor + offset)
            out[l]["%s_mean" % param][step].append(values[l]*factor + offset)
        grib_release(gid)
    f.close()

    base = datetime.datetime.strptime(date+time, '%Y%m%d%H%M')
    print base

    for l in range(len(index)):
        mean = {}


        mean["y_values"] = []
        mean["x_date_values"] = []
        for s in out[l][param]["steps"]:
            mean["y_values"].append(numpy.mean(out[l]["%s_mean" % param][s]))
            valid = base +  datetime.timedelta(hours=int(s))
            mean["x_date_values"].append("%s" % valid)

        out[l]["%s_mean" % param] = mean


    return

def deterministic(files, index, out):

    f = open(files["deterministic"])
    factor = float(files["factor"])
    offset = float(files["offset"])

    mcount = grib_count_in_file(f)


    for i in range(mcount):
        gid = grib_new_from_file(f)
        if i == 0 :
            param =  str(grib_get(gid, "parameter"))

            for l in range(len(index)):
                out[l][param]["forecast"] = []

        values = grib_get_elements(gid,"values",index)
        for l in range(len(index)):
            out[l][param]["forecast"].append(values[l]*factor+offset)

        grib_release(gid)
    f.close()
    return

def previous(files, key, index, out):

    f = open(files[key])
    factor = float(files["factor"])
    offset = float(files["offset"])

    mcount = grib_count_in_file(f)


    for i in range(mcount):
        gid = grib_new_from_file(f)
        if i == 0 :
            param =  str(grib_get(gid, "parameter"))
            step = str(grib_get(gid, "step"))
            date =  str(grib_get(gid, "date"))
            time =   "%02d" % grib_get(gid, "time")
            for l in range(len(index)):
                out[l][key] = {"y_values" : [], "x_date_values" : [] }
            base = datetime.datetime.strptime(date+time, '%Y%m%d%H%M')
        step = grib_get(gid, "step")
        valid = base +  datetime.timedelta(hours=step)


        values = grib_get_elements(gid,"values",index)
        for l in range(len(index)):

            if ( valid > reference ) :
                out[l][key]["y_values"].append(values[l]*factor+offset)
                out[l][key]["x_date_values"].append("%s" % valid)


        grib_release(gid)
    f.close()
    return

def control(files, index, out):
    global reference
    f = open(files["control"])

    factor = float(files["factor"])
    offset = float(files["offset"])

    mcount = grib_count_in_file(f)


    for i in range(mcount):

        gid = grib_new_from_file(f)
        if i == 0 :

            param =  str(grib_get(gid, "parameter"))
            date =  str(grib_get(gid, "date"))
            time =  "%02d" % grib_get(gid, "time")

            for l in range(len(index)):
                out[l]["date"]  = date
                out[l]["time"]  = time
                out[l][param]["control"] = []
        reference = datetime.datetime.strptime(date+time, '%Y%m%d%H%M')
        values = grib_get_elements(gid,"values",index)
        for l in range(len(index)):
            out[l][param]["control"].append(values[l]*factor+offset)

        grib_release(gid)
    f.close()
    return



def data(stations, files):
    out = []
    index = []
    lsm = open("lsm.cf")
    gid = grib_new_from_file(lsm)

    for station in stations:
        nearest = grib_find_nearest(gid,station["lat"],station["lon"],1)
        name =  "%.2f_%.2f_%s.json" % (station["lat"], station["lon"], files["param"])
        out.append({
            "station_name" : station["name"],
            "output" : name,
            "location" : {  "lat" : station["lat"], "lon" : station["lon"] },
            "EPS_location" : {  "distance": nearest[0].distance, "latitude" : nearest[0].lat, "longitude" : nearest[0].lon },
            "forecast_location" : {  "distance": nearest[0].distance, "latitude" : nearest[0].lat, "longitude" : nearest[0].lon }
        })
        index.append(nearest[0].index)
        station[files["param"]] = name

    grib_release(gid)
    lsm.close()

    ensemble(files, index, out)
    control(files, index, out)
    deterministic(files, index, out)
    previous(files, "d-1", index, out)
    previous(files, "d-2", index, out)
    for i in out:
        f = open(i["output"], 'w')
        print >> f, json.dumps(i, indent=4, separators=(',', ': ') )



def plumes(location):

    z500 = "%.2f_%.2f_%s.json" % (location["lat"], location["lon"], "z500")
    precip = "%.2f_%.2f_%s.json" % (location["lat"], location["lon"], "precip")
    t850 = "%.2f_%.2f_%s.json" % (location["lat"], location["lon"], "t850")

    out = output(
        output_formats=location["output_formats"],
        output_name_first_page_number='off',
        output_name=str(location["output_name"]) ,
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
        page_y_position=18.
        )

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

    # horizontal axis for bottom plot (with month and year)

    horizontal_bot = maxis(
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
        axis_months_label= "on",
        axis_months_label_colour=  "#0b265a",
        axis_months_label_height = 0.4,
        axis_tick_colour= "#0b265a",
        axis_type =  "date",
        axis_years_label = "on",
        axis_years_label_colour=  "#0b265a",
        axis_years_label_height = 0.4
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

    tempe = mwrepjson(
                wrepjson_family =  "eps",
                wrepjson_input_filename =  t850,
                wrepjson_parameter =  "130",
                wrepjson_temperature_correction= "on",
                wrepjson_plumes_interval =  1.,
                wrepjson_product_information= "ECMWF Ensemble Forecasts " ,
                wrepjson_parameter_information= "Temperature 850hPa",
                wrepjson_y_percentage = 15.
                )

    tempe_contour = mcont(
                legend = "on",
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
                contour_shade_method = "area_fill"
                )

    tempe_plumes = mepsplumes (
                eps_plume_line_colour = "#fa73bf",
                eps_plume_forecast_line_colour = "#cd0174",
                eps_plume_forecast_line_thickness = 5,
                eps_plume_control_line_thickness = 5,
                eps_plume_control_line_colour = "#cd0174",
                eps_plume_line_style = "dot",
                eps_plume_line_thickness = 2
                )
    t_1 = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  t850,
                wrepjson_keyword =  "d-1",
                wrepjson_title =  "off",
                )

    t_1_graph = mgraph(
                        graph_line_colour = "navy",
                        graph_line_style  = "dot",
                        graph_line_thickness =  5,
                        legend =  "on",
                        legend_user_text = "D-1"
                    )
    t_2 = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  t850,
                wrepjson_keyword =  "d-2",
                wrepjson_title =  "off",
                )

    t_2_graph = mgraph(
                        graph_line_colour = "navy",
                        graph_line_style  = "dot",
                        graph_line_thickness =  3,
                        legend =  "on",
                        legend_user_text = "D-2"
                    )
    tempe_mean = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  t850,
                wrepjson_keyword =  "130_mean",
                wrepjson_title =  "off",
                )

    tempe_mean_graph = mgraph(
                        graph_line_colour = "#BF00CD",
                        graph_line_style  = "solid",
                        graph_line_thickness =  3,
                        legend =  "on",
                        legend_user_text = "Mean"
                    )

     # definition of the title

    lines = [ "ECMWF Ensemble forecasts <json_info key='station_name'/>",
              "Location: <json_info key='location'/>",
              "Base Time: <json_info key='date'/>",
              "<font size='0.1'> .  </font>",
              "<font size='0.4'><json_info key='parameter_info'/></font><font size='0.4'> - Probability for </font><font size='0.4'><json_info key='plumes_interval'/></font><font size='0.4'>&deg;</font><font size='0.4'>C intervals</font>",
             ]

    title = mtext(
        text_lines=lines,
        text_html='true',
        text_colour='#0b265a',
        text_font_size=0.5,
        text_mode='positional',
        text_box_x_position=1.,
        text_box_y_position=7.8,
        text_box_x_length=10.,
        text_box_y_length=2.5,
        text_border='off',
        text_justification='left',
        )
    legend = mlegend(
        #legend_box_mode='positional',
        legend_box_x_position=12.,
        legend_box_y_position=8.5,
        legend_box_x_length=11.,
        legend_box_y_length=2.5,
        legend_border='off',
        #legend_column_count =  5,
        #legend_entry_text_width =  60.,
        legend_text_colour =  "#0b265a",
        legend_text_composition =  "user_text_only",
        legend_text_font_size =  0.35,
        legend_user_lines =  ["0.5-10%" , "1-30%", "30-50%", "50-100%",  "HRES", "Control", "ENS" ]
        )
    # To the plot
    middle = page(
            layout='positional',
            page_x_length=21.,
            page_y_length=9.,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=9.5
            )

    rr = mwrepjson(
                wrepjson_family =  "eps",
                wrepjson_input_filename =  precip,
                wrepjson_parameter =  "143",
                wrepjson_parameter_information= "Ensemble members of Total Precipitation (mm/6h)",
                wrepjson_y_percentage = 15.
                )

    precip_plumes = mepsplumes (
                        eps_plume_line_colour = "#4a80e8",
                        eps_plume_forecast_line_colour = "#cd0174",
                        eps_plume_forecast_line_thickness = 5,
                        eps_plume_control_line_thickness = 5,
                        eps_plume_control_line_colour = "#cd0174",
                        eps_plume_line_style = "dot",
                        eps_plume_line_thickness = 2,
                        eps_plume_legend='off'
                        )
    precip_mean = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  precip,
                wrepjson_keyword =  "143_mean",
                wrepjson_title =  "off",
                )

    precip_mean_graph = mgraph(
                        graph_line_colour = "#BF00CD",
                        graph_line_style  = "dot",
                        graph_line_thickness =  3,
                        legend_user_text = "Mean"
                    )

    rr_1 = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  precip,
                wrepjson_keyword =  "d-1",
                wrepjson_title =  "off",
                )

    rr_1_graph = mgraph(
                        graph_line_colour = "navy",
                        graph_line_style  = "dot",
                        graph_line_thickness =  5,
                        legend =  "on",
                        legend_user_text = "D-1"
                    )
    rr_2 = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  precip,
                wrepjson_keyword =  "d-2",
                wrepjson_title =  "off",
                )

    rr_2_graph = mgraph(
                        graph_line_colour = "navy",
                        graph_line_style  = "dot",
                        graph_line_thickness =  3,
                        legend =  "on",
                        legend_user_text = "D-2"
                    )
    short_title = mtext(
        text_lines=["<json_info key='parameter_info'/>"],
        text_html='true',
        text_colour='#0b265a',
        text_font_size=0.4,
        text_mode='positional',
        text_box_x_position=1.,
        text_box_y_position=7.5,
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
            page_y_position=1.
            )

    geop = mwrepjson(
                wrepjson_family =  "eps",
                wrepjson_input_filename =  z500,
                wrepjson_parameter =  "129",
                wrepjson_parameter_information =  "Geopotential at 500 hPa - Probability for 2.5dam intervals",
                wrepjson_plumes_interval = 2.5,
                wrepjson_y_percentage = 15.
                )

    geop_contour = mcont(
                    legend="off",
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

    geop_plumes = mepsplumes (
                    eps_plume_line_colour = "#e86349",
                    eps_plume_forecast_line_colour = "#cee849",
                    eps_plume_forecast_line_thickness = 5,
                    eps_plume_control_line_thickness = 5,
                    eps_plume_control_line_colour = "#cee849",
                    eps_plume_line_style = "dot",
                    eps_plume_line_thickness = 2,
                    eps_plume_legend='off'
                    )
    geop_mean = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  z500,
                wrepjson_keyword =  "129_mean", wrepjson_title =  "off",

                )

    geop_mean_graph = mgraph(
                        graph_line_colour = "#cee849",
                        #graph_line_colour = "red",
                        graph_line_style  = "dot",
                        graph_line_thickness =  3,
                        legend =  "on",
                        legend_user_text = "Mean"
                    )

    geop_1 = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  z500,
                wrepjson_keyword =  "d-1", wrepjson_title =  "off",
                )

    geop_1_graph = mgraph(
                        graph_line_colour = "#0b265a",
                        graph_line_style  = "dot",
                        graph_line_thickness =  5,
                        legend =  "on",
                        legend_user_text = "D-1"
                    )
    geop_2 = mwrepjson(
                wrepjson_family =  "data",
                wrepjson_input_filename =  z500,
                wrepjson_keyword =  "d-2", wrepjson_title =  "off",

                )

    geop_2_graph = mgraph(
                        graph_line_colour = "#0b265a",
                        graph_line_style  = "dot",
                        graph_line_thickness =  3,
                        legend =  "on",
                        legend_user_text = "D-2"
                    )


    plot(
        out,
        top,
        projection,
        horizontal,
        vertical,
        tempe, tempe_contour, tempe_plumes,
        title,

        tempe_mean, tempe_mean_graph,
        t_1, t_1_graph,t_2, t_2_graph,
        legend,
        middle,
        projection, horizontal, vertical,
        rr, precip_plumes,
        precip_mean, precip_mean_graph,
        rr_1, rr_1_graph,rr_2, rr_2_graph,
        short_title,
        bottom,
        projection, horizontal_bot, vertical,
        geop, geop_contour, geop_plumes,
        geop_mean, geop_mean_graph,
        geop_1, geop_1_graph, geop_2, geop_2_graph,
        short_title
        )

def main():



    if len(sys.argv) != 2 :
        print "No station list given"
        return 1

    try:
        stations = json.loads(open(sys.argv[1]).read())
        params = ["t850", "precip", "z500"]

        for param in params :
            print "get", param
            data(stations["stations"], configuration[param])
        for station in stations["stations"]:
            plumes(station)
        return 0


    except GribInternalError,err:
        if VERBOSE:
            traceback.print_exc(file=sys.stderr)
        else:
            print >>sys.stderr,err.msg

        return 1

if __name__ == "__main__":
    sys.exit(main())
