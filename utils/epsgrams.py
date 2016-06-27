# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import traceback
import sys

from gribapi import *
import json
from Magics.macro import *
import numpy
import datetime



reference = datetime.datetime.now()

def filename(path, param, type):
    return ( "%s/%s_%s.grib" % (path, type, param) )


def getdata(key, path, param, index, out):

    f = open( filename(path, param, key))

    mcount = grib_count_in_file(f)

    if key == "deterministic":
        key = "forecast"

    for i in range(mcount):
        gid = grib_new_from_file(f)
        if i == 0 :
            #param =  str(grib_get(gid, "parameter"))

            for l in range(len(index)):
                out[l][param][key] = []

        values = grib_get_elements(gid,"values",index)
        for l in range(len(index)):

            out[l][param][key].append(values[l])
        grib_release(gid)
    f.close()
    return


def control(path, param,  index, out ):


    f = open( filename(path, param, "control"))

    mcount = grib_count_in_file(f)

    for i in range(mcount):
        gid = grib_new_from_file(f)
        step = str(grib_get(gid, "step"))
        if i == 0 :
            #param =  str(grib_get(gid, "parameter"))
            date =  str(grib_get(gid, "date"))
            time =  "%02d" % grib_get(gid, "time")


            for l in range(len(index)):
                out[l][param] = {}
                out[l]["date"]  = date
                out[l]["time"]  = time
                out[l][param]["control"] = []
                out[l][param]["steps"] = []

        reference = datetime.datetime.strptime(date+time, '%Y%m%d%H%M')
        values = grib_get_elements(gid,"values",index)
        for l in range(len(index)):
            out[l][param]["control"].append(values[l])
            out[l][param]["steps"].append(step)
        grib_release(gid)
    f.close()
    return



def data(stations, param, path):
    out = []
    index = []

    lsm = open("%s/lsm.cf" % (path,) )

    detz_file = open("%s/det.z" % (path,) )
    epsz_file = open("%s/eps.z" % (path,) )
    gid = grib_new_from_file(lsm)
    detz = grib_new_from_file(detz_file)
    epsz = grib_new_from_file(epsz_file)


    for station in stations:
        nearest = grib_find_nearest(gid,station["lat"],station["lon"],1)
        name =  "%.2f_%.2f_%s.json" % (station["lat"], station["lon"], param)

        out.append({
            "station_name" : station["name"],
            "output" : name,
            "height": 8000.,
            "metadata": {
                "points_along_meridian" : grib_get(gid, "Nj"),
                "deterministic_height": 142.679443359,
                "eps_height": 149.973892212,
            },
            "location" : {  "lat" : station["lat"], "lon" : station["lon"] },
            "EPS_location" : {  "distance": nearest[0].distance, "latitude" : nearest[0].lat, "longitude" : nearest[0].lon },
            "forecast_location" : {  "distance": nearest[0].distance, "latitude" : nearest[0].lat, "longitude" : nearest[0].lon },
        })


        index.append(nearest[0].index)
        station[param] = name

    eps = grib_get_elements(epsz,"values",index)
    det = grib_get_elements(detz,"values",index)
    for l in range(len(index)):
            out[l]["metadata"]["deterministic_height"] = det[l]
            out[l]["metadata"]["eps_height"] = eps[l]

    grib_release(gid)
    grib_release(detz)
    grib_release(epsz)
    lsm.close()
    epsz_file.close()
    detz_file.close()



    control( path, param,  index, out )
    getdata("deterministic", path, param, index, out)
    getdata("max", path, param, index, out)
    getdata("min", path, param, index, out)
    getdata("median", path, param, index, out)
    getdata("ninty", path, param, index, out)
    getdata("seventyfive", path, param, index, out)
    getdata("twentyfive", path, param, index, out)
    getdata("ten", path, param, index, out)


    for i in out:
        f = open("%s.data" % (param, ), 'w')
        print >> f, json.dumps(i, indent=4, separators=(',', ': ') )



def epsgrams(location):

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

    y = 6.
    last = 21.5
    suby = 4.5
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

    # horizontal axis for bottom plot (with month and year)

    horizontal_bot = maxis(
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
        axis_months_label= "on",
        axis_months_label_colour=  "navy",
        axis_months_label_height = font_size,
        axis_tick_colour= "navy",
        axis_type =  "date",
        axis_years_label = "on",
        axis_years_label_colour=  "navy",
        axis_years_label_height = font_size
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

    cc_data =   mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = "tcc.data",
                        wrepjson_parameter = "tcc",
                        wrepjson_parameter_information =  "Total Cloud Cover (okta) ",
                        wrepjson_parameter_scaling_factor =  8.,
                        wrepjson_product_information =  "HRES Forecast and ENS Distribution "
                    )
    cc_graph = mepsgraph( eps_box_border_thickness = 2,
                          eps_box_width = 1.5)
    cc_text = mtext(
                text_colour =  "navy",
                text_font_size = 0.2,
                text_justification =  "left",
                text_line_1 =  "<font size='0.5'>EPS Meteogram</font><font size='0.6'><json_info key='expver'/></font>",
                text_line_2 =  "<font size='0.5'><json_info key='station_name'/></font><font size='0.6'><json_info key='location'/></font> <font size='0.6'><json_info key='grid_point'/></font> <font size='0.6'><json_info key='height'/></font>",
                text_line_3 =  "<font size='0.5'><json_info key='product_info'/></font> <font size='0.6'><json_info key='date'/></font>",
                text_line_4 = "<font size='0.5' colour='white'>.</font>",
                text_line_5 =  "<font size='0.35'><json_info key='parameter_info'/></font>",
                text_line_count =  5,
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

    pr_data = mwrepjson(
                        wrepjson_family =  "eps",
                        wrepjson_input_filename = "pr.data",
                        wrepjson_parameter = "pr",
                        wrepjson_parameter_information =  "Total Precipitation (mm/6h)",
                        wrepjson_parameter_scaling_factor =  1000.,
                        wrepjson_y_axis_threshold = 3.
                    )
    pr_graph = mepsgraph( eps_box_border_thickness = 2,
                          eps_box_width = 1.5,
                          eps_maximum_font_size =  0.35)

    ws_data = mwrepjson(wrepjson_family =  "eps",
                        wrepjson_input_filename = "ws.data",
                        wrepjson_parameter = "ws",
                        wrepjson_parameter_information =  "10m Wind Speed (m/s)",
                        )
    ws_graph = mepsgraph( eps_box_border_thickness = 2,
                          eps_box_width = 1.5,
                          )
    t2m_data = mwrepjson(

                        wrepjson_family =  "eps",
                        wrepjson_input_filename = "2t.data",
                        wrepjson_parameter = "2t",
                        wrepjson_parameter_information =  "2m Temperature(&deg;C)",
                        wrepjson_parameter_offset_factor = -273.15,
                        wrepjson_temperature_correction =  "on"
                    )
    t2m_graph = mepsgraph( eps_box_border_thickness = 2,
                          eps_box_width = 1.5,
                           eps_font_colour = "navy",
                        eps_legend_font_size =  font_size,
                        eps_grey_legend = "off",
                        legend='on'
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
    frame4 = page(
            layout='positional',
            page_x_length=21.,
            page_y_length=y,
            page_id_line='off',
            page_x_position=0.,
            page_y_position=last
            )

    text = mtext(
        text_lines=["<json_info key='parameter_info'/>"],
        text_html='true',
        text_colour='navy',
        text_font_size=font_size,
        #text_mode='positional',
        text_box_x_position=1.,
        text_box_y_position=5.1,
        text_box_x_length=10.,
        text_box_y_length=2.5,
        text_border='off',
        text_justification='left',
        )

    t2m_text = mtext(
        text_lines=["<json_info key='parameter_info'/><json_info key='full_temperature_correction_info'/>"],
        text_html='true',
        text_colour='navy',
        text_font_size=font_size,
        #text_mode='positional',
        text_box_x_position=1.,
        text_box_y_position=5.1,
        text_box_x_length=10.,
        text_box_y_length=2.5,
        text_border='off',
        text_justification='left',
        )
    legend = mlegend(
                legend_text_colour = "navy",
                legend_entry_text_width =  99.,
                legend_entry_plot_direction =  "row",
                legend_text_font_size =  font_size,
                legend_box_mode='positional',
                legend_box_x_position = 1.,
                legend_box_y_position = -4.5,
                legend_box_x_length   = 12.,
                legend_box_y_length   = 5.,
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
        pr_data, pr_graph, text,
        frame3,
        projection,
        horizontal,
        vertical,
        ws_data, ws_graph, text,
        frame4,
        projection,
        horizontal,
        vertical,
        t2m_data, t2m_graph, t2m_text,
        legend
        )

'''

'''
def main():

    if len(sys.argv) != 2 :
        print "No station list given"
        return 1

    try:
        stations = json.loads(open(sys.argv[1]).read())
        params = ["tcc", "pr", "ws", "2t"]
        for param in params :

            data(stations["stations"], param, "/tmp/cgs/epsdata/eps20160118000001/grib")
        for station in stations["stations"]:
            epsgrams(station)
        return 0


    except GribInternalError,err:
        if VERBOSE:
            traceback.print_exc(file=sys.stderr)
        else:
            print >>sys.stderr,err.msg

        return 1

if __name__ == "__main__":
    sys.exit(main())
