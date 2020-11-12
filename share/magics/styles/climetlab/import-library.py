#!/usr/bin/env python3
import json
import yaml
import os

FIX = {
    "level_list": "contour_level_list",
    "shade": "contour_shade",
    "shade_colour_table": "contour_shade_colour_table",
    "shade_height_table": "contour_shade_height_table",
    "shade_marker_table": "contour_shade_marker_table",
    "shade_max_level": "contour_shade_max_level",
    "shade_min_level": "contour_shade_min_level",
    "shade_technique": "contour_shade_technique",
    "level_selection_type": "contour_level_selection_type",
}

SKIP = {
    # Not needed
    "contour_label_quality",
    # Marked as not implemented
    "wind_arrow_cross_boundary",
    # Does not exists
    "input_field_supress_below",
    "contour_hilo_suppress_radius",
    "contour_hilo_quality",
    "contour_data_transformation",
    "contour_above_line_colour",
    "contour_below_line_colour",
    "contour_below_line_style",
    "contour_line_plotting",
    "contour_split_level",
    "contour_split_line_colour",
    # Metview only below:
    "contour_shade_label_blanking",
    "grib_scaling_of_retrieved_fields",
    "grib_scaling_of_derived_fields",
    # xml is in 'unused'
    "image_colour_table_creation_mode",
    "image_pixel_selection_frequency",
}

UNITS = {
    "C": "degC",
}


def number(x):

    if x in ("None", "none"):
        return 0

    try:
        return int(x)
    except Exception:
        s = float(x)
        if s == int(s):
            return int(s)
        return s


def scale(x):
    if x > 1:
        return x / 255.0
    return x


def color(x):
    r = y = None
    try:
        x = [
            v.strip()
            for v in x.replace(" ", "").replace("(", ",").replace(")", "").split(",")
        ]
        r = x.pop(0)
        y = [number(v) for v in x]
        if x[0] == "rgb":
            y = [scale(v) for v in y]
        x = "%s(%s)" % (r, ",".join([str(v) for v in y]))

    except Exception:
        print(x, y, r)
        raise
    return x


def tidy(x):
    if isinstance(x, list):
        return [tidy(y) for y in x]

    if isinstance(x, dict):
        d = {}
        for k, v in x.items():
            if k in SKIP:
                continue

            k = FIX.get(k, k)
            assert (
                k.startswith("legend")
                or k.startswith("wind")
                or k.startswith("input")
                or k.startswith("contour")
                or k.startswith("symbol")
                or k.startswith("grib")
                or k.startswith("image")
            ), k
            d[k] = tidy(v)

            if k in (
                "contour_level_list",
                "contour_shade_height_table",
                "contour_line_thickness_rainbow_list",
                "contour_shade_marker_table",
                "symbol_advanced_table_level_list",
                "symbol_advanced_table_marker_list",
                "contour_gradients_step_list",
                "symbol_advanced_table_height_list",
            ):
                d[k] = [number(y) for y in d[k].split("/")]

            if k in (
                "contour_shade_cell_resolution",
                "contour_shade_max_level",
                "contour_shade_min_level",
                "contour_max_level",
                "contour_min_level",
                "contour_interval",
                "contour_reference_level",
                "wind_flag_length",
                "wind_flag_origin_marker_size",
                "wind_arrow_calm_below",
                "symbol_height",
                "symbol_marker_index",
                "contour_label_height",
                "symbol_text_font_size",
                "contour_internal_reduction_factor",
                "contour_interpolation_ceiling",
            ):
                d[k] = number(d[k])

            if "_colour" in k and isinstance(d[k], str):
                d[k] = d[k].lower()

            if k.endswith("_list") and isinstance(d[k], str):
                d[k] = tidy(d[k].split("/"))

            if k.endswith("_table") and isinstance(d[k], str):
                d[k] = tidy(d[k].split("/"))

        return d

    if x == "on":
        return True

    if x == "off":
        return False

    if isinstance(x, str):
        x = x.strip()
        assert x
        if "/" not in x and (
            x.startswith("rgb(")
            or x.startswith("rgba(")
            or x.startswith("hsl(")
            or x.startswith("hsv(")
        ):
            x = color(x)

    return x


here = os.path.dirname(__file__)
ecmwf = os.path.join(here, "..", "ecmwf")

with open(os.path.join(ecmwf, "styles.json")) as f:
    styles = json.loads(f.read())

for k, v in list(styles.items()):
    styles[k] = tidy(v)

matches = {}

for path in os.listdir(ecmwf):
    if path != "styles.json" and path.endswith(".json"):
        with open(os.path.join(ecmwf, path)) as f:
            x = json.loads(f.read())
            try:
                assert isinstance(x, list) and len(x) == 1, (path, x)
                assert "preferred_units" not in x[0], (path, x)
                assert "styles" in x[0], (path, x)
                matches[path[:-5]] = x[0]
            except Exception as e:
                print(e)

for k, v in matches.items():
    v.pop("eccharts_layer", None)
    units = v.pop("prefered_units", None)

    for n in v["styles"]:
        if n in styles:
            if units:
                if "contour_units" in styles[n]:
                    print("WARNING: more than one units for style", n)
                    styles[n]["contour_units"] = None
                else:
                    styles[n]["contour_units"] = UNITS.get(units, units)
        else:
            print("ERROR", k, "no style named", n)


for k, v in styles.items():

    if "contour_units" in v:
        if v["contour_units"] is None:
            del v["contour_units"]

    y = dict(magics=dict(mcont=v))
    with open(os.path.join(here, "styles", k + ".yaml"), "w") as f:
        print(yaml.safe_dump(y, default_flow_style=False), file=f)


for k, v in matches.items():

    y = v
    with open(os.path.join(here, "rules", k + ".yaml"), "w") as f:
        print(yaml.safe_dump(y, default_flow_style=False), file=f)
