#!/usr/bin/env python3
# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import json
import sys
import yaml


def tidy(x):
    if isinstance(x, (list, tuple)):
        return [tidy(y) for y in x]

    if isinstance(x, dict):
        d = {}
        for k, v in x.items():
            if "_list" in k:
                d[k] = [tidy(y) for y in v.split("/")]
            else:
                d[k] = tidy(v)
        return d

    if x == "off":
        return False

    if x == "on":
        return True

    try:
        return int(x)
    except ValueError:
        pass

    try:
        return float(x)
    except ValueError:
        pass

    return x


FIX = {
    "subpage_projection": "subpage_map_projection",
    "grib_tile": None,
    "subpage_margin_top": "view_margin_top",
    "subpage_margin_right": "view_margin_right",
    "subpage_margin_left": "view_margin_left",
    "subpage_margin_bottom": "view_margin_bottom",
    "subpage_fitted_mode": "view_fitted_mode",
}


def fix(prefix, x):
    d = {}
    for k, v in x.items():
        if not k.startswith(prefix):
            k = prefix + "_" + k
        k = FIX.get(k, k)
        if k is None:
            continue
        d[k] = v

    return d


with open(sys.argv[1]) as f:
    jactions = tidy(json.loads(f.read()))

print(jactions, file=sys.stderr)

drivers = jactions.pop("drivers")
assert len(drivers) == 1

output = fix("output", drivers[0])

page = jactions.pop("page")
map_ = page.pop("map")
plot = map_.pop("plot")

mcont = fix("contour", plot.pop("contour"))
mgrib = fix("grib", plot.pop("grib"))

if "border" in map_:
    output["page_frame"] = map_.pop("border")

mmap = fix("subpage", map_)

if "page_id" in page:
    output["page_id_line"] = page.pop("page_id")


yactions = [dict(output=output), dict(mmap=mmap), dict(mgrib=mgrib), dict(mcont=mcont)]


print(yaml.dump(dict(plot=yactions), default_flow_style=False))

print(jactions, file=sys.stderr)
print(page, file=sys.stderr)
