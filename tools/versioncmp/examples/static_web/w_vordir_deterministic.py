# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


from MetPy import *
from MagPy import *
import sys

print 'plotting:'
arg_dict = {}
for i in sys.argv[1:]:
	print i
	arg_name,arg_value = string.split(i,"=")
	arg_list = string.split(arg_value,",")
	arg_dict[arg_name] = arg_list
print

grib_files = ["vorticity.grib","divergence.grib"]
# --- vorticity has specific areas ...
list_areas=[[-30,-30,30,100],[-30,90,30,-140],[-30,-150,30,-20]]

w700 = FieldSet("wind.grib")
windex = FieldIndex(w700,"level","step")

for grib_file in grib_files:

	vor700_fieldset = FieldSet(grib_file) * 100000 
	vor700_index = FieldIndex(vor700_fieldset,"step")

	coast = Coastlines(libfile='magpylib.cfg',libname='coastlines',map_coastline_land_shade="off",)
	contour_vodiv_neg = Contour(libfile='magpylib.cfg',libname="vodiv_neg",)
	contour_vodiv_pos = Contour(libfile='magpylib.cfg',libname="vodiv_pos",)
	w700_wind = Wind(libfile='magpylib.cfg',libname="wind700rhdiv",)
	layout = Layout(orientation="landscape",format="a4",layout=SimpleLayout(1,1),plot_coastlines="last",)
	box = LegendBox(legend_box_x_position=104,legend_box_y_position=0,
		legend_box_x_length=6,legend_box_y_length=95,
		legend_display_type   = 'continuous',legend_title="on",legend_title_text="10**-5 s-1",
		legend_text_maximum_height=1,legend_text_quality="medium",)

	for area in list_areas:

		geography = CornerArea(projection='cylindrical',area=area,)

		for step in arg_dict["step"]:

			iw700 = windex.constrained_access(wanted=2,level = 700,step=step) 
			vor700 = vor700_index.constrained_access(wanted=1,step=step)

			s = SubLayout(
				coastlines = coast,
				plot_coastlines = "both",
				geography = geography,
				layout = AbsoluteLayout([
					[3,1,85,95],
				]),
				page_id_line                    =   "off",
				page_id_line_system_plot        =   "off",
				page_id_line_date_plot          =   "off",
				page_id_line_errors_plot        =   "off",
				page_id_line_logo_plot          =   "off",
				page_id_line_user_text          =   str(arg_dict["text"][0]),
			)

			print "plotting:",area," step:",step
			title = FieldAutoTitle(vor700,text = [None,"${titleParameterName} / v-velocity"])
			layout.plot(s(box,FieldInput(vor700),contour_vodiv_neg,contour_vodiv_pos,UVWindFieldInput(iw700[0],iw700[1]),w700_wind,title))
				

