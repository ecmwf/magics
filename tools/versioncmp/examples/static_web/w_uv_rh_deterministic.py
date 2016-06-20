# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


from MetPy import *
from MagPy import *

levels = [700,850]

print 'plotting:'
arg_dict = {}
for i in sys.argv[1:]:
	print i
	arg_name,arg_value = string.split(i,"=")
	arg_list = string.split(arg_value,",")
	arg_dict[arg_name] = arg_list
print

rh700 = FieldSet("rh.grib") 
w700 = FieldSet("wind.grib")

windex = FieldIndex(w700,"level","step")
rhindex = FieldIndex(rh700,"level","step")

coast = Coastlines(libfile='magpylib.cfg',libname='coastlines',)
rh700_contour = Contour(libfile='magpylib.cfg',libname="rh700_contour",)
rh700_contour1 = Contour(libfile='magpylib.cfg',libname="rh700_contour1",)
layout = Layout(orientation="landscape",format="a4",layout=SimpleLayout(1,1),)

for level in levels:
	for area in arg_dict["area"]:

		geography = CornerArea(libfile="areas.cfg",libname=area,)
		width = geography.configuration("width",82)
		s = SubLayout(
			coastlines = coast,
			plot_coastlines = "both",
			geography = geography,
			layout = AbsoluteLayout([
				[8,5,width,90],
			]),
			page_id_line                    =   "off",
			page_id_line_system_plot        =   "off",
			page_id_line_date_plot          =   "off",
			page_id_line_errors_plot        =   "off",
			page_id_line_logo_plot          =   "off",
			page_id_line_user_text          =   str(arg_dict["text"][0]),
			)
		box = LegendBox(legend_box_x_position=104,legend_box_y_position=20,
			legend_box_x_length   = 10,legend_box_y_length=100,legend_text_font_size="0.4",
			legend_display_type   = 'continuous',legend_title="on",legend_title_text="%",
			legend_text_maximum_height=0.5)

		thin = geography.configuration("thin")
		w700_wind = Wind(libfile='magpylib.cfg',libname="wind700850", wind_thinning_factor =  thin)

		for step in arg_dict["step"]:

			print "plotting:",area," level:",level," step:",step
			iw700 = windex.constrained_access(wanted=2,level = level,step=step) 
			irh700 = rhindex.constrained_access(wanted=1,level = level,step=step)
			title = FieldAutoTitle(irh700,text = [None,"${titleParameterName} / Winds"])
			layout.plot(s(box,FieldInput(irh700),rh700_contour,rh700_contour1,UVWindFieldInput(iw700[0],iw700[1]),w700_wind,title))

