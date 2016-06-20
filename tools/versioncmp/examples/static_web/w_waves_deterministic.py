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

m = MissingValue(0)
m.apply()
waveheight_fieldset = FieldSet("waveheight.grib")
wavedir_fieldset = FieldSet("wavedir.grib")
ice_fieldset = FieldSet("waveicedir.grib")

waveheight_index = FieldIndex(waveheight_fieldset,"step")
wavedir_index = FieldIndex(wavedir_fieldset,"step","param")
ice_index = FieldIndex(ice_fieldset,"step")

coast = Coastlines(libfile='magpylib.cfg',libname='coastlines',)
contour_wave = Contour(libfile='magpylib.cfg',libname="contour_wave",)
contour_ice = Contour(libfile='magpylib.cfg',libname="contour_ice",)

for area in arg_dict["area"]:

	geography = CornerArea(libfile='wave_areas.cfg',libname=area,)
	thin = geography.configuration("thin",5)
	contour_wind = Wind(libfile='magpylib.cfg',libname="icewind",
		wind_thinning_factor  =   thin,)
	layout = Layout(orientation="landscape",format="a4",layout=SimpleLayout(1,1),)

	width = geography.configuration("width",80)
	s = SubLayout(
		coastlines = coast,
		geography = geography,
		plot_coastlines = "last",
		layout = AbsoluteLayout([
		[3,8,width,90],
		]),
		page_id_line                    =   "off",
		page_id_line_system_plot        =   "off",
		page_id_line_date_plot          =   "off",
		page_id_line_errors_plot        =   "off",
		page_id_line_logo_plot          =   "off",
		page_id_line_user_text          =   str(arg_dict["text"][0]),
	)

	box = LegendBox(
		legend_box_x_position = 104,
		legend_box_y_position = 10,
		legend_box_x_length   = 10,
		legend_box_y_length   = 100,
		legend_display_type   = 'continuous',
		legend_title = 'on',
		legend_text_colour = 'black',
		legend_title_text = 'Significant wave height in meters',
		legend_text_maximum_height = .5
		)


	for step in arg_dict["step"]:

		waveheight = waveheight_index.constrained_access(wanted=1,step=step)
		ice = ice_index.constrained_access(wanted=1,step=step)
		wavedir = wavedir_index.constrained_access(wanted=1,step=step,param='mwd')
		print "plotting:",area," step:",step

		title = FieldAutoTitle(ice,text = [None,"Significant wave height and mean direction"])
		layout.plot(s(
				box,
				FieldInput(waveheight),
				contour_wave,
				FieldInput(ice),
				contour_ice,
				SDWindFieldInput(waveheight,wavedir),
				contour_wind,
			title))

