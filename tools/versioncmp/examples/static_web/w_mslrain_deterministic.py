# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


from MetPy import *
from MagPy import *
from BasePy.Memory import Memory
import sys

print "Beginning of script --------------"
print Memory()

print "plotting:"
arg_dict = {}
for i in sys.argv[1:]:
    print i
    arg_name,arg_value = string.split(i,"=")
    arg_list = string.split(arg_value,",")
    arg_dict[arg_name] = arg_list
print

mslp_fieldset = FieldSet("mslp.grib") / 100
mslp_index = FieldIndex(mslp_fieldset,"step")
convective_precip = FieldSet("cp.grib") * 1000
large_scale_precip = FieldSet("lsp.grib") * 1000

total_precip = convective_precip + large_scale_precip
precip_index = FieldIndex(total_precip,"step")

coast = Coastlines(libfile='magpylib.cfg',libname="coastlines",map_coastline_land_shade="off",)
mslp_contour = Contour(libfile='magpylib.cfg',libname="mslp",)
rainfall_contour = Contour(libfile='magpylib.cfg',libname="rainfall",)
#box = LegendBox(legend_box_x_position = 104,legend_box_y_position = 0,legend_box_x_length   = 5, legend_box_y_length=100,legend_display_type='continuous',)

box = LegendBox(legend_box_x_position=100,legend_box_y_position=5,legend_box_x_length=10,
                legend_box_y_length=90,legend_text_font_size="0.4",legend_text_colour="black",legend_display_type='continuous',)


#---------------------------------------------------------------
# we compute the accumulation of rain once for all.
#---------------------------------------------------------------
rainfall = {}
for step in arg_dict["step"]:
	step=int(step)
	if step <> 0 and step <> 240:
		precip_minus = precip_index.constrained_access(wanted=1,step=(step-6))
		precip_plus  = precip_index.constrained_access(wanted=1,step=(step+6))
		rainfall[step] = precip_plus - precip_minus

print "Before big loop --------------"
print Memory()

for area in arg_dict["area"]:
	geography = CornerArea(libfile="areas.cfg",libname=area)
	layout = Layout(orientation="landscape",format="a4",layout=SimpleLayout(1,1),)

	width = geography.configuration("width",85) 
	s = SubLayout(
		coastlines   =   coast,
		geography = geography,
		layout = AbsoluteLayout([
			[4,1,width,95],
		]),
		plot_coastlines = "both",
		page_id_line                    =   "off",
		page_id_line_system_plot        =   "off",
		page_id_line_date_plot          =   "off",
		page_id_line_errors_plot        =   "off",
		page_id_line_logo_plot          =   "off",
		page_id_line_user_text          =   str(arg_dict["text"][0]),
	)

	for step in arg_dict["step"]:
		print Memory()
		mslp = mslp_index.constrained_access(wanted=1,step=step)
		step = int(step)
		print "plotting:",area," step:",step
		if step == 0 or step == 240:
			title = FieldAutoTitle(mslp,text = [None,"${titleParameterName}"])
			layout.plot(s(box,FieldInput(mslp),mslp_contour,title))
		else:
			title = FieldAutoTitle(mslp,text = [None,"${titleParameterName} / 12hr Accumulated precipitation (VT-6h/VT+6h)"])
			layout.plot(s(box,FieldInput(rainfall[step]),rainfall_contour,FieldInput(mslp),mslp_contour,title))

print "End of big loop --------------"
print Memory()					
