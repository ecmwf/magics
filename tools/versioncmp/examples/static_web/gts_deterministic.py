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

print "plotting:"
arg_dict = {}
for i in sys.argv[1:]:
	print i
	arg_name,arg_value = string.split(i,"=")
	arg_list = string.split(arg_value,",")
	arg_dict[arg_name] = arg_list
print

t850_fieldset = FieldSet("t850.grib") - 273.15
z500_fieldset = FieldSet("z500.grib") / 9.8065 / 10

t850_index = FieldIndex(t850_fieldset,"step")
z500_index = FieldIndex(z500_fieldset,"step")

coast 			= Coastlines(libfile = 'magpylib.cfg',libname	= "coastlines", map_coastline_land_shade = "off")
t850_contour	= Contour( libfile	= 'magpylib.cfg', libname =	"t850")
z500_contour	= Contour( libfile	= 'magpylib.cfg', libname =	"z500")

for area in arg_dict["area"]: 
	geography = CornerArea( libfile = "areas.cfg", libname	= area)

	layout = Layout(
		orientation						=	"landscape",
		format							=	"a4",
		layout							=	SimpleLayout(1,1),
		coastlines                      =   coast,
		geography                       =   geography,
		plot_coastlines                  =   "last",
		page_id_line                    =   "off",
		page_id_line_system_plot        =   "off",
		page_id_line_date_plot          =   "off",
		page_id_line_errors_plot        =   "off",
		page_id_line_logo_plot          =   "off",
		page_id_line_user_text          =   str(arg_dict["text"][0]),
	)

	for step in arg_dict["step"]:
		t850 = t850_index.constrained_access(wanted=1,step=step)
		z500 = z500_index.constrained_access(wanted=1,step=step)

		print "plotting:",area," step:",step
		title = FieldAutoTitle(t850,z500,text = [None,"${titleParameterName}"])
		layout.plot(FieldInput(t850),t850_contour,FieldInput(z500),z500_contour,title)

