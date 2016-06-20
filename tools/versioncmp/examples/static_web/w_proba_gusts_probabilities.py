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
import os
import string

#--------------------------------------------------------------------
# Dictionary to know which contour to use depending on the 
# parameter. This is a bit ugly because it gets the $TASK sms
# variable.
#--------------------------------------------------------------------
contour_map = {
	'2t'	:	'blue',
	'ff'	:	'red',
	'gusts' :	'red',
	'tp'	:	'green',
	'tpr'	:	'green',
	'mwp'	:	'blue',
	'swh'	:	'blue',
	'rain'	:	'green',
}

task=os.environ['TASK']
l = string.split(task,'_')
contour_name = '_' + contour_map[l[-1]]
print 'using contour:',contour_name

print 'plotting:'
arg_dict = {}
for i in sys.argv[1:]:
    print i
    arg_name,arg_value = string.split(i,"=")
    arg_list = string.split(arg_value,",")
    arg_dict[arg_name] = arg_list

probas = FieldSet('probabilities.grib') 
missing = MissingValue(0)
missing.apply(probas)

coast = Coastlines(libfile='probabilities.cfg',libname='coastlines',)
contour1 = Contour(libfile='probabilities.cfg',libname='contour_proba' + contour_name)
contour2 = Contour(libfile='probabilities.cfg',libname='contour_proba' + contour_name + '1')
layout = Layout(orientation="landscape",format="a4",plot_coastlines="both", layout = SimpleLayout(1,1))
box = LegendBox(legend_box_x_position=100,legend_box_y_position=10,legend_box_x_length=10, legend_box_y_length=90,legend_text_font_size="0.4",legend_text_colour="black", legend_display_type='continuous',)
for area in arg_dict["area"]:

	geography = CornerArea(libfile='areas.cfg',libname=area,)
	width = geography.configuration("width",90)
	s = SubLayout(
		coastlines = coast,
		plot_coastlines = "both",
		geography = geography,
		layout = AbsoluteLayout([
			[1,4,width,90],
		]),
		page_id_line                    =   "off",
		page_id_line_system_plot        =   "off",
		page_id_line_date_plot          =   "off",
		page_id_line_errors_plot        =   "off",
		page_id_line_logo_plot          =   "off",
		page_id_line_user_text          =   str(arg_dict["text"][0]),
		)

	print 'plotting:',area
	for probs in probas:
		title = FieldAutoTitle(probs,text = [None,"${titleParameterName}"], text_font_size="0.37")
		layout.plot(s(box,FieldInput(probs),contour1,contour2,title))
		
