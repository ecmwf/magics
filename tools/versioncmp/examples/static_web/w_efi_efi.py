from MetPy import *
from MagPy import *
import sys

arg_dict = {}
for i in sys.argv[1:]:
	arg_name,arg_value = string.split(i,"=")
	arg_list = string.split(arg_value,",")
	arg_dict[arg_name] = arg_list

efi = FieldSet("efi.grib") 

lsmoro = FieldSet('lsmoroEPS.grib')
index = FieldIndex(lsmoro,'parameter')

lsm = index.constrained_access(wanted=1,parameter='lsm')
oro = index.constrained_access(wanted=1,parameter='z')
grib_oro = oro * lsm / 9.81


#--------------------------

coast = Coastlines(libfile='magpylib.cfg',libname='coastlines',)
common_common = Definition(libfile='efi.cfg',libname='common_common')
contour_in_common = Definition(libfile='efi.cfg',libname='contour_in_common')
shade_in_common = Definition(libfile='efi.cfg',libname='shade_in_common')
contour2 = Contour(libfile='efi.cfg',libname='contour2',)
shade2 = Contour(libfile='efi.cfg',libname='shade2')
contour = Contour(libfile='efi.cfg',libname='contour')
shade = Contour(libfile='efi.cfg',libname='shade')
contour3 = Contour(libfile='efi.cfg',libname='contour3',)
shade3 = Contour(libfile='efi.cfg',libname='shade3')
layout = Layout(orientation="landscape",format="a4",layout=SimpleLayout(1,1))
box = LegendBox(legend_box_x_position=100,legend_box_y_position=0,legend_box_x_length=10, legend_box_y_length=100,legend_text_font_size="0.4",legend_text_colour="black",legend_display_type='continuous',)

#merge the contours and shades
contour.merge(common_common)
contour.merge(contour_in_common)
contour2.merge(common_common)
contour2.merge(contour_in_common)
contour3.merge(common_common)
contour3.merge(contour_in_common)
shade.merge(shade_in_common)
shade.merge(common_common)
shade2.merge(shade_in_common)
shade2.merge(common_common)
shade3.merge(shade_in_common)
shade3.merge(common_common)

#---------------------------

for area in arg_dict["area"]:

	geography = CornerArea(libfile='areas.cfg',libname=area,)
	c_oro = geography.configuration('c_oro')
	contour_orography = Contour(libfile='efi.cfg',libname='contour_orography',
		contour_interval = c_oro,)
	width = geography.configuration("width",90)
	s = SubLayout(
		coastlines = coast,
		plot_coastlines = "both",
		geography = geography,
		layout = AbsoluteLayout([
			[1,2,width,95],
		]),
		page_id_line                    =   "off",
		page_id_line_system_plot        =   "off",
		page_id_line_date_plot          =   "off",
		page_id_line_errors_plot        =   "off",
		page_id_line_logo_plot          =   "off",
		page_id_line_user_text          =   str(arg_dict["text"][0]),
		)
	
	for efi_field in efi:

		width = geography.configuration('width',90)
		title = FieldAutoTitle(efi_field,text = [None,"${titleParameterName}"], text_font_size=0.4)
		if efi_field.get("param") == str('2ti'):
			layout.plot(s(box,FieldInput(grib_oro),contour_orography,FieldInput(efi_field),contour2,shade2,contour,shade,title))
		else:
			layout.plot(s(box,FieldInput(grib_oro),contour_orography,FieldInput(efi_field),contour3,shade3,title))

