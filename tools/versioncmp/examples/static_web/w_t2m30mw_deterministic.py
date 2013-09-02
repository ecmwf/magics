
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

t2m_fieldset = FieldSet("2t.grib") - 273.15 
w30_fieldset = FieldSet("30mw.grib")
t2m_index = FieldIndex(t2m_fieldset,"step")
w30_index = FieldIndex(w30_fieldset,"step")

coast = Coastlines(map_coastline_colour='black',map_grid_colour='black',map_coastline_resolution = 'medium',
    map_grid_latitude_increment		=	20.0,map_grid_longitude_increment=20.0,
    map_label						=	'on',)

t2m_contour = Contour(libfile='magpylib.cfg',libname="t2m",)

box = LegendBox(legend_box_x_position=101,legend_box_y_position=95,legend_box_x_length=15,
    legend_box_y_length=40,legend_text_font_size="0.4",legend_text_colour="black",legend_display_type='continuous',)

for area in arg_dict["area"]:

    geography = CornerArea(libfile='areas.cfg',libname=area,)
    thin = geography.configuration("thin")
    w30_contour = Wind(libfile='magpylib.cfg',libname="w30", wind_thinning_factor  =   thin)
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

        t2m = t2m_index.constrained_access(wanted=1,step=step)
        w30 = w30_index.constrained_access(wanted=2,step=step)

        title = FieldAutoTitle(t2m,text = [None,"${titleParameterName} / 30 Metres Wind"])
        layout.plot(s(box, FieldInput(t2m),t2m_contour,UVWindFieldInput(w30[0],w30[1]),w30_contour,title))
