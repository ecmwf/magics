
from MetPy import *
from MagPy import *

col_list=['HSL(0,0,1)','HSL(29,0.12,0.92)','HSL(29,0.23,0.83)','HSL(29,0.35,0.75)','HSL(300,0.12,0.92)','HSL(344,0.17,0.84)','HSL(2,0.26,0.75)','HSL(10,0.37,0.67)','HSL(300,0.23,0.83)','HSL(326,0.26,0.75)','HSL(344,0.34,0.67)','HSL(355,0.43,0.58)','HSL(300,0.35,0.75)','HSL(318,0.37,0.67)','HSL(333,0.43,0.58)','HSL(344,0.5,0.5)','HSL(180,0.17,0.92)','HSL(138,0.08,0.84)','HSL(70,0.12,0.75)','HSL(50,0.22,0.67)','HSL(223,0.15,0.84)','HSL(262,0.04,0.75)','HSL(7,0.1,0.67)','HSL(19,0.21,0.59)','HSL(256,0.21,0.75)','HSL(289,0.16,0.67)','HSL(330,0.18,0.58)','HSL(352,0.26,0.5)','HSL(271,0.3,0.67)','HSL(294,0.27,0.59)','HSL(318,0.29,0.5)','HSL(337,0.34,0.42)','HSL(180,0.34,0.83)','HSL(166,0.24,0.75)','HSL(138,0.17,0.67)','HSL(99,0.17,0.58)','HSL(199,0.29,0.75)','HSL(194,0.18,0.67)','HSL(169,0.07,0.58)','HSL(67,0.08,0.5)','HSL(223,0.3,0.67)','HSL(231,0.19,0.58)','HSL(262,0.09,0.5)','HSL(339,0.1,0.42)','HSL(242,0.34,0.58)','HSL(256,0.25,0.5)','HSL(283,0.2,0.42)','HSL(317,0.2,0.33)','HSL(180,0.5,0.75)','HSL(171,0.4,0.67)','HSL(158,0.32,0.58)','HSL(138,0.26,0.5)','HSL(192,0.45,0.67)','HSL(187,0.34,0.59)','HSL(176,0.24,0.5)','HSL(152,0.15,0.42)','HSL(207,0.43,0.58)','HSL(207,0.32,0.5)','HSL(206,0.2,0.42)','HSL(203,0.08,0.33)','HSL(223,0.44,0.5)','HSL(227,0.33,0.42)','HSL(237,0.22,0.33)','HSL(262,0.13,0.25)']
cont_list=[-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,21.5,22.5,23.5,24.5,25.5,26.5,27.5,28.5,29.5,30.5,31.5,32.5,33.5,34.5,35.5,36.5,37.5,38.5,39.5,40.5,41.5,42.5,43.5,44.5,45.5,46.5,47.5,48.5,49.5,50.5,51.5,52.5,53.5,54.5,55.5,56.5,57.5,58.5,59.5,60.5,61.5,62.5,63.5]

print "plotting:"
arg_dict = {}
for i in sys.argv[1:]:
	print i
	arg_name,arg_value = string.split(i,"=")
	arg_list = string.split(arg_value,",")
	arg_dict[arg_name] = arg_list
print

coast = Coastlines(
	libfile							=	'magpylib.cfg',
	libname							=	"coastlines",
	map_coastline_land_shade        =   "off",
)

clouds_contour = Contour(
	libfile							=	'magpylib.cfg',
	libname							=	"clouds",
	contour_level_list              =  cont_list,
	contour_shade_colour_list       =  col_list,
)

clouds = FieldSet("clouds.grib") 
index = FieldIndex(clouds,"parameter","step")

for area in arg_dict["area"]:
	for step in arg_dict["step"]:

		lcc = index.constrained_access(wanted = 1,parameter = 'lcc',step = step)
		mcc = index.constrained_access(wanted = 1,parameter = 'mcc',step = step)
		hcc = index.constrained_access(wanted = 1,parameter = 'hcc',step = step)

		colour_resol = 4
		hcc_component = hcc * (colour_resol-1) + 0.5
		mcc_component = mcc * (colour_resol-1) + 0.5
		lcc_component = lcc * (colour_resol-1) + 0.5
		hcc_component.truncate()
		mcc_component.truncate()
		lcc_component.truncate()

		result = (colour_resol*colour_resol)*hcc_component
		result += colour_resol * mcc_component
		result += lcc_component

		geography = CornerArea( libfile = "areas.cfg", libname = area)

		layout = Layout(
			orientation						=	"landscape",
			format							=	"a4",
			geography						=	geography,
			layout							=	SimpleLayout(1,1),
			coastlines						=	coast,
			plot_coastlines                 =   "last",
			page_id_line                    =   "off",
			page_id_line_system_plot        =   "off",
			page_id_line_date_plot          =   "off",
			page_id_line_errors_plot        =   "off",
			page_id_line_logo_plot          =   "off",
			page_id_line_user_text          =   str(arg_dict["text"][0]),
		)
		title= FieldAutoTitle(result,text = [ None,'<font colour="HSL(29,0.43,0.75)"> Low </font><font colour="HSL(360,0.5,0.5)"> L+M </font><font colour="HSL(300,0.24,0.75)"> Medium </font><font colour="HSL(209,0.43,0.5)"> M+H </font><font colour="HSL(180,0.5, 0.75)"> High </font><font colour="HSL(120,0.24,0.5)"> H+L </font><font colour="black"> H+M+L</font> clouds'])
		print "plotting:",area," step:",step
		layout.plot(FieldInput(result),clouds_contour,title)

