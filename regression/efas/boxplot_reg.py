from csv import reader
from datetime import datetime
from Magics.macro import *
#read input file
f= file('boxplot.data','rb')
r= reader(f,delimiter=';')
rows= []
for row in r:
	#skip useless lines
	if row[0][0]=='X': continue
	if row[0][0]=='-': continue
	if row[0][0]==' ': continue
	rows+= [row]
f.close()

#limit values
max_date= '0'
min_date= '9'
max_y=    -10000000000
min_y=     10000000000

#dictionary structure
data= {}

data['WB_FOR']= {} 
data['WB_FOR']['DATE']= []
data['WB_FOR']['DWD']=  []
data['WB_FOR']['EUD']=  []

data['WB_OBS']= {} 
data['WB_OBS']['DATE']= []
data['WB_OBS']['OBS']=  []

data['WB_OBS']= {} 
data['WB_OBS']['DATE']= []
data['WB_OBS']['OBS']=  []

data['FOR_DETETMINISTIC']= {} 
data['FOR_DETETMINISTIC']['DATE']= []
data['FOR_DETETMINISTIC']['DWD']=  []
data['FOR_DETETMINISTIC']['EUD']=  []

data['FOR_PROBABILISTIC']= {} 
data['FOR_PROBABILISTIC']['DATE']= []
data['FOR_PROBABILISTIC']['QMIN']= []
data['FOR_PROBABILISTIC']['Q25']=  []
data['FOR_PROBABILISTIC']['QMED']= []
data['FOR_PROBABILISTIC']['Q75']=  []
data['FOR_PROBABILISTIC']['QMAX']= []

#fill the dictionary
row_type= ''
for row in rows:
	f1= row[0]
	if f1.find('WB_FOR')==0:
		row_type= 'FOR'
		continue
	if f1.find('WB_OBS')==0:
		row_type= 'OBS'
		continue
	if f1.find('FOR_DETETMINISTIC')==0:
		row_type= 'DET'
		continue
	if f1.find('FOR_PROBABILISTIC')==0:
		row_type= 'PRO'
		continue
	if f1.find('THlow')==0:
		data['THlow']=     float(row[0].split('=')[1])
		data['THmedium']=  float(row[1].split('=')[1])
		data['THHigh']=    float(row[2].split('=')[1])
		data['THextreme']= float(row[3].split('=')[1])
		continue
	#convert numbers format
	row= [row[0]]+[float(ele) for ele in row[1:]]
	if row_type=='FOR':
		date= datetime.strptime(row[0],'%m/%d/%Y %I:%M:%S %p')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:%S')
		data['WB_FOR']['DATE']+= [row[0]]
		data['WB_FOR']['DWD']+=  [row[1]]
		data['WB_FOR']['EUD']+=  [row[2]]
	if row_type=='OBS':
		date= datetime.strptime(row[0],'%m/%d/%Y %I:%M:%S %p')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:%S')
		data['WB_OBS']['DATE']+= [row[0]]
		data['WB_OBS']['OBS']+=  [row[1]]
	if row_type=='DET':
		date= datetime.strptime(row[0],'%d/%m/%Y %I:%M %p')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:00')
		data['FOR_DETETMINISTIC']['DATE']+= [row[0]]
		data['FOR_DETETMINISTIC']['DWD']+=  [row[1]]
		data['FOR_DETETMINISTIC']['EUD']+=  [row[2]]
	if row_type=='PRO':
		date= datetime.strptime(row[0],'%m/%d/%Y %I:%M:%S %p')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:%S')
		data['FOR_PROBABILISTIC']['DATE']+= [row[0]]
		data['FOR_PROBABILISTIC']['QMIN']+= [row[1]]
		data['FOR_PROBABILISTIC']['Q25']+=  [row[2]]
		data['FOR_PROBABILISTIC']['QMED']+= [row[3]]
		data['FOR_PROBABILISTIC']['Q75']+=  [row[4]]
		data['FOR_PROBABILISTIC']['QMAX']+= [row[5]]

	#calculate range of dates
	max_date= max(max_date,row[0])
	min_date= min(min_date,row[0])

	#do not use missing values when calculating extreme values
	values= [ele for ele in row[1:] if ele!=1.7e+308]
	max_y=    max([max_y]+values)
	min_y=    min([min_y]+values)

#################################################################
#test
for key in data:
	obj= data[key]
	if type(obj)==type({}):
		for k2 in obj: print key,k2,obj[k2]
	else:
		print key, obj
print 'limits values:'
print 'x:',[min_date,max_date]
print 'y:',[min_y,max_y]
#################################################################

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name="boxplot_reg")

min = round(min_y - ((max_y-min_y)*0.1))
max = round(max_y + ((max_y-min_y)*0.1))

# Setting the cartesian view
projection = mmap(
    subpage_y_position=2.,
    subpage_map_projection='cartesian',
    subpage_x_axis_type='date',
    subpage_y_axis_type='regular',
    subpage_x_date_min=min_date,
    subpage_x_date_max=max_date,
    subpage_y_min=min,
    subpage_y_max=max,
    )

# Vertical axis
vertical = maxis(
    axis_orientation='vertical',
    axis_type='regular',
    axis_tick_label_height=0.40,
    axis_tick_label_colour='navy',
    axis_grid='on',
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    axis_title='on',
	axis_title_text='Discharge (m3/s)',
    axis_title_font_style='bold',
    )

# Horizontal axis
horizontal = maxis(
    axis_orientation='horizontal',
    axis_type='date',
    axis_grid='on',
    axis_days_label_height=0.40,
    axis_months_label_height=0.40,
    axis_years_label_height=0.50,
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    axis_title='on',
	axis_title_text='Date (days)',
    axis_title_font_style='bold',
    )

# dwd black curve
dwd_input = minput(input_x_type='date',
                   input_date_x_values=data['FOR_DETETMINISTIC']['DATE'],
                   input_y_values=data['FOR_DETETMINISTIC']['DWD'])

dwd_graph = mgraph(graph_line_colour='black',
                   graph_line_thickness=4,
				   legend='on',
				   legend_user_text="DWD")

# eud red curve
eud_input = minput(input_x_type='date',
                   input_date_x_values=data['FOR_DETETMINISTIC']['DATE'],
                   input_y_values=data['FOR_DETETMINISTIC']['EUD'])

eud_graph = mgraph(graph_line_colour='red',
                   graph_line_thickness=4, 
				   legend='on',
				   legend_user_text="ECMWF")

# box plot

boxplot = mboxplot(boxplot_date_positions=data['FOR_PROBABILISTIC']['DATE'],
        boxplot_minimum_values = [x-50. for x in data['FOR_PROBABILISTIC']['QMIN']],
        boxplot_maximum_values = [x +50. for x in data['FOR_PROBABILISTIC']['QMAX']],
        boxplot_box_upper_values = [x +50. for x in data['FOR_PROBABILISTIC']['Q75']],
        boxplot_box_lower_values = data['FOR_PROBABILISTIC']['Q25'],
        boxplot_median_values = data['FOR_PROBABILISTIC']['QMED'],
		boxplot_box_colour = "rgb(0.65,0.58,0.92)")

# wb_obs
obs_input = minput(input_x_type='date',
                   input_date_x_values=data['WB_OBS']['DATE'],
                   input_y_values=data['WB_OBS']['OBS'])
                   


# obs plotting
obs_symb = msymb(
    symbol_type='marker',
    symbol_colour='black', 
    symbol_height=0.5,
    symbol_marker_index=15,
    legend='on',
	legend_user_text="WB_obs"
    )


# wb_for for eud
eud_for_input = minput(input_x_type='date',
                   input_date_x_values=data['WB_FOR']['DATE'],
                   input_y_values=data['WB_FOR']['EUD'])
# obs plotting
eud_symb = msymb(
    symbol_type='marker',
    symbol_colour='black', 
    symbol_height=0.3,
    symbol_connect_line='false',
    symbol_marker_index=15,
    legend='on',
	legend_user_text="WB_ECMWF"
    )

# wb_for for dwd
dwd_for_input = minput(input_x_type='date',
                   input_date_x_values=data['WB_FOR']['DATE'],
                   input_y_values=data['WB_FOR']['DWD'])
# obs plotting
dwd_symb = msymb(
    symbol_type='marker',
    symbol_colour='red', 
    symbol_height=0.3,
    symbol_marker_index=15,
    legend='on',
	legend_user_text="WB_DWD",
    )

# wb_for for dwd
dates = [min_date, max_date]
print data["THlow"]
lows = [data["THlow"], data["THlow"]]
mediums = [data["THmedium"], data["THmedium"]]
highs = [data["THHigh"], data["THHigh"]]
extremes = [data["THextreme"], data["THextreme"]]
                

green ='rgb(0.78,0.95,0.17)'
yellow ='rgb(0.98,0.96,0.02)'
red ='rgb(0.93,0.34,0.35)'
purple ='rgb(0.79,0.35,0.95)'
low =  mgraph(
				x_date_values = dates,
				x2_date_values = dates,
				y_values = lows,
				y2_values = mediums,
				graph_line_colour=green,
				graph_shade_colour=green,
                graph_line_thickness=4, 
                graph_type='area', 
                graph_shade='on', 
				legend='on',
				legend_user_text="Low%.2f"% (data["THlow"]))      

medium =  mgraph(
				x_date_values = dates,
				x2_date_values = dates,
				y_values = mediums,
				y2_values = highs,
				graph_line_colour=yellow,
				graph_shade_colour=yellow,
                graph_line_thickness=4, 
                graph_type='area', 
                graph_shade='on', 
				legend='on',
				legend_user_text="Med-%.2f"% (data["THmedium"]))      

high =  mgraph(
				x_date_values = dates,
				x2_date_values = dates,
				y_values = highs,
				y2_values = extremes,
				graph_line_colour=red,
				graph_shade_colour=red,
                graph_line_thickness=4, 
                graph_type='area', 
                graph_shade='on', 
				legend='on',
				legend_user_text="High-%.2f" % (data["THHigh"]))      
extreme =  mgraph(
				x_date_values = dates,
				x2_date_values = dates,
				y_values = extremes,
				y2_values = [max, max],
				graph_line_colour="yellow",
				graph_shade_colour=purple,
                graph_line_thickness=6, 
                graph_type='area', 
                graph_shade='on', 
				legend='on',
				legend_user_text="Sev-%.2f" % (data["THextreme"]))      
 
plot(output, projection, vertical, horizontal, 
        low , medium, high, extreme,
	 	eud_input, eud_graph,
		dwd_input, dwd_graph,
        boxplot,
		eud_for_input, eud_symb,
		dwd_for_input, dwd_symb,
        obs_input, obs_symb
        
	)	
