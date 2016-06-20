# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

from csv import reader
from datetime import datetime
from Magics.macro import *
#read input file
f= file('EUEtemp.txt','rb')
r= reader(f,delimiter=';')
rows= []
for row in r:
	
	if (len(row) == 0 ): continue
	#skip useless lines
	if row[0][0]=='X': continue
	if row[0][0]=='#': continue
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
data['FOR_PROBABILISTIC']['TMIN']= []
data['FOR_PROBABILISTIC']['T25']=  []
data['FOR_PROBABILISTIC']['TMED']= []
data['FOR_PROBABILISTIC']['T75']=  []
data['FOR_PROBABILISTIC']['TMAX']= []

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
	if f1.find('T0=')==0:
		row_type= 'T0'		
		date= datetime.strptime(row[0],'T0=%d/%m/%Y %H:%M')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:00')
		t0=row[0]
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
		date= datetime.strptime(row[0],'%d/%m/%Y %H:%M')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:00')
		data['WB_FOR']['DATE']+= [row[0]]
		data['WB_FOR']['DWD']+=  [row[1]]
		data['WB_FOR']['EUD']+=  [row[2]]
	if row_type=='OBS':
		date= datetime.strptime(row[0],'%d/%m/%Y %H:%M:%S %p')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:%S')
		data['WB_OBS']['DATE']+= [row[0]]
		data['WB_OBS']['OBS']+=  [row[1]]
	if row_type=='DET':
		date= datetime.strptime(row[0],'%d/%m/%Y %H:%M')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:00')
		data['FOR_DETETMINISTIC']['DATE']+= [row[0]]
		data['FOR_DETETMINISTIC']['DWD']+=  [row[1]]
		data['FOR_DETETMINISTIC']['EUD']+=  [row[2]]
	if row_type=='PRO':
		date= datetime.strptime(row[0],'%d/%m/%Y %H:%M')
		row[0]= datetime.strftime(date,'%Y-%m-%d %H:%M:00')
		data['FOR_PROBABILISTIC']['DATE']+= [row[0]]
		data['FOR_PROBABILISTIC']['TMIN']+= [row[1]]
		data['FOR_PROBABILISTIC']['T25']+=  [row[2]]
		data['FOR_PROBABILISTIC']['TMED']+= [row[3]]
		data['FOR_PROBABILISTIC']['T75']+=  [row[4]]
		data['FOR_PROBABILISTIC']['TMAX']+= [row[5]]

	#calculate range of dates
	max_date= max(max_date,row[0])
	min_date= min(min_date,row[0])
	
	

	

	#do not use missing values when calculating extreme values
	values= [ele for ele in row[1:] if ele!=1.7e+308]
	max_y=    max([max_y]+values)
	min_y=    min([min_y]+values)

#################################################################
#test


min = datetime.strptime(min_date,'%Y-%m-%d %H:%M:00')
# Round the date 
min_date =  datetime.strftime(min,'%Y-%m-%d 00:00:00')
base =  datetime.strptime(min_date,'%Y-%m-%d %H:%M:00')
ref = datetime.strptime(t0,'%Y-%m-%d %H:%M:00')

#################################################################

output = output(output_formats=['png'],
                output_name_first_page_number='off',
                output_name="tempe")

min = round(min_y - ((max_y-min_y)*0.1))
max = round(max_y + ((max_y-min_y)*0.1))

width = 27.
height =  (2*width)/4

# Setting the cartesian view
projection = mmap(
	super_page_x_length=width+3,
	super_page_y_length = height+5,
    subpage_x_position=2.2,
	subpage_y_position=2.8,
    subpage_map_projection='cartesian',
	subpage_x_length= width,
	subpage_y_length= height,
    subpage_x_axis_type='date',
    subpage_y_axis_type='regular',
    subpage_x_date_min=min_date,
    subpage_x_date_max=max_date,
    subpage_y_min=min,
    subpage_y_max=max,
	page_id_line="off",
    )

# Vertical axis
vertical = maxis(
    axis_orientation='vertical',
    axis_type='regular',
    axis_tick_label_height=0.70,
    axis_tick_label_colour='navy',
    axis_grid='on',
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
    axis_title='on',
	axis_title_text='Temperature [&deg;C]',
	axis_title_height=0.7,
    )



# Horizontal axis
horizontal = maxis(
    axis_orientation='horizontal',
    axis_type='date',
    axis_grid='on',
    axis_days_label_height=0.50,
    axis_months_label_height=0.50,
    axis_years_label_height=0.50,
    axis_grid_colour='grey',
    axis_grid_thickness=1,
    axis_grid_line_style='dot',
	axis_days_sunday_label_colour='black',
    axis_title='on',
	axis_title_text='Date',
	axis_title_height=0.7,
	axis_days_label_position=0
	
    )

# dwd black curve
dwd_input = minput(input_x_type='date',
                   input_date_x_values=data['FOR_DETETMINISTIC']['DATE'],
                   input_y_values=data['FOR_DETETMINISTIC']['DWD'])

dwd_plot = msymb(symbol_type='marker',
				symbol_colour='black', 
				symbol_height=0.5,
				symbol_marker_index=15,
				legend='on',
				symbol_outline='on',
				symbol_outline_thickness=2,
				legend_text_colour='black',
				#legend_text_font_size=0.4,
				legend_user_text="DWD")

# eud red curve
eud_input = minput(input_x_type='date',
                   input_date_x_values=data['FOR_DETETMINISTIC']['DATE'],
                   input_y_values=data['FOR_DETETMINISTIC']['EUD'])

eud_plot = msymb(symbol_type='marker',
				symbol_colour='red',
				symbol_height=0.5,
				symbol_outline='on',
				symbol_outline_thickness=2,
				symbol_marker_index=15,
				legend='on',
				legend_text_colour='black',
				#legend_text_font_size=0.4,
				legend_user_text="EUD")



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

t75_25_input = minput(input_x_type='date',
  input_date_x_values = data['FOR_PROBABILISTIC']['DATE'],
  input_y2_values = data['FOR_PROBABILISTIC']['T75'],
  input_date_x2_values = data['FOR_PROBABILISTIC']['DATE'],
  input_y_values = data['FOR_PROBABILISTIC']['T25'],
)

t75_25_graph = mgraph(graph_type = "area",
  graph_line_colour='rgb(0.8509804, 0.8509804, 1)',
  graph_shade_colour = "rgba(0.8509804, 0.8509804, 1,1)",
  legend='on',
  legend_user_text="EUE 25%-75%  "
  
)

tmin_input = minput(input_x_type='date',
  input_date_x_values = data['FOR_PROBABILISTIC']['DATE'],
  input_y_values = data['FOR_PROBABILISTIC']['TMIN'],
)

tmin_graph = mgraph(
  graph_line_colour='rgb(0.8509804, 0.8509804, 1.0000000)',
  graph_line_thickness=2,
  legend='off',
  legend_user_text="Min VAREPS"
)

tmax_input = minput(input_x_type='date',
  input_date_x_values = data['FOR_PROBABILISTIC']['DATE'],
  input_y_values = data['FOR_PROBABILISTIC']['TMAX'],
)

tmax_graph = mgraph(
  graph_line_colour='rgb(0.8509804, 0.8509804, 1.0000000)',
  legend='on',
  graph_line_thickness=2,
  legend_user_text="EUE 0%-100%"
)
# forecast base time
ref_input = minput(input_x_type='date',
                   input_date_x_values=[t0, t0],
                   input_y_values=[min, max])

ref_graph = mgraph(
  graph_line_colour='grey',
  legend='off',
  graph_line_thickness=4,
  
)
legend= mlegend(legend='on',
				legend_text_colour='black',
				legend_text_font_size="0.7",
				legend_entry_text_width=70.
				)
				
plot(output, projection, vertical, horizontal, 
	  ref_input, ref_graph,
	  t75_25_input, t75_25_graph,
	  tmin_input, tmin_graph,
	  tmax_input, tmax_graph,
      eud_input, eud_plot,
      dwd_input, dwd_plot,
	  legend
	)	
