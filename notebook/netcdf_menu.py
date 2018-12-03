parameters = ['divergence','fraction_of_cloud_cover','geopotential',
            'ozone_mass_mixing_ratio','potential_vorticity','relative_humidity',
            'specific_cloud_ice_water_content','specific_cloud_liquid_water_content','specific_humidity',
            'specific_rain_water_content','specific_snow_water_content','temperature',
            'u_component_of_wind','v_component_of_wind','vertical_velocity',
            'vorticity']

pressure_levels = ['1','2','3',
            '5','7','10',
            '20','30','50',
            '70','100','125',
            '150','175','200',
            '225','250','300',
            '350','400','450',
            '500','550','600',
            '650','700','750',
            '775','800','825',
            '850','875','900',
            '925','950','975',
            '1000']

types = ['ensemble_mean','ensemble_members','ensemble_spread',
            'reanalysis']
style = {'description_width': 'initial'}


parameter = widgets.Dropdown(options = parameters, description='Choose parameter:', style=style)
level = widgets.Dropdown(options = pressure_levels, description='Choose level:', style=style)

Type = widgets.Dropdown(options=types, value = 'reanalysis', description='Product types:', style=style)

times = widgets.BoundedIntText(
    value=12,
    min=0,
    max=24,
    step=1,
    description='Hour:',
    disabled=False
)

d = datetime.datetime(2017, 1, 1, 12)

dates = widgets.DatePicker(
    description='Pick a Date',
    value=d,
    disabled=False
)

items_auto = [parameter, level, Type, dates, times]

box_layout = Layout(display='flex',
                    flex_flow='row',
                    align_items='stretch',
                    width='100%')

box_auto = Box(children=items_auto, layout=box_layout)
