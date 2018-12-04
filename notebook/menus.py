t_press = ['2m_dewpoint_temperature','2m_temperature','ice_temperature_layer_1',
            'ice_temperature_layer_2','ice_temperature_layer_3','ice_temperature_layer_4',
            'maximum_2m_temperature_since_previous_post_processing','mean_sea_level_pressure','minimum_2m_temperature_since_previous_post_processing',
            'sea_surface_temperature','skin_temperature','surface_pressure']
clouds = ['cloud_base_height','high_cloud_cover','low_cloud_cover',
            'medium_cloud_cover','total_cloud_cover','total_column_cloud_ice_water',
            'total_column_cloud_liquid_water','vertical_integral_of_divergence_of_cloud_frozen_water_flux','vertical_integral_of_divergence_of_cloud_liquid_water_flux',
            'vertical_integral_of_eastward_cloud_frozen_water_flux','vertical_integral_of_eastward_cloud_liquid_water_flux','vertical_integral_of_northward_cloud_frozen_water_flux',
            'vertical_integral_of_northward_cloud_liquid_water_flux']
lakes = ['lake_bottom_temperature','lake_cover','lake_depth',
            'lake_ice_depth','lake_ice_temperature','lake_mix_layer_depth',
            'lake_mix_layer_temperature','lake_shape_factor','lake_total_layer_temperature']
prec = ['convective_precipitation','convective_rain_rate','instantaneous_large_scale_surface_precipitation_fraction',
            'large_scale_precipitation','large_scale_precipitation_fraction','large_scale_rain_rate',
            'maximum_total_precipitation_rate_since_previous_post_processing','minimum_total_precipitation_rate_since_previous_post_processing','precipitation_type',
            'total_column_rain_water','total_precipitation']
snow = ['convective_snowfall','convective_snowfall_rate_water_equivalent','large_scale_snowfall',
            'large_scale_snowfall_rate_water_equivalent','snow_albedo','snow_density',
            'snow_depth','snow_evaporation','snowfall',
            'snowmelt','temperature_of_snow_layer','total_column_snow_water']
parameter_groups = {'Temperature and pressure' : t_press, 
                    'Clouds' : clouds, 
                    'Lakes' : lakes,
                    'Precipitation and rain' : prec,
                    'Snow' : snow}
types = ['ensemble_mean','ensemble_members','ensemble_spread',
            'reanalysis']
style = {'description_width': 'initial'}

group = widgets.Dropdown(options=parameter_groups,value = t_press,description='Group:', style=style)

parameters = widgets.Dropdown(options = t_press,description='Choose parameter:', style=style)


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

items_auto = [parameters, Type, dates, times]

box_layout = Layout(display='flex',
                    flex_flow='row',
                    align_items='stretch',
                    width='100%')

box_auto = Box(children=items_auto, layout=box_layout)

def on_value_change(change):
        parameters.options=change["new"]
    

    

group.observe(on_value_change, names='value')
