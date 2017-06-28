# magics test executable run in script

add_magics_test(NAME web_scores SOURCES scores.json RESOURCES score-data.json IMAGE scores.png THRESHOLD 5 JSON)
add_magics_test(NAME web_15days_clim SOURCES 15days_clim.json RESOURCES 15days_clim.data IMAGE 15days_clim.png THRESHOLD 5 JSON)
add_magics_test(NAME web_wave SOURCES wave-template.json RESOURCES wave-data.json IMAGE wave.png THRESHOLD 5 JSON)
add_magics_test(NAME web_10days SOURCES 10days.json RESOURCES 10days.data IMAGE 10days.png THRESHOLD 5 JSON)
add_magics_test(NAME web_10days_special SOURCES 10days-special.json RESOURCES 10days-special.data IMAGE 10days-special.png THRESHOLD 5 JSON)
add_magics_test(NAME web_10days_zero SOURCES 10days-zero.json RESOURCES 10days-zero.data IMAGE 10days-zero.png THRESHOLD 5 JSON)
add_magics_test(NAME web_monthly_2t SOURCES monthly_2t.json RESOURCES monthly_2t.data IMAGE monthly_2t.png THRESHOLD 5 JSON)
add_magics_test(NAME web_epsgram_sample SOURCES epsgram_sample.py 
        RESOURCES cloud-cover-clim-data.json  cloud-data.json wave-data.json IMAGE epsgram_sample.ps THRESHOLD 5 PYTHON)

add_magics_test(NAME web_cmf_timeseries SOURCES cmf_timeseries.json  IMAGE cmf_timeseries.png DATA cmf.nc THRESHOLD 2 JSON)
add_magics_test(NAME web_cmf_advanced_timeseries SOURCES cmf_advanced_timeseries.json  IMAGE cmf_advanced_timeseries.png RESOURCES cmf2.nc THRESHOLD 2 JSON)

add_magics_test(NAME web_cmf_graph SOURCES cmf_graph.py  IMAGE cmf_graph.png DATA cmf.nc THRESHOLD 2 PYTHON)
add_magics_test(NAME web_preciptype SOURCES preciptype.py  IMAGE preciptype.png DATA fc_type.grib THRESHOLD 2 PYTHON)

add_magics_test(NAME efas_geojson SOURCES efas.py  IMAGE efas.png RESOURCES efas.json THRESHOLD 2 PYTHON)

add_magics_test(NAME web_globalefi SOURCES globalefi.magml  IMAGE globalefi.png DATA efi_2t0.grib  efi_fx0.grib  efi_tp0.grib  em0.grib THRESHOLD 2 MAGML)

add_magics_test(NAME web_cyclone SOURCES cyclone-template.json  IMAGE cyclone.png RESOURCES cyclone.json  THRESHOLD 2 JSON)
add_magics_test(NAME web_ptype SOURCES ptypegram.py  RESOURCES meteogram-ptype.json IMAGE ptypegram.png THRESHOLD 2 PYTHON)

#add_magics_test(NAME web_cyclone_trajectory SOURCES cyclone-trajectory-template.json  IMAGE cyclone-trajectory.png RESOURCES cyclone-trajectory.json THRESHOLD 2 JSON)

#add_magics_test(NAME web_wave_dir SOURCES wave-dir.json  IMAGE wave-dir.png RESOURCES wave-dir.data THRESHOLD 1 JSON)
#add_magics_test(NAME web_wave_dir_land SOURCES wave-dir-land.json  IMAGE wave-dir-land.png RESOURCES wave-dir-land.data THRESHOLD 1 JSON)


