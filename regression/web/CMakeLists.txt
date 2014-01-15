# magics test executable run in script

add_magics_test(NAME web_scores SOURCES scores.json RESOURCES score-data.json IMAGE scores.png THRESHOLD 10 JSON)
add_magics_test(NAME web_15days_clim SOURCES 15days_clim.json RESOURCES 15days_clim.data IMAGE 15days_clim.png THRESHOLD 10 JSON)
add_magics_test(NAME web_10_days SOURCES classic_10days.json RESOURCES 10days.json IMAGE 10_days.ps THRESHOLD 10 JSON)
add_magics_test(NAME web_epsgram_sample SOURCES epsgram_sample.py 
        RESOURCES cloud-cover-clim-data.json  cloud-data.json wave-data.json IMAGE epsgram_sample.ps THRESHOLD 10 PYTHON)

