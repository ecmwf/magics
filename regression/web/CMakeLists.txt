# magics test executable run in script

add_magics_test(NAME web_scores SOURCES scores.json RESOURCES score-data.json IMAGE scores.png THRESHOLD 10 JSON)
add_magics_test(NAME web_15days_clim SOURCES 15days_clim.json RESOURCES 15days_clim.data IMAGE 15days_clim.png THRESHOLD 10 JSON)
add_magics_test(NAME web_10days SOURCES 10days.json RESOURCES 10days.data IMAGE 10days.png THRESHOLD 10 JSON)
add_magics_test(NAME web_10days_special SOURCES 10days-special.json RESOURCES 10days-special.data IMAGE 10days-special.png THRESHOLD 10 JSON)
add_magics_test(NAME web_10days_zero SOURCES 10days-zero.json RESOURCES 10days-zero.data IMAGE 10days-zero.png THRESHOLD 10 JSON)
add_magics_test(NAME web_epsgram_sample SOURCES epsgram_sample.py 
        RESOURCES cloud-cover-clim-data.json  cloud-data.json wave-data.json IMAGE epsgram_sample.ps THRESHOLD 10 PYTHON)

