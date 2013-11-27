# magics test executable run in script

add_magics_test(NAME web_scores SOURCES scores.json RESOURCES score-data.json IMAGE scores.png THRESHOLD 10 JSON)
add_magics_test(NAME web_10_days SOURCES classic_10days.json RESOURCES 10days.json IMAGE 10_days.ps THRESHOLD 10 JSON)

