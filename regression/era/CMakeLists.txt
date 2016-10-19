
add_magics_test(NAME era_era SOURCES era.py IMAGE era.png RESOURCES obs.template obs.json THRESHOLD 2 PYTHON)
add_magics_test(NAME era_obsjson SOURCES obsjson.py IMAGE obsjson.png RESOURCES obs.template THRESHOLD 2 PYTHON)
add_magics_test(NAME era_generic SOURCES generic.py IMAGE generic.png RESOURCES small.template small.json THRESHOLD 2 PYTHON)
