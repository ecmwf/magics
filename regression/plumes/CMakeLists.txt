# magics test executable run in script

add_magics_test( NAME plumes_classic
                  SOURCES plumes.py
                  RESOURCES z500.json precip.json t850.json
                  IMAGE plumes.ps PYTHON)


add_magics_test( NAME plumes_monthly
                  SOURCES monthly_plumes.json
                  RESOURCES monthly_z500.json monthly_tp.json monthly_t850.json
                  IMAGE monthly_plumes.ps JSON)

