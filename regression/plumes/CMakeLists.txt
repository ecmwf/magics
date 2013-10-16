# magics test executable run in script

add_magics_test( NAME plumes_classic
                  SOURCES plumes.py
                  RESOURCES z500.json precip.json t850.json
                  IMAGE plumes.ps PYTHON)


