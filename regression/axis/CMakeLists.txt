# magics test executable run in script

add_magics_test(NAME axis01 SOURCES axis01.F IMAGE axis01.ps THRESHOLD 2)
add_magics_test(NAME axis02 SOURCES axis02.F IMAGE axis02.ps THRESHOLD 2)
add_magics_test(NAME automatic_axis SOURCES automatic_axis.F IMAGE automatic_axis.ps THRESHOLD 2)
add_magics_test(NAME logarithmic SOURCES logarithmic.py IMAGE logarithmic.png THRESHOLD 2 PYTHON)
add_magics_test(NAME position_list SOURCES position_list.py IMAGE position_list.png THRESHOLD 2 PYTHON)


