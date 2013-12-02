# magics test executable run in script

add_magics_test(NAME axis01 SOURCES axis01.F IMAGE axis01.ps THRESHOLD 50)
add_magics_test(NAME axis02 SOURCES axis02.F IMAGE axis02.ps THRESHOLD 50)
add_magics_test(NAME automatic_axis SOURCES automatic_axis.F IMAGE automatic_axis.ps THRESHOLD 50)

