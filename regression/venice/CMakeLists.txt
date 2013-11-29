# magics test executable run in script

add_magics_test(NAME venice_test01 SOURCES test01.f IMAGE test01.ps THRESHOLD 50)
add_magics_test(NAME venice_test02 SOURCES test02.f RESOURCES input_test02 IMAGE test02.ps DATA TOWER.ope_2010 WAS_RES.2010 THRESHOLD 50)
add_magics_test(NAME venice_test03 SOURCES test03.f RESOURCES fort.11 fort.12 fort.13 fort.14 fort.15 input03 IMAGE test03.ps DATA TOWER.ope_2010 WAS_RES.2010 THRESHOLD 50)
add_magics_test(NAME venice_test05 SOURCES test05.f RESOURCES fort.51 IMAGE test05.ps THRESHOLD 50)
add_magics_test(NAME venice_test06 SOURCES test06.f RESOURCES fort.61 IMAGE test06.ps THRESHOLD 50)
add_magics_test(NAME venice_test08 SOURCES test08.f RESOURCES fort.82 fort.83 Logo_Ismar.png Logo_ICPSM.png IMAGE test08.ps THRESHOLD 50)

