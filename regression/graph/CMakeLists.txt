add_magics_test(NAME graph_bar SOURCES bar.F IMAGE bar.ps    THRESHOLD 2)
add_magics_test(NAME graph_boxplot_ex SOURCES boxplot_ex.F IMAGE boxplot_ex.ps     THRESHOLD 2)
add_magics_test(NAME graph_graf01 SOURCES graf01.F IMAGE graf01.ps     THRESHOLD 2)
add_magics_test(NAME graph_graf02 SOURCES graf02.F IMAGE graf02.ps    THRESHOLD 2)
add_magics_test(NAME graph_graf03 SOURCES graf03.F IMAGE graf03.ps     THRESHOLD 2)
add_magics_test(NAME graph_graf04 SOURCES graf04.F IMAGE graf04.ps     THRESHOLD 2)
add_magics_test(NAME graph_tephi SOURCES tephi.py IMAGE tephi.ps  THRESHOLD 5 PYTHON)
add_magics_test(NAME graph_lines SOURCES graph_lines.F IMAGE graph_lines.ps  THRESHOLD 2)
#add_magics_test(NAME graph_venice_01 SOURCES graph_venice_01.f IMAGE graph_venice_01.ps 
#            DATA TOWER.ope_2010 WAS_RES.2010 THRESHOLD 2)
add_magics_test(NAME graph_venice_02 SOURCES graph_venice_02.F IMAGE graph_venice_02.ps  
            DATA ANCON.ope_2010 MONOP.ope_2010 PESCA.ope_2010 POMAE.ope_2010 graph_venice_02_stdin.txt THRESHOLD 2)
