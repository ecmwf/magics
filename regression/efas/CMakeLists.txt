# magics test executable run in script


add_magics_test(NAME efas_graph SOURCES efas_graph.py IMAGE efas_graph.png THRESHOLD 5 PYTHON)
add_magics_test(NAME efas_boxplot_log SOURCES boxplot_log.py IMAGE boxplot_log.png RESOURCES boxplot.data THRESHOLD 5 PYTHON)
add_magics_test(NAME efas_boxplot_reg SOURCES boxplot_reg.py IMAGE boxplot_reg.png RESOURCES boxplot.data THRESHOLD 5 PYTHON)



