import magics as ma
import numpy as np

ma.init()

ma.setr("subpage_y_position", 2.) 
ma.setc("subpage_map_projection",'cartesian')
ma.setc("subpage_x_axis_type",'date')
ma.setc("subpage_y_axis_type",'regular')
ma.setc("subpage_x_date_min","2012-03-01 12:00:00")
ma.setc("subpage_x_date_max","2012-03-03 12:00:00")
ma.setr("subpage_x_min",25.)
ma.setr("subpage_x_max",75.)
ma.setr("subpage_y_min",25.)
ma.setr("subpage_y_max",75.)

ma.setc("axis_orientation","vertical")
ma.setc("axis_type","regular")
ma.setc("axis_tick_label_colour",'navy')
ma.setc("axis_grid","on")
ma.setc("axis_grid_colour","grey")
ma.setc("axis_grid_line_style","dot")
ma.setr("axis_tick_label_height",0.4)
ma.seti("axis_grid_thickness",1)
ma.axis()

ma.setc("axis_orientation","horizontal")
ma.setc("axis_type","date")
ma.setc("axis_grid","on")
ma.setr("axis_days_label_height",0.40)
ma.setr("axis_months_label_height",0.40)
ma.setr("axis_years_label_height",0.50)
ma.setc("axis_grid_colour","grey")
ma.seti("axis_grid_thickness",1)
ma.setc("axis_grid_line_style","dot")
ma.axis()


x = ["2012-03-02 00:00:00","2012-03-02 12:00:00","2012-03-03 00:00:00"]
y = np.array([50.,30.,40.])
ma.set1c("input_date_x_values",x,3)
ma.set1r("input_y_values",y,3)
ma.minput()

ma.setc("graph_type","bar")
ma.setc("graph_bar_colour",'evergreen')
ma.setr("graph_bar_width",7200.)

ma.graph()

ma.set1c("text_lines",["Simple Bar","Second line"],2)
ma.setc("text_justification","left")
ma.setr("text_font_size",1.)
ma.setc("text_colour", "charcoal")

ma.text();

ma.finalize()
