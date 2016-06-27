# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

import Magics
import numpy

taxis_key='andate'
taxis_values=["2011-09-01 00:00", "2011-10-01 00:00", "2011-11-01 00:00",
"2011-12-01 00:00", "2012-01-01 00:00", "2012-02-01 00:00",
"2012-03-01 00:00", "2012-04-01 00:00"]

tseries_key='meanobsvalue'
tseries_values= numpy.array([4.0, 25.5, 6.8, 2., 12.4, 8.9, 3.2, 5.3])

Magics.init ()
Magics.set1c("output_format",['png','ps'])
Magics.setc("output_name","axis_python")
Magics.setr('subpage_y_position',2.0)
Magics.setc('subpage_map_projection','CARTESIAN')
Magics.setc('axis_orientation', 'HORIZONTAL')
Magics.setc('axis_type','DATE')
Magics.setc('axis_date_type','months')
Magics.setc('axis_date_min_value','2011-08-01 00:00')
Magics.setc('axis_date_max_value','2012-08-01 00:00')
Magics.setr('axis_tick_interval', 1.)
Magics.setc('axis_title_text',taxis_key)
Magics.axis()

Magics.setc('axis_orientation', 'VERTICAL')
Magics.setc('axis_type','regular')
Magics.setr('axis_min_value',0.0)
Magics.setr('axis_max_value',30.0)
Magics.setr('axis_tick_interval', 5.)
Magics.setc('axis_title_text',tseries_key)
Magics.axis()

Magics.set1c('graph_curve_date_x_values',taxis_values)
Magics.set1r('graph_curve_y_values',tseries_values)
Magics.graph()

Magics.setc('text_line_1','Example date axis with python')
Magics.text()

Magics.finalize()
