/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/


#include "GribLoopStep.h"
#include "GribDecoder.h"
#include "DateTime.h"
#include "ViewNode.h"
#include <locale>



void GribLoopStep::print(ostream&) const
{
	
}
/*
void DateGribLoopStep::operator()(GribDecoder&, LayerNode&)
{
	grib.open();
	long date = grib.getLong("date");    
    long hour = grib.getLong("hour");  
    long mn   = grib.getLong("minute"); 
    long step = grib.getLong("step"); 
       
    Date part1 = Date(date);
    Time part2 = Time(hour, mn, 0);
    DateTime base(part1, part2);
    base = base + (step *3600);
    
    
    DateTime valid(part1, part2);
    valid = valid + (step *3600);
    valid = valid + ( long(span_) * 3600);    
    ostringstream from, to;

    tm convert = base;
    locale loc("");
      
    from.imbue(loc);
    to.imbue(loc);
    
    const time_put<char>& tfac = use_facet<time_put<char> >(loc); 
    string format = "%Y-%m-%dT%H:%M:00Z";
    tfac.put(from, from, ' ', &convert, format.c_str(), format.c_str()+format.length());    
    
    convert = valid;
    
    tfac.put(to, to, ' ', &convert, format.c_str(), format.c_str()+format.length());
    layer.setName(from.str());
	layer.timestamp(from.str());
	layer.timespan(to.str());
}


void ParamGribLoopStep::operator()(GribDecoder&, LayerNode& layer)
{
	layer.timestamp("unknown");
	layer.timespan("unnkwon");
	
}

*/


	

