/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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


	

