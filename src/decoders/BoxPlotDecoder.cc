/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotDecoder.h
    \brief Implementation of the Template class BoxPlotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 29-Sep-2005
    
    Changes:

*/



#include "BoxPlotDecoder.h"

using namespace magics;

BoxPlotDecoder::BoxPlotDecoder() 
{
}


BoxPlotDecoder::~BoxPlotDecoder() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void BoxPlotDecoder::print(ostream& out)  const
{
	out << "BoxPlotDecoder[";
	BoxPlotDecoderAttributes::print(out);
	out << "]";
}

void BoxPlotDecoder::customisedPoints(const std::set<string>& , CustomisedPointsList& out) 
{
	vector<int> sizes;

	doublearray::const_iterator x = x_.begin();
	sizes.push_back(x_.size());	
	doublearray::const_iterator min = min_.begin();
	sizes.push_back(min_.size());
	doublearray::const_iterator max = max_.begin();
	sizes.push_back(max_.size());
	doublearray::const_iterator lower = lower_.begin();
	sizes.push_back(lower_.size());
	doublearray::const_iterator upper = upper_.begin();
	sizes.push_back(upper_.size());
	doublearray::const_iterator median = median_.begin();
	sizes.push_back(median_.size());
	

	vector<int>::const_iterator size = std::min_element(sizes.begin(), sizes.end());

	for (int i = 0; i != *size; i++)
	{
		CustomisedPoint* point = new CustomisedPoint();
		point->longitude(*x);	
	    (*point)["x"] = *x++;
		(*point)["min"] = *min++;
		(*point)["max"] = *max++;
		(*point)["lower"] = *lower++;
		(*point)["upper"] = *upper++;
		(*point)["median"] = *median++;
	  
		out.push_back(point);
	}
}

void BoxPlotDecoder::getReady(const Transformation& transformation)
{
	try {
		for ( vector<string>::const_iterator x = date_x_.begin(); x != date_x_.end(); ++x )
			x_.push_back(transformation.x(*x));
		}
	catch(...)
	{}
}
