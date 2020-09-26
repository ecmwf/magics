
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file HistogramLegendMethodAttributes.h
    \\brief Definition of HistogramLegendMethod Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "HistogramLegendMethodWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


HistogramLegendMethodWrapper::HistogramLegendMethodWrapper(): histogramlegendmethod_(new HistogramLegendMethod())

{

	
	
	LegendMethodWrapper::object(histogramlegendmethod_);
	
	
} 
HistogramLegendMethodWrapper::HistogramLegendMethodWrapper(HistogramLegendMethod* histogramlegendmethod): histogramlegendmethod_(histogramlegendmethod)
{
	
	
	LegendMethodWrapper::object(histogramlegendmethod_);
	
} 

HistogramLegendMethodWrapper::~HistogramLegendMethodWrapper()
{
	
}

void HistogramLegendMethodWrapper::set(const MagRequest& request)
{
	
	
	
	LegendMethodWrapper::set(request);
	

	if  (request.countValues("LEGEND_LABEL_FREQUENCY") ) {
		int label_frequency_value = request("LEGEND_LABEL_FREQUENCY");
		histogramlegendmethod_->label_frequency_ = label_frequency_value;
		}
	if  (request.countValues("LEGEND_HISTOGRAM_BORDER") ) {
		string histo_border_value = request("LEGEND_HISTOGRAM_BORDER");
		histogramlegendmethod_->histo_border_ = MagTranslator<string, bool>()(histo_border_value);
		}
	if  (request.countValues("LEGEND_HISTOGRAM_MEAN_VALUE") ) {
		string histo_mean_value = request("LEGEND_HISTOGRAM_MEAN_VALUE");
		histogramlegendmethod_->histo_mean_ = MagTranslator<string, bool>()(histo_mean_value);
		}
	if  (request.countValues("LEGEND_HISTOGRAM_MEAN_VALUE_MARKER") ) {
		int histo_mean_marker_value = request("LEGEND_HISTOGRAM_MEAN_VALUE_MARKER");
		histogramlegendmethod_->histo_mean_marker_ = histo_mean_marker_value;
		}
	if  (request.countValues("LEGEND_HISTOGRAM_MEAN_VALUE_MARKER_SIZE") ) {
		double histo_mean_size_value = request("LEGEND_HISTOGRAM_MEAN_VALUE_MARKER_SIZE");
		histogramlegendmethod_->histo_mean_size_ = histo_mean_size_value;
		}
	if  (request.countValues("LEGEND_HISTOGRAM_MAX_VALUE") ) {
		string histo_max_value = request("LEGEND_HISTOGRAM_MAX_VALUE");
		histogramlegendmethod_->histo_max_ = MagTranslator<string, bool>()(histo_max_value);
		}
	if  (request.countValues("LEGEND_HISTOGRAM_GRID_THICKNESS") ) {
		int histo_grid_thickness_value = request("LEGEND_HISTOGRAM_GRID_THICKNESS");
		histogramlegendmethod_->histo_grid_thickness_ = histo_grid_thickness_value;
		}
	
	if  (request.countValues("LEGEND_HISTOGRAM_BORDER_COLOUR") ) {
		string histo_border_colour_value = request("LEGEND_HISTOGRAM_BORDER_COLOUR");
		histogramlegendmethod_->histo_border_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(histo_border_colour_value));
	}
		
	if  (request.countValues("LEGEND_HISTOGRAM_MEAN_VALUE_MARKER_COLOUR") ) {
		string histo_mean_colour_value = request("LEGEND_HISTOGRAM_MEAN_VALUE_MARKER_COLOUR");
		histogramlegendmethod_->histo_mean_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(histo_mean_colour_value));
	}
		
	if  (request.countValues("LEGEND_HISTOGRAM_GRID_COLOUR") ) {
		string histo_grid_colour_value = request("LEGEND_HISTOGRAM_GRID_COLOUR");
		histogramlegendmethod_->histo_grid_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(histo_grid_colour_value));
	}
		
	
	if  (request.countValues("LEGEND_HISTOGRAM_GRID_LINE_STYLE") ) {
		string histo_grid_style_value = request("LEGEND_HISTOGRAM_GRID_LINE_STYLE");
		histogramlegendmethod_->histo_grid_style_ = MagTranslator<string, LineStyle>()(histo_grid_style_value);
	}
		
	
}

void HistogramLegendMethodWrapper::print(ostream& out)  const
{
	out << "HistogramLegendMethodWrapper[]";
}


    






