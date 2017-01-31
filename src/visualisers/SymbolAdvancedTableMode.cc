/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SymbolAdvancedTableMode.h
    \brief Implementation of the Template class SymbolAdvancedTableMode.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 21-Jan-2004
    
    Changes:
    
*/



#include "SymbolAdvancedTableMode.h"
#include "UserPoint.h"
#include "Symbol.h"
#include "SymbolPlotting.h"
#include "LegendVisitor.h"
#include "Histogram.h"
#include "HistoVisitor.h"
#include "CartesianTransformation.h"
using namespace magics;

map<string,  SymbolAdvancedTableMode::TextHandler> SymbolAdvancedTableMode::textHandlers_;

SymbolAdvancedTableMode::SymbolAdvancedTableMode() 
{
	/*
	if ( textHandlers_.empty() ) {
		textHandlers_["none"] = &SymbolAdvancedTableMode::none;
		textHandlers_["centre"] = &SymbolAdvancedTableMode::centre;
		textHandlers_["top"] = &SymbolAdvancedTableMode::top;
		textHandlers_["bottom"] = &SymbolAdvancedTableMode::bottom;
		textHandlers_["right"] = &SymbolAdvancedTableMode::right;
	}
	*/
}

SymbolAdvancedTableMode::~SymbolAdvancedTableMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void SymbolAdvancedTableMode::print(ostream& out)  const
{
    out << "SymbolAdvancedTableMode[";
    SymbolAdvancedTableModeAttributes::print(out);
    out << "]";
}



SymbolProperties SymbolAdvancedTableMode::operator()(double value) const 
{	
    return map_.find(value, SymbolProperties());   
}



bool SymbolAdvancedTableMode::accept(double value) const 
{ 
	return map_.accept(value);
}



void SymbolAdvancedTableMode::prepare()
{
  
    
    

} 

void SymbolAdvancedTableMode::adjust(double min, double max)
{
	static map<string, TextSymbol::TextPosition> texthandlers;
	if ( texthandlers.empty() ) {
		texthandlers["none"] = TextSymbol::M_NONE;
		texthandlers["left"] = TextSymbol::M_LEFT;
		texthandlers["top"] = TextSymbol::M_ABOVE;
		texthandlers["bottom"] = TextSymbol::M_BELOW;
		texthandlers["right"] = TextSymbol::M_RIGHT;
	}
	map_.clear();
	MagLog::dev() << "Data going from " << min << " to " << max << endl;
	levels_->set(*this);
	
	
	
	levels_->calculate(min, max, false);
	if (levels_->size() == 1) {
			levels_->push_back(levels_->front());
	}
	colourMethod_->set(*this);
	height_method_->set(*this);
	colourMethod_->prepare(*levels_, *levels_);
	height_method_->prepare(*levels_);
	
	if ( markers_.empty() ) {
		markers_.push_back(15);
	}
	if ( markers_names_.empty() ) {
			markers_names_.push_back("ww_01");
	}
	
	TextSymbol::TextPosition position;
	
	
	if ( text_list_.empty() ) {
		position = TextSymbol::M_NONE;
		text_list_.push_back("");
	}
	else {
		string type = lowerCase(text_display_type_);
		map<string, TextSymbol::TextPosition>::iterator pos = texthandlers.find(type);
		position = ( pos != texthandlers.end()) ? pos->second : TextSymbol::M_NONE;
	}
	
	MagFont font(text_font_name_);
	font.colour(*text_font_colour_);
	font.size(text_font_size_);
	font.style(text_font_style_);
	

	
    LevelSelection::const_iterator level = levels_->begin();
    vector<int>::const_iterator marker = markers_.begin();
    vector<string>::const_iterator marker_name = markers_names_.begin();
    const bool index = magCompare(parent_->marker_mode_, "index");
    SymbolProperties last;
    vector<string>::const_iterator text = text_list_.begin();
    while ( true) {
    	if (level+1 == levels_->end() ) break;

    	MagLog::debug() << "[" << *level << ", " << *(level+1) << "]=" << *marker << "(marker)" << *text << "(text)"<< endl;

    	SymbolProperties properties;
    	if ( index )
    		properties = SymbolProperties(colourMethod_->right(*level), height_method_->height(*level), *marker     , *text);
    	else
    		properties = SymbolProperties(colourMethod_->right(*level), height_method_->height(*level), *marker_name, *text);

    	properties.position_ = position;
    	properties.font_ = font;

    	properties.outline_ = parent_->outline_;
    	properties.outlineColour_ = *parent_->outline_colour_;
    	properties.outlineStyle_ = parent_->outline_style_;
    	properties.outlineThickness_ = parent_->outline_thickness_;

    	properties.connectLine_ = parent_->connect_;
    	properties.connectLineColour_ = (parent_->automatic_connect_colour_ ) ? colourMethod_->right(*level) : *parent_->connect_colour_;
    	properties.connectLineStyle_ = parent_->connect_style_;
    	properties.connectLineThickness_ = parent_->connect_thickness_;
    	properties.blanking_ = parent_->text_blanking_;
    	last = properties;
    	map_[Interval(*level, *(level+1)) ] = properties;
    	
    	if ( marker+1 == markers_.end() ) {
    		if ( marker_policy_ == M_CYCLE )
                marker =  markers_.begin(); 
    	}
    	else 
    		marker++;
    	if ( marker_name+1 == markers_names_.end() ) {
        		if ( marker_policy_ == M_CYCLE )
                    marker_name =  markers_names_.begin();
        	}
        else
        		marker_name++;
    	if ( text+1 == text_list_.end() ) {
    	    	if ( text_policy_ == M_CYCLE )
    	                text =  text_list_.begin(); 
    	    	}
    	    	else 
    	    		text++;
    	level++;
    }
    // Here we add a last interval to close to the right the lst Interval.

    MagLog::debug() << "[" << *level << ", " << (*level)+epsilon << "]=" <<colourMethod_->right(*level) << *marker << "(marker)" << *text << "(text)"<< endl;
    map_[Interval(*level, (*level)+epsilon) ] = last;

}

void build(const IntervalMap<SymbolProperties>& in, IntervalMap<Colour>& out)
{
	// Here we want to get the last interval ;



	for ( IntervalMap<SymbolProperties>::const_iterator interval = in.begin(); interval != in.end(); ++interval)
		out.insert(make_pair(Interval(interval->first.min_, interval->first.max_), interval->second.colour_));

}


void SymbolAdvancedTableMode::visit(Data& data, LegendVisitor& legend)
{
	legend.newLegend();
	IntervalMap<SymbolProperties>::const_iterator last = map_.end();
		    				--last;
	IntervalMap<SymbolProperties>::iterator first = map_.begin();

	    	switch (legend.legendType())  {
	    		case LegendMethod::CONTINUOUS:
	    		{

	    	    for ( IntervalMap<SymbolProperties>::const_iterator interval = map_.begin(); interval != last; ++interval) {
	    			Polyline* box = new Polyline();
	    	        
	    	        double min =interval->first.min_;
	    	        double max = interval->first.max_;
	    	        
	    	        box->setShading(new FillShadingProperties());
	    	        
	    	        box->setFillColour(interval->second.colour_);
	    	        box->setFilled(true);
	    	        
	    	        legend.add(new BoxEntry(min, max, box));
	    	    }
	    	        break;
	    		}
	    		case LegendMethod::DISJOINT:
	    		{
	    			for ( IntervalMap<SymbolProperties>::iterator interval = first; interval != last; ++interval) {
	    				if ( magCompare(interval->second.marker_, "none") )
	    					// ignore entry
	    					continue;
	    				Symbol* symbol = new Symbol();
	    				(*symbol).setColour(interval->second.colour_);
	    				(*symbol).setSymbol(interval->second.marker_);
	    				(*symbol).setHeight(interval->second.height_);

	    				string str=data.legendText(interval->first.min_,interval->first.max_);
	    				if(str.empty())
	    				{
	    					ostringstream text;
	    					text << interval->first.min_ << "-" <<  interval->first.max_;
	    					str=text.str();
	    				}
	    				legend.add(new SimpleSymbolEntry(interval->second.label_.empty() ? str : interval->second.label_, symbol));
	    			}
	    			break;
	    		}
	    		case LegendMethod::HISTOGRAM:
	    		{
	    			IntervalMap<Colour> beans;
	    			build(map_, beans);
	    			Histogram helper;
	    			// here we need the transformation.. to know wich points where displayed!
	    			IntervalMap<int>& histogram = helper.histogram(beans, data.points(legend.transformation(), false));
	    			double mean = helper.mean();

	    			int maxh = 0;
	    			for (IntervalMap<int>::const_iterator  interval = histogram.begin(); interval != histogram.end(); ++interval){
	    				if ( maxh < interval->second ) maxh = interval->second;
	    			}
	    			bool first = true;
	    			for ( IntervalMap<SymbolProperties>::const_iterator interval = map_.begin(); interval != last; ++interval) {
	    				   Polyline* box = new Polyline();

	    				   double min =interval->first.min_;
	    				   double max = interval->first.max_;

	    				   box->setShading(new FillShadingProperties());
	    				   box->setFillColour(interval->second.colour_);
	    				   box->setFilled(true);
	    				   BoxEntry* entry = new BoxEntry(min, max, box);
	    				   int count = histogram.find(min, -1);
	    				   // here we have to add the last interval!
	    				   if ( max == levels_->back() ) {

	    					   count +=  histogram.find(max, 0);
	    				   }
	    				   entry->population(count);
	    				   entry->totalPopulation(maxh);
	    				   if ( min <= mean && mean <= max ) {
	    					   entry->mean(mean);

	    				   }
	    				   if (first) {
	    					   entry->first();
	    					   first = false;
	    				   }


	    				   legend.add(entry);

	    			}
	    			break;

	    		}
	    	
	    }
	    legend.last();
}




void SymbolAdvancedTableMode::visit(Data& data, HistoVisitor& visitor)
{
	IntervalMap<Colour> beans;
	if ( !visitor.basic() ) // Why did we put this possibility???
		build(map_, beans);
	Histogram helper;
	helper.visit(beans, data, data.points(*visitor.dataLayoutTransformation(), false), visitor);
}


