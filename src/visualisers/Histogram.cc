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

/*! \file Histogram.cc
    \brief Implementation of the Template class Histogram.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 18-May-2004
    
    Changes:
    
*/



#include "Histogram.h"
#include "Layout.h"
#include "PointsHandler.h"
#include "IntervalMap.h"
#include "CartesianTransformation.h"

using namespace magics;



Histogram::Histogram()
{
}


Histogram::~Histogram()
{
}

/*!
 Class information are given to the output-stream.
*/		

void Histogram::print(ostream& out)  const
{
	out << "Histogram[";
	HistogramAttributes::print(out);
	out << "]";
}


void Histogram::bean(PointsHandler& points)
{
	points.setToFirst();
	mean_ = 0;
	population_ = 0;
	while (points.more()) {
		try {
			double val = points.current().value();						
			IntervalMap<int>::iterator count = counter_.get(val);
			mean_ += val;
			population_++;
			count->second++;
		}
		catch (...)
		{
			//MagLog::debug() <<  points.current().value() << " is not in range..." << endl;
		}

		points.advance();
	}
	mean_ /= population_;

}



void Histogram::prepare(PointsHandler& points)
{
		levels_->set(*this);
		levels_->calculate(points.min(), points.max(), false);
	
		
		double delta=0;
		LevelSelection::const_iterator min = levels_->begin();
		LevelSelection::const_iterator max = levels_->begin();
		++max;
		while ( max != levels_->end())
		{
			if(levels_->end()-max != 1 || !same(*min,*max))
			{			  
		  		counter_.insert(make_pair(Interval(*min, *max), 0));
			}
			
			//If it is the last interval and the min and max are the same 
			//we just increase max by the size of the previous interval to
			//have a proper bin.
			else
			{
			  	counter_.insert(make_pair(Interval(*min, (*max)+delta), 0));
			}
			
			delta=(*max)-(*min);
			
			min++;
			max++;			  	
			
		}
		bean(points);
}


static double xmin_;
static double xmax_;
static double ymin_;
static double ymax_;
//static double width_;
static double height_;


IntervalMap<int>& Histogram::histogram(const IntervalMap<Colour>& beans, PointsHandler& points)
{

	for ( IntervalMap<Colour>::const_iterator entry = beans.begin(); entry != beans.end(); ++entry) {
		counter_.insert(make_pair(Interval(entry->first.min_, entry->first.max_), 0));
	}
	bean(points);
	return counter_;
}


void Histogram::visit(const IntervalMap<Colour>& beans, Data& data, PointsHandler& points, HistoVisitor& visitor)
{
	MagLog::dev() << *this << endl;
	CartesianTransformation* cartesian = new CartesianTransformation();

	ostringstream mins;
	ostringstream maxs;
	ostringstream vals;
	ostringstream cols;

	if ( beans.empty() ) {
		prepare(points);
	}

	else {
		for ( IntervalMap<Colour>::const_iterator entry = beans.begin(); entry != beans.end(); ++entry) {
				counter_.insert(make_pair(Interval(entry->first.min_, entry->first.max_), 0));
		}
		bean(points);
	}

	if ( points.empty() || counter_.empty() )
		return;
		 vector<int> y;
				   vector<double> x;
				   for ( IntervalMap<int>::const_iterator count = counter_.begin(); count != counter_.end(); ++count) {
					   y.push_back(count->second );
					   x.push_back(count->first.min_ )	;
					   x.push_back(count->first.max_ )	;
					   MagLog::dev() << "[" << count->first.min_ << "-" <<  count->first.max_ << "]-->" <<  count->second << endl;
				   }

				   MagLog::dev()<< "minx=" << *std::min_element(x.begin(), x.end()) << endl;
				   MagLog::dev()<<  "maxx=" << *std::max_element(x.begin(), x.end()) << endl;
				   xmin_ = *std::min_element(x.begin(), x.end());
				   ymin_ = 0;
				   xmax_ = *std::max_element(x.begin(), x.end());
				   ymax_ = *std::max_element(y.begin(), y.end());

				   if ( xmin_ == xmax_ ) xmax_ = xmin_ +1;
				   if ( ymin_ == ymax_ ) ymax_ = ymin_ +1;


				   double step = (ymax_ - ymin_)/5;

				   	double log = log10(step);
				   	double ws = pow(10., int(log));
				   	double inc = ceil(step/ws)*ws;
				   	vector<double> ticks;
				   	for (double x = ymin_; x <= ymax_ +inc; x+=inc)
				   		ticks.push_back(x);
				   	ymax_ = ticks.back();
				   	cartesian->setAutomaticX(true);
				   	cartesian->setAutomaticY(true);

				   cartesian->setMinX(xmin_-width_*0.1);
				   cartesian->setMaxX(xmax_+width_*0.1);
				   cartesian->setMinY(-height_*0.1);
				   MagLog::dev()<<  "maxy=" << *std::max_element(y.begin(), y.end()) << endl;
				   cartesian->setMaxY(ymax_+height_*0.1);
				   visitor.transformation(cartesian);
	Polyline* frame = new Polyline();
	frame->setColour(Colour("navy"));
	frame->setThickness(1);
	frame->setLineStyle(M_SOLID);
	frame->push_back(PaperPoint(xmin_, ymin_));
	frame->push_back(PaperPoint(xmax_, ymin_));
	frame->push_back(PaperPoint(xmax_, ymax_));
	frame->push_back(PaperPoint(xmin_, ymax_));
	frame->push_back(PaperPoint(xmin_, ymin_));


	double left = xmin_ - (width_*0.05);
	double bottom = ymin_ - height_ *0.1;

	double font_size =0.12;
	for (vector<double>::iterator tick = ticks.begin(); tick!=ticks.end(); ++tick){
		Text* val = new Text();
		val->addText(*tick, Colour("navy"), font_size);
		val->push_back(PaperPoint(left, *tick));
		val->setVerticalAlign(MHALF);
		val->setJustification(MRIGHT);
		visitor.push_back(val);
		Polyline* grid = new Polyline();
			grid->setColour(Colour("grey"));
			grid->setThickness(1);
			grid->setLineStyle(M_DOT);
			grid->push_back(PaperPoint(xmin_, *tick));
			grid->push_back(PaperPoint(xmax_, *tick));
			visitor.push_back(grid);
		grid = new Polyline();
			grid->setColour(Colour("navy"));
			grid->setThickness(1);
			grid->setLineStyle(M_SOLID);
			grid->push_back(PaperPoint(xmin_-(width_*0.025), *tick));
			grid->push_back(PaperPoint(xmin_, *tick));
			visitor.push_back(grid);
	}


	double last=0.;
	string sep;
	Polyline* bar = 0;
	int barCnt=0;
	int barLabelFreq=1+(counter_.size()/10) - ((counter_.size()%10 == 0)?1:0);
	
	for ( IntervalMap<int>::const_iterator count = counter_.begin(); count != counter_.end(); ++count, barCnt++) {
		Polyline* grid = new Polyline();
					grid->setColour(Colour("grey"));
					grid->setThickness(1);
					grid->setLineStyle(M_DOT);
					grid->push_back(PaperPoint(count->first.min_, ymin_));
					grid->push_back(PaperPoint(count->first.min_, ymax_));
					visitor.push_back(grid);
		Colour colour = beans.find(count->first.min_, *colour_);
		if (bar)
			visitor.push_back(bar);

		bar = new Polyline();
		bar->setColour(Colour("navy"));
		bar->setThickness(1);
		bar->setLineStyle(M_SOLID);
		
		bar->setFilled(true);
	    bar->setFillColour(colour);

	    mins << sep << count->first.min_;
	    maxs << sep << count->first.max_;
	    vals << sep << count->second;
	    cols << sep << colour.red() << ":" << colour.green() << ":"  << colour.blue(); 	
	    
	    sep = "/";

		FillShadingProperties* shading = new FillShadingProperties();
          
		bar->setShading(shading);
		bar->push_back(PaperPoint(count->first.min_, 0));
		bar->push_back(PaperPoint(count->first.min_, count->second));
		bar->push_back(PaperPoint(count->first.max_, count->second));
		bar->push_back(PaperPoint(count->first.max_, 0));
		bar->push_back(PaperPoint(count->first.min_, 0));

		
		if(barCnt % barLabelFreq ==0)
		{
			Text* from = new Text();
			from->addText(count->first.min_, Colour("navy"), font_size);
			from->setVerticalAlign(MTOP);
			from->setJustification(MLEFT);
			from->setAngle(45.);
			from->push_back(PaperPoint(count->first.min_, bottom));
		
			/*Text* to = new Text();
			to->addText(count->first.min_, Colour("navy"), font_size);
			to->setVerticalAlign(MTOP);
			to->push_back(PaperPoint(count->first.max_, bottom));*/

			visitor.push_back(from);
		}
		//visitor.push_back(to);
		/*grid = new Polyline();
		grid->setColour(Colour("navy"));
		grid->setThickness(1);
		grid->setLineStyle(M_DASH);
		grid->push_back(PaperPoint(xmin_, count->second));
		grid->push_back(PaperPoint(count->first.min_, count->second));
		visitor.push_back(grid);*/

		Polyline* vert = new Polyline();
			vert->setColour(Colour("navy"));
			vert->setThickness(1);
			vert->setLineStyle(M_SOLID);
			vert->push_back(PaperPoint(count->first.min_, ymin_));
			vert->push_back(PaperPoint(count->first.min_, ymin_ - (height_*0.025)));
			visitor.push_back(vert);

		last = count->first.max_;
	}

	Polyline* vert = new Polyline();
				vert->setColour(Colour("navy"));
				vert->setThickness(1);
				vert->setLineStyle(M_SOLID);
				vert->push_back(PaperPoint(last, ymin_));
				vert->push_back(PaperPoint(last, ymin_ - (height_*0.025)));
				visitor.push_back(vert);

	if (bar)
			visitor.push_back(bar);
	data.setInfo("histogram_min", mins.str());
	data.setInfo("histogram_max", maxs.str());
	data.setInfo("histogram_val", vals.str());
	data.setInfo("histogram_col", cols.str());

	visitor.push_back(frame);
}


void Histogram::visit(LegendVisitor& legend)
{
}
