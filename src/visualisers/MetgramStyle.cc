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

/*! \file MetgramStyle.cc
    \brief Implementation of the Template class MetgramStyle.
    
    Magics Team - ECMWF 2006
    
    Started: Mon 16-Oct-2006
    
    Changes:
    
*/

#include "MetgramStyle.h"

#include "DateTime.h"
#include "Text.h"
#include "Polyline.h"
#include "Flag.h"

#include "LegendVisitor.h"
#include "Transformation.h"
#include <cfloat>

using namespace magics;

MetgramStyle::MetgramStyle() 
{
}


MetgramStyle::~MetgramStyle() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void MetgramStyle::print(ostream& out)  const
{
	out << "MetgramStyle[";
	out << "]";
}

void MetgramBar::print(ostream& out)  const
{
	out << "MetgramBar[]";
}

void MetgramCurve::print(ostream& out)  const
{
	out << "MetgramCurve[]";
}

void MetgramFlags::print(ostream& out)  const
{
	out << "MetgramFlags[]";
}

DateTime readDate(CustomisedPoint& point)
{
	MagDate date1((long)point["year"], (long)point["month"], (long)point["day"]);
	magics::MagTime time((long)point["hours"], (long)point["minutes"], (long)point["seconds"]);

	time -= Second(point["shift"] * 3600);
	return DateTime(date1, time); 
}

void MetgramBar::operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& visitor) 
{
	if ( points.empty() )
		return;
	vector<double> xpos;
	vector<double> limits;
	vector<double> ypos;
	
	Polyline* first  = new Polyline();
	first->setColour(Colour("blue"));
	first->setLineStyle(M_DASH);
	
	visitor.push_back(first);
	Polyline* last  = new Polyline();
	last->setColour(Colour("blue"));
	last->setLineStyle(M_DASH);

	visitor.push_back(last);

	MagDate date((long)(*points.front())["year"], (long)(*points.front())["month"], (long)(*points.front())["day"]);
	MagTime time((long)(*points.front())["hours"], (long)(*points.front())["minutes"], (long)(*points.front())["seconds"]);
	DateTime  base = readDate(*points.front());
	double left, right = 0.;

	const Transformation& transformation = visitor.transformation();
	double xmin =  transformation.getMinPCX();
	double xmax =  transformation.getMaxPCX();

	CustomisedPointsList::const_iterator previous = points.end();
    
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) 
	{
		
		DateTime step =  readDate(**point);
		DateTime next;
		double x = step - base;
		point++;
		if ( point != points.end()) 			
			next = readDate(**point);			
		else next = step;
		point--;
		  x -= (**point)["shift"];
		xpos.push_back(x);
	
		Polyline* box  = new Polyline();	
        box->setFilled(true);
		box->setShading(new FillShadingProperties());
		box->setFillColour(Colour("blue"));
		box->setColour(Colour("blue"));

		
	
		if  ( (*point)->find("as_rain") != (*point)->end() ) {
			left = ( previous != points.end() ) ? right : x;
			right = x;
		}
		else {
			left = ( previous != points.end() ) ? right : x - (next - step)/2;
			//left = ( previous != points.end() ) ? right : x; 
			right = ( next - step ) ? x + (next - step)/2 : x + ( x - right);
		}
		
		previous = point;

		if (left < xmin) left = xmin; 
		if (right > xmax) right = xmax; 
		
		if ( (*point)->find("curve1") != (*point)->end() )   {
			box->push_back(PaperPoint(left, 0));
			box->push_back(PaperPoint(left, (**point)["curve1"]));
			box->push_back(PaperPoint(right, (**point)["curve1"]));
			box->push_back(PaperPoint(right, 0));
			box->push_back(PaperPoint(left, 0));
			ypos.push_back((**point)["curve1"]);
			limits.push_back(x);
		}
		visitor.push_back(box);
	}
	
	double miny = *std::min_element(ypos.begin(), ypos.end());
	double maxy = *std::max_element(ypos.begin(), ypos.end());
	
	if ( !limits.empty() ) {
		first->push_back(PaperPoint(limits.front(), miny));
		first->push_back(PaperPoint(limits.front(), maxy));
	
		last->push_back(PaperPoint(limits.back(), miny));
		last->push_back(PaperPoint(limits.back(), maxy));
	}
}

void MetgramCurve::operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& visitor) 
{	
	if ( points.empty() )
		return;
	Polyline* curve1  = new Polyline();
	curve1->setColour(*colour_);
	curve1->setThickness(thickness_);
	//curve1->setLineStyle(line_style_);
	
	Polyline* curve2  = new Polyline();
	curve2->setColour(Colour("blue"));
	curve2->setThickness(2); 
	
	vector<double> xpos;
	vector<double> ypos;

	MagDate date((long)(*points.front())["year"], (long)(*points.front())["month"], (long)(*points.front())["day"]);
	MagTime time((long)(*points.front())["hours"], (long)(*points.front())["minutes"], (long)(*points.front())["seconds"]);
	DateTime  base = DateTime(date, time);	

	double x1 = 0.;
	double x2 = 0.;
	double y1 = DBL_MAX;
	double y2 = DBL_MIN;

	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		
		MagDate date((long)(**point)["year"], (long)(**point)["month"], (long)(**point)["day"]);
		MagTime time((long)(**point)["hours"], (long)(**point)["minutes"], (long)(**point)["seconds"]);

		DateTime orig = DateTime(date, time);		
		
		double x = orig - base;		
		xpos.push_back(x);		
	    x -= (**point)["shift"];
		
		if ( (*point)->find("curve1") != (*point)->end() )   {
			curve1->push_back(PaperPoint(x, (**point)["curve1"]));
			ypos.push_back((**point)["curve1"]);
			if ( (**point)["curve1"] < y1 ) {
				x1 = x;
				y1 = (**point)["curve1"];
			}
		}
		
		if ( (*point)->find("curve2") != (*point)->end() )   {
			curve2->push_back(PaperPoint(x, (**point)["curve2"]));
			ypos.push_back((**point)["curve2"]);
			if ( (**point)["curve2"] > y2 ) {
				x2 = x;
				y2 = (**point)["curve2"];
			}
		}
	}
	
	if ( !curve2->empty() ) visitor.push_back(curve2);
	if ( !curve1->empty() ) visitor.push_back(curve1);
	
	double minx = -6*3600;
	double maxx = *std::max_element(xpos.begin(), xpos.end()) + (6 *3600);
	double miny = *std::min_element(ypos.begin(), ypos.end());
	double maxy = *std::max_element(ypos.begin(), ypos.end());
	
	double height = maxy-miny;
	double width = maxx-minx;
	
	if ( points.front()->find("temperature") !=  points.front()->end() ) {
	
	    Text* text1 = new Text();
	    text1->addText("T2m", Colour("red"), 0.3);
	    text1->push_back(PaperPoint(x1, y1+(height*.50)));
	    visitor.push_back(text1);
	    
	    Polyline* arrow1 = new Polyline();
	    arrow1->setColour(Colour("red"));
	    arrow1->setThickness(2);
	    arrow1->push_back(PaperPoint(x1-(width*0.003), y1+(height*.15)));    
	    arrow1->push_back(PaperPoint(x1, y1+(height*.05)));
	    arrow1->push_back(PaperPoint(x1, y1+(height*.4)));
	    arrow1->push_back(PaperPoint(x1, y1+(height*.05))); 
	    arrow1->push_back(PaperPoint(x1+(width*0.003), y1+(height*.15)));
	    visitor.push_back(arrow1);
	    
	    Text* text2 = new Text();
	    text2->addText("T850", Colour("blue"), 0.3);
	    text2->push_back(PaperPoint(x2, y2-(height*.5)));
	    visitor.push_back(text2);
		
	    Polyline* arrow2 = new Polyline();
	    arrow2->setColour(Colour("blue"));
	    arrow2->setThickness(2);
	    arrow2->push_back(PaperPoint(x2-(width*0.005), y2-(height*.15)));    
	    arrow2->push_back(PaperPoint(x2, y2-(height*0.05)));
	    arrow2->push_back(PaperPoint(x2, y2-(height*.4)));
	    arrow2->push_back(PaperPoint(x2, y2-(height*.05)));
	    arrow2->push_back(PaperPoint(x2+(width*0.005), y2-(height*.15)));
	    visitor.push_back(arrow2);
	}
}

void MetgramFlags::operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& visitor)
{
	if ( points.empty() )
			return;
	MagDate date((long)(*points.front())["year"], (long)(*points.front())["month"], (long)(*points.front())["day"]);
	MagTime time((long)(*points.front())["hours"], (long)(*points.front())["minutes"], (long)(*points.front())["seconds"]);
	DateTime  base = DateTime(date, time);	
	
	Flag* flags = new Flag();
	flags->setColour(*colour_); 
	flags->setLength(length_);
   
	flags->setOriginMarker("magics_15");
	flags->setOriginHeight(0.05);
	flags->setConvention(KNOTS);
	int i = 0;
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		i++;
        if ( i % frequency_ ) 
            continue;
		MagDate date((long)(**point)["year"], (long)(**point)["month"], (long)(**point)["day"]);
		MagTime time((long)(**point)["hours"], (long)(**point)["minutes"], (long)(**point)["seconds"]);
		
		
		DateTime orig = DateTime(date, time);				
		double x = orig - base;		
		  x -= (**point)["shift"];
		
        
		if ( (*point)->find("curve1") != (*point)->end() && (*point)->find("curve2") != (*point)->end() )   {
			PaperPoint pos(x, 0);
			flags->push_back(ArrowPoint((**point)["curve1"], (**point)["curve2"], pos));
		}
	}
	if ( !flags->empty() ) visitor.push_back(flags);	
}

void MetgramCurve::visit(LegendVisitor& legend) 
{
	MagLog::dev() << "MetgramGraph::visit(LegendBase&) " << endl;
	Polyline* curve1  = new Polyline();
	curve1->setColour(Colour("red"));
	curve1->setThickness(2);
	
	Polyline* curve2  = new Polyline();
	curve2->setColour(Colour("blue"));
	curve2->setThickness(2);
	legend.add(new LineEntry("curve1", curve1));
	legend.add(new LineEntry("curve2", curve2));
}

