/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file EpsGraph.cc
    \brief Implementation of the Template class EpsGraph.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/



#include "EpsGraph.h"
#include "PointsHandler.h"
#include "DateTime.h"
#include "Text.h"
#include "LegendVisitor.h"

//#include "InteractiveSet.h"
#include <cfloat>
#include <locale>

using namespace magics;

string writeDate(DateTime& date, const string& format)
{
	std::locale loc("");

	ostringstream visitor;
	visitor.imbue(loc);

	const std::time_put<char>& facet = std::use_facet<std::time_put<char> >(loc); 

	tm convert = date;
	facet.put(visitor, visitor, ' ', &convert, format.c_str(), format.c_str()+format.length());    

	return visitor.str();
}

void tick(double min, double max, vector<double>& ticks)
{
	float inc;
	int nb = 7;
	float step;
	float log, ws;

	double wmax = std::max(min, max);
	double wmin = std::min(min, max);
	
  //cvisitor << "min=" << min << " max=" << max << endl;
	
	while (nb < 20)
	{
		step = (wmax-wmin)/nb;
		log = log10(step);
		ws = pow(10., int(log));
		inc = ceil(step/ws)*ws;
		//MagLog::dev() << "Automatic method ---> increment = " << inc << " ---> try base=" << inc/ws << endl;
			
		if ( wmax-wmin != inc && (inc/ws == 1 || inc/ws == 2 || inc/ws == 5 || inc/ws == 10) ) {
			//MagLog::dev() << "Automatic method ---> increment " << inc << " OK! " << endl;
			break;
		}
		nb++;	
	}
	
	float first = floor(min/inc) *inc; 
	double val = first > min ? first-inc : first;
        bool last = true;
        while (  last )
	{ 
		if (val >= max ) last = false;
		ticks.push_back(val);

		//cvisitor << "val->" << val << endl;
		 val+=inc;
	}
}
class EmptyEntry : public LegendEntry
{
public:
	EmptyEntry() : LegendEntry(" ") {}
    void set(const PaperPoint&, BasicGraphicsObjectContainer&) {}
};

class EpsEntry : public LegendEntry
{
public:
	EpsEntry() : LegendEntry(" ") {}
	void colour(const Colour& colour) { colour_ = colour; }
	void borderColour(const Colour& colour) { border_colour_ = colour; }
	void font(const MagFont& font) { font_ = font; }
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{

		double x = point.x();
		double y = point.y() - 0.125 ;

	
		Polyline* box  = new Polyline();
		box->setColour(border_colour_);
		box->setFilled(true);
		box->setShading(new FillShadingProperties());
		box->setFillColour(colour_);
		
        

		double width = 0.15;
		double height = 1.5; 
		double top = y+height;
		double bottom = y-height;
		box->push_back(PaperPoint(x-width, y));
		box->push_back(PaperPoint(x-width, top));
		box->push_back(PaperPoint(x+width, top));
		box->push_back(PaperPoint(x+width, y));
		box->push_back(PaperPoint(x-width, y));
		box->push_back(PaperPoint(x-width, bottom));
		box->push_back(PaperPoint(x+width, bottom));
		box->push_back(PaperPoint(x+width, y));
		box->push_back(PaperPoint(x-width, y));
		visitor.push_back(box);
		Polyline* up  = new Polyline();
		up->setColour(border_colour_);
		(*up).push_back(PaperPoint(x, top+height));
		(*up).push_back(PaperPoint(x, top));
		visitor.push_back(up);
		Polyline* down  = new Polyline();
		down->setColour(border_colour_);
		(*down).push_back(PaperPoint(x, bottom));
		(*down).push_back(PaperPoint(x, bottom-height));
		visitor.push_back(down);
		
		// Now the text...
		Text* max  = new Text();
		max->setText("max");
		max->setFont(font_);		
		max->setJustification(MLEFT);
		(*max).push_back(PaperPoint(x + 1*.2, bottom-height));
		visitor.push_back(max);
		
		Text* min  = new Text();
		min->setText("min");
		min->setFont(font_);
		min->setJustification(MLEFT);		
		(*min).push_back(PaperPoint(x + 1*.2, top+height));
		visitor.push_back(min);
        
       
		Text* seventyfive  = new Text();
		seventyfive->setText("75%");
		seventyfive->setFont(font_);
		seventyfive->setJustification(MLEFT);		
		seventyfive->push_back(PaperPoint(x + 1*0.3, bottom));
		visitor.push_back(seventyfive);
		
		Text* fifty  = new Text();
		fifty->setText("median");
		fifty->setFont(font_);
		fifty->setJustification(MLEFT);
		(*fifty).push_back(PaperPoint(x + 1*0.3, y));
		visitor.push_back(fifty);
		
		Text* twentyfive  = new Text();
		twentyfive->setText("25%");
		twentyfive->setFont(font_);
		twentyfive->setJustification(MLEFT);
		twentyfive->push_back(PaperPoint(x + 1*0.3, top));
		visitor.push_back(twentyfive);
	}
	
protected:
	Polyline*  box_;
	Colour colour_;
	Colour border_colour_;
	MagFont font_;
};


class FullEpsEntry : public EpsEntry
{
public:
	FullEpsEntry() {}

	virtual void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
		

		double x = point.x();
		double y = point.y() ;

		MagLog::dev() << "FulleEps Entry->  [" << x << ", " << y << "]" << endl;
	
		Polyline* box  = new Polyline();
		box->setColour(border_colour_);
		box->setFilled(true);
		box->setFillColour(colour_);
		box->setShading(new FillShadingProperties());

		double width = 1*0.2;
		double height = 1*0.2; 
		double top1 = y+height;
		double bottom1 = y-height;
		double top2 = y+height+height;
		double bottom2 = y-height-height;
		box->push_back(PaperPoint(x-width, y));
		box->push_back(PaperPoint(x-width, top1));
		box->push_back(PaperPoint(x+(width/2), top1));
		box->push_back(PaperPoint(x+(width/2), top2));
		box->push_back(PaperPoint(x-(width/2), top2));
		box->push_back(PaperPoint(x-(width/2), top1));
		box->push_back(PaperPoint(x+width, top1));
		box->push_back(PaperPoint(x+width, y));
		box->push_back(PaperPoint(x-width, y));
		box->push_back(PaperPoint(x-width, bottom1));
		box->push_back(PaperPoint(x+(width/2), bottom1));
		box->push_back(PaperPoint(x+(width/2), bottom2));
		box->push_back(PaperPoint(x-(width/2), bottom2));
		box->push_back(PaperPoint(x-(width/2), bottom1));
		box->push_back(PaperPoint(x+width, bottom1));
		box->push_back(PaperPoint(x+width, y));
		box->push_back(PaperPoint(x-width, y));
		visitor.push_back(box);
		Polyline* up  = new Polyline();
		up->setColour(border_colour_);
		(*up).push_back(PaperPoint(x, top2+height));
		(*up).push_back(PaperPoint(x, top2));
		visitor.push_back(up);
		Polyline* down  = new Polyline();
		down->setColour(border_colour_);
		(*down).push_back(PaperPoint(x, bottom2));
		(*down).push_back(PaperPoint(x, bottom2-height));
		visitor.push_back(down);
		
		// Now the text...
		Text* max  = new Text();
		max->setText("max");
		max->setFont(font_);
		max->setJustification(MLEFT);
		max->push_back(PaperPoint(x + 0.5, bottom2-height));

		visitor.push_back(max);
		
		Text* min  = new Text();
		min->setText("min");
		min->setFont(font_);
		min->setJustification(MLEFT);
		min->push_back(PaperPoint(x + 1*0.5, top2+height));
		visitor.push_back(min);
        
		Text* ninety  = new Text();
		ninety->setText("90%");
		ninety->setFont(font_);
		ninety->setJustification(MLEFT);
		ninety->push_back(PaperPoint(x + 0.5, bottom2));
		visitor.push_back(ninety);
		
		Text* ten  = new Text();
		ten->setText("10%");
		ten->setFont(font_);
		ten->setJustification(MLEFT);
		ten->push_back(PaperPoint(x + 0.5, top2));
		visitor.push_back(ten);
		
		Text* seventyfive  = new Text();
		seventyfive->setText("75%");
		seventyfive->setFont(font_);
		seventyfive->setJustification(MLEFT);
		seventyfive->push_back(PaperPoint(x + 0.75, bottom1));
		visitor.push_back(seventyfive);
		
		Text* fifty  = new Text();
		fifty->setText("median");
		fifty->setFont(font_);
		fifty->setJustification(MLEFT);
		(*fifty).push_back(PaperPoint(x + 0.75, y));
		visitor.push_back(fifty);
		
		Text* twentyfive  = new Text();
		twentyfive->setText("25%");
		twentyfive->setFont(font_);
		twentyfive->setJustification(MLEFT);
		twentyfive->push_back(PaperPoint(x + 0.75, top1));

		visitor.push_back(twentyfive);
	}
};

class WindRoseEntry : public EpsEntry
{
public:
	WindRoseEntry(const Colour& colour) { colour_ = colour; }

	virtual void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
		

		double x = point.x()-0.5;
		double y = point.y() ;

		MagLog::dev() << "FulleEps Entry->  [" << x << ", " << y << "]" << endl;
	
		
	MagFont font("sansserif", "normal", 0.25);
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	
	float start = x;
	
	for ( int i = 0; i != 100; i ++ ) { 
	    
		Hsl hsl = colour_.hsl();    
        float light = hsl.light_;        
        hsl.light_ += (0.99 - light)*((100-i)/100.);
    	Colour colour(hsl);
		
		Polyline* box  = new Polyline();
		box->setColour(colour);
		box->setFilled(true);
		box->setFillColour(colour);
		box->setShading(new FillShadingProperties());
		
		double width = 0.0225;
		double height = 1*0.25; 
		box->push_back(PaperPoint(start, y));
		box->push_back(PaperPoint(start, y+height));
		box->push_back(PaperPoint(start+width, y+height));
		box->push_back(PaperPoint(start+width, y));
		box->push_back(PaperPoint(start, y));		
		visitor.push_back(box);
		
		start+=width;
		
	}
		
		Polyline* box  = new Polyline();
		box->setColour(border_colour_);
		box->setFilled(false);
		
		
		double height = 1*0.25; 
		box->push_back(PaperPoint(x, y));
		box->push_back(PaperPoint(x, y+height));
		box->push_back(PaperPoint(start, y+height));
		box->push_back(PaperPoint(start, y));
		box->push_back(PaperPoint(x, y));		
		visitor.push_back(box);
		
		// Now the text...
		Text* text  = new Text();
		text->setText("0%");
		text->setFont(font_);
		text->setJustification(MLEFT);
		text->push_back(PaperPoint(x, y-0.2));
		visitor.push_back(text);
		text  = new Text();
		text->setText("25%");
		text->setFont(font_);
		text->setJustification(MLEFT);
		text->push_back(PaperPoint(x+0.5, y-0.2));
		visitor.push_back(text);	
		text  = new Text();
		text->setText("50%");
		text->setFont(font_);
		text->setJustification(MLEFT);
		text->push_back(PaperPoint(x+1, y-0.2));
		visitor.push_back(text);	
		text  = new Text();
		text->setText("75%");
		text->setFont(font_);
		text->setJustification(MLEFT);
		text->push_back(PaperPoint(x+1.5, y-0.2));
		visitor.push_back(text);
		text  = new Text();
		text->setText("100%");
		text->setFont(font_);
		text->setJustification(MLEFT);
		text->push_back(PaperPoint(x+2, y-0.2));
		visitor.push_back(text);		
		
		
		
		
	}
	string text_;
	
};


class WaveRoseEntry : public EpsEntry
{
public:
	WaveRoseEntry(vector<Colour>& colour) { colours_ = colour; }

	virtual void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{

		double x = point.x();
		double y = point.y() ;

		MagLog::dev() << "FulleEps Entry->  [" << x << ", " << y << "]" << endl;
	
		
	MagFont font("sansserif", "normal", 0.25);
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	
	float start = x;
	
	
	for ( vector<Colour>::const_iterator colour = colours_.begin(); colour != colours_.end(); ++colour) { 
		
		Polyline* box  = new Polyline();
		box->setColour(*colour);
		box->setFilled(true);
		box->setShading(new FillShadingProperties());
		box->setFillColour(*colour);
		double width = 0.4;
		double height = 1*0.25; 
		box->push_back(PaperPoint(start, y));
		box->push_back(PaperPoint(start, y+height));
		box->push_back(PaperPoint(start+width, y+height));
		box->push_back(PaperPoint(start+width, y));
		box->push_back(PaperPoint(start, y));		
		visitor.push_back(box);
		
		start+=width;
		
	}
		
		Polyline* box  = new Polyline();
		box->setColour(border_colour_);
		box->setFilled(false);
		
		double height = 1*0.25; 
		box->push_back(PaperPoint(x, y));
		box->push_back(PaperPoint(x, y+height));
		box->push_back(PaperPoint(start, y+height));
		box->push_back(PaperPoint(start, y));
		box->push_back(PaperPoint(x, y));		
		visitor.push_back(box);
		
		// Now the text...
		Text* text  = new Text();
		text->setText("1");
		text->setFont(font_);
		text->setJustification(MCENTRE);
		text->push_back(PaperPoint(x+0.35, y-0.2));
		visitor.push_back(text);
		
		text  = new Text();
		text->setText("2.5");
		text->setFont(font_);
		text->setJustification(MCENTRE);
		text->push_back(PaperPoint(x+0.8, y-0.2));
		visitor.push_back(text);	

		text  = new Text();
		text->setText("4");
		text->setFont(font_);
		text->setJustification(MCENTRE);
		text->push_back(PaperPoint(x+1.2, y-0.2));
		visitor.push_back(text);	
		
		text  = new Text();
		text->setText("6");
		text->setFont(font_);
		text->setJustification(MCENTRE);
		text->push_back(PaperPoint(x+1.6, y-0.2));
		visitor.push_back(text);
		
		text  = new Text();
		text->setText("9 m");
		text->setFont(font_);
		text->setJustification(MLEFT);
		text->push_back(PaperPoint(x+2.0, y-0.2));
		visitor.push_back(text);		
		
		
		
		
	}
	string text_;
	vector<Colour> colours_;
	
};


class EpsControl : public LegendEntry
{
public:
	EpsControl(const string& model, double resolution, const string& type, double height) :
			LegendEntry(" "), legend_size_(height)
	{
		ostringstream title;
		// carefull here this text is depending of the resolution! 
		MagLog::dev() << "EpsControl=>resolution" << resolution << endl; 
        int km = maground(40000/(2*(resolution+1)+2));  
		title << model << "(" << tostring(km) + " km)";
		title_ = title.str();
	}
	EpsControl(const string& title, double height) : LegendEntry(" "), legend_size_(height)
	{
		
		title_ = title;
	}
	void font_size(double height) { legend_size_ = height; }	
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
		

		double x = point.x();
		double y = point.y() - 0.125;
		

		MagLog::dev() << "Legend at Point[" << point.x() << ", " << point.y() << "]" << endl;
	
		Text* text  = new Text();
		text->addText(title_, Colour("red"),  legend_size_); // should be customisable
		text->setJustification(MLEFT);
		(*text).push_back(PaperPoint(x + 1*0.04, y));
		visitor.push_back(text);
	}
	
protected:
	string title_;
	double legend_size_;
};


class EpsForecast : public LegendEntry
{
public:
	EpsForecast(const string& model, double resolution, const string& type, double height) :
		LegendEntry(" "), legend_size_(height)
	{
		MagLog::dev() << "EpsForecsat=>resolution" << resolution << endl; 
		ostringstream title;
        int km = maground(40000/(4*(resolution+1)));
		title <<  model << " (" + tostring(km) + " km)";
		title_ = title.str();
	}
	
	EpsForecast(const string& title, double height) : 
		LegendEntry(" "), legend_size_(height)

		{
			
			title_ = title;
		}	
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
	
		

		double x = point.x();
		double y = point.y()- 0.125;

		
		Text* text  = new Text();
		text->addText(title_, Colour("blue"),  legend_size_); // should be customisable
		text->setJustification(MLEFT);
		(*text).push_back(PaperPoint(x + 1*0.04, y));
		visitor.push_back(text);
	}
	
protected:
	string title_;
	double legend_size_;
};


class EpsCalval1 : public LegendEntry
{
public:
	EpsCalval1(double) : LegendEntry(" ")
	{
		ostringstream title;
		//title << "T" << (2*resolution)-1 << " OPS";
        title << "CAL/VAL 1";
		title_ = title.str();
	}
		
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
		
	
		
	
		double x = point.x();
		double y = point.y()- 0.125;
		Polyline* line  = new Polyline();
		line->setColour(Colour("green"));
		
		line->setLineStyle(M_SOLID);
		line->setThickness(2);
		line->push_back(PaperPoint(x-1*0.025, y));
		line->push_back(PaperPoint(x+1*0.025, y));
		visitor.push_back(line);
		
		Text* text  = new Text();
		text->setText(title_);
		text->setJustification(MLEFT);
		(*text).push_back(PaperPoint(x + 1*0.04, y));
		visitor.push_back(text);
	}
	
protected:
	string title_;
};

class EpsCalval2 : public LegendEntry
{
public:
	EpsCalval2(double, double height) : LegendEntry(" "), legend_size_(height)
	{
		ostringstream title;
		//title << "T" << (2*resolution)-1 << " OPS";
        title << "CAL/VAL 2";
		title_ = title.str();
	}
		
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
		
		
		
	
		double x = point.x();
		double y = point.y()- 0.125;
		Polyline* line  = new Polyline();
		line->setColour(Colour("magenta"));
		
		
		line->setThickness(2);
		line->push_back(PaperPoint(x-1*0.025, y));
		line->push_back(PaperPoint(x+1*0.025, y));
		visitor.push_back(line);
		
		Text* text  = new Text();
		text->addText(title_, Colour("blue"), legend_size_ ); // should be customisable
		text->setJustification(MLEFT);
		(*text).push_back(PaperPoint(x + 1*0.04, y));
		visitor.push_back(text);
	}
	
protected:
	string title_;
	double legend_size_;
};


EpsGraph::EpsGraph() : forecast_(false), control_(false), eps_(true)
{
}


EpsGraph::~EpsGraph() 
{}

/*!
 Class information are given to the visitorput-stream.
*/		
void EpsGraph::print(ostream& visitor)  const
{
	visitor << "EpsGraph[";
	visitor << "]";
}


Polyline* EpsGraph::newControl() {

	Polyline* control  = new Polyline();
	control->setColour(*control_colour_);
	control->setLineStyle(control_style_);
	control->setThickness(control_thickness_);
	return control;
}

Polyline* EpsGraph::newForecast() {

	
	Polyline* forecast  = new Polyline();
	forecast->setColour(*deterministic_colour_);
	forecast->setThickness(deterministic_thickness_);
	forecast->setLineStyle(deterministic_style_);
	return forecast;
}

void EpsGraph::pushControl(Polyline* control, BasicGraphicsObjectContainer& visitor)
{
	const Transformation& transformation = visitor.transformation();
	if ( !control->empty() && whisker_) {		
		transformation(*control, visitor);
		control_ = true;
	}
	else 
		control_ = false;
	
}

void EpsGraph::pushForecast(Polyline* forecast, BasicGraphicsObjectContainer& visitor)
{
	const Transformation& transformation = visitor.transformation();
	if ( !forecast->empty() && deterministic_) {					
		transformation(*forecast, visitor);
		forecast_ = true;
	}
	else
		forecast_ = false;
}

void EpsGraph::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{
	
	CustomisedPointsList points; 
	std::set<string> request;

	const Transformation& transformation = visitor.transformation();
	
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	
	if (points.empty()) return;

	
	
	
	Polyline* control = newControl();
	Polyline* forecast = newForecast();


	resolution_ = (*points.front())["resolution"];
	DateTime base = points.front()->base();
	
    vector<BasicGraphicsObject*> list, list2;
    
    if ( points.size() < 2 ) return;
    fullEps_ = false;
    
    Colour colour = *colour_;
    
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		resolution_ = (**point)["resolution"];
		double missing = (**point)["missing"];
		
		double x = (**point)["step"] + box_shift_ *3600;
		double width = (box_width_ == -1) ? (**point)["width"] : box_width_ * 3600;
		
		if ( (**point)["right"] ) colour = *right_colour_;
		if ( (**point)["left"] )  colour = *left_colour_;
		
		double max = ((*point)->find("max") != (*point)->end()) ? (*point)->find("max")->second : (*point)->find("maximum")->second;
		double min = ((*point)->find("min") != (*point)->end()) ? (*point)->find("min")->second : (*point)->find("minimum")->second;

		CustomisedPoint::const_iterator ten   = (*point)->find("ten");
        CustomisedPoint::const_iterator ninty = (*point)->find("ninety");

        if ( (*point)->find("control") != (*point)->end() &&  (**point)["control"]!= missing  )
        	control->push_back(PaperPoint(x, (**point)["control"]));		
        else {
        	pushControl(control, visitor);
        	control = newControl();
        }
		if ( (*point)->find("hres") != (*point)->end() &&  (**point)["hres"] != missing)
            forecast->push_back(PaperPoint(x, (**point)["hres"]));
        else {
        	pushForecast(forecast, visitor);
        	forecast = newForecast();
        }
        if ( (*point)->find("median") == (*point)->end() )  {        	
            eps_ = false;  
            continue;
        }

        if (  (**point)["median"] == missing ) {        	
                   eps_ = false;  
                   continue;
               }
     
	vector<double> eps;
	eps.push_back( (**point)["min"]);
	eps.push_back( (**point)["ten"]);
	eps.push_back( (**point)["twenty_five"]);
	eps.push_back( (**point)["median"]);
	eps.push_back( (**point)["seventy_five"]);
	eps.push_back( (**point)["ninety"]);
	eps.push_back( (**point)["max"]);

	std::sort(eps.begin(), eps.end());

	for (vector<double>::iterator e = eps.begin(); e != eps.end(); ++e) {
		if ( same(*e, 0) ) *e = 0;    
    }
   

    double epsmin, eps10, eps25,  eps50, eps75, eps90, epsmax;
    if ( ninty != (*point)->end() ) {
        	epsmin = eps[0];
        	eps10 = eps[1];
       		eps25 = eps[2];
        	eps50 = eps[3];
        	eps75 = eps[4];
        	eps90 = eps[5];
        	epsmax = eps[6];
	}
	else {
        	epsmin = eps[0];
       		eps25 = eps[1];
        	eps50 = eps[2];
        	eps75 = eps[3];
        	epsmax = eps[4];
	}
	// Prepare the colors :
	int size = quantiles_colour_.size();
	Colour colour10_25 = ( size >= 1 ) ? Colour(quantiles_colour_[0]) : colour;
	Colour colour25_75 = ( size >= 2 ) ? Colour(quantiles_colour_[1]) : colour;
	Colour colour75_90 = ( size >= 3 ) ? Colour(quantiles_colour_[2]) : colour10_25;

		Polyline* box10_25  = new Polyline();
		box10_25->setColour(*border_colour_);
		box10_25->setThickness(border_thickness_);
		box10_25->setFilled(true);
		box10_25->setFillColour(colour10_25);
		box10_25->setShading(new FillShadingProperties());
        
        Polyline* box25_75  = new Polyline();
		box25_75->setColour(*border_colour_);
		box25_75->setThickness(border_thickness_);
		box25_75->setFilled(true);
		box25_75->setFillColour(colour25_75);
		box25_75->setShading(new FillShadingProperties());
		
		Polyline* box75_90  = new Polyline();
		box75_90->setColour(*border_colour_);
		box75_90->setThickness(border_thickness_);
		box75_90->setFilled(true);
		box75_90->setFillColour(colour75_90);
		box75_90->setShading(new FillShadingProperties());


        Polyline* median  = new Polyline();
        median->setColour(*median_colour_);
        
        Polyline* bar1  = new Polyline();
        bar1->setColour(*border_colour_);
        bar1->setThickness(border_thickness_);

        Polyline* bar2  = new Polyline();
        bar2->setColour(*border_colour_);
        bar2->setThickness(border_thickness_);

        box25_75->push_back(PaperPoint(x-width, eps50));
		box25_75->push_back(PaperPoint(x-width, eps75));
        
        if ( ninty != (*point)->end() ) {
        	fullEps_ = true;
        	if ( eps75 != eps90 ) {
            box75_90->push_back(PaperPoint(x-(width/2), eps75));
            box75_90->push_back(PaperPoint(x-(width/2), eps90) );
            box75_90->push_back(PaperPoint(x+(width/2), eps90));
            box75_90->push_back(PaperPoint(x+(width/2), eps75));
        	}
            
        }
        if ( eps75 != eps25 ) {
		box25_75->push_back(PaperPoint(x+width, eps75));
		box25_75->push_back(PaperPoint(x+width, eps25));
        }
        if ( ten != (*point)->end() ) {
        	if ( eps25 != eps10 ) {

            box10_25->push_back(PaperPoint(x+(width/2), eps25));
            box10_25->push_back(PaperPoint(x+(width/2),eps10));
            box10_25->push_back(PaperPoint(x-(width/2), eps10));
            box10_25->push_back(PaperPoint(x-(width/2), eps25));
        	}
        }
        if ( eps25 != eps50 ) {
        box25_75->push_back(PaperPoint(x-width, eps25));
        box25_75->push_back(PaperPoint(x-width, eps50));
        }
    	bar1->push_back(PaperPoint(x+width, eps25));
    	bar1->push_back(PaperPoint(x-width, eps25));

    	bar2->push_back(PaperPoint(x+width, eps75));
    	bar2->push_back(PaperPoint(x-width, eps75));

    	median->push_back(PaperPoint(x+width, eps50));
		median->push_back(PaperPoint(x-width, eps50));
		
		
		Polyline* top  = new Polyline();
		top->setColour(*border_colour_);
		top->setThickness(border_thickness_);
		
	     
		(*top).push_back(PaperPoint(x, epsmax > transformation.getMaxY() ? transformation.getMaxY() : epsmax));
		
		if ( ninty != (*point)->end() ) 
            (*top).push_back(PaperPoint(x, eps90));
        else 
            (*top).push_back(PaperPoint(x, eps75));
		Polyline* bottom  = new Polyline();
		bottom->setColour(*border_colour_);
		bottom->setThickness(border_thickness_);
		(*bottom).push_back(PaperPoint(x, epsmin));
        if ( ten != (*point)->end() ) 
            (*bottom).push_back(PaperPoint(x, eps10));
		else 
           (*bottom).push_back(PaperPoint(x, eps25));
		
		if (whisker_) {
            transformation(*box10_25, visitor); 
            transformation(*box25_75, visitor); 
            transformation(*box75_90, visitor);
            transformation(*top, visitor);
            transformation(*bottom, visitor);
            transformation(*median, visitor);
            transformation(*bar1, visitor);
            transformation(*bar2, visitor);
		}
		
		// find the max! 
		vector<double> ypos;
		 for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		   		double max = ((*point)->find("max") != (*point)->end()) ? (*point)->find("max")->second : (*point)->find("maximum")->second;
				double min = ((*point)->find("min") != (*point)->end()) ? (*point)->find("min")->second : (*point)->find("minimum")->second;
		        if (max != missing ) ypos.push_back(max);
				if (min != missing ) ypos.push_back(min);
				if ( (*point)->find("control") != (*point)->end() )
					if ((**point)["control"] != missing) ypos.push_back((**point)["control"]);
			    if ( (*point)->find("hres") != (*point)->end() )
				    if ((**point)["hres"] != missing ) ypos.push_back((**point)["hres"]);
		    
		    }
		
		    double maxy = *std::max_element(ypos.begin(), ypos.end());
		    maxy = ( max_ > INT_MIN ) ? max_ : maxy;
		    double maxlabel = DBL_MIN;
		   		        if (max > maxy) 
		   		        		maxlabel = max;
		   		        if (min > maxy) 
		   		        		maxlabel = min; 
       
       
       if ( max > transformation.getMaxY() ) {
       	 Text* label = new Text();
		 MagFont font(max_font_name_, max_font_style_, max_font_size_);
		 font.colour(*max_font_colour_);		
		 label->setText(tostring(maground(max)));
		 label->setFont(font);
		 label->push_back(PaperPoint(x, transformation.getMaxY()*1.05));
		 visitor.push_back(label);
       }

		 
       }
	
	pushControl(control, visitor);
	pushForecast(forecast, visitor);
	
}

void EpsLight::print(ostream& visitor)  const
{
	visitor << "EpsLight[";
	visitor << "]";
}



void EpsLight::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{

/*
	#6AE9FF,#B0EBF5,#B4DAE0,#A3BBBF,#889B9E,#6AE9FF,#B0EBF5,#B4DAE0
	Hsl hsl = colour.hsl();
    
    float light = hsl.light_;
   
    hsl.light_ += (0.99 - light)*((point["total"]- point[direction.first])/point["total"]);
*/
   	
   	vector<Colour> colours;

   	colours.push_back(Colour("#6AE9FF"));
   
   	colours.push_back(Colour("#B0EBF5"));

	colours.push_back(Colour("#B4DAE0"));
	
	colours.push_back(Colour("#A3BBBF"));
	
	colours.push_back(Colour("#889B9E"));
	
	
   	CustomisedPointsList points; 
	std::set<string> request;

	const Transformation& transformation = visitor.transformation();
	
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	
	if (points.empty()) return;
	
    
    
    if ( points.size() < 2 ) return;
    
    
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
	
		
		double x = (**point)["step"];
		double width = (3600*3);
		
		double max = ((*point)->find("max") != (*point)->end()) ? (*point)->find("max")->second : (*point)->find("maximum")->second;
		double min = ((*point)->find("min") != (*point)->end()) ? (*point)->find("min")->second : (*point)->find("minimum")->second;


		CustomisedPoint::const_iterator ten   = (*point)->find("ten");
        CustomisedPoint::const_iterator ninty = (*point)->find("ninety");

	      
		vector<double> eps;
		eps.push_back( (**point)["min"]);
		eps.push_back( (**point)["ten"]);
		eps.push_back( (**point)["twenty_five"]);
		eps.push_back( (**point)["median"]);
		eps.push_back( (**point)["seventy_five"]);
		eps.push_back( (**point)["ninety"]);
		eps.push_back( (**point)["max"]);

		std::sort(eps.begin(), eps.end());

		for (vector<double>::iterator e = eps.begin(); e != eps.end(); ++e) {
			if ( same(*e, 0) )
				*e = 0;
	        
	    }
	   
        double epsmin, eps10, eps25,  eps50, eps75, eps90, epsmax;
        if ( ninty != (*point)->end() ) {
        	epsmin = eps[0];
        	eps10 = eps[1];
       		eps25 = eps[2];
        	eps50 = eps[3];
        	eps75 = eps[4];
        	eps90 = eps[5];
        	epsmax = eps[6];
		}
		else {
	        	epsmin = eps[0];
	       		eps25 = eps[1];
	        	eps50 = eps[2];
	        	eps75 = eps[3];
	        	epsmax = eps[4];
		}

		if ( epsmax == 0 )
			continue;

		map<double, float> lights;

		lights[epsmin] = 0;
		lights[eps10] = 0.20;
		lights[eps25] = 0.50;
		

		float y = 1.;
		float height = 1./(colours.size()*4);

		for ( vector<Colour>::iterator colour = colours.begin(); colour != colours.end(); ++colour) {
			
			for (int i = 0; i < 4; i++ ) {
				Polyline* box  = new Polyline();
				box->setColour(*colour);
				box->setFilled(true);
				box->setFillColour(*colour);
/*
				Hsl hsl = colour->hsl();
	    		float light = hsl.light_;
	    		hsl.light_ += (0.99 - light)*((point["total"]- point[direction.first])/point["total"]);
*/				
				box->setShading(new FillShadingProperties());
				box->push_back(PaperPoint(x+width, y));
				box->push_back(PaperPoint(x-width, y));

				box->push_back(PaperPoint(x-width, y-height));
				box->push_back(PaperPoint(x+width, y-height));
				box->push_back(PaperPoint(x+width, y));
				y -= height;
				transformation(*box, visitor); 
			}
		} 
	}

}

void EpsLight::visit(LegendVisitor& legend)
{

}

void EpsGraph::visit(LegendVisitor& legend)
{
	
    if ( !legend_ ) return;
	EpsEntry* entry = fullEps_ ? new FullEpsEntry() : new EpsEntry();
	
	if ( grey_legend_ ) {
		entry->colour(Colour("grey"));
		entry->borderColour(Colour("charcoal"));
	}
	else {
		entry->colour(*colour_);
		entry->borderColour(*border_colour_);
	}
    MagFont font(font_);
    font.style(font_style_);
	font.size(legend_size_);
	font.colour(*font_colour_);
	if ( grey_legend_ ) {
		font.colour( Colour("charcoal"));
	}

	entry->font(font);
	if ( whisker_ && eps_ )	
        legend.add(entry);
	
     
    if ( control_  )   {
    	MagLog::dev() << "LEGEND-> " << legend_control_text_ << endl;
    	if (legend_control_text_.empty() )

    		legend.add(new EpsControl(control_legend_, resolution_, legend_resolution_, legend_size_));
    	else 

    		legend.add(new EpsControl(legend_control_text_, legend_size_));
    }
    if ( forecast_  ) {
    	if (legend_forecast_text_.empty() )
    	    legend.add(new EpsForecast(deterministic_legend_, resolution_, legend_resolution_, legend_size_));
    	else 
    	    legend.add(new EpsForecast(legend_forecast_text_, legend_size_));
    }
    	
   
}



void triangle2(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos, double max)
{

	
	double shift = 3.14*0.125;
	
	Polyline* poly = new Polyline();

	poly->setThickness(2);
	
	double c = 1 - point[direction.first]/200;
	
	ostringstream colour;
	colour << "Rgb(" << c << ", " << c << ", " << c << ")" << endl; 
	
	poly->setFillColour(colour.str());	
	poly->setColour(Colour("Rgb(0.5, 0.5, 0.5)"));
	
	double length = 9*3600;
	double x0 = length;
//	double y0 = 0;
//	double a = length *tan(shift);
//	double b = length *tan(-shift);
	
	double x = x0 * cos(direction.second);
	double y = x0 * sin(direction.second);
	double x1 = x0 * cos(direction.second - shift);
	double y1 = x0 * sin(direction.second - shift);
	double x2 = x0 * cos(direction.second + shift);
	double y2 = x0 * sin(direction.second + shift);
	
	double xs = 0;
	double ys = 0;
	
	if ( point[direction.first] == max) {
		xs = 3*3600 * cos(direction.second);
		ys = 3*3600 * sin(direction.second);
	}
	
	poly->push_back(PaperPoint(pos + xs , ys ));	
	poly->push_back(PaperPoint(pos + xs + x1, ys + y1));
	poly->push_back(PaperPoint(pos + xs + x, ys + y));
	poly->push_back(PaperPoint(pos + xs + x2, ys + y2));
	poly->push_back(PaperPoint(pos + xs, ys));	
	
	poly->setFilled(true);
	poly->setShading(new FillShadingProperties());
	
	visitor.push_back(poly);

    if ( int(point[direction.first]/2) < 5 ) return;
    Text* text = new Text();
	MagFont font("sansserif", "normal", 0.25);
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	text->setFont(font);
	text->setText( tostring(int(point[direction.first]/2)) );
		
		text->push_back(PaperPoint(pos + 11*3600 * cos(direction.second), 11.5*3600 * sin(direction.second)));
		visitor.push_back(text);
		
    
}

void triangle3(const Colour& colour, const Colour& border, const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos, double max)
{
    
    Hsl hsl = colour.hsl();
    
    float light = hsl.light_;
   
    hsl.light_ += (0.99 - light)*((point["total"]- point[direction.first])/point["total"]);
     
	
	double shift = 3.14*0.125;
	
	Polyline* poly = new Polyline();
	poly->setThickness(1);
	
	
	poly->setFillColour(Colour(hsl));	
	poly->setColour(border);
	
	double length =  (point[direction.first]*(12*3600)/max);
	double x0 = length;
//	double y0 = 0;
//	double a = length *tan(shift);
//	double b = length *tan(-shift);
	
	double x = x0 * cos(direction.second);
	double y = x0 * sin(direction.second);
	double x1 = x0 * cos(direction.second - shift);
	double y1 = x0 * sin(direction.second - shift);
	double x2 = x0 * cos(direction.second + shift);
	double y2 = x0 * sin(direction.second + shift);
	
	double xs = 0;
	double ys = 0;
	
	
	poly->push_back(PaperPoint(pos + xs , ys ));	
	poly->push_back(PaperPoint(pos + xs + x1, ys + y1));
	poly->push_back(PaperPoint(pos + xs + x, ys + y));
	poly->push_back(PaperPoint(pos + xs + x2, ys + y2));
	poly->push_back(PaperPoint(pos + xs, ys));	
	
	poly->setFilled(true);
	poly->setShading(new FillShadingProperties());
	
	visitor.push_back(poly);

    if ( int(point[direction.first]/2) < 5 ) return;
    Text* text = new Text();
	MagFont font("sansserif", "normal", 0.25);
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	text->setFont(font);
	text->setText( tostring(int(point[direction.first]/2)) );
		
		text->push_back(PaperPoint(pos + 11*3600 * cos(direction.second), 11.5*3600 * sin(direction.second)));
		visitor.push_back(text);
		
    
}

void EpsCloud::triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos)
{
    
	 Hsl hsl = colour_->hsl();
    
    float light = hsl.light_;
   
    hsl.light_ += (0.99 - light)*((100- point[direction.first])/100);
     
	
	double shift = 3.14*0.125;
	
	Polyline* poly = new Polyline();
	poly->setStroke(true);
	
	
	poly->setFillColour(Colour(hsl));	
	poly->setColour(Colour(hsl));
	
	double x0= 12*3600;
	
	
	double x = x0 * cos(direction.second);
	double y = x0 * sin(direction.second);
	double x1 = x0 * cos(direction.second - shift);
	double y1 = x0 * sin(direction.second - shift);
	double x2 = x0 * cos(direction.second + shift);
	double y2 = x0 * sin(direction.second + shift);
	
	double xs = 0;
	double ys = 0;
	
	
	poly->push_back(PaperPoint(pos + xs , ys ));	
	poly->push_back(PaperPoint(pos + xs + x1, ys + y1));
	poly->push_back(PaperPoint(pos + xs + x, ys + y));
	poly->push_back(PaperPoint(pos + xs + x2, ys + y2));
	poly->push_back(PaperPoint(pos + xs, ys));	
	
	poly->setFilled(true);
	poly->setShading(new FillShadingProperties());
	
	visitor.push_back(poly);

	
    
}




void triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos, double)
{

	if ( !point[direction.first] ) return; 
	double shift = 3.14*0.125;
	
	//cvisitor << "Triangle---->" << direction.first << "=" << point[direction.first] << " " << scale << endl;
	Polyline* poly = new Polyline();

	poly->setThickness(2);
	poly->setFillColour(Colour("Rgb(0.7, 0.7, 0.7)"));	
		poly->setColour(Colour("Rgb(0.5, 0.5, 0.5)"));
	
	double length = point[direction.first];
	
	if ( length > 100) 
		length = ((3 * length /100 ) + 6 )*3600;
	else 
		if ( length >  50 )
			length = ((3 *length/50) + 3 ) *3600;
		else
			length = (6*length/50) *3600;
	
		

	double x0 = length;

	
	double x = x0 * cos(direction.second);
	double y = x0 * sin(direction.second);
	double x1 = x0 * cos(direction.second - shift);
	double y1 = x0 * sin(direction.second - shift);
	double x2 = x0 * cos(direction.second + shift);
	double y2 = x0 * sin(direction.second + shift);
	
	poly->push_back(PaperPoint(pos, 0));	
	poly->push_back(PaperPoint(pos + x1, y1));
	poly->push_back(PaperPoint(pos + x, y));
	poly->push_back(PaperPoint(pos + x2, y2));
	poly->push_back(PaperPoint(pos, 0));	
	
	poly->setFilled(true);
	poly->setShading(new FillShadingProperties());
	
	Polyline* median = new Polyline();
	median->setColour(Colour("black"));
	median->setThickness(1);
	median->push_back(PaperPoint(pos, 0));	
	median->push_back(PaperPoint(pos+x, y));	
	
	
	visitor.push_back(poly);
	
    
}


void EpsWind::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{ 
	CustomisedPointsList points; 
	std::set<string> request;

	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	if (points.empty()) return;
	

	

	
	
	DateTime base = points.front()->base();

	
    
	
	
	map<string, float> directions;
	if ( magCompare(convention_, "oceanographic" ) ) {
		directions["east"] = 0 + 3.14;
		directions["north"] = 3.14 * 0.5 +3.14;
		directions["north_east"] = 3.14*0.25  +3.14;
		directions["north_west"] = 3.14*0.75 +3.14;
		directions["south"] = 3.14*1.5 +3.14;
		directions["south_east"] = 3.14*1.75 +3.14;
		directions["south_west"] = 3.14*1.25 +3.14;
		directions["west"] = 3.14 +3.14;
	}
	else {
		directions["east"] = 0;
		directions["north"] = 3.14 * 0.5;
		directions["north_east"] = 3.14*0.25;
		directions["north_west"] = 3.14*0.75;
		directions["south"] = 3.14*1.5;
		directions["south_east"] = 3.14*1.75;
		directions["south_west"] = 3.14*1.25;
		directions["west"] = 3.14;
	}
	
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {		
		double total = 0;
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction) {
			
			if ( (*point)->find(direction->first) == (*point)->end() ) {
				vector<string> classification;
				classification.push_back("one");
				classification.push_back("two");
				classification.push_back("three");
				classification.push_back("four");
				classification.push_back("five");
				classification.push_back("six");
				double val = 0;
				for ( vector<string>::const_iterator key = classification.begin(); key != classification.end(); ++key) {
					CustomisedPoint::const_iterator value = (*point)->find(direction->first + "_" + *key);
					if ( value != (*point)->end() ) {

						val += ( value->second > 9998.) ? 0 :  value->second;
						cout << value->second << " ---> " << val << endl;
					}
				}			
				(**point)[direction->first] = val;
				
				
			}
			total += (**point)[direction->first];
		}
		(**point)["total"] = total;
		
		
		//InteractiveSet* iset = new InteractiveSet();
		//iset->addAction("onmouseover", new InteractiveMagnify(4));
		vector<double> values;
		double scale = 0;	
	    
		
    
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction)
			values.push_back((**point)[direction->first]);
			
		double ms = *std::max_element(values.begin(), values.end());
		double x = (**point)["step"] + (**point)["shift"];
	
		
			Polyline* grid = new Polyline();
			grid->setColour(Colour("grey"));
			grid->setThickness(2);
			grid->setLineStyle(M_DOT);		
			scale = 200;
			double l100 = 12*3600;
			for (float angle = 0; angle <= 2; angle+=0.1) 	
				grid->push_back(PaperPoint(x +(l100 * cos(3.14*angle)) , l100 * sin(3.14*angle)));					
			//iset->push_back(grid);
			
		
		


		visitor.push_back(grid);
		
		
		
	
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction)
				triangle(*direction, **point, visitor, x, ms);
		
	}
	

}

void EpsWave::visit(LegendVisitor& legend)
{
	MagFont font(legend.font_, legend.font_style_, tonumber(legend.font_dimension_));
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	vector<Colour> colours;
	colours.push_back(Colour("greenish_blue"));
	colours.push_back(Colour("yellow_green"));
	colours.push_back(Colour("greenish_yellow"));
	colours.push_back(Colour("orangish_yellow"));
	colours.push_back(Colour("yellowish_orange"));
	colours.push_back(Colour("reddish_orange"));
	
	WaveRoseEntry* wave = new WaveRoseEntry(colours);
	wave->borderColour(Colour("grey"));		
	wave->font(font);
		
	legend.add(wave);
}

void EpsWind::visit(LegendVisitor& legend)
{
	if ( !legend_ ) return;
	MagFont font(legend.font_, legend.font_style_, tonumber(legend.font_dimension_));
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	
       WindRoseEntry* wind = new WindRoseEntry(*colour_);
		
		
		
		
		wind->borderColour(*border_colour_);		
		wind->font(font);
		
		legend.add(wind);
    
}
void EpsCloud::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{ 
	CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	map<string, float> directions;
	
				directions["one"] = 3.14 * 3/8;
				directions["2"] = 3.14 * 1/8;
				directions["3"] =  -3.14*1/8;
				directions["4"] = -3.14* 3/8;
				directions["5"] = -3.14* 5/8;
				directions["6"] = -3.14* 7/8.;
				directions["7"] = -3.14 *9/8;
				directions["8"] =-3.14 *11/8; ;
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {	

		(**point)["total"] = 100;
		double x = (**point)["step"] + (**point)["shift"];
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction)
				triangle(*direction, **point, visitor, x);
	
	Polyline* grid = new Polyline();
				grid->setColour(Colour("grey"));
				grid->setThickness(2);
				grid->setLineStyle(M_DOT);		
			
				double l100 = 12*3600;
				for (float angle = 0; angle <= 2; angle+=0.1) 	
					grid->push_back(PaperPoint(x +(l100 * cos(3.14*angle)) , l100 * sin(3.14*angle)));					
			
					visitor.push_back(grid);
	}		
			


		
}

void EpsBar::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{
	CustomisedPointsList points;
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	map<string, float> directions;

				directions["1"] = 1;
				directions["2"] = 2;
				directions["3"] = 3;
				directions["4"] = 4;
				directions["5"] = 5;
				directions["6"] = 6;
				directions["7"] = 7;
				directions["8"] = 8;
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {

		(**point)["total"] = 100;
		double x = (**point)["step"] + (**point)["shift"];
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction) {
			 Hsl hsl = colour_->hsl();
			 float light = hsl.light_;
			 hsl.light_ += (0.99 - light)*((100- (**point)[direction->first])/100);
			 Polyline* poly = new Polyline();
			 poly->setThickness(1);
			 poly->setFillColour(Colour(hsl));
			 poly->setColour(Colour(hsl));
			 x = x*3600;
			 double width = 3 * 3600;
			 poly->push_back(PaperPoint(x-width, direction->second-1));
			 poly->push_back(PaperPoint(x+width, direction->second-1));
			 poly->push_back(PaperPoint(x+width, direction->second));
			 poly->push_back(PaperPoint(x-width, direction->second));
			 poly->push_back(PaperPoint(x-width, direction->second-1));
			 visitor.push_back(poly);

		}
	}





}

void EpsBar::visit(LegendVisitor& legend)
{

	MagLog::dev() << " EpsBar::visit(LegendVisitor&) --> to be implemented! " << endl;
}

void EpsCloud::visit(LegendVisitor& legend)
{
	
	MagFont font("sansserif", "normal", 0.25);
	font.colour(Colour("Rgb(0.2, 0.2, 0.2)"));
	
       WindRoseEntry* wind = new WindRoseEntry(*colour_);
		
		
		
		
		wind->borderColour(*border_colour_);		
		wind->font(font);
		
		legend.add(wind);
    
}
void EpsWind::print(ostream&) const
{
}
void EpsCloud::print(ostream&) const
{
}
void EpsBar::print(ostream&) const
{
}
void EpsWave::print(ostream&) const
{
}

void triangle5(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos, double max)
{
	
	vector<Colour> colours;
	colours.push_back(Colour("greenish_blue"));
	colours.push_back(Colour("yellow_green"));
	colours.push_back(Colour("greenish_yellow"));
	colours.push_back(Colour("orangish_yellow"));
	colours.push_back(Colour("yellowish_orange"));
	colours.push_back(Colour("reddish_orange"));
	
	vector<string> forces;
	forces.push_back(direction.first + "_one");
	forces.push_back(direction.first + "_two");
	forces.push_back(direction.first + "_three");
	forces.push_back(direction.first + "_four");
	forces.push_back(direction.first + "_five");
	forces.push_back(direction.first + "_six");
	double total = 0;
	Polyline* poly;
	vector<Colour>::iterator colour = colours.begin();
	Colour border("grey");
	
	double r = 12*3600;
	double factor = r*r/max;
	
	double previous = 0;
	double shift = 3.14*0.125;
	
	for (vector<string>::const_iterator force = forces.begin(); force != forces.end(); ++ force) {
		double count =  point[*force];
		if ( !count ) {
			colour++; 
			continue;
		}
		
	    poly = new Polyline();
	    poly->setThickness(1);
		poly->setFillColour(*colour);	
	    poly->setColour(border);
		
		total += count;	    
		double length =  sqrt(total*factor);
		double x0 = length;
		
	
		double x1 = x0 * cos(direction.second);
		double y1 = x0 * sin(direction.second);
		double x = x0 * cos(direction.second - shift);
		double y = x0 * sin(direction.second - shift);
		double x2 = x0 * cos(direction.second - (2*shift));
		double y2 = x0 * sin(direction.second - (2*shift));
	   
	    double px1 = previous * cos(direction.second);
		double py1 = previous * sin(direction.second);
		double px = previous * cos(direction.second - shift);
		double py = previous * sin(direction.second - shift);
		double px2 = previous * cos(direction.second - (2*shift));
		double py2 = previous * sin(direction.second - (2*shift));
		
		previous = x0;
        colour++;
	
	    
	
		poly->push_back(PaperPoint(pos + px1 , py1 ));	
		poly->push_back(PaperPoint(pos + x1,  y1));
		poly->push_back(PaperPoint(pos + x,  y));
		poly->push_back(PaperPoint(pos +  x2,  y2));
		poly->push_back(PaperPoint(pos + px2, py2));
		poly->push_back(PaperPoint(pos + px, py));	
		poly->push_back(PaperPoint(pos + px1, py1));	
	
		poly->setFilled(true);
		poly->setShading(new FillShadingProperties());
	
		visitor.push_back(poly);
	}
	
	

        
	
}

void EpsWave::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{ 
	CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	if (points.empty()) return;
	
	vector<double> xpos;

	DateTime base = points.front()->base();

	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		xpos.push_back((**point)["last"]);		  
    }
	
	map<string, float> directions;
	
	directions["east"] = 0 + 3.14;
	directions["north"] = 3.14 * 0.5 +3.14;
	directions["north_east"] = 3.14*0.25  +3.14;
	directions["north_west"] = 3.14*0.75 +3.14;
	directions["south"] = 3.14*1.5 +3.14;
	directions["south_east"] = 3.14*1.75 +3.14;
	directions["south_west"] = 3.14*1.25 +3.14;
	directions["west"] = 3.14 +3.14;


	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {		
		double total = 0;

		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction) {
			
			if ( (*point)->find(direction->first) == (*point)->end() ) {
				vector<string> classification;
				classification.push_back("one");
				classification.push_back("two");
				classification.push_back("three");
				classification.push_back("four");
				classification.push_back("five");
				classification.push_back("six");
				double val = 0;
				for ( vector<string>::const_iterator key = classification.begin(); key != classification.end(); ++key) {
					CustomisedPoint::const_iterator value = (*point)->find(direction->first + "_" + *key);
					if ( value != (*point)->end() ) {
						val += (value->second > 9998.)  ? 0 : value->second;
						cout << value->second << " ---> " << val << endl;
					}
				}			
				(**point)[direction->first] = val;
				
			}
			total += (**point)[direction->first];
		}
		(**point)["total"] = (total) ? total : 50;
		
		
		//InteractiveSet* iset = new InteractiveSet();
		//iset->addAction("onmouseover", new InteractiveMagnify(4));
		vector<double> values;
		double scale = 0;	
	    
		
    
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction)
			values.push_back((**point)[direction->first]);
			
		double ms = *std::max_element(values.begin(), values.end());
		double x = (**point)["step"] + (**point)["shift"];
	
		
		Polyline* grid = new Polyline();
		grid->setColour(Colour("grey"));
		grid->setThickness(2);
		grid->setLineStyle(M_DOT);
		scale = 200;
		double l100 = 12*3600;
		for (float angle = 0; angle <= 2; angle+=0.1)
			grid->push_back(PaperPoint(x +(l100 * cos(3.14*angle)) , l100 * sin(3.14*angle)));
			visitor.push_back(grid);
			
	
		

		//visitor.push_back(iset);
		
		
		if (total == 0) continue;
	
		for ( map<string, float>::const_iterator direction = directions.begin(); direction != directions.end(); ++direction)
			{
				triangle5(*direction, **point, visitor, x, ms);
			}

		// Draw the Control
		if ( (*point)->find("control") != (*point)->end() ) {
			Polyline* control = new Polyline();
			control->setColour(Colour("red"));
			control->setThickness(2);
			control->setLineStyle(M_DASH);

			double angle = (**point)["control"] - 180.;

			control->push_back(PaperPoint(x , 0));
			control->push_back(PaperPoint(x +(l100 * sin(angle*(3.14/180.))), l100 * cos(angle*(3.14/180.))));
			visitor.push_back(control);
		}

		// Draw the Forecast
		if ( (*point)->find("hres") != (*point)->end() ) {
			Polyline* hres = new Polyline();
			hres->setColour(Colour("blue"));
			hres->setThickness(2);
			hres->setLineStyle(M_SOLID);


			double angle = (**point)["hres"]-180;
			hres->push_back(PaperPoint(x , 0));
			hres->push_back(PaperPoint(x +(l100 * sin(angle*(3.14/180.))), l100 * cos(angle*(3.14/180.))));
			visitor.push_back(hres);
		}
	}
	

}




CdfGraph::CdfGraph() 
{
}

CdfGraph::~CdfGraph()
{
}
void CdfGraph::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{


	CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!
	
	//visitor << "CdfGraph::preparePlot(Data<PaperPoint>& data, visitor&)" << endl;
	
	if (points.empty()) return;
	
	vector<string>::iterator icolour = colour_.begin();
    // First make sure that we have enough colour/style/thicknes
    
    while (style_.size() < colour_.size()) 
        style_.push_back("solid");
    while (thickness_.size() < colour_.size()) 
        thickness_.push_back(2);    
    
	
	
    // First set some defaults!
	



    
    	
    	
    	Polyline* efi  = new Polyline();
	    efi->setColour(*clim_colour_);
	    efi->setLineStyle(clim_style_);
	    efi->setThickness(clim_thickness_);
	    vector<double> clim;
		for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {		
			
			vector<double> steps;
			for ( int i = 0; i <=100; i++) {		    
				ostringstream key;
				key << "clim_" << i;
				map<string, double>::const_iterator step = (*point)->find(key.str());
				if (step != (*point)->end() ) {
                    MagLog::dev() << key.str() << ":" << step->second << "-->" << i << endl;
					efi->push_back(PaperPoint(step->second, i));
					clim.push_back(step->second);
                }
			}			
		
		}
		
		visitor.push_back(efi);
		

		Polyline* box = new Polyline();
		box->setColour(Colour("navy"));
		box->setFilled(true);      
		box->setFillColour(Colour("white"));      
			
		FillShadingProperties* shading = new FillShadingProperties();          

		box->setShading(shading);
		
		double w = 1.5*( transformation.getMaxX() - transformation.getMinX() )/ visitor.absoluteWidth();
		
		box->push_back(PaperPoint(transformation.getMaxX(), 43));
		box->push_back(PaperPoint(transformation.getMaxX()-w, 43));
		box->push_back(PaperPoint(transformation.getMaxX()-w, 57));
		box->push_back(PaperPoint(transformation.getMaxX(), 57));
		box->push_back(PaperPoint(transformation.getMaxX(), 43));
		
		
	/*
		Text* mint = new Text();
		mint->setJustification(MLEFT);		
		mint->push_back(PaperPoint(transformation.getMaxX()-(w*.95), 46));
		
		Text* maxt = new Text();
		maxt->push_back(PaperPoint(transformation.getMaxX()-(w*0.95), 52));
		maxt->setJustification(MLEFT);
		
		if ( !clim.empty() ) {
			vector<double>::const_iterator min = std::min_element(clim.begin(), clim.end());
			vector<double>::const_iterator max = std::max_element(clim.begin(), clim.end());
			maxt->addText("Max: " +  tostring(maground(*max)) , Colour("navy"), 0.3);
			mint->addText("Min : " +  tostring(maground(*min)) , Colour("navy"), 0.3);
		}
		else {
			maxt->addText("Max: ?" , Colour("navy"), 0.3);
			mint->addText( "Min : ?", Colour("navy"), 0.3);
		}
      */
		
	vector<string>::iterator style = style_.begin();
	vector<int>::iterator thickness = thickness_.begin();
   
	vector<BasicGraphicsObject*> sorter;
	int step = 1;
    while ( icolour != colour_.end() ) {
    	Colour colour(*icolour);
      
    	
    
    	
    	Polyline* efi  = new Polyline();
	    efi->setColour(colour);
	    efi->setLineStyle(MagTranslator<string, LineStyle>()(*style));
	    efi->setThickness(*thickness);
	    ++thickness;
        ++style;
		for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {		
			
			vector<double> steps;
			for ( int i = 0; i <=100; i++) {		    
				ostringstream key;
				key << step << "_" << i;
				map<string, double>::const_iterator step = (*point)->find(key.str());
				if (step != (*point)->end() ) {
                    MagLog::dev() << key.str() << ":" << step->second << "-->" << i << endl;
					efi->push_back(PaperPoint(step->second, i));
                }
			}	
			ostringstream key;
			key << step << "_step";
			map<string, double>::const_iterator info = (*point)->find(key.str());
			int s = info->second;
			ostringstream legend;
			legend << "t+ [" << s - 24 << "-" << s <<"h] " ;
			legend_.push_back(legend.str());
			
		}
		
		if ( !efi->empty() ) {		
			sorter.push_back(efi);
			usedColours_.push_back(*icolour);
		}
		else 
			delete efi;
		
	
		// go to next step!
		    ++icolour;
		    step++;
		}
    
    // Here we revert the curve to have the fisrt plotted last! 
        for ( vector<BasicGraphicsObject*>::reverse_iterator object = sorter.rbegin(); object != sorter.rend(); ++object) 
        	visitor.push_back(*object);
            
       //visitor.push_back(box);
		//visitor.push_back(mint);
		//visitor.push_back(maxt);

}


void CdfGraph::visit(LegendVisitor& legend)

{

	Polyline* line = new Polyline();

		                    line->setColour(Colour("black"));

		                    line->setThickness(4);
		                    legend.add(new LineEntry("", line));
		                    return;
	
	vector<string>::reverse_iterator style = style_.rbegin();
	vector<int>::reverse_iterator thickness = thickness_.rbegin();
	vector<string>::reverse_iterator text = legend_.rbegin();
    
   
	for (vector<string>::reverse_iterator colour = usedColours_.rbegin(); colour != usedColours_.rend(); ++colour) {
		Polyline* efi  = new Polyline();
		efi->setColour(Colour(*colour));
		
       
        efi->setLineStyle(MagTranslator<string, LineStyle>()(*style));
		efi->setThickness(*thickness);
		LineEntry* entry = new LineEntry(*text, efi);

		legend.add(entry);
        ++thickness;
        ++style;
        ++text;
	}
	 Polyline* efi  = new Polyline();
			efi->setColour(*clim_colour_);
			efi->setLineStyle(clim_style_);
			efi->setThickness(clim_thickness_);
			LineEntry* entry = new LineEntry("Climate t+[24-48h]", efi);
			
			legend.add(entry);    

}

void EpsShade::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{
	CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!
	double max = transformation.getMaxPCY();

	
	if (points.empty()) return;

    
	Polyline* first  = new Polyline();	
	first->setLineStyle(line_style_);
	first->setThickness(line_thickness_);
	first->setFilled(true);
	first->setShading(new FillShadingProperties());
    
    Polyline* firstmin = first->getNew();
    Polyline* firstmax = first->getNew();
    
    Colour cmin = Colour("sky");
    Colour cmax = Colour("RGB(1.0, 0.222, 0.222)");
    
    Polyline* second  = new Polyline();
	
	second->setLineStyle(line_style_);
	second->setThickness(line_thickness_);
	second->setFilled(true);
	second->setShading(new FillShadingProperties());
	Polyline* secondmin = second->getNew();
    Polyline* secondmax = second->getNew();
    
    
	Polyline* median  = new Polyline();
	median->setLineStyle(line_style_);
	median->setThickness(line_thickness_);
    
    Polyline* medianmin = median->getNew();
    Polyline* medianmax = median->getNew();
   
	Polyline* backtop  = new Polyline();
	//backtop->setLineStyle(M_DASH);
	backtop->setThickness(2);
    backtop->setColour(*colour_);
    Polyline* backtopmin = backtop->getNew();
    Polyline* backtopmax = backtop->getNew();
    backtopmin->setColour(cmin);
    backtopmax->setColour(cmax);
   
    Polyline* backbottom  = new Polyline();
	//backbottom->setLineStyle(M_DOT);
	backbottom->setThickness(2);
    backbottom->setColour(*colour_);
    
    Polyline* backbottommin = backbottom->getNew();
    Polyline* backbottommax = backbottom->getNew();
    
    backbottommin->setColour(cmin);
    backbottommax->setColour(cmax);
    
    Hsl hsl = colour_->hsl();  
    Hsl hslmin = cmin.hsl(); 
    Hsl hslmax = cmax.hsl(); 
    
    float step = (0.9 - hsl.light_)/3.;
    median->setColour(Colour(hsl));
    medianmin->setColour(Colour(hslmin));
    medianmax->setColour(Colour(hslmax));
    
    hsl.light_ += 2*step;     
    hslmin.light_ += 2*step; 
    hslmax.light_ += 2*step; 
    second->setFillColour(Colour(hsl));
    second->setColour(Colour(hsl));
    secondmin->setFillColour(Colour(hslmin));
    secondmin->setColour(Colour(hslmin));
    secondmax->setFillColour(Colour(hslmax));
    secondmax->setColour(Colour(hslmax));
    
    hsl.light_ += step;
    hslmin.light_ += step; 
    hslmax.light_ += step; 
    first->setFillColour(Colour(hsl));   
    first->setColour(Colour(hsl));
    firstmin->setFillColour(Colour(hslmin));   
    firstmin->setColour(Colour(hslmin));
    firstmax->setFillColour(Colour(hslmax));   
    firstmax->setColour(Colour(hslmax));
    
	
	
		

	DateTime base = points.front()->base();
   
    
    
    
    
	vector<PaperPoint> ten, tenmin, tenmax;
	vector<PaperPoint> ninty, nintymin, nintymax;;
	vector<PaperPoint> twentyfive, twentyfivemin, twentyfivemax;
	vector<PaperPoint> seventyfive, seventyfivemin, seventyfivemax;
    vector<PaperPoint> one, onemin, onemax;
	vector<PaperPoint> ninetynine, ninetyninemin, ninetyninemax;
    
    
    
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		
		double x = (**point)["step"];
		
        
		
		CustomisedPoint::const_iterator y1 = (*point)->find("one");       
		CustomisedPoint::const_iterator y10 = (*point)->find("ten");
        CustomisedPoint::const_iterator y90 = (*point)->find("ninety");
        CustomisedPoint::const_iterator y99 = (*point)->find("ninety_nine");  
		CustomisedPoint::const_iterator y50 = (*point)->find("fifty");
		CustomisedPoint::const_iterator y25 = (*point)->find("twenty_five");
		CustomisedPoint::const_iterator y75 = (*point)->find("seventy_five");

        if ( (**point)["tmin"] ) {		
            tenmin.push_back(PaperPoint(x, y10->second));
		    nintymin.push_back(PaperPoint(x, y90->second));
		    twentyfivemin.push_back(PaperPoint(x, y25->second));
		    seventyfivemin.push_back(PaperPoint(x, y75->second));
		    medianmin->push_back(PaperPoint(x, y50->second));
            onemin.push_back(PaperPoint(x, y1->second));
            ninetyninemin.push_back(PaperPoint(x, y99->second));
        }
        else if ( (**point)["tmax"] ) {
            tenmax.push_back(PaperPoint(x, y10->second));
		    nintymax.push_back(PaperPoint(x, y90->second));
		    twentyfivemax.push_back(PaperPoint(x, y25->second));
		    seventyfivemax.push_back(PaperPoint(x, y75->second));
		    medianmax->push_back(PaperPoint(x, y50->second));
            onemax.push_back(PaperPoint(x, y1->second));
            ninetyninemax.push_back(PaperPoint(x, y99->second));
        }
        else {
            ten.push_back(PaperPoint(x, y10->second));
		    ninty.push_back(PaperPoint(x, y90->second));
		    twentyfive.push_back(PaperPoint(x, y25->second));
		    seventyfive.push_back(PaperPoint(x, y75->second));
		    median->push_back(PaperPoint(x, y50->second));
            one.push_back(PaperPoint(x, y1->second));
            ninetynine.push_back(PaperPoint(x, y99->second));
        }
     }
        
     
	
     for (vector<PaperPoint>::iterator point = one.begin(); point != one.end(); ++point) 
     	backbottom->push_back(*point); 
     for (vector<PaperPoint>::reverse_iterator point = ninetynine.rbegin(); point != ninetynine.rend(); ++point)
     	backtop->push_back(*point);
     for (vector<PaperPoint>::iterator point = ten.begin(); point != ten.end(); ++point)
     	first->push_back(*point);
     for (vector<PaperPoint>::reverse_iterator point = ninty.rbegin(); point != ninty.rend(); ++point)
     	first->push_back(*point);     
     if ( !first->empty() )
        first->push_back(first->front());      
	 for (vector<PaperPoint>::iterator point = twentyfive.begin(); point != twentyfive.end(); ++point)
     	second->push_back(*point);
     for (vector<PaperPoint>::reverse_iterator point = seventyfive.rbegin(); point != seventyfive.rend(); ++point)
     	second->push_back(*point);     	
     if ( !second->empty() )
        second->push_back(second->front());
     
     for (vector<PaperPoint>::iterator point = onemin.begin(); point != onemin.end(); ++point) 
     	backbottommin->push_back(*point); 
     for (vector<PaperPoint>::reverse_iterator point = ninetyninemin.rbegin(); point != ninetyninemin.rend(); ++point)
     	backtopmin->push_back(*point);
     for (vector<PaperPoint>::iterator point = tenmin.begin(); point != tenmin.end(); ++point)
     	firstmin->push_back(*point);
     for (vector<PaperPoint>::reverse_iterator point = nintymin.rbegin(); point != nintymin.rend(); ++point)
     	firstmin->push_back(*point);     
     if ( !firstmin->empty() ) 
        firstmin->push_back(firstmin->front());      
	 for (vector<PaperPoint>::iterator point = twentyfivemin.begin(); point != twentyfivemin.end(); ++point)
     	secondmin->push_back(*point);
     for (vector<PaperPoint>::reverse_iterator point = seventyfivemin.rbegin(); point != seventyfivemin.rend(); ++point)
     	secondmin->push_back(*point);     	
     if ( !secondmin->empty() )
        secondmin->push_back(secondmin->front());
     
     for (vector<PaperPoint>::iterator point = onemax.begin(); point != onemax.end(); ++point) 
     	backbottommax->push_back(*point); 
     for (vector<PaperPoint>::reverse_iterator point = ninetyninemax.rbegin(); point != ninetyninemax.rend(); ++point)
     	backtopmax->push_back(*point);
     for (vector<PaperPoint>::iterator point = tenmax.begin(); point != tenmax.end(); ++point)
     	firstmax->push_back(*point);
     for (vector<PaperPoint>::reverse_iterator point = nintymax.rbegin(); point != nintymax.rend(); ++point)
     	firstmax->push_back(*point);     
     if ( !firstmax->empty() ) 
        firstmax->push_back(firstmax->front());      
	 for (vector<PaperPoint>::iterator point = twentyfivemax.begin(); point != twentyfivemax.end(); ++point)
     	secondmax->push_back(*point);
     for (vector<PaperPoint>::reverse_iterator point = seventyfivemax.rbegin(); point != seventyfivemax.rend(); ++point)
     	secondmax->push_back(*point);     	
     if ( !secondmax->empty() ) 
        secondmax->push_back(secondmax->front());
     
   
   
    if ( !first->empty() ) transformation(*first, visitor);
	if ( !second->empty() ) transformation(*second, visitor);
	
    if ( !firstmin->empty() ) transformation(*firstmin, visitor);
	if ( !secondmin->empty() ) transformation(*secondmin, visitor);
    
	
    if ( !firstmax->empty() ) transformation(*firstmax, visitor);
	if ( !secondmax->empty() ) transformation(*secondmax, visitor);
    
	if ( !median->empty() ) transformation(*median, visitor);
    if ( !medianmin->empty() ) transformation(*medianmin, visitor);
    if ( !medianmax->empty() ) transformation(*medianmax, visitor);
    
    if ( !backbottom->empty() ) transformation(*backbottom, visitor);
    if ( !backtop->empty() ) transformation(*backtop, visitor);
    if ( !backbottommin->empty() ) transformation(*backbottommin, visitor);
    if ( !backtopmin->empty() ) transformation(*backtopmin, visitor);
    if ( !backbottommax->empty() ) transformation(*backbottommax, visitor);
    if ( !backtopmax->empty() ) transformation(*backtopmax, visitor);
    
	
}
class EpsShadeEntry : public LegendEntry
{
public:
	EpsShadeEntry() : LegendEntry(" ")
	{
		
	}
		
	void set(const PaperPoint& point, BasicGraphicsObjectContainer& visitor)
	{
        if (!first_) return;
		
        first_ = false;
        double x = point.x();
		double y = point.y();
        
      
        double height1=0.6;
        double height2=0.4;
        double height3=0.2;
        double width = 0.4;
        double xtext= x+0.7;
        
        
		
	
		
        Colour colour("grey");
        Polyline* median  = new Polyline();
		median->setColour(colour);		
		median->setLineStyle(M_SOLID);
		median->setThickness(4);
		median->push_back(PaperPoint(x-width, y));
		median->push_back(PaperPoint(x+width, y));
        
        Polyline* top  = new Polyline();
		top->setColour(colour);		
		top->setLineStyle(M_DASH);
		top->setThickness(2);
		top->push_back(PaperPoint(x-width, y+height1));
		top->push_back(PaperPoint(x+width, y+height1));
        
        Polyline* bottom  = new Polyline();
		bottom->setColour(colour);		
		bottom->setLineStyle(M_DASH);
		bottom->setThickness(2);
        bottom->push_back(PaperPoint(x-width, y-height1));
		bottom->push_back(PaperPoint(x+width, y-height1));
        
        Hsl hsl = colour.hsl();
        float step = (0.9 - hsl.light_)/3.;
        
       
        hsl.light_ += 2*step;
        
        Polyline* second  = new Polyline();       
	    second->setFilled(true);
	    second->setShading(new FillShadingProperties());
        second->setFillColour(Colour(hsl));
        second->setColour(Colour(hsl));
        second->push_back(PaperPoint(x-width, y-height3));
		second->push_back(PaperPoint(x+width, y-height3));
        second->push_back(PaperPoint(x+width, y+height3));
		second->push_back(PaperPoint(x-width, y+height3));
        
        hsl.light_ += step;
        
        Polyline* first  = new Polyline();       
	    first->setFilled(true);
	    first->setShading(new FillShadingProperties());
        first->setFillColour(Colour(hsl));
        first->setColour(Colour(hsl));
        first->push_back(PaperPoint(x-width, y-height2));
		first->push_back(PaperPoint(x+width, y-height2));
        first->push_back(PaperPoint(x+width, y+height2));
		first->push_back(PaperPoint(x-width, y+height2));
        
    
    
	
	   
   
	
		
        visitor.push_back(first);
        visitor.push_back(second);
        
		visitor.push_back(top);
        visitor.push_back(bottom);
		visitor.push_back(median);
        
        Text* text  = new Text();
		text->setText("M-Climate");
        text->setFont(font_);
		
		text->push_back(PaperPoint(x+0.4, y-0.8));
		visitor.push_back(text);
        
        Text* text99  = new Text();
		text99->setText("99%");
		text99->setFont(font_);
		text99->setJustification(MLEFT);
		text99->push_back(PaperPoint(xtext, y-height1));
		visitor.push_back(text99);  
        
        Text* text90  = new Text();
		text90->setText("90%");
		text90->setFont(font_);
		text90->setJustification(MLEFT);
		text90->push_back(PaperPoint(xtext, y-height2));
		visitor.push_back(text90);
        
        Text* text75  = new Text();
		text75->setText("75%");
		text75->setFont(font_);
		text75->setJustification(MLEFT);
		text75->push_back(PaperPoint(xtext, y-height3));
		visitor.push_back(text75);
        
		Text* text50  = new Text();
		text50->setText("median");
		text50->setFont(font_);
		text50->setJustification(MLEFT);
		text50->push_back(PaperPoint(xtext, y));
		visitor.push_back(text50);
        
        Text* text25  = new Text();
		text25->setText("25%");
		text25->setFont(font_);
		
		text25->setJustification(MLEFT);
		text25->push_back(PaperPoint(xtext, y+height3));
		visitor.push_back(text25);
        
        Text* text10  = new Text();
		text10->setText("10%");
		text10->setFont(font_);
	
		text10->setJustification(MLEFT);
		text10->push_back(PaperPoint(xtext, y+height2));
		visitor.push_back(text10);
        
        Text* text1  = new Text();
		text1->setText("1%");
        text1->setFont(font_);
	
		text1->setJustification(MLEFT);
		text1->push_back(PaperPoint(xtext, y+height1));
		visitor.push_back(text1);
      
	}
    
    MagFont font_;
	
protected:
	static bool first_;
};

bool EpsShadeEntry::first_ = true;

void EpsShade::visit(LegendVisitor& legend)
{
    EpsShadeEntry *entry = new EpsShadeEntry();
    entry->font_ = MagFont("sansserif");
    entry->font_.size(0.4);
    entry->font_.colour(legend.colour_->name());
    legend.add(entry);
}

EpsShade::EpsShade()
{
}

EpsShade::~EpsShade()
{
}

bool alldigit(const string& name)
{
	for (string::const_iterator c = name.begin(); c != name.end(); ++c)
		if ( !isdigit(*c) ) return false;
	return true;
} 

void EpsDirection::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{
    CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	if (points.empty()) return;
		

	DateTime base = points.front()->base();
	
	
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {		
	
        
		
		
		
		//InteractiveSet* iset = new InteractiveSet();
		//iset->addAction("onmouseover", new InteractiveMagnify(4));
			
			
		
		    double x = (**point)["step"] + (**point)["shift"];
		    
		    if ( (**point)[keyword_] == 9999 ) continue;
		    
	        double angle = ((2*3.14) - (((**point)[keyword_]-90)/180) * 3.14) + 3.14;
		
			Polyline* grid = new Polyline();
			grid->setColour(*line_colour_);
			grid->setThickness(line_thickness_);
			grid->setLineStyle(line_style_);		
			
			double l100 = 12*3600;
            grid->push_back(PaperPoint(x +(l100 * cos(angle)) , l100 * sin(angle)));	
            
            grid->push_back(PaperPoint(x, 0));	
			
            			
			visitor.push_back(grid); //iset->push_back(grid);
			
		
		

		

		//visitor.push_back(iset);
		
		
		
		
	}
	
}
void EpsWind::triangle(const pair<string, float>& direction, CustomisedPoint& point, BasicGraphicsObjectContainer& visitor, double pos, double max)
{
    
    Hsl hsl = colour_->hsl();
    
    float light = hsl.light_;
   
    hsl.light_ += (0.99 - light)*((point["total"]- point[direction.first])/point["total"]);
     
	
	double shift = 3.14*0.125;
	
	Polyline* poly = new Polyline();
	poly->setThickness(1);
	
	
	poly->setFillColour(Colour(hsl));	
	poly->setColour(*border_colour_);
	
	double r = 12*3600;
	double val = point[direction.first];
	double factor = r*r/max;
	
	
	
	double length =  sqrt(val*factor);
	double x0 = length;
//	double y0 = 0;
//	double a = length *tan(shift);
//	double b = length *tan(-shift);
	
	double x = x0 * cos(direction.second);
	double y = x0 * sin(direction.second);
	double x1 = x0 * cos(direction.second - shift);
	double y1 = x0 * sin(direction.second - shift);
	double x2 = x0 * cos(direction.second + shift);
	double y2 = x0 * sin(direction.second + shift);
	
	double xs = 0;
	double ys = 0;
	
	
	poly->push_back(PaperPoint(pos + xs , ys ));	
	poly->push_back(PaperPoint(pos + xs + x1, ys + y1));
	poly->push_back(PaperPoint(pos + xs + x, ys + y));
	poly->push_back(PaperPoint(pos + xs + x2, ys + y2));
	poly->push_back(PaperPoint(pos + xs, ys));	
	
	poly->setFilled(true);
	poly->setShading(new FillShadingProperties());
	
	visitor.push_back(poly);


   
}
EpsPlume::EpsPlume()
{
	methods_["time_serie"] = &EpsPlume::timeserie;
	methods_["vertical_profile"] = &EpsPlume::verticalprofile;
}

void EpsPlume::visit(LegendVisitor& legend)
{
	if (!legend_) return;
	if ( shading_ )
	{
		for ( vector<Colour>::iterator entry = shading_legend_.begin(); entry != shading_legend_.end(); ++entry) {
			 Polyline* box = new Polyline();




			        	box->setShading(new FillShadingProperties());
			        box->setFillColour(*entry);
			        box->setFilled(true);
			        box->setStroke(true);
			        box->setColour(Colour("black"));
			        legend.add(new BoxEntry("", box));
		}
	}
	if ( forecast_) {
		Polyline* forecast  = new Polyline();
		forecast->setColour(*forecast_line_colour_);
		forecast->setThickness(forecast_line_thickness_);
		forecast->setLineStyle(forecast_line_style_);
		legend.add(new LineEntry("Oper", forecast));
	}
	if ( control_) {
		Polyline* control  = new Polyline();
		control->setColour(*control_line_colour_);
		control->setThickness(control_line_thickness_);
		control->setLineStyle(control_line_style_);
		legend.add(new LineEntry("Ctrl", control));
	}
	if ( line_ ) {
		Polyline* line  = new Polyline();
		line->setColour(*line_colour_);
		line->setThickness(line_thickness_);
		line->setLineStyle(line_style_);
		legend.add(new LineEntry("EMem", line));
	}
	if (median_) {
		Polyline* median  = new Polyline();
		median->setColour(*median_line_colour_);
		median->setThickness(median_line_thickness_);
		median->setLineStyle(median_line_style_);
		legend.add(new LineEntry("Median", median));
	}
}

void EpsPlume::timeserie(Data& data, BasicGraphicsObjectContainer& visitor)
{
	CustomisedPointsList points; 
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!


	if (points.empty()) return;



	map<string, Polyline* > lines;
	Polyline* control  = new Polyline();
	control->setColour(*control_line_colour_);
	control->setThickness(control_line_thickness_);
	control->setLineStyle(control_line_style_);

	Polyline* forecast  = new Polyline();
	forecast->setColour(*forecast_line_colour_);
	forecast->setThickness(forecast_line_thickness_);
	forecast->setLineStyle(forecast_line_style_);

	Polyline* median  = new Polyline();
	median->setColour(*median_line_colour_);
	median->setThickness(median_line_thickness_);
	median->setLineStyle(median_line_style_);
	map<double, vector<PaperPoint> > shading;
	if ( shading_ ) {
		for ( vector<double>::iterator level = shading_levels_.begin(); level != shading_levels_.end(); ++level)
			shading.insert(make_pair(*level, vector<PaperPoint>()));
	}
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {

		double x = (**point)["step"] + (**point)["shift"];
		double missing = (**point)["missing"];

		vector<double> members;
		for ( map<string, double>::const_iterator value = (*point)->begin(); value != (*point)->end(); ++value) {

			if ( alldigit(value->first) ) {
				if ( line_ ) {
					map<string, Polyline* >::iterator  iline = lines.find(value->first);
					if ( iline == lines.end() ) {
						Polyline* line  = new Polyline();
						line->setColour(*line_colour_);
						line->setThickness(line_thickness_);
						line->setLineStyle(line_style_);
						lines[value->first] = line;
						iline = lines.find(value->first);
					}
					if (  value->second != missing )
						(iline->second)->push_back(PaperPoint(x, value->second));
				}
				members.push_back(value->second);
			}

			if ( forecast_ ) {
			if ( value->first == "hres" &&  value->second != missing )
				forecast->push_back(PaperPoint(x, value->second));
			}
			if ( control_ ) {
			if ( value->first == "control" &&  value->second != missing)
				control->push_back(PaperPoint(x, value->second));
			}


		}
		if ( median_ ) {
					std::sort(members.begin(), members.end());
					median->push_back(PaperPoint(x, members[25]));
		}
		if ( shading_ ) {
			for ( vector<double>::iterator level = shading_levels_.begin(); level != shading_levels_.end(); ++level) {
				int i = *level/2;
				if ( i >= members.size() )
					i = members.size() -1; ;
				shading[*level].push_back(PaperPoint(x, members[i]));
			}

		}



	}


	vector<string>::iterator colour = shading_colours_.begin();

	for (int i = 0; i < shading_levels_.size()/2; i++) {

		double bottom = shading_levels_[i];
		double top  = shading_levels_[shading_levels_.size() - 1 - i];
		Colour col =  ( colour == shading_colours_.end() ) ? Colour("blue") : *colour;
		Polyline* line  = new Polyline();
		line->setColour(col);
		line->setFilled(true);
		line->setShading(new FillShadingProperties());
		line->setFillColour(col);
		visitor.push_back(line);
		for ( vector<PaperPoint>::iterator point = shading[bottom].begin();  point != shading[bottom].end(); ++point)
			line->push_back(*point);

		for ( vector<PaperPoint>::reverse_iterator point = shading[top].rbegin();  point != shading[top].rend(); ++point)
					line->push_back(*point);

		double grey = ((col.red()+col.blue()+col.green())/3.);

		col.setColour(grey, grey, grey);

		++colour;
		shading_legend_.push_back(col);
	}
	if ( line_)
			for ( map<string, Polyline* >::const_iterator line = lines.begin(); line != lines.end(); ++line) {
			transformation(*line->second, visitor);
		}
		if (control_)
			transformation(*control, visitor);
		if (forecast_)
			transformation(*forecast, visitor);
		if (median_)
			transformation(*median, visitor);

}

void EpsPlume::verticalprofile(Data& data, BasicGraphicsObjectContainer& visitor)
{
	CustomisedPointsList points;
	std::set<string> request;
	const Transformation& transformation = visitor.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!


	if (points.empty()) return;



	map<string, Polyline* > lines;
	Polyline* control  = new Polyline();
	control->setColour(*control_line_colour_);
	control->setThickness(control_line_thickness_);
	control->setLineStyle(control_line_style_);

	Polyline* forecast  = new Polyline();
	forecast->setColour(*forecast_line_colour_);
	forecast->setThickness(forecast_line_thickness_);
	forecast->setLineStyle(forecast_line_style_);


	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		double y = (**point)["y"];
		for ( map<string, double>::const_iterator value = (*point)->begin(); value != (*point)->end(); ++value) {
			if ( value->second == (**point)["missing"] )
				continue;
			if ( alldigit(value->first) ) {
				map<string, Polyline* >::iterator  iline = lines.find(value->first);
				if ( iline == lines.end() ) {
					Polyline* line  = new Polyline();
					line->setColour(*line_colour_);
					line->setThickness(line_thickness_);
					line->setLineStyle(line_style_);
					lines[value->first] = line;
					iline = lines.find(value->first);
				}
				(iline->second)->push_back(transformation(UserPoint(value->second, y)));
			}

			if ( value->first == "hres" )
				forecast->push_back(transformation(UserPoint(value->second, y)));
			if ( value->first == "control" )
				control->push_back(transformation(UserPoint(value->second, y)));
		}
	}

	for ( map<string, Polyline* >::const_iterator line = lines.begin(); line != lines.end(); ++line) {
		visitor.push_back(line->second);
	}
	visitor.push_back(control);
	visitor.push_back(forecast);
}
void EpsPlume::operator()(Data& data, BasicGraphicsObjectContainer& visitor)
{
	method_ = lowerCase(method_);
	std::map<string, Method>::iterator method = methods_.find(method_);
	if ( method == methods_.end() ) {
		MagLog::warning() << "Could not find method [" << method_ << "] for plotting : no plot could be done." << endl;
		return;
	}
	(this->*method->second)(data, visitor);
	
	
}    
       

EfiGraph::EfiGraph() {}
EfiGraph::~EfiGraph() {}
    
    
    
void EfiGraph::operator()(Data& data, BasicGraphicsObjectContainer& out)
{
	CustomisedPointsList points;
	std::set<string> request;
	const Transformation& transformation = out.transformation();
	data.customisedPoints(transformation, request, points, true); // we want all the points!

	vector<string>::iterator colour = box_colour_.begin();


	Polyline* ref  = new Polyline();
	ref->setColour(*normal_colour_);
	ref->setThickness(normal_thickness_);
	ref->setLineStyle(normal_style_);
	ref->push_back(PaperPoint(0, 1));
	MagFont font(font_, font_style_, font_size_);
	font.colour(*font_colour_);

	out.push_back(ref);
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {

		ref->push_back(PaperPoint(0, (**point)["steps"]+1));
		for (int i = 1; i <= (**point)["steps"]; i++) {
			ostringstream key;
			key << "efi" << i << "_value";
			ostringstream step;
			step << "efi" << i << "_step";
			bool available = ((*point)->find(step.str()) != (*point)->end());

			int s = (**point)[step.str()];
			ostringstream legend;
			legend << "t+ [" << s - 36 << "-" << s - 12 <<"h] " ;

			Polyline* box  = new Polyline();
			box->setColour(*border_colour_);
			box->setThickness(border_thickness_);
			box->setLineStyle(border_style_);
			box->setFilled(true);
			box->setShading(new FillShadingProperties());
			box->setFillColour(*colour);
			++colour;
			double val = (**point)[key.str()];
			box->push_back(PaperPoint(0, i ));
			box->push_back(PaperPoint(val, i ));
			box->push_back(PaperPoint(val, i +1));
			box->push_back(PaperPoint(0, i +1));
			box->push_back(PaperPoint(0, i ));
			out.push_back(box);
			if ( available ) {
				Text* text  = new Text();
				text->setText(legend.str());
				text->setJustification(MRIGHT);
				text->setFont(font);
				text->push_back(PaperPoint(-101 , i+0.5));
				out.push_back(text);
				Text* value  = new Text();
				value->setText(tostring(maground(val))+"%");
				value->setJustification(MLEFT);
				value->setFont(font);
				value->push_back(PaperPoint(101 , i+0.5));
				out.push_back(value);
			}
		}
	}
	Text* text  = new Text();
	text->setText("-100%");
	text->setJustification(MCENTRE);
	text->setFont(font);
	text->push_back(PaperPoint(-100 , 0.5));
	out.push_back(text);
	text  = new Text();
	text->setText("-50%");
	text->setJustification(MCENTRE);
	text->setFont(font);
	text->push_back(PaperPoint(-50 , 0.5));
	out.push_back(text);
	text  = new Text();
	text->setText("EFI");
	text->setJustification(MCENTRE);
	text->setFont(font);
	text->push_back(PaperPoint(0 , 0.5));
	out.push_back(text);
	text  = new Text();
	text->setText("50%");
	text->setJustification(MCENTRE);
	text->setFont(font);
	text->push_back(PaperPoint(50 , 0.5));
	out.push_back(text);
	text  = new Text();
	text->setText("100%");
	text->setJustification(MCENTRE);
	text->setFont(font);
	text->push_back(PaperPoint(100 , 0.5));
	out.push_back(text);
}
void EfiGraph::visit(LegendVisitor&)
{
}

	

	
