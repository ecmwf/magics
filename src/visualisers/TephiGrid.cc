/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TephiGrid.cc
    \brief Implementation of the Template class TephiGrid.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 3-Oct-2006
    
    Changes:
    
*/



#include "TephiGrid.h"
#include "Layout.h"
#include "Polyline.h"
#include "Text.h"
#include "PaperPoint.h"
#include "PaperPoint.h"
#include"Transformation.h"
#include "Layer.h"
#include "SciMethods.h"
using namespace magics;

TephiGrid::TephiGrid()
{
}


TephiGrid::~TephiGrid()
{
}

void tempe(double x)
{

}


void step(set<double>& values, set<double>& labels, double from, double to, double ref, double step, int freq)
{

	if ( ref > to ) return;
	int l = 0;
	for ( double v = ref; v < to; v += step) {
		values.insert(v);

		if ( l % freq == 0 ) {


			labels.insert(v);
		}
		l++;
	}
	if ( ref < from ) return;
	l = 0;
	for ( double v = ref; v > from; v -= step) {
			values.insert(v);

			if ( l % freq == 0 ) {

				labels.insert(v);
			}
			l++;
	}
}

void TephiGrid::visit(DrawingVisitor& out)
{


	const Transformation& tephi = out.transformation();
	vector<double> tempe;
	double maxpcx = (tephi.getMaxPCX() + (tephi.getMinPCX()*.25))/1.25;
	double minpcx = tephi.getMinPCX();
	PaperPoint lr(maxpcx, tephi.getMinPCY());
	UserPoint xy;
	tephi.revert(lr, xy);

	double tmin;
	double tmax;
	double pmin;
	double pmax;

	tephi.boundingBox(tmin, pmin, tmax, pmax);


	double tfactor = 10;
	tmax = int(tmax/tfactor)*tfactor + tfactor;
	tmin = int(tmin/tfactor)*tfactor - tfactor;
	
	double pfactor = 100;
	pmax = int(pmax/pfactor)*pfactor + pfactor;
	pmin = int(pmin/pfactor)*pfactor;


	double thmin = magics::theta(tmin+273.15, pmin*100.)-237.15;
	thmin = -100;
	double thmax = magics::theta(tmin+273.15, pmax*100.) -237.15;
	thmax = +450;





	// Isotherm;
	if ( isotherm_ ) {
		std::set<double> isotherms;
		std::set<double> labels;

		MagFont font(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
		font.colour(*isotherm_label_colour_);

		double theta = magics::theta(xy.x()+273.15, (xy.y())*100.);


		step(isotherms, labels, tmin, tmax, isotherm_reference_, isotherm_interval_, isotherm_label_frequency_);

		for (std::set<double>::iterator t = isotherms.begin();  t != isotherms.end(); ++t ) {
			
			Polyline poly;
			Colour colour = (*t != isotherm_reference_) ? *isotherm_colour_ : *isotherm_reference_colour_;
			poly.setColour(colour);
			poly.setThickness(isotherm_thickness_);
			poly.setLineStyle(isotherm_style_);

			poly.push_back(tephi(UserPoint(*t, pressureFromTheta(thmin+273.15, *t+273.15)/100.)));
			poly.push_back(tephi(UserPoint(*t, pressureFromTheta(thmax+273.15, *t+273.15)/100.)));

			tephi(poly, out.layout());

			std::set<double>::iterator label = labels.find(*t);
			if ( label == labels.end() )
				continue;
			UserPoint point(*t, pressureFromTheta(theta, *t+273.15)/100.);
			PaperPoint xy = tephi(point);
			if ( tephi.in(xy) ) {
				Text* text = new Text();

				text->addText("T=" + tostring(*t), font);
				text->setAngle(-3.14/4);
				text->setBlanking(true);
				text->push_back(xy);
				out.push_back(text);
			}

		}
	}

	if ( dry_adiabatic_ ) {
		PaperPoint ll(tephi.getMinPCX(), tephi.getMinPCY());
		UserPoint tp_ll;
		tephi.revert(ll, tp_ll);
		std::set<double> dry;
		std::set<double> labels;

		MagFont font(dry_adiabatic_label_font_, dry_adiabatic_label_style_, dry_adiabatic_label_size_);
		font.colour(*dry_adiabatic_label_colour_);

		step(dry, labels, thmin, thmax, dry_adiabatic_reference_, dry_adiabatic_interval_, dry_adiabatic_label_frequency_);

		for (std::set<double>::iterator t = dry.begin();  t != dry.end(); ++t ) {

			Polyline poly;
			poly.setLineStyle(dry_adiabatic_style_);
			poly.setColour(*dry_adiabatic_colour_);
			poly.setThickness(dry_adiabatic_thickness_);
			poly.push_back(tephi(UserPoint(tmin, magics::pressureFromTheta(*t+273.15, tmin+273.15)/100)));
			poly.push_back(tephi(UserPoint(tmax, magics::pressureFromTheta(*t+273.15, tmax+273.15)/100)));

			tephi(poly, out.layout());
			std::set<double>::iterator label = labels.find(*t);
			if ( label == labels.end() )
				continue;
			UserPoint point(tp_ll.x(), pressureFromTheta(*t+273.15, tp_ll.x()+273.15)/100.);
			if ( tephi.in(point) ) {
				Text* text = new Text();
				text->setAngle(3.14/4);
				text->addText("th=" + tostring(*t), font);
				text->setBlanking(true);
				text->push_back(tephi(point));
				out.push_back(text);
			}
		}
	}
	if ( isobar_ ) {

		std::set<double> isobars;
		std::set<double> labels;

		step(isobars, labels, pmin, pmax, isobar_reference_, isobar_interval_, isobar_label_frequency_);

		for (std::set<double>::iterator p = isobars.begin();  p != isobars.end(); ++p ) {
			Polyline poly;
			poly.setColour(*isobar_colour_);
			poly.setLineStyle(isobar_style_);
			poly.setThickness(isobar_thickness_);
			for (double t  = tmin; t < tmax; t += 1) {
				PaperPoint xy = tephi(UserPoint(t, *p));
				poly.push_back(xy);
				std::set<double>::iterator label = labels.find(*p);
				if ( label == labels.end() )
					continue;
				if (xy.x() >= maxpcx ) {
					map<double, PaperPoint>::iterator label = pressureRightLabels_.find(*p);
					if ( label == pressureRightLabels_.end() ) {
						xy.x(tephi.getMinPCX()*0.75);
						pressureRightLabels_.insert(make_pair(*p, xy));
					}
				}
				if (xy.x() <= minpcx ) {
					xy.x(tephi.getMaxPCX()*0.85);
					map<double, PaperPoint>::iterator label = pressureLeftLabels_.find(*p);
					if ( label == pressureLeftLabels_.end() ) {
						pressureLeftLabels_.insert(make_pair(*p, xy));
					}
					else
						pressureLeftLabels_[*p] = xy;

				}
			}
			tephi(poly, out.layout());
			Polyline* axe = new Polyline();
			axe->setColour(Colour("black"));
			axe->setLineStyle(M_DASH);
			axe->setThickness(1);

			axe->push_back(PaperPoint(maxpcx, tephi.getMinPCY()));

			axe->push_back(PaperPoint(maxpcx, tephi.getMaxPCY()));
			out.push_back(axe);
		}

	}
	if ( mixing_ratio_) {
		vector<float> ratios;
		ratios.push_back(0.1);
		ratios.push_back(0.2);
		ratios.push_back(0.5);
		ratios.push_back(0.7);
		ratios.push_back(1.);
		ratios.push_back(1.5);
		ratios.push_back(2.);
		ratios.push_back(3.);
		ratios.push_back(5.);
		ratios.push_back(7.);
		ratios.push_back(10.);
		ratios.push_back(15.);
		ratios.push_back(20.);
		ratios.push_back(30.);
		// Humidity Mixing ratio Lines
		int grid = 0;
		int label = 0;
		for (vector<float>::iterator r = ratios.begin(); r != ratios.end(); ++r) {
			if ( grid % mixing_ratio_frequency_ )
				continue;
			grid++;
			Polyline poly;
			poly.setColour(*mixing_ratio_colour_);
			poly.setLineStyle(mixing_ratio_style_);
			poly.setThickness(mixing_ratio_thickness_);
			for ( double p = pmin; p < pmax; p += 10) {
				double t = temperatureFromMixingRatio(*r, p*100);
				PaperPoint xy = tephi(UserPoint(t-273.15, p));
				
				poly.push_back(xy);
				
				if ( label % mixing_ratio_label_frequency_ )
						continue;
				label++;
				if (xy.y() < tephi.getMinPCY() ) {
					xy.y(tephi.getMaxPCY()*.5);
					if ( tephi.in(xy) ) {
						map<double, PaperPoint>::iterator label = mixingLabels_.find(*r);
						if ( label == mixingLabels_.end() ) {
							mixingLabels_.insert(make_pair(*r, xy));
						}
					}

				}
			}
			tephi(poly, out.layout());



		}
	}

	if ( saturated_adiabatic_ ){
		MagFont font(saturated_adiabatic_label_font_, saturated_adiabatic_label_style_, saturated_adiabatic_label_size_);
		font.colour(*saturated_adiabatic_label_colour_);
	 // saturated adiabatic
		std::set<double> sat;

		std::set<double> labels;

		step(sat, labels, thmin, thmax, saturated_adiabatic_reference_, saturated_adiabatic_interval_,
						saturated_adiabatic_label_frequency_);


		for (std::set<double>::iterator  thetaw = sat.begin(); thetaw != sat.end(); ++thetaw ) {
			if ( *thetaw > 50.) continue;
			Polyline poly;
			poly.setColour(*saturated_adiabatic_colour_);
			poly.setLineStyle(saturated_adiabatic_style_);
			poly.setThickness(saturated_adiabatic_thickness_);
			double s = thetaEq(*thetaw+273.15, *thetaw+273.15, 1000*100);

			double pl = -1;
			for ( double p = 200; p < pmax; p += 1) {
				double t = temperatureFromThetaEq(s, p*100)-273.15;
				if (t >= -40 )
					poly.push_back(tephi(UserPoint(t, p)));
			 	else
			 		pl = p;
			}
			tephi(poly, out.layout());
			std::set<double>::iterator label = labels.find(*thetaw);
			if ( label == labels.end() )
				continue;
			UserPoint point(-40.5, pl-1);
			if ( tephi.in(point) ) {
				Text* text = new Text();
				text->addText(tostring(*thetaw), font);
		 	 	text->setAngle(-3.14/4);
		 	 	text->push_back(tephi(point));
		 	 	out.push_back(text);
			}

		}
	}
	if ( true ) {
		MagFont font(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
		font.colour(*isotherm_label_colour_);

		for ( int i = 25; i < 60; i += 25) {
			Polyline poly;
			Colour colour =  *isotherm_colour_ ;
			poly.setColour(colour);
			poly.setThickness(isotherm_thickness_);
			poly.setLineStyle(M_DASH);
	
			for ( double p = pmin; p <= pmax; p += 10) {
				poly.push_back(tephi(UserPoint(1000.+i, p)));
			 	
			}
			tephi(poly, out.layout());
			
			


			UserPoint pt(1000+i, tephi.getMaxPCY());

			PaperPoint xy = tephi(pt);
			xy.y(tephi.getMaxPCY()*.5);
			infoLabels_.insert(make_pair(i, xy));
	
	 	
	 	}
	 	std::set<double> isobars;
		std::set<double> labels;

		step(isobars, labels, pmin, pmax, isobar_reference_, isobar_interval_, isobar_label_frequency_);

	 	for (std::set<double>::iterator p = isobars.begin();  p != isobars.end(); ++p ) {
			Polyline poly;
			poly.setColour(*isobar_colour_);
			poly.setLineStyle(M_DASH);
			poly.setThickness(isobar_thickness_);
			poly.push_back(tephi(UserPoint(1000, *p)));
			poly.push_back(tephi(UserPoint(1100, *p)));	
			
		
			tephi(poly, out.layout());
			
		}


	}

}


/*!
 Class information are given to the output-stream.
*/
void TephiGrid::print(ostream& out)  const
{
	out << "TephiGrid[";
	TephiGridAttributes::print(out);
	out << "]";
}
void TephiGrid::visit(LeftAxisVisitor& out)
{
	MagFont font(isobar_label_font_, isobar_label_style_, isobar_label_size_);

		font.colour(*isobar_label_colour_);

		for (map<double, PaperPoint>::iterator label = pressureLeftLabels_.begin(); label != pressureLeftLabels_.end(); ++label) {

			Text *text= new Text();
			text->setText(tostring(label->first));
			text->setFont(font);
			text->setBlanking(true);
			text->setJustification(MRIGHT);
			text->push_back(label->second);
			out.push_back(text);
		}
}
void TephiGrid::visit(RightAxisVisitor& out)
{
	MagFont font(isobar_label_font_, isobar_label_style_, isobar_label_size_);

	font.colour(*isobar_label_colour_);

	for (map<double, PaperPoint>::iterator label = pressureRightLabels_.begin(); label != pressureRightLabels_.end(); ++label) {

		Text *text= new Text();
		text->setText(tostring(label->first));
		text->setFont(font);
		text->setBlanking(true);
		text->setJustification(MLEFT);
		text->push_back(label->second);
		out.push_back(text);
	}

}
void TephiGrid::visit(BottomAxisVisitor& out)
{
	MagFont font(mixing_ratio_label_font_, mixing_ratio_label_style_, mixing_ratio_label_size_);
	font.colour(*mixing_ratio_label_colour_);
	
	for (map<double, PaperPoint>::iterator label = mixingLabels_.begin(); label != mixingLabels_.end(); ++label) {
		Text *text= new Text();
		text->setText(tostring(label->first*10.));
		text->setFont(font);
		text->setBlanking(true);

		text->push_back(label->second);
		out.push_back(text);
	}
	font = MagFont(isotherm_label_font_, isotherm_label_style_, isotherm_label_size_);
	font.colour(*isotherm_label_colour_);
			
	for (map<double, PaperPoint>::iterator label = infoLabels_.begin(); label != infoLabels_.end(); ++label) {
		Text *text= new Text();
		text->setText(tostring(label->first));
		text->setFont(font);
		text->setBlanking(true);

		text->push_back(label->second);
		out.push_back(text);
	}
}

void TephiGrid::visit(TopAxisVisitor& out)
{

}
void TephiGrid::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors)
{
	// First we create the layer!
	// and push It to the parent layer! 
	StaticLayer* tephi = new NoDataLayer(this);
	//taylor->id(iconName_);	
	//taylor>name(iconName_);
	layer.add(tephi);
	
	for  (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
	    tephi->set(*visitor);
		(*visitor)->visit(*this);
	}
}
