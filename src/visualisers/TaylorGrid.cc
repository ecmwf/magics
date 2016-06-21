/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TaylorGrid.cc
    \brief Implementation of the Template class TaylorGrid.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 3-Oct-2006
    
    Changes:
    
*/



#include "TaylorGrid.h"
#include "Layout.h"
#include "Polyline.h"
#include "Text.h"
#include "PaperPoint.h"
#include "PaperPoint.h"
#include"Transformation.h"
#include "Layer.h"

using namespace magics;

TaylorGrid::TaylorGrid() 
{
}


TaylorGrid::~TaylorGrid() 
{
}

void TaylorGrid::list(double ref, double inc, std::set<double>& out) const {
	Transformation& projection = transformation();
	const double min = projection.getMinX();
	const double max = projection.getMaxX();
	
	for (double val = ref; val <= max; val += inc ) 
		out.insert(val);
	for (double val = ref; val >= min; val -= inc ) 
		out.insert(val);
}

void TaylorGrid::visit(DrawingVisitor& out)
{
	vector<double> labels;
	labels.push_back(0.);
	labels.push_back(0.1);
	labels.push_back(0.2);
	labels.push_back(0.3);
	labels.push_back(0.4);
	labels.push_back(0.5);
	labels.push_back(0.6);
	labels.push_back(0.7);
	labels.push_back(0.8);
	labels.push_back(0.9);
	labels.push_back(0.95);
	labels.push_back(0.99);

	MagLog::dev() << "TaylorGrid::prepareGraphics()-->needs to be implemented! " <<  endl;
	const Transformation& projection = out.transformation();
	const double min = projection.getMinX();
	const double max = projection.getMaxX();
	
	Polyline* horizontal = new Polyline();
	horizontal->setThickness(primary_thickness_);
	horizontal->setColour(*primary_colour_);
	horizontal->push_back(projection(UserPoint(min,1)));
	horizontal->push_back(projection(UserPoint(max,1)));
	out.push_back(horizontal);

	Polyline* vertical = new Polyline();
	vertical->setThickness(primary_thickness_);
	vertical->setColour(*primary_colour_);
	vertical->push_back(projection(UserPoint(min,0)));
	vertical->push_back(projection(UserPoint(max,0)));
	
	out.push_back(vertical);
	
	Polyline* border = new Polyline();	
	border->setThickness(primary_thickness_);
	border->setColour(*primary_colour_); 
	for (double angle = 0.; angle <= 1.; angle += 0.01 ) { 
		
		border->push_back(projection(UserPoint(max, angle)));  
	}
	border->push_back(projection(UserPoint(max, 1)));
	
	std::set<double> values;
	list(primary_reference_, primary_increment_, values);
	
	for (std::set<double>::iterator value = values.begin(); value != values.end(); ++value) {
		Polyline* arc = new Polyline();
		if (* value == primary_reference_ ) {
			arc->setThickness(reference_thickness_);
			arc->setColour(*reference_colour_); 
			arc->setLineStyle(reference_style_); 
		}
		else {
			arc->setThickness(primary_thickness_);
			arc->setColour(*primary_colour_); 
			arc->setLineStyle(primary_style_); 
		}
		for (double angle = 0.; angle <= 1.; angle += 0.01 ) {				 			
			arc->push_back(projection(UserPoint(*value, angle)));	    
		}
		arc->push_back(projection(UserPoint(*value, 1)));
		out.push_back(arc);
	}
	
	for ( vector<double>::const_iterator angle = labels.begin(); angle != labels.end(); ++angle ) {				 
		Polyline* line = new Polyline();
		line->setLineStyle(primary_style_);
		line->setThickness(primary_thickness_);
		line->setColour(*primary_colour_);
		
		line->push_back(projection(UserPoint(max, *angle)));	
		line->push_back(projection(UserPoint(min, 0)));	
		out.push_back(line);
		
		if ( primary_label_ ) {
		    Text* label = new Text();
		    label->addText(*angle, *primary_label_colour_, primary_label_height_);
		
		    label->setAngle( (1.57-acos(*angle)));
		  
		    label->push_back(projection(UserPoint(max + 0.03, *angle)));	
		    out.push_back(label);
		}
	   
	}
	if ( primary_label_ ) {
		Text* label = new Text();
		label->addText(label_,*label_colour_, label_height_ );
		label->setAngle( 3.14/5.);   
		label->push_back(projection(UserPoint(max + 0.08, 0.6 )));
		out.push_back(label);
	}
	out.push_back(border);
	if ( secondary_grid_ ) secondary(out);
}

void TaylorGrid::secondary(DrawingVisitor&  out)
{	
	std::set<double> values;
	Transformation& projection = transformation();
//	double min = projection.getMinX();
//	double max = projection.getMaxX();
	list(secondary_reference_, secondary_increment_, values);
    
	for(std::set<double>::iterator value = values.begin(); value != values.end(); ++value) {
       
	    Polyline* arc = new Polyline();	
	    Text* label = new Text( );
	    label->addText(*value, *secondary_colour_,0.35);

            double radius = *value;
            double centre = primary_reference_;
            double pos = 1.4;
            PaperPoint paper(centre + radius*cos(pos), radius*sin(pos));

	    if ( projection.in(paper) )
		label->push_back(paper);
            else{
                pos = 1.8;
                paper = PaperPoint(centre + radius*cos(pos), radius*sin(pos));
                if ( projection.in(paper) )
			label->push_back(paper);
	    }
            if (!same(*value, 0)) out.push_back(label);

	    arc->setThickness(secondary_thickness_);
	    arc->setColour(*secondary_colour_); 
	    arc->setLineStyle(secondary_style_); 

	    for ( double angle = 0.; angle <= 3.2; angle += 0.01 ) {
			PaperPoint paper(centre + radius*cos(angle), radius*sin(angle));

			if ( projection.in(paper) )
				arc->push_back(paper);
	    }
	    out.push_back(arc);
	}
}

/*!
 Class information are given to the output-stream.
*/
void TaylorGrid::print(ostream& out)  const
{
	out << "TaylorGrid[";
	TaylorGridAttributes::print(out);
	out << "]";
}

void TaylorGrid::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors)
{
	// First we create the layer!
	// and push It to the parent layer! 
	StaticLayer* taylor = new NoDataLayer(this);
	//taylor->id(iconName_);	
	//taylor>name(iconName_);
	layer.add(taylor);
	
	for  (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
	    taylor->set(*visitor);
		(*visitor)->visit(*this);
	}
}
