/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SpotDecoder.h
    \brief Implementation of the Template class SpotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/
 
#include "LegendVisitor.h"
#include "Polyline.h"

class EfiEntry : public LegendEntry
{
public:
	EfiEntry(int efi) : LegendEntry(""), efi_(efi)
	{
		if ( efi_ > -200 ) label_ = tostring(efi_) + "%"; 
	}

	void set(const PaperPoint& point, BasicGraphicsObjectContainer& task)
	{
		MagLog::dev() << "LEGEND_STYLE --->" << style_ << endl; 
		MagLog::dev() << "LEGEND_TYPE --->" << type_ << endl;
		MagLog::dev() << "LEGEND_COLOUR --->" << colour_ << endl;
		double x = point.x()-0.7;
		double y = point.y();

		double factor = 1.5;
		double half = factor/2;
		double width = efi_* factor/100.;
		double height = 0.5; 
		double top = y-height;
		double bottom = y+height;

		if ( magCompare(style_, "title") )
		{
			Text* efi = new Text();
			efi->addText("EFI", textColour_, textHeight_);
			efi->push_back(PaperPoint(x, y));	
			task.push_back(efi);
			Text* t1 = new Text();
			t1->addText("-100%", textColour_, textHeight_);
			t1->push_back(PaperPoint(x-factor, y));	
			task.push_back(t1);
			Text* t2 = new Text();
			t2->addText("-50%", textColour_, textHeight_);
			t2->push_back(PaperPoint(x-half, y));	
			task.push_back(t2);
			Text* t3 = new Text();
			t3->addText("50%", textColour_, textHeight_);
			t3->push_back(PaperPoint(x+half, y));	
			task.push_back(t3);
			Text* t4 = new Text();
			t4->addText("100%", textColour_, textHeight_);
			t4->push_back(PaperPoint(x+factor, y));	
			task.push_back(t4);

			return;              
		}

		Polyline* normal  = new Polyline();
		normal->setColour(normalColour_);
		normal->setThickness(normalThickness_);
		normal->push_back(PaperPoint(x, bottom));
		normal->push_back(PaperPoint(x, top));

		Polyline* lmax  = new Polyline();
		lmax->setColour(normalColour_);
		lmax->setThickness(1);
		lmax->push_back(PaperPoint(x+factor, bottom));
		lmax->push_back(PaperPoint(x+factor, top));			

		Polyline* lmin  = new Polyline();
		lmin->setColour(normalColour_);
		lmin->setThickness(1);
		lmin->push_back(PaperPoint(x-factor, bottom));
		lmin->push_back(PaperPoint(x-factor, top));

		Polyline* l1  = new Polyline();
		l1->setColour(normalColour_);
		l1->setThickness(1);
		l1->setLineStyle(M_DOT);
		l1->push_back(PaperPoint(x+half, bottom));
		l1->push_back(PaperPoint(x+half, top));			

		Polyline* l2  = new Polyline();
		l2->setColour(normalColour_);
		l2->setThickness(1); 
		l2->setLineStyle(M_DOT);
		l2->push_back(PaperPoint(x-half, bottom));
		l2->push_back(PaperPoint(x-half, top));

		Polyline* lt  = new Polyline();
		lt->setColour(normalColour_);
		lt->setThickness(1); 
		lt->setLineStyle(M_DOT);
		lt->push_back(PaperPoint(x-factor, top));
		lt->push_back(PaperPoint(x+factor, top));

		Polyline* lb  = new Polyline();
		lb->setColour(normalColour_);
		lb->setThickness(1); 
		lb->setLineStyle(M_DOT);
		lb->push_back(PaperPoint(x-factor, bottom));
		lb->push_back(PaperPoint(x+factor, bottom));

		task.push_back(lmin);
		task.push_back(lmax);
		task.push_back(l1);
		task.push_back(l2);
		task.push_back(lt);
		task.push_back(lb);
		if (efi_ > -200)
		{
		  if ( magCompare(type_, "both") || 
		     ( efi_ < 0  && magCompare(type_, "negative") ) || 
		     ( efi_ > 0  && magCompare(type_, "positive") ) )
		  {			
			Polyline* box  = new Polyline();
			box->setColour(normalColour_);
			box->setFilled(true);
			box->setFillColour(colour_);

			FillShadingProperties* shading = new FillShadingProperties();                      
			box->setShading(shading);


			box->push_back(PaperPoint(x, bottom));
			box->push_back(PaperPoint(x, top));
			box->push_back(PaperPoint(x+width, top));
			box->push_back(PaperPoint(x+width, bottom));
			box->push_back(PaperPoint(x, bottom));
			task.push_back(box);
		  }
		}
		task.push_back(normal);
            
		if ( magCompare(style_, "bottom") )
		{
			Polyline* line  = new Polyline();
			line->setColour(normalColour_);
			line->setThickness(1); 
			line->push_back(PaperPoint(x-factor, bottom));
			line->push_back(PaperPoint(x+factor, bottom));
			task.push_back(line);
		} 
		if ( magCompare(style_, "top") )
		{
			Polyline* line  = new Polyline();
			line->setColour(normalColour_);
			line->setThickness(1); 
			line->push_back(PaperPoint(x-factor, top));
			line->push_back(PaperPoint(x+factor, top));
			task.push_back(line);
		}
	}

        double textColumnPosition() { return 0.85; }
	void type(const string& type)  { type_ = type; }
        void style(const string& style)  { style_ = style; }
	void colour(const Colour& colour) { colour_ = colour; }
	void normalColour(const Colour& colour) { normalColour_ = colour; }
	void normalThickness(int thickness) { normalThickness_ = thickness; }
        void textColour(const Colour& colour) { textColour_ = colour; }
        void textHeight(double height) { textHeight_ = height; }

protected:
	int efi_;
	Colour colour_;
	Colour textColour_;
	double textHeight_;
	string type_;
	string style_;
	Colour normalColour_;
	int     normalThickness_;
};
