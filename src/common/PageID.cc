/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PageID.h
    \brief Implementation of the Template class PageID.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 29-Mar-2004
    
    Changes:
    
*/

#include "PageID.h"
#include "Text.h"
#include "PaperPoint.h"
#include "System.h"
#include "Transformation.h"
using namespace magics;

PageID::PageID() 
{
}

PageID::~PageID() 
{
}


/*!
 Class information are given to the output-stream.
*/		
void PageID::print(ostream& out)  const
{
	out << "PageID[";
	out << "]";
}


NoPageID* PageID::clone() const
{
	PageID* id = new PageID();
	
	id->copy(*this);
	return id;
}

/*!
 Method generating the ID string and send it. The logo is NOT plotted here. 
 
 \sa UserMagLogoPlotting MagLogoPlotting
 
 \todo Do we need to add errors?
 \todo We always plot 'getMagicsVersionString()' is this right?
 \todo Is vertical alignement middle not better?
*/
void PageID::visit(BasicGraphicsObjectContainer& out)
{
	char* regression = getenv("MAGPLUS_REGRESSION");
	if ( regression )
		return;

    SystemInfo info;
    ostringstream text;
    string sep;

    if ( magics_) {
    	text << getMagicsVersionString();
    	sep = " - ";
    }
    if ( system_ ) {
    	text << sep << info.getHostName() << " - " << info.getUserID();
    	sep = " - ";
    }
    if ( date_ )  {
    	text << sep  << info.getTime();
    	sep = " - ";
    }

    if ( text_ && user_text_ != "" )   text << sep << user_text_;

    Text* id = new Text();

    MagFont font(font_);
    font.style(font_style_);
    font.size(PageIDAttributes::height_);
    font.colour(*colour_);

    id->addText(text.str(), font);
    id->setJustification(MLEFT);
    id->setVerticalAlign(MHALF);

    const double y = (PageIDAttributes::height_ /out.absoluteHeight())*100; 	 
    // Line position  0.5 cm/ 0.5cm...
    const double x = 2.5; //  in % of the page

    id->push_back(PaperPoint(x, y));

    out.push_back(id);
    (*logo_)(out);
}


