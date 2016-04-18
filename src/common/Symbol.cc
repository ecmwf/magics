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

/*! \file Symbol.cc
    \brief Implementation of Symbol graphics class (template).
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/
#include "Symbol.h"
#include "Polyline.h"
#include "ImportObject.h"
#include "Text.h"

using namespace magics;


Symbol::Symbol() : marker_(0), height_(0.2),outline_(false), connectLine_(false)
{
}


Symbol::~Symbol() 
{
}


void Symbol::print(ostream& out)  const
{
	out << "Symbol<P>[";
	BasicGraphicsObject::print(out);
	out << ", height = " << height_;
	out << ", marker = " << marker_;
	out << ", nb_points = " << this->size();
	out << "]";
}


SymbolProperties::SymbolProperties(Colour colour, double height, const string& marker, const string& label) : 
        colour_(colour), 
        height_(height),
        marker_(marker),
        label_(label), position_(Symbol::M_NONE),outline_(false),
        connectLine_(false), image_(false)
{ font_.size(height_); font_.colour(colour_); }

SymbolProperties::SymbolProperties(Colour colour, double height, int marker, const string& label) : 
        colour_(colour), 
        height_(height),
        marker_(Symbol::convert(marker)),
        label_(label),position_(Symbol::M_NONE),outline_(false),
        connectLine_(false), image_(false)
{ font_.size(height_); font_.colour(colour_); }

SymbolProperties::SymbolProperties() : 
        colour_(Colour("none")), 
        height_(0),
        marker_(Symbol::convert(1)),
        label_(""), position_(Symbol::M_NONE),outline_(false),
        connectLine_(false), image_(false)
{ font_.size(height_); font_.colour(colour_); }


void SymbolProperties::setSymbol(const string& symbol, int marker)
{
	marker_ = !symbol.empty() ? symbol : Symbol::convert(marker);
}

Symbol* SymbolProperties::symbol(const string& type) const
{
	Symbol* symbol = 0;

	const unsigned text_size = text_.size();
	
	if ( magCompare(type, "marker")  )	{
		if ( image_  ) {
			ImageSymbol* img =  new ImageSymbol(image_path_, image_format_);
			img->set(image_width_, image_height_);
			symbol = img;
		}

		else
			if ( text_size  ) {
				TextSymbol* text = new TextSymbol();
				text->position(position_);
				symbol = text;
				text->font(font_);
				text->text(text_);
				text->blanking(blanking_);
			}
			else
				symbol = new Symbol();
	}
	if ( magCompare(type, "both") ) {
		TextSymbol* text = new TextSymbol();

		text->position(position_);
		symbol = text;
		text->font(font_);
		text->text(text_);
		text->blanking(blanking_);
	}

	if ( magCompare(type, "text") )
	{
		if ( text_size == 0 ) {
			SimpleTextSymbol* stext = new SimpleTextSymbol(label_); 
			stext->position(position_);
			symbol = stext;
			stext->font(font_);
			vector<string> helper;		
			helper.push_back(label_);
			stext->text(helper);
		}
		else {
			TextSymbol* text = new TextSymbol();		
			text->position(position_);
			symbol = text;
			text->font(font_);
			text->text(text_);
			text->blanking(blanking_);
		}
	}
	if ( magCompare(type, "number") )
	{
		TextSymbol* text = new TextSymbol();		
		text->position(position_);
		symbol = text;
		MagFont font = font_;
		font.colour(colour_);
		text->font(font);

		text->text(text_);
		text->blanking(blanking_);
		symbol->setColour(Colour("none"));
		symbol->setSymbol(marker_);
		symbol->setHeight(0.0);

		return symbol;
	}
	
	symbol->setColour(colour_);
	symbol->setSymbol(marker_);
	symbol->setHeight(height_);
	symbol->outline(outline_, outlineColour_, outlineThickness_, outlineStyle_);
	symbol->connectline(connectLine_, connectLineColour_, connectLineThickness_, connectLineStyle_);

	return symbol;
}


void ComplexSymbol::print(ostream& out) const
{
	out << "ComplexSymbol[" << endl;
	for (vector<GraphicsItem*>::const_iterator item = items_.begin(); item != items_.end(); ++item)
		out << **item  << ", " << endl;
	out << "]" << endl;
}


struct WithinFunction
{
	WithinFunction(const Polyline& box) : boundingbox_(box) {}
	const Polyline& boundingbox_;
	bool operator()(const PaperPoint& point) {
		return ( boundingbox_.within(point) == false );
	}
};

void Symbol::redisplay(const BaseDriver& driver) const
{
	if ( !connectLine_ ) {
		if ( !colour_.none()  )
			driver.redisplay(*this);
		return;
	}
	// We create a Polyline!
	Polyline line;
	line.setColour(connectLineColour_);
	line.setLineStyle(connectLineStyle_);
	line.setThickness(connectLineThickness_);
	for ( vector<PaperPoint>::const_iterator point = begin(); point != end(); ++point) {
		line.push_back(*point);
	}
	//first  we remove the point that are outside!
	// here we cast to be able to clean the symbol list!
	Symbol symbol = *this;
	symbol.connectline(false);

	vector<PaperPoint>::iterator last = std::remove_if(symbol.begin(), symbol.end(), WithinFunction(boundingbox_));
	symbol.erase(last, symbol.end());

	driver.redisplay(symbol);

	vector<Polyline> lines;

	boundingbox_.clip(line, lines);

	for ( vector<Polyline>::const_iterator l = lines.begin(); l != lines.end(); ++l) {

		driver.redisplay(*l);
	}
}

void Symbol::boundingbox(const Polyline& boundingbox)
{
	boundingbox_ = boundingbox;
}

void TextSymbol::redisplay(const BaseDriver& driver) const
{
	if (!connectLine()) {
	if (position_ == M_CENTRE) {
		// send text and symbol;
		Symbol::redisplay(driver);
		// Now we display the text
		vector<PaperPoint>::const_iterator point = begin();
		vector<string>::const_iterator info = texts_.begin();
		while ( point != end() || info != texts_.end() ) {
			Text text;
			text.addText(*info, font_);
			text.push_back(*point);
			text.setBlanking(blanking_);
			text.setVerticalAlign(MHALF);
			text.setJustification(MCENTRE);
			++point;
			text.redisplay(driver);

			++info;
		}
		return;
	}
	// We make sure here that we have the right number of texxt.
	if ( texts_.empty()) {
		texts_.push_back("?");
	}
	while ( texts_.size() < size() ) {
		texts_.push_back( texts_.back() );
	}

	driver.redisplay(*this);
	return;
	}
	// We create a Polyline!
		Polyline line;
		line.setColour(connectLineColour());
		line.setLineStyle(connectLineStyle());
		line.setThickness(connectLineThickness());
		for ( vector<PaperPoint>::const_iterator point = begin(); point != end(); ++point) {
			line.push_back(*point);
		}

		vector<Polyline> lines;

		boundingbox_.clip(line, lines);

		for ( vector<Polyline>::const_iterator l = lines.begin(); l != lines.end(); ++l) {
			driver.redisplay(*l);
		}
		//first  we remove the point that are outside!
		// here we cast to be able to clean the symbol list!
		TextSymbol symbol = *this;
		symbol.connectline(false);

		vector<PaperPoint>::iterator last = std::remove_if(symbol.begin(), symbol.end(), WithinFunction(boundingbox_));
		symbol.erase(last, symbol.end());

		driver.redisplay(symbol);
}

void ImageSymbol::redisplay(const BaseDriver& driver) const
{
	for ( vector<PaperPoint>::const_iterator point = begin(); point != end(); ++point) {
		ImportObject*  object = new ImportObject();
		object->setOrigin(*point);
		object->setPath(path_);
		object->setFormat(format_);
		object->setWidth(width_);
		object->setHeight(height_);
		driver.redisplay(*object);
	}
}
