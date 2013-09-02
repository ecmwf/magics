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

/*! \file Text.h
    \brief Definition of Text graphics class.
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/
#ifndef Text_H
#define Text_H

#include "magics.h"

#include "Label.h"
#include "BaseDriver.h"
#include "BasicGraphicsObject.h"

namespace magics {
	
enum TextElevation { NORMAL, SUPERSCRIPT ,SUBSCRIPT };

class NiceText
{
public:
	NiceText(): elevation_(NORMAL) {}
	~NiceText() {}

	void font(const MagFont& font) { font_ = font; }
	const MagFont& font() const { return font_; }

	void elevation(const TextElevation& elevation) { elevation_ = elevation; }
	TextElevation elevation() const { return elevation_; }

	void text(const string& text) { text_ = text; }
	const string& text() const { return text_; }

protected:
	void print(ostream& out) const 
	{
		out << "[" << text_ << " (" << font_ << ", " << elevation_ << "]" << endl;
	}
	
	MagFont       font_;
	TextElevation elevation_;
	string        text_;

	// -- Friends
	friend ostream& operator<<(ostream& s,const NiceText& p)
		{ p.print(s); return s; }
};


class Text: public BasicGraphicsObject, public vector<PaperPoint> {

public:
	Text();
	
   	Text* clone() const 
	{    	
    		Text* text = new Text; 
		
               
		for(vector<PaperPoint>::const_iterator it=this->begin(); it != this->end(); it++)
		{ 
			text->push_back(*it);
		}	

		for(vector<NiceText>::const_iterator it=textBegin(); it != textEnd(); it++)
		{
			text->addNiceText(*it);
		}

		text->setJustification(justification_);
		text->setBlanking(blanking_);
		text->setAngle(angle_); 
		text->setVerticalAlign(verticalAlign_); 
		

		return text;
    	}
		
	virtual ~Text();
	
	void addText(const string& text, const Colour& colour, double height) {
		nice_.push_back(NiceText());
		nice_.back().text(text);
		MagFont font;
		font.colour(colour); 
		font.size(height); 
		nice_.back().font(font);
	}
	void setText(const string& text);
	void addText(double val, const Colour& colour, double height) {
		addText(tostring(val), colour, height);
	}
	
	void addText(const string& text, const MagFont& f) {
		nice_.push_back(NiceText());
		nice_.back().text(text);
		nice_.back().font(f);
		font_ = f;
	}
	void setFont(const MagFont& f) {
		    if (nice_.empty() == false)
				nice_.back().font(f);
		    font_ = f;
	}
	const MagFont& getFont() {
		    return nice_.empty() ? font_ : nice_.back().font();
		}
	double getMaxFontSize();
	vector<NiceText>::const_iterator textBegin() const {return nice_.begin();}
	vector<NiceText>::const_iterator textEnd() const {return nice_.end();}

	void addNiceText(const NiceText& text) const {nice_.push_back(text);}
	const vector<NiceText>& getNiceText() const {return nice_;}
	double getFontMax(); 
	
	bool noText() { return nice_.empty(); } 
	void clear() {  nice_.clear(); }


	void redisplay(const BaseDriver& driver) const;
	
	
	void setJustification(Justification justification) {justification_ = justification; }
	Justification getJustification() const { return justification_; }
	
	void setBlanking(bool blanking) {blanking_ = blanking; }
	bool getBlanking() const { return blanking_; }

	VerticalAlign getVerticalAlign() const { return verticalAlign_; }
		void setVerticalAlign(const VerticalAlign va) { verticalAlign_ = va; }

		/*!
		  \brief Method setting the angle in which the text is written.
		  
		  The angle gives the gradient in which the text is written. Angles are given 
		  in degree and an angle of 0 degree is horizontal. Angles are going 
		  anticlockwise.
		  
		  \sa setAngle
		*/
		double getAngle() const {return angle_;}
		/*!
		  \brief Method getting the angle in which the text is written.
		  
		  The angle gives the gradient in which the text is written. Angles are given 
		  in degree and an angle of 0 degree is horizontal. Angles are going 
		  anticlockwise.
		  
		  \sa getAngle
		*/
		void setAngle(double angle) {angle_ = angle;}
protected:
	virtual void print(ostream&) const; 
	
	Justification justification_;
	bool           blanking_;
	VerticalAlign verticalAlign_;
	double        angle_;
	MagFont font_;

	
	
	
private:
// No copy allowed
	Text(const Text&);
	Text& operator=(const Text&);
	mutable vector<NiceText> nice_; 

// -- Friends
	friend ostream& operator<<(ostream& s,const Text& p)
		{ p.print(s); return s; }
};

} // namespace magics

#endif
