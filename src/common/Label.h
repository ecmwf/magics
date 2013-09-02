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

/*! \file Label.h
    \brief Definition of the Template class Label.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 16-Mar-2004
    
    Changes:
    	Stephan - 18th March 2005: add vertical align
    
*/
#ifndef Label_H
#define Label_H

#include "magics.h"
#include "MagFont.h"


namespace magics {

class Label  {

public:
	Label(const string& label = "");
	

	Label(double label);
	virtual ~Label();


	
	
    
	string getText() const { return label_; }
	void setText(const string& text) { label_ = text; }
    
	void setText(double val, const string& = "") {
		ostringstream s;
		s << val;
		setText(s.str());  
	}

    Label* clone() const {
    	
    	Label* label = new Label(label_); 
		label->font(font_);
		label->setVisible(visible_);
		label->setBlanking(blanking_);
		label->setJustification(justification_);
		label->setVerticalAlign(verticalAlign_);
		label->setAngle(angle_);
		return label;
    }

    void copy(const Label& other) {

    		font_ = other.font_;
    		visible_ = other.visible_;
    		blanking_= other.blanking_;
    		justification_= other.justification_;
    		verticalAlign_= other.verticalAlign_;
    		angle_= other.angle_;


        }
	string getFont() const { return font_.name(); }
	void setFont(const string& font) { font_.name(font); }

	double getFontSize() const { return font_.size(); }
	void setFontSize(double size) { font_.size(size); }
	
	double getHeight() const { return font_.size(); }
	void setHeight(double size) { font_.size(size); }
	
	string getFontStyle() const { return font_.style(); }
	void setFontStyle(const string& style) { font_.style(style); }
	
	const Colour& getFontColour() const { return font_.colour(); }
	void setFontColour(const Colour& colour) { font_.colour(colour); }

	const MagFont& font() { return font_; }
	void font(const MagFont& font) { font_ = font; }
	

	Justification getJustification() const { return justification_; }
	void setJustification(const Justification justification) { justification_ = justification; }

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
    
	
    
	bool getBlanking() const { return blanking_; }
	void setBlanking(bool blanking) { blanking_ = blanking; }
    
	bool isVisible() const { return visible_; }
	void setVisible(bool visible) { visible_ = visible; }

	static 	double convertFontSize(double height) { return height * 48; }
	static 	double height2Point(double height) { return height * 48; }
	static 	double point2Height(double height) { return height / 48; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	string  label_;
	
	MagFont       font_;
	Justification justification_;
	VerticalAlign verticalAlign_;
	double   angle_;
	bool       blanking_;
	bool       visible_;

private:

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Label& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
