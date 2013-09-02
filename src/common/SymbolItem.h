/*! \file SymbolItem.h
    \brief Definition of the Template class SymbolItem.
    
    Magics Team - ECMWF 2009
    
    Started: Tue 27-Jan-2009
    
    Changes:
    
*/

#ifndef SymbolItem_H
#define SymbolItem_H

#include "magics.h"
#include "MagFont.h"
#include "ArrowProperties.h"

namespace magics {
class BaseDriver;
class ComplexSymbol;

class GraphicsItem {

public:
	GraphicsItem();
	virtual ~GraphicsItem();
	void x(int x) { x_ = x; }
	void y(int y) { y_ = y; }
	int x() const { return x_;}
	int y() const { return y_; }
	virtual void redisplay(const ComplexSymbol&, const BaseDriver&) {}
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 int x_; // relative coordinate to the Symbol parent
	 int y_;
private:
    //! Copy constructor - No copy allowed
	GraphicsItem(const GraphicsItem&);
    //! Overloaded << operator to copy - No copy allowed
	GraphicsItem& operator=(const GraphicsItem&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GraphicsItem& p)
		{ p.print(s); return s; }

};

class SymbolItem : public GraphicsItem
{
public:
	SymbolItem() {}
	~SymbolItem() {}
	
	void redisplay(const ComplexSymbol&, const BaseDriver&);
	
	double height() const { return height_; }
	void height(double height)  { height_ = height; }
	
	const string& symbol() const { return symbol_; }
	void symbol(const string& symbol)  { symbol_ = symbol; }

	const Colour& colour() const { return colour_; }
	void colour(const Colour& colour)  { colour_ = colour; }	

protected:
	virtual void print(ostream&) const; 
	double	height_;
	string	symbol_;
	Colour colour_;
	
};
class TextItem : public GraphicsItem
{
public:
	TextItem() {}
	~TextItem() {}
	
	void redisplay(const ComplexSymbol&, const BaseDriver&);
	
	const string& text() const { return text_; }
	void text(const string& text)  { text_ = text; }
	
	MagFont font() const { return font_; }
	void font(const MagFont& font)  { font_ = font; }
	
protected:
	virtual void print(ostream&) const; 
	string	text_;
	MagFont font_;
	
};
class FlagItem : public GraphicsItem, public ArrowProperties
{
	
public:
	FlagItem() {}
	~FlagItem() {}
	
	void redisplay(const ComplexSymbol&, const BaseDriver&);
	
	double length() const             { return length_; }
	void length(double length)        { length_ = length; }

	void speed(double speed)          { speed_ = speed; }
	double speed() const              { return  speed_; }
	
	void direction(double direction)  { direction_ = direction; }
	double direction() const          { return  direction_; }
	
	void convention(FlagConvention convention)  { convention_ = convention; }
	FlagConvention convention() const           { return convention_; }
	
protected:
	virtual void print(ostream&) const; 
	double         length_;
	FlagConvention convention_;
	double speed_;
	double direction_; 
};
} // namespace magics
#endif
