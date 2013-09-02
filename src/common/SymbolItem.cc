/*! \file GraphicItem.cc
    \brief Implementation of the Template class GraphicItem.
    
    Magics Team - ECMWF 2009
    
    Started: Tue 27-Jan-2009
    
    Changes:
    
*/



#include "SymbolItem.h"
#include "BaseDriver.h"

using namespace magics;

GraphicsItem::GraphicsItem() 
{
}


GraphicsItem::~GraphicsItem() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void GraphicsItem::print(ostream& out)  const
{
	out << "GraphicsItem[";
	out << "x=" << x_<< ", ";
	out << "y=" << y_;
	out << "]";
}

/*!
 Class information are given to the output-stream.
*/		
void SymbolItem::print(ostream& out)  const
{
	out << "SymbolItem[";
	out << "x=" << x_<< ", ";
	out << "y=" << y_<< ", ";
	out << "symbol=" << symbol_<< ", ";
	out << "colour=" << colour_;	
	out << "]";
}

/*!
 Class information are given to the output-stream.
*/		
void TextItem::print(ostream& out)  const
{
	out << "TextItem[";
	out << "x=" << x_ << ", ";
	out << "y=" << y_<< ", ";
	out << "text=" << text_<< ", ";
	out << "font=" << font_;	
	out << "]";
}

/*!
 Class information are given to the output-stream.
*/		
void FlagItem::print(ostream& out)  const
{
	out << "FlagItem[";
	out << "x=" << x_<< ", ";
	out << "y=" << y_;
	out << "]";
}

void FlagItem::redisplay(const ComplexSymbol& symbol, const BaseDriver& driver) 
{
	driver.redisplay(*this, symbol);
}

void SymbolItem::redisplay(const ComplexSymbol& symbol, const BaseDriver& driver) 
{
	driver.redisplay(*this, symbol);
}
void TextItem::redisplay(const ComplexSymbol& symbol, const BaseDriver& driver) 
{
	driver.redisplay(*this, symbol);
}


