/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphicItem.cc
    \brief Implementation of the Template class GraphicItem.

    Magics Team - ECMWF 2009

    Started: Tue 27-Jan-2009

    Changes:

*/


#include "SymbolItem.h"
#include "BaseDriver.h"

using namespace magics;

GraphicsItem::GraphicsItem() {}


GraphicsItem::~GraphicsItem() {}

/*!
 Class information are given to the output-stream.
*/
void GraphicsItem::print(ostream& out) const {
    out << "GraphicsItem[";
    out << "x=" << x_ << ", ";
    out << "y=" << y_;
    out << "]";
}

/*!
 Class information are given to the output-stream.
*/
void SymbolItem::print(ostream& out) const {
    out << "SymbolItem[";
    out << "x=" << x_ << ", ";
    out << "y=" << y_ << ", ";
    out << "symbol=" << symbol_ << ", ";
    out << "colour=" << colour_;
    out << "]";
}

/*!
 Class information are given to the output-stream.
*/
void TextItem::print(ostream& out) const {
    out << "TextItem[";
    out << "x=" << x_ << ", ";
    out << "y=" << y_ << ", ";
    out << "text=" << text_ << ", ";
    out << "font=" << font_;
    out << "]";
}

/*!
 Class information are given to the output-stream.
*/
void FlagItem::print(ostream& out) const {
    out << "FlagItem[";
    out << "x=" << x_ << ", ";
    out << "y=" << y_;
    out << "]";
}

void FlagItem::redisplay(const ComplexSymbol& symbol, const BaseDriver& driver) {
    driver.redisplay(*this, symbol);
}

void SymbolItem::redisplay(const ComplexSymbol& symbol, const BaseDriver& driver) {
    driver.redisplay(*this, symbol);
}
void TextItem::redisplay(const ComplexSymbol& symbol, const BaseDriver& driver) {
    driver.redisplay(*this, symbol);
}
