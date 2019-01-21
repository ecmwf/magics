/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Symbol.h
    \brief Definition of Symbol graphics class.

    Magics Team - ECMWF 2004

    Started: Jan 2004

    Changes:

*/
#ifndef Symbol_H
#define Symbol_H

#include "magics.h"

#include "BaseDriver.h"
#include "BasicGraphicsObject.h"
#include "Polyline.h"
#include "SymbolItem.h"

namespace magics {


class Symbol : public BasicGraphicsObject, public vector<PaperPoint> {
public:
    enum TextPosition
    {
        M_NONE,
        M_BELOW,
        M_ABOVE,
        M_LEFT,
        M_RIGHT,
        M_CENTRE
    };
    Symbol();
    virtual ~Symbol();

    void redisplay(const BaseDriver& driver) const;

    void setMarker(int m) {
        marker_ = m;
        symbol_ = convert(m);
    }

    static string convert(int m) {
        ostringstream symbol;
        symbol << "magics_" << m;
        return symbol.str();
    }

    void boundingbox(const Polyline& boundingbox);
    virtual void push_back(const PaperPoint& point, const string& = "") { vector<PaperPoint>::push_back(point); }

    int getMarker() const { return marker_; }

    void setSymbol(const string& symbol) { symbol_ = symbol; }
    const string& getSymbol() const { return symbol_; }

    void setColour(const Colour& colour) { colour_ = colour; }
    const Colour& getColour() const { return colour_; }

    void setHeight(double h) { height_ = h; }
    double getHeight() const { return height_; }

    void setDistanceApart(double a) { apart_ = a; }
    double getDistanceApart() const { return apart_; }

    void outlineColour(const Colour& a) { outlineColour_ = a; }

    const Colour& outlineColour() const { return outlineColour_; }
    bool outline() const { return outline_; }

    int outlineThickness() const { return outlineThickness_; }
    LineStyle outlineLineStyle() const { return outlineStyle_; }

    void outline(bool outline, const Colour& colour, int thickness, LineStyle style) {
        outline_          = outline;
        outlineColour_    = colour;
        outlineThickness_ = thickness;
        outlineStyle_     = style;
    }
    void connectline(bool connect) { connectLine_ = connect; }
    void connectline(bool connectline, const Colour& colour, int thickness, LineStyle style) {
        connectLine_          = connectline;
        connectLineColour_    = colour;
        connectLineThickness_ = thickness;
        connectLineStyle_     = style;
    }
    int connectLineThickness() const { return connectLineThickness_; }
    LineStyle connectLineStyle() const { return connectLineStyle_; }
    const Colour& connectLineColour() const { return connectLineColour_; }
    bool connectLine() const { return connectLine_; }

    // private:
    // No copy allowed


protected:
    virtual void print(ostream&) const;
    Polyline boundingbox_;

private:
    int marker_;
    double height_;
    string symbol_;
    Colour colour_;
    double apart_;
    Colour outlineColour_;
    LineStyle outlineStyle_;
    int outlineThickness_;
    bool outline_;

    bool connectLine_;
    Colour connectLineColour_;
    LineStyle connectLineStyle_;
    int connectLineThickness_;


    // -- Friends
    friend ostream& operator<<(ostream& s, const Symbol& p) {
        p.print(s);
        return s;
    }
};


class TextSymbol : public Symbol {
public:
    TextSymbol() : position_(M_BELOW), blanking_(false) {}
    ~TextSymbol() {}

    void push_back(const PaperPoint& point, const string& text) {
        Symbol::push_back(point);
        if (text == "_FORCE_EMPTY_TEXT_") {
            texts_.push_back("");
            return;
        }
        if (!text.empty())
            texts_.push_back(text);
    }

    void text(const vector<string>& text) { texts_ = text; }
    void redisplay(const BaseDriver& driver) const;

    vector<string>::const_iterator textBegin() const { return texts_.begin(); }
    vector<string>::const_iterator textEnd() const { return texts_.end(); }
    const vector<string>& text() const { return texts_; }

    void font(const MagFont& font) { font_ = font; }
    const MagFont& font() const { return font_; }
    void blanking(bool blanking) { blanking_ = blanking; }
    bool blanking() const { return blanking_; }

    void position(TextPosition position) { position_ = position; }
    TextPosition position() const { return position_; }

protected:
    mutable vector<string> texts_;
    MagFont font_;
    TextPosition position_;
    bool blanking_;
};


class ImageSymbol : public Symbol {
public:
    ImageSymbol(const string& path, const string& format) : path_(path), format_(format) {}
    ~ImageSymbol() {}
    void redisplay(const BaseDriver& driver) const;

    void set(double width, double height) {
        width_  = width;
        height_ = height;
    }

protected:
    string path_;
    string format_;
    double width_;
    double height_;
};
class SimpleTextSymbol : public TextSymbol {
public:
    SimpleTextSymbol(const string& text) : text_(text) {}
    ~SimpleTextSymbol() {}

    void push_back(const PaperPoint& point) { TextSymbol::push_back(point, text_); }
    void push_back(const PaperPoint& point, const string&) { TextSymbol::push_back(point, text_); }

protected:
    string text_;
};


class ComplexSymbol : public Symbol {
public:
    ComplexSymbol() : rows_(1), columns_(1) {}
    ComplexSymbol(int rows, int columns) : rows_(rows), columns_(columns) {}
    ~ComplexSymbol() {}

    void redisplay(const BaseDriver& driver) const {
        MagLog::dev() << "Redisplay -->" << *this << endl;
        driver.redisplay(*this);
    }
    void add(GraphicsItem* item) { items_.push_back(item); }
    AutoVectorIterable<GraphicsItem> items() const { return AutoVectorIterable<GraphicsItem>(items_); }

protected:
    virtual void print(ostream&) const;
    AutoVector<GraphicsItem> items_;
    int rows_;
    int columns_;
};

struct SymbolProperties {
    Colour colour_;
    double height_;
    string marker_;
    string label_;
    MagFont font_;
    bool blanking_;
    Symbol::TextPosition position_;
    vector<string> text_;

    bool outline_;
    Colour outlineColour_;
    LineStyle outlineStyle_;
    int outlineThickness_;

    bool connectLine_;
    Colour connectLineColour_;
    LineStyle connectLineStyle_;
    int connectLineThickness_;


    bool image_;
    string image_path_;
    string image_format_;
    int image_width_;
    int image_height_;

    SymbolProperties(Colour colour, double height, const string& marker, const string& label = "");
    SymbolProperties(Colour colour, double height, int marker, const string& label = "");
    SymbolProperties();

    void setSymbol(const string& symbol, int marker);
    void position(Symbol::TextPosition position) { position_ = position; }

    virtual ~SymbolProperties() {}
    bool operator<(const SymbolProperties& other) const {
        if (other.colour_ == colour_)
            if (other.height_ == height_)
                return other.marker_ < marker_;
            else
                return other.height_ < height_;
        else
            return other.colour_ < colour_;
    }

    void label(const string& label) { label_ = label; }

    virtual void print(ostream& out) const {
        out << "SymbolProperty[";
        out << colour_ << "(colour),";
        out << height_ << "(height),";
        out << marker_ << "(marker)";
        out << label_ << "(label)";
        out << "]";
    }

    Symbol* symbol(const string&) const;
    //
    // -- Friends
    //! Overloaded << operator to call print().
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolProperties& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
