/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LegendVisitor.h
    \brief Definition of the Template class LegendNode.

    Magics Team - ECMWF 2007

    Started: Tue 6-Mar-2007

    Changes:

*/

#ifndef LegendNode_H
#define LegendNode_H

#include "magics.h"

#include "SceneVisitor.h"

#include "LegendVisitorAttributes.h"
#include "PaperPoint.h"
#include "XmlBasicNodeAttributes.h"
#include "XmlNode.h"

namespace magics {

class Arrow;
class Flag;
class Text;
class Symbol;
class Polyline;
class HistogramLegendMethod;

class LegendEntry {
public:
    LegendEntry(const string& label) :
        last_(false),
        first_(false),
        text_(true),
        label_(label),
        fromto_(false),
        borderColour_("black"),
        meanSet_(false),
        userMin_(false),
        userMax_(false),
        histogram_(0) {}
    LegendEntry(double label) :
        last_(false),
        first_(false),
        text_(true),
        label_(""),
        fromto_(false),
        borderColour_("black"),
        meanSet_(false),
        userMin_(false),
        userMax_(false),
        histogram_(0) {
        format(label);
    }
    LegendEntry(double min, double max) :
        last_(false),
        first_(false),
        text_(true),
        label_(""),
        fromto_(false),
        borderColour_("black"),
        meanSet_(false),
        userMin_(false),
        userMax_(false),
        histogram_(0) {
        format(min, max);
    }

    virtual ~LegendEntry() {}

    virtual void set(const PaperPoint&, BasicGraphicsObjectContainer&) {}
    //! rowBox + columnBox used for a continuous legend...
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);

    virtual void rowHisto(const PaperPoint& point, BasicGraphicsObjectContainer& out, const Colour&) {
        rowBox(point, out);
    }
    virtual void columnHisto(const PaperPoint& point, BasicGraphicsObjectContainer& out, const Colour&) {
        columnBox(point, out);
    }

    virtual bool needContinuousText(Text&) { return false; }
    virtual bool needText() { return true; }
    virtual const string& label() const;
    virtual void interpret(string&) const {}
    void setArrow(Arrow* arrow) { arrow_ = arrow; }
    string from() { return tostring(from_); }
    string to() { return tostring(to_); }

    void userText(const string& text, const string& automatic) {
        if (userText_.size())
            return;
        userText_ = text;

        automatic_ = magCompare(automatic, "automatic_text_only");
    }
    const string& userText() { return userText_; }
    const string& units() { return units_; }
    void units(const string& units) { units_ = units; }

    void font(const MagFont& font) { font_ = font; }
    void angle(double angle) { angle_ = angle; }
    void set(const LegendVisitor&);
    void last() { last_ = true; }
    bool isLast() { return last_; }
    bool isFirst() { return first_; }
    void first() { first_ = true; }
    void notext() { text_ = false; }
    void text() { text_ = true; }
    void factor(double factor) { factor_ = factor; }

    void width(double width) { width_ = width; }
    PaperPoint centreSymbolBox(const PaperPoint&);
    virtual PaperPoint leftTextBox(const PaperPoint&);

    double computeWidth(double);

    // Information needed for histogram mode
    int population() const { return population_; }
    int totalPopulation() const { return totalPopulation_; }
    void population(int population) { population_ = population; }
    void totalPopulation(int population) { totalPopulation_ = population; }
    void mean(double mean) {
        meanValue_ = mean;
        meanSet_   = true;
    }
    void histogramInformation(HistogramLegendMethod* histo) { histogram_ = histo; }
    void borderColour(const Colour& colour) { borderColour_ = colour; }
    void minText(const string& text) {
        minText_ = text;
        label_   = text;
        userMin_ = true;
    }
    void maxText(const string& text) {
        maxText_ = text;
        label_   = text;
        userMax_ = true;
    }
    double min() { return from_; }
    double max() { return to_; }

protected:
    bool last_;
    bool first_;
    bool text_;
    bool automatic_;  // True if the tlegend text is generated automaticaly from the value.
    mutable string label_;
    bool fromto_;
    Colour borderColour_;
    bool meanSet_;
    HistogramLegendMethod* histogram_;

    string userText_;
    string units_;
    string minText_;
    string maxText_;
    bool userMin_;
    bool userMax_;

    double from_;
    double to_;

    string format_;
    MagFont font_;
    double angle_;
    double width_;
    double factor_;

    int population_;
    int totalPopulation_;
    double meanValue_;

    friend class LegendVisitor;
    // double textPosition_;
    virtual Colour colour() { return Colour(); }
    void format(double val) {
        fromto_ = true;
        from_   = val;
        to_     = val;
    }

    void format(double min, double max) {
        from_   = min;
        to_     = max;
        fromto_ = true;
    }

    Arrow* arrow_;
};


class EmptyEntry : public LegendEntry {
public:
    EmptyEntry() : LegendEntry("") {}
    ~EmptyEntry() {}
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    const string& label() const {
        static string empty;
        return empty;
    }
    void interpret(string& text) const { text = ""; }
    bool needText() { return false; }
};


class SymbolEntry : public LegendEntry {
public:
    SymbolEntry(const string& label, Symbol* symbol = 0) : LegendEntry(label), symbol_(symbol) {}
    SymbolEntry(double label, Symbol* symbol = 0) : LegendEntry(label), symbol_(symbol) { format(label); }
    SymbolEntry(double min, double max, Symbol* symbol = 0) : LegendEntry(min, max), symbol_(symbol) {
        format(min, max);
    }
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    ~SymbolEntry();

protected:
    Symbol* symbol_;
    Colour colour();
};


class SimpleSymbolEntry : public SymbolEntry {
public:
    SimpleSymbolEntry(const string& label, Symbol* symbol = 0) : SymbolEntry(label, symbol) {}
    SimpleSymbolEntry(double label, Symbol* symbol = 0) : SymbolEntry(label, symbol) {}
    SimpleSymbolEntry(double min, double max, Symbol* symbol = 0) : SymbolEntry(min, max, symbol) {}
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
};


class LineEntry : public LegendEntry {
public:
    LineEntry(const string label, Polyline* line = 0) : LegendEntry(label), line_(line) {}
    LineEntry(double label, Polyline* line = 0) : LegendEntry(label), line_(line) { format(label); }
    LineEntry(double min, double max, Polyline* line = 0) : LegendEntry(min, max), line_(line) { format(min, max); }
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    bool needContinuousText(Text& text) {
        text.setJustification(MLEFT);
        return true;
    }
    ~LineEntry();

protected:
    Polyline* line_;
    Colour colour();
};

class CdfEntry : public LineEntry {
public:
    CdfEntry(const string label, Polyline* line = 0) : LineEntry(label, line) {}
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    ~CdfEntry();
};
class RainbowEntry : public LineEntry {
public:
    RainbowEntry(double label, Polyline* line = 0) : LineEntry(label, line) {}
    bool needContinuousText(Text&) { return true; }
    void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);

    ~RainbowEntry();
};


class DoubleLineEntry : public LegendEntry {
public:
    DoubleLineEntry(const string label, Polyline* line1 = 0, Polyline* line2 = 0) :
        LegendEntry(label), line1_(line1), line2_(line2) {}
    DoubleLineEntry(double label, Polyline* line1 = 0, Polyline* line2 = 0) :
        LegendEntry(label), line1_(line1), line2_(line2) {
        format(label);
    }
    DoubleLineEntry(double min, double max, Polyline* line1 = 0, Polyline* line2 = 0) :
        LegendEntry(min, max), line1_(line1), line2_(line2) {
        format(min, max);
    }
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    bool needContinuousText(Text& text) {
        text.setJustification(MLEFT);
        return true;
    }
    ~DoubleLineEntry();

protected:
    Polyline* line1_;
    Polyline* line2_;
    Colour colour();
};


class ArrowEntry : public LegendEntry {
public:
    ArrowEntry(const string label, Arrow* arrow = 0) : LegendEntry(label), arrow_(arrow) {}
    ArrowEntry(const double label, Arrow* arrow = 0) : LegendEntry(label), arrow_(arrow) { format(label); }
    ArrowEntry(const double min, double max, Arrow* arrow = 0) : LegendEntry(min, max), arrow_(arrow) {
        format(min, max);
    }
    ~ArrowEntry();
    void setArrow(Arrow* arrow) { arrow_ = arrow; }
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    bool needContinuousText(Text&) { return false; }
    PaperPoint leftTextBox(const PaperPoint&);

protected:
    Arrow* arrow_;
};


class FlagEntry : public LegendEntry {
public:
    FlagEntry(const string label, Flag* flag = 0) : LegendEntry(label), flag_(flag) {}
    FlagEntry(const double label, Flag* flag = 0) : LegendEntry(label), flag_(flag) { format(label); }
    FlagEntry(const double min, double max, Flag* flag = 0) : LegendEntry(min, max), flag_(flag) { format(min, max); }
    ~FlagEntry();
    void setArrow(Flag* flag) { flag_ = flag; }
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    virtual void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    bool needContinuousText(Text&) { return false; }
    PaperPoint leftTextBox(const PaperPoint&);

protected:
    Flag* flag_;
};


class BoxEntry : public LegendEntry {
public:
    BoxEntry(const string label, Polyline* box = 0) : LegendEntry(label), box_(box) {}
    BoxEntry(const double label, Polyline* box = 0) : LegendEntry(label), box_(box) { format(label); }
    BoxEntry(double min, double max, Polyline* box = 0) : LegendEntry(min, max), box_(box) { format(min, max); }
    ~BoxEntry();
    void set(const PaperPoint&, BasicGraphicsObjectContainer&);
    void rowBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    void columnBox(const PaperPoint&, BasicGraphicsObjectContainer&);
    void rowHisto(const PaperPoint&, BasicGraphicsObjectContainer&, const Colour&);
    void columnHisto(const PaperPoint&, BasicGraphicsObjectContainer&, const Colour&);

protected:
    Polyline* box_;
    Colour colour();
};


class LegendVisitor : public LayoutVisitor,
                      public LegendVisitorAttributes,
                      public BasicPositionalObject,
                      public AutoVector<LegendEntry> {
public:
    LegendVisitor();
    virtual ~LegendVisitor();
    virtual void getReady();
    virtual LegendVisitor* clone() {
        NOTIMP;
        return 0;
    }
    virtual Layout& layout() const {
        ASSERT(layout_);
        return *layout_;
    }
    virtual Layout* layoutPtr() const {
        ASSERT(layout_);
        return layout_;
    }
    // virtual Layout* legend()  { ASSERT( layout_); return this; }

    void set(const XmlNode& node) { LegendVisitorAttributes::set(node); }
    void set(const std::map<string, string>& map) { LegendVisitorAttributes::set(map); }
    void add(LegendEntry* entry) { AutoVector<LegendEntry>::push_back(entry); }
    void newLegend() {
        if (!AutoVector<LegendEntry>::empty())
            add(new EmptyEntry());
    }
    double getFont_size() const { return font_size_; }
    // Layout* execute(AnimationStep& step,  const Layout*);
    void visit(BasicGraphicsObjectContainer&);
    void visit(BasicSceneObject&);
    void visit(AnimationStep&);
    void visit(MetaDataVisitor&);
    bool positional() { return positional_; }
    bool right() { return magCompare(box_position_, "right"); }
    bool top() { return magCompare(box_position_, "top"); }
    void finish(BasicGraphicsObjectContainer&);
    LegendMethod::LegendType legendType() const { return method_->name(); }
    void last() {
        if (!AutoVector<LegendEntry>::empty())
            AutoVector<LegendEntry>::back()->last();
    }


    const Transformation& transformation() { return LayoutVisitor::transformation(); }
    void transformation(Transformation* transformation) { return LayoutVisitor::transformation(transformation); }
    string format() const { return format_; }

    // Legend entries information, will be added to the metadata information ...
    static void addLegendInfo(const string&, const string&);

    static vector<map<string, string>> legendEntriesInfo_;
    static map<string, string> legendInfo_;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    typedef string (LegendVisitor::*Composition)(const string&, const string&);
    map<string, Composition> compositions_;
    // Calculate the grid!
    void grid();

    void horizontal();
    void vertical();
    void topTitle();
    void bottomTitle();
    void rightTitle();
    void leftTitle();


    typedef void (LegendVisitor::*Builder)();
    map<string, Builder> builders_;
    map<magics::Position, Builder> titleBuilders_;

    void build();

    vector<PaperPoint> positions_;
    PaperPoint titlePosition_;
    Justification titleJustification_;
    float titleAngle_;
    int entriesNumber_;


    bool positional_;
    LegendLayout* legend_;

    string both(const string& automatic, const string& user) { return automatic + " " + user; }
    string user_only(const string&, const string& user) {
        if (user.empty())
            return " ";
        return user;
    }
    string automatic_only(const string& automatic, const string&) { return automatic; }

    double view_x_;
    double view_y_;
    double view_width_;
    double view_height_;
    double font_size_;

private:
    //! Copy constructor - No copy allowed
    LegendVisitor(const LegendVisitor&);
    //! Overloaded << operator to copy - No copy allowed
    LegendVisitor& operator=(const LegendVisitor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const LegendVisitor& p) {
        p.print(s);
        return s;
    }
};


class XmlLegendVisitor : public LegendVisitor, public XmlBasicNodeAttributes {
public:
    XmlLegendVisitor() { positional_ = true; }
    ~XmlLegendVisitor() {}
    void set(const XmlNode& node) {
        XmlNode view = node;
        view.name("view");
        XmlBasicNodeAttributes::set(view);
        LegendVisitor::set(node);
    }
    void copy(const XmlLegendVisitor& other) {
        XmlBasicNodeAttributes::copy(other);
        LegendVisitor::copy(other);
    }
    LegendVisitor* clone() {
        XmlLegendVisitor* legend = new XmlLegendVisitor();
        legend->copy(*this);
        legend->positional_ = true;
        return legend;
    }
    void getReady();
};


class FortranPositionalLegendVisitor : public LegendVisitor {
public:
    FortranPositionalLegendVisitor() { positional_ = true; }
    ~FortranPositionalLegendVisitor() {}
    void set(const XmlNode& node) { LegendVisitor::set(node); }
    void getReady();
    void copy(const FortranPositionalLegendVisitor& other) { LegendVisitor::copy(other); }
    LegendVisitor* clone() {
        FortranPositionalLegendVisitor* legend = new FortranPositionalLegendVisitor();
        legend->copy(*this);
        legend->positional_ = true;
        return legend;
    }
};


class FortranAutomaticLegendVisitor : public LegendVisitor {
public:
    FortranAutomaticLegendVisitor() { positional_ = false; }
    ~FortranAutomaticLegendVisitor() {}
    void set(const XmlNode& node) { LegendVisitor::set(node); }
    void getReady();
    LegendVisitor* clone() {
        FortranAutomaticLegendVisitor* legend = new FortranAutomaticLegendVisitor();
        legend->copy(*this);
        legend->positional_ = false;
        return legend;
    }
};

}  // namespace magics
#endif
