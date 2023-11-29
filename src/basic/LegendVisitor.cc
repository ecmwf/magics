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

/*!
    \brief Implementation of the Template class LegendNode.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue 6-Mar-2007

    Changes:

*/

#include "LegendVisitor.h"

#include "AnimationRules.h"
#include "Arrow.h"
#include "Dimension.h"
#include "Flag.h"
#include "LegendMethod.h"
#include "MagicsFormat.h"
#include "MetaData.h"
#include "Polyline.h"
#include "Symbol.h"
#include "Text.h"
#include "Transformation.h"

using namespace magics;

vector<map<string, string>> LegendVisitor::legendEntriesInfo_;
map<string, string> LegendVisitor::legendInfo_;

LegendVisitor::LegendVisitor() {
    static int i = 0;
    ostringstream n;
    // BasicSceneObject::name(n.str());
    BasicSceneObject::name(string());

    i++;
    layout_ = new Layout();
    layout_->name("legend");
    current_                             = layout_;
    compositions_["both"]                = &LegendVisitor::both;
    compositions_["automatic_text_only"] = &LegendVisitor::automatic_only;
    compositions_["user_text_only"]      = &LegendVisitor::user_only;
    view_x_                              = 5;
    view_y_                              = 5;
    view_width_                          = 90;
    view_height_                         = 90;
    positional_                          = false;
    font_size_                           = 0.2;
}

LegendVisitor::~LegendVisitor() {}

/*!
 Class information are given to the output-stream.
*/
void LegendVisitor::print(ostream& out) const {
    out << "LegendVisitor[";
    LayoutVisitor::print(out);
    out << "]";
}

void LegendVisitor::visit(BasicGraphicsObjectContainer&) {}

void LegendVisitor::visit(BasicSceneObject& parent) {
    // First visit the parent to collect the legend entry!
    parent.visit(*this);
}

void LegendVisitor::visit(AnimationStep&) {}
PaperPoint LegendEntry::centreSymbolBox(const PaperPoint& middle) {
    // The box is going from x-1 to x+1
    // The width of the Symbol box is 100-width;
    PaperPoint point(middle);
    point.x_ = (middle.x_ - 1) + ((100 - width_) / 100);
    MagLog::dev() << "LegendEntry::centreSymbolBox" << point << endl;
    return point;
}

PaperPoint LegendEntry::leftTextBox(const PaperPoint& middle) {
    // The box is going from x-1 to x+1
    // The width of the Symbol box is 100-width;

    PaperPoint point(middle);
    point.x_ = (middle.x_ - 1) + (2. * (100 - width_) / 100);
    MagLog::dev() << "LegendEntry::leftTextBox" << point << endl;
    return point;
}
PaperPoint ArrowEntry::leftTextBox(const PaperPoint& middle) {
    PaperPoint point(middle);
    point.x_ = (middle.x_ - 1) + (2.5 * (100 - width_) / 100);
    point.y_ += 0.2;
    return point;
}
PaperPoint FlagEntry::leftTextBox(const PaperPoint& middle) {
    // The box is going from x-1 to x+1
    // The width of the Symbol box is 100-width;

    PaperPoint point(middle);
    point.x_ = (middle.x_ - 1) + (2.5 * (100 - width_) / 100);
    point.y_ += 0.5;
    return point;
}
double LegendEntry::computeWidth(double width) {
    return ((100 - width_) / 50) * width;
}

void LineEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& out) {
    double x = point.x();
    double y = point.y();

    line_->push_back(PaperPoint(x - 0.5, y));
    line_->push_back(PaperPoint(x + 0.5, y));
    out.push_back(line_);
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line_style", tostring(line_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line_thickness", tostring(line_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "line");
}
void RainbowEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& out) {
    double x = point.x();
    double y = point.y();

    line_->push_back(PaperPoint(x - 0.5, y + 0.5));
    line_->push_back(PaperPoint(x + 0.5, y + 0.5));
    out.push_back(line_);
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line_style", tostring(line_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line_thickness", tostring(line_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "rainbow");
}
void RainbowEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& out) {
    double x = point.x();
    double y = point.y();

    line_->push_back(PaperPoint(x - 0.9, y));
    line_->push_back(PaperPoint(x - 0.2, y));
    out.push_back(line_);
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line_style", tostring(line_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line_thickness", tostring(line_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "rainbow");
}
void LineEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& out) {
    double x = point.x();
    double y = point.y();

    line_->push_back(PaperPoint(x - 15, y));
    line_->push_back(PaperPoint(x + 1, y));
    out.push_back(line_);
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line_style", tostring(line_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line_thickness", tostring(line_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "line");
}
void DoubleLineEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& out) {
    set(point, out);
}
void DoubleLineEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& out) {
    set(point, out);
}

void LegendVisitor::build() {
    legend_ = new LegendLayout();

    LegendVisitor::legendEntriesInfo_.clear();
    LegendVisitor::legendInfo_.clear();

    legend_->x(view_x_);
    legend_->y(view_y_);
    legend_->width(view_width_);
    legend_->height(view_height_);
    current_->setCoordinates(0, 100, 0, 100);
    current_->clear();
    current_->blankIt();
    current_->push_back(legend_);
    positions_.clear();

    grid();

    // populate the layout!

    vector<string>::const_iterator label        = lines_.begin();
    vector<PaperPoint>::const_iterator position = positions_.begin();
    if (title_) {
        Text* legend = new Text();
        MagFont font(font_);
        
        double font_size = (title_font_size_ == -1) ? font_size_ : title_font_size_;
        font.size(font_size);
        Colour colour = title_font_colour_->automatic() ? *colour_ : *title_font_colour_;
        font.colour(colour);

        legend->setFont(font);
        legend->setText(title_text_);
        legend->setJustification(titleJustification_);
        legend->setAngle(titleAngle_);
        legend->push_back(titlePosition_);
        legend_->push_back(legend);
        legendInfo_.insert(make_pair("legend_title", title_text_));
    }

    if (!empty()) {
        if (use_min_) {
            front()->userText(use_min_text_, "user");
            front()->minText(use_min_text_);
            legendInfo_.insert(make_pair("legend_min_text", use_min_text_));
        }
        if (use_max_) {
            back()->userText(use_max_text_, "user");
            back()->maxText(use_max_text_);
            legendInfo_.insert(make_pair("legend_max_text", use_max_text_));
        }

        back()->units(units_text_);
    }

    for (auto& entry : *this) {
        entry->width(text_width_);
        Text* legend = new Text();
        MagFont font(font_, font_style_, font_size_);
        font.colour(*colour_);
        entry->font(font);
        entry->factor(symbol_factor_);
        entry->angle((orientation_ / 180.) * 3.14);
        entry->borderColour(entry_border_ ? *entry_border_colour_ : Colour("automatic"));

        entry->set(*this);
        string text;
        if (entry->needText()) {
            std::map<string, Composition>::iterator composition = compositions_.find(lowerCase(composition_));
            string user;
            // if the entry has its own user text we use it in priority!
            string utext = entry->userText();
            if (!utext.empty())
                user = utext;
            else
                user = (label != lines_.end()) ? *label : "";
            string automatic = entry->label() + entry->units();

            if (composition == compositions_.end()) {
                text = both(automatic, user);
            }
            else
                text = (this->*composition->second)(automatic, user);
            if (!lines_.empty()) {
                ++label;
                if (label == lines_.end())
                    --label;
            }
        }
        entry->userText(text, composition_);
        legend->setFont(font);
        legend->setText(text);
        legend->setAngle((orientation_ / 180.) * 3.14);
        (*legend).setVerticalAlign(orientation_ ? VerticalAlign::BOTTOM : VerticalAlign::HALF);

        if (text == "ignore")
            continue;

        legendEntriesInfo_.push_back(map<string, string>());
        
        (magCompare(direction_, "column")) ? (*method_).column(*entry, position->x(), position->y(), *legend, *legend_)
                                           : (*method_).row(*entry, position->x(), position->y(), *legend, *legend_);

        ++position;
        legend->setAngle((orientation_ / 180.) * 3.14);
        legend_->push_back(legend);
    }
    current_->frameIt();
}

void LegendVisitor::horizontal() {
    // Here we have an horizontal legend
    int rows   = 1;
    int column = 0;

    for (unsigned int i = 0; i < entriesNumber_; i++) {
        if (columns_ > 1 && column >= columns_) {
            column = 0;
            rows++;
        }
        positions_.push_back(PaperPoint(column * 2 + 1, rows));
        column++;
    }

    int nb = (columns_ > 1) ? columns_ : entriesNumber_;
    legend_->setCoordinates(0, nb * 2, rows + 1, 0);

    if (title_) {
        if (title_position_ == Position::AUTOMATIC)
            title_position_ = Position::TOP;
        (this->*titleBuilders_[title_position_])();
    }

    // here we have a title, we need to adjust the coordinates used in the legend
    // dependind on its position
    if (magCompare(entry_orientation_, "right_left")) {
        std::reverse(begin(), end());
        for (auto& entry : *this)
            std::swap(entry->from_, entry->to_);
        back()->last_   = true;
        front()->last_  = false;
        back()->first_  = false;
        front()->first_ = true;
        std::reverse(lines_.begin(), lines_.end());
    }
}
void LegendVisitor::vertical() {
    int row    = 1;
    int column = 1;
    int rows   = entriesNumber_ / columns_;
    if (entriesNumber_ % columns_)
        rows++;

    // calculate the number of rows we need!
    for (unsigned int i = 0; i < entriesNumber_; i++) {
        MagLog::debug() << "add position [" << column << ", " << row << "]" << endl;
        positions_.push_back(PaperPoint(column, row));
        row++;
        if (row > rows) {
            row = 1;
            column += 1;
        }
    }

    int nb = (columns_ > 1) ? columns_ : column;

    legend_->setCoordinates(0, nb, 0.5, rows + 0.5);
    if (title_) {
        if (title_position_ == Position::AUTOMATIC)
            title_position_ = Position::LEFT;
        (this->*titleBuilders_[title_position_])();
    }

    if (magCompare(entry_orientation_, "top_bottom")) {
        std::reverse(begin(), end());
        for (auto& entry : *this)
            std::swap(entry->from_, entry->to_);
        back()->last_   = true;
        front()->last_  = false;
        back()->first_  = false;
        front()->first_ = true;
        std::reverse(lines_.begin(), lines_.end());
    }
}
void LegendVisitor::topTitle() {
    double minx = legend_->minX();
    double miny = legend_->minY();
    double maxx = legend_->maxX();
    double maxy = legend_->maxY();

    double height  = (100 / (100 - title_ratio_)) * (maxy - miny);
    double newmaxy = miny + height;

    titleJustification_ = Justification::CENTRE;
    titleAngle_         = 0;
    titlePosition_      = PaperPoint((maxx - minx) / 2, (newmaxy + maxy) / 2);

    legend_->maxY(newmaxy);
}
void LegendVisitor::bottomTitle() {
    double minx = legend_->minX();
    double miny = legend_->minY();
    double maxx = legend_->maxX();
    double maxy = legend_->maxY();

    double height  = (100 / (100 - title_ratio_)) * (maxy - miny);
    double newminy = maxy - height;

    titlePosition_      = PaperPoint((maxx - minx) / 2, (newminy + miny) / 2);
    titleAngle_         = 0;
    titleJustification_ = Justification::CENTRE;

    legend_->minY(newminy);
}
void LegendVisitor::rightTitle() {
    double minx = legend_->minX();
    double miny = legend_->minY();
    double maxx = legend_->maxX();
    double maxy = legend_->maxY();

    bool column    = (legend_->absoluteWidth() < legend_->absoluteHeight());
    double width   = (100 / (100 - title_ratio_)) * (maxx - minx);
    double newmaxx = minx + width;

    titleAngle_  = column ? magics::PI * 0.5 : 0;
    float factor = column ? 0.25 : 0.15;
    // Special adjustement for the web ( PDF creation)
    // titlePosition_ = Paperoint(maxx+ (newmaxx-maxx)*factor, (maxy - miny)/2);
    // titleJustification_ = ( column ) ?  Justification::CENTRE : Justification::RIGHT;
    titlePosition_      = PaperPoint(maxx + (newmaxx - maxx) * factor, (maxy + 3 * miny) / 4);
    titleJustification_ = Justification::LEFT;

    legend_->maxX(newmaxx);
}
void LegendVisitor::leftTitle() {
    double minx = legend_->minX();
    double miny = legend_->minY();
    double maxx = legend_->maxX();
    double maxy = legend_->maxY();

    bool column = (legend_->absoluteWidth() < legend_->absoluteHeight());

    titleJustification_ = (column) ? Justification::CENTRE : Justification::RIGHT;
    float factor        = (column) ? 0.25 : 0.15;
    titleAngle_         = (column) ? magics::PI * 1.5 : 0;

    double width   = (100 / (100 - title_ratio_)) * (maxx - minx);
    double newminx = maxx - width;

    titlePosition_ = PaperPoint(minx + (newminx - minx) * 0.1, (maxy - miny) / 2);
    legend_->minX(newminx);
}

void LegendVisitor::grid() {
    if (builders_.empty()) {
        builders_["column"] = &LegendVisitor::vertical;
        builders_["row"]    = &LegendVisitor::horizontal;
    }

    if (titleBuilders_.empty()) {
        titleBuilders_[Position::TOP]    = &LegendVisitor::topTitle;
        titleBuilders_[Position::BOTTOM] = &LegendVisitor::bottomTitle;
        titleBuilders_[Position::LEFT]   = &LegendVisitor::leftTitle;
        titleBuilders_[Position::RIGHT]  = &LegendVisitor::rightTitle;
    }

    if (magCompare(direction_, "automatic")) {
        if (current_->absoluteWidth() < current_->absoluteHeight())
            direction_ = "column";
    }

    vector<string>::const_iterator label = lines_.begin();
    entriesNumber_                       = 0;
    for (auto& entry : *this) {
        entry->set(*this);
        string text = (label != lines_.end() && !label->empty()) ? *label : entry->label();

        if (!lines_.empty()) {
            ++label;
            if (label == lines_.end())
                --label;
        }
        if (text == "ignore")
            continue;
        entriesNumber_++;
    }

    std::map<string, Builder>::iterator builder = builders_.find(lowerCase(direction_));
    if (builder == builders_.end()) {
        horizontal();
    }
    else {
        (this->*builder->second)();
    }
}

void SymbolEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width  = computeWidth(0.8) / 2;
    double height = 0.4;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();

    double incx = width / 3;
    double incy = height / 3;

    Colour colour = (borderColour_.automatic()) ? symbol_->getColour() : borderColour_;

    for (float px = x - width; px < x + width; px += incx)
        for (float py = y - height + incy; py < y + height; py += incy)
            symbol_->push_back(PaperPoint(px, py));

    legend.push_back(symbol_);
}

void SymbolEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width  = 1;
    double height = 0.5;

    double x = point.x();
    double y = point.y();

    double incx = width / 3;
    double incy = height / 6;

    Polyline* frame = new Polyline();
    Colour colour   = (borderColour_.automatic()) ? symbol_->getColour() : borderColour_;
    frame->setColour(Colour("grey"));
    frame->push_back(PaperPoint(x - width, y - height));
    frame->push_back(PaperPoint(x - width, y + height));
    frame->push_back(PaperPoint(x + width, y + height));
    frame->push_back(PaperPoint(x + width, y - height));
    frame->push_back(PaperPoint(x - width, y - height));
    legend.push_back(frame);
    for (float px = x - width; px < x + width; px += incx)
        for (float py = y - height + incy; py < y + height; py += incy)
            symbol_->push_back(PaperPoint(px, py));
    legend.push_back(symbol_);
}

void SymbolEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width  = computeWidth(0.8) / 2;
    double height = 0.2;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();

    double incx = width / 3;
    double incy = height / 3;

    Polyline* frame = new Polyline();
    Colour colour   = (borderColour_.automatic()) ? symbol_->getColour() : borderColour_;
    frame->setColour(Colour("grey"));
    frame->push_back(PaperPoint(x - width, y - height));
    frame->push_back(PaperPoint(x - width, y + height));
    frame->push_back(PaperPoint(x + width, y + height));
    frame->push_back(PaperPoint(x + width, y - height));
    frame->push_back(PaperPoint(x - width, y - height));
    legend.push_back(frame);
    for (float px = x - width; px < x + width; px += incx)
        for (float py = y - height + incy; py < y + height; py += incy)
            symbol_->push_back(PaperPoint(px, py));
    legend.push_back(symbol_);
}

Colour SymbolEntry::colour() {
    return symbol_->getColour();
}

void ArrowEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    PaperPoint pos(centreSymbolBox(point));
    double width = computeWidth(0.8) / 2;
    pos.x_ -= width;
    arrow_->push_back(ArrowPoint(arrow_->getScale(), 0, pos));
    legend.push_back(arrow_);
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "arrow");
    LegendVisitor::addLegendInfo("legend_entry_colour", arrow_->getColour().name());
}
void ArrowEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    set(point, legend);
    Text* text = new Text();
    text->addText(label_, font_);
    text->push_back(leftTextBox(point));
    text->setJustification(Justification::LEFT);
    legend.push_back(text);
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "arrow");
    LegendVisitor::addLegendInfo("legend_entry_colour", arrow_->getColour().name());
}
void ArrowEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    PaperPoint pos(centreSymbolBox(point));
    double width = computeWidth(0.8) / 2;
    pos.x_ -= width;
    pos.y_ += 0.2;

    Text* text = new Text();
    text->addText(label_, font_);
    text->push_back(pos);
    text->setJustification(Justification::LEFT);
    legend.push_back(text);

    pos.y_ -= 0.2;
    arrow_->push_back(ArrowPoint(arrow_->getScale(), 0, pos));
    legend.push_back(arrow_);
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "arrow");
    LegendVisitor::addLegendInfo("legend_entry_colour", arrow_->getColour().name());
}

void FlagEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    PaperPoint pos = centreSymbolBox(point);
    double width   = computeWidth(0.8) / 2;
    pos.x_ -= width;
    flag_->push_back(ArrowPoint(0, 0, pos));
    legend.push_back(flag_);
    flag_->setLength(legend.absoluteHeight());
    flag_->back().set(40, 85);

    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "flag");
    LegendVisitor::addLegendInfo("legend_entry_colour", flag_->getColour().name());
}

void FlagEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    set(point, legend);
    Text* text = new Text();
    text->addText(label_, font_);
    text->push_back(leftTextBox(point));
    text->setJustification(Justification::LEFT);
    legend.push_back(text);
    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "flag");
    LegendVisitor::addLegendInfo("legend_entry_type", flag_->getColour().name());
}
void FlagEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    set(point, legend);
    PaperPoint pos(centreSymbolBox(point));
    double width = computeWidth(0.8) / 2;
    pos.x_ -= width;
    pos.y_ += 0.2;

    Text* text = new Text();
    text->addText(label_, font_);
    text->push_back(pos);
    text->setJustification(Justification::LEFT);
    legend.push_back(text);

    LegendVisitor::addLegendInfo("legend_entry_text", label_);
    LegendVisitor::addLegendInfo("legend_entry_type", "flag");
    LegendVisitor::addLegendInfo("legend_entry_type", flag_->getColour().name());
}
Colour BoxEntry::colour() {
    return box_->getFillColour();
}


void LegendEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    Polyline* box                  = new Polyline();
    FillShadingProperties* shading = new FillShadingProperties();
    box->setFillColour(colour());
    box->setShading(shading);

    Polyline* frame = new Polyline();
    MagLog::debug() << "BoxEntry--->set at " << point << endl;
    double width  = 0.8;
    double height = 0.5;
    double x      = point.x();
    double y      = point.y();

    box->push_back(PaperPoint(x, y - height));
    box->push_back(PaperPoint(x, y + height));
    box->push_back(PaperPoint(x + 2, y + height));
    box->push_back(PaperPoint(x + 2, y - height));
    box->push_back(PaperPoint(x - width, y - height));
    box->setColour(Colour("black"));
    frame->setColour(Colour("black"));
    frame->push_back(PaperPoint(x - width, y - height));
    frame->push_back(PaperPoint(x - width, y + height));
    frame->push_back(PaperPoint(x + 2, y + height));
    frame->push_back(PaperPoint(x + 2, y - height));
    frame->push_back(PaperPoint(x - width, y - height));

    legend.push_back(frame);
}

void LegendEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    Polyline* box = new Polyline();
    Colour col    = colour();
    if (col == "none") {
        FillShadingProperties* shading = new FillShadingProperties();
        box->setFillColour(colour());

        box->setShading(shading);
    }
    Polyline* frame = new Polyline();

    double x      = point.x();
    double y      = point.y();
    double width  = 0.4;
    double height = 0.5;

    box->push_back(PaperPoint(x - width, y - height));
    box->push_back(PaperPoint(x - width, y + height));
    box->push_back(PaperPoint(x, y + height));
    box->push_back(PaperPoint(x, y - height));
    box->push_back(PaperPoint(x - width, y - height));
    box->setColour(Colour("black"));
    frame->setColour(Colour("black"));
    frame->push_back(PaperPoint(x - width, y - height));
    frame->push_back(PaperPoint(x - width, y + height));
    frame->push_back(PaperPoint(x, y + height));
    frame->push_back(PaperPoint(x, y - height));
    frame->push_back(PaperPoint(x - width, y - height));

    legend.push_back(frame);

    ostringstream top, bottom;
    top << MagicsFormat(format_, from_);
    bottom << MagicsFormat(format_, to_);

    Text* from = new Text();
    Text* to   = new Text();
    from->setVerticalAlign(VerticalAlign::HALF);

    to->setVerticalAlign(VerticalAlign::HALF);
    from->addText(top.str(), font_);

    to->addText(bottom.str(), font_);

    to->push_back(PaperPoint(x + 0.25, y + height));
    from->push_back(PaperPoint(x + 0.25, y - height));

    legend.push_back(from);
    legend.push_back(to);
}

void EmptyEntry::rowBox(const PaperPoint&, BasicGraphicsObjectContainer&) {}

void EmptyEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {}

void BoxEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    MagLog::debug() << "BoxEntry--->set at " << point << endl;
    double width  = computeWidth(0.8) / 2;
    double height = 0.4;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();
    if ( isOobMin() ) {
        box_->push_back(PaperPoint(x - width, y));
        box_->push_back(PaperPoint(x + width, y + height));
        box_->push_back(PaperPoint(x + width, y - height));
        box_->push_back(PaperPoint(x - width, y));
        LegendVisitor::addLegendInfo("legend_entry_min_text", "");
        LegendVisitor::addLegendInfo("legend_entry_max_text", to());
        LegendVisitor::addLegendInfo("legend_entry_type", "min_out_of_bond");
    }
    else if ( isOobMax() ) {
        box_->push_back(PaperPoint(x - width, y - height));
        box_->push_back(PaperPoint(x - width, y + height));
        box_->push_back(PaperPoint(x + width, y));
        box_->push_back(PaperPoint(x - width, y - height));
        LegendVisitor::addLegendInfo("legend_entry_min_text", from());
        LegendVisitor::addLegendInfo("legend_entry_max_text", "");
        LegendVisitor::addLegendInfo("legend_entry_type", "max_out_of_bond");
    }

    else {
        box_->push_back(PaperPoint(x - width, y - height));
        box_->push_back(PaperPoint(x - width, y + height));
        box_->push_back(PaperPoint(x + width, y + height));
        box_->push_back(PaperPoint(x + width, y - height));
        box_->push_back(PaperPoint(x - width, y - height));
        box_->setColour(Colour("black"));
        LegendVisitor::addLegendInfo("legend_entry_min_text", from());
        LegendVisitor::addLegendInfo("legend_entry_max_text", to());
        LegendVisitor::addLegendInfo("legend_entry_type", "colorbar");
    }
    box_->setColour(Colour("black"));
    LegendVisitor::addLegendInfo("legend_entry_colour", box_->getFillColour().rgb());
    legend.push_back(box_);
}



void BoxEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width  = 1;
    double height = 0.4;
    double x      = point.x();
    double y      = point.y();

    if (text_ &&  !isOobMin()) {
        Text* from = new Text();
        from->push_back(PaperPoint(x - width, y - height - 0.25));
        from->setVerticalAlign(VerticalAlign::BOTTOM);
        from->setAngle(angle_);
        legend.push_back(from);

        if (automatic_) {
            if (!userMin_) {
                ostringstream bottom;
                bottom << MagicsFormat(format_, from_);
                minText_ = bottom.str();
            }

            from->addText(minText_, font_);
        }
        else if (!last_)
            from->addText(userText_, font_);
        else if (userMax_) {
            ostringstream bottom;
            bottom << MagicsFormat(format_, from_);
            from->addText(bottom.str(), font_);
        }
    }
   

    Polyline* top = new Polyline();
    top->setColour(borderColour_);
    top->setThickness(2);
    Polyline* bottom = new Polyline();
    bottom->setColour(borderColour_);
    bottom->setThickness(2);

    if ( isOobMin() ) {
        top->push_back(PaperPoint(x, y));
        top->push_back(PaperPoint(x + (width * 1.), y + (height * 2)));
    
        bottom->push_back(PaperPoint(x, y));
        bottom->push_back(PaperPoint(x + width, y - height));
        
        box_->push_back(PaperPoint(x, y));
        box_->push_back(PaperPoint(x + width, y + height + height));
        box_->push_back(PaperPoint(x + width, y - height));
        box_->push_back(PaperPoint(x, y));
    }
    else if ( isOobMax() ) {
        top->push_back(PaperPoint(x - (width * 1.), y + (height * 2)));
        top->push_back(PaperPoint(x , y));
    
        bottom->push_back(PaperPoint(x - width, y - height));
        bottom->push_back(PaperPoint(x, y));
        
        box_->push_back(PaperPoint(x - width, y - height));
        box_->push_back(PaperPoint(x - width, y + height + height));
        box_->push_back(PaperPoint(x , y));
        box_->push_back(PaperPoint(x - width, y - height));

    }
    else {
        top->push_back(PaperPoint(x - (width * 1.), y + (height * 2)));
        top->push_back(PaperPoint(x + (width * 1.), y + (height * 2)));
    
        bottom->push_back(PaperPoint(x - width, y - height));
        bottom->push_back(PaperPoint(x + width, y - height));
        
        box_->push_back(PaperPoint(x - width, y - height));
        box_->push_back(PaperPoint(x - width, y + height + height));
        box_->push_back(PaperPoint(x + width, y + height + height));
        box_->push_back(PaperPoint(x + width, y - height));
        box_->push_back(PaperPoint(x - width, y - height));

    }





   

    // Small check
    Colour colour = (borderColour_.automatic()) ? box_->getFillColour() : borderColour_;

    if (box_->getFillColour() == Colour("none")) {
        box_->setFilled(false);
        colour = Colour("black");
    }
    box_->setColour(colour);

    legend.push_back(box_);
    legend.push_back(top);
    legend.push_back(bottom);

    if (last_ && !oob_max_) {
        Polyline* right = new Polyline();
        right->push_back(PaperPoint(x + width, y - height));
        right->push_back(PaperPoint(x + width, y + (height * 2)));
        right->setColour(borderColour_);
        right->setThickness(2);
        legend.push_back(right);
        Text* to = new Text();
        to->setAngle(angle_);
        to->setVerticalAlign(VerticalAlign::BOTTOM);
        to->push_back(PaperPoint(x + width, y - height - 0.25));
        legend.push_back(to);
        if (automatic_) {
            if (!userMax_) {
                ostringstream to;
                to << MagicsFormat(format_, to_);
                maxText_ = to.str();
            }
            to->addText(maxText_, font_);
        }
        else
            to->addText(userText_, font_);
    }

    if (first_ && !oob_min_) {
        Polyline* left = new Polyline();
        left->push_back(PaperPoint(x - width, y - height));
        left->push_back(PaperPoint(x - width, y + (height * 2)));
        left->setColour(borderColour_);
        left->setThickness(2);
        legend.push_back(left);
    }

    LegendVisitor::addLegendInfo("legend_entry_colour", box_->getFillColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_min_text", from());
    LegendVisitor::addLegendInfo("legend_entry_max_text", to());
    LegendVisitor::addLegendInfo("legend_entry_type", "colorbar");
}




  
     

 
void BoxEntry::rowHisto(const PaperPoint& point, BasicGraphicsObjectContainer& legend, const Colour& colour) {
    //	ShadingProperties* shading = box_->getShading();
    // Normall the pilyline left the low value on their left!

    MagLog::debug() << "BoxEntry--->set at " << point << endl;
    double width  = 1;
    double bottom = -0.7;
    double top    = -0.8 + (1. * population_ / totalPopulation_);

    double x = point.x();
    double y = point.y() - 1;
    vector<Polyline*> ticks;

    Colour grid_colour   = histogram_->gridColour();
    int grid_thickness   = histogram_->gridThickness();
    LineStyle grid_style = histogram_->gridStyle();

    if (text_) {
        Text* from = new Text();
        from->push_back(PaperPoint(x - width, y + 1.3));
        from->setVerticalAlign(VerticalAlign::TOP);
        from->setAngle(angle_);
        legend.push_back(from);

        if (automatic_) {
            ostringstream bottom;
            bottom << MagicsFormat(format_, from_);
            from->addText(bottom.str(), font_);
        }
        else
            from->addText(userText_, font_);
        Polyline* tick = new Polyline();
        tick->setLineStyle(LineStyle::SOLID);
        tick->setColour(Colour("black"));
        tick->push_back(PaperPoint(x - width, y + 1.1));
        tick->push_back(PaperPoint(x - width, y + 0.85));
        ticks.push_back(tick);
    }
    if (last_) {
        Text* to = new Text();
        to->setAngle(angle_);
        to->setVerticalAlign(VerticalAlign::TOP);
        to->push_back(PaperPoint(x + width, y + 1.3));
        legend.push_back(to);
        if (automatic_) {
            ostringstream top, bottom;
            top << MagicsFormat(format_, to_);
            to->addText(top.str(), font_);
        }
        else
            to->addText(userText_, font_);

        Polyline* axe = new Polyline();
        axe->setColour(grid_colour);
        axe->setThickness(grid_thickness);
        axe->setLineStyle(grid_style);
        axe->push_back(PaperPoint(x + width, y - 0.2));
        axe->push_back(PaperPoint(x + width, y + 0.8));
        legend.push_back(axe);
        Polyline* tick = new Polyline();
        tick->setLineStyle(LineStyle::SOLID);
        tick->setColour(Colour("black"));
        tick->push_back(PaperPoint(x + width, y + 1.1));
        tick->push_back(PaperPoint(x + width, y + 0.85));
        ticks.push_back(tick);
    }
    if (first_) {
        Text* to = new Text();
        to->setAngle(angle_);
        to->setVerticalAlign(VerticalAlign::BOTTOM);
        to->setJustification(Justification::RIGHT);
        to->push_back(PaperPoint(x - width, y - 0.2));
        legend.push_back(to);
        if (userText_.empty() && histogram_->max() && automatic_) {
            ostringstream top, bottom;
            top << MagicsFormat(format_, totalPopulation_);
            to->addText(top.str(), font_);
        }
        else
            to->addText(userText_, font_);
        Polyline* axe = new Polyline();
        axe->setColour(grid_colour);
        axe->setThickness(grid_thickness);
        axe->setLineStyle(grid_style);
        axe->push_back(PaperPoint(x - width, y - 0.2));
        axe->push_back(PaperPoint(x - width, y + 0.8));

        legend.push_back(axe);
        legend.push_back(to);
    }

    box_->push_back(PaperPoint(x - width, y - bottom));
    box_->push_back(PaperPoint(x - width, y - top));
    box_->push_back(PaperPoint(x + width, y - top));
    box_->push_back(PaperPoint(x + width, y - bottom));
    box_->push_back(PaperPoint(x - width, y - bottom));
    // Small check

    Colour border = colour.automatic() ? box_->getFillColour() : colour;
    if (border == Colour("none")) {
        box_->setFilled(false);
    }

    box_->setColour(border);
    legend.push_back(box_);
    Polyline* box = box_->getNew();
    box->setColour(border);
    box->push_back(PaperPoint(x - width, y + 1.1));
    box->push_back(PaperPoint(x + width, y + 1.1));
    box->push_back(PaperPoint(x + width, y + 0.85));
    box->push_back(PaperPoint(x - width, y + 0.85));
    box->push_back(PaperPoint(x - width, y + 1.1));
    legend.push_back(box);

    Polyline* topbox = box_->getNew();
    topbox->setColour(grid_colour);
    topbox->setThickness(grid_thickness);
    topbox->setLineStyle(grid_style);
    topbox->push_back(PaperPoint(x - width, y + 1.1));
    topbox->push_back(PaperPoint(x + width, y + 1.1));
    legend.push_last(topbox);

    Polyline* bottombox = box_->getNew();
    bottombox->setColour(grid_colour);
    bottombox->setThickness(grid_thickness);
    bottombox->setLineStyle(grid_style);
    bottombox->push_back(PaperPoint(x - width, y + 0.85));
    bottombox->push_back(PaperPoint(x + width, y + 0.85));
    legend.push_last(bottombox);

    Polyline* topline = new Polyline();
    topline->setColour(box_->getFillColour());
    topline->setLineStyle(LineStyle::DASH);
    topline->push_back(PaperPoint(x - width, y - 0.2));
    topline->push_back(PaperPoint(x + width, y - 0.2));

    legend.push_last(topline);

    Polyline* bottomline = new Polyline();
    bottomline->setColour(grid_colour);
    bottomline->setThickness(grid_thickness);
    bottomline->setLineStyle(grid_style);

    bottomline->push_back(PaperPoint(x - width, y + 0.8));
    bottomline->push_back(PaperPoint(x + width, y + 0.8));
    legend.push_last(bottomline);

    for (vector<Polyline*>::iterator tick = ticks.begin(); tick != ticks.end(); ++tick)
        legend.push_back(*tick);

    if (histogram_->mean() && meanSet_) {
        Symbol* symbol = new Symbol();

        symbol->setColour(histogram_->meanColour());
        ostringstream m;
        m << "magics_" << histogram_->meanMarker();
        symbol->setSymbol(m.str());

        symbol->setHeight(histogram_->meanSize());
        double pos = ((2 * width) / (to_ - from_)) * (meanValue_ - from_) + (x - width);
        symbol->push_back(PaperPoint(pos, y + 1.));
        legend.push_last(symbol);
    }
}
void BoxEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width  = computeWidth(0.8) / 2;
    double height = 0.5;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();
    PaperPoint pt = leftTextBox(point);

    if (text_) {
        Text* from = new Text();
        from->setJustification(Justification::LEFT);
        from->setVerticalAlign(VerticalAlign::HALF);
        if (automatic_) {
            if (!userMin_) {
                ostringstream bottom;
                bottom << MagicsFormat(format_, from_);
                minText_ = bottom.str();
            }

            from->addText(minText_, font_);
        }
        else if (!last_)
            from->addText(userText_, font_);
        else {
            if (automatic_ || userMax_) {
                ostringstream bottom;
                bottom << MagicsFormat(format_, from_);
                from->addText(bottom.str(), font_);
            }
        }
        PaperPoint pfrom(pt);
        pfrom.y_ = y - height;
        from->push_back(pfrom);
        from->setAngle(angle_);
        legend.push_back(from);
    }
    if (last_) {
        Text* to = new Text();
        to->setVerticalAlign(VerticalAlign::HALF);
        to->setJustification(Justification::LEFT);
        to->setAngle(angle_);

        if (automatic_) {
            if (maxText_.empty()) {
                ostringstream to;
                to << MagicsFormat(format_, to_);
                maxText_ = to.str();
            }
            to->addText(maxText_, font_);
        }
        else
            to->addText(userText_, font_);
        PaperPoint pto(pt);
        pto.y_ = y + height;
        to->push_back(pto);
        legend.push_back(to);
    }
    box_->push_back(PaperPoint(x - width, y - height));
    box_->push_back(PaperPoint(x - width, y + height));
    box_->push_back(PaperPoint(x + width, y + height));
    box_->push_back(PaperPoint(x + width, y - height));
    box_->push_back(PaperPoint(x - width, y - height));
    Colour colour = (borderColour_.automatic()) ? box_->getFillColour() : borderColour_;
    if (box_->getFillColour() == Colour("none")) {
        box_->setFilled(false);
    }
    box_->setColour(colour);
    legend.push_back(box_);

    Polyline* left = new Polyline();
    left->push_back(PaperPoint(x - width, y - (height * 1.1)));
    left->push_back(PaperPoint(x - width, y + (height * 1.1)));
    left->setColour(borderColour_);
    left->setThickness(2);

    Polyline* right = new Polyline();
    right->push_back(PaperPoint(x + width, y - height * 1.1));
    right->push_back(PaperPoint(x + width, y + (height * 1.1)));
    right->setColour(borderColour_);
    right->setThickness(2);

    legend.push_back(left);
    legend.push_back(right);

    if (last_) {
        Polyline* top = new Polyline();
        top->push_back(PaperPoint(x - width, y + (height)));
        top->push_back(PaperPoint(x + width, y + (height)));
        top->setColour(borderColour_);
        top->setThickness(2);
        legend.push_back(top);
    }
    if (first_) {
        Polyline* bottom = new Polyline();
        bottom->push_back(PaperPoint(x - width, y - height));
        bottom->push_back(PaperPoint(x + width, y - height));
        bottom->setColour(borderColour_);
        bottom->setThickness(2);
        legend.push_back(bottom);
    }

    LegendVisitor::addLegendInfo("legend_entry_colour", box_->getFillColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_min_text", from());
    LegendVisitor::addLegendInfo("legend_entry_max_text", to());
    LegendVisitor::addLegendInfo("legend_entry_type", "colorbar");
}
 

void BoxEntry::columnHisto(const PaperPoint& point, BasicGraphicsObjectContainer& legend, const Colour& colour) {
    MagLog::debug() << "BoxEntry--->set at " << point << endl;
    double width  = computeWidth(0.8) / 2;
    double height = 0.5;
    //    double bottom = 0.7;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();
    PaperPoint pt = leftTextBox(point);

    if (text_) {
        Text* from = new Text();
        from->setJustification(Justification::LEFT);
        from->setVerticalAlign(VerticalAlign::HALF);
        if (userText_.empty() || last_) {
            ostringstream bottom;
            bottom << MagicsFormat(format_, from_);
            from->addText(bottom.str(), font_);
        }
        else
            from->addText(userText_, font_);
        PaperPoint pfrom(pt);
        pfrom.y_ = y - height;
        from->push_back(pfrom);
        from->setAngle(angle_);
        legend.push_back(from);
    }
    if (last_) {
        Text* to = new Text();
        to->setVerticalAlign(VerticalAlign::HALF);
        to->setJustification(Justification::LEFT);
        to->setAngle(angle_);
        if (userText_.empty()) {
            ostringstream top, bottom;
            top << MagicsFormat(format_, to_);
            to->addText(top.str(), font_);
        }
        else
            to->addText(userText_, font_);
        PaperPoint pto(pt);
        pto.y_ = y + height;
        to->push_back(pto);
        legend.push_back(to);
    }
    box_->push_back(PaperPoint(x - width, y - height));
    box_->push_back(PaperPoint(x - width, y + height));
    box_->push_back(PaperPoint(x + width, y + height));
    box_->push_back(PaperPoint(x + width, y - height));
    box_->push_back(PaperPoint(x - width, y - height));
    Colour border = colour.automatic() ? box_->getFillColour() : colour;
    if (border == Colour("none")) {
        box_->setFilled(false);
    }
    box_->setColour(border);
    legend.push_back(box_);
}

Colour LineEntry::colour() {
    return line_->getColour().rgb();
}

Colour DoubleLineEntry::colour() {
    return line1_->getColour().rgb();
}

void LineEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width = computeWidth(0.8) / 2;
    PaperPoint p = centreSymbolBox(point);
    double x     = p.x();
    double y     = p.y();
    line_->push_back(PaperPoint(x - width, y));
    line_->push_back(PaperPoint(x + width, y));
    legend.push_back(line_);
    LegendVisitor::addLegendInfo("legend_entry_line_colour", line_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line_style", tostring(line_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line_thickness", tostring(line_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_type", "line");
}

void CdfEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double right = computeWidth(0.7) / 2;
    double left  = computeWidth(1) / 2;
    PaperPoint p = centreSymbolBox(point);
    double x     = p.x();
    double y     = p.y();
    line_->push_back(PaperPoint(x - left, y));
    line_->push_back(PaperPoint(x + right, y));
    legend.push_back(line_);
}

void DoubleLineEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    double width  = computeWidth(0.8) / 2;
    double height = (line2_) ? 0.2 : 0;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();
    line1_->push_back(PaperPoint(x - width, y - height));
    line1_->push_back(PaperPoint(x + width, y - height));
    legend.push_back(line1_);

    if (!line2_) {
        LegendVisitor::addLegendInfo("legend_entry_line_colour", line1_->getColour().rgb());
        LegendVisitor::addLegendInfo("legend_entry_line_style", tostring(line1_->getLineStyle()));
        LegendVisitor::addLegendInfo("legend_entry_line_thickness", tostring(line1_->getThickness()));
        LegendVisitor::addLegendInfo("legend_entry_type", "line");
        return;
    }
    line2_->push_back(PaperPoint(x - width, y + height));
    line2_->push_back(PaperPoint(x + width, y + height));
    legend.push_back(line2_);
    LegendVisitor::addLegendInfo("legend_entry_line1_colour", line1_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line1_style", tostring(line1_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line1_thickness", tostring(line1_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_line2_colour", line2_->getColour().rgb());
    LegendVisitor::addLegendInfo("legend_entry_line2_style", tostring(line2_->getLineStyle()));
    LegendVisitor::addLegendInfo("legend_entry_line2_thickness", tostring(line2_->getThickness()));
    LegendVisitor::addLegendInfo("legend_entry_type", "double-line");
}

void check(const string& line, vector<string>& lines) {
    if (!line.empty()) {
        lines.push_back(line);
    }
}

void LegendVisitor::getReady() {
    if (lines_.empty() == false)
        return;

    check(text1_, lines_);
    check(text2_, lines_);
    check(text3_, lines_);
    check(text4_, lines_);
    check(text5_, lines_);
    check(text6_, lines_);
    check(text7_, lines_);
    check(text8_, lines_);
    check(text9_, lines_);
    check(text10_, lines_);

    if (lines_.empty())
        check(text_, lines_);

    if (!lines_.empty() && composition_ == "automatic_text_only")
        composition_ = "user_text_only";
}

void XmlLegendVisitor::getReady() {
    ASSERT(BasicSceneObject::parent_);

    Dimension bottom(bottom_, BasicSceneObject::parent_->absoluteWidth(), 0);
    Dimension left(left_, BasicSceneObject::parent_->absoluteHeight(), 0);
    Dimension width(XmlBasicNodeAttributes::width_, BasicSceneObject::parent_->absoluteWidth(), 100);
    Dimension height(XmlBasicNodeAttributes::height_, BasicSceneObject::parent_->absoluteHeight(), 100);

    Dimension mb(margin_bottom_, height.absolute(), 0);
    Dimension ml(margin_left_, width.absolute(), 10);
    Dimension mr(margin_right_, width.absolute(), 10);
    Dimension mt(margin_top_, height.absolute(), 0);

    // We have to calculate the new x, y, width, height
    double xl = ml.percent() * width.absolute() / 100;
    //	double xr = mr.percent()*width.absolute()/100;
    double yb = mb.percent() * height.absolute() / 100;
    //	double yt = mt.percent()*height.absolute()/100;
    double text_x = left.percent() + xl * 100 / BasicSceneObject::parent_->absoluteWidth();
    double text_y = bottom.percent() + yb * 100 / BasicSceneObject::parent_->absoluteHeight();
    ;

    view_x_      = ml.percent();
    view_y_      = mb.percent();
    view_height_ = 100 - mb.percent() - mt.percent();
    view_width_  = 100 - ml.percent() - mr.percent();

    layout_->x(left.percent());
    layout_->y(bottom.percent());
    layout_->width(width.percent());
    layout_->height(height.percent());

    // adjust the font size!...
    Dimension text(font_dimension_, height.absolute(), 10);
    font_size_ = text.absolute();

    layout_->frame(XmlBasicNodeAttributes::blanking_, XmlBasicNodeAttributes::border_,
                   *XmlBasicNodeAttributes::border_colour_, XmlBasicNodeAttributes::border_style_,
                   XmlBasicNodeAttributes::border_thickness_, Colour("white"));
    layout_->display(XmlBasicNodeAttributes::display_);
    if (!lines_.empty() && composition_ == "automatic_text_only")
        composition_ = "user_text_only";
}

void FortranPositionalLegendVisitor::getReady() {
    MagLog::dev() << "FortranLegendVisitor::getReady()" << endl;
    LegendVisitor::getReady();
    if (box_x_ != -1)
        layout_->x(box_x_ / BasicPositionalObject::absoluteWidth() * 100);
    if (box_y_ != -1)
        layout_->y(box_y_ / BasicPositionalObject::absoluteHeight() * 100);
    if (box_width_ != -1)
        layout_->width(box_width_ / BasicPositionalObject::absoluteWidth() * 100);
    if (box_height_ != -1)
        layout_->height(box_height_ / BasicPositionalObject::absoluteHeight() * 100);

    // adjust the font size!...
    Dimension text(font_dimension_, box_height_, 10);
    font_size_ = text.absolute();
    layout_->Layout::frame(blanking_, border_, *border_colour_, border_line_style_, border_thickness_, Colour("white"));
}

void LegendVisitor::finish(BasicGraphicsObjectContainer& parent) {
    newLayout();
    parent.push_back(current_);

    build();

    clear();
    // parent.push_back(current_);
}

void FortranAutomaticLegendVisitor::getReady() {
    MagLog::dev() << "FortranAutomaticTextNode::getReady()" << endl;
    LegendVisitor::getReady();
    Dimension text(font_dimension_, 1, 10);
    font_size_ = text.absolute();
    layout_->Layout::frame(blanking_, border_, *border_colour_, border_line_style_, border_thickness_, Colour("white"));
    // Special setting for automatic legend

    if (top()) {
        view_x_     = box_margin_;
        view_width_ = 100 - (2 * box_margin_);
    }
    else {
        view_y_      = box_margin_;
        view_height_ = 100 - (2 * box_margin_);
    }
}

void LegendEntry::set(const LegendVisitor& attributes) {
    format_ = attributes.format();
}

void SimpleSymbolEntry::set(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    symbol_->setHeight(symbol_->getHeight() * factor_);

    symbol_->push_back(centreSymbolBox(point));
    legend.push_back(symbol_);
}

const string& LegendEntry::label() const {
    if (!label_.empty() || !fromto_)
        return label_;
    if (userMin_) {
        label_ = minText_;
        return label_;
    }
    if (userMax_) {
        label_ = maxText_;
        return label_;
    }

    if (from_ == to_) {
        ostringstream nice;
        nice << MagicsFormat(format_, from_);
        label_ = nice.str();
        return label_;
    }
    ostringstream nice;
    nice << MagicsFormat(format_, from_) << "-" << MagicsFormat(format_, to_);
    label_ = nice.str();
    return label_;
}
void SimpleSymbolEntry::rowBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    Polyline* box                  = new Polyline();
    FillShadingProperties* shading = new FillShadingProperties();
    box->setFillColour(colour());
    box->setShading(shading);
    box->setFilled(true);
    double width  = 1;
    double height = 0.4;
    double x      = point.x();
    double y      = point.y();

    if (text_) {
        Text* from = new Text();
        from->push_back(PaperPoint(x - width, y - height - 0.25));
        from->setVerticalAlign(VerticalAlign::BOTTOM);
        from->setAngle(angle_);
        legend.push_back(from);
        if (userText_.empty() || last_) {
            ostringstream bottom;
            bottom << MagicsFormat(format_, from_);
            from->addText(bottom.str(), font_);
        }
        else
            from->addText(userText_, font_);
    }
    if (last_) {
        Text* to = new Text();
        to->setAngle(angle_);
        to->setVerticalAlign(VerticalAlign::BOTTOM);
        to->push_back(PaperPoint(x + width, y - height - 0.25));
        legend.push_back(to);
        if (userText_.empty()) {
            ostringstream top, bottom;
            top << MagicsFormat(format_, to_);
            to->addText(top.str(), font_);
        }
        else
            to->addText(userText_, font_);
    }

    box->push_back(PaperPoint(x - width, y - height));
    box->push_back(PaperPoint(x - width, y + height + height));
    box->push_back(PaperPoint(x + width, y + height + height));
    box->push_back(PaperPoint(x + width, y - height));
    box->push_back(PaperPoint(x - width, y - height));
    // Small check
    Colour colour = (borderColour_.automatic()) ? box->getFillColour() : borderColour_;

    if (box->getFillColour() == Colour("none")) {
        box->setFilled(false);
        colour = Colour("black");
    }
    box->setColour(colour);

    legend.push_back(box);
}
void SimpleSymbolEntry::columnBox(const PaperPoint& point, BasicGraphicsObjectContainer& legend) {
    Polyline* box                  = new Polyline();
    FillShadingProperties* shading = new FillShadingProperties();
    box->setFillColour(colour());
    box->setShading(shading);
    box->setFilled(true);
    MagLog::debug() << "BoxEntry--->set at " << point << endl;
    double width  = computeWidth(0.8) / 2;
    double height = 0.5;
    PaperPoint p  = centreSymbolBox(point);
    double x      = p.x();
    double y      = p.y();
    PaperPoint pt = leftTextBox(point);

    if (text_) {
        Text* from = new Text();
        from->setJustification(Justification::LEFT);
        from->setVerticalAlign(VerticalAlign::HALF);
        if (userText_.empty() || last_) {
            ostringstream bottom;
            bottom << MagicsFormat(format_, from_);
            from->addText(bottom.str(), font_);
        }
        else
            from->addText(userText_, font_);
        PaperPoint pfrom(pt);
        pfrom.y_ = y - height;
        from->push_back(pfrom);
        from->setAngle(angle_);
        legend.push_back(from);
    }
    if (last_) {
        Text* to = new Text();
        to->setVerticalAlign(VerticalAlign::HALF);
        to->setJustification(Justification::LEFT);
        to->setAngle(angle_);
        if (userText_.empty()) {
            ostringstream top, bottom;
            top << MagicsFormat(format_, to_);
            to->addText(top.str(), font_);
        }
        else
            to->addText(userText_, font_);
        PaperPoint pto(pt);
        pto.y_ = y + height;
        to->push_back(pto);
        legend.push_back(to);
    }
    box->push_back(PaperPoint(x - width, y - height));
    box->push_back(PaperPoint(x - width, y + height));
    box->push_back(PaperPoint(x + width, y + height));
    box->push_back(PaperPoint(x + width, y - height));
    box->push_back(PaperPoint(x - width, y - height));
    Colour colour = (borderColour_.automatic()) ? box->getFillColour() : borderColour_;
    if (box->getFillColour() == Colour("none")) {
        box->setFilled(false);
    }
    box->setColour(colour);
    legend.push_back(box);
}

void LegendVisitor::visit(MetaDataVisitor& visitor) {
    int i = 0;
    ostringstream out;
    out << "{";
    string c1 = "";
    
    for (auto& entry : legendInfo_) {
        out << c1 << "\"" << entry.first << "\":\"" << entry.second << "\"";
        c1 = ",";
    }


    out << c1 << "\"legend_entries\" : [";
    c1 = "";
    for (auto& entry : legendEntriesInfo_) {
        out << c1 << "{";
        c1        = ",";
        string c2 = "";
        for (auto& info : entry) {
            out << c2 << "\"" << info.first << "\":\"" << info.second << "\"";
            c2 = ",";
        }
        out << "}";
    }
    out << "]";

    out << "}";

    visitor.add("legend", out.str());
}
void LegendVisitor::addLegendInfo(const string& key, const string& value) {
    legendEntriesInfo_.back().insert(make_pair(key, value));
}

SymbolEntry::~SymbolEntry() {}          // { delete symbol_; }
FlagEntry::~FlagEntry() {}              //{ delete flag_; }
BoxEntry::~BoxEntry() {}                //{ delete box_; }
ArrowEntry::~ArrowEntry() {}            //{ delete arrow_; }
DoubleLineEntry::~DoubleLineEntry() {}  //{ { delete line1_; delete line2_; }
LineEntry::~LineEntry() {}
CdfEntry::~CdfEntry() {}          //{ delete line_;}
RainbowEntry::~RainbowEntry() {}  //{ delete line_;}

