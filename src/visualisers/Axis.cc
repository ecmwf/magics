/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Axis.cc
    \brief Implementation of the Template class Axis.

    Magics Team - ECMWF 2004

    Started: Fri 7-May-2004

    Changes:

 */

#include "Axis.h"
#include "Factory.h"
#include "Layout.h"
#include "MagLog.h"

#include "Layer.h"
#include "Polyline.h"
#include "SceneVisitor.h"
#include "Text.h"

using namespace magics;

Axis::Axis() : currentLabel_(0) {
    labelHelpers_["number"]     = &Axis::number;
    labelHelpers_["label_list"] = &Axis::labellist;
    labelHelpers_["latitude"]   = &Axis::latitude;
    labelHelpers_["longitude"]  = &Axis::longitude;
    labelHelpers_["date"]       = &Axis::basic;
    labelHelpers_["geoline"]    = &Axis::basic;
    title_position_             = -1;
}

Axis::~Axis() {}

/*!
 Class information are given to the output-stream.
 */
void Axis::print(ostream& out) const {
    out << "Axis[";
    AxisAttributes::print(out);
    out << "]";
}

VerticalAxis::VerticalAxis() {
    if (!magCompare(position_, "left") && !magCompare(position_, "right"))
        position_ = "left";
}

HorizontalAxis::HorizontalAxis() {
    if (!magCompare(position_, "bottom") && !magCompare(position_, "top"))
        position_ = "bottom";
}


void Axis::ticks(double min, double max, vector<double>& ticks) {
    const double step  = (max - min) / 10.;
    const double log   = log10(step);
    const double istep = pow(10., int(log));
    const double inc   = ceil(step / istep) * istep;
    double first       = floor(min / inc) * inc;

    if (first > min)
        first -= inc;

    for (double val = first; val <= max; val += inc) {
        ticks.push_back(val);
    }
    ticks.push_back(ticks.back() + inc);
}

void Axis::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors) {
    // First we create the layer!
    // and push It to the parent layer!
    StaticLayer* axis = new NoDataLayer(this);


    axis->icon(*this);

    layer.add(axis);

    for (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
        axis->set(*visitor);
        (*visitor)->visit(*this);
    }
}

void Axis::visit(TextVisitor&) {
    // Nothing to put in the title
}


void VerticalAxis::tick(VerticalAxisVisitor& axis) {
    if (!tick_)
        return;

    double x1, x2;

    axis.tick(x1, x2, magCompare(tick_position_, "out"));

    const Transformation& transformation = axis.transformation();

    for (AxisItems::const_iterator y = items_.begin(); y != items_.end(); ++y) {
        if (!(*y)->isTick())
            continue;
        if (!transformation.inY((*y)->position()))
            continue;

        Polyline* tick = new Polyline();
        PaperPoint p1  = PaperPoint(x1, transformation.y((*y)->position()));
        PaperPoint p2  = PaperPoint(x2, transformation.y((*y)->position()));
        tick->push_back(p1);
        tick->push_back(p2);
        Colour calcol = (tick_colour_->automatic()) ? *line_colour_ : *tick_colour_;
        tick->setColour(calcol);
        tick->setThickness(tick_thickness_);
        axis.push_back(tick);
    }
}

void HorizontalAxis::tick(HorizontalAxisVisitor& axis) {
    if (!tick_)
        return;

    double y1, y2;
    axis.tick(y1, y2, magCompare(tick_position_, "out"));

    const Transformation& transformation = axis.transformation();

    for (AxisItems::const_iterator x = items_.begin(); x != items_.end(); ++x) {
        if (!(*x)->isTick())
            continue;
        if (!transformation.inX((*x)->position()))
            continue;
        Polyline* tick = new Polyline();
        double px      = transformation.x((*x)->position());
        tick->push_back(PaperPoint(px, y1));
        tick->push_back(PaperPoint(px, y2));
        Colour calcol = (tick_colour_->automatic()) ? *line_colour_ : *tick_colour_;
        tick->setColour(calcol);
        tick->setThickness(tick_thickness_);
        axis.push_back(tick);
    }
}
void HorizontalAxis::line(TopAxisVisitor& out) const {
    if (!line_)
        return;
    PaperPoint from(out.minX(), out.minY());
    PaperPoint to(out.maxX(), out.minY());

    Polyline* axis = new Polyline();

    axis->setColour(*line_colour_);
    axis->setThickness(line_thickness_);
    axis->setLineStyle(line_style_);
    axis->push_back(from);
    axis->push_back(to);
    out.push_back(axis);
}

void HorizontalAxis::line(BottomAxisVisitor& out) const {
    if (!line_)
        return;
    PaperPoint from(out.minX(), out.maxY());
    PaperPoint to(out.maxX(), out.maxY());

    Polyline* axis = new Polyline();

    axis->setColour(*line_colour_);
    axis->setThickness(line_thickness_);
    axis->setLineStyle(line_style_);
    axis->push_back(from);
    axis->push_back(to);
    out.push_back(axis);
}


void VerticalAxis::line(LeftAxisVisitor& out) const {
    if (!line_)
        return;
    PaperPoint from(out.maxX(), out.minY());
    PaperPoint to(out.maxX(), out.maxY());

    Polyline* axis = new Polyline();

    axis->setColour(*line_colour_);
    axis->setThickness(line_thickness_);
    axis->setLineStyle(line_style_);
    axis->push_back(from);
    axis->push_back(to);
    out.push_back(axis);
}

void VerticalAxis::line(RightAxisVisitor& out) const {
    if (!line_)
        return;
    PaperPoint from(out.minX(), out.minY());
    PaperPoint to(out.minX(), out.maxY());

    Polyline* axis = new Polyline();

    axis->setColour(*line_colour_);
    axis->setThickness(line_thickness_);
    axis->setLineStyle(line_style_);
    axis->push_back(from);
    axis->push_back(to);
    out.push_back(axis);
}

void HorizontalAxis::label(HorizontalAxisVisitor& axis) {
    if (!label_)
        return;
    Justification justification          = axis.justificationTickLabel(label_orientation_);
    const Transformation& transformation = axis.transformation();
    double angle                         = (magCompare(label_orientation_, "horizontal")) ? 0 : -1.57;
    int count                            = -1;
    map<int, double> positions;
    double y1, y2;
    axis.tick(y1, y2, magCompare(tick_position_, "out"));
    positions[-1]   = y1;
    title_position_ = y1;
    for (AxisItems::const_iterator x = items_.begin(); x != items_.end(); ++x) {
        if (!transformation.inX((*x)->position()))
            continue;
        if ((*x)->isLabel() == false)
            continue;
        if ((*x)->isFirst() && !label_first_)
            continue;
        if ((*x)->isLast() && !label_last_)
            continue;
        string label = createLabel(**x);
        if (label.empty())
            continue;
        count++;
        if (count % label_frequency_)
            continue;

        double height = ((*x)->height() == DBL_MIN || (*x)->height() == 0) ? label_height_ : (*x)->height();
        double pos    = axis.offsetTickLabel(height, (*x)->level());

        map<int, double>::iterator p = positions.find((*x)->level());

        if (p != positions.end()) {
            pos = p->second;
        }
        else {
            p                        = positions.find((*x)->level() - 1);
            pos                      = axis.offsetTickLabel(height, p->second);
            positions[(*x)->level()] = pos;

            title_position_ = pos;
        }

        PaperPoint point(transformation.x((*x)->position()), pos);

        bool out = false;
        if (magCompare(label_position_, "inter_tick")) {
            AxisItems::const_iterator next = x;
            next++;
            while (next != items_.end() && !(*next)->isLabel())
                next++;
            out = true;
            if (next != items_.end() && (*x)->level() == (*next)->level()) {
                // string nlabel = createLabel(**next);
                const double x1 = transformation.x((*x)->position());
                const double x2 = transformation.x((*next)->position());
                point           = PaperPoint((x2 + x1) / 2, pos);
                out             = (!transformation.inX((x2 + x1) / 2));
            }
        }


        if (out)
            continue;
        Text* text = new Text();

        Colour newcolour;
        if ((*x)->colour() != "undef")
            newcolour = Colour((*x)->colour());
        else
            newcolour = (label_colour_->automatic()) ? *line_colour_ : *label_colour_;


        MagFont font(label_font_);
        font.colour(newcolour);
        font.style(label_font_style_);
        font.size(height);
        (*x)->setFont(font);
        text->setFont(font);

        text->setText(label);
        text->setJustification(justification);
        text->setVerticalAlign(MBOTTOM);
        text->setAngle(angle);

        text->push_back(point);
        axis.push_back(text);
    }
}


void VerticalAxis::label(VerticalAxisVisitor& axis) {
    if (!label_)
        return;
    Justification justification          = axis.justificationTickLabel(label_orientation_);
    const Transformation& transformation = axis.transformation();

    int count = -1;
    map<int, double> positions;
    double x1, x2;
    axis.tick(x1, x2, magCompare(tick_position_, "out"));
    x1              = axis.offsetTickLabel(0.025, x1);
    positions[-1]   = x1;
    title_position_ = x1;


    PaperPoint point;

    for (AxisItems::const_iterator y = items_.begin(); y != items_.end(); ++y) {
        if ((*y)->isLabel() == false)
            continue;

        string label = createLabel(**y);

        if (!transformation.inY((*y)->position()))
            continue;

        if ((*y)->isFirst() && !label_first_)
            continue;
        if ((*y)->isLast() && !label_last_)
            continue;


        if (label.empty())
            continue;
        count++;
        if (count % label_frequency_)
            continue;


        double height = ((*y)->height() == DBL_MIN || (*y)->height() == 0) ? label_height_ : (*y)->height();
        double pos;
        map<int, double>::iterator p = positions.find((*y)->level());

        if (p != positions.end()) {
            pos = p->second - axis.offsetTickLabel(height, 0);
            ;
        }
        else {
            p                        = positions.find((*y)->level() - 1);
            pos                      = p->second;
            double newpos            = axis.offsetTickLabel(height, p->second);
            positions[(*y)->level()] = newpos;
        }

        double tpos = axis.offsetTickLabel(height * label.size(), p->second);
        if (tpos < title_position_)
            title_position_ = tpos;

        PaperPoint point(pos, transformation.y((*y)->position()));

        bool out = false;


        if (magCompare(label_position_, "inter_tick")) {
            AxisItems::const_iterator next = y;
            next++;
            while (next != items_.end() && !(*next)->isLabel())
                next++;
            out = true;
            if (next != items_.end() && (*y)->level() == (*next)->level()) {
                const double y1 = transformation.y((*y)->position());
                const double y2 = transformation.y((*next)->position());
                point           = PaperPoint(pos, (y2 + y1) / 2);
                out             = !transformation.inY((y2 + y1) / 2);
            }
        }


        if (out)
            continue;
        Text* text = new Text();
        text->setJustification(justification);
        Colour newcolour = (label_colour_->automatic()) ? *line_colour_ : *label_colour_;

        MagFont font(label_font_);
        font.colour(newcolour);
        font.style(label_font_style_);
        font.size(height);
        (*y)->setFont(font);
        text->setFont(font);
        text->setText(label);
        text->setJustification(justification);
        text->setVerticalAlign(MHALF);
        text->push_back(point);
        axis.push_back(text);
    }
}

string Axis::number(const AxisItem& in) {
    return in.label();
}

string Axis::labellist(const AxisItem& in) {
    if (label_labels_.empty())
        return in.label();


    if (currentLabel_ < label_labels_.size()) {
        string label = label_labels_[currentLabel_];
        currentLabel_++;

        return label;
    }
    else
        return label_labels_.empty() ? in.label() : label_labels_.back();
}

string Axis::latitude(const AxisItem& item) {
    ostringstream lat;
    string ns = "&#176;";
    if (item.position() < 0)
        ns += "S";
    if (item.position() >= 0)
        ns += "N";
    float y = float(maground(abs(item.position()) * 100)) / 100;
    lat << y << ns;
    return lat.str();
}

string Axis::longitude(const AxisItem& item) {
    ostringstream lon;
    string ew = "&#176;";
    if (item.position() < 0)
        ew += "W";
    if (item.position() >= 0)
        ew += "E";
    float x = float(maground(abs(item.position()) * 100)) / 100;
    lon << x << ew;
    return lon.str();
}
string Axis::basic(const AxisItem& item) {
    return item.label();
}

string Axis::createLabel(const AxisItem& in) {
    MagLog::dev() << "Axis::createLabel " << in << " with method : " << label_type_ << endl;

    map<string, LabelHelper>::iterator helper = labelHelpers_.find(lowerCase(label_type_));
    if (helper != labelHelpers_.end())
        return (this->*helper->second)(in);
    else {
        MagLog::warning() << "Could not the method " << label_type_ << " to setup axis labels..." << endl;
        return in.label();
    }
}

void HorizontalAxis::title(HorizontalAxisVisitor& out) {
    if (!title_)
        return;
    Text* text = new Text();

    MagFont font(title_font_, title_font_style_, title_height_);
    font.colour((title_colour_->automatic()) ? *line_colour_ : *title_colour_);
    text->setVerticalAlign(out.textAlignment("horizontal"));
    text->setFont(font);
    text->setText(title_text_);

    double x = (out.minX() + out.maxX()) / 2;
    text->push_back(PaperPoint(x, out.offsetTickLabel(title_height_, title_position_)));

    out.push_back(text);
}

void VerticalAxis::title(VerticalAxisVisitor& out) {
    if (!title_)
        return;
    out.frameIt();
    double angle = out.angleTitle();
    double x     = title_position_;

    Text* text = new Text();
    MagFont font(title_font_, title_font_style_, title_height_);
    font.colour((title_colour_->automatic()) ? *line_colour_ : *title_colour_);
    text->setAngle((title_orientation_ == "horizontal") ? 0 : angle);
    text->setFont(font);
    text->setText(title_text_);

    double y = (out.minY() + out.maxY()) / 2;
    text->push_back(PaperPoint(x, y));

    out.push_back(text);
}

void HorizontalAxis::grid(DrawingVisitor& out) const {
    double bottom = out.minY();
    double top    = out.maxY();
    double pos;
    const Transformation& transformation = out.transformation();

    if (!grid_) {
        if (grid_reference_level_ != INT_MAX) {
            for (AxisItems::const_iterator x = items_.begin(); x != items_.end(); ++x) {
                pos = (*x)->position();
                if (pos == grid_reference_level_) {
                    Polyline* grid = new Polyline();
                    grid->push_back(PaperPoint(transformation.x(pos), bottom));
                    grid->push_back(PaperPoint(transformation.x(pos), top));
                    Colour colour = !grid_reference_colour_->automatic() ? *grid_reference_colour_ : *grid_colour_;
                    grid->setColour(colour);
                    grid->setLineStyle(grid_reference_style_);
                    grid->setThickness(grid_reference_thickness_);
                    out.push_back(grid);
                    return;
                }
            }
            return;
        }
        else
            return;
    }


    for (AxisItems::const_iterator x = items_.begin(); x != items_.end(); ++x) {
        pos = (*x)->position();

        if (minor_tick_ && minor_grid_ && (*x)->isMinorTick()) {
            if (!transformation.inX(pos))
                continue;

            Polyline* grid = new Polyline();
            grid->push_back(PaperPoint(transformation.x(pos), bottom));
            grid->push_back(PaperPoint(transformation.x(pos), top));
            grid->setColour(*minor_grid_colour_);
            grid->setLineStyle(minor_grid_style_);
            grid->setThickness(minor_grid_thickness_);
            out.push_back(grid);

            continue;
        }
        if (!(*x)->isGrid())
            continue;

        if (!transformation.inX(pos))
            continue;
        Polyline* grid = new Polyline();
        grid->push_back(PaperPoint(transformation.x(pos), bottom));
        grid->push_back(PaperPoint(transformation.x(pos), top));
        if (pos == grid_reference_level_) {
            Colour colour = !grid_reference_colour_->automatic() ? *grid_reference_colour_ : *grid_colour_;
            grid->setColour(colour);
            grid->setLineStyle(grid_reference_style_);
            grid->setThickness(grid_reference_thickness_);
        }
        else {
            grid->setColour(*grid_colour_);
            grid->setLineStyle(grid_style_);
            grid->setThickness(grid_thickness_);
        }
        out.push_back(grid);
    }
}

void VerticalAxis::grid(DrawingVisitor& out) const {
    double left  = out.minX();
    double right = out.maxX();
    double pos;
    const Transformation& transformation = out.transformation();

    if (!grid_) {
        if (grid_reference_level_ != INT_MAX) {
            for (AxisItems::const_iterator y = items_.begin(); y != items_.end(); ++y) {
                pos = (*y)->position();
                if (pos == grid_reference_level_) {
                    Polyline* grid = new Polyline();
                    grid->push_back(PaperPoint(left, transformation.y(pos)));
                    grid->push_back(PaperPoint(right, transformation.y(pos)));
                    Colour colour = !grid_reference_colour_->automatic() ? *grid_reference_colour_ : *grid_colour_;
                    grid->setColour(colour);
                    grid->setLineStyle(grid_reference_style_);
                    grid->setThickness(grid_reference_thickness_);
                    out.push_back(grid);
                    return;
                }
            }
            return;
        }
        else
            return;
    }


    for (AxisItems::const_iterator y = items_.begin(); y != items_.end(); ++y) {
        pos = (*y)->position();
        if (minor_tick_ && minor_grid_ && (*y)->isMinorTick()) {
            if (!transformation.inY(pos))
                continue;

            Polyline* grid = new Polyline();
            grid->push_back(PaperPoint(left, transformation.y(pos)));
            grid->push_back(PaperPoint(right, transformation.y(pos)));
            grid->setColour(*minor_grid_colour_);
            grid->setLineStyle(minor_grid_style_);
            grid->setThickness(minor_grid_thickness_);
            out.push_back(grid);
            continue;
        }
        if (!(*y)->isGrid())
            continue;

        if (!transformation.inY(pos))
            continue;
        Polyline* grid = new Polyline();
        grid->push_back(PaperPoint(left, transformation.y(pos)));
        grid->push_back(PaperPoint(right, transformation.y(pos)));
        if (pos == grid_reference_level_) {
            Colour colour = !grid_reference_colour_->automatic() ? *grid_reference_colour_ : *grid_colour_;
            grid->setColour(colour);
            grid->setLineStyle(grid_reference_style_);
            grid->setThickness(grid_reference_thickness_);
        }
        else {
            grid->setColour(*grid_colour_);
            grid->setLineStyle(grid_style_);
            grid->setThickness(grid_thickness_);
        }
        out.push_back(grid);
    }
}

void VerticalAxis::minortick(VerticalAxisVisitor& axis) {
    if (!minor_tick_)
        return;
    double x1, x2;
    axis.minortick(x1, x2, magCompare(tick_position_, "out"));

    const Transformation& projection = axis.transformation();

    for (AxisItems::const_iterator y = items_.begin(); y != items_.end(); ++y) {
        if (!(*y)->isMinorTick())
            continue;
        if (!projection.inY((*y)->position()))
            continue;
        Polyline* tick = new Polyline();
        tick->push_back(PaperPoint(x1, (*y)->position()));
        tick->push_back(PaperPoint(x2, (*y)->position()));
        Colour calcol = (minor_tick_colour_->automatic()) ? *tick_colour_ : *minor_tick_colour_;
        tick->setColour(calcol);
        tick->setThickness(minor_tick_thickness_);
        axis.push_back(tick);
    }
}

void HorizontalAxis::minortick(HorizontalAxisVisitor& axis) {
    if (!minor_tick_)
        return;
    double y1, y2;
    axis.minortick(y1, y2, magCompare(tick_position_, "out"));
    const Transformation& projection = axis.transformation();
    for (AxisItems::const_iterator x = items_.begin(); x != items_.end(); ++x) {
        if (!(*x)->isMinorTick())
            continue;
        if (!projection.inX((*x)->position()))
            continue;

        Polyline* tick = new Polyline();

        tick->push_back(PaperPoint((*x)->position(), y1));
        tick->push_back(PaperPoint((*x)->position(), y2));
        Colour calcol = (minor_tick_colour_->automatic()) ? *tick_colour_ : *minor_tick_colour_;
        tick->setColour(calcol);
        tick->setThickness(minor_tick_thickness_);
        axis.push_back(tick);
    }
}


void HorizontalAxis::tip(TopAxisVisitor& out) const {}

void HorizontalAxis::tip(BottomAxisVisitor& out) const {
    if (!tip_)
        return;
    Text* text = new Text();

    MagFont font;
    font.size(tip_height_);
    font.colour((tip_colour_->automatic()) ? *label_colour_ : *tip_colour_);
    text->setFont(font);
    text->setText(tip_text_);
    text->setVerticalAlign(MTOP);

    double x = out.maxX() - (out.maxX() - out.minX()) * 0.05;

    text->push_back(PaperPoint(x, title_position_));

    out.push_back(text);
}

void VerticalAxis::tip(LeftAxisVisitor& out) const {
    if (!tip_)
        return;
    double angle   = out.angleTip();
    const double x = title_position_;

    Text* text = new Text();
    MagFont font;
    font.size(tip_height_);
    font.colour((tip_colour_->automatic()) ? *label_colour_ : *tip_colour_);
    text->setFont(font);
    text->setText(tip_text_);

    text->setAngle((orientation_ == "horizontal") ? 0 : angle);

    double y = out.maxY() - (out.maxY() - out.minY()) * 0.05;
    ;


    text->push_back(PaperPoint(x, y));

    out.push_back(text);
}

void VerticalAxis::tip(RightAxisVisitor& out) const {}
