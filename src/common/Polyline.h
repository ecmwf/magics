/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Polyline.h
    \brief Definition of polyline graphics class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

*/
#ifndef Polyline_H
#define Polyline_H

#include "magics.h"

#include "Arrow.h"
#include "AutoVector.h"
#include "BaseDriver.h"
#include "BasicGraphicsObject.h"
#include "Label.h"


namespace magics {

struct ShadingProperties {
    ShadingProperties() {}
    virtual ~ShadingProperties() {}
    virtual ShadingProperties* clone()                = 0;
    virtual void draw(const BaseDriver& driver) const = 0;

    virtual void print(ostream& out) const {
        out << "ShadingProperties[";
        out << "]";
    }
    friend ostream& operator<<(ostream& s, const ShadingProperties& p) {
        p.print(s);
        return s;
    }
};

struct FillShadingProperties : public ShadingProperties {
    FillShadingProperties() {}
    ~FillShadingProperties() {}
    ShadingProperties* clone() {
        FillShadingProperties* shading = new FillShadingProperties();
        return shading;
    }
    void draw(const BaseDriver& driver) const { driver.shade(*this); }
    void print(ostream& out) const {
        out << "FillShadingProperties[";
        out << "]";
    }
};


struct DotShadingProperties : public ShadingProperties {
    DotShadingProperties() : angle_(45), size_(0.02), density_(25) {}
    ~DotShadingProperties() {}
    void draw(const BaseDriver& driver) const { driver.shade(*this); }
    double angle_;
    double size_;
    double density_;
    ShadingProperties* clone() {
        DotShadingProperties* shading = new DotShadingProperties();

        shading->size_    = size_;
        shading->density_ = density_;
        return shading;
    }

    void print(ostream& out) const {
        out << "DotShadingProperties["
            << ", angle=" << angle_ << ", size=" << size_ << ", density=" << density_ << "]";
    }
};

struct HatchShadingProperties : public ShadingProperties {
    HatchShadingProperties() : index_(0), thickness_(1), density_(18) {}
    ~HatchShadingProperties() {}
    void draw(const BaseDriver& driver) const { driver.shade(*this); }
    int index_;
    int thickness_;
    double density_;
    ShadingProperties* clone() {
        HatchShadingProperties* shading = new HatchShadingProperties();

        shading->index_     = index_;
        shading->thickness_ = thickness_;
        shading->density_   = density_;
        return shading;
    }
    void print(ostream& out) const {
        out << "HatchShadingProperties[";
        out << ", thickness=" << thickness_;
        out << ", index=" << index_;
        out << ", density=" << density_;
        out << "]";
    }
};


class PolylineProperties : public BasicGraphicsObject {
public:
    PolylineProperties() :
        thickness_(1),
        dash_length_(10),
        style_(M_SOLID),
        fill_(false),
        shading_(0),
        arrow_(0),
        stroke_(true),
        antialiasing_(true) {}
    virtual ~PolylineProperties() {
        if (shading_)
            delete shading_;
        if (arrow_)
            delete arrow_;
    }

    virtual void copy(const PolylineProperties& other) {
        label_.copy(other.label_);
        colour_       = other.colour_;
        thickness_    = other.thickness_;
        dash_length_  = other.dash_length_;
        style_        = other.style_;
        fill_         = other.fill_;
        antialiasing_ = other.antialiasing_;
        fill_colour_  = other.fill_colour_;
        stroke_       = other.stroke_;
        if (shading_)
            delete shading_;
        shading_ = other.shading_ ? other.shading_->clone() : 0;
        arrow_   = other.arrow_ ? other.arrow_->clone() : 0;
    }
    //! Method to set the thickness of a drawn line.
    void setThickness(int t) { thickness_ = t; }
    //! Method to get the thickness of a drawn line.
    int getThickness() const { return thickness_; }

    //! Method to set the length of dashes in the line.
    void setDashLength(int dl) { dash_length_ = dl; }
    //! Method to get the length of dashes in the line.
    int getDashLength() const { return dash_length_; }

    //! Method to set the style for the drawn line.
    void setLineStyle(LineStyle ls) { style_ = ls; }
    //! Method to get the style for the drawn line.
    LineStyle getLineStyle() const { return style_; }

    //! Method to set the colour for the drawn line.
    void setColour(const Colour& colour) { colour_ = colour; }
    //! Method to get the colour for the drawn line.
    const Colour& getColour() const { return colour_; }

    //! Method to set filling.
    void setFilled(bool fill) { fill_ = fill; }
    //! Method to get the style for the drawn line.
    bool isFilled() const { return fill_; }

    void setFillColour(Colour col) { fill_colour_ = col; }
    Colour getFillColour() const { return fill_colour_; }

    void setLabel(const Label& label) { label_ = label; }
    const Label& getLabel() const { return label_; }
    void setAntiAliasing(bool antialiasing) { antialiasing_ = antialiasing; }
    bool getAntiAliasing() const { return antialiasing_; }

    void setStroke(bool stroke) { stroke_ = stroke; }
    bool isStroked() const { return stroke_; }

    void setShading(ShadingProperties* shading) {
        if (shading_)
            delete shading_;
        shading_ = shading;
    }
    void setArrow(ArrowProperties* arrow) {
        if (arrow_)
            delete arrow_;
        arrow_ = arrow;
    }

    ArrowProperties* arrowProperties() const { return arrow_; }
    ShadingProperties* getShading() const { return shading_; }

protected:
    int thickness_;
    int dash_length_;
    LineStyle style_;
    bool fill_;
    Colour fill_colour_;
    ShadingProperties* shading_;
    Label label_;
    ArrowProperties* arrow_;
    Colour colour_;
    bool stroke_;
    bool antialiasing_;
};


using namespace magics;


class Polyline : public PolylineProperties {
public:
    typedef vector<deque<PaperPoint>> Holes;


    Polyline();
    ~Polyline();

    void reproject(const Transformation&);
    bool reproject(BasicGraphicsObjectContainer& out) const;
    void redisplay(const BaseDriver& driver) const;

    Colour cellColour_;
    double cellValue_;
    int index_;

    const Colour& cellColour() const { return cellColour_; }
    double cellValue() const { return cellValue_; }

    int index() const { return index_; }
    void index(int index) { index_ = index; }

    void cellColour(const Colour& colour) { cellColour_ = colour; }
    void cellValue(double value) { cellValue_ = value; }
    void cellInfo(const Colour& colour, double value) {
        cellColour_ = colour;
        cellValue_  = value;
    }

    bool concatenate(Polyline&);
    void intersection(Polyline&);
    Polyline* clone() const;
    const deque<PaperPoint>& polygon() const { return polygon_; }
    deque<PaperPoint>& polygon() { return polygon_; }
    void push_back(const PaperPoint& point) {
        if (empty()) {
            polygon_.push_back(point);
            return;
        }
        PaperPoint last = back();
        if (point.x_ != last.x_ || point.y_ != last.y_)
            polygon_.push_back(point);
    }
    void rotate(int index) { std::rotate(polygon_.begin(), polygon_.begin() + index, polygon_.end()); }

    void push_back(double x, double y) { polygon_.push_back(PaperPoint(x, y)); }
    void southClean(bool);

    void box(const PaperPoint&, const PaperPoint&);
    bool empty() const { return polygon_.empty(); }
    bool closed() const { return polygon_.front() == polygon_.back(); }
    void close() {
        if (empty())
            return;
        if (!closed())
            polygon_.push_back(polygon_.front());
    }

    auto end() const { return polygon_.end(); }
    auto begin() const { return polygon_.begin(); }
    unsigned int size() const { return polygon_.size(); }
    const PaperPoint& get(int i) const { return polygon_[i]; }
    const PaperPoint& front() const { return polygon_.front(); }
    const PaperPoint& back() const { return polygon_.back(); }
    void push_front(const PaperPoint& point) { polygon_.push_front(point); }

    void push_front(Polyline&);
    void push_back(Polyline&);

    // Is the pointincluded in the polyline"
    bool in(const PaperPoint&)const;

    void reserve(double);

    /*!
      Counts checks if a given point is in the Polyline
      This method can be used to see if a point at x_in and y_in
      lies inside a CLOSED Polyline. If counts is odd the point
      should lie inside.
    */
    bool inside(const float x_in, const float y_in) const;


    bool allMissing() const {
        deque<PaperPoint>::const_iterator p = polygon_.begin();
        while (p != polygon_.end()) {
            if (!p->missing())
                return false;
            p++;
        }
        return true;
    }

    bool someMissing() const {
        deque<PaperPoint>::const_iterator p = polygon_.begin();
        while (p != polygon_.end()) {
            if (p->missing())
                return true;
            p++;
        }
        return false;
    }

    bool within(const PaperPoint& point) const;
    void clip(const Polyline& poly, vector<Polyline*>&) const;
    void intersect(const Polyline& poly, vector<Polyline*>&) const;


    // Polyline* simplify(double);
    Polyline* getNew() const;


    // Holes
    void newHole(const Polyline&);
    void newHole();
    void push_back_hole(const PaperPoint& point);


    void hole(Holes::const_iterator, Polyline&) const;
    Holes::const_iterator beginHoles() const;
    Holes::const_iterator endHoles() const;

    void hole(Holes::const_iterator, vector<double>&, vector<double>&) const;
    unsigned int numberOfHoles() const { return holes_.size(); }
    void clearHoles() { holes_.clear(); }
    void clear() { polygon_.clear(); }
    Holes& holes();

protected:
    void print(ostream&) const;


public:
    deque<PaperPoint> polygon_;
    Holes holes_;


private:
    // Polyline(const Polyline&);
    //
    // -- Friends
    friend ostream& operator<<(ostream& s, const Polyline& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics


#endif
