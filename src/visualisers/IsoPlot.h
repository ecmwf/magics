/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IsoPlot.h
    \brief Definition of the Template class IsoPlot.

    Magics Team - ECMWF 2004

    Started: Wed 3-Mar-2004

    Changes:

*/

#ifndef IsoPlot_H
#define IsoPlot_H

#include "magics.h"

#include <limits>
#include "AutoVector.h"
#include "BasicSceneObject.h"
#include "IsoHelper.h"
#include "IsoPlotAttributes.h"
#include "Polyline.h"
#include "SegmentJoiner.h"
#include "Timer.h"
#include "XmlNode.h"

namespace magics {


class Cell;

class CellArray : public AutoVector<Cell> {
public:
    CellArray(MatrixHandler& data, IntervalMap<int>& range, const Transformation& transformation, int, int, float,
              const string&);
    CellArray(MatrixHandler& data, IntervalMap<int>& range);
    int columns_;
    int rows_;
    double missing_;

    Matrix points_;
    IntervalMap<int> rangeFinder_;
    MatrixHandler& data_;

    Cell* operator()(int row, int column) const { return (*this)[row * columns_ + column].get(); }

    double rows() const { return points_.rows(); }
    double columns() const { return points_.columns(); }
    double value(const pair<int, int>& pos) const { return points_(pos.first, pos.second); }
    double value(int row, int column) const { return points_(row, column); }
    double row(int row, int column) const { return points_.row(row, column); }
    double row(const pair<int, int>& pos) const { return points_.row(pos.first, pos.second); }

    double column(const pair<int, int>& pos) const { return points_.column(pos.first, pos.second); }
    double column(int row, int column) const { return points_.column(row, column); }
    double range(const pair<int, int>& pos) const { return rangeFinder_.find(points_(pos.first, pos.second), -1); }

    ~CellArray() {}
};

class GridArray : public CellArray {
public:
    GridArray(MatrixHandler& data, IntervalMap<int>& range, const Transformation& transformation, int, int, float,
              const string&);
};


enum RangeType
{
    outOfRange,
    singleRange,
    multipleRange
};


class Cell {
public:
    Cell(const CellArray& parent) : parent_(parent), missing_(parent.missing_) {}
    Cell(const CellArray& parent, int row, int column) : parent_(parent), row_(row), column_(column) {
        indexes_[0] = std::make_pair(row_, column_);
        indexes_[1] = std::make_pair(row_, column_ + 1);
        indexes_[2] = std::make_pair(row_ + 1, column_ + 1);
        indexes_[3] = std::make_pair(row_ + 1, column_);

        min_        = 100000000;
        max_        = -min_;
        outOfRange_ = 0;
        missing_    = parent_.missing_;
        for (int i = 0; i < 4; i++) {
            int index = range(i);
            if (index != -1) {
                if (index < min_)
                    min_ = index;
                if (index > max_)
                    max_ = index;
            }
            else
                outOfRange_++;
        }
    }

    ~Cell() {}


    const CellArray& parent_;
    int row_;
    int column_;


    virtual RangeType range() {
        if (outOfRange_ == 4)
            return outOfRange;
        if (min_ == max_ && !outOfRange_)
            return singleRange;
        return multipleRange;
    }


    double missing_;

    bool missing(int i) { return (same(missing_, this->parent_.value(this->indexes_[i]))); }


    int min_;
    int max_;

    int outOfRange_;

    std::pair<int, int> indexes_[4];


    const pair<int, int>& index(int i) {
        ASSERT(i < 4);
        return indexes_[i];
    }

    virtual double value(int i) const { return this->parent_.value(this->indexes_[i]); }

    virtual double height(int i, double val) const {
        double value = this->parent_.value(this->indexes_[i]);

        return (same(value, missing_)) ? 0 : value - val;
    }
    virtual int coef(int i, double val) const {
        double height = this->parent_.value(this->indexes_[i]) - val;
        int c         = (height && !std::isnan(height)) ? (height / abs(height)) + 1 : 1;


        return c;
    }

    bool isMissing(int i) const { return (same(missing_, this->parent_.value(this->indexes_[i]))); }

    virtual double column(int i) const { return parent_.column(this->indexes_[i]); }
    void xysect(int i, int j, double value, double& x, double& y) const {
        double v1 = this->parent_.value(this->indexes_[i]) - value;
        double v2 = this->parent_.value(this->indexes_[j]) - value;

        double x1 = this->parent_.column(this->indexes_[i]);
        double x2 = this->parent_.column(this->indexes_[j]);
        double y1 = this->parent_.row(this->indexes_[i]);
        double y2 = this->parent_.row(this->indexes_[j]);
        x         = (v2 * x1 - v1 * x2) / (v2 - v1);
        y         = (v2 * y1 - v1 * y2) / (v2 - v1);
    }

    virtual double row(int i) const { return this->parent_.row(this->indexes_[i]); }
    virtual int range(int i) { return this->parent_.range(this->indexes_[i]); }
    int findRange(double value) { return this->parent_.rangeFinder_.find(value, -1); }
};


class GridCell : public Cell {
public:
    GridCell(const CellArray&, int row, int column, const Transformation& transformation, const string&);

    ~GridCell() {}

    const Transformation& transformation_;
    double columns_[4];
    double rows_[4];
    int range_;

    RangeType range() { return (range_ == -1) ? outOfRange : singleRange; }


    double missing_;
    double value_;


    bool missing(int i) { return (same(missing_, this->parent_.value(this->indexes_[i]))); }


    virtual double value(int i) const { return value_; }

    virtual double row(int i) const { return rows_[i]; }
    virtual double column(int i) const { return columns_[i]; }
    const pair<int, int>& index(int i) {
        ASSERT(i < 4);
        return indexes_[i];
    }

    bool isMissing(int i) const { return (same(missing_, this->parent_.value(this->indexes_[i]))); }


    int range(int i) const { return range_; }

    int findRange(double value) { return this->parent_.rangeFinder_.find(value, -1); }
};


class CellBox;


class IsoPlot : public IsoPlotAttributes, public ColourTechniqueInterface {
public:
    IsoPlot();
    virtual ~IsoPlot();

    // Implements the Visdef Interface...
    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&);
    virtual void visit(Data&, LegendVisitor&);

    void set(const map<string, string>& map) { IsoPlotAttributes::set(map); }

    void set(const XmlNode& node) { IsoPlotAttributes::set(node); }

    void toxml(ostream& out) const { IsoPlotAttributes::toxml(out); }

    void setTag(const string&) {
        // IsoPlotAttributes::setTag(tag);
    }
    // implemts colourtechnique interface for the rainbow method ...
    const Colour& getMinColour() const { return *rainbowMinColour_; }
    const Colour& getMaxColour() const { return *rainbowMaxColour_; }
    const string& getDirection() const { return rainbowDirection_; }
    stringarray getColours() const { return rainbowColours_; }
    ListPolicy getPolicy() const { return rainbowColourPolicy_; }

    const Colour& rainbow(double value);

    void adjust(double min, double max) {
        min_ = min;
        max_ = max;
    }

    virtual IsoPlot* clone() const {
        IsoPlot* object = new IsoPlot();
        object->copy(*this);
        return object;
    }

    int shadingIndex(double value) const { return this->shading_->shadingIndex(value); }


    void isoline(Cell&, CellBox* = 0) const;
    virtual bool needIsolines() const { return true; }
    void operator()(Data& data, BasicGraphicsObjectContainer& parent) { (*this->shading_)(data, parent); }
    virtual void visit(Data&, PointsHandler&, HistoVisitor&);
    virtual bool method(ContourMethod* method) { return (*this->shading_).method(method); }
    void isoline(MatrixHandler&, BasicGraphicsObjectContainer&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


    bool prepare(MatrixHandler&);

    double min_;
    double max_;

    vector<vector<Polyline*>*> lines_;

    double missing_;
    vector<double> levels_;
    bool shadingMode_;

    vector<IsoData*> segments_;

    map<Colour, IsoData*> colourShapes_;
    int threads_;

    map<double, int> thickness_list_;
    map<double, LineStyle> line_style_list_;

    void setThicknessAndStyle();

    int thickness(double);
    LineStyle line_style(double);


private:
    //! Copy constructor - No copy allowed
    IsoPlot(const IsoPlot&);
    //! Overloaded << operator to copy - No copy allowed
    IsoPlot& operator=(const IsoPlot&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const IsoPlot& p) {
        p.print(s);
        return s;
    }
};


class NoIsoPlot : public IsoPlot {
public:
    NoIsoPlot() { this->setTag("noisoline"); };
    ~NoIsoPlot(){};

    // Implements the Visualiser Interface...
    void operator()(MatrixHandler&, BasicGraphicsObjectContainer&);

    void set(const XmlNode& node) {
        if (magCompare(node.name(), "noisoline")) {
            XmlNode iso = node;
            iso.name("isoline");
            IsoPlotAttributes::set(iso);
        }
        else
            IsoPlotAttributes::set(node);
    }

    IsoPlot* clone() const {
        IsoPlot* object = new NoIsoPlot();
        return object;
    }
    bool needIsolines() { return this->label_->label(); }
    void visit(Data&, LegendVisitor&);
    bool method(ContourMethod*) { return false; }

protected:
    void print(ostream& out) const {
        out << "NoIsoPlot"
            << "\n";
    }
};


template <>
class MagTranslator<string, IsoPlot> {
public:
    IsoPlot* operator()(const string& val) { return SimpleObjectMaker<IsoPlot>::create(val); }

    IsoPlot* magics(const string& param) {
        IsoPlot* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics


#endif
