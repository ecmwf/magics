/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CartesianTransformation.h
    \brief Definition of the Template class CartesianTransformation.

    Magics Team - ECMWF 2005

    Started: Mon 19-Sep-2005

    Changes:

*/

#ifndef CartesianTransformation_H
#define CartesianTransformation_H

#include "magics.h"

#include "CartesianTransformationAttributes.h"
#include "Transformation.h"

namespace magics {
class XmlNode;

class CartesianTransformation : public Transformation, public CartesianTransformationAttributes {
public:
    CartesianTransformation();
    virtual ~CartesianTransformation() override;
    virtual void operator()(Layout&) const;
    void init() override {
        x_->set();
        y_->set();
        Transformation::init();
    }
    void cleaninit() override { Transformation::init(); }
    virtual void toxml(ostream& out) const override { CartesianTransformationAttributes::toxml(out); }
    virtual void set(const map<string, string>& map) override { CartesianTransformationAttributes::set(map); }
    virtual void set(const XmlNode& node) override { CartesianTransformationAttributes::set(node); }
    virtual bool accept(const string& node) override { return CartesianTransformationAttributes::accept(node); }
    virtual double x(double x) const override { return (*x_)(x); }
    virtual double y(double y) const override { return (*y_)(y); }
    virtual double rx(double x) const override { return (*x_).revert(x); }
    virtual double ry(double y) const override { return (*y_).revert(y); }
    virtual bool fast_reproject(double& x, double& y) const override {
        x = (*x_)(x);
        y = (*y_)(y);
        return true;
    }
    void reprojectComponents(double& x, double& y, pair<double, double>&) const override;

    virtual PaperPoint operator()(const UserPoint& xy) const override {
        return PaperPoint((*x_)(xy.x()), (*y_)(xy.y()), xy.value());
    }

    void aspectRatio(double&, double&) override;

    virtual Transformation* clone() const override {
        CartesianTransformation* object = new CartesianTransformation();
        object->copy(*this);
        return object;
    }

    virtual void adjustXAxis(Layout& layout) const override;
    virtual void adjustYAxis(Layout& layout) const override;

    virtual double getMinX() const override { return x_->min(); }
    virtual double getMaxX() const override { return x_->max(); }
    virtual double getMinY() const override { return y_->min(); }
    virtual double getMaxY() const override { return y_->max(); }

    virtual double getMinPCX() const override { return x_->minpc(); }
    virtual double getMaxPCX() const override { return x_->maxpc(); }
    virtual double getMinPCY() const override { return y_->minpc(); }
    virtual double getMaxPCY() const override { return y_->maxpc(); }

    double x(const string& val) const override { return (*x_)(val); }
    double y(const string& val) const override { return (*y_)(val); }


    virtual void setMinMaxX(double min, double max) override { x_->minmax(min, max); }
    virtual void setMinMaxY(double min, double max) override { y_->minmax(min, max); }


    virtual const string& getReferenceX() const override {
        referenceX_ = x_->reference();
        return referenceX_;
    }
    virtual const string& getReferenceY() const override {
        referenceY_ = y_->reference();
        return referenceY_;
    }


    virtual void setDataMinMaxX(double min, double max) const override { x_->dataMinMax(min, max); }
    virtual void setDataMinMaxY(double min, double max) const override { y_->dataMinMax(min, max); }


    vector<double> getDataVectorMinX() const override { return x_->mins(); }
    vector<double> getDataVectorMaxX() const override { return x_->maxs(); }
    vector<double> getDataVectorMinY() const override { return y_->mins(); }
    vector<double> getDataVectorMaxY() const override { return y_->maxs(); }


    Polyline& getPCBoundingBox() const override {
        boxes();
        return *PCEnveloppe_;
    }

    void boxes() const;


    virtual void setDataMinMaxX(double minx, double maxx, const string& ref) const override {
        x_->dataMinMax(minx, maxx, ref);
        referenceX_ = x_->reference();
    }
    virtual void setDataMinMaxY(double min, double max, const string& ref) const override {
        y_->dataMinMax(min, max, ref);
        referenceY_ = y_->reference();
    }


    virtual void setAutomaticX(bool automatic) override { x_->automatic(automatic); }
    virtual void setAutomaticY(bool automatic) override { y_->automatic(automatic); }
    virtual bool getAutomaticX() const override { return x_->automatic() != AxisAutomaticSetting::OFF; }
    virtual bool getAutomaticY() const override { return y_->automatic() != AxisAutomaticSetting::OFF; }

    virtual bool in(double x, double y) const {
        double minx = std::min(x_->min(), x_->max());
        double maxx = std::max(x_->min(), x_->max());
        double miny = std::min(y_->min(), y_->max());
        double maxy = std::max(y_->min(), y_->max());
        return (minx <= x && x <= maxx && miny <= y && y <= maxy);
    }

    string xAxisType() const override { return x_->type(); }
    string yAxisType() const override { return y_->type(); }
    void visit(MetaDataVisitor&, double, double, double, double, double, double) override;
    void getNewDefinition(const UserPoint&, const UserPoint&, string&) const override;
    void setDefinition(const string&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    CartesianTransformation(const CartesianTransformation&);
    //! Overloaded << operator to copy - No copy allowed
    CartesianTransformation& operator=(const CartesianTransformation&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const CartesianTransformation& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
