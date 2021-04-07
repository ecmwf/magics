/*
 * (C) Copyright 1996-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef EMAGRAM_H
#define EMAGRAM_H

#include "Coordinate.h"
#include "TephigramAttributes.h"
#include "Transformation.h"
#include "XmlNode.h"

namespace magics {

/*! \class Tephigram
    \brief Implements a new projection
    \ingroup projection

    This projection ...
*/

class Emagram : public Transformation, public TephigramAttributes {
public:
    Emagram();
    ~Emagram() override;

    /*!
      \brief sets  from an XML node
    */
    void set(const XmlNode& node) override {
        Transformation::set(node);
        TephigramAttributes::set(node);
    }
    /*!
      \brief sets  from a map
    */
    void set(const map<string, string>& map) override {
        Transformation::set(map);
        TephigramAttributes::set(map);
    }

    virtual Transformation* clone() const override {
        Emagram* transformation = new Emagram();
        return transformation;
    }

    /*!
    \\brief Initialise the projection
    */
    virtual void init() override;
    /*!
    \\brief
    */
    virtual PaperPoint operator()(const UserPoint&) const override;
    /*!
    \\brief
    */
    virtual bool getAutomaticX() const override { return x_automatic_; }
    virtual bool getAutomaticY() const override { return y_automatic_; }
    virtual void setMinMaxX(double, double) override;
    virtual void setMinMaxY(double, double) override;
    virtual PaperPoint operator()(const PaperPoint&) const override;
    /*!
    \\brief
    */
    virtual void revert(const PaperPoint&, UserPoint&) const override;

    void revert(const vector<std::pair<double, double> >&, vector<std::pair<double, double> >&) const override;


    /*!
    \\brief Does the projection needs the coastalines to be shifted!
    */
    virtual bool needShiftedCoastlines() const override;
    /*!
    \\brief set the aspect ratio!
    */
    virtual void aspectRatio(double&, double&) override;
    /*!
    \\brief set the bounding box!
    */
    virtual void boundingBox(double&, double&, double&, double&) const override;

    /*!
    \\brief return the xmin in user coordinates!
    */
    virtual double getMinX() const override;
    /*!
    \\brief return the ymin in user coordinates!
    */
    virtual double getMinY() const override;
    /*!
    \\brief return the xmax in user coordinates!
    */
    virtual double getMaxX() const override;
    /*!
    \\brief return the ymax in user coordinates!
    */
    virtual double getMaxY() const override;
    /*!
    \\brief set the xmin in user coordinates!
    */
    virtual void setMinX(double);
    /*!
    \\brief return the ymin in user coordinates!
    */
    virtual void setMinY(double);
    /*!
    \\brief return the xmax in user coordinates!
    */
    virtual void setMaxX(double);
    /*!
    \\brief return the ymax in user coordinates!
    */
    virtual void setMaxY(double);
    /*!
    \\brief return the xmin in projection coordinates!
    */
    virtual double getMinPCX() const override;
    /*!
    \\brief return the ymin in projection coordinates!
    */
    virtual double getMinPCY() const override;
    /*!
    \\brief return the xmax in projection coordinates!
    */
    virtual double getMaxPCX() const override;
    virtual double getMaxTestPCX() const;
    /*!
    \\brief return the ymax in projection coordinates!
    */
    virtual double getMaxPCY() const override;

    virtual Polyline& getPCBoundingBox() const override;
    virtual Polyline& getUserBoundingBox() const override;

    virtual void setDefinition(const string&) override;
    void getNewDefinition(const UserPoint&, const UserPoint&, string&) const override;
    bool in(const PaperPoint& point) const override;
    void operator()(const Polyline& poly, BasicGraphicsObjectContainer& out) const override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    double minPCX_;
    double maxPCX_;
    double minPCY_;
    double maxPCY_;

private:
    //! Copy constructor - No copy allowed
    Emagram(const Emagram&);
    //! Overloaded << operator to copy - No copy allowed
    Emagram& operator=(const Emagram&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Emagram& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics

#endif  // EMAGRAM_H
