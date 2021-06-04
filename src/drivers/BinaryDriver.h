/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file BinaryDriver.h
    \brief Definition of BinaryDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Sun Oct  4 20:28:15 2009
*/

#ifndef _MPP_BinaryDriver_H
#define _MPP_BinaryDriver_H

#include "BaseDriver.h"
#include "BinaryDriverAttributes.h"
#include "XmlNode.h"

namespace magics {

/*! \class BinaryDriver
    \brief This driver produces output for Binary
    \ingroup drivers

    This driver ...
*/
class BinaryDriver : public BaseDriver, public BinaryDriverAttributes {
public:
    BinaryDriver();
    ~BinaryDriver() override;
    void open() override;
    void close() override;

    /*!
      \brief sets a new XML node
    */
    void set(const XmlNode& node) override {
        if (magCompare(node.name(), "mgb")) {
            XmlNode basic = node;
            basic.name("driver");
            BaseDriver::set(basic);
            basic.name("binary");
            BinaryDriverAttributes::set(basic);
        }
    }

    /*!
      \brief sets a new map
    */
    void set(const map<std::string, std::string>& map) override {
        BaseDriver::set(map);
        BinaryDriverAttributes::set(map);
    }

private:
    MAGICS_NO_EXPORT void startPage() const override;
    MAGICS_NO_EXPORT void endPage() const override;
    MAGICS_NO_EXPORT void project(const Layout& lay) const override;
    MAGICS_NO_EXPORT void unproject() const override;

    MAGICS_NO_EXPORT void setNewLineWidth(const MFloat) const override;
    MAGICS_NO_EXPORT void setNewColour(const Colour& col) const override;
    MAGICS_NO_EXPORT void setLineParameters(const LineStyle style, const MFloat w) const override;

    MAGICS_NO_EXPORT void renderPolyline(const int, MFloat*, MFloat*) const override;
    MAGICS_NO_EXPORT void renderPolyline2(const int n, MFloat* x, MFloat* y) const override;
    MAGICS_NO_EXPORT void renderSimplePolygon(const int, MFloat*, MFloat*) const override;
    MAGICS_NO_EXPORT void renderSimplePolygon(const Polyline& line) const override;
    MAGICS_NO_EXPORT void renderText(const Text& text) const override;
    MAGICS_NO_EXPORT void circle(const MFloat x, const MFloat y, const MFloat r, const int) const override;
    MAGICS_NO_EXPORT bool renderPixmap(MFloat, MFloat, MFloat, MFloat, int, int, unsigned char*, int, bool, bool) const override;
    MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const override;
    MAGICS_NO_EXPORT void renderImage(const ImportObject& obj) const;

    // BinaryDriver specific member functions BEGIN
    MAGICS_NO_EXPORT void renderWindArrow(const Arrow& arrow) const override;
    MAGICS_NO_EXPORT void renderWindFlag(const Flag& flag) const override;

    MAGICS_NO_EXPORT MFloat setAngleY(const MFloat y) const override { return -y; }
    MAGICS_NO_EXPORT MFloat setSymbolY(const MFloat y) const override { return -y; }
    MAGICS_NO_EXPORT MFloat setFlagY(const MFloat y) const override { return y; }
    MAGICS_NO_EXPORT MFloat setY(const MFloat y) const override { return y; }

    mutable ofstream out_;

    // BinaryDriver specific member functions END

    //! Method to print string about this class on to a stream of type ostream (virtual).
    void print(ostream&) const override;
    MAGICS_NO_EXPORT void debugOutput(const string& s) const override { MagLog::debug() << s << endl; }

    //! Copy constructor - No copy allowed
    BinaryDriver(const BinaryDriver&);
    //! Overloaded << operator to copy - No copy allowed
    BinaryDriver& operator=(const BinaryDriver&);

    mutable stack<MFloat> offsetsX_;
    mutable stack<MFloat> offsetsY_;
    mutable int dimensionYglobal_;

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BinaryDriver& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
