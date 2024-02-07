/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file OutputFactory.cc
    \brief Implementation of the Template class OutputFactory.
    \author Meteorological Visualisation Section, ECMWF

    Started: Wed July 2006
*/

#include "OutputFactory.h"
#include "DriverManager.h"
#include "GeoJsonDriver.h"
#include "KMLDriver.h"
#include "PostScriptDriver.h"

#ifdef MAGICS_RASTER
#include "GDDriver.h"
#endif


using namespace magics;

#ifdef MAGICS_RASTER
GDDriver* GdOutputFactory::driver_ = 0;
#endif
PostScriptDriver* PS_PsOutputFactory::driver_ = 0;


OutputFactory::OutputFactory() {}

OutputFactory::~OutputFactory() {}

/*!
 Class information are given to the output-stream.
*/
void OutputFactory::print(ostream& out) const {
    out << "OutputFactory[";
    out << "]";
}

void OutputFactory::set(DriverManager&, const XmlNode&) {}


void PS_PsOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    if (!driver_) {
        driver_ = new PostScriptDriver();
        magics.push_back(driver_);
    }
    driver_->set(node);
    driver_->setPS(true);
}

void PS_PsOutputFactory::reset() {
    // The driver has already been deleted
    driver_ = 0;
}

void PS_PdfOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    if (!driver_) {
        driver_ = new PostScriptDriver();
        magics.push_back(driver_);
    }
    driver_->set(node);
    driver_->setPDF(true);
}

void PS_EpsOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    // Add a new PsDriver only for this purpose!
    PostScriptDriver* driver = new PostScriptDriver();
    driver->set(node);
    driver->setEPS(true);

    magics.push_back(driver);
}


#ifdef MAGICS_RASTER
void GD_GifOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    if (!driver_) {
        driver_ = new GDDriver();
        magics.push_back(driver_);
    }
    driver_->set(node);
    driver_->setGIF(true);
}

void GD_PngOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    if (!driver_) {
        driver_ = new GDDriver();
        magics.push_back(driver_);
    }

    driver_->set(node);
    driver_->setPNG(true);
}

void GD_JpegOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    if (!driver_) {
        driver_ = new GDDriver();
        magics.push_back(driver_);
    }
    driver_->set(node);
    driver_->setJPG(true);
}

void GD_GifAnimOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    GDDriver* driver = new GDDriver();
    driver->set(node);
    driver->setAnimation(true);

    magics.push_back(driver);
}

void GdOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    driver_ = new GDDriver();
    driver_->set(node);

    magics.push_back(driver_);
}

void GdOutputFactory::reset() {
    // The driver has already been deleted
    driver_ = 0;
}
#endif

#include "BinaryDriver.h"
void BinaryOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    BinaryDriver* driver = new BinaryDriver();
    driver->set(node);

    magics.push_back(driver);
}

#include "SVGDriver.h"
void SVG_SvgOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    SVGDriver* driver = new SVGDriver();
    driver->set(node);

    magics.push_back(driver);
}

/*
#ifdef MAGICS_QT
#include "QtDriver.h"
void QT_OutputFactory::set(DriverManager& magics, const XmlNode& node)
{
    QtDriver* driver = new QtDriver();
    driver->set(node);

    magics.push_back(driver);
}
#endif
*/

#ifdef HAVE_CAIRO
#include "CairoDriver.h"
void CAIRO_PdfOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setPDF();

    magics.push_back(driver);
}

void CAIRO_CairoOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setCairo();

    magics.push_back(driver);
}

void CAIRO_PngOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setPNG();

    magics.push_back(driver);
}

void CAIRO_PsOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setPS();

    magics.push_back(driver);
}

void CAIRO_EpsOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setEPS();

    magics.push_back(driver);
}

void CAIRO_SvgOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setSVG();

    magics.push_back(driver);
}

/*
void CAIRO_XOutputFactory::set(DriverManager& magics, const XmlNode& node)
{
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setX();

    magics.push_back(driver);
}
*/

void CAIRO_GeoTiffOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setGEOTIFF();

    magics.push_back(driver);
}

void CAIRO_WebpOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    CairoDriver* driver = new CairoDriver();
    driver->set(node);
    driver->setWEBP();

    magics.push_back(driver);
}
#endif

void KML_KmlOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    KMLDriver* driver = new KMLDriver();
    driver->set(node);

    magics.push_back(driver);
}

void GEOJSON_GeoJsonOutputFactory::set(DriverManager& magics, const XmlNode& node) {
    GeoJsonDriver* driver = new GeoJsonDriver();
    driver->set(node);

    magics.push_back(driver);
}
