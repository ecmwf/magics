/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file OutputHandler.cc
    \brief Implementation of the Template class OutputHandler.
    \author Meteorological Visualisation Section, ECMWF

    Started: Wed July 2006
*/

#include "OutputHandler.h"
#include "DriverManager.h"
#include "OutputFactory.h"
#include "XmlNode.h"

using namespace magics;

double OutputHandler::lineSpacing_ = 1.2;

void OutputHandler::set(DriverManager& magics) {
    std::string user_file = ParameterManager::getString("output_file");
    if (!user_file.empty()) {
        Tokenizer parse(".");
        vector<std::string> bits;
        parse(user_file, bits);
        if (bits.size() > 1) {
            std::string fmt = format_;

            format_ = bits[bits.size() - 1];
#if 0
            if ((fmt != "ps" && fmt != format_) || formats_.size()) {
                MagLog::warning() << "'output_file' provided, ignoring 'output_format(s)'. Format set to '" << format_
                                  << "'" << std::endl;
            }
#endif
            formats_.clear();
            formats_.push_back(format_);
        }
        else {
            MagLog::warning() << "'output_file' does not have an extension" << std::endl;
        }
    }

    if (formats_.empty())
        formats_.push_back(format_);

    for (vector<string>::const_iterator format = formats_.begin(); format != formats_.end(); ++format) {
        OutputFactory* factory = MagTranslator<string, OutputFactory>()(*format);
        ASSERT(factory);

        XmlNode node;
        factory->set(magics, node);
        factories_.push_back(factory);
    }

    // Patch for driver png : make the line spacing smaller in case of png !
    if (formats_.size() == 1 && (formats_.front() == "png" || formats_.front() == "pdf"))
        lineSpacing_ = 1.;
}

void OutputHandler::set(const XmlNode& node, DriverManager& magics) {
    try {
        OutputFactory* factory = MagTranslator<string, OutputFactory>()(node.name());
        if (node.name() == "png" && factories_.empty())
            lineSpacing_ = 0.8;
        else if (node.name() == "pdf" && factories_.empty())
            lineSpacing_ = 0.8;
        else if (node.name() == "mgb" && factories_.empty())
            lineSpacing_ = 0.8;
        else
            lineSpacing_ = 1.2;

        ASSERT(factory);

        factory->set(magics, node);
        factories_.push_back(factory);
    }
    catch (...) {
        if (MagicsGlobal::strict()) {
            throw;
        }
    }
}

void OutputHandler::drivers(vector<string>& ds) {
    vector<string> all = {"ps",        "eps",     "ps_pdf", "gd_png", "jpeg",  "gif",       "gif_animation",
                          "svg",       "mgb",     "png",    "pdf",    "cairo", "cairo_svg", "cairo_ps",
                          "cairo_eps", "geotiff", "kml",    "geojson"};

    for (const auto& d : all) {
        try {
            OutputFactory* factory = MagTranslator<string, OutputFactory>()(d);
            ds.push_back(d);
        }
        catch (...) {
            if (MagicsGlobal::strict()) {
                throw;
            }
        }
    }
}


OutputHandler::~OutputHandler() {
    for (const auto& factory : factories_)
        factory->reset();
}


/*
 This code was copied from OutputFactory.cc to hear to work in
 STATIC Magics++ libraries!
*/
static SimpleObjectMaker<PS_PsOutputFactory, OutputFactory> ps("ps");
static SimpleObjectMaker<PS_EpsOutputFactory, OutputFactory> eps("eps");
static SimpleObjectMaker<PS_PdfOutputFactory, OutputFactory> ps_pdf("ps_pdf");

#ifdef MAGICS_RASTER
static SimpleObjectMaker<GD_PngOutputFactory, OutputFactory> gdpng("gd_png");
static SimpleObjectMaker<GD_GifOutputFactory, OutputFactory> gif("gd_gif");
#endif

static SimpleObjectMaker<SVG_SvgOutputFactory, OutputFactory> svg("svg");

static SimpleObjectMaker<BinaryOutputFactory, OutputFactory> binary("mgb");

#ifdef HAVE_CAIRO
static SimpleObjectMaker<CAIRO_PngOutputFactory, OutputFactory> png("png");
static SimpleObjectMaker<CAIRO_PdfOutputFactory, OutputFactory> pdf("pdf");
// static SimpleObjectMaker<CAIRO_XOutputFactory,      OutputFactory> x("x");
static SimpleObjectMaker<CAIRO_CairoOutputFactory, OutputFactory> cairo("cairo");
static SimpleObjectMaker<CAIRO_SvgOutputFactory, OutputFactory> csvg("cairo_svg");
static SimpleObjectMaker<CAIRO_PsOutputFactory, OutputFactory> cps("cairo_ps");
static SimpleObjectMaker<CAIRO_EpsOutputFactory, OutputFactory> ceps("cairo_eps");
#ifdef HAVE_GEOTIFF
static SimpleObjectMaker<CAIRO_GeoTiffOutputFactory, OutputFactory> geotiff("geotiff");
#endif
#endif

static SimpleObjectMaker<KML_KmlOutputFactory, OutputFactory> kml("kml");
static SimpleObjectMaker<GEOJSON_GeoJsonOutputFactory, OutputFactory> geojson("geojson");
