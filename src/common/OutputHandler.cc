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

void OutputHandler::set(DriverManager& magics) 
{
	if ( formats_.empty() ) formats_.push_back(format_);

	for ( vector<string>::const_iterator format = formats_.begin(); format != formats_.end(); ++format)
	{
		OutputFactory* factory = MagTranslator<string, OutputFactory>()(*format);
        ASSERT(factory);

		XmlNode node;
		factory->set(magics, node); 
		factories_.push_back(factory);
	}

	// Patch for driver png : make the line spacing smaller in case of png !
	if (  formats_.size() == 1 && (formats_.front() == "png" || formats_.front() == "pdf") )
			lineSpacing_ = 1.;


}

void OutputHandler::set(const XmlNode& node, DriverManager& magics) 
{
	try {
		OutputFactory* factory = MagTranslator<string, OutputFactory>()(node.name());
		if (node.name() == "png" && factories_.empty() )
			lineSpacing_ = 0.8;
		else if (node.name() == "pdf" && factories_.empty() )
			lineSpacing_ = 0.8;
		else
			lineSpacing_ = 1.2;

        ASSERT(factory);
	
		factory->set(magics, node); 
		factories_.push_back(factory);
	}
	catch (...)
	{
	}
}


OutputHandler::~OutputHandler()
{
	for ( vector<OutputFactory* >::const_iterator factory = factories_.begin(); factory != factories_.end(); ++factory)
		(*factory)->reset(); 
}


/*
 This code was copied from OutputFactory.cc to hear to work in 
 STATIC Magics++ libraries!
*/
static SimpleObjectMaker<PS_PsOutputFactory,   OutputFactory> ps("ps");
static SimpleObjectMaker<PS_EpsOutputFactory, OutputFactory> eps("eps");
static SimpleObjectMaker<PS_PdfOutputFactory,  OutputFactory> ps_pdf("ps_pdf");

#ifdef MAGICS_RASTER
#ifdef MAGICS_CAIRO
static SimpleObjectMaker<GD_PngOutputFactory, OutputFactory> gdpng("gd_png");
#else
static SimpleObjectMaker<GD_PngOutputFactory, OutputFactory> gdpng("png");
#endif
static SimpleObjectMaker<GD_JpegOutputFactory, OutputFactory> jpeg("jpeg");
static SimpleObjectMaker<GD_GifOutputFactory, OutputFactory> gif("gif");
static SimpleObjectMaker<GD_GifAnimOutputFactory, OutputFactory> gif_anim("gif_animation");
#endif

static SimpleObjectMaker<SVG_SvgOutputFactory, OutputFactory> svg("svg");

static SimpleObjectMaker<BinaryOutputFactory, OutputFactory> binary("mgb");

#ifdef MAGICS_CAIRO
static SimpleObjectMaker<CAIRO_PngOutputFactory,    OutputFactory> png("png");
static SimpleObjectMaker<CAIRO_PdfOutputFactory,    OutputFactory> pdf("pdf");
static SimpleObjectMaker<CAIRO_XOutputFactory,      OutputFactory> x("x");
static SimpleObjectMaker<CAIRO_CairoOutputFactory,  OutputFactory> cairo("cairo");
static SimpleObjectMaker<CAIRO_SvgOutputFactory,    OutputFactory> csvg("cairo_svg");
static SimpleObjectMaker<CAIRO_PsOutputFactory,     OutputFactory> cps("cairo_ps");
static SimpleObjectMaker<CAIRO_EpsOutputFactory,    OutputFactory> ceps("cairo_eps");
#ifdef MAGICS_GEOTIFF
static SimpleObjectMaker<CAIRO_GeoTiffOutputFactory,OutputFactory> geotiff("geotiff");
#endif
#endif

static SimpleObjectMaker<KML_KmlOutputFactory, OutputFactory> kml("kml");
static SimpleObjectMaker<GEOJSON_GeoJsonOutputFactory, OutputFactory> geojson("geojson");
