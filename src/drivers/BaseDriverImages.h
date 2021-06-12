/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BaseDriverImages.h
    \brief Implementation of methods to display images of driver base class.

    Magics Team - ECMWF 2005

    Started: March 2005

    Changes:

*/

#include "magics.h"

#ifdef HAVE_CAIRO
#include <cairo.h>
#endif

using namespace magics;

/*!
  \brief Image render method for ALL drivers.

  This method should be used by all Magics drivers to render image objects.
*/
MAGICS_NO_EXPORT void BaseDriver::renderImage(const ImportObject& obj) const {
    std::string f         = obj.getFormat();
    GraphicsFormat format = GraphicsFormat::PNG;
    if (magCompare(f, "ps"))
        format = GraphicsFormat::PS;
    else if (magCompare(f, "eps"))
        format = GraphicsFormat::EPS;
    else if (magCompare(f, "gif"))
        format = GraphicsFormat::GIF;
    else if (magCompare(f, "jpeg") || magCompare(f, "jpg"))
        format = GraphicsFormat::JPG;
    else if (magCompare(f, "png"))
        format = GraphicsFormat::PNG;
    else if (magCompare(f, "svg"))
        format = GraphicsFormat::SVG;

    MFloat width  = 0;
    MFloat height = 0;

    if (obj.getWidth() == -1 && magCompare(f, "png") ) {
#ifndef HAVE_CAIRO
        MagLog::error()
            << "BaseDriverImages: image size cannot be determined (Cairo library required)!"
            << endl;
        return;
#else
        cairo_surface_t* surface = cairo_image_surface_create_from_png(obj.getPath().c_str());
        if (cairo_surface_status(surface))
        {
            MagLog::error() << "BaseDriverImages: Cannot read PNG to establish size - " <<obj.getPath().c_str()<< endl;
            return;
        }
        width  = cairo_image_surface_get_width(surface);
        height = cairo_image_surface_get_height(surface);
#endif
    }
    else {
        width  = obj.getWidth();
        height = obj.getHeight();
    }
    const MFloat ow = (obj.getWidth() < 0) ? convertCM(1. / coordRatioX_) : width;
    const MFloat oh = (obj.getHeight() < 0) ? convertCM(1. / coordRatioY_) : height;

    const ImageProperties::OriginReference ori = obj.getOriginReference();
    if (ori == ImageProperties::centre)
        convertToPixmap(obj.getPath(), format, 300, projectX(obj.getOrigin().x() - (ow * .5)),
                        projectY(obj.getOrigin().y() - (oh * .5)), projectX(obj.getOrigin().x() + (ow * .5)),
                        projectY(obj.getOrigin().y() + (oh * .5)));
    else
        convertToPixmap(obj.getPath(), format, 300, projectX(obj.getOrigin().x()), projectY(obj.getOrigin().y()),
                        projectX(obj.getOrigin().x() + ow), projectY(obj.getOrigin().y() + oh));
}  // end BaseDriver::renderImage()


/*!
  \brief converting object to pixmap

  This method should be used by all Magics drivers

*/
MAGICS_NO_EXPORT bool BaseDriver::convertToPixmap(const string& fname, const GraphicsFormat format, const int reso,
                                                  const MFloat x0, const MFloat y0, const MFloat x1,
                                                  const MFloat y1) const {
#ifdef MAGICS_ON_WINDOWS
    return false;
#else
    debugOutput("Start Image conversion");

    int Landscape = 0;
    MFloat bx1 = 100.;
    MFloat by1 = 100.;
    unsigned char* image = 0;
    int col = 0, row = 0;
    int status = 0;
    string pixmapFormat("rgb");

#ifdef HAVE_CAIRO
    if (format == GraphicsFormat::PNG) {
        cairo_surface_t* surface = cairo_image_surface_create_from_png(fname.c_str());
        if (cairo_surface_status(surface))
        {
            MagLog::error() << "BaseDriverImages: Cannot read PNG through Cairo!" << endl;
            return false;
        }

        col = cairo_image_surface_get_width(surface);
        row = cairo_image_surface_get_height(surface);

        int bytes = 0;

        switch (cairo_image_surface_get_format(surface)) {
            case CAIRO_FORMAT_A1:
            default:
                MagLog::error() << "BaseDriverImages: Cannot read PNG A1 through Cairo!" << endl;
                return false;
            case CAIRO_FORMAT_A8:
                MagLog::error() << "BaseDriverImages: Cannot read PNG A8 through Cairo!" << endl;
                return false;
            case CAIRO_FORMAT_RGB24:
                MagLog::error() << "BaseDriverImages: Read PNG RGB24" << endl;
                bytes = 3;
                break;
            case CAIRO_FORMAT_ARGB32:
                MagLog::debug() << "BaseDriverImages: Read PNG ARGB32" << endl;
                pixmapFormat = "rgba";
                bytes = 4;
                break;
          }

        uint8_t*   src        = cairo_image_surface_get_data(surface);
        const int  src_stride = cairo_image_surface_get_stride(surface);
        image                 = new unsigned char[row * col * bytes];
        unsigned char* p      = image;
  
        for (int i = 0; i < row; i++) {
            uint32_t *s = (uint32_t*)(src + i * src_stride);        
            for (int j = 0; j < col; j++) {
                uint32_t point = s[j];
                const unsigned char a = (unsigned char) ((point >> 24) & 0xff);
                const unsigned char r = (unsigned char) ((point >> 16) & 0xff);
                const unsigned char g = (unsigned char) ((point >> 8) & 0xff);
                const unsigned char b = (unsigned char) ((point >> 0) & 0xff);
                *(p++) = r;
                *(p++) = g;
                *(p++) = b;
                *(p++) = a;
            }
        }
    } else
#endif
    {
        MagLog::warning() << "BaseDriverImages: graphics formats (" << format << ") is NOT supported!" << endl;
        return 1;
    }

    bool alpha = (pixmapFormat == "rgba");
    status = renderPixmap(x0, y0, x1, y1, col, row, image, Landscape, alpha, false);

    if (!status)
        MagLog::warning()
            << "BaseDriver::convertToPixmap() -> no Pixmap could be drawn! Zero size of at least one dimension."
            << std::endl;
    delete[] image;

    return status;
#endif
}
