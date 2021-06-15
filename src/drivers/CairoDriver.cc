/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*

 I currently have a function that accepts a cairo context (with a path already set)
 and strokes the path with a pre-set width and color. The context can have any arbitrary
 transformation already applied to it. So given this situation, how would I ensure
 that I get a uniform line width from the stroke even when they may have applied a
 deforming scale to the context? Any ideas would be appreciated!

Something like:

  cairo_save (cr);
  cairo_identity_matrix (cr);
  cairo_set_line_width (cr, width);
  cairo_stroke (cr);
  cairo_restore (cr);

*/

/*! \file CairoDriver.cc
    \brief Implementation of CairoDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Oct 15 20:49:32 2007

   \todo Fix 'convert' dependency in "renderImage"
   \todo Check how much drivers are dependent on writing temp files in local directory (thread safety)
*/
#include <cairo.h>

#include "CairoDriver.h"

#include "Image.h"
#include "ImportObject.h"
#include "Polyline.h"
#include "Symbol.h"
#include "System.h"
#include "Text.h"
#include "Timer.h"

#include <iconv.h>  // Only for AIX?
#include <pango/pangocairo.h>

#if CAIRO_HAS_PDF_SURFACE
#include <cairo-pdf.h>
#endif

#if CAIRO_HAS_PS_SURFACE
#include <cairo-ps.h>
#endif

#if CAIRO_HAS_SVG_SURFACE
#include <cairo-svg.h>
#endif

#define FONT_SCALE 25 * .7  //! \todo clean-up!!!


using namespace magics;

/*!
  \brief Constructor
*/
CairoDriver::CairoDriver() : offsetX_(0), offsetY_(0), backend_("PDF") {
    cr_ = 0;
}

/*!
  \brief Destructor
*/
CairoDriver::~CairoDriver() {}

/*!
  \brief Opening the driver
*/
void CairoDriver::open() {
    MagLog::info() << "Cairo version used is: " << cairo_version_string() << " backend: " << backend_ << std::endl;

    MFloat ratio = getYDeviceLength() / getXDeviceLength();
    int width    = maground(width_);

    string mbg_tmpl = mgb_template_;
    if (!mbg_tmpl.empty()) {
        setDimensionsFromBinary(mbg_tmpl, ratio, width);
    }

    setCMscale(MFloat(width) / getXDeviceLength());
    dimensionXglobal_ = width;
    MagLog::dev() << "width -->" << width << endl;
    MagLog::dev() << "ratio -->" << ratio * width << endl;
    MagLog::dev() << "ratio -->" << maground(ratio * width) << endl;
    MagLog::dev() << "ratio -->" << static_cast<int>(ratio * width) << endl;
    dimensionYglobal_ = maground(ratio * width);

    coordRatioY_ = -1;

    setupNewSurface();

    cairo_status_t res = cairo_surface_status(surface_);
    if (res != CAIRO_STATUS_SUCCESS) {
        MagLog::warning() << "Cairo > " << cairo_status_to_string(res) << endl;
        return;
    }

    if (magCompare(antialias_, "off")) {
        cairo_set_antialias(cr_, CAIRO_ANTIALIAS_NONE);
        cairo_font_options_t* cf = cairo_font_options_create();
        cairo_font_options_set_antialias(cf, CAIRO_ANTIALIAS_NONE);
    }
}


void CairoDriver::setupNewSurface() const {
    if (magCompare(backend_, "png") || magCompare(backend_, "geotiff")) {
        surface_ = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, dimensionXglobal_, dimensionYglobal_);
    }
    else if (magCompare(backend_, "pdf")) {
#if CAIRO_HAS_PDF_SURFACE
        fileName_ = getFileName("pdf");
        surface_  = cairo_pdf_surface_create(fileName_.c_str(), dimensionXglobal_, dimensionYglobal_);
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 16, 0)
        cairo_pdf_surface_set_metadata(surface_, CAIRO_PDF_METADATA_TITLE, title_.c_str());
        const SystemInfo info;
        cairo_pdf_surface_set_metadata(surface_, CAIRO_PDF_METADATA_AUTHOR, info.getUserID().c_str());
        const string creator = output_creator_ + " " + getMagicsVersionString();
        cairo_pdf_surface_set_metadata(surface_, CAIRO_PDF_METADATA_CREATOR, creator.c_str());
//        cairo_pdf_surface_set_metadata(surface_, CAIRO_PDF_METADATA_KEYWORDS, <string with tags for Intraplots>;        
#endif
#else
        MagLog::error() << "CairoDriver: PDF output NOT supported! Enable PDF support in your Cairo installation."
                        << std::endl;
#endif
    }
    else if (magCompare(backend_, "ps")) {
#if CAIRO_HAS_PS_SURFACE
        fileName_                  = getFileName("ps");
        const int dimensionXglobal = static_cast<int>(getXDeviceLength() * 72 / 2.54);
        const int dimensionYglobal = static_cast<int>(getYDeviceLength() * 72 / 2.54);
        surface_                   = cairo_ps_surface_create(fileName_.c_str(), dimensionXglobal, dimensionYglobal);
#else
        MagLog::error() << "CairoDriver: PS output NOT supported! Enable PS support in your Cairo installation."
                        << std::endl;
#endif
    }
    else if (magCompare(backend_, "eps")) {
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 5, 2)
        fileName_ = getFileName("eps");
        surface_  = cairo_ps_surface_create(fileName_.c_str(), dimensionXglobal_, dimensionYglobal_);
        cairo_ps_surface_set_eps(surface_, true);
#else
        MagLog::error() << "CairoDriver: EPS output NOT supported! You need at least version Cairo 1.5.2.\n"
                        << "PostScript is generated instead." << std::endl;
        fileName_ = getFileName("ps");
        surface_  = cairo_ps_surface_create(fileName_.c_str(), dimensionXglobal_, dimensionYglobal_);
#endif
    }
    else if (magCompare(backend_, "svg")) {
#if CAIRO_HAS_SVG_SURFACE
        fileName_ = getFileName("svg", currentPage_ + 1);

        surface_ = cairo_svg_surface_create(fileName_.c_str(), dimensionXglobal_, dimensionYglobal_);
#else
        MagLog::error() << "CairoDriver: SVG output NOT supported! Enable SVG support in your Cairo installation."
                        << std::endl;
#endif
    }
    else {
        MagLog::error() << "CairoDriver: The backend " << backend_ << " is NOT supported!" << std::endl;
    }

    cairo_status_t status = cairo_surface_status(surface_);
    if (status) {
        MagLog::error() << "CairoDriver: the output file (" << backend_ << ") could NOT be generated!"
                        << " -> " << cairo_status_to_string(status) << std::endl;
    }

    if (!cr_)
        cr_ = cairo_create(surface_);

    // set PS META information
    if (magCompare(backend_, "ps")) {
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 8, 0)
        const SystemInfo info;
        const string s1 = "%%Title: " + title_;
        cairo_ps_surface_dsc_comment(surface_, s1.c_str());
        const string s2 = "%%Creator2: " + output_creator_ + " " + getMagicsVersionString();
        cairo_ps_surface_dsc_comment(surface_, s2.c_str());
        const string s3 = "%%For: " + info.getUserID() + "@" + info.getHostName();
        cairo_ps_surface_dsc_comment(surface_, s3.c_str());

        dimensionXglobal_ = static_cast<int>(getXDeviceLength() * 72 / 2.54);
        dimensionYglobal_ = static_cast<int>(getYDeviceLength() * 72 / 2.54);
        cairo_ps_surface_dsc_comment(surface_, "%%PageOrientation: Landscape");

       // if (magCompare(MAGICS_SITE, "ecmwf"))
       //     cairo_ps_surface_dsc_comment(surface_, "%%Copyright: ECMWF");
#endif
    }

    if (magCompare(transparent_, "off") || !(magCompare(backend_, "png") || magCompare(backend_, "geotiff"))) {
        cairo_set_source_rgb(cr_, 1.0, 1.0, 1.0); /* white */
    }
    else {
        cairo_set_source_rgba(cr_, 1.0, 1.0, 1.0, 0.0); /* white transparent */
    }
    cairo_paint(cr_);
    cairo_set_line_join(cr_, CAIRO_LINE_JOIN_BEVEL);

    dimensionX_  = static_cast<MFloat>(dimensionXglobal_);
    dimensionY_  = static_cast<MFloat>(dimensionYglobal_);
    currentPage_ = 0;
}

/*!
  \brief Closing the driver
*/
void CairoDriver::close() {
    currentPage_ = 0;
    if (magCompare(backend_, "pdf") && !fileName_.empty())
        printOutputName("CAIRO pdf " + fileName_);
    if (magCompare(backend_, "ps") && !fileName_.empty())
        printOutputName("CAIRO ps " + fileName_);

    cairo_surface_destroy(surface_);
    cairo_destroy(cr_);
}


/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.
*/
MAGICS_NO_EXPORT void CairoDriver::startPage() const {
    if (currentPage_ > 0) {
        if (magCompare(backend_, "png") || magCompare(backend_, "geotiff")) {
            cairo_destroy(cr_);
            cairo_surface_destroy(surface_);

            surface_ = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, dimensionXglobal_, dimensionYglobal_);
            cr_      = cairo_create(surface_);
            if (magCompare(transparent_, "off")) {
                cairo_set_source_rgb(cr_, 1.0, 1.0, 1.0); /* white */
            }
            else {
                cairo_set_source_rgba(cr_, 1.0, 1.0, 1.0, 0.0); /* white transparent */
            }
            cairo_paint(cr_);
        }
#if CAIRO_HAS_SVG_SURFACE
        else if (magCompare(backend_, "svg")) {
            cairo_destroy(cr_);
            cairo_surface_destroy(surface_);

            fileName_ = getFileName("svg", currentPage_ + 1);
            MagLog::dev() << "Cairo - SVG - create file " << fileName_ << endl;
            surface_ = cairo_svg_surface_create(fileName_.c_str(), dimensionXglobal_, dimensionYglobal_);
            cr_      = cairo_create(surface_);
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 4, 0)
            cairo_svg_surface_restrict_to_version(surface_, CAIRO_SVG_VERSION_1_1);
#endif
        }
#endif
        else if (magCompare(backend_, "eps")) {
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 5, 2)
            cairo_destroy(cr_);
            cairo_surface_destroy(surface_);

            fileName_ = getFileName("eps", currentPage_ + 1);
            surface_  = cairo_ps_surface_create(fileName_.c_str(), dimensionXglobal_, dimensionYglobal_);
            cairo_ps_surface_set_eps(surface_, true);
            cr_ = cairo_create(surface_);
#endif
        }
    }

    if (currentPage_ == 0 || (!magCompare(backend_, "pdf") && !magCompare(backend_, "ps"))) {
        cairo_translate(cr_, 0, static_cast<MFloat>(dimensionYglobal_));
    }

    if (magCompare(antialias_, "off"))
        cairo_set_antialias(cr_, CAIRO_ANTIALIAS_NONE);
    else
        cairo_set_antialias(cr_, CAIRO_ANTIALIAS_SUBPIXEL);

    currentPage_++;
    newPage_ = true;
}


/*!
  \brief ending a new page

  This method has to take care that for formats with multiple output
  files are closed.
*/
MAGICS_NO_EXPORT void CairoDriver::endPage() const {
    cairo_show_page(cr_);

    if (magCompare(backend_, "eps")) {
        if (!fileName_.empty())
            printOutputName("CAIRO eps " + fileName_);
    }
    else if (magCompare(backend_, "svg")) {
        if (!fileName_.empty())
            printOutputName("CAIRO svg " + fileName_);
    }
    else if (magCompare(backend_, "png")) {
        Timer timer("cairo", "write png");
        fileName_ = getFileName("png", currentPage_);

        cairo_status_t status = CAIRO_STATUS_SUCCESS;
        if (magCompare(palette_, "on")) {
            if (!write_8bit_png()) {
                MagLog::warning() << "CairoDriver::renderPNG > palletted PNG failed! Generate 24 bit one ..." << endl;
                status = cairo_surface_write_to_png(surface_, fileName_.c_str());
            }
        }
        else {
            status = cairo_surface_write_to_png(surface_, fileName_.c_str());
        }
        if (status != CAIRO_STATUS_SUCCESS) {
            MagLog::error() << "PNG could NOT be written - " << cairo_status_to_string(status) << endl;
            MagLog::error() << "^^^^^^^^^^^^^^^^^^^^^^^^" << endl;
        }
        if (!fileName_.empty())
            printOutputName("CAIRO png " + fileName_);
    }
    else if (magCompare(backend_, "geotiff")) {
#ifdef HAVE_GEOTIFF
        fileName_ = getFileName("tif", currentPage_);
        write_tiff();
#else
        MagLog::error() << "CairoDriver: GEOTIFF not enabled!" << std::endl;
#endif
    }
}


void CairoDriver::newLayer(Layer&) const {
    cairo_save(cr_);
}

#include "CairoDriver-blur.h"

void CairoDriver::closeLayer(Layer&) const {
    if (applyGaussianBlur_ > 0) {
        blur_image_surface(surface_, 50);
    }
    cairo_restore(cr_);
}


#ifdef HAVE_GEOTIFF

#include <geotiffio.h>
#include <tiffio.h>
/*!
  \brief write raster into (Geo)Tiff

  Only the raw raster (normally written to a PNG) is here written into a (Geo)Tiff.

  \sa http://trac.osgeo.org/geotiff/
  \sa http://www.remotesensing.org/geotiff/spec/geotiffhome.html
  \sa http://www.remotesensing.org/geotiff/spec/geotiff6.html

*/
MAGICS_NO_EXPORT void CairoDriver::write_tiff() const {
    int compression = 1;

    unsigned char* data = cairo_image_surface_get_data(surface_);
    int width           = cairo_image_surface_get_width(surface_);
    int height          = cairo_image_surface_get_height(surface_);
    const int stride    = cairo_image_surface_get_stride(surface_);

    TIFF* tif = TIFFOpen(fileName_.c_str(), "w");
    if (!tif) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(fileName_);
        }
        MagLog::warning() << "CairoDriver: Unable to open TIFF file " << fileName_ << std::endl;
        return;
    }

    GTIF* gtif = GTIFNew(tif);
    if (!gtif) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(fileName_);
        }
        MagLog::warning() << "CairoDriver: Unable to open GeoTIFF file " << fileName_ << std::endl;
        return;
    }

    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 4);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
    TIFFSetField(tif, TIFFTAG_SOFTWARE, "Magics");

    GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelGeographic);
    GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
    GTIFKeySet(gtif, GTCitationGeoKey, TYPE_ASCII, 0, "MagicsPlot");
    GTIFKeySet(gtif, GeographicTypeGeoKey, TYPE_SHORT, 1, KvUserDefined);
    GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII, 0, "Everest Ellipsoid Used.");
    GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
    GTIFKeySet(gtif, GeogLinearUnitsGeoKey, TYPE_SHORT, 1, Linear_Meter);
    GTIFKeySet(gtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1, KvUserDefined);
    GTIFKeySet(gtif, GeogEllipsoidGeoKey, TYPE_SHORT, 1, Ellipse_WGS_84);
    GTIFKeySet(gtif, GeogSemiMajorAxisGeoKey, TYPE_DOUBLE, 1, (double)6377298.556);
    GTIFKeySet(gtif, GeogInvFlatteningGeoKey, TYPE_DOUBLE, 1, (double)300.8017);

    if (compression > 1) {
        if (compression > 10)
            compression = 10;
        TIFFSetField(tif, TIFFTAG_COMPRESSION, compression);
    }

    // DPI
    TIFFSetField(tif, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
    TIFFSetField(tif, TIFFTAG_XRESOLUTION, (float)90.);
    TIFFSetField(tif, TIFFTAG_YRESOLUTION, (float)90.);

    unsigned char* buf;
    if (TIFFScanlineSize(tif))
        buf = (unsigned char*)_TIFFmalloc(4 * width);
    else
        buf = (unsigned char*)_TIFFmalloc(TIFFScanlineSize(tif));

    unsigned char* pscanline;
    for (unsigned int i = 0; i < height; i++) {
        pscanline = buf;
        for (unsigned int j = 0; j < stride; j++) {
            *pscanline++ = data[i * stride + j++];
            *pscanline++ = data[i * stride + j++];
            *pscanline++ = data[i * stride + j++];
            *pscanline++ = data[i * stride + j];
        }
        TIFFWriteScanline(tif, buf, i, 0);
    }
    TIFFClose(tif);
    _TIFFfree(buf);
    return;
}
#endif  // HAVE_GEOTIFF

#include <png.h>
/*!
  \brief write raster into 8 bit PNG

  Only the raw raster (normally written to a 32 bit PNG) is here written into a 8 bit.

*/
//#define PNG_DEBUG 3

MAGICS_NO_EXPORT bool CairoDriver::write_8bit_png() const {
    return false;
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void CairoDriver::project(const Layout& layout) const {
    cairo_save(cr_);

    // push current state
    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    const MFloat oldHeight = dimensionY_;
    offsetsX_.push(offsetX_);
    offsetsY_.push(offsetY_);
    scalesX_.push(coordRatioX_);
    scalesY_.push(coordRatioY_);

    offsetX_ += layout.x() * 0.01 * dimensionX_;
    offsetY_ -= layout.y() * 0.01 * dimensionY_;
    dimensionX_ = layout.width() * 0.01 * dimensionX_;
    dimensionY_ = layout.height() * 0.01 * dimensionY_;

    const MFloat sumX = layout.maxX() - layout.minX();
    const MFloat sumY = layout.maxY() - layout.minY();

    if (sumX != 0 && sumY != 0) {
        coordRatioX_ = dimensionX_ / sumX;
        coordRatioY_ = -dimensionY_ / sumY;
    }

    offsetX_ = projectX(-layout.minX());
    offsetY_ = projectY(-layout.minY());


    if (layout.clipp()) {
        //		cairo_set_source_rgb(cr_, 1,0,0);
        cairo_rectangle(cr_, projectX(layout.minX()), projectY(layout.minY()),
                        projectX(layout.maxX()) - projectX(layout.minX()),
                        projectY(layout.maxY()) - projectY(layout.minY()));
        cairo_clip(cr_);
        //		cairo_stroke(cr_);
    }

    // write meta info
    if (layout.isNavigable() &&
        (magCompare(backend_, "png") || magCompare(backend_, "svg") || magCompare(backend_, "geotiff"))) {
        const double offsetX = projectX(layout.minX());
        const double offsetY = projectY(layout.maxY());
        layout.pushDriverInfo(offsetX, oldHeight + offsetY, dimensionX_, dimensionY_);
    }
}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

  \sa UnLayout
*/
MAGICS_NO_EXPORT void CairoDriver::unproject() const {
    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();
    offsetX_ = offsetsX_.top();
    offsetsX_.pop();
    offsetY_ = offsetsY_.top();
    offsetsY_.pop();
    coordRatioX_ = scalesX_.top();
    scalesX_.pop();
    coordRatioY_ = scalesY_.top();
    scalesY_.pop();

    cairo_restore(cr_);
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void CairoDriver::setNewColour(const Colour& colour) const {
    currentColour_ = colour;
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished.

  \sa setLineParameters()
  \todo Find a better way than multiple by 0.6
*/
MAGICS_NO_EXPORT void CairoDriver::setNewLineWidth(const MFloat width) const {
    //        currentLineWidth_ = (width > 0.01 ? width : 0.01) * .6;
    currentLineWidth_ = width * .5;
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param width width of the line
*/
MAGICS_NO_EXPORT void CairoDriver::setLineParameters(const LineStyle linestyle, const MFloat width) const {
    setNewLineWidth(width);

#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 4, 0)
    if (cairo_get_dash_count(cr_) == 0 && linestyle == LineStyle::SOLID)
        return;
#endif
    switch (linestyle) {
        case LineStyle::DASH:  // 6 on - 2 off
        {
            cairo_set_line_cap(cr_, CAIRO_LINE_CAP_SQUARE);
            const double dash_line[] = {4.};
            cairo_set_dash(cr_, dash_line, 1, 0.);
        } break;
        case LineStyle::DOT:  // 1 on - 2 off
        {
            setNewLineWidth(2 * width);
            const double dotted_line[] = {0., 6.};
            cairo_set_line_cap(cr_, CAIRO_LINE_CAP_ROUND);
            cairo_set_dash(cr_, dotted_line, 2, 0.);
        } break;
        case LineStyle::CHAIN_DASH:  // 4 on - 2 off -  1 on - 2 off
        {
            const double chain_dash_line[] = {4., 4., 0., 6.};
            cairo_set_line_cap(cr_, CAIRO_LINE_CAP_SQUARE);
            cairo_set_dash(cr_, chain_dash_line, 4, 0.);
        } break;
        case LineStyle::CHAIN_DOT:  // 4 on - 2 off -  1 on - 2 off - 1 on - 2 off
        {
            const double chain_dot_line[] = {4., 4., 0., 6., 0., 6.};
            cairo_set_line_cap(cr_, CAIRO_LINE_CAP_SQUARE);
            cairo_set_dash(cr_, chain_dot_line, 6, 0.);
        } break;
        default:  // SOLID
        {
            cairo_set_line_cap(cr_, CAIRO_LINE_CAP_SQUARE);
            const double solid_line[] = {4., 0.};
            cairo_set_dash(cr_, solid_line, 0, 0.);
        } break;
    }
}

/*!
  \brief renders polylines

  This method renders a polyline given as two MFloat arrays. The two
  arrays given as X and Y values have to be at least the length of
  <i>n</i>. All values beyond <i>n</i> will be ignored. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void CairoDriver::renderPolyline(const int n, MFloat* x, MFloat* y) const {
    if (n < 2 || (currentColour_ == Colour("none")))
        return;
    cairo_save(cr_);

    MFloat xx = projectX(x[0]);
    MFloat yy = projectY(y[0]);
    cairo_move_to(cr_, xx, yy);

    for (int l = 1; l < n; l++) {
        xx = projectX(x[l]);
        yy = projectY(y[l]);
        cairo_line_to(cr_, xx, yy);
    }

    cairo_identity_matrix(cr_);
    cairo_set_line_width(cr_, currentLineWidth_);
    cairo_set_source_rgba(cr_, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                          currentColour_.alpha());
    cairo_stroke(cr_);
    cairo_restore(cr_);
}

/*!
  \brief renders a single line

  This method renders a polyline with two points.The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void CairoDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const {
    if (n != 2 || (currentColour_ == Colour("none")))
        return;

    cairo_save(cr_);
    cairo_move_to(cr_, x[0], y[0]);
    cairo_line_to(cr_, x[1], y[1]);
    cairo_identity_matrix(cr_);
    cairo_set_line_width(cr_, currentLineWidth_);

    cairo_set_source_rgba(cr_, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                          currentColour_.alpha());
    cairo_stroke(cr_);
    cairo_restore(cr_);
}


/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param line polyline to be filled
*/
MAGICS_NO_EXPORT void CairoDriver::renderSimplePolygon(const magics::Polyline& line) const {
    setNewColour(line.getFillColour());
    const unsigned int n = line.size();
    if (n < 3 || (currentColour_ == Colour("none")))
        return;
    cairo_save(cr_);
    line.getShading()->draw(*this);

    cairo_set_source_rgba(cr_, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                          currentColour_.alpha());

    MFloat xx = projectX(line.get(0).x());
    MFloat yy = projectY(line.get(0).y());
    cairo_move_to(cr_, xx, yy);

    for (unsigned int l = 1; l < n; l++) {
        const PaperPoint& pp = line.get(l);
        xx                   = projectX(pp.x());
        yy                   = projectY(pp.y());
        cairo_line_to(cr_, xx, yy);
    }

    cairo_set_fill_rule(cr_, CAIRO_FILL_RULE_EVEN_ODD);

    magics::Polyline::Holes::const_iterator h  = line.beginHoles();
    magics::Polyline::Holes::const_iterator he = line.endHoles();

    for (; h != he; ++h) {
        cairo_new_sub_path(cr_);
        vector<double> x;
        vector<double> y;
        line.hole(h, x, y);
        if (x.empty())
            continue;
        cairo_move_to(cr_, projectX(x[0]), setY(projectY(y[0])));
        vector<double>::const_iterator yt = y.begin();
        vector<double>::const_iterator it = x.begin();
        ++it;
        ++yt;
        for (; it != x.end();) {
            cairo_line_to(cr_, projectX(*it), setY(projectY(*yt)));
            ++it;
            ++yt;
        }
    }

    renderSimplePolygon();
}


/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void CairoDriver::renderSimplePolygon(const int n, MFloat* x, MFloat* y) const {
    if (n < 3 || (currentColour_ == Colour("none")))
        return;
    cairo_save(cr_);

    cairo_set_source_rgba(cr_, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                          currentColour_.alpha());

    MFloat xx = projectX(x[0]);
    MFloat yy = projectY(y[0]);
    cairo_move_to(cr_, xx, yy);

    for (int l = 1; l < n; l++) {
        xx = projectX(x[l]);
        yy = projectY(y[l]);
        cairo_line_to(cr_, xx, yy);
    }

    cairo_close_path(cr_);

    renderSimplePolygon();
}

/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void CairoDriver::renderSimplePolygon() const {
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 2, 0)
    if (currentShading_ == Shading::DOT) {
        const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
        const int density               = (int)sqrt(pro->density_);
        if (density <= 0)
            return;
        const int s              = (int)(pro->size_ * convertCM(1.) * 5.);
        const MFloat square_size = convertCM(1.) / density;

        cairo_surface_t* pat_surface;
        cairo_pattern_t* pattern;
        cairo_t* cr2;

        pat_surface = cairo_surface_create_similar(cairo_get_group_target(cr_), CAIRO_CONTENT_COLOR_ALPHA, square_size,
                                                   square_size);
        cr2         = cairo_create(pat_surface);

        cairo_set_source_rgba(cr2, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                              currentColour_.alpha());
        const MFloat off = (square_size)*.5;
        cairo_rectangle(cr2, off, off, s, s);
        cairo_fill(cr2);

        pattern = cairo_pattern_create_for_surface(cairo_get_target(cr2));

        cairo_pattern_set_extend(pattern, CAIRO_EXTEND_REPEAT);

        cairo_set_source(cr_, pattern);
        cairo_fill(cr_);

        cairo_pattern_destroy(pattern);
        cairo_surface_destroy(pat_surface);
        cairo_destroy(cr2);
    }
    else if (currentShading_ == Shading::HATCH) {
        const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
        indexHatch_                       = pro->index_;
        if (indexHatch_ < 1 || indexHatch_ > 6) {
            MagLog::warning() << "CairoDriver::renderSimplePolygon > Hatch index " << indexHatch_
                              << " is wrong. No hatch sahding possible!" << endl;
            return;
        }
        const int density = (int)(1. / pro->density_ * 150);

        cairo_surface_t* pat_surface;
        cairo_pattern_t* pattern;
        cairo_t* cr2;

        pat_surface =
            cairo_surface_create_similar(cairo_get_group_target(cr_), CAIRO_CONTENT_COLOR_ALPHA, density, density);
        cr2 = cairo_create(pat_surface);
        cairo_surface_destroy(pat_surface);

        cairo_set_source_rgba(cr2, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                              currentColour_.alpha());
        if (indexHatch_ == 1 || indexHatch_ == 3)  // horizontal
        {
            cairo_move_to(cr2, 0, density * .5 + .5);
            cairo_line_to(cr2, density + .5, density * .5 + .5);
        }
        if (indexHatch_ == 2 || indexHatch_ == 3)  // vertical
        {
            cairo_move_to(cr2, density + .5 * .5, 0);
            cairo_line_to(cr2, density + .5 * .5, density + .5);
        }
        if (indexHatch_ == 4 || indexHatch_ == 6) {
            cairo_move_to(cr2, 0, 0);
            cairo_line_to(cr2, density + .5, density + .5);
        }
        if (indexHatch_ == 5 || indexHatch_ == 6) {
            cairo_move_to(cr2, density + .5, 0);
            cairo_line_to(cr2, 0, density + .5);
        }
        cairo_identity_matrix(cr_);
        cairo_set_line_width(cr_, pro->thickness_ * .5);
        cairo_stroke(cr2);

        pattern = cairo_pattern_create_for_surface(cairo_get_target(cr2));
        cairo_destroy(cr2);

        cairo_pattern_set_extend(pattern, CAIRO_EXTEND_REPEAT);

        cairo_set_source(cr_, pattern);
        cairo_fill(cr_);

        cairo_pattern_destroy(pattern);
    }
    else
#else
    if (currentShading_ == Shading::HATCH || currentShading_ == Shading::DOT)
        MagLog::error() << "CairoDriver: For hatch and dot shading you need at least Cairo 1.2!\n"
                        << "             Solid shading used instead." << std::endl;
#endif
    {
        cairo_fill(cr_);
    }
    cairo_restore(cr_);
    currentShading_ = Shading::SOLID;
}


/*!
  \brief renders text strings

  Cairo expects a string as a char array, where each character is expressed as
  16 bit Unicode. Expat however delivers Multi-Byte encoding!

  \sa http://www.pygtk.org/docs/pygtk/pango-markup-language.html

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void CairoDriver::renderText(const Text& text) const {
    if (text.empty())
        return;
    const vector<NiceText>& niceT = text.getNiceText();
    if (niceT.empty())
        return;

    enum Justification horizontal     = text.getJustification();
    const enum VerticalAlign vertical = text.getVerticalAlign();

    vector<NiceText>::const_iterator niceText    = text.textBegin();
    vector<NiceText>::const_iterator niceTextEnd = text.textEnd();

    ostringstream alltext;

    for (; niceText < niceTextEnd; niceText++) {
        if ((*niceText).text().empty())
            continue;
        MagFont magfont                = (*niceText).font();
        const std::set<string>& styles = magfont.styles();

        setNewColour(magfont.colour());
        const int r = (int)(currentColour_.red() * 255.);
        const int g = (int)(currentColour_.green() * 255.);
        const int b = (int)(currentColour_.blue() * 255.);
        if (r < 0 || g < 0 || b < 0) {
            MagLog::debug() << "CAIRO: Text " << (*niceText).text() << " is RGB -1!" << endl;
        }
        else {
            ostringstream col;
            col << hex << "#";
            if (r > 15)
                col << r;
            else
                col << "0" << r;
            if (g > 15)
                col << g;
            else
                col << "0" << g;
            if (b > 15)
                col << b;
            else
                col << "0" << b;

            gchar* t = g_markup_escape_text((*niceText).text().c_str(), -1);

            alltext << "<span color=\"" << col.str() << "\" font_family=\"" << magfont.name() << "\" size=\""
                    << int(magfont.size() * FONT_SCALE * 1024 * font_scale_) << "\"";
            if (styles.find("bold") != styles.end())
                alltext << " weight=\"bold\"";
            else if (styles.find("italic") != styles.end())
                alltext << " style=\"italic\"";
            else if (styles.find("bolditalic") != styles.end())
                alltext << " style=\"italic\" weight=\"bold\"";
            if (text.getBlanking())
                alltext << " background=\"#FFFFFF\"";
            if (styles.find("underlined") != styles.end())
                alltext << " underline=\"single\"";
            if ((*niceText).elevation() == SUPERSCRIPT)
                alltext << "><sup";
            else if ((*niceText).elevation() == SUBSCRIPT)
                alltext << "><sub";
            alltext << ">" << t;
            g_free(t);

            if ((*niceText).elevation() == SUPERSCRIPT)
                alltext << "</sup>";
            else if ((*niceText).elevation() == SUBSCRIPT)
                alltext << "</sub>";
            alltext << "</span>";
        }
    }

    const string alltextstring = alltext.str();

    const char* glyphs = alltextstring.c_str();
    const size_t len   = alltextstring.length();

    GError* pError = 0;
    PangoAttrList* pAttrList;
    char* pText = 0;

    pango_parse_markup(glyphs, len, 0, &pAttrList, &pText, NULL, &pError);
    if (pError) {
        MagLog::warning() << "CAIRO-PANGO: " << pError->message << "\n  for text: " << glyphs << endl;
        return;
    }

    PangoLayout* layout = pango_cairo_create_layout(cr_);
    pango_layout_set_text(layout, pText, -1);
    pango_layout_set_attributes(layout, pAttrList);

    //  T E X T co-ordinates
    //  --> start always bottom left
    //
    unsigned int noTexts = text.size();
    for (unsigned int nT = 0; nT < noTexts; nT++)  // for all string COORDINATES
    {
        cairo_save(cr_);
        const double xxx = projectX(text[nT].x());
        const double yyy = projectY(text[nT].y());

        int w, h;
        pango_layout_get_size(layout, &w, &h);
        double width  = w / PANGO_SCALE;
        double height = h / PANGO_SCALE;

        MFloat x = 0;
        if (horizontal == Justification::CENTRE)
            x = width * .5;
        else if (horizontal == Justification::RIGHT)
            x = width;

        MFloat y = 0.;
        if (vertical == VerticalAlign::BASE) {
            y = height * .85;
        }
        else if (vertical == VerticalAlign::HALF) {
            y = height * .5;
        }
        else if (vertical == VerticalAlign::BOTTOM) {
            y = height;
        }

        cairo_move_to(cr_, xxx, yyy);
        const double angle = text.getAngle();
        if (fabs(angle) > EPSILON) {
            cairo_rotate(cr_, angle);
        }
        pango_cairo_update_layout(cr_, layout);
        cairo_rel_move_to(cr_, -x, -y);
        pango_cairo_show_layout(cr_, layout);
        cairo_restore(cr_);
    }
}

/*!
  \brief drawing a circle

  This method renders given text strings.

  The meaning of the last parameter <i>s</i> is as follows:
     - 0-8 determines how many quarters of the circle are filled. Starting from the top clock-wise.
     - 9 fills the whole circle but leaves a vertical bar empty in the middle of the circle.

  \todo check if this is right and correct colour in other drivers for case fill = 9!!!

  \param x X Position
  \param y Y Position
  \param r Radius of circle
  \param s Style which determines how the circle is shaded
*/
MAGICS_NO_EXPORT void CairoDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const {
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 2, 0)
    const MFloat xx = projectX(x);
    const MFloat yy = projectY(y);

    cairo_save(cr_);
    cairo_new_sub_path(cr_);

    cairo_set_line_width(cr_, currentLineWidth_);
    int fill = s;
    if (fill == 10) {
        cairo_set_line_width(cr_, currentLineWidth_ * 5);
        fill = 0;
    }
    cairo_set_source_rgba(cr_, currentColour_.red(), currentColour_.green(), currentColour_.blue(),
                          currentColour_.alpha());

    if ((s > 0) && (fill != 9)) {
        cairo_arc(cr_, xx, yy, r, -M_PI * .5, M_PI * ((0.25 * fill) - .5));
        cairo_line_to(cr_, xx, yy);
        cairo_fill(cr_);
    }

    if (fill == 9) {
        cairo_arc(cr_, xx - 0.5, yy, r, M_PI * .5, -M_PI * .5);
        cairo_fill(cr_);
        cairo_arc(cr_, xx + 0.5, yy, r, -M_PI * .5, M_PI * .5);
        cairo_fill(cr_);
    }

    cairo_arc(cr_, xx, yy, r, 0., M_PI * 2.);
    cairo_stroke(cr_);
    cairo_restore(cr_);
#else
    MagLog::warning() << "CairoDriver::circle requires at least cairo version 1.2!" << endl;
#endif
}

/*!
  \brief render pixmaps

  This method renders pixmaps. These are used for cell shading and raster input (GIFs and PNGs).

  \note Normally implemented in BaseDriver
  \sa renderCellArray()

  \param x0 x of lower corner
  \param y0 y of lower corner
  \param x1 x of higher corner
  \param y1 y of higher corner
  \param w width of pixmap
  \param h height of pixmap
  \param pixmap contents
  \param alpha transparency
*/
MAGICS_NO_EXPORT bool CairoDriver::renderPixmap(MFloat x0, MFloat y0, MFloat x1, MFloat y1, int w, int h,
                                                unsigned char* pixmap, int, bool alpha, bool offset) const {
    MagLog::debug() << "CD:renderPixmap> " << w << "x" << h << endl;
    unsigned char* p = pixmap;
    const MFloat dx  = (x1 - x0) / w;
    const MFloat dy  = (y1 - y0) / h;  // Minus needed for Y axis correction
    MFloat a         = 1.;

    if(offset){
        x0 += offsetX_;
        y0 += offsetY_;
    }

    cairo_save(cr_);
//    cairo_antialias_t t = cairo_get_antialias(cr_);
//    cairo_set_antialias(cr_, CAIRO_ANTIALIAS_NONE);

    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            const MFloat r = *(p++);
            const MFloat g = *(p++);
            const MFloat b = *(p++);
            if (alpha)   a = *(p++);

            //if ((r * g * b) > 0)
            {
                if (!alpha) cairo_set_source_rgb( cr_, r, g, b);
                else        cairo_set_source_rgba(cr_, r, g, b, a);

                const MFloat x = x0 + (j * dx);
                const MFloat y = y0 + (i * dy);
                cairo_rectangle(cr_, x, y, dx, dy);
                cairo_stroke_preserve(cr_);
                cairo_fill(cr_);
            }
        }
    }
    cairo_restore(cr_);
//    cairo_set_antialias(cr_, t);
    return true;
}

/*!
  \brief Image render method for ALL drivers.

  This method should be used by all Magics drivers to render image objects.
*/
MAGICS_NO_EXPORT void CairoDriver::renderImage(const ImportObject& obj) const {
    std::string file = obj.getPath();

    if (!magCompare(obj.getFormat(), "png")) {
        std::string cmd = "convert " + file + "[1] ___magics_cairo_temp.png";
        MagLog::info() << "CairoDriver::renderImage calling convert ... with: " << cmd << endl;
        int status = system(cmd.c_str());
        if (status) {
            MagLog::error() << "\nCairoDriver: Command exit not zero - NO PNG produced!\n"
                            << " COMMAND: " << cmd << "\n"
                            << endl;
            return;
        }
        file = "___magics_cairo_temp.png";
    }

    cairo_surface_t* image = cairo_image_surface_create_from_png(file.c_str());

    if (image) {
        cairo_save(cr_);
        int w = cairo_image_surface_get_width(image);
        int h = cairo_image_surface_get_height(image);

        const MFloat oow = (obj.getWidth()  < 0) ? w / coordRatioX_ : obj.getWidth();
        const MFloat ooh = (obj.getHeight() < 0) ? h / coordRatioY_ : obj.getHeight();
        const MFloat x   = projectX(obj.getOrigin().x());
        const MFloat y   = projectY(obj.getOrigin().y());
        const MFloat oh  = fabs(projectY(obj.getOrigin().y() + ooh) - y);
        const MFloat ow  = fabs(projectX(obj.getOrigin().x() + oow) - x);

        const ImageProperties::OriginReference ori = obj.getOriginReference();
        if (ori == ImageProperties::centre)
            cairo_translate(cr_, x - (ow * .5), y - (oh * .5));
        else
            cairo_translate(cr_, x, y - oh);
        cairo_scale(cr_, ow / w, oh / h);
        cairo_set_source_surface(cr_, image, 0, 0);
        cairo_paint(cr_);

        cairo_surface_destroy(image);
        cairo_restore(cr_);
        if (magCompare(file, "___magics_cairo_temp.png"))
            remove("___magics_cairo_temp.png");
    }
    else
        MagLog::warning() << "CairoDriver-> Could NOT read the image file " << file << " !" << endl;
}


/*!
  \brief render cell arrays for cell shading

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool CairoDriver::renderCellArray(const Image& image) const {
    MagLog::debug() << "CD:renderCellArray> " << image.getWidth() << "x" << image.getHeight() << endl;
    ColourTable& lt  = image.getColourTable();
    const int width  = image.getNumberOfColumns();
    const int height = image.getNumberOfRows();
    const MFloat x0  = projectX(image.getOrigin().x());
    const MFloat y0  = projectY(image.getOrigin().y());
    const MFloat scX = (image.getWidth() * coordRatioX_) / width;
    const MFloat scY = (image.getHeight() * coordRatioY_) / height;
    const double wid = projectX(image.getOrigin().x() + image.getWidth()) - projectX(image.getOrigin().x());
    const double hei = projectY(image.getOrigin().y() + image.getHeight()) - projectY(image.getOrigin().y());

    cairo_save(cr_);
    cairo_translate(cr_, x0, y0);

    if ((width > 300) || (height > 300))  // high resoltion enough to use image
    {
        cairo_surface_t* result = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
        if (cairo_surface_status(result) != CAIRO_STATUS_SUCCESS) {
            MagLog::warning() << "CAIRO:renderImage> cannot create surface (" << width << "x" << height << ")" << endl;
            return result;
        }
        cairo_surface_flush(result);

        unsigned char* current_row = cairo_image_surface_get_data(result);
        int stride                 = cairo_image_surface_get_stride(result);

        for (unsigned int h = 0; h < height; h++) {
            uint32_t* row = (uint32_t*)current_row;
            for (unsigned int w = 0; w < width; w++) {
                const short c = image[w + (width * h)];
                double al     = lt[c].alpha();
                if ((lt[c].red() * lt[c].green() * lt[c].blue() < 0.))
                    al = 0.;  // missing data will be fully transparent
                const uint32_t cr    = (uint32_t)(al * lt[c].red() * 255.);
                const uint32_t cg    = (uint32_t)(al * lt[c].green() * 255.);
                const uint32_t cb    = (uint32_t)(al * lt[c].blue() * 255.);
                const uint32_t alint = (uint32_t)(al * 255.);
                row[w]               = (alint << 24) | (cr << 16) | (cg << 8) | cb;
            }
            current_row += stride;
        }
        cairo_surface_mark_dirty(result);

        cairo_scale(cr_, scX, -scY);
        cairo_set_source_surface(cr_, result, 0, 0);
        cairo_paint(cr_);

        cairo_surface_destroy(result);
    }
    else {
        cairo_antialias_t t = cairo_get_antialias(cr_);
        cairo_set_antialias(cr_, CAIRO_ANTIALIAS_NONE);
        for (unsigned int h = 0; h < height; h++) {
            for (unsigned int w = 0; w < width; w++) {
                const short c  = image[w + (width * h)];
                const float cr = lt[c].red();
                const float cg = lt[c].green();
                const float cb = lt[c].blue();
                if (cr * cg * cb >= 0) {
                    cairo_set_source_rgba(cr_, cr, cg, cb, lt[c].alpha());
                    cairo_set_line_width(cr_, 0.01);
                    cairo_rectangle(cr_, w * scX, h * -scY, scX, -scY);
                    cairo_fill_preserve(cr_);
                    cairo_stroke(cr_);
                }
            }
        }
        cairo_set_antialias(cr_, t);
    }

    cairo_restore(cr_);
    return true;
}


/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void CairoDriver::debugOutput(const string& s) const {
    MagLog::debug() << s << endl;
}

/*!
  \brief class information are given to the output-stream
*/
void CairoDriver::print(ostream& out) const {
    out << "CairoDriver[";
    out << "]";
}

//! Method to plot symbols
/*!
 Needs special treatment of MagLogo.
*/
MAGICS_NO_EXPORT void CairoDriver::renderSymbols(const Symbol& symbol) const {
    debugOutput("Start CairoDriver Symbols");

    const string symbolName = symbol.getSymbol();
    const string logo       = "logo_";
    if (symbolName.find(logo) == std::string::npos) {
        BaseDriver::renderSymbols(symbol);
    }
    else {
        string logofile;
        if (symbolName == "logo_cams")
            logofile = buildSharePath("CAMS_combined.png");
        else if (symbolName == "logo_c3s")
            logofile = buildSharePath("C3S_combined.png");
        else
            logofile = buildSharePath("ecmwf_logo_2014.png");

        cairo_surface_t* image = cairo_image_surface_create_from_png(logofile.c_str());
        cairo_status_t ret     = cairo_surface_status(image);

        if (!ret) {
            cairo_save(cr_);
            cairo_translate(cr_, projectX(symbol[0].x()), projectY(symbol[0].y()) - convertCM(symbol.getHeight() * .3));
            const MFloat sizeY = -convertCM(symbol.getHeight() * .1) * coordRatioY_;
            const int w        = cairo_image_surface_get_width(image);
            const int h        = cairo_image_surface_get_height(image);
            const double ratio = w / h;
            const MFloat sizeX = sizeY * ratio;
            cairo_scale(cr_, sizeX / w, sizeY / h);
            cairo_set_source_surface(cr_, image, 0, 0);
            cairo_paint(cr_);
            cairo_surface_destroy(image);
            cairo_restore(cr_);
        }
        else
            MagLog::warning() << "CairoDriver - Could NOT read logo " << logofile << endl;
    }
}

MAGICS_NO_EXPORT bool CairoDriver::convertToPixmap(const string& fname, const GraphicsFormat format, const int reso,
                                                   const MFloat wx0, const MFloat wy0, const MFloat wx1,
                                                   const MFloat wy1) const {
    if (format == GraphicsFormat::PNG) {
        cairo_save(cr_);
        cairo_surface_t* image = cairo_image_surface_create_from_png(fname.c_str());
        int w                  = cairo_image_surface_get_width(image);
        int h                  = cairo_image_surface_get_height(image);

        cairo_translate(cr_, wx0, wy0);
        cairo_scale(cr_, (wx1 - wx0) / w, -(wy1 - wy0) / h);

        cairo_set_source_surface(cr_, image, 0, 0);
        cairo_paint(cr_);

        cairo_surface_destroy(image);
        cairo_restore(cr_);
        return true;
    }
    else
        return BaseDriver::convertToPixmap(fname, format, reso, wx0, wy0, wx1, wy1);
}

static SimpleObjectMaker<CairoDriver, BaseDriver> Cairo_driver("Cairo");
