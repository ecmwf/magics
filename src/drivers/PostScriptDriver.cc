/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PostScriptDriver.cc
    \brief Implementation of PostScriptDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: March 2004

*/

#include "PostScriptDriver.h"
#include <iomanip>
#include "Image.h"
#include "Polyline.h"
#include "Symbol.h"
#include "System.h"
#include "Text.h"

/*! \brief function to convert between PS ISO encoding and Unicode

See <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">entities.html</a> for
a description of the characters.
\sa TextNode.cc
\note C O P I E D   F R O M  TEXTNODE.CC
*/
string specialPS(const string& sp) {
    static map<string, string> specialsPS;
    if (specialsPS.empty()) {
        specialsPS["©"]   = "251";  // copyright
        specialsPS["°"]   = "260";  // degres
        specialsPS["956"] = "265";  // mu
        specialsPS["¼"]   = "274";  // vulgar fraction one quarter
        specialsPS["½"]   = "275";  // vulgar fraction one half
        specialsPS["¾"]   = "276";  // vulgar fraction three quarters
        specialsPS["¿"]   = "277";  // inverted question mark
        specialsPS["À"]   = "300";  // latin capital letter A with grave
        specialsPS["Á"]   = "301";  // latin capital letter A with acute

        specialsPS["Ã"] = "303";  // latin capital letter A with tilde
        specialsPS["Ä"] = "304";  // latin capital letter A with diaeresis
        specialsPS["Å"] = "305";  // latin capital letter A with ring above
        specialsPS["Æ"] = "306";  // latin capital letter AE
        specialsPS["Ç"] = "307";  // latin capital letter C with cedilla

        specialsPS["È"] = "310";  // latin capital letter E with grave
        specialsPS["É"] = "311";  // latin capital letter E with acute
        specialsPS["Ê"] = "312";  // latin capital letter E with circumflex
        specialsPS["Ë"] = "313";  // latin capital letter E with  diaeresis

        specialsPS["Ì"] = "314";  // latin capital letter I with grave
        specialsPS["Í"] = "315";  // latin capital letter I with acute
        specialsPS["Î"] = "316";  // latin capital letter I with circumflex
        specialsPS["Ï"] = "317";  // latin capital letter I with  diaeresis

        specialsPS["Ñ"] = "321";  // latin capital letter N with tilde

        specialsPS["Ò"] = "322";  // latin capital letter O with grave
        specialsPS["Ó"] = "323";  // latin capital letter O with acute
        specialsPS["Ô"] = "325";  // latin capital letter O with  diaeresis
        specialsPS["Õ"] = "330";  // latin capital letter O slash

        specialsPS["Ù"] = "331";  // latin capital letter U with grave
        specialsPS["Ú"] = "332";  // latin capital letter U with acute
        specialsPS["Û"] = "333";  // latin capital letter U with circumflex
        specialsPS["Ü"] = "334";  // latin capital letter U with  diaeresis

        specialsPS["Ý"] = "335";  // latin capital letter Y with acute

        specialsPS["Þ"] = "336";  // latin capital letter THORN
        specialsPS["ß"] = "337";  // latin small letter sharp s = ess-zed

        specialsPS["à"] = "340";  // latin small letter a with grave
        specialsPS["á"] = "341";  // latin small letter a with cute
        specialsPS["â"] = "342";  // latin small letter a with circumflex
        specialsPS["ã"] = "343";  // latin small letter a with tilde
        specialsPS["ä"] = "344";  // latin small letter a with  diaeresis
        specialsPS["å"] = "345";  // latin small letter a with ring above
        specialsPS["æ"] = "346";  // latin small letter ae
        specialsPS["ç"] = "347";  // latin small letter c with cedilla

        specialsPS["è"] = "350";  // latin small letter e with grave
        specialsPS["é"] = "351";  // latin small letter e with cute
        specialsPS["ê"] = "352";  // latin small letter e with circumflex
        specialsPS["ë"] = "353";  // latin small letter e with ring above

        specialsPS["ì"] = "354";  // latin small letter i with grave
        specialsPS["í"] = "355";  // latin small letter i with cute
        specialsPS["î"] = "356";  // latin small letter i with circumflex
        specialsPS["ï"] = "357";  // latin small letter i with diaeresis

        specialsPS["ñ"] = "361";  // latin small letter n with tilde

        specialsPS["ò"] = "362";  // latin small letter o with grave
        specialsPS["ó"] = "363";  // latin small letter o with cute
        specialsPS["ö"] = "364";  // latin small letter o with diaeresis
        specialsPS["õ"] = "365";  // latin small letter o with tilde

        specialsPS["ø"] = "370";  // latin small letter o slash

        specialsPS["ù"] = "371";  // latin small letter u with grave
        specialsPS["ú"] = "372";  // latin small letter u with cute
        specialsPS["û"] = "373";  // latin small letter u with circumflex
        specialsPS["ü"] = "374";  // latin small letter u with diaeresis

        specialsPS["ý"] = "375";  // latin small letter y with acute

        specialsPS["þ"] = "376";  // latin small letter THORN
        specialsPS["ÿ"] = "377";  // latin small letter y with diaeresis
    }

    const map<string, string>::iterator f = specialsPS.find(sp);
    return (f == specialsPS.end()) ? "" : "\\" + f->second;
}


using namespace magics;

/*!
  \brief Constructor

The PostScript driver produces one or more (if split is activated) text files
in PostScript format.

*/
PostScriptDriver::PostScriptDriver() : ps_(true), pdf_(false), eps_(false), maxPathSize_(200), deviceColourModel_(1) {
    readFonts();
}

/*!
  \brief Destructor
*/
PostScriptDriver::~PostScriptDriver() {
    relieveFonts();
}

/*!
  \brief Opening the driver
*/
void PostScriptDriver::open() {
    currentPage_ = 0;
    setCMscale(300. / 2.54);  // cm -> pixel / based on 300 DPI
    if (!isSplit())
        openFile();
}

/*!
  \brief Closing the driver
*/
void PostScriptDriver::close() {
    if (!isSplit())
        closeFile();

    currentPage_ = 0;  // reset values for new pages ...
    coordRatioX_ = 1.;
    coordRatioY_ = 1.;
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up. Strongly
  depends on what output was selected (split or eps)
*/
MAGICS_NO_EXPORT void PostScriptDriver::startPage() const {
    dimensionX_ = convertCM(getXDeviceLength());  // 72   = points / inch
    dimensionY_ = convertCM(getYDeviceLength());  // 2.54 = cm / inch

    MFloat resolution = 300.;  // work with 300 DPI
    MFloat ratio      = 1.;
    int width         = 0;
    string mbg_tmpl   = mgb_template_;

    if (!mbg_tmpl.empty()) {
        setDimensionsFromBinary(mbg_tmpl, ratio, width);
        setCMscale(35.);
        resolution  = 80.;  // dpi for web
        dimensionX_ = width;
        dimensionY_ = maground(dimensionX_ * ratio);
    }

    newPage_ = true;
    if (isSplit())
        openFile();
    currentPage_++;
    fstream* ps = getStream();

    if (!isSplit())
        *ps << "%%Page: " << currentPage_ << " " << currentPage_ << "\n";
    else
        *ps << "%%Page: 1 1\n";

    // Here the whole page gets scaled to the resolution!
    *ps << "gs " << 72. / resolution << " dup s ";

    if (!isEPS() && (dimensionX_ > dimensionY_))
        *ps << static_cast<int>(dimensionY_) << " 0 t 90 ro ";

    *ps << "1 lw [] 0 sd ";

    setDeviceColourModel(colour_model_);

    *ps << "2 setlinejoin 0 1 SUP 0 10 SF 0 SHA 0 SVA\n";  // MITER is now always on
    *ps << "0 0 0 0 Y n 0 0 m " << dimensionX_ << " 0 rl 0 " << dimensionY_ << " rl " << -dimensionX_
        << " 0 rl cp fill\n";  // force white background
    currentColour_ = Colour("none");
    if (scale_ < 1.0) {
        *ps << dimensionX_ * .5 << " " << dimensionY_ * .5 << " t\n";
        *ps << scale_ << " " << scale_ << " s\n";
        *ps << -dimensionX_ * .5 << " " << -dimensionY_ * .5 << " t\n";
    }
}

/*!
  \brief ending a page

  This method has to take care that for formats with multiple output
  files are closed.
*/
MAGICS_NO_EXPORT void PostScriptDriver::endPage() const {
    fstream* ps = getStream();
    *ps << "S\n";
    debugOutput("End of page");
    if (isSplit())
        closeFile();
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void PostScriptDriver::project(const magics::Layout& layout) const {
    currentWrittenColour_ = Colour("NONE");
    debugOutput("Begin layout " + layout.name());

    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    scalesX_.push(coordRatioX_);
    scalesY_.push(coordRatioY_);

    MFloat offsetX_ = (layout.x() * 0.01 * dimensionX_);
    MFloat offsetY_ = (layout.y() * 0.01 * dimensionY_);
    dimensionX_     = layout.width() * 0.01 * dimensionX_;
    dimensionY_     = layout.height() * 0.01 * dimensionY_;

    const MFloat sumX = layout.maxX() - layout.minX();
    const MFloat sumY = layout.maxY() - layout.minY();

    if (sumX != 0 && sumY != 0) {
        coordRatioX_ = (dimensionX_ / sumX);
        coordRatioY_ = (dimensionY_ / sumY);
    }

    const MFloat X_ = offsetX_ + projectX(-layout.minX());
    const MFloat Y_ = offsetY_ + projectY(-layout.minY());

    fstream* ps = getStream();
    *ps << "gs";
    if (layout.clipp()) {
        *ps << " " << offsetX_ << " " << offsetY_ << " " << dimensionX_ << " " << dimensionY_ << " rectclip";
    }
    *ps << " " << X_ << " " << Y_ << " t";
    *ps << "\n";
}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

*/
MAGICS_NO_EXPORT void PostScriptDriver::unproject() const {
    currentWrittenColour_ = Colour("NONE");
    dimensionY_           = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();
    coordRatioX_ = scalesX_.top();
    scalesX_.pop();
    coordRatioY_ = scalesY_.top();
    scalesY_.pop();

    fstream* ps = getStream();
    *ps << "gr\n";
    setLineParameters(LineStyle::SOLID, 1);
    debugOutput("End layout");
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void PostScriptDriver::setNewColour(const Colour& colour) const {
    if (currentColour_ == colour)
        return;
    currentColour_ = colour;
}

MAGICS_NO_EXPORT void PostScriptDriver::writeColour() const {
    if (currentWrittenColour_ == currentColour_)
        return;
    currentWrittenColour_ = currentColour_;
    MFloat c, m, y, k, gray;
    const MFloat r = currentColour_.red();
    const MFloat g = currentColour_.green();
    const MFloat b = currentColour_.blue();

    fstream* ps   = getStream();
    streamsize ss = ps->precision(2);
    switch (getDeviceColourModel()) {
        case 0:  // rgb
            *ps << r << " " << g << " " << b << " C\n";
            break;
        case 1:  // CMYK
            c = 1. - r;
            m = 1. - g;
            y = 1. - b;
            k = (c < m) ? c : m;
            k = (y < k) ? y : k;
            if (k == 1.)
                *ps << "0 0 0 1 Y\n";
            else {
                c = (c - k) / (1. - k);
                m = (m - k) / (1. - k);
                y = (y - k) / (1. - k);
                *ps << c << " " << m << " " << y << " " << k << " Y\n";
            }
            break;
        case 2:  // monochrome - RGB
            if ((r == 1.) && (g == 1.) && (b == 1.))
                *ps << "1 1 1 C\n";
            else
                *ps << "0 0 0 C\n";
            break;
        case 3:  // RGB gray
            gray = 0.3 * r + 0.59 * g + 0.11 * b;
            *ps << gray << " " << gray << " " << gray << " C\n";
            break;
        case 4:  // monochrome - CMYK
            if ((r == 1.) && (g == 1.) && (b == 1.))
                *ps << "0 0 0 0 Y\n";
            else
                *ps << "0 0 0 1 Y\n";
            break;
        case 5:  // CMYK gray
            gray = 0.3 * r + 0.59 * g + 0.11 * b;
            *ps << "0 0 0 " << gray << " Y\n";
            break;
        default:  // RGB
            *ps << r << " " << g << " " << b << " C\n";
            break;
    }  // end switch
    ps->precision(ss);
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void PostScriptDriver::setNewLineWidth(const MFloat width) const {
    if (currentLineWidth_ == width)
        return;
    currentLineWidth_ = width;

    fstream* ps = getStream();
    *ps << currentLineWidth_ << " lw\n";
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT void PostScriptDriver::setLineParameters(const LineStyle linestyle, const MFloat w) const {
    setNewLineWidth(w);
    if (currentLineType_ == linestyle)
        return;
    currentLineType_ = linestyle;

    fstream* ps = getStream();

    const int width = (int)(currentLineWidth_ + .5);
    const int sw    = (currentLineWidth_ > 2.) ? 1 : 0;

    switch (currentLineType_) {
        case LineStyle::SOLID:
            *ps << "[] 0 sd\n";
            break;
        case LineStyle::DASH:
            *ps << "[" << ((sw) ? 4 * width : 16) << " " << ((sw) ? 1 * width : 8) << "] 8 sd\n";
            break;
        case LineStyle::DOT:
            *ps << "[" << ((sw) ? width : 4) << " " << ((sw) ? width : 8) << "] 4 sd\n";
            break;
        case LineStyle::CHAIN_DASH:
            *ps << "[" << ((sw) ? width * 4 : 16) << " " << ((sw) ? width * 1 : 8) << " " << ((sw) ? width * 1 : 4)
                << " " << ((sw) ? width * 1 : 8) << " "
                << "] 0 sd\n";
            break;
        case LineStyle::CHAIN_DOT:
            *ps << "[" << ((sw) ? width * 4 : 12) << " " << ((sw) ? width * 1 : 8) << " " << ((sw) ? width * 1 : 4)
                << " " << ((sw) ? width * 1 : 8) << " " << ((sw) ? width * 1 : 4) << " " << ((sw) ? width * 1 : 8)
                << " "
                << "] 0 sd\n";
            break;
        default:
            *ps << "[] 0 sd\n";
            break;
    }  // end switch
    return;
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
MAGICS_NO_EXPORT void PostScriptDriver::renderPolyline(const int n, MFloat* x, MFloat* y) const {
    if (n < 2 || (currentColour_ == Colour("NONE")))
        return;
    writeColour();

    MFloat* xx = x;
    MFloat* yy = y;

    std::fstream* ps = getStream();

    if (n == 2) {
        const MFloat ix0 = projectX(*xx);
        xx++;
        const MFloat iy0 = projectY(*yy);
        yy++;
        const MFloat dx0 = projectX(*xx) - ix0;
        const MFloat dy0 = projectY(*yy) - iy0;
        if (zero(dx0) && zero(dy0))
            return;
        *ps << dx0 << " " << dy0 << " " << ix0 << " " << iy0 << " B\n";
    }
    else {
        int nn = n;
        MFloat *dx, *dy;
        streamsize ss = ps->precision(2);

        while (nn > 1) {
            unsigned int p = (nn > int(maxPathSize_)) ? maxPathSize_ : nn;
            dx             = new MFloat[p + 1];
            dy             = new MFloat[p + 1];

            MFloat kx = projectX(*xx);
            xx++;
            MFloat ky = projectY(*yy);
            yy++;

            *(dx++) = kx;
            *(dy++) = ky;

            unsigned int i;
            for (i = 1; i < p; i++) {
                const MFloat cx = projectX(*xx);
                xx++;
                *(dx++)         = cx - kx;
                kx              = cx;
                const MFloat cy = projectY(*yy);
                yy++;
                *(dy++) = cy - ky;
                ky      = cy;
            }

            int counter = 0;  // to avoid to long lines
            for (i = p - 1; i > 0; i--) {
                const MFloat ddx = *(--dx);
                const MFloat ddy = *(--dy);

                if (!(zero(ddx) && zero(ddy))) {
                    *ps << ddx << " " << ddy << " ";
                    counter++;
                }
                else
                    p--;

                if (counter > 15) {
                    *ps << "\n";
                    counter = 0;
                }
            }
            --dx;
            --dy;

            if (p > 1)
                *ps << p - 1 << " " << *dx << " " << *dy << " p\n";

            nn = nn - maxPathSize_;
            if (++nn > 1) {
                // Compensate for additional point (last becomes first)
                --xx;
                --yy;
            }
            delete[] dx;
            delete[] dy;
        }  // end while
        ps->precision(ss);
    }
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
MAGICS_NO_EXPORT void PostScriptDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const {
    if (n != 2 || (currentColour_ == Colour("NONE")))
        return;
    writeColour();

    MFloat* xx = x;
    MFloat* yy = y;

    const MFloat ix0 = *xx;
    xx++;
    const MFloat iy0 = *yy;
    yy++;
    const MFloat dx0 = *xx - ix0;
    const MFloat dy0 = *yy - iy0;

    if (zero(dx0) && zero(dy0))
        return;

    std::fstream* ps = getStream();
    *ps << dx0 << " " << dy0 << " " << ix0 << " " << iy0 << " B\n";
}

/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param line polyline to be filled
*/
void PostScriptDriver::renderSimplePolygon(const magics::Polyline& line) const {
    unsigned int n = line.size();
    setNewColour(line.getFillColour());
    line.getShading()->draw(*this);

    if (n < 3 || (currentColour_ == Colour("NONE")))
        return;
    writeColour();

    std::fstream* ps = getStream();
    *ps << "gs\n";

    if (currentShading_ == Shading::DOT) {
        const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
        const int density               = (int)sqrt(pro->density_);
        if (density <= 0)
            return;
        const MFloat square_size = convertCM(1.) / density;

        int s = (int)pro->size_ * convertCM(1.);
        if (s < 2)
            s = 2;

        const MFloat r = currentColour_.red();
        const MFloat g = currentColour_.green();
        const MFloat b = currentColour_.blue();

        MFloat c = 1. - r;
        MFloat m = 1. - g;
        MFloat z = 1. - b;
        MFloat k = (c < m) ? c : m;
        k        = (z < k) ? z : k;

        *ps << "/Pat {\n gs 0 0 " << square_size << " " << square_size << " rectclip gr gs ";
        if (k == 1.)
            *ps << "0 0 0 1";
        else {
            c = (c - k) / (1. - k);
            m = (m - k) / (1. - k);
            z = (z - k) / (1. - k);
            *ps << c << " " << m << " " << z << " " << k;
        }
        *ps << " setcmykcolor 1 1 m 0 " << s << " rl " << s << " 0 rl 0 -" << s << " rl cp fill gr "
            << "} bind def\n"
            << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
            << "/BBox [0 0 " << square_size << " " << square_size << "] /XStep " << square_size << " /YStep "
            << square_size << "\n"
            << "/PaintProc { Pat }\n"
            << ">>\n"
            << "matrix makepattern setpattern\n";
    }
    else if (currentShading_ == Shading::HATCH) {
        const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
        indexHatch_                       = pro->index_;
        if (indexHatch_ < 1 || indexHatch_ > 6) {
            MagLog::warning() << "PostScriptDriver::renderSimplePolygon > Hatch index " << indexHatch_
                              << " is wrong. No hatch shading possible!" << endl;
            if (indexHatch_ == 0)
                MagLog::debug() << "PostScriptDriver::renderSimplePolygon > Hatch index is 0. Alternative hatch "
                                   "patterns between 1-6 should have been sent!"
                                << endl;
            return;
        }
        const int s = (int)(pro->density_);

        const MFloat r = currentColour_.red();
        const MFloat g = currentColour_.green();
        const MFloat b = currentColour_.blue();

        MFloat c = 1. - r;
        MFloat m = 1. - g;
        MFloat z = 1. - b;
        MFloat k = (c < m) ? c : m;
        k        = (z < k) ? z : k;

        *ps << "/Pat {\n gs 0 0 " << s << " " << s << " rectclip gr gs ";
        if (k == 1.)
            *ps << "0 0 0 1";
        else {
            c = (c - k) / (1. - k);
            m = (m - k) / (1. - k);
            z = (z - k) / (1. - k);
            *ps << c << " " << m << " " << z << " " << k;
        }

        *ps << " setcmykcolor";

        if (indexHatch_ == 1 || indexHatch_ == 3)  // horizontal
        {
            *ps << " 0 " << s * .5 << " m " << s << " 0 rl st";
        }
        if (indexHatch_ == 2 || indexHatch_ == 3) {
            *ps << " " << s * .5 << " 0 m 0 " << s << " rl st";
        }
        if (indexHatch_ == 4 || indexHatch_ == 6) {
            *ps << " 0 0 m " << s << " " << s << " rl st";
        }
        if (indexHatch_ == 5 || indexHatch_ == 6) {
            *ps << " 0 " << s << " m " << s << " -" << s << " rl st";
        }

        *ps << " gr } bind def\n"
            << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
            << "/BBox [0 0 " << s << " " << s << "] /XStep " << s << " /YStep " << s << "\n"
            << "/PaintProc { Pat }\n"
            << ">>\n"
            << "matrix makepattern setpattern\n";
    }  // end hatch

    *ps << "n ";

    magics::Polyline::Holes::const_iterator h  = line.beginHoles();
    magics::Polyline::Holes::const_iterator he = line.endHoles();

    for (; h != he; ++h) {
        vector<double> x;
        vector<double> y;
        line.hole(h, x, y);
        MFloat old_x    = projectX(x[0]);
        MFloat old_y    = projectY(y[0]);
        unsigned int nt = x.size();

        int pcounter = 0;
        for (int i = nt - 1; i > -1; --i) {
            const MFloat xx    = projectX(x[i]);
            const MFloat yy    = projectY(y[i]);
            const MFloat diffX = old_x - xx;
            const MFloat diffY = old_y - yy;

            if (!(zero(diffX) && zero(diffY))) {
                *ps << diffX << " " << diffY << " ";
                old_x = xx;
                old_y = yy;
                pcounter++;
                if (pcounter % 10 == 0)
                    *ps << "\n";
            }
        }
        *ps << pcounter << " " << projectX(x[0]) << " " << projectY(y[0]) << " F P\n";
    }

    if ((line.get(n - 1).x() == line.get(0).x()) && (line.get(n - 1).y() == line.get(0).y()))
        n--;

    MFloat old_x = projectX(line.get(0).x());
    MFloat old_y = projectY(line.get(0).y());

    const MFloat mx = old_x;
    const MFloat my = old_y;
    int pcounter    = 0;
    for (int i = n - 1; i > -1; --i) {
        const PaperPoint& pp = line.get(i);
        const MFloat xx      = projectX(pp.x());
        const MFloat yy      = projectY(pp.y());
        const MFloat diffX   = old_x - xx;
        const MFloat diffY   = old_y - yy;

        if (!(zero(diffX) && zero(diffY))) {
            *ps << diffX << " " << diffY << " ";
            old_x = xx;
            old_y = yy;
            if (pcounter % 10 == 0)
                *ps << "\n";
            pcounter++;
        }
    }
    *ps << pcounter << " " << mx << " " << my << " F P\n";
    *ps << "E gr\n";
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
MAGICS_NO_EXPORT void PostScriptDriver::renderSimplePolygon(const int n, MFloat* x, MFloat* y) const {
    if (n < 3 || (currentColour_ == Colour("NONE")))
        return;

    int nn = n;
    if ((x[nn - 1] == x[0]) && (y[nn - 1] == y[0]))
        nn--;

    const int N = nn + 1;
    MFloat* rx  = new MFloat[N];
    MFloat* ry  = new MFloat[N];
    MFloat* dx  = rx;
    dx++;
    MFloat* dy = ry;
    dy++;
    MFloat *xx = x, *yy = y;
    MFloat fx = projectX(*(xx++));
    MFloat fy = projectY(*(yy++));

    int i;
    for (i = 1; i < nn; i++) {
        const MFloat pxx = projectX(*xx);
        const MFloat pyy = projectY(*yy);
        *(dx++)          = pxx - fx;
        fx               = pxx;
        xx++;
        *(dy++) = pyy - fy;
        fy      = pyy;
        yy++;
    }

    std::fstream* ps = getStream();

    if (currentShading_ == Shading::DOT) {
        const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
        const int density               = (int)sqrt(pro->density_);
        if (density <= 0) {
            if (density < 0)
                MagLog::warning() << "PostScriptDriver::renderSimplePolygon > Dot density " << density
                                  << " is negative! No shading applied" << endl;
            return;
        }
        const MFloat square_size = convertCM(1.) / density;

        int s = (int)pro->size_ * convertCM(1.) * .2;
        if (s < 2)
            s = 2;

        const MFloat r = currentColour_.red();
        const MFloat g = currentColour_.green();
        const MFloat b = currentColour_.blue();

        MFloat c = 1. - r;
        MFloat m = 1. - g;
        MFloat z = 1. - b;
        MFloat k = (c < m) ? c : m;
        k        = (z < k) ? z : k;

        *ps << "gs  %%\n";

        *ps << "/Pat {\n gs 0 0 " << square_size << " " << square_size << " rectclip gr gs ";
        if (k == 1.)
            *ps << "0 0 0 1";
        else {
            c = (c - k) / (1. - k);
            m = (m - k) / (1. - k);
            z = (z - k) / (1. - k);
            *ps << c << " " << m << " " << z << " " << k;
        }
        *ps << " setcmykcolor 1 1 m 0 " << s << " rl " << s << " 0 rl 0 -" << s << " rl cp fill gr "
            << "} bind def\n"
            << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
            << "/BBox [0 0 " << square_size << " " << square_size << "] /XStep " << square_size << " /YStep "
            << square_size << "\n"
            << "/PaintProc { Pat }\n"
            << ">>\n"
            << "matrix makepattern setpattern\n";
    }
    else if (currentShading_ == Shading::HATCH) {
        const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
        indexHatch_                       = pro->index_;
        if (indexHatch_ < 1 || indexHatch_ > 6) {
            MagLog::warning() << "PostScriptDriver::renderSimplePolygon > Hatch index " << indexHatch_
                              << " is wrong. No hatch shading possible!" << endl;
            return;
        }
        const int s = (int)(pro->density_);

        const MFloat r = currentColour_.red();
        const MFloat g = currentColour_.green();
        const MFloat b = currentColour_.blue();

        MFloat c = 1. - r;
        MFloat m = 1. - g;
        MFloat z = 1. - b;
        MFloat k = (c < m) ? c : m;
        k        = (z < k) ? z : k;

        *ps << "gs\n"
            << "/Pat {\n gs 0 0 " << s << " " << s << " rectclip gr gs ";
        if (k == 1.)
            *ps << "0 0 0 1";
        else {
            c = (c - k) / (1. - k);
            m = (m - k) / (1. - k);
            z = (z - k) / (1. - k);
            *ps << c << " " << m << " " << z << " " << k;
        }

        *ps << " setcmykcolor";

        if (indexHatch_ == 1 || indexHatch_ == 3)  // horizontal
        {
            *ps << " 0 " << s * .5 << " m " << s << " 0 rl st";
        }
        if (indexHatch_ == 2 || indexHatch_ == 3) {
            *ps << " " << s * .5 << " 0 m 0 " << s << " rl st";
        }
        if (indexHatch_ == 4 || indexHatch_ == 6) {
            *ps << " 0 0 m " << s << " " << s << " rl st";
        }
        if (indexHatch_ == 5 || indexHatch_ == 6) {
            *ps << " 0 " << s << " m " << s << " -" << s << " rl st";
        }

        *ps << " gr } bind def\n"
            << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
            << "/BBox [0 0 " << s << " " << s << "] /XStep " << s << " /YStep " << s << "\n"
            << "/PaintProc { Pat }\n"
            << ">>\n"
            << "matrix makepattern setpattern\n";
    }  // end hatch
    else {
        *ps << "gs\n";
    }

    *dx   = projectX(x[0]) - projectX(x[nn - 1]);
    *dy   = projectY(y[0]) - projectY(y[nn - 1]);
    rx[0] = projectX(x[0]);
    ry[0] = projectY(y[0]);

    for (i = nn; i > 0; i--) {
        *ps << (*dx--) << " " << *(dy--) << " ";
    }
    *ps << nn << " " << rx[0] << " " << ry[0] << " e gr\n";

    delete[] rx;
    delete[] ry;
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \note As of version 2.0 there are two forms of describing text in Text.
  \todo Underlining of text

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void PostScriptDriver::renderText(const Text& text) const {
    if (text.empty())
        return;
    const vector<NiceText>& niceT = text.getNiceText();
    if (niceT.empty())
        return;
    currentWrittenColour_ = Colour("NONE");  // reset colours

    fstream* ps   = getStream();
    streamsize ss = ps->precision(2);

    *ps << int(text.getJustification()) << " SHA " << int(text.getVerticalAlign()) << " SVA ";

    vector<NiceText>::const_iterator niceText    = text.textBegin();
    vector<NiceText>::const_iterator niceTextEnd = text.textEnd();

    int counterSubStrings = 0;
    ostringstream all_text;
    for (; niceText < niceTextEnd;) {
        all_text << (*niceText).text();
        niceText++;
        counterSubStrings++;
    }

    niceText          = text.textBegin();
    MFloat old_offset = 0;
    int count         = 0;

    for (; niceText < niceTextEnd;) {
        const MagFont magfont = (*niceText).font();
        MFloat height         = convertCM(magfont.size());
        if (height < EPSILON) {
            ++niceText;
            continue;
        }

        const std::set<string>& styles = magfont.styles();

        string style = "";
        if (styles.find("bold") != styles.end())
            style = "bold";
        else if (styles.find("italic") != styles.end())
            style += "italic";
        else if (styles.find("bolditalic") != styles.end())
            style += "bolditalic";
        if (style == "")
            style = "normal";
        const string lowFont = lowerCase(magfont.name() + "_" + style);

        fontMapIter iter = FontMap_.find(lowFont);

        const bool underlined = (styles.find("underlined") != styles.end()) ? true : false;

        int font;
        if (iter != FontMap_.end())
            font = iter->second.id;
        else {
            font = 0;  // if not found get default
            MagLog::warning() << "PostScriptDriver: Font " << lowFont
                              << " is not registered!\n   Default font used for " << (*niceText).text() << "." << endl;
        }

        setNewColour(magfont.colour());
        writeColour();

        MFloat offset = 0;
        if ((*niceText).elevation() == NORMAL) {
            offset     = -old_offset;
            old_offset = 0;
        }
        else if ((*niceText).elevation() == SUPERSCRIPT) {
            offset     = height * .5 - old_offset;
            old_offset = offset;
            height *= .8;
        }
        else if ((*niceText).elevation() == SUBSCRIPT) {
            offset     = -height * .2 - old_offset;
            old_offset = offset;
            height *= .8;
        }

        *ps << font << " " << static_cast<int>(height) << " SF ";

        // plot strings
        string textCommand = (text.getBlanking()) ? "TB" : "T";
        if (underlined)
            textCommand = "TU";

        int len  = (*niceText).text().length() + 1;
        char* pp = new char[len];
        strcpy(pp, (*niceText).text().c_str());
        string spp = (*niceText).text();
        char* p    = pp;
        ostringstream tmp;
        int counter = 0;

        while (*p) {
            if (*p == '(') {
                tmp << "\\(";
            }
            else if (*p == ')') {
                tmp << "\\)";
            }
            else if (*p == '\\') {
                tmp << "\\\\";
            }
            else if (*p & 0x80) {
                tmp << specialPS(spp.substr(counter, 2));
            }  // temp fix for multibyte char (degree sign)
            else {
                tmp << *p;
            }
            p++;
            counter++;
        }
        delete[] pp;

        const string showCommand = (underlined) ? "ushow" : "show";
        unsigned int noTexts     = text.size();
        for (unsigned int nT = 0; nT < noTexts; nT++)  // for all string CO-ORDINATES
        {
            if (niceText == text.textBegin())  // if first text string
            {
                const MFloat x0    = projectX(text[nT].x());
                const MFloat y0    = projectY(text[nT].y()) + offset;
                const MFloat angle = 360. - (text.getAngle() * 57.29577951);

                *ps << "gs " << x0 << " " << y0 << " t ";
                if (angle != 0.)
                    *ps << angle << " ro ";

                if (counterSubStrings > 1) {
                    *ps << "(" << all_text.str() << ") stringwidth pop HA mul VA Height mul moveto "
                        << "(" << tmp.str() << ") " << showCommand << "\n";
                }
                else {
                    *ps << "(" << tmp.str() << ") 0 0 " << textCommand << "\n";
                }
            }
            else  // all other substrings
            {
                *ps << " 0 " << offset << " rmoveto\n";
                *ps << "(" << tmp.str() << ") " << showCommand << "\n";
            }
            count++;
            if (niceText + 1 == text.textEnd())
                *ps << "gr\n";
        }
        niceText++;
    }  // endfor all nicetexts
    ps->precision(ss);
    currentColour_ = Colour("none");
}

/*!
  \brief drawing a circle

  This method renders given text strings.

  The meaning of the last parameter <i>s</i> is as follows:
     - 0-8 determines how many quarters of the circle are filled. Starting from the top clock-wise.
     - 9 fills the whole circle but leaves a vertical bar empty in the middle of the circle.

  \param x X Position
  \param y Y Position
  \param r Radius of circle
  \param s Style which determines how the circle is shaded
*/
MAGICS_NO_EXPORT void PostScriptDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const {
    writeColour();
    std::fstream* ps = getStream();
    const MFloat cx  = projectX(x);
    const MFloat cy  = projectY(y);

    if (s < 8) {
        *ps << "n " << cx << " " << cy << " " << r << " 0 360 arc st\n";
        if (s > 0)
            *ps << "n " << cx << " " << cy << " m " << cx << " " << cy << " " << r << " 90 " << 90 - (s * 45)
                << " arn\n";
    }
    else
        *ps << "n " << cx << " " << cy << " " << r << " 0 360 ar\n";
    if (s == 9) {
        *ps << "1 1 1 C n " << cx << " " << cy + r - 1 << " m 0 " << -r - r + 2 << " rl st\n";
        const Colour col = currentColour_;
        currentColour_   = Colour("white");
        writeColour();
        setNewColour(col);
    }
}

/*!
  \brief render pixmaps

  This method renders pixmaps. These are used for cell shading and raster input (GIFs and PNGs).

  \sa renderCellArray()

  \param x0 x of lower corner
  \param y0 y of lower corner
  \param x1 x of higher corner
  \param y1 y of higher corner
  \param width width of pixmap
  \param height height of pixmap
  \param pixmap contents
  \param landscape says if contents is landscape
  \param alpha transparency of array
*/
MAGICS_NO_EXPORT bool PostScriptDriver::renderPixmap(MFloat x0, MFloat y0, MFloat x1, MFloat y1, int width, int height,
                                                     unsigned char* pixmap, int landscape, bool alpha, bool ) const {
    if (landscape)  // swop w/h
    {
        const int x = width;
        width       = height;
        height      = x;
    }
    if (height == 0 || width == 0)
        return false;

    unsigned char* p    = pixmap;
    std::fstream* ps    = getStream();
    const int col_model = getDeviceColourModel();
    const MFloat dx     = x1 - x0 + 1;
    const MFloat dy     = y1 - y0 + 1;

    *ps << "gs /pic " << width * ((col_model == 1) ? 4 : 3) << " string def " << x0 << " " << y0 << " t " << dx << " "
        << dy << " s " << width << " " << height << " 8\n"
        << "[" << width << " 0 0 " << height << " 0 0] "
        << "{currentfile pic readhexstring pop}"
        << " false " << ((col_model == 1) ? 4 : 3) << " colorimage\n";

    char* t = new char[9];
    int nl  = 0;
    for (int j = height - 1; j >= 0; j--) {
        for (int i = width - 1; i >= 0; i--) {
            // Get image left-right and bottom-up
            int n = (landscape) ? (height * i + j) * 3 : (j * width + width - 1 - i) * 3;
            if (alpha)
                n = (landscape) ? (height * i + j) * 4 : (j * width + width - 1 - i) * 4;
            unsigned char* p2 = p + n;
            unsigned char r   = *(p2++);
            unsigned char g   = *(p2++);
            unsigned char b   = *(p2++);
            if (alpha)
                p2++;  // ignore alpha values ... :-(
            short kr, kg, kb, kc, km, ky, kk;
            MFloat cc, cm, cy, ck;

            switch (col_model) {
                case 0:
                    sprintf(t, "%02hhx%02hhx%02hhx", r, g, b);
                    break;
                case 1:
                    cc = 1. - (r * 0.00392156);
                    cm = 1. - (g * 0.00392156);
                    cy = 1. - (b * 0.00392156);
                    ck = (cc < cm) ? cc : cm;
                    if (cy < ck)
                        ck = cy;
                    if (ck == 1.) {
                        kc = 0;
                        km = 0;
                        ky = 0;
                        kk = 255;
                    }
                    else {
                        kc = int(((cc - ck) / (1. - ck)) * 255.);
                        km = int(((cm - ck) / (1. - ck)) * 255.);
                        ky = int(((cy - ck) / (1. - ck)) * 255.);
                        kk = int(ck * 255.);
                    }
                    sprintf(t, "%02hx%02hx%02hx%02hx", kc, km, ky, kk);
                    break;
                case 2:
                    if ((r == 255) && (g == 255) && (b == 255)) {
                        kr = 255;
                        kg = 255;
                        kb = 255;
                    }
                    else {
                        kr = 0;
                        kg = 0;
                        kb = 0;
                    }
                    sprintf(t, "%02hx%02hx%02hx", kr, kg, kb);
                    break;
                case 3:
                    kr = int(0.3 * r + 0.59 * g + 0.11 * b);
                    sprintf(t, "%02hx%02hx%02hx", kr, kr, kr);
                    break;
                default:
                    sprintf(t, "%02hhx%02hhx%02hhx", r, g, b);
                    break;
            }
            *ps << t;
            if ((++nl) % 12 == 0)
                *ps << "\n";
        }
    }
    *ps << "gr" << std::endl;
    currentColour_ = Colour("none");
    delete[] t;
    return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool PostScriptDriver::renderCellArray(const Image& image) const {
    ColourTable& lt  = image.getColourTable();
    const int width  = image.getNumberOfColumns();
    const int height = image.getNumberOfRows();
    const MFloat x0  = image.getOrigin().x();
    const MFloat y0  = image.getOrigin().y();
    const double scX = (image.getWidth()) / width;
    const double scY = -(image.getHeight()) / height;

    fstream* ps = getStream();
    *ps << "gs" << std::endl;

    for (unsigned int h = 0; h < height; h++) {
        for (unsigned int w = 0; w < width; w++) {
            const short c  = image[w + (width * h)];
            const float cr = lt[c].red();
            const float cg = lt[c].green();
            const float cb = lt[c].blue();
            if (cr * cg * cb >= 0) {
                setNewColour(Colour(cr, cg, cb));
                writeColour();
                const MFloat xx0 = x0 + (w * scX);
                const MFloat yy0 = y0 + (h * scY);

                MFloat x[4] = {xx0, xx0 + scX, xx0 + scX, xx0};
                MFloat y[4] = {yy0, yy0, yy0 + scY, yy0 + scY};
                renderSimplePolygon(4, &x[0], &y[0]);
            }
        }
    }
    *ps << "gr" << std::endl;

    return true;
}


/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void PostScriptDriver::debugOutput(const string& s) const {
    if (debug_)
        pFile_ << "%% " << s << "\n";
}

/*!
  \brief class information are given to the output-stream
*/
void PostScriptDriver::print(ostream& out) const {
    out << "PostScriptDriver[";
    out << "]";
}

//! Method to plot symbols
/*!

*/
MAGICS_NO_EXPORT void PostScriptDriver::renderSymbols(const Symbol& symbol) const {
    setNewColour(symbol.getColour());
    writeColour();
    currentShading_         = Shading::SOLID;
    const string symbolName = symbol.getSymbol();
    const string logo       = "logo_ecmwf";
    if (symbolName.find(logo) == std::string::npos) {
        BaseDriver::renderSymbols(symbol);
    }
    else {
        pFile_ << " " << projectX(symbol[0].x()) << " " << projectY(symbol[0].y()) - convertCM(symbol.getHeight() * .3)
               << " t 0.4 0.4 s" << endl;
        pFile_ << "gs\n"
               << "183.473 76.074 m 183.473 63.121 l 146.492 63.121 l 146.492 48.105 l 180.438 48.105 l 180.438 36.094 "
                  "l 146.492 36.094 l 146.492 18.977 l 184.262 18.977\n"
               << "l 184.262 6.023 l 131.086 6.023 l 131.086 76.074 l P 183.473 76.074 m fill\n"
               << "234.574 57.387 m 233.656 58.859 232.492 60.172 231.141 61.262 c 228.277 63.559 224.711 64.805 "
                  "221.039 64.793 c 217.973 64.891 214.934 64.199 212.211\n"
               << "62.793 c 209.84 61.496 207.82 59.645 206.324 57.398 c 204.781 55.047 203.668 52.441 203.043 49.699 "
                  "c 202.359 46.773 202.023 43.773 202.039 40.77 c 202.023\n"
               << "37.875 202.359 34.992 203.043 32.18 c 203.68 29.504 204.793 26.961 206.324 24.672 c 207.832 22.441 "
                  "209.855 20.605 212.219 19.316 c 214.945 17.91 217.984\n"
               << "17.219 221.051 17.316 c 225.762 17.316 229.434 18.754 232.062 21.629 c 234.844 24.812 236.543 "
                  "28.797 236.918 33.012 c 251.832 33.012 l 251.523\n"
               << "28.91 250.508 24.895 248.828 21.141 c 247.281 17.738 245.082 14.68 242.352 12.129 c 239.625 9.621 "
                  "236.422 7.688 232.934 6.445 c 229.121 5.086 225.105\n"
               << "4.41 221.059 4.441 c 216.094 4.363 211.164 5.312 206.586 7.234 c 202.457 8.988 198.758 11.613 "
                  "195.742 14.934 c 192.75 18.289 190.449 22.203 188.977\n"
               << "26.453 c 187.379 31.043 186.582 35.871 186.625 40.73 c 186.578 45.684 187.375 50.605 188.977 55.293 "
                  "c 190.449 59.605 192.746 63.586 195.742 67.016 c\n"
               << "198.734 70.395 202.43 73.074 206.566 74.875 c 214.637 78.219 223.621 78.641 231.973 76.074 c "
                  "235.309 75.016 238.43 73.371 241.191 71.223 c 243.934\n"
               << "69.062 246.215 66.375 247.906 63.324 c 249.742 59.988 250.883 56.316 251.262 52.531 c 236.344 "
                  "52.531 l 236.098 54.254 235.492 55.906 234.574 57.387 c P 234.574 57.387 m fill\n"
               << "279.66 76.074 m 296.047 27.906 l 296.246 27.906 l 311.742 76.074 l 333.426 76.074 l 333.426 6.004 l "
                  "319 6.004 l 319 55.664 l 318.801 55.664 l 301.633\n"
               << " 6.023 l 289.762 6.023 l 272.594 55.176 l 272.395 55.176 l 272.395 6.023 l 257.977 6.023 l 257.977 "
                  "76.074 l P 279.66 76.074 m fill\n"
               << "394.41 6.023 m 382.586 53.703 l 382.398 53.703 l 370.715 6.023 l 355.121 6.023 l 336.578 76.098 l "
                  "351.984 76.098 l 363.066 28.418 l 363.27 28.418\n"
               << " l 375.391 76.074 l 389.816 76.074 l 401.816 27.824 l 402.02 27.824 l 413.488 76.074 l 428.594 "
                  "76.074 l 409.758 6.004 l 394.41 6.004 l P 394.41 6.023 m fill\n"
               << "481 76.074 m 481 63.121 l 447.156 63.121 l 447.156 46.938 l 476.496 46.938 l 476.496 34.922 l "
                  "447.164 34.922 l 447.164 6.023 l 431.758 6.023 l 431.758\n"
               << " 76.074 l 481.012 76.074 l P 481 76.074 m fill\n"
               << "0.402 36.836 m 34.848 36.836 l 34.848 46.164 l 0.402 46.164 l -0.137 43.078 -0.137 39.922 0.402 "
                  "36.836 c P 0.402 36.836 m fill\n"
               << "115.121 33.59 m 111.5 14.293 94.715 0.258 75.078 0.117 c 69.25 0.215 63.5 1.465 58.16 3.801 c "
                  "52.703 1.328 46.773 0.086 40.781 0.156 c 21.168 0.16\n"
               << " 4.344 14.148 0.762 33.43 c 25.875 33.43 l 28.719 27.184 34.949 23.176 41.812 23.18 c 49.266 23.312 "
                  "55.867 28.02 58.422 35.023 c 60.812 28.211 67.102\n"
               << " 23.539 74.312 23.211 c 81.527 22.883 88.215 26.965 91.215 33.531 c P 115.121 33.59 m fill\n"
               << "115.121 49.41 m 111.496 68.73 94.676 82.77 75.02 82.883 c 69.207 82.801 63.473 81.543 58.16 79.188 "
                  "c 52.715 81.699 46.777 82.961 40.781 82.883\n"
               << "c 21.148 82.891 4.305 68.879 0.742 49.57 c 25.855 49.57 l 28.688 55.84 34.934 59.867 41.812 59.859 "
                  "c 49.273 59.723 55.875 55 58.422 47.988 c 60.82 54.793\n"
               << " 67.113 59.465 74.324 59.789 c 81.535 60.117 88.223 56.031 91.227 49.469 c P 115.121 49.41 m fill\n"
               << "gr" << endl;
    }
}


/*!
    \note The file header can only be written after the Fonts have been read

    \sa open() startPage()
*/
MAGICS_NO_EXPORT void PostScriptDriver::openFile() const {
    if (!isSplit())
        fileName_ = getFileName("ps");
    else {
        if (!isEPS())
            fileName_ = getFileName("ps", currentPage_ + 1);
        else
            fileName_ = getFileName("eps", currentPage_ + 1);
    }
    if (isPDF()) {
        const string::size_type pos = fileName_.rfind(".pdf");
        if (pos != string::npos)
            fileName_.replace(pos, 4, ".ps");
    }

    if (pFile_.is_open())
        pFile_.close();
    pFile_.clear();
    pFile_.open(fileName_.c_str(), std::ios::out);
    if (!pFile_) {
        MagLog::error() << " PostScriptDriver --> Cannot write output file to what was specified: " << fileName_
                        << endl;
        MagLog::error() << "";
        throw CannotOpenFile(fileName_);
    }

    pFile_.setf(ios_base::fixed);
    pFile_.unsetf(ios::showpoint);
    pFile_.precision(2);
    writePSFileHeader();
}

/*!
    \brief Method to close the PostScript output file.

    \sa close() endPage()
*/
MAGICS_NO_EXPORT void PostScriptDriver::closeFile() const {
    // write end of file
    writePSFileEnd();

    // close + remove files
    pFile_.close();

    const string fps = fileName_;

    if (isPDF()) {
        const string::size_type pos = fileName_.rfind(".ps");
        if (pos != string::npos)
            fileName_.replace(pos, 3, ".pdf");
        printOutputName("PS pdf " + fileName_);

        // the -q option means no output - warnings may not show up!!! (fonts)
        string cmd = "( gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite -sOutputFile=";
        cmd.append(fileName_);
        cmd.append(" -c .setpdfwrite -f ");
        cmd.append(fps);
        cmd.append(" )");

        int status = system(cmd.c_str());
        if (status) {
            MagLog::error() << "\nPostScriptDriver: Command exit not zero - NO PDF produced!\n"
                            << " COMMAND: " << cmd << "\n"
                            << endl;
            setPS(true);
        }
    }
    if (!isPS() && !isEPS())
        remove(fps.c_str());
    else {
        if (isPS())
            printOutputName("PS ps " + fps);
        else
            printOutputName("PS eps " + fps);
    }
}


/*!
   \brief Method writing the PostScript file header.
*/
MAGICS_NO_EXPORT void PostScriptDriver::writePSFileHeader() const {
    fstream* ps = getStream();
    const SystemInfo info;

    *ps << "%!PS-Adobe-3.0";
    if (isEPS())
        *ps << " EPSF-3.0";
    *ps << "\n%%Title: " << title_ << "\n%%Creator: ";
    if (!output_creator_.empty())
        *ps << output_creator_ << " and ";
    *ps << getMagicsVersionString() << "\n%%CreationDate: " << info.getTime() << "\n%%For: "
        << info.getUserID() << "@" << info.getHostName() << "\n";

    MFloat dimensionX = getXDeviceLength() * 72. / 2.54;  // 72   = points / inch
    MFloat dimensionY = getYDeviceLength() * 72. / 2.54;  // 2.54 = cm / inch

    string orientation = (getXDeviceLength() < getYDeviceLength()) ? "Portrait" : "Landscape";

    MFloat ratio    = 1.;
    int width       = 0;
    string mbg_tmpl = mgb_template_;

    if (!mbg_tmpl.empty()) {
        setDimensionsFromBinary(mbg_tmpl, ratio, width);
        dimensionX  = width;
        dimensionY  = maground(width * ratio);
        orientation = (dimensionX < dimensionY) ? "Portrait" : "Landscape";
    }

    if (isEPS()) {
        *ps << "%%LanguageLevel: 2\n%%Pages: 1\n"
            << "%%BoundingBox: 0 0 " << static_cast<int>(dimensionX) << " " << static_cast<int>(dimensionY) + 1 << "\n";
    }
    else {
        if (isPDF() && !isPS()) {
            *ps << "%%Orientation: " << orientation << "\n%%LanguageLevel: 2\n%%Pages: 1\n";
        }
        else {
            MFloat big, small;
            if (dimensionX > dimensionY) {
                big   = dimensionX;
                small = dimensionY;
            }
            else {
                big   = dimensionY;
                small = dimensionX;
            }

            *ps << "%%Orientation: " << orientation << "\n%%LanguageLevel: 2\n%%Pages: (atend)\n"
                << "%%BoundingBox: 0 0 " << static_cast<int>(small) + 1 << " " << static_cast<int>(big) + 1 << "\n";
        }
    }

    *ps << "%%EndComments\n%%BeginProlog\n";
    if (isEPS()) {
        *ps << "save\n"
            << "countdictstack\n"
            << "mark\n"
            << "newpath\n"
            << "/showpage {} def\n"
            << "/setpagedevice {pop} def\n";
    }

    *ps << "/S { gr showpage } def /m {moveto} def /st {stroke} def /rl {rlineto} def /ro {rotate} def /cp {closepath} "
           "def /d { {rmoveto rlineto} repeat stroke} bind def /gr {grestore} def /gs {gsave} def /n { newpath } def\n"
        << "/sa {save} def /lw {setlinewidth } def /ar {arc fill} def /arn {arcn fill} def /l { lineto } bind def /c { "
           "curveto } bind def\n"
        << "/sd {setdash} def /C { setrgbcolor } def /Y { setcmykcolor } def  /B { moveto rlineto stroke } bind def "
           "/BB { moveto lineto stroke } bind def /t { translate } def /s {scale} def /K { /UY exch def /UX exch def "
           "/LY exch def \n"
        << "/LX exch def gsave newpath LX LY moveto UX LY lineto UX UY lineto LX UY lineto closepath newpath } def /lp "
           "{ moveto rlineto } bind def /p { moveto {rlineto} repeat stroke} bind def /po { moveto {rlineto} repeat } "
           "bind def\n"
        << "/q {moveto rlineto stroke} bind def /f {moveto {rlineto} repeat fill} bind def /e {moveto {rlineto} repeat "
           "eofill} bind def /F {moveto {rlineto} repeat} bind def /E {eofill} bind def /P { closepath } bind def\n"
        << "/SAVEMT matrix def\n"
        << "/Degreevec\n"
        << "[\n"
        << "	8#100 /at 8#251 /copyright 8#260 /degree 8#306 /AE 8#301 /Aacute 8#304 /Adieresis 8#300 /Agrave 8#305 "
           "/Aring 8#303 /Atilde 8#307 /Ccedilla 8#311 /Eacute 8#312 /Ecircumflex 8#313 /Edieresis 8#310 /Egrave\n"
        << "	8#320 /Eth 8#315 /Iacute 8#316 /Icircumflex 8#317 /Idieresis 8#314 /Igrave 8#321 /Ntilde 8#323 /Oacute "
           "8#325 /Odieresis 8#322 /Ograve 8#330 /Oslash 8#325 /Otilde 8#336 /Thorn 8#332 /Uacute 8#333 /Ucircumflex\n"
        << "	8#334 /Udieresis 8#331 /Ugrave 8#335 /Yacute 8#341 /aacute 8#342 /acircumflex 8#222 /acute 8#264 "
           "/acute 8#344 /adieresis 8#346 /ae 8#340 /agrave 8#345 /aring 8#343 /atilde\n"
        << "	8#226 /breve 8#246 /brokenbar 8#237 /caron 8#347 /ccedilla 8#270 /cedilla 8#242 /cent 8#223 "
           "/circumflex 8#244 /currency 8#250 /dieresis 8#227 /dotaccent 8#220 /dotlessi\n"
        << "	8#351 /eacute 8#352 /ecircumflex 8#350 /egrave 8#360 /eth 8#241 /exclamdown 8#337 /germandbls 8#221 "
           "/grave 8#253 /guillemotleft 8#273 /guillemotright 8#235 /hungarumlaut\n"
        << "	8#255 /hyphen 8#355 /iacute 8#356 /icircumflex 8#357 /idieresis 8#354 /igrave 8#254 /logicalnot 8#257 "
           "/macron 8#265 /mu\n"
        << "	8#327 /multiply 8#361 /ntilde 8#363 /oacute 8#364 /odieresis 8#236 /ogonek 8#362 /ograve 8#275 "
           "/onehalf 8#274 /onequarter 8#271 /onesuperior 8#252 /ordfeminine\n"
        << "	8#272 /ordmasculine 8#370 /oslash 8#365 /otilde 8#266 /paragraph 8#267 /periodcentered 8#261 "
           "/plusminus 8#277 /questiondown 8#256 /registered 8#232 /ring 8#247 /section 8#243 /sterling\n"
        << "	8#376 /thorn 8#276 /threequarters 8#263 /threesuperior 8#224 /tilde 8#262 /twosuperior 8#372 /uacute "
           "8#373 /ucircumflex 8#374 /udieresis 8#371 /ugrave 8#375 /yacute 8#377 /ydieresis 8#245 /yen\n"
        << "] def\n"
        << "/reencsmalldict 12 dict def\n"
        << "/ReEncodeSmall\n"
        << "{	reencsmalldict begin\n"
        << "	/basefontname exch def\n"
        << "	/basefontdict basefontname findfont def\n"
        << "	/newfont basefontdict maxlength dict def\n"
        << "	basefontdict\n"
        << "	{ exch dup /FID ne\n"
        << "		{ dup /Encoding eq\n"
        << "			{ exch dup length array copy newfont 3 1 roll put}\n"
        << "		{exch newfont 3 1 roll put}\n"
        << "		ifelse\n"
        << "	}\n"
        << "	{ pop pop }\n"
        << "	ifelse\n"
        << "	} forall\n"
        << "	newfont /FontName /Magicsfontname put\n"
        << "	Degreevec aload pop\n"
        << "	Degreevec length 2 idiv\n"
        << "	{newfont /Encoding get 3 1 roll put\n"
        << "	} repeat\n"
        << "	/Magicsfontname newfont definefont pop\n"
        << "	end\n"
        << "} def\n"
        << "/SF\n"
        << "{\n"
        << "/Height exch def\n"
        << "/Font exch def\n";

    fontMapIter mapit;

    for (mapit = FontMap_.begin(); mapit != FontMap_.end(); mapit++)
        *ps << "Font " << (*mapit).second.id << " eq { /" << (*mapit).second.ps_name << " } if\n";

    *ps << "ReEncodeSmall /Magicsfontname findfont Height scalefont setfont\n"
        << "} def\n"
        << "/SUP\n"
        << "{ /CHUPY exch def /CHUPX exch def } def\n"
        << "/ST\n"
        << "{ /YPOS exch def /XPOS exch def [ CHUPY CHUPX neg CHUPX CHUPY XPOS YPOS ] concat} def\n"
        << "/SHA\n"
        << "{ /a exch def a 0 eq { /HA 0 def  } if a 1 eq { /HA -0.5 def } if a 2 eq { /HA -1 def } if } def\n"
        << "/SVA\n"
        << "{ /b exch def\n"
        << "	Font  4 lt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { "
           "/VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
        << "	} if\n"
        << "	Font  8 lt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.76 def } if b 2 eq { /VA -0.725 def } if b 3 eq { "
           "/VA -0.3625 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.035 def } if\n"
        << "	} if\n"
        << "	Font 12 lt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { "
           "/VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
        << "	} if\n"
        << "	Font 12 eq { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { "
           "/VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
        << "	} if\n"
        << "	Font 12 gt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { "
           "/VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
        << "	} if\n"
        << "} def\n"
        << "/T\n"
        << "{\n"
        << "	ST\n"
        << "	/text exch def\n"
        << "	text stringwidth pop HA mul VA Height mul moveto\n"
        << "	text show\n"
        << "} def\n"
        << "/TU\n"
        << "{\n"
        << "	ST\n"
        << "	/text exch def\n"
        << "	text stringwidth pop HA mul VA Height mul moveto\n"
        << "	text show\n"
        << "	n\n"
        << "	text stringwidth pop HA mul VA Height mul moveto\n"
        << "	text stringwidth rl\n"
        << "	cp st\n"
        << "} def\n"
        << "/TB\n"
        << "{\n"
        << "	ST\n"
        << "	/text exch def\n"
        << "	text stringwidth\n"
        << "	4 add /y1 exch def\n"
        << "	4 add /x1 exch def\n"
        << "	text stringwidth pop HA mul VA Height mul moveto\n"
        << "	gs n x1 -2 add HA mul VA Height mul -2 add moveto x1 0 rlineto 0 12 Height add .7 mul rlineto x1 neg 0 "
           "rlineto cp 1 setgray fill gr\n"
        << "	text show\n"
        << "} def\n"
        << "/ushow\n"
        << "{\n"
        << "	SAVEMT currentmatrix pop\n"
        << "	/text exch def\n"
        << "	text show\n"
        << "	SAVEMT setmatrix\n"
        << "} def\n"
        << "%%EndProlog" << endl;
}

MAGICS_NO_EXPORT void PostScriptDriver::writePSFileEnd() const {
    fstream* ps = getStream();
    if (!isEPS()) {
        const int realpagenumber = (isSplit()) ? 1 : currentPage_;

        *ps << "%%Trailer\n"
            << "%%Pages: " << realpagenumber << "\n"
            << "%%EOF\n";
        ps->close();
    }
    else {
        *ps << "%%Trailer\n"
            << "cleartomark\n"
            << "countdictstack\n"
            << "exch sub { end } repeat\n"
            << "restore\n"
            << "%%EOF\n";
    }
}

MAGICS_NO_EXPORT void PostScriptDriver::setDeviceColourModel(const string& m) const {
    if (m.empty())
        deviceColourModel_ = 1;  // use default
    else if (magCompare(m, "RGB"))
        deviceColourModel_ = 0;
    else if (magCompare(m, "CMYK"))
        deviceColourModel_ = 1;
    else if (magCompare(m, "MONOCHROME"))
        deviceColourModel_ = 2;
    else if (magCompare(m, "GRAY"))
        deviceColourModel_ = 3;
    else if (magCompare(m, "CMYK_MONOCHROME"))
        deviceColourModel_ = 4;
    else if (magCompare(m, "CMYK_GRAY"))
        deviceColourModel_ = 5;
    else {
        MagLog::warning() << "PostScriptDriver::setDeviceColourModel() -> " << m
                          << " is unknown model! CMYK model is used." << endl;
        deviceColourModel_ = 1;
    }
}

static SimpleObjectMaker<PostScriptDriver, BaseDriver> PostScript_driver("PostScript");
