/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SVGDriver.cc
    \brief Implementation of SVGDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Fri Oct 26 20:58:21 2007
*/

#include <Image.h>
#include <ImportObject.h>
#include <Layer.h>
#include <Polyline.h>
#include <SVGDriver.h>
#include <Symbol.h>
#include <System.h>
#include <Text.h>
#include <iomanip>

//! For generating SVGZ files
extern "C" {
#include <sys/stat.h>
#include "minizip/zip.h"

#define MAXFILENAME 256
#define WRITEBUFFERSIZE 16384
}

#ifdef MAGICS_RASTER
#include <gd.h>
#endif


int svg_pattern_count = 0;

using namespace magics;

/*!
  \brief Constructor
*/
SVGDriver::SVGDriver() :
    groupString_(""),
    group_counter_(0),
    inkscape_(true)  //,interactive_(false), interactiveCounter_(0)
{
    readFonts();
}

/*!
  \brief Destructor
*/
SVGDriver::~SVGDriver() {
    relieveFonts();
}

/*!
  \brief Opening the driver
*/
void SVGDriver::open() {}

/*!
  \brief Closing the driver
*/
void SVGDriver::close() {}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.
*/
MAGICS_NO_EXPORT void SVGDriver::startPage() const {
    debugOutput("Page - START");
    MFloat ratio = getRatio();
    int wid      = width_;

    string mbg_tmpl = mgb_template_;
    if (!mbg_tmpl.empty()) {
        setDimensionsFromBinary(mbg_tmpl, ratio, wid);
    }

    dimensionX_      = wid;
    dimensionY_      = dimensionX_ * ratio;
    const int height = static_cast<int>(dimensionY_);
    const int width  = static_cast<int>(dimensionX_);

    setCMscale(30.);

    currentPage_++;

    string filename = getFileName("svg", currentPage_ + 1);
    tmp_pFile_      = filename;

    pFile_.open(filename.c_str(), std::ios::out);

    if (!pFile_) {
        MagLog::error() << " SVGDriver --> Cannot write output file to what was specified: " << fileName_ << endl;
        MagLog::error() << "";
        throw NoSuchFileException("Error opening SVG output file!");
    }

    svg_output_resource_list_.push_back(filename);

    const bool xml_header = true;
    if (xml_header) {
        pFile_ << "<?xml version=\"1.0\" ?>\n";
    }

    pFile_ << "<svg version=\"1.1\" encoding=\"iso-8859-1\" baseProfile=\"full\"\n"
           << "xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n"
           << "xmlns:cc=\"http://creativecommons.org/ns#\"\n"
           << "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n"
           << "xmlns=\"http://www.w3.org/2000/svg\"\n"
           << "xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n"
           << "xmlns:ev=\"http://www.w3.org/2001/xml-events\"\n";

    if (inkscape_)
        pFile_ << "xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"\n"
               << "xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"\n"
               << "inkscape:version=\"0.46\"\n"
               << "inkscape:output_extension=\"org.inkscape.output.svg.inkscape\"\n";

    if (fixSize_)
        pFile_ << "width=\"" << width << "px\" height=\"" << height << "px\" ";

    pFile_ << "viewBox=\"0 0 " << width + 1 << " " << height + 1 << "\" xml:space=\"preserve\"";
    pFile_ << ">\n<title>" << title_ << "</title>\n";

    if (!desc_.empty())
        pFile_ << "<desc>" << desc_ << "</desc>\n";
    const SystemInfo info;
    pFile_ << "<metadata id=\"MAGICSmetadata\">\n<rdf:RDF><cc:Work rdf:about=\"\">\n"
           << "\t<dc:format>image/svg+xml</dc:format>\n"
           << "\t<dc:title>" << title_ << "</dc:title>\n"
           << "\t<dc:author>" << info.getUserID() << " on " << info.getHostName() << "</dc:author>\n"
           << "\t<dc:date>" << info.getTime() << "</dc:date>\n"
           << "\t<dc:language>en-GB</dc:language>\n";
    if (!desc_.empty())
        pFile_ << "\t<dc:description>" << desc_ << "</dc:description>\n";
    if (!output_creator_.empty())
        pFile_ << "\t<dc:creator><cc:Agent><dc:title>" << output_creator_ << "</dc:title></cc:Agent></dc:creator>\n";
    pFile_ << "\t<dc:publisher><cc:Agent>\n"
           << "\t <dc:title>" << getMagicsVersionString() << "</dc:title>\n"
           << "\t</cc:Agent></dc:publisher>\n"
           << "\t<dc:coverage>Plot of meteorological data</dc:coverage>\n";
    if (!meta_.empty())
        pFile_ << "<!-- \n" << meta_ << "\n-->\n";
    pFile_ << "</cc:Work></rdf:RDF>\n</metadata>\n";
    pFile_ << "<g id=\"page\" transform=\"translate(0," << dimensionY_ << ")\" vector-effect=\"non-scaling-stroke\">\n";
}


/*!
  \brief ending a page

  This method has to take care that for formats with multiple output
  files are closed.
*/
MAGICS_NO_EXPORT void SVGDriver::endPage() const {
    group_counter_ = 0;
    debugOutput("Page - END");
    closeGroup();
    pFile_ << "</g><!--ePage-->\n";  // needed for y-axis translation in beginning!
    pFile_ << "</svg>\n";
    pFile_.close();

    layers_.clear();

    stringarray::iterator it    = svg_output_resource_list_.begin();
    stringarray::iterator itend = svg_output_resource_list_.end();
    for (; it != itend; it++) {
        printOutputName("SVG misc " + (*it));
    }
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void SVGDriver::project(const magics::Layout& layout) const {
    debugOutput("Layout - " + layout.name() + " START");
    closeGroup();

    dimensionStack_.push(dimensionX_);
    dimensionStack_.push(dimensionY_);
    const MFloat oldHeight = dimensionY_;
    scalesX_.push(coordRatioX_);
    scalesY_.push(coordRatioY_);

    const MFloat Xoff = layout.x() * 0.01 * dimensionX_;
    const MFloat Yoff = layout.y() * 0.01 * dimensionY_;
    dimensionX_       = layout.width() * 0.01 * dimensionX_;
    dimensionY_       = layout.height() * 0.01 * dimensionY_;

    const MFloat sumX = layout.maxX() - layout.minX();
    const MFloat sumY = layout.maxY() - layout.minY();

    if (sumX != 0 && sumY != 0) {
        coordRatioX_ = dimensionX_ / sumX;
        coordRatioY_ = dimensionY_ / sumY;
    }

    const MFloat x_set = Xoff + projectX(-layout.minX());
    const MFloat y_set = Yoff + projectY(-layout.minY());

    group_counter_++;

    if (layout.clipp()) {
        const double clip_height = projectY(layout.maxY()) - projectY(layout.minY());
        pFile_ << "<defs>\n"
               << " <clipPath id=\"clip_" << layout.name() << "\">\n"
               << "  <rect x=\"" << projectX(layout.minX()) << "\" y=\"" << projectY(setY(layout.minY())) - clip_height
               << "\" width=\"" << projectX(layout.maxX()) - projectX(layout.minX()) << "\" height=\"" << clip_height
               << "\" />\n"
               << " </clipPath>\n"
               << "</defs>" << endl;
    }

    pFile_ << "<g";
    if (!layout.name().empty())
        pFile_ << " id=\"" << layout.name() << "\"";
    if (!zero(x_set) || !zero(y_set))
        pFile_ << " transform=\"translate(" << x_set << "," << setY(y_set) << ")\"";

    if (layout.clipp()) {
        pFile_ << " clip-path=\"url(#clip_" << layout.name() << ")\"";
    }
    pFile_ << ">\n";

    if (layout.isNavigable()) {
        const double offsetX = projectX(-layout.minX()) + projectX(layout.minX());
        const double offsetY = projectY(layout.minY()) + projectY(layout.minY());
        layout.pushDriverInfo(offsetX, oldHeight + offsetY, dimensionX_, dimensionY_);
    }
}

MAGICS_NO_EXPORT void SVGDriver::unproject() const {
    closeGroup();
    dimensionY_ = dimensionStack_.top();
    dimensionStack_.pop();
    dimensionX_ = dimensionStack_.top();
    dimensionStack_.pop();
    coordRatioX_ = scalesX_.top();
    scalesX_.pop();
    coordRatioY_ = scalesY_.top();
    scalesY_.pop();

    pFile_ << "</g>\n";
    group_counter_--;
    debugOutput("Layout - END");
}


MAGICS_NO_EXPORT string SVGDriver::buildLayerName(const Layer* layer, string type) const {
    string s = layer->name();
    if (s.empty())
        return type;

    char chars[] = "<>";
    for (unsigned int i = 0; i < strlen(chars); ++i) {
        s.erase(std::remove(s.begin(), s.end(), chars[i]), s.end());
    }
    return s;
}


/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa Layer
*/
MAGICS_NO_EXPORT void SVGDriver::redisplay(const StaticLayer& layer) const {
    currentLayer_ = buildLayerName(&layer, string("StaticLayer"));

    newLayer();
    layer.visit(*this);
    closeLayer();
}


MAGICS_NO_EXPORT void SVGDriver::redisplay(const StepLayer& layer) const {
    currentLayer_ = buildLayerName(&layer, string("StepLayer"));

    newLayer();
    layer.visit(*this);
    closeLayer();
}


/*!
   \sa BaseDriver::redisplay(const NoDataLayer&)
 */
MAGICS_NO_EXPORT void SVGDriver::redisplay(const NoDataLayer& layer) const {
    currentLayer_ = buildLayerName(&layer, string("Coastline"));

    newLayer();
    layer.visit(*this);
    closeLayer();
}


/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.
*/
MAGICS_NO_EXPORT void SVGDriver::newLayer() const {
    closeGroup();
    debugOutput("Layer - " + currentLayer_ + " START");

    pFile_ << "<g";
    if (!currentLayer_.empty()) {
        if (inkscape_)
            pFile_ << " inkscape:groupmode=\"layer\" inkscape:label=\"" << currentLayer_ << "\"";
        pFile_ << " id=\"" << currentLayer_ << "\"><title>" << currentLayer_
               << "</title>\n";  // mark layers for SVG Edit
    }
    else
        pFile_ << ">\n";

    layers_.push_back(currentLayer_);
}

/*!
  \brief close the current layer

  This method will close an existing layer.
*/
MAGICS_NO_EXPORT void SVGDriver::closeLayer() const {
    closeGroup();
    pFile_ << "</g>\n";
    debugOutput("Layer - " + currentLayer_ + " END");
}

MAGICS_NO_EXPORT void SVGDriver::openGroup(string g) const {
    if (!magCompare(g, groupString_)) {
        closeGroup();
        pFile_ << "<g " << g << ">" << std::endl;
        groupString_ = g;
    }
}

MAGICS_NO_EXPORT void SVGDriver::closeGroup() const {
    if (!groupString_.empty()) {
        pFile_ << "</g>" << std::endl;
        groupString_ = "";
    }
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void SVGDriver::setNewColour(const Colour& colour) const {
    if (currentColour_ == colour)
        return;
    currentColour_ = colour;
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void SVGDriver::setNewLineWidth(const MFloat width) const {
    currentLineWidth_ = width * 0.5;
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT int SVGDriver::setLineParameters(const LineStyle linestyle, const MFloat w) const {
    const MFloat widi = w * 0.5;
    currentLineType_  = linestyle;

    ostringstream stream;
    stream << "stroke-width=\"" << widi << "px\" ";
    if (currentColour_.alpha() < 1.)
        stream << "stroke-opacity=\"" << currentColour_.alpha() << "\" ";
    stream << "stroke=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
           << static_cast<int>(currentColour_.green() * 255) << "," << static_cast<int>(currentColour_.blue() * 255)
           << ")\""
           << " fill=\"none\"";

    if (currentLineType_ == M_DASH)
        stream << " stroke-dasharray=\"" << 2 * widi << "," << 2 * widi << "\"";
    else if (currentLineType_ == M_DOT)
        stream << " stroke-dasharray=\"" << 1 * widi << "," << 2 * widi << "\"";
    else if (currentLineType_ == M_CHAIN_DASH)
        stream << " stroke-dasharray=\"" << 4 * widi << "," << 2 * widi << "," << 2 * widi << "," << 2 * widi << "\"";
    else if (currentLineType_ == M_CHAIN_DOT)
        stream << " stroke-dasharray=\"" << 4 * widi << "," << 2 * widi << "," << 2 * widi << "," << 2 * widi << ","
               << 2 * widi << "\"";

    openGroup(stream.str());
    return 0;
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
MAGICS_NO_EXPORT void SVGDriver::renderPolyline(const int n, MFloat* x, MFloat* y) const {
    if (n > 1 || (currentColour_ == Colour("none"))) {
        MFloat old_x = projectX(x[0]);
        MFloat old_y = setY(projectY(y[0]));
        pFile_ << "<path d=\"M" << old_x << " " << old_y;

        for (int is = 1; is < n; is++) {
            const MFloat x2    = projectX(x[is]);
            const MFloat y2    = setY(projectY(y[is]));
            const MFloat diffX = x2 - old_x;
            const MFloat diffY = y2 - old_y;

            if (x2 == old_x && x2 == projectX(x[is + 1])) {}
            else if (y2 == old_y && y2 == setY(projectY(y[is + 1]))) {
            }
            else {
                pFile_ << "l" << diffX << " " << diffY;
                old_x = x2;
                old_y = y2;
            }
        }
        pFile_ << "\"/>\n";
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
MAGICS_NO_EXPORT void SVGDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const {
    if (n != 2 || (currentColour_ == Colour("none")))
        return;
    //	closeGroup();
    const int r = static_cast<int>(currentColour_.red() * 255);
    const int g = static_cast<int>(currentColour_.green() * 255);
    const int b = static_cast<int>(currentColour_.blue() * 255);
    pFile_ << "<path stroke=\"rgb(" << r << "," << g << "," << b << ")\" d=\"M" << x[0] << " " << setY(y[0]) << "L"
           << x[1] << " " << setY(y[1]) << "\"/>\n";
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
MAGICS_NO_EXPORT void SVGDriver::renderSimplePolygon(const int n, MFloat* x, MFloat* y) const {
    if (currentColour_.alpha() < 0.01 || n < 2 || (currentColour_ == Colour("none")))
        return;
    const int r = static_cast<int>(currentColour_.red() * 255);
    const int g = static_cast<int>(currentColour_.green() * 255);
    const int b = static_cast<int>(currentColour_.blue() * 255);
    if (r * g * b < 0.)
        return;

    debugOutput("renderSimplePolygon[]");

    ostringstream gStream;
    if (currentColour_.alpha() < 1.)
        gStream << "fill-opacity=\"" << currentColour_.alpha() << "\" stroke-opacity=\"0.01\" ";
    gStream << "stroke=\"rgb(" << r << "," << g << "," << b << ")\" stroke-width=\"0.01\" fill-rule=\"evenodd\"";
    openGroup(gStream.str());

    int count    = 1;
    MFloat old_x = projectX(x[0]);
    MFloat old_y = setY(projectY(y[0]));

    double sumV = 0;
    double sumH = 0;
    ostringstream stream;
    stream << "d=\"M" << old_x << " " << old_y;

    for (int is = 1; is < n; is++) {
        const MFloat xx    = projectX(x[is]);
        const MFloat yy    = setY(projectY(y[is]));
        const MFloat diffX = xx - old_x;
        const MFloat diffY = yy - old_y;

        if (fabs(diffX) > 0.001 || fabs(diffY) > 0.001) {
            if (fabs(diffX) < 0.001)  // vertical
            {
                if (fabs(sumH) > 0.001) {
                    stream << "h" << sumH;
                    sumH = 0.;
                }  // empty horizontal
                sumV += diffY;
            }
            else if (fabs(diffY) < 0.001) {
                if (fabs(sumV) > 0.001) {
                    stream << "v" << sumV;
                    sumV = 0.;
                }
                sumH += diffX;
            }
            else {
                if (fabs(sumV) > 0.001) {
                    stream << "v" << sumV;
                    sumV = 0.;
                }
                else if (fabs(sumH) > 0.001) {
                    stream << "h" << sumH;
                    sumH = 0.;
                }
                stream << "l" << diffX << " " << diffY;
            }
            old_x = xx;
            old_y = yy;
            count++;
        }
    }
    if (fabs(sumV) > 0.001) {
        stream << "v" << sumV;
    }
    else if (fabs(sumH) > 0.001) {
        stream << "h" << sumH;
    }

    if (count > 2) {
        if (currentShading_ == M_SH_DOT) {
            const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
            const int density               = (int)sqrt(pro->density_);
            if (density <= 0)
                return;
            const MFloat square_size = 1. / density;
            const MFloat s           = pro->size_;

            pFile_ << "<pattern id=\"D_" << svg_pattern_count
                   << "\" patternUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"" << square_size << "cm\" height=\""
                   << square_size << "cm\">\n"
                   << "   <rect x=\"0cm\" y=\"0cm\" width=\"" << s << "cm\" height=\"" << s
                   << "cm\" stroke=\"none\" fill=\"rgb(" << r << "," << g << "," << b << ")\" ";
            if (currentColour_.alpha() < 1.)
                pFile_ << "fill-opacity=\"" << currentColour_.alpha() << "\" ";
            pFile_ << "/>\n"
                   << "</pattern>\n"
                   << "<path fill=\"url(#D_" << svg_pattern_count << ")\" stroke=\"none\" " << stream.str() << "\"/>\n";
            svg_pattern_count++;
        }
        else if (currentShading_ == M_SH_HATCH) {
            const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
            indexHatch_                       = pro->index_;
            const int density                 = (int)(1. / pro->density_ * 150);

            pFile_ << "<pattern id=\"H_" << svg_pattern_count
                   << "\" patternUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"" << density << "\" height=\""
                   << density << "\">\n"
                   << " <g stroke=\"rgb(" << r << "," << g << "," << b << ")\">\n";

            if (indexHatch_ == 1 || indexHatch_ == 3)  // horizontal
            {
                pFile_ << "  <polyline points=\"0," << density * .5 << " " << density << "," << density * .5
                       << "\"\n/>";
            }
            if (indexHatch_ == 2 || indexHatch_ == 3) {
                pFile_ << "  <polyline points=\"" << density * .5 << ",0 " << density * .5 << "," << density
                       << "\"\n/>";
            }
            if (indexHatch_ == 4 || indexHatch_ == 6) {
                pFile_ << "  <polyline points=\"0,0 " << density << "," << density << "\"\n/>";
            }
            if (indexHatch_ == 5 || indexHatch_ == 6) {
                pFile_ << "  <polyline points=\"" << density << "," << density << " 0,0\"\n/>";
            }
            pFile_ << " </g>\n</pattern>\n"
                   << "<path fill=\"url(#H_" << svg_pattern_count << ")\" stroke=\"none\" " << stream.str() << "\"/>\n";
            svg_pattern_count++;
        }
        else {
            pFile_ << "<path fill=\"rgb(" << r << "," << g << "," << b << ")\" ";
            pFile_ << stream.str() << "\"/>\n";
        }
    }
}

void SVGDriver::renderSimplePolygon(const magics::Polyline& line) const {
    const unsigned int n = line.size();
    if (n < 3)
        return;
    line.getShading()->draw(*this);
    setNewColour(line.getFillColour());
    if (currentColour_.alpha() < 0.01)
        return;
    debugOutput("renderSimplePolygon<Polyline>");
    const int r = static_cast<int>(currentColour_.red() * 255);
    const int g = static_cast<int>(currentColour_.green() * 255);
    const int b = static_cast<int>(currentColour_.blue() * 255);
    if (r * g * b < 0.)
        return;

    // Build group tag
    ostringstream gStream;
    gStream << "fill-rule=\"evenodd\"";
    if (currentColour_.alpha() < 1.)
        gStream << " fill-opacity=\"" << currentColour_.alpha() << "\" stroke-opacity=\"0.01\"";

    if (line.isStroked() && !(currentColour_ == Colour("NONE"))) {
        gStream << " stroke=\"rgb(" << r << "," << g << "," << b << ")\"";
    }
    else {
        gStream << " stroke=\"none\"";
    }
    openGroup(gStream.str());

    MFloat old_x = projectX(line.get(0).x());
    MFloat old_y = setY(projectY(line.get(0).y()));

    double sumV = 0.;
    double sumH = 0.;
    ostringstream stream;
    stream << "d=\"M" << old_x << " " << old_y;

    for (unsigned int i = 1; i < n; i++) {
        const PaperPoint& pp = line.get(i);
        const MFloat xx      = projectX(pp.x());
        const MFloat yy      = setY(projectY(pp.y()));
        const MFloat diffX   = xx - old_x;
        const MFloat diffY   = yy - old_y;

        if (fabs(diffX) > 0.001 || fabs(diffY) > 0.001) {
            if (fabs(diffX) < 0.001)  // vertical
            {
                if (fabs(sumH) > 0.001) {
                    stream << "h" << sumH;
                    sumH = 0.;
                }  // empty horizontal
                sumV += diffY;
            }
            else if (fabs(diffY) < 0.001) {
                if (fabs(sumV) > 0.001) {
                    stream << "v" << sumV;
                    sumV = 0.;
                }
                sumH += diffX;
            }
            else {
                if (fabs(sumV) > 0.001) {
                    stream << "v" << sumV;
                    sumV = 0.;
                }
                else if (fabs(sumH) > 0.001) {
                    stream << "h" << sumH;
                    sumH = 0.;
                }
                stream << "l" << diffX << " " << diffY;
            }
            old_x = xx;
            old_y = yy;
        }
    }
    if (fabs(sumV) > 0.001) {
        stream << "v" << sumV;
    }
    else if (fabs(sumH) > 0.001) {
        stream << "h" << sumH;
    }

    magics::Polyline::Holes::const_iterator h  = line.beginHoles();
    magics::Polyline::Holes::const_iterator he = line.endHoles();

    for (; h != he; ++h) {
        vector<double> x;
        vector<double> y;
        line.hole(h, x, y);
        if (x.empty())
            continue;
        stream << "z\nM" << projectX(x[0]) << " " << setY(projectY(y[0]));
        vector<double>::const_iterator yt = y.begin();
        vector<double>::const_iterator it = x.begin();
        ++it;
        ++yt;
        for (; it != x.end();) {
            stream << "L" << projectX(*it) << " " << setY(projectY(*yt));
            ++it;
            ++yt;
        }
    }

    ostringstream strFill;

    if (currentShading_ == M_SH_DOT) {
        const DotShadingProperties* pro = (DotShadingProperties*)currentShadingProperties_;
        const int density               = (int)sqrt(pro->density_);
        if (density <= 0)
            return;
        const MFloat square_size = 1. / density;
        const MFloat s           = pro->size_;

        pFile_ << "<pattern id=\"D_" << svg_pattern_count
               << "\" patternUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"" << square_size << "cm\" height=\""
               << square_size << "cm\">\n"
               << "   <rect x=\"0cm\" y=\"0cm\" width=\"" << s << "cm\" height=\"" << s
               << "cm\" stroke=\"none\" fill=\"rgb(" << r << "," << g << "," << b << ")\" ";
        if (currentColour_.alpha() < 1.)
            pFile_ << "fill-opacity=\"" << currentColour_.alpha() << "\" ";
        pFile_ << "/>\n"
               << "</pattern>\n";
        strFill << "url(#D_" << svg_pattern_count << ")";
        svg_pattern_count++;
    }
    else if (currentShading_ == M_SH_HATCH) {
        const HatchShadingProperties* pro = (HatchShadingProperties*)currentShadingProperties_;
        indexHatch_                       = pro->index_;
        const int density                 = (int)(1. / pro->density_ * 150);

        pFile_ << "<pattern id=\"H_" << svg_pattern_count
               << "\" patternUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"" << density << "\" height=\"" << density
               << "\">\n"
               << " <g stroke=\"rgb(" << r << "," << g << "," << b << ")\">\n";

        if (indexHatch_ == 1 || indexHatch_ == 3)  // horizontal
        {
            pFile_ << "  <polyline points=\"0," << density * .5 << " " << density << "," << density * .5 << "\"\n/>";
        }
        if (indexHatch_ == 2 || indexHatch_ == 3) {
            pFile_ << "  <polyline points=\"" << density * .5 << ",0 " << density * .5 << "," << density << "\"\n/>";
        }
        if (indexHatch_ == 4 || indexHatch_ == 6) {
            pFile_ << "  <polyline points=\"0,0 " << density << "," << density << "\"\n/>";
        }
        if (indexHatch_ == 5 || indexHatch_ == 6) {
            pFile_ << "  <polyline points=\"" << density << "," << density << " 0,0\"\n/>";
        }
        pFile_ << " </g>\n</pattern>\n";
        strFill << "url(#H_" << svg_pattern_count << ")";
        svg_pattern_count++;
    }
    else {
        strFill << "rgb(" << r << "," << g << "," << b << ")";
    }
    pFile_ << "<path fill=\"" << strFill.str() << "\" " << stream.str() << "\"/>\n";
}


/*!
  \brief renders text strings

  This method renders given text strings.

  \note As of version 2.0 there are two forms of describing text in Text.

   The'dy' parameter is used to shift text vertically because the 'basline-shift'
   parameter does not work in Firefox 1.5 + 2.0.

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void SVGDriver::renderText(const Text& text) const {
    if (text.empty())
        return;

    MFloat text_scale = 0.6;  // for Firefox 2

    const vector<NiceText>& niceT = text.getNiceText();
    if (niceT.empty())
        return;

    Justification horizontalAlign = text.getJustification();
    string justification          = "middle";
    if (horizontalAlign == MLEFT)
        justification = "start";
    else if (horizontalAlign == MRIGHT)
        justification = "end";

    VerticalAlign verticalAlign = text.getVerticalAlign();

    unsigned int noTexts                         = text.size();
    vector<NiceText>::const_iterator niceTextEnd = text.textEnd();

    for (unsigned int tex = 0; tex < noTexts; tex++)  // for all sting COORDINATES
    {
        vector<NiceText>::const_iterator niceText = text.textBegin();

        for (; niceText < niceTextEnd; niceText++) {
            const MagFont magfont = (*niceText).font();
            setNewColour(magfont.colour());
            const MFloat dheight           = magfont.size() * text_scale;
            const std::set<string>& styles = magfont.styles();
            string style                   = "normal";
            const string font              = magfont.name() + "_" + style;

            string verticalJustification = "no-change";
            if (verticalAlign == MBASE)
                verticalJustification = "alphabetic";
            else if (verticalAlign == MTOP)
                verticalJustification = "hanging";
            else if (verticalAlign == MHALF)
                verticalJustification = "middle";

            fontMapIter iter = FontMap_.find(font);
            string ttf;
            if (iter != FontMap_.end())
                ttf = iter->second.css_name;
            else {
                ttf = FontMap_["sansserif_normal"].css_name;  // if not found get default
                MagLog::warning() << "SVGDriver: Font " << font << " is not registered! Default font is used." << endl;
            }

            const double xxx = projectX(text[tex].x());
            const double yyy = setY(projectY(text[tex].y()));

            if (niceText == text.textBegin()) {
                if (tex == 0) {
                    ostringstream stream;
                    stream << "text-anchor=\"" << justification << "\" font-size=\"" << dheight << "cm\" font-family=\""
                           << ttf << "\" ";
                    if (styles.find("bolditalic") != styles.end())
                        stream << "font-weight=\"bold\" font-style=\"italic\" ";
                    else if (styles.find("bold") != styles.end())
                        stream << "font-weight=\"bold\" ";
                    else if (styles.find("italic") != styles.end())
                        stream << "font-style=\"italic\" ";
                    if (text.getAngle() != 0)
                        stream << "transform=\"rotate(" << text.getAngle() * 57.29577951 << "," << xxx << "," << yyy
                               << ")\" ";
                    stream << "fill=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
                           << static_cast<int>(currentColour_.green() * 255) << ","
                           << static_cast<int>(currentColour_.blue() * 255) << ")\"";
                    openGroup(stream.str());
                }

                pFile_ << "<text x=\"" << xxx << "\" y=\"" << yyy << "\" dominant-baseline=\"" << verticalJustification
                       << "\"";
                pFile_ << ">" << (*niceText).text();
            }
            else {
                pFile_ << "<tspan dominant-baseline=\"" << verticalJustification << "\""
                       << " font-size=\"" << dheight << "cm\" font-family=\"" << ttf << "\" ";
                if (styles.find("bolditalic") != styles.end())
                    pFile_ << "font-weight=\"bold\" font-style=\"italic\" ";
                else if (styles.find("bold") != styles.end())
                    pFile_ << "font-weight=\"bold\" ";
                else if (styles.find("italic") != styles.end())
                    pFile_ << "font-style=\"italic\" ";
                pFile_ << "fill=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
                       << static_cast<int>(currentColour_.green() * 255) << ","
                       << static_cast<int>(currentColour_.blue() * 255) << ")\""
                       << ">" << (*niceText).text() << "</tspan>";
            }
        }  // end for all strings
        pFile_ << "</text>\n";
    }  // end for all co-ordinates
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
MAGICS_NO_EXPORT void SVGDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const {
    const int cx = static_cast<int>(projectX(x));
    const int cy = setY(static_cast<int>(projectY(y)));

    ostringstream stream;

    if (s < 8 && s >= 0) {
        stream << "stroke=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
               << static_cast<int>(currentColour_.green() * 255) << "," << static_cast<int>(currentColour_.blue() * 255)
               << ")\""
               << " fill=\"none\"";
        openGroup(stream.str());

        if (s == 0) {
            pFile_ << "<circle cx=\"" << cx << "\" cy=\"" << cy << "\" r=\"" << r << "\"/>\n";
        }
        else {
            if (s == 2)
                pFile_ << "<path d=\"M" << cx << " " << cy - r << " v" << r << " h" << r << " a" << r << "," << r
                       << " 0 0 0 " << -r << "," << -r << "\" ";
            else if (s == 4)
                pFile_ << "<path d=\"M" << cx << " " << cy - r << " v" << 2 * r << " a" << r << "," << r << " 0 0 0 "
                       << 0 << "," << -2 * r << "\" ";
            else if (s == 6)
                pFile_ << "<path d=\"M" << cx << " " << cy - r << " v" << r << " h" << -r << " a" << r << "," << r
                       << " 1 1 0 " << r << "," << -r << "\" ";
            pFile_ << "fill=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
                   << static_cast<int>(currentColour_.green() * 255) << ","
                   << static_cast<int>(currentColour_.blue() * 255) << ")\""
                   << "/>\n"
                   << "<circle cx=\"" << cx << "\" cy=\"" << cy << "\" r=\"" << r
                   << "\""
                      "/>\n";
        }
    }
    else if (s == 8) {
        stream << "stroke=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
               << static_cast<int>(currentColour_.green() * 255) << "," << static_cast<int>(currentColour_.blue() * 255)
               << ")\""
               << " fill=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
               << static_cast<int>(currentColour_.green() * 255) << "," << static_cast<int>(currentColour_.blue() * 255)
               << ")\"";
        openGroup(stream.str());

        pFile_ << "<circle cx=\"" << cx << "\" cy=\"" << cy << "\" r=\"" << r << "\"/>\n";
    }
    if (s == 9) {
        stream << "stroke=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
               << static_cast<int>(currentColour_.green() * 255) << "," << static_cast<int>(currentColour_.blue() * 255)
               << ")\""
               << " fill=\"rgb(" << static_cast<int>(currentColour_.red() * 255) << ","
               << static_cast<int>(currentColour_.green() * 255) << "," << static_cast<int>(currentColour_.blue() * 255)
               << ")\"";
        openGroup(stream.str());
        pFile_ << "<circle cx=\"" << cx << "\" cy=\"" << cy << "\" r=\"" << r << "\"/>\n"
               << "<polyline points=\"" << cx << "," << cy + (r * .9) << " " << cx << "," << cy - (r * .9)
               << "\" width=\"2\" stroke=\"white\" fill=\"white\"/>" << std::endl;
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
  \param w width of pixmap
  \param h height of pixmap
  \param pixmap contents

*/
MAGICS_NO_EXPORT bool SVGDriver::renderPixmap(MFloat x0, MFloat y0, MFloat x1, MFloat y1, int w, int h,
                                              unsigned char* pixmap, int, bool) const {
    unsigned char* p = pixmap;
    const MFloat dx  = (x1 - x0) / w;
    const MFloat dy  = -(y1 - y0) / h;  // Minus needed for Y axis correction

    const MFloat X0 = x0;
    const MFloat Y0 = y0;

    debugOutput("Pixmap - START");
    pFile_ << "<g pointer-events=\"none\" >\n";

    for (int i = h - 1; i >= 0; i--) {
        for (int j = 0; j < w; x0 += dx, j++) {
            const int r = (int)*(p++);
            const int g = (int)*(p++);
            const int b = (int)*(p++);
            if (r * g * b > 0.) {
                const int x0 = static_cast<int>(X0 + (j * dx));
                const int y0 = static_cast<int>(Y0 + (i * dy));
                pFile_ << " <rect x=\"" << x0 << "\" y=\"" << setY(y0) << "\" width=\"" << dx << "\" height=\"" << dy
                       << "\""
                       << " fill=\"rgb(" << r << "," << g << "," << b << ")\" "
                       << "stroke=\"none\" />\n";
            }
        }
        x0 = X0;
        y0 += dy;
    }
    pFile_ << "</g>\n";
    debugOutput("Pixmap - END");
    return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool SVGDriver::renderCellArray(const Image& image) const {
    ColourTable& lt  = image.getColourTable();
    const int width  = image.getNumberOfColumns();
    const int height = image.getNumberOfRows();
    const MFloat x0  = projectX(image.getOrigin().x());
    const MFloat y0  = projectY(image.getOrigin().y());

    debugOutput("Cell array - START");
#ifdef MAGICS_RASTER
    if (!external_)
#endif
    {
        const MFloat x1 = projectX(image.getOrigin().x() + image.getWidth());
        const MFloat y1 = projectY(image.getOrigin().y() + image.getHeight());
        const MFloat dx = (x1 - x0) / width;
        const MFloat dy = (y1 - y0) / height;

        pFile_ << "<g pointer-events=\"none\" stroke=\"none\" transform=\"translate(" << x0 << "," << -y0 << ") scale("
               << dx << "," << dy << ")\">\n";
        for (int i = height - 1; i >= 0; i--) {
            for (int j = 0; j < width; j++) {
                const int in  = width * i + j;
                const short c = image[in];

                if (!(lt[c] == "undefined") && (lt[c].red() * lt[c].green() * lt[c].blue()) >= 0.) {
                    const int r = static_cast<int>(lt[c].red() * 255.);
                    const int g = static_cast<int>(lt[c].green() * 255.);
                    const int b = static_cast<int>(lt[c].blue() * 255.);
                    pFile_ << "<rect x=\"" << j << "\" y=\"" << i << "\" width=\"1\" height=\"1\" fill=\"#" << hex;

                    if (r == g && g == b)  // to safe 3 digits
                    {
                        if (r == 0)
                            pFile_ << "000";
                        else if (r == 255)
                            pFile_ << "fff";
                        else
                            pFile_ << r << r << r;
                    }
                    else if (((r == 0) || (r == 255)) && ((g == 0) || (g == 255)) && ((b == 0) || (b == 255))) {
                        if (r == 255)
                            pFile_ << "f";
                        else
                            pFile_ << "0";
                        if (g == 255)
                            pFile_ << "f";
                        else
                            pFile_ << "0";
                        if (b == 255)
                            pFile_ << "f";
                        else
                            pFile_ << "0";
                    }
                    else {
                        if (r > 15)
                            pFile_ << r;
                        else
                            pFile_ << "0" << r;
                        if (g > 15)
                            pFile_ << g;
                        else
                            pFile_ << "0" << g;
                        if (b > 15)
                            pFile_ << b;
                        else
                            pFile_ << "0" << b;
                    }
                    pFile_ << dec << "\"/>\n";
                }  // point has colour
            }
        }
        pFile_ << "</g>\n";
    }
#ifdef MAGICS_RASTER
    else {
        pFile_ << "<g pointer-events=\"none\" stroke=\"none\">\n";

        stringstream out;
        out << "page" << currentPage_ << "_" << currentLayer_;

        string filename = tmp_pFile_ + "_include_" + out.str() + ".png";

        ColourTable& lt  = image.getColourTable();
        const int width  = image.getNumberOfColumns();
        const int height = image.getNumberOfRows();

        gdImagePtr im = gdImageCreateTrueColor(width, height);
        gdImageColorResolveAlpha(im, 255, 255, 255, 0);

        for (int i = height - 1; i >= 0; i--) {
            for (int j = 0; j < width; j++) {
                const int in  = width * i + j;
                const short c = image[in];
                int col       = 0;
                const int r   = static_cast<int>(lt[c].red() * 255.);
                const int g   = static_cast<int>(lt[c].green() * 255.);
                const int b   = static_cast<int>(lt[c].blue() * 255.);

                col = gdImageColorResolveAlpha(im, r, g, b, 50);
            }
        }

        gdImageAlphaBlending(im, 1);
        gdImageSaveAlpha(im, 1);  // save transparency

        svg_output_resource_list_.push_back(filename);

        FILE* outFile = fopen(filename.c_str(), "wb");
        gdImagePng(im, outFile);
        fclose(outFile);
        gdImageDestroy(im);

        if (!external_) {
            pFile_ << "<image x=\"" << x0 << "\" y=\"" << setY(y0) << "\" "
                   << " width=\"" << projectX(image.getWidth()) << "\" height=\"" << projectY(image.getHeight())
                   << "\" xlink:href=\"data:;base64,";

            // use uuencode to convet to base?
            string cmd = "( uuencode --base64 ";
            cmd.append(filename);
            cmd.append(" too > temp.b64 )");

            int status = system(cmd.c_str());
            if (status) {
                MagLog::error() << "\nSVGDriver: Command exit not zero - NO Base64 produced!\n"
                                << " COMMAND: " << cmd << "\n"
                                << endl;
                return false;
            }

            char str[256];
            int i = 0;
            fstream fout("temp.b64", ios::in);
            while (!fout.eof()) {
                fout.getline(str, 256);
                if (i > 0 && strncmp(str, "=", 1))
                    pFile_ << str << "\n";
                i++;
            }
            fout.close();
            pFile_ << "\" />\n";
            remove("temp.b64");
            remove(filename.c_str());
        }
        else  // if external files
        {
            FILE* fin = fopen(filename.c_str(), "rb");
            if (fin == 0) {
                MagLog::error() << "Open file " << filename << " to be added to SVG FAILED!" << endl;
                return 1;
            }
            else {
                pFile_ << "<image x=\"" << x0 << "\" y=\"" << setY(y0) << "\" "
                       << " width=\"" << projectX(image.getWidth()) << "\" height=\"" << projectY(image.getHeight())
                       << "\" xlink:href=\"" << filename << "\" />\n";
            }
            if (fin)
                fclose(fin);
        }
        pFile_ << "</g>\n";
    }
#endif
    debugOutput("Cell array - END");
    return true;
}

/*!
  \brief Image render method for ALL drivers.

  This method should be used by all Magics++ drivers to render image objects.
*/
MAGICS_NO_EXPORT void SVGDriver::renderImage(const ImportObject& obj) const {
    { BaseDriver::renderImage(obj); }
}

/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void SVGDriver::debugOutput(const string& s) const {
    if (debug_)
        pFile_ << "<!-- " << s << " -->\n";
}

/*!
  \brief class information are given to the output-stream
*/
void SVGDriver::print(ostream& out) const {
    out << "SVGDriver[";
    out << "]";
}

//! Method to plot symbols
/*!
 Needs special treatment of MagLogo. Much better quality when imported as GIF!
*/
MAGICS_NO_EXPORT void SVGDriver::renderSymbols(const Symbol& symbol) const {
    debugOutput("Symbols - START");
    closeGroup();

    if (symbol.getSymbol() == "logo_ecmwf") {
        if (inkscape_)
            pFile_ << "<g inkscape:groupmode=\"layer\" inkscape:label=\"ECMWF_logo\">\n"
                   << " <title>ECMWF_logo</title>\n";
        const MFloat x = projectX(symbol[0].x());
        const MFloat y = projectY(symbol[0].y());

        if (!magCompare(logoLocation_, "INLINE")) {
            string logofile;
            if (magCompare(logoLocation_, "LOCAL"))
                logofile = "ecmwf_logo.png";
            else
                logofile = buildConfigPath("ecmwf_logo.png");
            svg_output_resource_list_.push_back(logofile);
            pFile_ << "<a xlink:href=\"http://www.ecmwf.int\">"
                   << "<image x=\"" << x - (y * 1.35) << "\" y=\"" << setY(y + (y * .5)) << "\" width=\"" << y * 5.4
                   << "\" height=\"" << y << "\" xlink:href=\"" << logofile << "\" />"
                   << "</a>\n";
        }
        else {
            pFile_ << "<g transform=\"translate(" << x - (y * 1.35) << "," << setY(y + (y * .5)) << ")\">\n";
            const string s = buildConfigPath("ecmwf_logo_2014.svg");
            ifstream psfile(s.c_str());

            if (!psfile) {
                MagLog::error() << "PostScriptDriver::copyMacro() --> Cannot open PostScript Macro file! " << s
                                << " Is MAGPLUS_HOME set correctly?\n";
                return;
            }
            char ch;
            while (psfile.get(ch)) {
                pFile_.put(ch);
            }
            psfile.close();
            pFile_ << "</g>\n";
        }
        if (inkscape_)
            pFile_ << "</g><!-- Logo end -->\n";
    }
    else
        BaseDriver::renderSymbols(symbol);

    debugOutput("Symbols - END");
}

static SimpleObjectMaker<SVGDriver, BaseDriver> SVG_driver("SVG");
