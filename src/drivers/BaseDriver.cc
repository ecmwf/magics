/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BaseDriver.cc
    \brief Implementation of driver base class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

*/

#include <BaseDriver.h>

#include <Arrow.h>
#include <BinaryObject.h>
#include <Colour.h>
#include <Flag.h>
#include <Image.h>
#include <ImportObject.h>
#include <Layer.h>
#include <Layout.h>
#include <PaperPoint.h>
#include <Polyline.h>
#include <Symbol.h>
#include <Text.h>

#include <BaseDriverImages.h>
#include <BaseDriverSymbols.h>
#include <BaseDriverWind.h>

#include <System.h>
#include <Timer.h>

#include "magics_windef.h"

using namespace magics;

int BaseDriver::numFiles_ = 0;


//! Constructor for all drivers
/*!
  Main job is to initialise most variables and read font information
*/
BaseDriver::BaseDriver() :
    currentPage_(-1),
    fileName_(""),
    currentLayer_(""),
    currentLineType_(M_SOLID),
    currentLineWidth_(-1),
    currentLineStyle_(1),
    currentColour_(Colour("white")),
    coordRatioX_(1),
    coordRatioY_(1),
    newPage_(true),  // newLayout_(true), // external_(false),
    disabled_(false),
    alphaEnabled_(false),
    applyGaussianBlur_(-1.),
    indexHatch_(0),
    currentShading_(M_SH_NONE),
    cmScale_(1.),
    xDeviceLength_(MagTranslator<double, double>().magics("SUPER_PAGE_X_LENGTH")),
    yDeviceLength_(MagTranslator<double, double>().magics("SUPER_PAGE_Y_LENGTH")),
    obs_distance_(-1.) {}

BaseDriver::~BaseDriver() {
    FontMap_.clear();
}


/*! \brief Method set solid fill shading properties

 \sa renderSimplePolygon
*/
void BaseDriver::shade(const FillShadingProperties& properties) const {
    currentShading_           = M_SH_SOLID;
    currentShadingProperties_ = &properties;
}

/*! \brief Method set hatch fill shading properties

 \sa renderSimplePolygon
*/
void BaseDriver::shade(const HatchShadingProperties& properties) const {
    currentShading_           = M_SH_HATCH;
    currentShadingProperties_ = &properties;
}

/*! \brief Method set dot fill shading properties

 \sa renderSimplePolygon
*/
void BaseDriver::shade(const DotShadingProperties& properties) const {
    currentShading_           = M_SH_DOT;
    currentShadingProperties_ = &properties;
}

/*!
 \brief Method to print list of all generated files in the driver
*/
void BaseDriver::printOutputName(const std::string& str) const {
    if (filelist_) {
        if (filelist_reset_)
            remove(filelist_name_.c_str());
        const SystemInfo info;
        fstream fs;
        if (numFiles_ == 0) {
            fs.open(filelist_name_.c_str(), fstream::out);
            fs << "# " << getMagicsVersionString() << " " << info.getHostName() << " " << info.getTime() << "\n";
        }
        else
            fs.open(filelist_name_.c_str(), fstream::out | fstream::app);

        fs << info.getTime() << " " << str << "\n";
        fs.close();
        numFiles_++;
    }
}


//! Method to read font information
/*!
 A hash table is produce to map font names to file names of TTF files.

  \todo make this a singleton!?
*/
void BaseDriver::readFonts() const {
    const string s = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "Fonts.dat";
    ifstream psfile(s.c_str());

    if (psfile) {
        int id;
        char temp[128];
        char magics_name[64];
        char ps_name[128];
        char ps_filename[128];
        char ttf_filename[128];
        char css_name[128];

        // read header (4 lines) and ignore
        psfile.getline(temp, 128);
        psfile.getline(temp, 128);
        psfile.getline(temp, 128);
        psfile.getline(temp, 128);

        while (!psfile.eof()) {
            magFont font;
            psfile >> id >> magics_name >> ps_name >> ps_filename >> ttf_filename >> css_name;
            font.id                       = id;
            font.magics_name              = magics_name;
            font.ps_name                  = ps_name;
            font.ps_filename              = ps_filename;
            font.ttf_filename             = ttf_filename;
            font.css_name                 = css_name;
            FontMap_[string(magics_name)] = font;
        }
        psfile.close();
    }
    else
        MagLog::error() << "BaseDriver::readFonts() --> Cannot open Font file " << s << " ! No text can be plotted.\n";
}

/*! \brief formulating a filename

  This method is used to add the page number for multipage output,
  such in PostScriptDriver (if EPS or splitted), GDDriver (if not animated)
  and SVGDriver.

  It preserves the file name extension, and it needs it !!!
*/
string BaseDriver::getFileName(const string& extension, const unsigned int no) const {
    // offsetting the current page number if the users has set so
    const unsigned int no2 = (firstvalue_ >= 0) ? (no + firstvalue_ - 1) : no;

    string ext  = "." + extension;
    bool full   = false;
    bool legacy = false;

    string filename = name_;
    if (filename.empty()) {
        filename = fullname_;
        if (!filename.empty())
            full = true;
    }
    if (filename.empty()) {
        filename = legacyname_;
        if (!filename.empty())
            legacy = true;
    }
    if (filename.empty()) {
        filename = extension;
    }

    // if nothing is set
    if (filename.empty())
        filename = "magics";

    // name stays the same as given
    if (no == 0) {
        if (full || (legacy && extension == "ps"))
            return filename;
        if (extension == "ps" || extension == "pdf" || extension == "kmz") {
            filename += ext;
            return filename;
        }
    }

    if (full) {
        ext                   = "";
        string::size_type pos = filename.find_last_of(".");
        if (pos != string::npos) {
            const string f = filename.substr(0, pos);
            const string t = filename.substr(pos);
            ext            = t;
            filename       = f;
        }
    }

    int numberWidth = numberingwidth_;
    if (numberWidth > 4) {
        numberWidth = 4;
        MagLog::warning() << "Output --> NumberingWidth only allows values between 1 and 4. 4 is used now.\n";
    }

    if ((no2 > 1) || firstnumber_ || legacy) {
        char ostr[8];
        if (numberWidth == 4) {
            sprintf(ostr, "%04u", no2);
        }
        else if (numberWidth == 3) {
            sprintf(ostr, "%03u", no2);
        }
        else if (numberWidth == 2) {
            sprintf(ostr, "%02u", no2);
        }
        else {
            sprintf(ostr, "%u", no2);
        }
        filename += separator_ + ostr + ext;
    }
    else  // if first page no number
    {
        filename += ext;
    }
    return filename;
}


/*!
  \brief processing layouts
  This methods processes the Layout objects. It needs to be checked if a Layout is a new page or not.
  \sa Layout
*/
MAGICS_NO_EXPORT void BaseDriver::redisplay(const Layout& layout) const {
    project(layout);
    staLayouts_.push(&layout);
    layout.visit(*this);  // visit this layout!
    unproject();
}


MAGICS_NO_EXPORT void BaseDriver::redisplay(const RootLayout& root) const {
    root.visit(*this);  // visit this ROOT layout!
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const LegendLayout& legend) const {
    redisplay((const Layout&)legend);
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const SceneLayout& scene) const {
    redisplay((const Layout&)scene);
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const magics::StartPage&) const {
    startPage();
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const magics::EndPage&) const {
    endPage();
    vecPoints_.clear();
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const ClearObject&) const {
    vecPoints_.clear();
}


/*!
  \brief Decision how to stroke/fill simple polygon

  Overwritten in SVGDriver::redisplay(const Polyline& line) const
*/
void BaseDriver::redisplay(const magics::Polyline& line) const {
    if (line.isFilled())
        renderSimplePolygon(line);
    if (line.isStroked())
        printLine(line);
}

void BaseDriver::redisplay(const Arrow& arrow) const {
    renderWindArrow(arrow);
}

void BaseDriver::redisplay(const Flag& flag) const {
    renderWindFlag(flag);
}

void BaseDriver::redisplay(const ImportObject& object) const {
    renderImage(object);
}

void BaseDriver::redisplay(const Image& object) const {
    renderCellArray(object);
}

void BaseDriver::redisplay(const Text& text) const {
    renderText(text);
}

void BaseDriver::redisplay(const Symbol& symbol) const {
    renderSymbols(symbol);
}

void BaseDriver::redisplay(const TextSymbol& symbol) const {
    renderTextSymbols(symbol);
}

void BaseDriver::redisplay(const ComplexSymbol& symbol) const {
    renderComplexSymbols(symbol);
}

void BaseDriver::redisplay(const TextItem& text, const ComplexSymbol& symbol) const {
    renderTextItem(text, symbol);
}

void BaseDriver::redisplay(const SymbolItem& text, const ComplexSymbol& symbol) const {
    renderSymbolItem(text, symbol);
}

void BaseDriver::redisplay(const FlagItem& text, const ComplexSymbol& symbol) const {
    renderFlagItem(text, symbol);
}


/*
 \brief Least Square fit of a line.

 Returns the angle (in relation to horizon == 0 ).

 \sa printLine
*/
double BaseDriver::LSF(MFloat* x, MFloat* y, int i0) const {
    double angle         = 0.;
    double x_sum         = 0.;
    double y_sum         = 0.;
    const unsigned int n = 3;

    for (unsigned int r = 0; r < n; r++) {
        x_sum += projectX(x[i0 + r]);
        y_sum += projectY(y[i0 + r]);
    }

    const double x_over_n = x_sum / n;
    const double y_over_n = y_sum / n;
    double sxx            = 0;
    double sxy            = 0;

    for (unsigned int r = 0; r < n; r++) {
        const double xi = projectX(x[i0 + r]) - x_over_n;
        const double yi = projectY(y[i0 + r]) - y_over_n;
        sxx += (xi * xi);
        sxy += (xi * yi);
    }

    if ((abs(sxx) > 0.00001)) {
        angle = atan2((sxy / sxx), 1.);
    }
    else {
        angle = 10.;
        MagLog::debug() << "BaseDriver: Devision through zero prevented in calculation of Label angle!" << endl;
    }
    return angle;
}

/*!
  \brief Method plotting Polylines with Labels.

  \sa renderPolyline renderText

  \todo location memory for labels.
*/
void BaseDriver::printLine(const magics::Polyline& line) const {
    const unsigned long n = line.size();
    if (n < 2)
        return;

    bool arrowHead = (line.arrowProperties()) ? true : false;

    MFloat* x = new MFloat[n];
    MFloat* y = new MFloat[n];

    for (unsigned long s = 0; s < n; s++) {
        x[s] = line.get(s).x();
        y[s] = line.get(s).y();
    }

    // render line - driver specific part
    if (line.getThickness() > 0 && !(line.getColour() == Colour("NONE"))) {
        setNewColour(line.getColour());
        currentLineStyle_ = setLineParameters(line.getLineStyle(), line.getThickness());

        renderPolyline(n, x, y);

        magics::Polyline::Holes::const_iterator h  = line.beginHoles();
        magics::Polyline::Holes::const_iterator he = line.endHoles();

        for (; h != he; ++h) {
            vector<double> xx;
            vector<double> yy;
            line.hole(h, xx, yy);
            const unsigned long nn = xx.size();
            MFloat* nx             = new MFloat[nn];
            MFloat* ny             = new MFloat[nn];
            for (unsigned int is = 0; is < nn; is++) {
                nx[is] = xx[is];
                ny[is] = yy[is];
            }
            renderPolyline(nn, nx, ny);
            delete[] nx;
            delete[] ny;
        }
    }

    const unsigned int minimum_points_for_labelling = 25;  // must be at least -15 : see function LSF above

    if ((arrowHead || (line.getLabel().isVisible() && line.getLabel().getText() != "")) &&
        (n > minimum_points_for_labelling)) {
        ASSERT(staLayouts_.empty() == false);

        MFloat* labelx = new MFloat[n];  // in theory, we shouldn't need this many entries...
        MFloat* labely = new MFloat[n];  // in theory, we shouldn't need this many entries...

        // Store the minimum x,y corner, pretending that we're going to plot a label there.
        // We won't actually plot a label there though - it's just to make sure that
        // we don't plot one too close to that point.
        int num_labels = 1;
        labelx[0]      = staLayouts_.top()->minX();
        labely[0]      = staLayouts_.top()->minY();

        // Calculate how far apart the labels should be.
        // Our algorithm is to take the average of the width and height and divide by 2.
        // This should give us approximately 3 labels over the width of the page.
        const MFloat coords_range_x                     = fabs(staLayouts_.top()->maxX() - staLayouts_.top()->minX());
        const MFloat coords_range_y                     = fabs(staLayouts_.top()->maxY() - staLayouts_.top()->minY());
        const MFloat min_distance_between_labels        = (coords_range_x + coords_range_y) / 4.;
        const MFloat min_square_distance_between_labels = (min_distance_between_labels * min_distance_between_labels);

        unsigned int i = 10;
        while (i < n - minimum_points_for_labelling) {
            double angle        = LSF(x, y, i);
            const double angle2 = LSF(x, y, i + 1);

            if ((angle < 4.) && (angle2 < 4.) && fabs(angle - angle2) < 0.01) {
                const MFloat THIS_X = x[i];
                const MFloat THIS_Y = y[i];
                const MFloat PREV_X = labelx[num_labels - 1];
                const MFloat PREV_Y = labely[num_labels - 1];

                const double distance_squared =
                    ((THIS_X - PREV_X) * (THIS_X - PREV_X)) + ((THIS_Y - PREV_Y) * (THIS_Y - PREV_Y));

                if ((arrowHead) || (distance_squared > min_square_distance_between_labels)) {
                    if (arrowHead) {
                        MFloat pro_x = x[i];
                        MFloat pro_y = y[i];

                        Arrow arrow;
                        arrow.copy(*line.arrowProperties());
                        arrow.setColour(line.getColour());
                        arrow.setArrowPosition(M_HEAD_ONLY);
                        if ((x[i] - x[i + 3]) < 0.)
                            angle += PI;
                        const double dx = sin(angle + 1.5707963267949);
                        const double dy = -setAngleY(cos(angle + 1.5707963267949));
                        PaperPoint pp(pro_x, pro_y);
                        ArrowPoint apoint(dx, dy, pp);
                        arrow.push_back(apoint);
                        renderWindArrow(arrow);
                    }
                    else {
                        MFloat pro_x = x[i + 2];
                        MFloat pro_y = y[i + 2];
                        Text text;
                        PaperPoint pp(pro_x, pro_y);
                        text.push_back(pp);
                        Label label  = line.getLabel();
                        MagFont font = label.font();
                        text.setFont(font);
                        text.addText(label.getText(), font.colour(), font.size());
                        text.setBlanking(label.getBlanking());
                        text.setJustification(label.getJustification());
                        text.setVerticalAlign(MHALF);
                        text.setAngle(-setAngleY(angle));
                        text.setFont(font);
                        renderText(text);
                    }
                    labelx[num_labels] = x[i];
                    labely[num_labels] = y[i];
                    num_labels++;
                    i += 5;
                }
            }  // angles are not the same
            i += 5;
        }
        delete[] labelx;
        delete[] labely;
    }  // endif enough points for a label
    delete[] x;
    delete[] y;
    currentColour_ = Colour("none");
}

void BaseDriver::renderPolyline(vector<PaperPoint>& vP) const {
    const unsigned int size = vP.size();
    MFloat* x               = new MFloat[size];
    MFloat* y               = new MFloat[size];
    for (unsigned int i = 0; i < size; i++) {
        x[i] = vP[i].x();
        y[i] = vP[i].y();
    }
    renderPolyline(size, x, y);

    delete[] x;
    delete[] y;
}

void BaseDriver::renderPolyline2(vector<PaperPoint>& vP) const {
    const unsigned int size = vP.size();
    MFloat* x               = new MFloat[size];
    MFloat* y               = new MFloat[size];
    for (int unsigned i = 0; i < size; i++) {
        x[i] = projectX(vP[i].x());
        y[i] = projectY(vP[i].y());
    }
    renderPolyline2(size, x, y);

    delete[] x;
    delete[] y;
}

void BaseDriver::renderSimplePolygon(vector<PaperPoint>& vP) const {
    const unsigned int size = vP.size();
    MFloat* x               = new MFloat[size];
    MFloat* y               = new MFloat[size];
    for (unsigned int i = 0; i < size; i++) {
        x[i] = vP[i].x();
        y[i] = vP[i].y();
    }
    renderSimplePolygon(size, x, y);

    delete[] x;
    delete[] y;
}

/*!
 Class information are given to the output-stream.
*/
void BaseDriver::print(ostream& out) const {
    out << "BaseDriver";
}

string BaseDriver::getTmpName() const {
#ifndef MAGICS_ON_WINDOWS
    string stmp;
    stmp += "magics_temp_ps_XXXXXX";
    char* mtmp = new char[stmp.length() + 1];
    stmp.copy(mtmp, string::npos);
    mtmp[stmp.length()] = '\0';
    const int m         = mkstemp(mtmp);
    stmp                = (m) ? mtmp : " ";
    delete[] mtmp;

    return stmp;
#else
    return "";
#endif
}

bool BaseDriver::renderCellArray(const Image&) const {
    return true;
}

bool BaseDriver::renderPixmap(MFloat, MFloat, MFloat, MFloat, int, int, unsigned char*, int, bool) const {
    return true;
}


/*!
  Overwritten in KMLDriver

  \sa Layer
*/
MAGICS_NO_EXPORT void BaseDriver::redisplay(const Layer& layer) const {
    MagLog::dev() << "BaseDriver::redisplay( layer) > " << layer.name() << endl;
}

void BaseDriver::redisplay(const SceneLayer& layer) const {
    for (vector<Layer*>::iterator l = layer.beginLayer(); l != layer.endLayer(); ++l) {
        (*l)->redisplay(*this);
    }
}

void BaseDriver::redisplay(const StaticLayer& layer) const {
    currentLayer_ = layer.name();
    layer.visit(*this);
}

void BaseDriver::redisplay(const NoDataLayer& layer) const {
    currentLayer_ = layer.name();
    layer.visit(*this);
}

void BaseDriver::redisplay(const StepLayer& layer) const {
    MagLog::dev() << "BaseDriver::redisplay( StepLayer&)" << layer.name() << endl;
}
/*!
 \brief Method to read and execute binary file

 This method is implemented for performance in Metview 4 and WREP

*/
void BaseDriver::setDimensionsFromBinary(string mbg_tmpl, MFloat& ratio, int& width) const {
    ifstream in(mbg_tmpl.c_str());
    if (in.is_open()) {
        char mag[6];
        in.read((char*)(&mag), 6);
        if (strncmp("MAGICS", mag, 6) != 0) {
            MagLog::error() << "Magics number of TEMPLATE binary file " << mbg_tmpl << " is wrong!\n"
                            << "   Please check endiness and/or if you gave the right filename." << endl;
            return;
        }

        int checksum;
        in.read((char*)(&checksum), sizeof(int));
        if (checksum != 10) {
            MagLog::error() << "Checksum of TEMPLATE binary file " << mbg_tmpl << " is wrong!\n"
                            << "   Please check endiness of your file." << endl;
            return;
        }

        int version;
        in.read((char*)(&version), sizeof(int));
        if (version != BINARY_VERSION)
            MagLog::error() << " MGB VERSION MISMATCH " << version << " != " << BINARY_VERSION << endl;
        int lengthHeader;
        in.read((char*)(&lengthHeader), sizeof(int));

        MFloat lx;
        MFloat ly;
        in.read((char*)(&lx), sizeof(MFloat));
        in.read((char*)(&ly), sizeof(MFloat));
        in.close();

        ratio = ly / lx;
        width = maground(lx);
    }
    else {
        MagLog::error() << "TEMPLATE binary file " << mbg_tmpl << " could NOT be opened!" << endl;
    }
}


/*!
 \brief Method to read and execute binary file

 This method is implemented for performance in Metview 4 and WREP

*/
void BaseDriver::redisplay(const BinaryObject& binary) const {
    Timer timer("binary", " read");
    const string mbg_tmpl = binary.getPath();
    ifstream in(mbg_tmpl.c_str());
    if (in.is_open()) {
        const float alpha  = binary.getTransparency();
        applyGaussianBlur_ = binary.getGaussianBlur();

        char mag[6];
        in.read((char*)(&mag), 6);
        if (strncmp("MAGICS", mag, 6) != 0) {
            MagLog::error() << "Magics number of binary file " << mbg_tmpl.c_str() << " is wrong!\n"
                            << "   Please check endiness and if you gave the right filename." << endl;
            return;
        }

        int checksum;
        in.read((char*)(&checksum), sizeof(int));
        if (checksum != 10) {
            MagLog::error() << "Checksum of binary file " << mbg_tmpl.c_str() << " is wrong!\n"
                            << "   Please check endiness of your file." << endl;
            return;
        }

        string old_currentLayer_ = currentLayer_;
        currentLayer_            = mbg_tmpl;

        Layer lay;
        lay.name(mbg_tmpl);
        newLayer(lay);

        int version;
        in.read((char*)(&version), sizeof(int));

        int lengthHeader;
        in.read((char*)(&lengthHeader), sizeof(int));


        MFloat lx;
        MFloat ly;
        in.read((char*)(&lx), sizeof(MFloat));
        in.read((char*)(&ly), sizeof(MFloat));

        Layout la;
        la.x(binary.getMgb_x());
        la.y(binary.getMgb_y());
        la.width(binary.getMgb_width());
        la.height(binary.getMgb_height());
        la.minX(0);
        la.minY(0);
        la.maxX(100);
        la.maxY(100);
        project(la);

        char c;
        while (in.get(c)) {
            int n;
            switch (c) {
                case 'T':
                    MFloat x;
                    MFloat y;
                    MFloat s;
                    int len, noNT;
                    int size;
                    {
                        in.read((char*)(&size), sizeof(int));
                        MFloat r, g, b;
                        in.read((char*)(&r), sizeof(MFloat));
                        in.read((char*)(&g), sizeof(MFloat));
                        in.read((char*)(&b), sizeof(MFloat));

                        MFloat angle;
                        in.read((char*)(&angle), sizeof(MFloat));
                        bool bl;
                        in.read((char*)(&bl), sizeof(bool));
                        enum Justification horizontal;
                        enum VerticalAlign vertical;
                        in.read((char*)(&horizontal), sizeof(enum Justification));
                        in.read((char*)(&vertical), sizeof(enum VerticalAlign));

                        in.read((char*)(&noNT), sizeof(int));

                        Text text;
                        text.setBlanking(bl);
                        text.setJustification(horizontal);
                        text.setVerticalAlign(vertical);
                        text.setAngle(angle);

                        for (int ntc = 0; ntc < noNT; ntc++) {
                            in.read((char*)(&r), sizeof(MFloat));
                            in.read((char*)(&g), sizeof(MFloat));
                            in.read((char*)(&b), sizeof(MFloat));
                            in.read((char*)(&s), sizeof(MFloat));
                            in.read((char*)(&len), sizeof(int));
                            char* tex = new char[len + 1];
                            in.read(tex, sizeof(char) * len);
                            tex[len] = '\0';
                            string str(tex);
                            text.addText(str, Colour(r, g, b, alpha), s);
                            delete[] tex;
                        }

                        for (int n = 0; n < size; n++) {
                            in.read((char*)(&x), sizeof(MFloat));
                            in.read((char*)(&y), sizeof(MFloat));
                            PaperPoint pp(x, y);
                            text.push_back(pp);
                        }
                        renderText(text);
                    }
                    break;

                case 'A':
                    in.read((char*)(&n), sizeof(int));
                    {
                        Arrow arrow;
                        double sc;
                        in.read((char*)(&sc), sizeof(double));
                        arrow.setScale(sc);
                        int index;
                        in.read((char*)(&index), sizeof(int));
                        arrow.setHeadIndex(index);
                        LineStyle ls;
                        in.read((char*)(&ls), sizeof(LineStyle));
                        arrow.setStyle(ls);
                        ArrowPosition ap;
                        in.read((char*)(&ap), sizeof(ArrowPosition));
                        arrow.setArrowPosition(ap);
                        int hi;
                        in.read((char*)(&hi), sizeof(int));
                        arrow.setHeadIndex(hi);
                        double hr;
                        in.read((char*)(&hr), sizeof(double));
                        arrow.setHeadRatio(hr);
                        MFloat r, g, b;
                        in.read((char*)(&r), sizeof(MFloat));
                        in.read((char*)(&g), sizeof(MFloat));
                        in.read((char*)(&b), sizeof(MFloat));
                        setNewColour(Colour(r, g, b, alpha));
                        arrow.setColour(Colour(r, g, b, alpha));

                        for (int pts = 0; pts < n; pts++) {
                            double x, y, ax, ay;
                            in.read((char*)(&x), sizeof(double));
                            in.read((char*)(&y), sizeof(double));
                            in.read((char*)(&ax), sizeof(double));
                            in.read((char*)(&ay), sizeof(double));
                            PaperPoint p(ax, ay);
                            ArrowPoint ap(x, y, p);
                            arrow.push_back(ap);
                        }
                        renderWindArrow(arrow);
                    }
                    break;
                case 'F':
                    in.read((char*)(&n), sizeof(int));
                    {
                        Flag flag;
                        double ll;
                        in.read((char*)(&ll), sizeof(double));
                        flag.setLength(ll);
                        LineStyle ls;
                        in.read((char*)(&ls), sizeof(LineStyle));
                        flag.setStyle(ls);
                        FlagConvention fc;
                        in.read((char*)(&fc), sizeof(FlagConvention));
                        flag.setConvention(fc);
                        Hemisphere he;
                        in.read((char*)(&he), sizeof(Hemisphere));
                        flag.setHemisphere(he);
                        double hr;
                        in.read((char*)(&hr), sizeof(double));
                        flag.setThickness(hr);
                        double hi;
                        in.read((char*)(&hi), sizeof(double));
                        flag.setOriginHeight(hi);

                        MFloat r, g, b;
                        in.read((char*)(&r), sizeof(MFloat));
                        in.read((char*)(&g), sizeof(MFloat));
                        in.read((char*)(&b), sizeof(MFloat));
                        setNewColour(Colour(r, g, b, alpha));
                        flag.setColour(Colour(r, g, b, alpha));

                        int len;
                        in.read((char*)(&len), sizeof(int));
                        char* tex = new char[len + 1];
                        in.read(tex, sizeof(char) * len);
                        tex[len] = '\0';
                        string str(tex);
                        flag.setOriginMarker(str);

                        for (int pts = 0; pts < n; pts++) {
                            double x, y, ax, ay;
                            in.read((char*)(&x), sizeof(double));
                            in.read((char*)(&y), sizeof(double));
                            in.read((char*)(&ax), sizeof(double));
                            in.read((char*)(&ay), sizeof(double));
                            PaperPoint p(ax, ay);
                            ArrowPoint ap(x, y, p);
                            flag.push_back(ap);
                        }
                        renderWindFlag(flag);
                    }
                    break;
                case 'H':
                    in.read((char*)(&n), sizeof(int));
                    {
                        MFloat* x = new MFloat[n];
                        MFloat* y = new MFloat[n];
                        in.read((char*)(x), sizeof(MFloat) * n);
                        in.read((char*)(y), sizeof(MFloat) * n);
                        renderPolyline(n, x, y);
                        delete[] x;
                        delete[] y;
                    }
                    break;

                case 'I':  // Images
                {
                    MFloat x0       = 0.;
                    MFloat x1       = 0.;
                    MFloat y0       = 0.;
                    MFloat y1       = 0.;
                    int height      = 0;
                    int width       = 0;
                    int noOfColours = 0;
                    double red      = 0.;
                    double green    = 0.;
                    double blue     = 0.;
                    double alpha    = 0.;

                    in.read((char*)(&width), sizeof(int));
                    in.read((char*)(&height), sizeof(int));
                    in.read((char*)(&x0), sizeof(MFloat));
                    in.read((char*)(&y0), sizeof(MFloat));
                    in.read((char*)(&x1), sizeof(MFloat));
                    in.read((char*)(&y1), sizeof(MFloat));
                    const int d = width * height;

                    in.read((char*)(&noOfColours), sizeof(int));
                    ColourTable table;

                    for (int v = 0; v < noOfColours; v++) {
                        in.read((char*)(&red), sizeof(double));
                        in.read((char*)(&green), sizeof(double));
                        in.read((char*)(&blue), sizeof(double));
                        in.read((char*)(&alpha), sizeof(double));
                        table.push_back(ColourTableEntry(Colour(red, green, blue, alpha)));
                    }

                    short* pixels = new short[d];
                    in.read((char*)(pixels), sizeof(short) * d);

                    Image object;
                    PaperPoint pp(x0, y0, 0.);
                    object.setOrigin(pp);
                    object.setWidth(x1);
                    object.setHeight(y1);
                    object.set(height, width);
                    for (int i = 0; i < d; i++)
                        object.push_back(pixels[i]);  // object(std::begin(pixels), std::end(pixels));
                    object.setColourTable(table);
                    renderCellArray(object);
                    delete[] pixels;
                } break;

                case 'B':
                    in.read((char*)(&n), sizeof(int));
                    {
                        MFloat* x = new MFloat[n];
                        MFloat* y = new MFloat[n];
                        in.read((char*)(x), sizeof(MFloat) * n);
                        in.read((char*)(y), sizeof(MFloat) * n);
                        renderPolyline2(n, x, y);
                        delete[] x;
                        delete[] y;
                    }
                    break;

                case 'R':
                    MFloat cx, cy, cr;
                    int cs;
                    in.read((char*)(&cx), sizeof(MFloat));
                    in.read((char*)(&cy), sizeof(MFloat));
                    in.read((char*)(&cr), sizeof(MFloat));
                    in.read((char*)(&cs), sizeof(int));
                    circle(cx, cy, cr, cs);
                    break;

                case 'X':
                    in.read((char*)(&n), sizeof(int));
                    {
                        MFloat* x = new MFloat[n];
                        MFloat* y = new MFloat[n];
                        in.read((char*)(x), sizeof(MFloat) * n);
                        in.read((char*)(y), sizeof(MFloat) * n);
                        magics::Polyline line;
                        for (int i = 0; i < n; i++) {
                            line.push_back(PaperPoint(x[i], y[i]));
                        }
                        unsigned int nh = 0;
                        in.read((char*)(&nh), sizeof(unsigned int));
                        if (nh != 0) {
                            for (unsigned int h = 0; h < nh; ++h) {
                                unsigned int nhx = 0;
                                in.read((char*)(&nhx), sizeof(unsigned int));
                                MFloat* xx = new MFloat[nhx];
                                MFloat* yy = new MFloat[nhx];
                                in.read((char*)(xx), sizeof(MFloat) * nhx);
                                in.read((char*)(yy), sizeof(MFloat) * nhx);
                                line.newHole();
                                for (unsigned int u = 0; u < nhx; ++u) {
                                    line.push_back_hole(PaperPoint(xx[u], yy[u]));
                                }
                            }
                        }
                        line.setFillColour(Colour("green"));
                        FillShadingProperties* shading = new FillShadingProperties();
                        line.setShading(shading);
                        MFloat r, g, b, a;
                        in.read((char*)(&r), sizeof(MFloat));
                        in.read((char*)(&g), sizeof(MFloat));
                        in.read((char*)(&b), sizeof(MFloat));
                        in.read((char*)(&a), sizeof(MFloat));
                        line.setFillColour(Colour(r, g, b, a * alpha));
                        renderSimplePolygon(line);
                        delete[] x;
                        delete[] y;
                    }
                    break;
                case 'S':
                    in.read((char*)(&n), sizeof(int));
                    {
                        MFloat* x = new MFloat[n];
                        MFloat* y = new MFloat[n];
                        in.read((char*)(x), sizeof(MFloat) * n);
                        in.read((char*)(y), sizeof(MFloat) * n);
                        renderSimplePolygon(n, x, y);
                        delete[] x;
                        delete[] y;
                    }
                    break;

                case 'W':
                    MFloat width;
                    in.read((char*)(&width), sizeof(MFloat));
                    setNewLineWidth(width);
                    break;

                case 'L':
                    MFloat w;
                    LineStyle linestyle;
                    in.read((char*)(&linestyle), sizeof(LineStyle));
                    in.read((char*)(&w), sizeof(MFloat));
                    setLineParameters(linestyle, w);
                    break;

                case 'C':
                    MFloat r, g, b, a;
                    in.read((char*)(&r), sizeof(MFloat));
                    in.read((char*)(&g), sizeof(MFloat));
                    in.read((char*)(&b), sizeof(MFloat));
                    in.read((char*)(&a), sizeof(MFloat));
                    setNewColour(Colour(r, g, b, a * alpha));
                    break;

                case 'N':
                    startPage();
                    break;

                case 'E':
                    endPage();
                    break;

                case 'P': {
                    Layout l;
                    double x, y, w, h, minx, miny, maxx, maxy;
                    in.read((char*)(&x), sizeof(double));
                    in.read((char*)(&y), sizeof(double));
                    in.read((char*)(&w), sizeof(double));
                    in.read((char*)(&h), sizeof(double));
                    in.read((char*)(&minx), sizeof(double));
                    in.read((char*)(&miny), sizeof(double));
                    in.read((char*)(&maxx), sizeof(double));
                    in.read((char*)(&maxy), sizeof(double));
                    l.x(x);
                    l.y(y);
                    l.width(w);
                    l.height(h);
                    l.minX(minx);
                    l.minY(miny);
                    l.maxX(maxx);
                    l.maxY(maxy);
                    project(l);
                } break;
                case 'U':
                    unproject();
                    break;
            }
        }
        unproject();
        in.close();
        closeLayer(lay);
        currentLayer_ = old_currentLayer_;
    }
    else {
        MagLog::error() << "TEMPLATE binary file " << mbg_tmpl << " could NOT be opened!" << endl;
    }
}
