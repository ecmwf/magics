/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BinaryDriver.cc
    \brief Implementation of BinaryDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Jan  4 20:28:15 2010

*/

#include <BinaryDriver.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>


using namespace magics;


/*!
  \brief Constructor
*/
BinaryDriver::BinaryDriver()
{
	MagLog::debug() << "BinaryDriver::BinaryDriver needs implementing." << std::endl;
}

/*!
  \brief Destructor
*/
BinaryDriver::~BinaryDriver() 
{
}

/*!
  \brief Opening the driver
*/
void BinaryDriver::open()
{
        dimensionX_ = maground(width_);
	const MFloat ratio = getYDeviceLength() / getXDeviceLength();
        dimensionY_ = maground(ratio*dimensionX_);

        setCMscale(dimensionX_/getXDeviceLength());

	out_.open(getFileName("mgb").c_str(),ios::out|ios::binary);
	if( !out_ ) {
		MagLog::error() << "BinaryDriver: Error opening output stream." << endl;
	}
	else
	{
		const int version  = BINARY_VERSION;
		const int checksum = 10;
		const int headersize = 2*sizeof(MFloat);
		const char mag[7] = "MAGICS";
		out_.write((char *)(&mag), 6);
		out_.write((char *)(&checksum),    sizeof(int));
		out_.write((char *)(&version),     sizeof(int));
		out_.write((char *)(&headersize),  sizeof(int));
		out_.write((char *)(&dimensionX_), sizeof(MFloat));
		out_.write((char *)(&dimensionY_), sizeof(MFloat));
		out_.flush();
	}
	dimensionYglobal_ = dimensionY_;
}

/*!
  \brief Closing the driver
*/
void BinaryDriver::close()
{
	endPage();
	currentPage_ = 0;
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.
*/
MAGICS_NO_EXPORT void BinaryDriver::startPage() const
{
	if(currentPage_ > 0) endPage();
	
	currentPage_++;
}

/*!
  \brief ending a page
 
  This method has to take care that for formats with multiple output 
  files are closed.
*/
MAGICS_NO_EXPORT void BinaryDriver::endPage() const
{
	out_.close();
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void BinaryDriver::project(const magics::Layout& layout) const
{
	char c = 'P';
	out_.write(&c, 1);
//	const MFloat oldHeight = dimensionY_;

	const double x = layout.x();
	const double y = layout.y();
	const double w = layout.width();
	const double h = layout.height();
	const double minX = layout.minX();
	const double minY = layout.minY();
	const double maxX = layout.maxX();
	const double maxY = layout.maxY();

	out_.write((char *)(&x), sizeof(double));
	out_.write((char *)(&y), sizeof(double));
	out_.write((char *)(&w), sizeof(double));
	out_.write((char *)(&h), sizeof(double));
	out_.write((char *)(&minX), sizeof(double));
	out_.write((char *)(&minY), sizeof(double));
	out_.write((char *)(&maxX), sizeof(double));
	out_.write((char *)(&maxY), sizeof(double));

	// for  J A V A S C R I P T
	// push current state
	dimensionStack_.push(dimensionX_);
	dimensionStack_.push(dimensionY_);
	const MFloat oldHeight = dimensionY_;
	offsetsX_.push(offsetX_);
	offsetsY_.push(offsetY_);
	scalesX_.push(coordRatioX_);
	scalesY_.push(coordRatioY_);

	offsetX_    += layout.x()     * 0.01 * dimensionX_;
	offsetY_    -= layout.y()     * 0.01 * dimensionY_;
	dimensionX_ =  layout.width() * 0.01 * dimensionX_;
	dimensionY_ =  layout.height()* 0.01 * dimensionY_;

	const MFloat sumX = layout.maxX() - layout.minX();
	const MFloat sumY = layout.maxY() - layout.minY();

	if( sumX!=0 && sumY!=0 )
	{
        	coordRatioX_ = dimensionX_/sumX;
        	coordRatioY_ = -dimensionY_/sumY;
	}

	offsetX_ = projectX( -layout.minX());
	offsetY_ = projectY( -layout.minY());

	if(layout.isNavigable())
	{
		const double offsetX = offsetX_ + projectX( layout.minX());
		const double offsetY = offsetY_ + projectY( layout.maxY());
		layout.pushDriverInfo(offsetX, oldHeight+offsetY, dimensionX_, dimensionY_);
	}
}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

*/
MAGICS_NO_EXPORT void BinaryDriver::unproject() const
{
	char c = 'U';
	out_.write(&c, 1);

	dimensionY_ = dimensionStack_.top();dimensionStack_.pop();
	dimensionX_ = dimensionStack_.top();dimensionStack_.pop();
	offsetX_ = offsetsX_.top();offsetsX_.pop();
	offsetY_ = offsetsY_.top();offsetsY_.pop();
	coordRatioX_  = scalesX_.top(); scalesX_.pop();
	coordRatioY_  = scalesY_.top(); scalesY_.pop();
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the 
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void BinaryDriver::setNewColour(const Colour &colour) const
{
	const MFloat r=colour.red();
	const MFloat g=colour.green();
	const MFloat b=colour.blue();
	const MFloat a=colour.alpha();
        char c = 'C';
        out_.write(&c, 1);
	out_.write((char *)(&r), sizeof(MFloat));
	out_.write((char *)(&g), sizeof(MFloat));
	out_.write((char *)(&b), sizeof(MFloat));
	out_.write((char *)(&a), sizeof(MFloat));
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the 
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void BinaryDriver::setNewLineWidth(const MFloat width) const
{
        char c = 'W';
        out_.write(&c, 1);
	out_.write((char *)(&width), sizeof(MFloat));
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the 
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT int BinaryDriver::setLineParameters(const LineStyle linestyle, const MFloat w) const
{
        char c = 'L';
        out_.write(&c, 1);
        out_.write((char *)(&linestyle), sizeof(LineStyle));
	out_.write((char *)(&w), sizeof(MFloat));
	return 0;
}

#include <Arrow.h>
#include <Flag.h>

void BinaryDriver::renderWindArrow(const Arrow &arrow) const
{
  /*
    arrow.getScale()
    arrow.getThickness()
    const LineStyle style = arrow.getStyle();
    const ArrowPosition pos = arrow.getArrowPosition();
  */
	char c = 'A';
	out_.write(&c, 1);
        const int no = arrow.size();
	out_.write((char *)(&no), sizeof(int));
        const double sc  = arrow.getScale();
	out_.write((char *)(&sc), sizeof(double));
        const int index  = arrow.getHeadIndex();
	out_.write((char *)(&index), sizeof(int));
	const LineStyle ls = arrow.getStyle();
	out_.write((char *)(&ls), sizeof(LineStyle));
	ArrowPosition ap = arrow.getArrowPosition();
	out_.write((char *)(&ap), sizeof(ArrowPosition));
	const int hi = arrow.getHeadIndex();
	out_.write((char *)(&hi), sizeof(int));
	const double hr = arrow.getHeadRatio();
	out_.write((char *)(&hr), sizeof(double));

	Colour colour = arrow.getColour();
	const MFloat r=colour.red();
	const MFloat g=colour.green();
	const MFloat b=colour.blue();
	out_.write((char *)(&r), sizeof(MFloat));
	out_.write((char *)(&g), sizeof(MFloat));
	out_.write((char *)(&b), sizeof(MFloat));

        Arrow::const_iterator arr = arrow.begin();

        for(int pts=0;pts<no;pts++)
        {
          const double x = arr->x_;
          out_.write((char *)(&x), sizeof(double));
          const double y = arr->y_;
          out_.write((char *)(&y), sizeof(double));
          const PaperPoint p = arr->point_;
          const double ax = p.x();
          out_.write((char *)(&ax), sizeof(double));
          const double ay = p.y();
          out_.write((char *)(&ay), sizeof(double));
//          out_.write((char *)(&p), sizeof(PaperPoint));
          ++arr;
        }
}

void BinaryDriver::renderWindFlag(const Flag &flag) const
{
/*
	flag.getThickness()
	LineStyle style = flag.getStyle()
	flag.getLength()
	flag.getColour()
	flag.size()
	string marker = flag.getOriginMarker()
	flag.getOriginHeight()
	flag.getConvention()==KNOTS
	flag.getHemisphere()==NORTH
*/
	char c = 'F';
	out_.write(&c, 1);
        const int no = flag.size();
	out_.write((char *)(&no), sizeof(int));
        const double sc  = flag.getLength();
	out_.write((char *)(&sc), sizeof(double));
	const LineStyle ls = flag.getStyle();
	out_.write((char *)(&ls), sizeof(LineStyle));
	FlagConvention fc = flag.getConvention();
	out_.write((char *)(&fc), sizeof(FlagConvention));
	Hemisphere he = flag.getHemisphere();
	out_.write((char *)(&he), sizeof(Hemisphere));
	const double hr = flag.getThickness();
	out_.write((char *)(&hr), sizeof(double));
	const double hi = flag.getOriginHeight();
	out_.write((char *)(&hi), sizeof(double));

	Colour colour = flag.getColour();
	const MFloat r=colour.red();
	const MFloat g=colour.green();
	const MFloat b=colour.blue();
	out_.write((char *)(&r), sizeof(MFloat));
	out_.write((char *)(&g), sizeof(MFloat));
	out_.write((char *)(&b), sizeof(MFloat));
	
	const string t=flag.getOriginMarker();
        const int len = t.length();
        out_.write((char *)(&len),sizeof(int));

        char pp[len];
        strcpy(pp, t.c_str());
        out_.write(pp,sizeof(char)*len);

        Flag::const_iterator fla = flag.begin();

        for(int pts=0;pts<no;pts++)
        {
          const double x = fla->x_;
          out_.write((char *)(&x), sizeof(double));
          const double y = fla->y_;
          out_.write((char *)(&y), sizeof(double));
          const PaperPoint p = fla->point_;
          const double ax = p.x();
          out_.write((char *)(&ax), sizeof(double));
          const double ay = p.y();
          out_.write((char *)(&ay), sizeof(double));
//          out_.write((char *)(&p), sizeof(PaperPoint));
          ++fla;
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
MAGICS_NO_EXPORT void BinaryDriver::renderPolyline(const int n, MFloat *x, MFloat *y) const
{
	char c = 'H';
	out_.write(&c, 1);
	out_.write((char *)(&n), sizeof(int));
	out_.write((char *)(x),  sizeof(MFloat)*n);
	out_.write((char *)(y),  sizeof(MFloat)*n);
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
MAGICS_NO_EXPORT void BinaryDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const
{
	char c = 'B';
	out_.write(&c, 1);
	out_.write((char *)(&n), sizeof(int));
	out_.write((char *)(x),  sizeof(MFloat)*n);
	out_.write((char *)(y),  sizeof(MFloat)*n);
}


/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param line Polyline to be shaded
*/
MAGICS_NO_EXPORT void BinaryDriver::renderSimplePolygon(const Polyline& line) const
{
	line.getShading()->draw(*this);
	setNewColour(line.getFillColour());
	const unsigned int n = line.size();
	if(n<3) return;

	MFloat *x = new MFloat[n];
	MFloat *y = new MFloat[n];
	for(unsigned int i=0;i<n;i++)
	{
		const PaperPoint& pp = line.get(i);
		x[i] = pp.x();
		y[i] = pp.y();
	}
	char c = 'X';
	out_.write(&c, 1);
	out_.write((char *)(&n), sizeof(int));
	out_.write((char *)(x),  sizeof(MFloat)*n);
	out_.write((char *)(y),  sizeof(MFloat)*n);
	delete [] x;
	delete [] y;
	Polyline::Holes::const_iterator h = line.beginHoles();
	Polyline::Holes::const_iterator he = line.endHoles();
	const unsigned int nh = line.numberOfHoles();
	out_.write((char *)(&nh), sizeof(int));

	if(nh!=0)
	{
	  for (; h != he; ++h)
	  {
		vector<double> x;
		vector<double> y;
		line.hole(h,x,y);
		const unsigned int nx = x.size();
		out_.write((char *)(&nx), sizeof(int));

		MFloat *xx = new MFloat[nx];
		MFloat *yy = new MFloat[nx];
		std::copy(x.begin(), x.end(), xx);
		std::copy(y.begin(), y.end(), yy);
		out_.write((char *)(xx),  sizeof(MFloat)*nx);
		out_.write((char *)(yy),  sizeof(MFloat)*nx);
		delete [] xx;
		delete [] yy;
	  }
	}
	Colour colour = line.getFillColour();
	const MFloat r=colour.red();
	const MFloat g=colour.green();
	const MFloat b=colour.blue();
	const MFloat a=colour.alpha();
	out_.write((char *)(&r), sizeof(MFloat));
	out_.write((char *)(&g), sizeof(MFloat));
	out_.write((char *)(&b), sizeof(MFloat));
	out_.write((char *)(&a), sizeof(MFloat));
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
MAGICS_NO_EXPORT void BinaryDriver::renderSimplePolygon(const int n, MFloat* x, MFloat* y) const
{
	char c = 'S';
	out_.write(&c, 1);
	out_.write((char *)(&n), sizeof(int));
	out_.write((char *)(x),  sizeof(MFloat)*n);
	out_.write((char *)(y),  sizeof(MFloat)*n);
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void BinaryDriver::renderText(const Text& text) const
{
    if(text.empty()) return;
    const vector<NiceText>& niceT = text.getNiceText();
    if(niceT.empty()) return;
    vector<NiceText>::const_iterator niceText = text.textBegin();

    char c = 'T';
    out_.write(&c, 1);

    const int s = text.size();
    out_.write((char *)(&s),sizeof(int));

    MagFont magfont = (*niceText).font();
    const Colour& colour = magfont.colour();
    const MFloat r=colour.red();
    const MFloat g=colour.green();
    const MFloat b=colour.blue();
    out_.write((char *)(&r), sizeof(MFloat));
    out_.write((char *)(&g), sizeof(MFloat));
    out_.write((char *)(&b), sizeof(MFloat));

    const MFloat an = text.getAngle();
    out_.write((char *)(&an),sizeof(MFloat));

    bool bl = text.getBlanking();
    out_.write((char *)(&bl),sizeof(bool));

    const enum Justification horizontal = text.getJustification();
    const enum VerticalAlign vertical   = text.getVerticalAlign();
    out_.write((char *)(&horizontal),sizeof(enum Justification));
    out_.write((char *)(&vertical),  sizeof(enum VerticalAlign));

    const int noNT = niceT.size();
    out_.write((char *)(&noNT),sizeof(int));

    for(int ntc=0;ntc<noNT;ntc++)
    {
      MagFont magfont = niceT[ntc].font();
      Colour colour = magfont.colour();
      const MFloat r=colour.red();
      const MFloat g=colour.green();
      const MFloat b=colour.blue();
      out_.write((char *)(&r), sizeof(MFloat));
      out_.write((char *)(&g), sizeof(MFloat));
      out_.write((char *)(&b), sizeof(MFloat));

      const MFloat sf = magfont.size();
      out_.write((char *)(&sf),sizeof(MFloat));

      const string t=niceT[ntc].text();
      const int len = t.length();
      out_.write((char *)(&len),sizeof(int));

      char pp[len];
      strcpy(pp, t.c_str());
      out_.write(pp,sizeof(char)*len);
    }

    for(int g=0;g<s;g++)
    {
        const MFloat x = text[g].x();
        const MFloat y = text[g].y();
        out_.write((char *)(&x),sizeof(MFloat));
        out_.write((char *)(&y),sizeof(MFloat));
    }
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
MAGICS_NO_EXPORT void BinaryDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const
{
	char c = 'R';
	out_.write(&c, 1);
	out_.write((char *)(&x), sizeof(MFloat));
	out_.write((char *)(&y), sizeof(MFloat));
	out_.write((char *)(&r), sizeof(MFloat));
	out_.write((char *)(&s), sizeof(int));
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
  \param landscape says if contents is landscape

*/
MAGICS_NO_EXPORT bool BinaryDriver::renderPixmap(MFloat x0,MFloat y0,MFloat x1,MFloat y1,
                                            int w,int h,unsigned char* pixmap,int landscape, bool) const
{
	MagLog::debug() << "BinaryDriver::renderPixmap needs implementing." << std::endl;
	return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are 
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool BinaryDriver::renderCellArray(const Image& image) const
{
	char cc = 'I';
	out_.write(&cc, 1);

//	ColourTable &lt = image.getColourTable();
	const int width  = image.getNumberOfColumns();
	out_.write((char *)(&width), sizeof(int));
	const int height = image.getNumberOfRows();
	out_.write((char *)(&height), sizeof(int));
	const MFloat x0 = image.getOrigin().x();
	out_.write((char *)(&x0), sizeof(MFloat));
	const MFloat y0 = image.getOrigin().y();
	out_.write((char *)(&y0), sizeof(MFloat));
	const MFloat x1 = image.getWidth();
	out_.write((char *)(&x1), sizeof(MFloat));
	const MFloat y1 = image.getHeight();
	out_.write((char *)(&y1), sizeof(MFloat));

	ColourTable &lt = image.getColourTable();
	int si = 0;
	for ( magvector<ColourTableEntry>::const_iterator colour = lt.begin(); colour != lt.end(); ++colour ) {
		si++; 
	}
	const int sii = si;


	out_.write((char *)(&sii), sizeof(int));
	for(int v=0;v<sii;v++)
	{
	  const double r=lt[v].red();
	  const double g=lt[v].green();
	  const double b=lt[v].blue();
	  const double a=lt[v].alpha();
	  out_.write((char *)(&r), sizeof(double));
	  out_.write((char *)(&g), sizeof(double));
	  out_.write((char *)(&b), sizeof(double));
	  out_.write((char *)(&a), sizeof(double));
	}

	short *c = new short[width*height];
	for (long i=0;i<width*height;i++)
		  c[i] = image[i];
	out_.write((char *)(c), sizeof(short)*width*height);
	delete [] c;
	return true;
}



/*!
  \brief class information are given to the output-stream
*/
void BinaryDriver::print(ostream& out)  const
{
	out << "BinaryDriver[";
	out << "]";
}

static SimpleObjectMaker<BinaryDriver, BaseDriver> Binary_driver("Binary");

