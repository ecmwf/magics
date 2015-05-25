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

/*! \file PostScriptDriver.cc
    \brief Implementation of PostScriptDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: March 2004

*/

#include <PostScriptDriver.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>
#include <System.h>
#include <Symbol.h>
#include <iomanip>

/*! \brief function to convert between PS ISO encoding and Unicode

See <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">entities.html</a> for
a description of the characters.
\sa TextNode.cc
\note C O P I E D   F R O M  TEXTNODE.CC
*/
string specialPS(const string& sp)
{
	static map<string, string> specialsPS;
	if ( specialsPS.empty() )
	{
		specialsPS["©"] = "251"; //copyright
		specialsPS["°"] = "260"; //degres
		specialsPS["956"] = "265"; //mu
		specialsPS["¼"] = "274"; //vulgar fraction one quarter
		specialsPS["½"] = "275"; //vulgar fraction one half
		specialsPS["¾"] = "276"; //vulgar fraction three quarters
		specialsPS["¿"] = "277"; //inverted question mark
		specialsPS["À"] = "300"; //latin capital letter A with grave
		specialsPS["Á"] = "301"; //latin capital letter A with acute

		specialsPS["Ã"] = "303"; //latin capital letter A with tilde
		specialsPS["Ä"] = "304"; //latin capital letter A with diaeresis
		specialsPS["Å"] = "305"; //latin capital letter A with ring above
		specialsPS["Æ"] = "306"; //latin capital letter AE
		specialsPS["Ç"] = "307"; //latin capital letter C with cedilla

		specialsPS["È"] = "310"; //latin capital letter E with grave
		specialsPS["É"] = "311"; //latin capital letter E with acute
		specialsPS["Ê"] = "312"; //latin capital letter E with circumflex
		specialsPS["Ë"] = "313"; //latin capital letter E with  diaeresis

		specialsPS["Ì"] = "314"; //latin capital letter I with grave
		specialsPS["Í"] = "315"; //latin capital letter I with acute
		specialsPS["Î"] = "316"; //latin capital letter I with circumflex
		specialsPS["Ï"] = "317"; //latin capital letter I with  diaeresis

		specialsPS["Ñ"] = "321"; //latin capital letter N with tilde

		specialsPS["Ò"] = "322"; //latin capital letter O with grave
		specialsPS["Ó"] = "323"; //latin capital letter O with acute
		specialsPS["Ô"] = "325"; //latin capital letter O with  diaeresis
		specialsPS["Õ"] = "330"; //latin capital letter O slash

		specialsPS["Ù"] = "331"; //latin capital letter U with grave
		specialsPS["Ú"] = "332"; //latin capital letter U with acute
		specialsPS["Û"] = "333"; //latin capital letter U with circumflex
		specialsPS["Ü"] = "334"; //latin capital letter U with  diaeresis

		specialsPS["Ý"] = "335"; //latin capital letter Y with acute

		specialsPS["Þ"] = "336"; //latin capital letter THORN
		specialsPS["ß"] = "337"; //latin small letter sharp s = ess-zed

		specialsPS["à"] = "340"; //latin small letter a with grave
		specialsPS["á"] = "341"; //latin small letter a with cute
		specialsPS["â"] = "342"; //latin small letter a with circumflex
		specialsPS["ã"] = "343"; //latin small letter a with tilde
		specialsPS["ä"] = "344"; //latin small letter a with  diaeresis
		specialsPS["å"] = "345"; //latin small letter a with ring above
		specialsPS["æ"] = "346"; //latin small letter ae
		specialsPS["ç"] = "347"; //latin small letter c with cedilla

		specialsPS["è"] = "350"; //latin small letter e with grave
		specialsPS["é"] = "351"; //latin small letter e with cute
		specialsPS["ê"] = "352"; //latin small letter e with circumflex
		specialsPS["ë"] = "353"; //latin small letter e with ring above

		specialsPS["ì"] = "354"; //latin small letter i with grave
		specialsPS["í"] = "355"; //latin small letter i with cute
		specialsPS["î"] = "356"; //latin small letter i with circumflex
		specialsPS["ï"] = "357"; //latin small letter i with diaeresis

		specialsPS["ñ"] = "361"; //latin small letter n with tilde

		specialsPS["ò"] = "362"; //latin small letter o with grave
		specialsPS["ó"] = "363"; //latin small letter o with cute
		specialsPS["ö"] = "364"; //latin small letter o with diaeresis
		specialsPS["õ"] = "365"; //latin small letter o with tilde

		specialsPS["ø"] = "370"; //latin small letter o slash

		specialsPS["ù"] = "371"; //latin small letter u with grave
		specialsPS["ú"] = "372"; //latin small letter u with cute
		specialsPS["û"] = "373"; //latin small letter u with circumflex
		specialsPS["ü"] = "374"; //latin small letter u with diaeresis

		specialsPS["ý"] = "375"; //latin small letter y with acute

		specialsPS["þ"] = "376"; //latin small letter THORN
		specialsPS["ÿ"] = "377"; //latin small letter y with diaeresis
	}

	const map<string, string>::iterator f = specialsPS.find(sp);
	return( f == specialsPS.end() ) ? "" : "\\"+f->second;
}


using namespace magics;

/*!
  \brief Constructor

The PostScript driver produces one or more (if split is activated) text files
in PostScript format.

*/
PostScriptDriver::PostScriptDriver() : ps_(true),pdf_(false),eps_(false),old_(true),
				maxPathSize_(200),deviceColourModel_(1)
{
  readFonts();
}

/*!
  \brief Destructor
*/
PostScriptDriver::~PostScriptDriver()
{
  relieveFonts();
}

/*!
  \brief Opening the driver
*/
void PostScriptDriver::open()
{
	currentPage_ = 0;
	setCMscale(resolution_/2.54);// cm -> pixel
	if(!isSplit()) openFile();
}

/*!
  \brief Closing the driver
*/
void PostScriptDriver::close()
{
	if(!isSplit()) closeFile();

	currentPage_ = 0; // reset values for new pages ...
	coordRatioX_ = 1.;
	coordRatioY_ = 1.;
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up. Strongly
  depends on what output was selected (split or eps)
*/
MAGICS_NO_EXPORT void PostScriptDriver::startPage() const
{
	dimensionX_ = convertCM(getXDeviceLength()); // 72   = points / inch
	dimensionY_ = convertCM(getYDeviceLength()); // 2.54 = cm / inch

	MFloat resolution=resolution_;
	MFloat ratio=1.;
	int    width=0;
	string mbg_tmpl = mgb_template_;

	if(!mbg_tmpl.empty())
	{
		setDimensionsFromBinary(mbg_tmpl,ratio,width);
		setCMscale(35.);
		resolution=80.; // dpi for web
		dimensionX_ = width;
		dimensionY_ = maground(dimensionX_*ratio);
	}

	newPage_ = true;
//	if(currentPage_ > 0) endPage();
	if(isSplit()) openFile();
	currentPage_++;
	fstream *ps = getStream();

	if(!isSplit()) *ps << "%%Page: " << currentPage_ << " " << currentPage_ << "\n";
	else *ps << "%%Page: 1 1\n";

	// Here the whole page gets scaled to the resolution!
	*ps << "gs " << 72./resolution << " dup s ";
if(old_)
{
	if( ( isEPS() && (dimensionX_ < dimensionY_) ) ||
	    (!isEPS() && (dimensionX_ > dimensionY_) ) )
		*ps << static_cast<int>(dimensionY_) << " 0 t 90 ro ";
}
	*ps << "1 lw [] 0 sd ";

	setDeviceColourModel(colour_model_);

	*ps << "2 setlinejoin 0 1 SUP 0 10 SF 0 SHA 0 SVA\n"; // MITER is now always on
	*ps << "0 0 0 0 Y n 0 0 m "<< dimensionX_<<" 0 rl 0 "<< dimensionY_ <<" rl "<< -dimensionX_<<" 0 rl cp fill\n";// force white background
	currentColour_ = Colour("none");
	if(scale_<1.0)
	{
	  *ps << dimensionX_*.5<< " " << dimensionY_*.5 << " t\n";
	  *ps << scale_<< " " << scale_ << " s\n";
	  *ps << -dimensionX_*.5<< " " << -dimensionY_*.5 << " t\n";
	}
}

/*!
  \brief ending a page

  This method has to take care that for formats with multiple output
  files are closed.
*/
MAGICS_NO_EXPORT void PostScriptDriver::endPage() const
{
	fstream *ps = getStream();
	*ps << "S\n";
	debugOutput("End of page");
	if(isSplit()) closeFile();
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void PostScriptDriver::project(const magics::Layout& layout) const
{
	currentWrittenColour_=Colour("NONE");
	debugOutput("Begin layout "+layout.name());

	dimensionStack_.push(dimensionX_);
	dimensionStack_.push(dimensionY_);
	scalesX_.push(coordRatioX_);
	scalesY_.push(coordRatioY_);

	MFloat offsetX_ = (layout.x()     * 0.01 * dimensionX_);
	MFloat offsetY_ = (layout.y()     * 0.01 * dimensionY_);
	dimensionX_  =     layout.width() * 0.01 * dimensionX_;
	dimensionY_  =     layout.height()* 0.01 * dimensionY_;

	const MFloat sumX = layout.maxX() - layout.minX();
	const MFloat sumY = layout.maxY() - layout.minY();

	if( sumX!=0 && sumY!=0 )
	{
		coordRatioX_ = (dimensionX_/sumX);
		coordRatioY_ = (dimensionY_/sumY);
	}

	const MFloat X_ = offsetX_+projectX( -layout.minX());
	const MFloat Y_ = offsetY_+projectY( -layout.minY());

	fstream *ps = getStream();
	*ps	<< "gs";
//	if(fabs(X_) > 0.0001 && fabs(Y_) > 0.0001 )
		*ps<<" "<< X_ <<" "<< Y_ <<" t";
	*ps	<<"\n";

}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

*/
MAGICS_NO_EXPORT void PostScriptDriver::unproject() const
{
	currentWrittenColour_=Colour("NONE");
	dimensionY_ = dimensionStack_.top();dimensionStack_.pop();
	dimensionX_ = dimensionStack_.top();dimensionStack_.pop();
	coordRatioX_  = scalesX_.top();scalesX_.pop();
	coordRatioY_  = scalesY_.top();scalesY_.pop();
	
	fstream *ps = getStream();
	*ps << "gr\n";
	setLineParameters(M_SOLID, 1);
	debugOutput("End layout");
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void PostScriptDriver::setNewColour(const Colour &colour) const
{
	if(currentColour_ == colour) return;
	currentColour_ = colour;
}

MAGICS_NO_EXPORT void PostScriptDriver::writeColour() const
{
	if(currentWrittenColour_==currentColour_) return;
	currentWrittenColour_=currentColour_;
	MFloat c,m,y,k,gray;
	const MFloat r = currentColour_.red();
	const MFloat g = currentColour_.green();
	const MFloat b = currentColour_.blue();

	fstream *ps = getStream();
	streamsize ss = ps->precision(2);
	switch(getDeviceColourModel())
	{
		case 0:    // rgb
			*ps << r << " " << g << " " << b << " C\n";
			break;
		case 1:    // CMYK
			c = 1. - r;
			m = 1. - g;
			y = 1. - b;
			k = (c < m) ? c : m;
			k = (y < k) ? y : k;
			if( k == 1. )
				*ps << "0 0 0 1 Y\n";
			else
			{
				c = (c - k) / (1.-k);
				m = (m - k) / (1.-k);
				y = (y - k) / (1.-k);
				*ps << c << " " << m << " " << y << " " << k << " Y\n";
			}
			break;
		case 2 :     // monochrome - RGB
			if ( (r ==1.) && (g == 1.) && ( b ==1. ))
				*ps << "1 1 1 C\n";
			else
				*ps << "0 0 0 C\n";
			break;
		case 3:     // RGB gray
			gray = 0.3*r + 0.59*g + 0.11*b;
			*ps << gray << " " << gray << " " << gray << " C\n";
			break;
		case 4 :    // monochrome - CMYK
			if ( (r ==1.) && (g == 1.) && ( b ==1. ))
				*ps << "0 0 0 0 Y\n";
			else
				*ps << "0 0 0 1 Y\n";
			break;
		case 5:    // CMYK gray
			gray = 0.3*r + 0.59*g + 0.11*b;
			*ps << "0 0 0 " << gray << " Y\n";
			break;
		default:   // RGB
			*ps << r << " " << g << " " << b << " C\n";
			break;
	}// end switch
	ps->precision(ss);
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished. 

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void PostScriptDriver::setNewLineWidth(const MFloat width) const
{
	if(currentLineWidth_ == width) return;
	currentLineWidth_ = width;

	fstream *ps = getStream();
	*ps << currentLineWidth_<< " lw\n";
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT int PostScriptDriver::setLineParameters(const LineStyle linestyle, const MFloat w) const
{
	setNewLineWidth(w);
	if(currentLineType_ == linestyle) return 0;
	currentLineType_ = linestyle;

	fstream *ps = getStream();

	const int width = (int)(currentLineWidth_+.5);
	const int sw = (currentLineWidth_ > 2.) ? 1 : 0;

	switch(currentLineType_)
	{
		case M_SOLID:
			*ps << "[] 0 sd\n";
			break;
		case M_DASH:
			*ps << "["  <<
				(( sw ) ?  4*width : 16 ) << " " <<
				(( sw ) ?  1*width :  8 ) << "] 8 sd\n";
			break;
		case M_DOT:
			*ps << "[" <<
				(( sw ) ? width : 4 ) << " " <<
				(( sw ) ? width : 8 ) << "] 4 sd\n";
			break;
		case M_CHAIN_DASH:
			*ps << "[" <<
				( (sw) ? width*4 : 16 ) << " " <<
				( (sw) ? width*1 :  8 ) << " " <<
				( (sw) ? width*1 :  4 ) << " " <<
				( (sw) ? width*1 :  8 ) << " "
				<< "] 0 sd\n";
			break;
		case M_CHAIN_DOT:
			*ps << "[" <<
				( (sw) ? width*4 : 12 ) << " " <<
				( (sw) ? width*1 :  8 ) << " " <<
				( (sw) ? width*1 :  4 ) << " " <<
				( (sw) ? width*1 :  8 ) << " " <<
				( (sw) ? width*1 :  4 ) << " " <<
				( (sw) ? width*1 :  8 ) << " "
				<< "] 0 sd\n";
			break;
		default:
			*ps << "[] 0 sd\n";
			break;
	}// end switch
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
MAGICS_NO_EXPORT void PostScriptDriver::renderPolyline(const int n, MFloat *x, MFloat *y) const
{
	if(n < 2 || (currentColour_==Colour("NONE"))) return;
	writeColour();

	MFloat *xx = x;
	MFloat *yy = y;

	std::fstream *ps = getStream();

	if(n == 2)
	{
		const MFloat ix0 = projectX(*xx);
		xx++;
		const MFloat iy0 = projectY(*yy);
		yy++;
		const MFloat dx0 = projectX(*xx)-ix0;
		const MFloat dy0 = projectY(*yy)-iy0;
		if( zero(dx0)&&zero(dy0) ) return;
		*ps << dx0 << " " << dy0 << " " << ix0 << " " << iy0 <<" B\n";
	}
	else
	{
		int nn = n;
		MFloat *dx,*dy;
		streamsize ss = ps->precision(2);

		while(nn>1)
		{
			unsigned int p = ( nn > int(maxPathSize_)) ? maxPathSize_ : nn;
			dx = new MFloat[p+1];
			dy = new MFloat[p+1];

			MFloat kx = projectX(*xx);
			xx++;
			MFloat ky = projectY(*yy);
			yy++;

			*(dx++) = kx;
			*(dy++) = ky;

			unsigned int i;
			for(i=1; i<p; i++)
			{
				const MFloat cx = projectX(*xx);xx++;
				*(dx++) = cx - kx;
				kx = cx;
				const MFloat cy = projectY(*yy);yy++;
				*(dy++) = cy - ky;
				ky = cy;
			}

			int counter = 0; // to avoid to long lines
			for(i=p-1; i>0; i--)
			{
				const MFloat ddx = *(--dx);
				const MFloat ddy = *(--dy);

				if( !(zero(ddx) && zero(ddy)) )
				{
					*ps << ddx << " " << ddy<< " ";
					counter++;
				}
				else
					p--;

				if(counter>15) {*ps << "\n";counter=0;}
			}
			--dx;--dy;

			if(p>1) *ps << p-1 << " " << *dx << " " << *dy << " p\n";

			nn = nn - maxPathSize_;
			if(++nn>1)
			{
				// Compensate for additional point (last becomes first)
				--xx;--yy;
			}
			delete [] dx;
			delete [] dy;
		}// end while
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
MAGICS_NO_EXPORT void PostScriptDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const
{
	if(n != 2 || (currentColour_==Colour("NONE"))) return;
	writeColour();

	MFloat *xx = x;
	MFloat *yy = y;

	const MFloat ix0 = *xx;
	xx++;
	const MFloat iy0 = *yy;
	yy++;
	const MFloat dx0 = *xx-ix0;
	const MFloat dy0 = *yy-iy0;

	if( zero(dx0) && zero(dy0) ) return;

	std::fstream *ps = getStream();
	*ps << dx0 << " " << dy0 << " " << ix0 << " " << iy0 <<" B\n";
}

/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param line polyline to be filled
*/
void PostScriptDriver::renderSimplePolygon(const Polyline& line) const
{ 
	unsigned int n = line.size();
	setNewColour(line.getFillColour());
	line.getShading()->draw(*this);

	if( n<3 || (currentColour_==Colour("NONE")) ) return;
	writeColour();

	std::fstream *ps = getStream();
	*ps << "gs\n";

	if (currentShading_==M_SH_DOT)
	{
		const DotShadingProperties *pro = (DotShadingProperties*)currentShadingProperties_;
		const int density = (int)sqrt(pro->density_);
		if(density<=0) return;
		const MFloat square_size = convertCM(1.)/density;

		int s = (int)pro->size_*convertCM(1.);
		if (s<2) s = 2;

		const MFloat r = currentColour_.red();
		const MFloat g = currentColour_.green();
		const MFloat b = currentColour_.blue();

		MFloat c = 1. - r;
		MFloat m = 1. - g;
		MFloat z = 1. - b;
		MFloat k = (c < m) ? c : m;
		      k = (z < k) ? z : k;

		*ps
		    << "/Pat {\n gs 0 0 " << square_size << " " << square_size << " rectclip gr gs ";
		    if( k == 1. )
		      *ps << "0 0 0 1";
		    else
		    {
		     c = (c - k) / (1.-k);
		     m = (m - k) / (1.-k);
		     z = (z - k) / (1.-k);
		     *ps << c << " " << m << " " << z << " " << k;
		    }
		*ps << " setcmykcolor 1 1 m 0 "<<s<<" rl "<<s<<" 0 rl 0 -"<<s<<" rl cp fill gr "
		    << "} bind def\n"
		    << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
		    << "/BBox [0 0 "<<square_size<<" "<<square_size<<"] /XStep "<<square_size<<" /YStep "<<square_size<<"\n"
		    << "/PaintProc { Pat }\n"
		    << ">>\n"
		    << "matrix makepattern setpattern\n";
	}
	else if (currentShading_==M_SH_HATCH)
	{
		const HatchShadingProperties *pro = (HatchShadingProperties*)currentShadingProperties_;
		indexHatch_ = pro->index_;
		if(indexHatch_<1 || indexHatch_>6)
		{
			MagLog::warning() << "PostScriptDriver::renderSimplePolygon > Hatch index " <<indexHatch_<<" is wrong. No hatch shading possible!" << endl;
			if(indexHatch_==0)
			  MagLog::debug() << "PostScriptDriver::renderSimplePolygon > Hatch index is 0. Alternative hatch patterns between 1-6 should have been sent!" << endl;
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
		      k = (z < k) ? z : k;

		*ps
		    << "/Pat {\n gs 0 0 " << s << " " << s << " rectclip gr gs ";
		    if( k == 1. )
		      *ps << "0 0 0 1";
		    else
		    {
		     c = (c - k) / (1.-k);
		     m = (m - k) / (1.-k);
		     z = (z - k) / (1.-k);
		     *ps << c << " " << m << " " << z << " " << k;
		    }

		*ps << " setcmykcolor";

		if(indexHatch_==1 || indexHatch_==3) // horizontal
		{
			*ps  << " 0 "<<s*.5<<" m "<<s<<" 0 rl st";
		}
		if(indexHatch_==2 || indexHatch_==3)
		{
			*ps  << " "<<s*.5<<" 0 m 0 "<<s<<" rl st";
		}
		if(indexHatch_==4 || indexHatch_==6)
		{
			*ps  << " 0 0 m "<<s<<" "<<s<<" rl st";
		}
		if(indexHatch_==5 || indexHatch_==6)
		{
			*ps  << " 0 "<<s<<" m "<<s<<" -"<<s<<" rl st";
		}

		*ps << " gr } bind def\n"
		    << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
		    << "/BBox [0 0 "<<s<<" "<<s<<"] /XStep "<<s<<" /YStep "<<s<<"\n"
		    << "/PaintProc { Pat }\n"
		    << ">>\n"
		    << "matrix makepattern setpattern\n";
	}// end hatch

	*ps << "n ";

	Polyline::Holes::const_iterator h = line.beginHoles();
	Polyline::Holes::const_iterator he = line.endHoles();

	for (; h != he; ++h)
	{
		vector<double> x;
		vector<double> y;
		line.hole(h,x,y);
		MFloat old_x = projectX(x[0]);
		MFloat old_y = projectY(y[0]);
		unsigned int nt = x.size();

//		if ( (line[n-1].x() == line[0].x()) && (line[n-1].y() == line[0].y()) ) {x_end--;nt--;}

		int pcounter=0;
		for(int i = nt-1; i > -1; --i)
		{
		  const MFloat xx = projectX(x[i]);
		  const MFloat yy = projectY(y[i]);
		  const MFloat diffX = old_x-xx;
		  const MFloat diffY = old_y-yy;

		  if( !(zero(diffX) && zero(diffY)) )
		  {
		    *ps << diffX <<" "<< diffY<<" ";
		    old_x = xx;
		    old_y = yy;
		    pcounter++;
		    if(pcounter%10==0) *ps << "\n";
		  }
		}
		*ps << pcounter << " " << projectX(x[0]) << " " << projectY(y[0]) << " F P\n";
	}

	if ( (line.get(n-1).x() == line.get(0).x()) && (line.get(n-1).y() == line.get(0).y()) ) n--;

	MFloat old_x = projectX(line.get(0).x());
	MFloat old_y = projectY(line.get(0).y());

	const MFloat mx = old_x;
	const MFloat my = old_y;
	int pcounter=0;
	for(int i = n-1; i > -1; --i)
	{
		const PaperPoint& pp = line.get(i);
		const MFloat xx = projectX(pp.x());
		const MFloat yy = projectY(pp.y());
		const MFloat diffX = old_x-xx;
		const MFloat diffY = old_y-yy;

/*		if(zero(diffX) && (pp.x()==line[i+1].x()))
		{
		  int j = i+1;
		  while(pp.x()==line[j].x())
		  {
		    j++;
		  }
		  i=j;
		  *ps << "0 " << old_y-projectY(line[i].y()) << " ";
		   old_y = projectY(line[i].y());
		   if(pcounter%10==0) *ps << "\n";
		   pcounter++;
		}
		else */ 
if( !(zero(diffX) && zero(diffY)) )
		{
		   *ps <<  diffX << " " << diffY << " ";
		   old_x = xx;
		   old_y = yy;
		   if(pcounter%10==0) *ps << "\n";
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
MAGICS_NO_EXPORT void PostScriptDriver::renderSimplePolygon(const int n, MFloat* x, MFloat* y) const
{
	if(n<3 || (currentColour_==Colour("NONE"))) return;

//MagLog::dev()<< "PS_SIMPLE " << n<<" points   col: "<< currentColour_<< endl;

	int nn = n;
	if ( (x[nn-1] == x[0]) && (y[nn-1] == y[0]) ) nn--;

	const int N = nn+1;
	MFloat *rx = new MFloat[N];
	MFloat *ry = new MFloat[N];
	MFloat *dx = rx; dx++;
	MFloat *dy = ry; dy++;
	MFloat *xx = x, *yy = y;
	MFloat fx = projectX(*(xx++));
	MFloat fy = projectY(*(yy++));

	int i;
	for( i = 1; i<nn; i++)
	{
		const MFloat pxx = projectX(*xx);
		const MFloat pyy = projectY(*yy);
		*(dx++) = pxx - fx; fx = pxx; xx++;
		*(dy++) = pyy - fy; fy = pyy; yy++;
	}

	std::fstream *ps = getStream();
	

	if (currentShading_==M_SH_DOT)
	{
		const DotShadingProperties *pro = (DotShadingProperties*)currentShadingProperties_;
		const int density = (int)sqrt(pro->density_);
		if(density<=0)
		{
			if(density<0) MagLog::warning() << "PostScriptDriver::renderSimplePolygon > Dot density " <<density<<" is negative! No shading applied" << endl;
			return;
		}
		const MFloat square_size = convertCM(1.)/density;

		int s = (int)pro->size_*convertCM(1.) *.2;
		if (s<2) s = 2;

		const MFloat r = currentColour_.red();
		const MFloat g = currentColour_.green();
		const MFloat b = currentColour_.blue();

		MFloat c = 1. - r;
		MFloat m = 1. - g;
		MFloat z = 1. - b;
		MFloat k = (c < m) ? c : m;
		      k = (z < k) ? z : k;

		*ps << "gs  %%\n";

		*ps
		    << "/Pat {\n gs 0 0 " << square_size << " " << square_size << " rectclip gr gs ";
		    if( k == 1. )
		      *ps << "0 0 0 1";
		    else
		    {
		     c = (c - k) / (1.-k);
		     m = (m - k) / (1.-k);
		     z = (z - k) / (1.-k);
		     *ps << c << " " << m << " " << z << " " << k;
		    }
		*ps << " setcmykcolor 1 1 m 0 "<<s<<" rl "<<s<<" 0 rl 0 -"<<s<<" rl cp fill gr "
		    << "} bind def\n"
		    << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
		    << "/BBox [0 0 "<<square_size<<" "<<square_size<<"] /XStep "<<square_size<<" /YStep "<<square_size<<"\n"
		    << "/PaintProc { Pat }\n"
		    << ">>\n"
		    << "matrix makepattern setpattern\n";

	}
	else if (currentShading_==M_SH_HATCH)
	{
		const HatchShadingProperties *pro = (HatchShadingProperties*)currentShadingProperties_;
		indexHatch_ = pro->index_;
		if(indexHatch_<1 || indexHatch_>6)
		{
			MagLog::warning() << "PostScriptDriver::renderSimplePolygon > Hatch index " <<indexHatch_<<" is wrong. No hatch shading possible!" << endl;
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
		      k = (z < k) ? z : k;

		*ps << "gs\n"
		    << "/Pat {\n gs 0 0 " << s << " " << s << " rectclip gr gs ";
		    if( k == 1. )
		      *ps << "0 0 0 1";
		    else
		    {
		     c = (c - k) / (1.-k);
		     m = (m - k) / (1.-k);
		     z = (z - k) / (1.-k);
		     *ps << c << " " << m << " " << z << " " << k;
		    }

		*ps << " setcmykcolor";

		if(indexHatch_==1 || indexHatch_==3) // horizontal
		{
			*ps  << " 0 "<<s*.5<<" m "<<s<<" 0 rl st";
		}
		if(indexHatch_==2 || indexHatch_==3)
		{
			*ps  << " "<<s*.5<<" 0 m 0 "<<s<<" rl st";
		}
		if(indexHatch_==4 || indexHatch_==6)
		{
			*ps  << " 0 0 m "<<s<<" "<<s<<" rl st";
		}
		if(indexHatch_==5 || indexHatch_==6)
		{
			*ps  << " 0 "<<s<<" m "<<s<<" -"<<s<<" rl st";
		}

		*ps << " gr } bind def\n"
		    << "<< /PatternType 1 /PaintType 1 /TilingType 1\n"
		    << "/BBox [0 0 "<<s<<" "<<s<<"] /XStep "<<s<<" /YStep "<<s<<"\n"
		    << "/PaintProc { Pat }\n"
		    << ">>\n"
		    << "matrix makepattern setpattern\n";
	}// end hatch
	else
	{
	  *ps << "gs\n";
	}

	*dx = projectX(x[0]) - projectX(x[nn-1]);
	*dy = projectY(y[0]) - projectY(y[nn-1]);
	rx[0] = projectX(x[0]);
	ry[0] = projectY(y[0]);

	for ( i=nn; i>0; i--)
	{
		*ps << (*dx--) << " " << *(dy--)<< " ";
	}
	*ps <<nn << " " << rx[0] << " " << ry[0] << " e gr\n";

	delete [] rx;
	delete [] ry;
//	currentShading_=M_SH_SOLID;
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \note As of version 2.0 there are two forms of describing text in Text.
  \todo Underlining of text

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void PostScriptDriver::renderText(const Text& text) const
{
	if(text.empty()) return;
	const vector<NiceText>& niceT = text.getNiceText();
	if(niceT.empty()) return;
	currentWrittenColour_=Colour("NONE");   // reset colours

	fstream *ps = getStream();
	streamsize ss = ps->precision(2);

	*ps << text.getJustification() << " SHA " << text.getVerticalAlign() << " SVA ";

	vector<NiceText>::const_iterator niceText = text.textBegin();
	vector<NiceText>::const_iterator niceTextEnd = text.textEnd();

	int u=0;
	ostringstream all_text;
	for(;niceText<niceTextEnd;)
	{
		all_text << (*niceText).text();
		niceText++;
		u++;
	}

	niceText = text.textBegin();
	MFloat old_offset = 0;
	int count = 0;

	for(;niceText<niceTextEnd;)
	{
		const MagFont magfont = (*niceText).font();
		MFloat height = convertCM(magfont.size());
		if(height < epsilon) {
			++niceText;
			continue;
		}

		const std::set<string>& styles = magfont.styles();

		string style = "";
		if(styles.find("bold") != styles.end()) style = "bold";
		else if(styles.find("italic") != styles.end()) style += "italic";
		else if(styles.find("bolditalic") != styles.end()) style += "bolditalic";
		if(style == "") style = "normal";
		const string lowFont = lowerCase(magfont.name()+"_"+style);

		fontMapIter iter = FontMap_.find(lowFont);

		const bool underlined = (styles.find("underlined") != styles.end()) ? true : false;

		int font;
		if(iter!=FontMap_.end())
			font = iter->second.id;
		else
		{
			font = 0; // if not found get default
			MagLog::warning() << "PostScriptDriver: Font "<< lowFont << " is not registered!\n   Default font used for "<<(*niceText).text()<<"."<< endl;
		}

		setNewColour(magfont.colour());
		writeColour();

		MFloat offset = 0;
		if((*niceText).elevation()==NORMAL)           {offset= -old_offset; old_offset=0;}
		else if((*niceText).elevation()==SUPERSCRIPT) {offset= height*.5 - old_offset; old_offset=offset; height *= .8; }
		else if((*niceText).elevation()==SUBSCRIPT)   {offset=-height*.2 - old_offset; old_offset=offset; height *= .8; }

		*ps << font << " " << static_cast<int>(height) << " SF ";

		// plot strings
		string textCommand = (text.getBlanking()) ? "TB" : "T";
		if(underlined) textCommand = "TU";

		const int len = (*niceText).text().length()+1;
		char pp[len];
		strcpy(pp, (*niceText).text().c_str());
		string spp= (*niceText).text();
		char *p = pp;
		ostringstream tmp;
		int counter=0;

		while(*p)
		{
		  if ( *p == '(')    {tmp << "\\(";}
		  else if ( *p == ')')    {tmp << "\\)";}
		  else if ( *p == '\\')   {tmp << "\\\\";}
		  else if ( *p & 0x80)    {tmp << specialPS(spp.substr(counter,2));}   // temp fix for multibyte char (degree sign)
		  else                    {tmp << *p;}
		  p++;
		  counter++;
		}

	  const string showCommand = (underlined) ? "ushow" : "show";
	  unsigned int noTexts = text.size();
	  for(unsigned int nT=0;nT<noTexts;nT++)  // for all string CO-ORDINATES
	  {
		if(niceText == text.textBegin())
		{
			const MFloat x0 = projectX(text[nT].x());
			const MFloat y0 = projectY(text[nT].y()) + offset;

			if(u>1)
			{
				const MFloat an = 360.-(text.getAngle()*57.29577951);
				if(an==0 || an==360)
				    *ps <<"gs "<< x0 << " " << y0 << " t ("<< all_text.str() << ") stringwidth pop HA mul VA Height mul moveto "
				        << "("<<tmp.str()<< ") "<<showCommand<<"\n";
				else
				    *ps <<"gs "<< x0 << " " << y0 << " t "<<an<< " ro ("<< all_text.str() << ") stringwidth pop HA mul VA Height mul moveto "
				        << "("<<tmp.str()<< ") "<<showCommand<<"\n";
			}
			else
			{


				const MFloat an = 360.-(text.getAngle()*57.29577951);
				if(an==0 || an==360)
					*ps <<"gs "<< x0 << " " << y0 << " t ("<<tmp.str()<< ") 0 0 "<<textCommand<<"\n";
				else
					*ps <<"gs "<< x0 << " " << y0 << " t "<<an<< " ro ("<<tmp.str()<< ") 0 0 "<<textCommand<<"\n";
			}
		}
		else
		{
		*ps << " 0 "<<offset<<" rmoveto\n";
			*ps << "("<<tmp.str()<< ") "<<showCommand<<"\n";
		}
		count++;
		if (niceText+1 == text.textEnd()) *ps <<"gr\n";
	   }
	   niceText++;
	} // endfor all nicetexts
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
MAGICS_NO_EXPORT void PostScriptDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const
{
	writeColour();
	std::fstream *ps = getStream();
	const MFloat cx = projectX(x);
	const MFloat cy = projectY(y);

	if(s < 8)
	{
		*ps << "n " << cx << " " << cy << " " << r << " 0 360 arc st\n";
		if(s > 0)
			*ps <<"n "<<cx<<" "<<cy<<" m "<<cx<<" "<<cy<<" "<<r<<" 90 "<<90-(s*45)<<" arn\n";
	}
	else *ps << "n " << cx << " " << cy << " " << r << " 0 360 ar\n";
	if(s == 9)
	{
		*ps << "1 1 1 C n "<<cx<<" "<<cy+r-1<<" m 0 "<<-r-r+2<<" rl st\n";
		const Colour col = currentColour_;
		currentColour_ = Colour("white");
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
MAGICS_NO_EXPORT bool PostScriptDriver::renderPixmap(MFloat x0,MFloat y0,MFloat x1,MFloat y1,
                                            int width,int height,unsigned char* pixmap,int landscape, bool alpha) const
{
	if(landscape) //swop w/h
	{
		 const int x = width;
		 width = height;
		 height = x;
	}
	if(height==0 || width==0)  return false;

	unsigned char *p = pixmap;
	std::fstream *ps = getStream();
	const int col_model = getDeviceColourModel();
	const MFloat dx = x1 - x0 + 1;
	const MFloat dy = y1 - y0 + 1;

	*ps << "gs /pic " << width*(( col_model == 1 ) ? 4 : 3) << " string def " << x0 << " " << y0
	    << " t " << dx << " " << dy << " s " << width
	    << " " << height << " 8\n"
	    << "[" << width << " 0 0 " << height << " 0 0] "
	    << "{currentfile pic readhexstring pop}"
	    << " false " << (( col_model == 1 ) ? 4 : 3) <<" colorimage\n";

	char *t = new char[9];
	int nl = 0;
	for(int j=height-1;j>=0;j--)
	{
	  for(int i=width-1;i>=0;i--)
	  {
		// Get image left-right and bottom-up
		const int n = ( landscape ) ? (height*i+j)*3 : (j*width + width-1-i )*3;
		unsigned char *p2 = p+n;
		unsigned char r = *(p2++);
		unsigned char g = *(p2++);
		unsigned char b = *(p2++);
		if(alpha) p2++;                 // ignore alpha values ... :-(
		short kr,kg,kb,kc,km,ky,kk;
		MFloat cc,cm,cy,ck;

		switch ( col_model )
		{
			 case 0:
				 sprintf(t,"%02hhx%02hhx%02hhx",r,g,b);
				 break;
			 case 1:
				 cc = 1. - (r*0.00392156); cm = 1. - (g*0.00392156); cy = 1. - (b*0.00392156);
				 ck = ( cc < cm ) ?  cc : cm;
				 if ( cy < ck ) ck = cy;
				 if( ck == 1. )
				 { kc = 0; km= 0; ky = 0; kk = 255;}
				 else
				 {
					 kc = int( ((cc - ck) / (1.-ck)) * 255.);
					 km = int( ((cm - ck) / (1.-ck)) * 255.);
					 ky = int( ((cy - ck) / (1.-ck)) * 255.);
					 kk = int( ck * 255.);
				 }
				 sprintf(t,"%02hx%02hx%02hx%02hx",kc,km,ky,kk);
				 break;
			 case 2:
				 if ( (r ==255) && (g == 255) && ( b ==255 ))
				  { kr = 255; kg = 255; kb = 255;}
				 else
				  { kr = 0; kg = 0; kb = 0;}
				 sprintf(t,"%02hx%02hx%02hx",kr,kg,kb);
				 break;
			 case 3:
				 kr = int(0.3*r + 0.59*g + 0.11*b);
				 sprintf(t,"%02hx%02hx%02hx",kr,kr,kr);
				 break;
			 default:
				 sprintf(t,"%02hhx%02hhx%02hhx",r,g,b);
				 break;
		}
		*ps << t;
		if ( (++nl)%12 == 0) *ps << "\n";
	   }
	 }
	 *ps << "gr" << std::endl;
	 currentColour_ = Colour("none");
	 delete [] t;
	 return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool PostScriptDriver::renderCellArray(const Image& image) const
{
   ColourTable &lt  = image.getColourTable();
   const int width  = image.getNumberOfColumns();
   const int height = image.getNumberOfRows();

//MagLog::dev()<<" PostScriptDriver::renderCellArray "<< width << endl;

   if(width > 0 && height > 0)
   {
	const int col_model = getDeviceColourModel();
	
	const MFloat x0 = projectX(image.getOrigin().x());
	const MFloat y0 = projectY(image.getOrigin().y()-image.getHeight());
	const MFloat x1 = projectX(image.getOrigin().x()+image.getWidth());
	const MFloat y1 = projectY(image.getOrigin().y());
	const MFloat dx = x1 - x0 + 1;
	const MFloat dy = y1 - y0 + 1;
//	bool mask = false;

	fstream *ps = getStream();
	*ps << "gs /pic " << width*(( col_model == 1 ) ? 4 : 3) << " string def " << x0 << " " << y0
	    << " t " << dx << " " << dy << " s " << width
	    << " " << height << " 8\n"
	    << "[" << width << " 0 0 " << height << " 0 0] "
	    << "{currentfile pic readhexstring pop}"
	    << " false " << (( col_model == 1 ) ? 4 : 3) <<" colorimage\n";

	char *t = new char[9];
	int nl = 0;
	for (int i=height-1;i>=0;i--)
	{
		for (int j=0;j<width;j++)
		{
			short kr,kg,kb,kc,km,ky,kk;
			MFloat cc,cm,cy,ck;
			const int in = width*i+j;
			const short c = image[in];

			MFloat r = lt[c].red();
			MFloat g = lt[c].green();
			MFloat b = lt[c].blue();

			if( (r*g*b>1) || (r*g*b<0) )
			{
				r = 1.;
				g = 1.;
				b = 1.;
//				MagLog::info()<< "PostScriptDriver-> Cellshading colour not defined in table! Colour index: "<<c<< std::endl;
//    PostScript will always 'overpaint' anything below missing data!!!!
//
			}

			switch ( col_model )
			{
			 case 0:
				 kr = short(r*255.);
				 kg = short(g*255.);
				 kb = short(b*255.);
				 sprintf(t,"%02hx%02hx%02hx",kr,kg,kb);
				 break;
			 case 1:
				 cc = 1.0 - r; cm = 1.0 - g; cy = 1. - b;
				 ck = ( cc < cm ) ?  cc : cm;
				 if ( cy < ck ) ck = cy;
				 if( ck == 1. )
				  { kc = 0; km = 0; ky = 0; kk = 255;}
				 else
				 {
					 kc = short(((cc - ck) / (1.-ck)) * 255.);
					 km = short(((cm - ck) / (1.-ck)) * 255.);
					 ky = short(((cy - ck) / (1.-ck)) * 255.);
					 kk = short(ck* 255.);
				 }
				 sprintf(t,"%02hx%02hx%02hx%02hx",kc,km,ky,kk);
				 break;
			 case 2:
				 if ( (r ==1.) && (g == 1.) && ( b ==1. ))
				  { kr = 255; kg = 255; kb = 255;}
				 else
				  { kr = 0; kg = 0; kb = 0;}
				 sprintf(t,"%02hx%02hx%02hx",kr,kg,kb);
				 break;
			 case 3:
				 ck = 0.3*r + 0.59*g + 0.11*b;
				 kr = short(ck*255.);
				 sprintf(t,"%02hx%02hx%02hx",kr,kr,kr);
				 break;
			}
			*ps << t;
			if ( (++nl)%12 == 0) *ps << "\n";
		}
	}
	*ps << "gr" << std::endl;
	delete [] t;
   }
   else
   {
	MagLog::warning() << "PostScriptDriver: failed to plot CellArray with wrong dimensions! Width: "<<width<<" Height: "<<height << std::endl;
   }
   return true;
}


/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void PostScriptDriver::debugOutput(const string &s) const
{
	if(debug_) PSOut_ << "%% "<<s<<"\n";
}

/*!
  \brief class information are given to the output-stream
*/
void PostScriptDriver::print(ostream& out)  const
{
	out << "PostScriptDriver[";
	out << "]";
}

//! Method to plot symbols
/*!

*/
MAGICS_NO_EXPORT void PostScriptDriver::renderSymbols(const Symbol& symbol) const
{
	setNewColour(symbol.getColour());
	writeColour();
	currentShading_=M_SH_SOLID;
	BaseDriver::renderSymbols(symbol);
}






/*!
	\note The file header can only be written after the Fonts have been read

	\sa open() startPage()
*/
MAGICS_NO_EXPORT void PostScriptDriver::openFile() const
{
	if(!isSplit()) fileName_ = getFileName("ps");
	else
	{
		if(!isEPS()) fileName_ = getFileName("ps" ,currentPage_+1);
		else         fileName_ = getFileName("eps",currentPage_+1);

		//
		// CODE for Cihan because of MetPy expected 'ps' as output file name
		//
		//  if you read this after 01/01/2013 -> PLEASE REMOVE
		//
/*		if(magCompare(fileName_,"ps.ps"))
		{
		  const string s = getEnvVariable("MAGPLUS_PS_SPECIAL_NAME");
		  if (!s.empty()) fileName_ = s;
		}
*/
	}
	if(isPDF())
	{
		const string::size_type pos = fileName_.rfind(".pdf");
		if(pos != string::npos) fileName_.replace(pos,4,".ps");
	}

	if(PSOut_.is_open()) PSOut_.close();
	PSOut_.clear();
	PSOut_.open(fileName_.c_str(),std::ios::out);
	if(!PSOut_){
		MagLog::error() << "PostScriptDriver::close() --> Cannot write PostScript file! " << fileName_ << "\n";
		MagLog::error() << "";  // to ensure that the error message is broadcast
		terminate();
	}

	PSOut_.setf(ios_base::fixed);
	PSOut_.unsetf(ios::showpoint);
	PSOut_.precision(2);
//	PSOut_<< setprecision(2);
	writePSFileHeader();
}

/*!
	\brief Method to close the PostScript output file.

	\sa close() endPage()
*/
MAGICS_NO_EXPORT void PostScriptDriver::closeFile() const
{
	// write end of file
	writePSFileEnd();

	// close + remove files
	PSOut_.close();

	const string fps = fileName_;

	if(isPDF())
	{
		const string::size_type pos = fileName_.rfind(".ps");
		if(pos != string::npos) fileName_.replace(pos,3,".pdf");
		printOutputName("PS pdf "+fileName_);

		// the -q option means no output - warnings may not show up!!! (fonts)
		string cmd = "( gs -q -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite -sOutputFile=";
		cmd.append(fileName_);
		cmd.append(" -c .setpdfwrite -f ");
		cmd.append(fps);
		cmd.append(" )");

		int status = system(cmd.c_str());
		if(status)
		{
			MagLog::error() << "\nPostScriptDriver: Command exit not zero - NO PDF produced!\n"
			             << " COMMAND: "<<cmd<<"\n"<< endl;
			setPS(true);
		}
	}
	if(!isPS() && !isEPS() ) remove(fps.c_str());
	else 
	{
		if(isPS()) printOutputName("PS ps "+fps);
		else printOutputName("PS eps "+fps);
	}
}


/*!
   \brief Method writing the PostScript file header.
*/
MAGICS_NO_EXPORT void PostScriptDriver::writePSFileHeader() const
{
	fstream *ps = getStream();
	const SystemInfo info;

	*ps << "%!PS-Adobe-3.0";
	if(isEPS()) *ps << " EPSF-3.0";
	*ps << "\n%%Title: "<< title_
	    << "\n%%Creator: "<< getMagicsVersionString() <<"\n%%CreationDate: " << info.getTime()
	    << "\n%%For: " << info.getUserID() << "@" << info.getHostName() << " " << info.getUserName()<<"\n";

	MFloat dimensionX = getXDeviceLength() * 72. / 2.54; // 72   = points / inch
	MFloat dimensionY = getYDeviceLength() * 72. / 2.54; // 2.54 = cm / inch

	string orientation = (getXDeviceLength() < getYDeviceLength()) ? "Portrait" : "Landscape";

	MFloat ratio=1.;
	int    width=0;
	string mbg_tmpl = mgb_template_;

	if(!mbg_tmpl.empty())
	{
		setDimensionsFromBinary(mbg_tmpl,ratio,width);
		dimensionX = width;
		dimensionY = maground(width*ratio);
		orientation = (dimensionX < dimensionY) ? "Portrait" : "Landscape";
	}

	if(isEPS())
	{
		*ps << "%%Pages: 1\n%%Orientation: "<<orientation<<"\n"
		    << "%%BoundingBox: 0 0 " << static_cast<int>(dimensionX) << " " << static_cast<int>(dimensionY)+1 << "\n";
	}
	else
	{
		if(old_)
		{
			if(isPDF() && !isPS())
			{
				*ps << "%%Orientation: "<<orientation<<"\n%%LanguageLevel: 2\n%%Pages: 1\n";
			}
			else
			{
				MFloat big, small;
				if(dimensionX>dimensionY) { big = dimensionX;small = dimensionY;}
				else { big = dimensionY;small = dimensionX;}

				*ps << "%%Orientation: "<<orientation<<"\n%%LanguageLevel: 2\n%%Pages: (atend)\n"
				    << "%%BoundingBox: 0 0 " << static_cast<int>(small)+1 << " " << static_cast<int>(big)+1<< "\n";
			}
		}
		else
		{
			*ps << "%%Pages: (atend)\n"
			    << "%%BoundingBox: 0 0 " << static_cast<int>(dimensionX)+1 << " " << static_cast<int>(dimensionY)+1<< "\n";
		}
	}

	*ps << "%%EndComments\n%%BeginProlog\n";
	if(!isEPS()) *ps << "/S { gr showpage } def\n";
	else *ps << "/S {gr} def\n"; // define "showpage" empty for EPS files

	//copyMacro(ps,"PostScriptMacro1.ps");
	*ps << "/m {moveto} def /st {stroke} def /rl {rlineto} def /ro {rotate} def /cp {closepath} def /d { {rmoveto rlineto} repeat stroke} bind def /gr {grestore} def /gs {gsave} def /n { newpath } def\n" 
	    << "/sa {save} def /lw {setlinewidth } def /ar {arc fill} def /arn {arcn fill} def\n"
	    << "/sd {setdash} def /C { setrgbcolor } def /Y { setcmykcolor } def  /B { moveto rlineto stroke } bind def /BB { moveto lineto stroke } bind def /t { translate } def /s {scale} def /K { /UY exch def /UX exch def /LY exch def \n"
	    << "/LX exch def gsave newpath LX LY moveto UX LY lineto UX UY lineto LX UY lineto closepath newpath } def /lp { moveto rlineto } bind def /p { moveto {rlineto} repeat stroke} bind def /po { moveto {rlineto} repeat } bind def\n"
	    << "/q {moveto rlineto stroke} bind def /f {moveto {rlineto} repeat fill} bind def /e {moveto {rlineto} repeat eofill} bind def /F {moveto {rlineto} repeat} bind def /E {eofill} bind def /P { closepath } bind def\n"
	    << "/SAVEMT matrix def\n"
	    << "/Degreevec\n"
	    << "[\n"
	    << "	8#100 /at 8#251 /copyright 8#260 /degree 8#306 /AE 8#301 /Aacute 8#304 /Adieresis 8#300 /Agrave 8#305 /Aring 8#303 /Atilde 8#307 /Ccedilla 8#311 /Eacute 8#312 /Ecircumflex 8#313 /Edieresis 8#310 /Egrave\n"
	    << "	8#320 /Eth 8#315 /Iacute 8#316 /Icircumflex 8#317 /Idieresis 8#314 /Igrave 8#321 /Ntilde 8#323 /Oacute 8#325 /Odieresis 8#322 /Ograve 8#330 /Oslash 8#325 /Otilde 8#336 /Thorn 8#332 /Uacute 8#333 /Ucircumflex\n"
	    << "	8#334 /Udieresis 8#331 /Ugrave 8#335 /Yacute 8#341 /aacute 8#342 /acircumflex 8#222 /acute 8#264 /acute 8#344 /adieresis 8#346 /ae 8#340 /agrave 8#345 /aring 8#343 /atilde\n"
	    << "	8#226 /breve 8#246 /brokenbar 8#237 /caron 8#347 /ccedilla 8#270 /cedilla 8#242 /cent 8#223 /circumflex 8#244 /currency 8#250 /dieresis 8#227 /dotaccent 8#220 /dotlessi\n"
	    << "	8#351 /eacute 8#352 /ecircumflex 8#350 /egrave 8#360 /eth 8#241 /exclamdown 8#337 /germandbls 8#221 /grave 8#253 /guillemotleft 8#273 /guillemotright 8#235 /hungarumlaut\n"
	    << "	8#255 /hyphen 8#355 /iacute 8#356 /icircumflex 8#357 /idieresis 8#354 /igrave 8#254 /logicalnot 8#257 /macron 8#265 /mu\n"
	    << "	8#327 /multiply 8#361 /ntilde 8#363 /oacute 8#364 /odieresis 8#236 /ogonek 8#362 /ograve 8#275 /onehalf 8#274 /onequarter 8#271 /onesuperior 8#252 /ordfeminine\n"
	    << "	8#272 /ordmasculine 8#370 /oslash 8#365 /otilde 8#266 /paragraph 8#267 /periodcentered 8#261 /plusminus 8#277 /questiondown 8#256 /registered 8#232 /ring 8#247 /section 8#243 /sterling\n"
	    << "	8#376 /thorn 8#276 /threequarters 8#263 /threesuperior 8#224 /tilde 8#262 /twosuperior 8#372 /uacute 8#373 /ucircumflex 8#374 /udieresis 8#371 /ugrave 8#375 /yacute 8#377 /ydieresis 8#245 /yen\n"
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

	for(mapit = FontMap_.begin();mapit != FontMap_.end(); mapit++)
		*ps << "Font " << (*mapit).second.id << " eq { /"<< (*mapit).second.ps_name<< " } if\n";

	//copyMacro(ps,"PostScriptMacro2.ps");
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
	    << "	Font  4 lt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { /VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
	    << "	} if\n"
	    << "	Font  8 lt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.76 def } if b 2 eq { /VA -0.725 def } if b 3 eq { /VA -0.3625 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.035 def } if\n"
	    << "	} if\n"
	    << "	Font 12 lt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { /VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
	    << "	} if\n"
	    << "	Font 12 eq { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { /VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
	    << "	} if\n"
	    << "	Font 12 gt { b 0 eq { /VA 0 def } if b 1 eq { /VA -0.7 def } if b 2 eq { /VA -0.6625 def } if b 3 eq { /VA -0.33125 def } if b 4 eq { /VA 0 def } if b 5 eq { /VA 0.0375 def } if\n"
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
	    << "	gs n x1 -2 add HA mul VA Height mul -2 add moveto x1 0 rlineto 0 12 Height add .7 mul rlineto x1 neg 0 rlineto cp 1 setgray fill gr\n"
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

/*!
   \brief Method copying macro code in the PostScript file header.
*/
/*
MAGICS_NO_EXPORT void PostScriptDriver::copyMacro(fstream *ps, const string &file) const
{
	const string s = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + file;
	ifstream psfile(s.c_str());

	if(!psfile){
		MagLog::error() << "PostScriptDriver::copyMacro() --> Cannot open PostScript Macro file! " << s <<
		 " Is MAGPLUS_HOME set correctly?\n";
		return;
	}
	char ch;
	while (psfile.get(ch)){ps->put(ch);}
	psfile.close();
}
*/

MAGICS_NO_EXPORT void PostScriptDriver::writePSFileEnd() const
{
	if(!isEPS())
	{
		fstream *ps = getStream();
		const int realpagenumber = (isSplit()) ? 1 : currentPage_;

		*ps << "%%Trailer\n";
		*ps << "%%Pages: " << realpagenumber << "\n";
		*ps << "%%EOF\n";
		ps->close();
	}
}

MAGICS_NO_EXPORT void PostScriptDriver::setDeviceColourModel(const string &m) const
{
	if(m.empty()) deviceColourModel_ = 1; // use default
	else if(magCompare(m,"RGB"))             deviceColourModel_ = 0;
	else if(magCompare(m,"CMYK"))            deviceColourModel_ = 1;
	else if(magCompare(m,"MONOCHROME"))      deviceColourModel_ = 2;
	else if(magCompare(m,"GRAY"))            deviceColourModel_ = 3;
	else if(magCompare(m,"CMYK_MONOCHROME")) deviceColourModel_ = 4;
	else if(magCompare(m,"CMYK_GRAY"))       deviceColourModel_ = 5;
	else
	{
		MagLog::warning() << "PostScriptDriver::setDeviceColourModel() -> "<< m
		               << " is unknown model! CMYK model is used." << endl;
		deviceColourModel_ = 1;
	}
}

static SimpleObjectMaker<PostScriptDriver, BaseDriver> PostScript_driver("PostScript");
