/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GDDriver.cc
    \brief Implementation of GDDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Oct 29 16:05:47 2007

    Try 'gdlib-config --version --features' and to check your GD installation!

    +----+2  Clipping area
    |    |
   1+----+

   void gdImageSetClip(gdImagePtr im, int  x1,  int  y1,  int  x2,  int  y2)
   void gdImageGetClip(gdImagePtr im, int *x1P, int *y1P, int *x2P, int *y2P)

   void gdImageSetThickness(gdImagePtr im, int thickness)

   Special characters:  &amp; &#x197; (hex) &#197; (dec)

   void gdImageAlphaBlending(gdImagePtr im, int blending)
   void gdImageSaveAlpha(gdImagePtr im, int saveFlag)
        saveFlag: 1:on 0:off  (PNG only) AS EARLY AS POSSIBLE!
*/

#include <GDDriver.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>
#include <Symbol.h>

#include <gdfontl.h>

using namespace magics;

/*!
  \brief Constructor
*/
GDDriver::GDDriver() :currentFont_(""),
                      currentFontSize_(0),currentPageCount_(0),
                      lastPage_(-1),scaleFactor_(1),
                      animated_(false),jpg_(false),gif_(false),png_(false),
                      clipping_(true),offsetX_(0),offsetY_(0)
{
  readFonts();
}

/*!
  \brief Destructor
*/
GDDriver::~GDDriver()
{
  relieveFonts();
}

/*!
  \brief Opening the driver

 The last page needs closing and GIF animation
 needs writing into its output file.

 \sa close()
*/
void GDDriver::open()
{
	currentPage_ = -1;
	//needed for multithread apps!
#ifndef MAGICS_GIF
	if(gdFontCacheSetup()!=0)  MagLog::error() << "GDDriver::readFonts() --> Cannot initialize font cache!\n";
#endif
//	setCMscale(30.);// cm -> pixel

	string mbg_tmpl = mgb_template_;
	MFloat ratio = getYDeviceLength() / getXDeviceLength();
	int width   = width_;
	if(!mbg_tmpl.empty())
	{
		setDimensionsFromBinary(mbg_tmpl,ratio,	width);
	}
	setCMscale(MFloat(width)/getXDeviceLength());
	scaleFactor_      = .8;
	dimensionXglobal_ = width;
	dimensionYglobal_ = static_cast<int>(ratio*width);
}

/*!
  \brief Closing the driver

 The last page needs closing and GIF animation
 needs writing into its output file. The animation
 is started in startPage().

 \sa endPage open()
*/
void GDDriver::close()
{
	if(animated_)
	{
#ifndef MAGICS_GIF
#ifndef MAGICS_GIF_ANIMATED
		MagLog::warning() << "GIF and Animated GIF are not supported in this version (Too old version of GD library!)";
		animated_ = false;
		png_ = true;
#else
		gdImageGifAnimEnd(outFile_);
		fclose(outFile_);
		outFile_ = 0;
#endif
#endif
	}
	if( jpg_ ) MagLog::info() << "JEPG is not a good format for scientific plots. Please consider using GIF or PNG.\n";

	currentPage_ = 0;
	currentPageCount_ = 0;

	const unsigned int isize = Images_.size();
	for(unsigned int i=0;i<isize;i++)
	{
		gdImageDestroy(Images_[i]);
	}
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.

 A new page in GD means a normally a new file.
 An MagException are GIF animation.

 In this method the binary output file is opened.
 This includes the formation of the file name.

 \sa endPage
*/
MAGICS_NO_EXPORT void GDDriver::startPage() const
{
	if(currentPage_==-1)
	{
		dimensionX_ = static_cast<float>(dimensionXglobal_-1);
		dimensionY_ = static_cast<float>(dimensionYglobal_-1);
		offsetY_ = dimensionY_;
		coordRatioY_ = -1;
	}

	currentPageCount_++;
	currentPage_++;
	newPage_ = true;

	int cPage = currentPage_;

	if(!animated_)
	{
		Images_.clear();
		currentPage_ = 0;
	}

	// No TrueColor otherwise transparent background does not work
	currentImage_ = gdImageCreateTrueColor(dimensionXglobal_,dimensionYglobal_);
	if(png_)
	{
		gdImageAlphaBlending(currentImage_, 0);
		gdImageSaveAlpha(currentImage_, 1); // save transparency
	}

	Images_.push_back(currentImage_);

#ifdef MAGICS_GIF_ANIMATED
	if(animated_ && cPage==0)
	{
		string fileName = getFileName("gif",0);
		outFile_ = fopen(fileName.c_str(),"wb");
		printOutputName(fileName);

		gdImageGifAnimBegin(currentImage_,outFile_,1/*global colormap*/,0/*infinite loop*/);
	}
#endif

	int white = gdImageColorAllocate(currentImage_, 255, 255, 255);
	if(white == -1) MagLog::error() << "GDDriver: Could NOT allocate background colour!\n";

	gdImageFilledRectangle(currentImage_,0,0,dimensionXglobal_,dimensionYglobal_,white);
	if(png_ && transparent_)
	{
		gdImageColorTransparent(currentImage_, white);
		white = gdImageColorAllocateAlpha(currentImage_, 255, 255, 255,127);
		gdImageFilledRectangle(currentImage_,0,0,dimensionXglobal_,dimensionYglobal_,white);
	}
	setNewColour(Colour(0,0,0)); // set colour to black as default
}

/*!
  \brief ending a page

  This method has to take care that for formats with multiple output
  files are closed.

 A page in GD means normally a output file.
 An MagException are GIF animation for which are
 closed in close().

 In this method the binary output file is opened.
 This includes the formation of the file name.

 \sa startPage() close()
*/
MAGICS_NO_EXPORT void GDDriver::endPage() const
{
#ifdef MAGICS_GIF
#ifdef MAGICS_GIF_ANIMATED
	if(animated_)
	{
		if(currentPage_==0) gdImageGifAnimAdd(currentImage_,outFile_,1,0,0,delay_,1,NULL);
		else                gdImageGifAnimAdd(currentImage_,outFile_,1,0,0,delay_,1,Images_[currentPage_-1]);
	}
	else
#endif
#endif
	{
		string fileName;
		// write image to file in PNG or JPEG - close and destoy
		if( gif_ )
		{
#ifndef MAGICS_GIF
			MagLog::warning() << "GIF and Animated GIF are not supported in this version (Too old version of GD library!). You get a PNG instead.";
			gif_ = false;
			png_ = true;
#else
			fileName = getFileName("gif",currentPageCount_);
			outFile_ = fopen(fileName.c_str(),"wb");
			if(outFile_)
			{
				gdImageGif(currentImage_,outFile_);
				fclose(outFile_);
				printOutputName(fileName);
			}
			else
				MagLog::error() << "GIF: cannot open file "<< fileName <<"!"<< endl;
#endif
		}

		if( png_ )
		{
			fileName = getFileName("png",currentPageCount_);
			outFile_ = fopen(fileName.c_str(),"wb");
			if(outFile_)
			{
				gdImagePng(currentImage_,outFile_);
				fclose(outFile_);
				printOutputName(fileName);
			}
			else
				MagLog::error() << "PNG: cannot open file "<< fileName <<"!"<< endl;
		}

		if( jpg_ )
		{
			fileName = getFileName("jpg",currentPageCount_);
			outFile_ = fopen(fileName.c_str(),"wb");
			if(outFile_)
			{
				gdImageJpeg(currentImage_,outFile_, quality_); // quality = -1 is default
				fclose(outFile_);
				printOutputName(fileName);
			}
			else
				MagLog::error() << "JPEG: cannot open file "<< fileName <<"!"<< endl;
		}
		outFile_ = 0;
	}

	newPage_ = false;
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void GDDriver::project(const magics::Layout& layout) const
{
	if(newPage_) newPage_=false;

	// push current state
	dimensionStack_.push(dimensionX_);
	dimensionStack_.push(dimensionY_);
	offsetsX_.push(offsetX_);
	offsetsY_.push(offsetY_);
	scalesX_.push(coordRatioX_);
	scalesY_.push(coordRatioY_);

	offsetX_    += layout.x()     * 0.01 * dimensionX_;
	offsetY_    -= layout.y()     * 0.01 * dimensionY_;
	dimensionX_ =  layout.width() * 0.01 * dimensionX_;
	dimensionY_ =  layout.height()* 0.01 * dimensionY_;

	const float sumX = layout.maxX() - layout.minX();
	const float sumY = layout.maxY() - layout.minY();

	if( sumX!=0 && sumY!=0 )
	{
		coordRatioX_ = dimensionX_/sumX;
		coordRatioY_ = -dimensionY_/sumY;
	}

	offsetX_ = projectX( -layout.minX());
	offsetY_ = projectY( -layout.minY());
}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

  \sa UnLayout
*/
MAGICS_NO_EXPORT void GDDriver::unproject() const
{
	dimensionY_ = dimensionStack_.top();dimensionStack_.pop();
	dimensionX_ = dimensionStack_.top();dimensionStack_.pop();
	offsetX_ = offsetsX_.top();offsetsX_.pop();
	offsetY_ = offsetsY_.top();offsetsY_.pop();
	coordRatioX_  = scalesX_.top(); scalesX_.pop();
	coordRatioY_  = scalesY_.top(); scalesY_.pop();
}


/*!
  \brief sets a new colour

  \sa Colour
*/
MAGICS_NO_EXPORT void GDDriver::setNewColour(const Colour &colour) const
{
	if(currentColour_ == colour) return;

	currentColour_ = colour;

	const int r = static_cast<int>(colour.red()*255.);
	const int g = static_cast<int>(colour.green()*255.);
	const int b = static_cast<int>(colour.blue()*255.);
	const int a = 127-static_cast<int>(colour.alpha()*127.);

	currentColourIndex_ = gdImageColorResolveAlpha(currentImage_,r,g,b,a);
	gdImageSetAntiAliased(currentImage_,currentColourIndex_);
	currentLineStyleIndex_ = gdAntiAliased;
}


/*!
  \brief sets new properties of how lines are drawn

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT int GDDriver::setLineParameters(const LineStyle linestyle, const MFloat w) const
{
//	int width = 1;
//	if      (w > 3.) width=static_cast<int>(w*.25);
//	else
	int width=w*.25;
	if(width<1) width=1;

	if(width==1 && linestyle==M_SOLID) return gdAntiAliased;

	const int col = gdAntiAliased;
	gdImageSetThickness(currentImage_,width);

	int *style;
	int stylelength;
	int k=0;

	switch(linestyle)
	{
		case M_SOLID:
			stylelength = 2*width;
			style = new int[stylelength];
			for(;k<2*width;k++) style[k] = col;
			break;
		case M_DASH: // 6 on - 2 off
			stylelength = 20*width;
			style = new int[stylelength];
			for(;k<10*width;k++) style[k] = gdTransparent;
			for(;k<20*width;k++) style[k] = col;
			break;
		case M_DOT: // 1 on - 2 off
			stylelength = 8*width;
			style = new int[stylelength];
			for(;k<3*width;k++) style[k] = col;
			for(;k<8*width;k++) style[k] = gdTransparent;
			break;
		case M_CHAIN_DASH: // 4 on - 2 off -  1 on - 2 off
			stylelength = 9*width;
			style = new int[stylelength];
			for(;k<4*width;k++) style[k] = col;
			for(;k<6*width;k++) style[k] = gdTransparent;
			for(;k<7*width;k++) style[k] = col;
			for(;k<9*width;k++) style[k] = gdTransparent;
			break;
		case M_CHAIN_DOT: // 4 on - 2 off -  1 on - 2 off - 1 on - 2 off
			stylelength = 12*width;
			style = new int[stylelength];
			for(;k<4*width;k++)  style[k] = col;
			for(;k<6*width;k++)  style[k] = gdTransparent;
			for(;k<7*width;k++)  style[k] = col;
			for(;k<9*width;k++)  style[k] = gdTransparent;
			for(;k<10*width;k++) style[k] = col;
			for(;k<12*width;k++) style[k] = gdTransparent;
			break;
		default:  // SOLID
			stylelength = 2*width;
			style = new int[stylelength];
			for(;k<2*width;k++) style[k] = col;
			break;
	}
	gdImageSetStyle(currentImage_,style,stylelength);
	currentLineStyleIndex_ = gdStyled;
	if(style) delete [] style;

	return gdStyledBrushed;
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
MAGICS_NO_EXPORT void GDDriver::renderPolyline(const int n, MFloat *x, MFloat *y) const
{
	if(n<2) return;

	MFloat old_x = projectX(x[0]);
	MFloat old_y = projectY(y[0]);

	for(int l = 1; l<n; l++)
	{
		const MFloat xx = projectX(x[l]);
		const MFloat yy = projectY(y[l]);

		gdImageLine(currentImage_,(int)old_x,(int)old_y,(int)xx,(int)yy,currentLineStyleIndex_);

		old_x = xx;
		old_y = yy;
	}
	// clean up
	setNewColour(Colour(0,0,0)); // set colour to black as default
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
MAGICS_NO_EXPORT void GDDriver::renderPolyline2(const int n, MFloat* x, MFloat* y) const
{
	if(n != 2) return;

	MFloat *xx = x;
	MFloat *yy = y;

	const float ix0 = (float)*xx;
	xx++;
	const float iy0 = (float)*yy;
	yy++;
	const float dx0 = (float)*xx;
	const float dy0 = (float)*yy;
	gdImageLine(currentImage_,ix0,iy0,dx0,dy0,currentLineStyleIndex_);
}

/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param nn number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void GDDriver::renderSimplePolygon(const int nn, MFloat* x, MFloat* y) const
{
	int n = nn;
	if ( (x[n-1] == x[0]) && (y[n-1] == y[0]) )  n--;

	MFloat *xx = x;
	MFloat *yy = y;

	gdPoint *xyP = new gdPoint[n];
	gdPoint *xy = xyP;

	for(int i = 0; i<n;xy++,i++)
	{
		xy->x = projectX(*(xx++));
		xy->y = projectY(*(yy++));
	}

	currentLineStyle_ = currentLineStyleIndex_;
	gdImageFilledPolygon(currentImage_,xyP,n,currentLineStyleIndex_);
	delete [] xyP;
}


/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param line polyline to be filled
*/
MAGICS_NO_EXPORT void GDDriver::renderSimplePolygon(const Polyline& line) const
{
	const unsigned int n = line.size();
	if(n<3 || (currentColour_==Colour("none")) ) return;
	line.getShading()->draw(*this);
	setNewColour(line.getFillColour());

	gdPoint *xy = new gdPoint[n];
	for(unsigned int l = 0; l<n; l++)
	{
		const PaperPoint& pp = line.get(l);
		xy[l].x = projectX(pp.x());
		xy[l].y = projectY(pp.y());
	}
	currentLineStyle_ = currentLineStyleIndex_;
	gdImageFilledPolygon(currentImage_,xy,n,currentLineStyleIndex_);
	delete [] xy;
}



/*!
  \brief renders text strings

  This method renders given text strings.

 GD offers two ways of plotting text: using own line fonts or using TTF fonts.

 By default line fonts are used if MAGICS_TTF is not set (see configure & config.h).

 \note Be careful: TTF text ploting seems to fail in multithread context (AIX Jan2006)
  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void GDDriver::renderText(const Text& text) const
{
#ifdef MAGICS_TTF
	if(text.empty()) return;
	const string MAGPLUS_HOME=getEnvVariable("MAGPLUS_HOME");

	const vector<NiceText>& niceT = text.getNiceText();
	if(niceT.empty() && (text.empty()) ) return;

	const float PNGASCENT = 0.12;
	const float PGNDESENT = 0.2;

	float H = 0.;
	float W = 0.;

	const float LastAngle_ = -text.getAngle();
	const float cs = cos(LastAngle_);
	const float ss = sin(LastAngle_);

	const enum Justification horizontal = text.getJustification();
	const enum VerticalAlign vertical   = text.getVerticalAlign();

	MFloat HA = 0.1; // MLEFT default
	if (horizontal==MCENTRE)     HA = -0.5;
	else if (horizontal==MRIGHT) HA = -1.;

	MFloat VA = 0.;
	if (vertical==MBASE) VA = + PGNDESENT;
	else if (vertical==MTOP)    VA = 1. + PNGASCENT + PGNDESENT;
	else if (vertical==MHALF)   VA = 0.5 + PGNDESENT;
	else if (vertical==MBOTTOM) VA = -0.1;

	string textString;

	vector<NiceText>::const_iterator niceText = text.textBegin();
	vector<NiceText>::const_iterator niceTextEnd = text.textEnd();

	int text_length = 0;

	for(;niceText<niceTextEnd;niceText++)
	{
		const MagFont magfont	= (*niceText).font();
		const std::set<string>& styles = magfont.styles();
		string style = "";
		if(styles.find("bold") != styles.end()) style = "bold";
		if(styles.find("italic") != styles.end()) style += "italic";
		if(style == "") style = "normal";
		const string lowFont = lowerCase(magfont.name()+"_"+style);
		fontMapIter iter = FontMap_.find(lowFont);

		string ttf = MAGPLUS_HOME + "/" + MAGICS_TTF_PATH;
		if(iter!=FontMap_.end())
			ttf += iter->second.ttf_filename;
		else
		{
			ttf += FontMap_["sansserif_normal"].ttf_filename; // if not found get default
			MagLog::warning() << "GDDriver: Font "<< lowFont << " is not registered! Default font is used."<< endl;
		}

		// test if font files exists
		FILE* fontfile = fopen(ttf.c_str(),"r");
		if(!fontfile)
		{
			const string new_font = lowerCase( "sansserif_"+style);
			MagLog::warning() << "GDDriver (pretty): TTF font file "<< ttf <<" could not be found! Font "<< new_font<< " is used instead!"<< endl;
			fontMapIter iter = FontMap_.find(new_font);

			ttf = MAGPLUS_HOME + "/" + MAGICS_TTF_PATH;
			if(iter!=FontMap_.end())
				ttf += iter->second.ttf_filename;
		}
		else fclose(fontfile);

		const float fontSize = scaleFactor_ * convertCM(magfont.size());

		int bbx[8];
		char *err = gdImageStringFT(NULL,&bbx[0],0,(char *)ttf.c_str(),fontSize,0.,10,10,(char*)(*niceText).text().c_str());
		if(err) MagLog::error() << "GDDriver::renderText() -> Could not determine text bounding box for >"<<(*niceText).text()<<"< !\n";

		text_length += (bbx[2] - bbx[0]);
	 }

	 text_length *= HA;
	 offsetsX_.push(offsetX_);
	 offsetX_ = offsetX_+text_length;

	 int offset = 0;
	 niceText = text.textBegin();
	 for(;niceText<niceTextEnd;niceText++)
	 {
		textString = (*niceText).text();
//		const int length =textString.length()+1;

		const MagFont magfont = (*niceText).font();
		const std::set<string>& styles = magfont.styles();
		string style = "";
		if(styles.find("bold") != styles.end()) style = "bold";
		if(styles.find("italic") != styles.end()) style += "italic";
		if(style == "") style = "normal";
		const string lowFont = lowerCase(magfont.name()+"_"+style);
		fontMapIter iter = FontMap_.find(lowFont);
		const float fontSize = scaleFactor_ * convertCM(magfont.size());

		string ttf = MAGPLUS_HOME + "/" + MAGICS_TTF_PATH;
		if(iter!=FontMap_.end())
			ttf += iter->second.ttf_filename;
		else
		{
			ttf += FontMap_["sansserif_normal"].ttf_filename; // if not found get default
			MagLog::warning() << "GDDriver: Font "<< lowFont << " is not registered! Default font is used."<< endl;
		}

		// test if font files exists
		if(currentFont_!=ttf)
		{
		  FILE* fontfile = fopen(ttf.c_str(),"r");
		  if(!fontfile)
		  {
			const string new_font = lowerCase( "sansserif_"+style);
			MagLog::warning() << "GDDriver (pretty): TTF font file "<< ttf <<" could not be found! Font "<< new_font<< " is used instead!"<< endl;
			fontMapIter iter = FontMap_.find(new_font);

			ttf = MAGPLUS_HOME + "/" + MAGICS_TTF_PATH;
			if(iter!=FontMap_.end())
				ttf += iter->second.ttf_filename;
		  }
		  else
		  {
	  		fclose(fontfile);
			currentFont_=ttf;
		  }
		}

		// get bbx to size text
		int bbx[8];
		char *err = gdImageStringFT(NULL,&bbx[0],0,(char *)ttf.c_str(),fontSize,0.,10,10,(char*)(*niceText).text().c_str());

		if (err)
		{
			MagLog::error() << "GDDriver: gdImageStringFT (prettyText) -> "<< err <<" in "<<ttf<< endl;
			return;
		}
		else
		{
			//W = text_length;
			W = (bbx[2] - bbx[0]);
			H = (bbx[1] - bbx[7]);
		}

		const float E1x = 0;
		const float E1y = H*VA;
		const float E1xcsE1yss =  E1x*cs + E1y*ss + offset;
		const float E1xssE1ycs = -E1x*ss + E1y*cs;
		offset += W;

//		const unsigned int tsize = (*niceText).text().size();
		setNewColour(magfont.colour());

	  unsigned int noTexts = text.size();
	  for(unsigned int nT=0;nT<noTexts;nT++)  // for all sting COORDINATES
	  {
		const int x0 = static_cast<int>( E1xcsE1yss + projectX(text[nT].x()));//lower left
		const int y0 = static_cast<int>( E1xssE1ycs + projectY(text[nT].y()));

		if(text.getBlanking())
		{
			err = gdImageStringFT(NULL,&bbx[0],0,(char *)ttf.c_str(),fontSize,LastAngle_,x0,y0,(char *)(*niceText).text().c_str());
			if(!err)
			{
				const int white = gdImageColorAllocate(currentImage_, 255, 255, 255);
				gdPoint points[4];
				points[0].x = bbx[0];  points[0].y = bbx[1];
				points[1].x = bbx[2];  points[1].y = bbx[3];
				points[2].x = bbx[4];  points[2].y = bbx[5];
				points[3].x = bbx[6];  points[3].y = bbx[7];
				gdImageFilledPolygon(currentImage_, points, 4, white);
			}
			else MagLog::error() << "GDDriver: gdImageStringFT -> "<< err <<" -> NO blanking!!!"<< endl;
		}
		err = gdImageStringFT(currentImage_,&bbx[0],currentColourIndex_,(char *)ttf.c_str(),fontSize,LastAngle_,x0,y0,(char *)(*niceText).text().c_str());
		if (err)
		{
			MagLog::error() << "GDDriver: gdImageStringFT -> "<< err << endl;
			gdImageString(currentImage_,gdFontGetLarge(),x0,y0,(unsigned char*)(*niceText).text().c_str(),currentColourIndex_);
		}
		delete [] err;
	  }
	}// endfor all text segments
	offsetX_ = offsetsX_.top();offsetsX_.pop();
#else
	MagLog::warning() << "GDDriver: No TTF fonts support in Magics++!" << endl;
	return;
#endif
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
MAGICS_NO_EXPORT void GDDriver::circle(const MFloat x, const MFloat y, const MFloat r, const int s) const
{
	const int rx = static_cast<int>(r*2.);
	const int ry = static_cast<int>(r*2.);
	const int cx = static_cast<int>(projectX(x));
	const int cy = static_cast<int>(projectY(y));

	if(s < 8)
	{
		gdImageArc(currentImage_,cx,cy,rx,ry,0,360,currentLineStyleIndex_);
		if(s > 0)
			gdImageFilledArc(currentImage_,cx,cy,rx,ry,270,270+(s*45),currentLineStyleIndex_, gdArc);
	}
	else gdImageFilledArc(currentImage_,cx,cy,rx,ry,0,360,currentLineStyleIndex_, gdArc);

	if(s == 9)
	{
		const int col = gdImageColorResolveAlpha(currentImage_,255,255,255,0);
		gdImageLine(this->currentImage_,cx,static_cast<int>(cy+r-1),cx,static_cast<int>(cy-r+1),col);
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
  \param alpha says if array is transparent

*/
MAGICS_NO_EXPORT bool GDDriver::renderPixmap(MFloat x0,MFloat y0,MFloat x1,MFloat y1,
                                            int w,int h,unsigned char* pixmap,int, bool alpha) const
{
	unsigned char *p = pixmap;
	const float dx =  (x1 - x0)/w;
	const float dy =  (y1 - y0)/h;

	const float X0 = x0;
	const float Y0 = y0;

	for(int i=h-1;i>=0;i--)
	{
		for(int j=0;j<w; x0+=dx,j++)
		{
			int col = 0;
			const int r = static_cast<int>( *(p++));
			const int g = static_cast<int>( *(p++));
			const int b = static_cast<int>( *(p++));
			if(alpha)
			{
				const int a = *(p++);
				col = gdImageColorResolveAlpha(this->currentImage_,r,g,b,a);
			}
			else
				col = gdImageColorResolveAlpha(this->currentImage_,r,g,b,0);

			const int x0 = static_cast<int>( X0+(j*dx) );
			const int y0 = static_cast<int>( Y0+(i*dy) );
			const int x1 = static_cast<int>( x0+dx);
			const int y1 = static_cast<int>( y0+dy);

			gdImageFilledRectangle(this->currentImage_,x0,y0,x1,y1,col);
		}
		x0 = X0;
		y0 += dy;
	}
	return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool GDDriver::renderCellArray(const Image& image) const
{
	ColourTable &lt = image.getColourTable();
	const int width  = image.getNumberOfColumns();
	const int height = image.getNumberOfRows();
	const float x0 = projectX(image.getOrigin().x());
	const float y0 = projectY(image.getOrigin().y());
	const float x1 = projectX(image.getOrigin().x()+image.getWidth());
	const float y1 = projectY(image.getOrigin().y()+image.getHeight());
	const float dx = (x1-x0)/width;
	const float dy = -(y1-y0)/height;

	for (int i=height-1;i>=0;i--)
	{
		for(int j=0;j<width; j++)
		{
		  const int in = width*i+j;
		  const short c = image[in];

 		  if(!(lt[c]=="undefined"))
		  {
			const int r = static_cast<int>(lt[c].red()*255.);
			const int g = static_cast<int>(lt[c].green()*255.);
			const int b = static_cast<int>(lt[c].blue()*255.);
			const int a = 127-static_cast<int>(lt[c].blue()*127.);

			int kk = gdImageColorResolveAlpha(this->currentImage_,r,g,b,a);
			const int wx = static_cast<int>( x0+(j*dx) );
			const int wy = static_cast<int>( y0+(i*dy) );
			gdImageFilledRectangle(this->currentImage_,wx,wy,static_cast<int>(wx+dx),static_cast<int>(wy+dy),kk);
		  }// point has colour
		}
	}
	return true;
}


/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void GDDriver::debugOutput(const string &s) const
{
	MagLog::debug() << s << endl;
}

/*!
  \brief class information are given to the output-stream
*/
void GDDriver::print(ostream& out)  const
{
	out << "GDDriver[";
	out << "]";
}

//! Method to plot logo
/*!
 MagLogo needs special treatment - much better quality when imported as GIF!
 \sa convertToPixmap1()
*/
MAGICS_NO_EXPORT void GDDriver::renderMagLogo(MFloat x, MFloat y) const
{
	GraphicsFormat format = PNG;
	const string logofile = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "ecmwf_logo.png";
	convertToPixmap(logofile, format, 300, x-40, y+10, x+50, y-5);
}

//! Method to plot symbols
/*!
 Needs special treatment of MagLogo. Much better quality when imported as GIF!
*/
MAGICS_NO_EXPORT void GDDriver::renderSymbols(const Symbol& symbol) const
{
	debugOutput("Start GDDriver Symbols");

	if(symbol.getSymbol()=="logo_ecmwf")
		renderMagLogo(projectX(symbol[0].x()),projectY(symbol[0].y()));
	else
		BaseDriver::renderSymbols(symbol);
}

static SimpleObjectMaker<GDDriver, BaseDriver> GD_driver("GD");
