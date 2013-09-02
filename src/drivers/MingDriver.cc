

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

/*! \file MingDriver.cc
    \brief Implementation of MingDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue Nov 20 12:26:52 2007

*/

#include <MingDriver.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>

#define SCALE 35

using namespace magics;

/*!
  \brief Constructor
*/
MingDriver::MingDriver() 
{
	setNewColour(Colour("black"));
}

/*!
  \brief Destructor
*/
MingDriver::~MingDriver() 
{
}

/*!
  \brief Opening the driver
*/
void MingDriver::open()
{
MagLog::warning() << " MingDriver is still EXPERIMENTAL !!!"<< endl;
	dimensionX_ = SCALE * getXDeviceLength();
	dimensionY_ = SCALE * getYDeviceLength();
	movie_ = new SWFMovie(9);  // version 9
	movie_->setRate(0.5);
	movie_->setDimension(dimensionX_,dimensionY_);
	offsetY_ = dimensionY_;
	coordRatioY_ = -1;
}

/*!
  \brief Closing the driver
*/
void MingDriver::close()
{
	endPage();
	currentPage_ = 0;

	string fileName = getFileName("swf",0);
	movie_->save(fileName.c_str());
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.
*/
MAGICS_NO_EXPORT void MingDriver::startPage() const
{
	if(currentPage_ > 0) endPage();

	movie_->nextFrame();

	currentPage_++;
	newPage_ = true;
}

/*!
  \brief ending a page
 
  This method has to take care that for formats with multiple output 
  files are closed.
*/
MAGICS_NO_EXPORT void MingDriver::endPage() const
{
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void MingDriver::project(const magics::Layout& layout) const
{
	if( (!magCompare(currentBox_,"non")) ) unprojectBox();

	newLayout_ = true;
	if(newPage_) newPage_=false;

	dimensionStack_.push(dimensionX_);
	dimensionStack_.push(dimensionY_);

	lastAreaHeightPercentage_ = layout.getAreaHeightPercentage() * 0.01;
	lastAreaWidthPercentage_  = layout.getAreaWidthPercentage()  * 0.01;

	const float Xoff = layout.getXOffsetPercentage() * 0.01 * dimensionX_;
	const float Yoff = layout.getYOffsetPercentage() * 0.01 * dimensionY_;

	offsetsX_.push(offsetX_);
	offsetsY_.push(offsetY_);
	offsetX_ = projectX(Xoff);
	offsetY_ = projectY(Yoff);
	dimensionX_= lastAreaWidthPercentage_  * dimensionX_;
	dimensionY_= lastAreaHeightPercentage_ * dimensionY_;
}

/*!
  \brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

  \sa UnLayout
*/
MAGICS_NO_EXPORT void MingDriver::unproject(const magics::UnLayout& unlayout) const
{
	if( (newPage_==false) && (!magCompare(currentBox_,"non")) ) {unprojectBox();}
	if(dimensionStack_.size()>1)
	{
		dimensionY_ = dimensionStack_.top();dimensionStack_.pop();
		dimensionX_ = dimensionStack_.top();dimensionStack_.pop();

		offsetX_ = offsetsX_.top();offsetsX_.pop();
		offsetY_ = offsetsY_.top();offsetsY_.pop();
	}
	currentBox_ = "non";
}

/*!
  \brief project into a box

  This method will update the offset and scale to position for new box.
  The properties of boxes are stored in the current Layout.

  \sa Layout
*/
MAGICS_NO_EXPORT void MingDriver::projectBox(const string& area) const
{
	if(magCompare(currentBox_,area)) return;
	if(newLayout_) newLayout_=false;
	else { unprojectBox();}

	currentBox_ = area;
	setNewColour(Colour(0.,0.,0.));  // to set colours right!

	Box *box = getBox(area);

	const float Xoff    = dimensionX_ * box->getX()      * 0.01;
	const float Yoff    = dimensionY_ * box->getY()      * 0.01;
	const float Xlength = dimensionX_ * box->getWidth()  * 0.01;
	const float Ylength = dimensionY_ * box->getHeight() * 0.01;
	const float Xmin = box->getXmin();
	const float Xmax = box->getXmax();
	const float Ymin = box->getYmin();
	const float Ymax = box->getYmax();
	const float sumX = Xmax - Xmin;
	const float sumY = Ymax - Ymin;

	boxoffsetsX_.push(offsetX_);
	boxoffsetsY_.push(offsetY_);
	offsetX_ += Xoff;
	offsetY_ -= Yoff;
	scalesX_.push(coordRatioX_);
	scalesY_.push(coordRatioY_);
	coordRatioX_ *= Xlength/sumX;
	coordRatioY_ *= Ylength/sumY;
	offsetX_ = projectX(-Xmin);
	offsetY_ = projectY(-Ymin);
}

/*!
  \brief unproject into a box

  This method will update the offset and scale to rest from the current box.
  The properties of boxes are stored in the current Layout.

  \sa Layout
*/
MAGICS_NO_EXPORT void MingDriver::unprojectBox() const
{
	if((!magCompare(currentBox_,"non")) && ( !offsetsX_.empty() && !scalesX_.empty() ) )
	{
		offsetX_ = boxoffsetsX_.top();
		boxoffsetsX_.pop();
		offsetY_ = boxoffsetsY_.top();
		boxoffsetsY_.pop();

		coordRatioX_  = scalesX_.top();
		scalesX_.pop();
		coordRatioY_  = scalesY_.top();
		scalesY_.pop();
	}
	currentBox_ = "non";
}

/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa PhysicalLayer
*/
MAGICS_NO_EXPORT void MingDriver::newLayer(const PhysicalLayer& layer) const
{
	movie_->nextFrame();
}

/*!
  \brief close the current layer

  This method will close an existing layer. This includes resets of existing boxes. 

  \sa UnPhysicalLayer PhysicalLayer
*/
MAGICS_NO_EXPORT void MingDriver::closeLayer(const UnPhysicalLayer& layer) const
{
}


/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the 
  current box is finished.

  \sa Colour
*/
MAGICS_NO_EXPORT void MingDriver::setNewColour(const Colour &colour) const
{
	if(currentColour_ == colour) return;
	currentColour_ = colour;
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the 
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void MingDriver::setNewLineWidth(const float width) const
{
	currentLineWidth_ = width;
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the 
  current box is finished.

  \sa LineStyle

  \param linestyle Object describing the line style
  \param w width of the line

*/
MAGICS_NO_EXPORT int MingDriver::setLineParameters(const LineStyle linestyle, const float w) const
{
	setNewLineWidth(w);

	MagLog::debug() << "MingDriver::setLineParameters needs implementing." <<endl;
	return 0;
}

/*!
  \brief renders polylines

  This method renders a polyline given as two float arrays. The two 
  arrays given as X and Y values have to be at least the length of
  <i>n</i>. All values beyond <i>n</i> will be ignored. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y array of y values
*/
MAGICS_NO_EXPORT void MingDriver::renderPolyline(const int n, float *x, float *y) const
{
	if(n<2) return;

	SWFShape *shape = new SWFShape();

	shape->setLine2(currentLineWidth_, (byte)currentColour_.red()*128,(byte)currentColour_.green()*128,(byte)currentColour_.blue()*128, (byte)128, 
	                 SWF_LINESTYLE_FLAG_HINTING | SWF_LINESTYLE_JOIN_BEVEL | SWF_LINESTYLE_FLAG_ENDCAP_SQUARE, 0);

	float old_x = projectX(x[0]);
	float old_y = projectY(y[0]);
	shape->movePenTo(old_x, old_y);

	for(int l = 1; l<n; l++)
	{
		const float xx = projectX(x[l]);
		const float yy = projectY(y[l]);
		shape->drawLineTo(xx, yy);

		old_x = xx;
		old_y = yy;
	}

	movie_->add(shape);
}

/*!
  \brief renders horizontal polylines

  This method renders a horizontal polyline given as two float arrays.
  This method is mainly thought for the scanline algorithm used by some 
  output drivers to shade areas. <i>n</i> has to be a multiple of 2!

  \sa setLineParameters()
  \param n number of points
  \param x array of x values
  \param y y value
*/
MAGICS_NO_EXPORT void MingDriver::renderPolyline2x(const int n, int* x, const int y) const
{
	MagLog::debug() << "MingDriver::renderPolyline2x needs implementing." <<endl;
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
MAGICS_NO_EXPORT void MingDriver::renderPolyline2(const int n, float* x, float* y) const
{
	MagLog::debug() << "MingDriver::renderPolyline2 needs implementing." <<endl;
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
MAGICS_NO_EXPORT void MingDriver::renderSimplePolygon(const int n, float* x, float* y) const
{
	MagLog::debug() << "MingDriver::renderSimplePolygon needs implementing." <<endl;
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \note As of version 2.0 there are two forms of describing text in Text.

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void MingDriver::renderText(const Text& text) const
{
   if(text.empty()) return;
   const vector<NiceText>& niceT = text.getNiceText();
   if(niceT.empty() && (text.getText().length()==0 || text.empty()) ) return;

   if(niceT.empty())
   {
   }
   /**************************************************************************
   ***
   ***    N I C E   T E X T
   ***
   **************************************************************************/
   else
   {
	vector<NiceText>::const_iterator niceText = text.textBegin();
	vector<NiceText>::const_iterator niceTextEnd = text.textEnd();

	int text_length = 0;
	for(;niceText<niceTextEnd;niceText++)
	{
		const MagFont magfont	= (*niceText).font();
		const string style	= (magfont.style()=="") ? "normal": magfont.style();
//		const bool underlined	= (magfont.style()=="underline") ? true : false; 
		const string font	= magfont.name()+"_"+style;
		const string lowFont	= lowerCase(font);
		fontMapIter iter	= FontMap_.find(lowFont);

		string ttf = getEnvVariable("MAGPLUS_HOME") + "/" + MAGICS_TTF_PATH;
		if(iter!=FontMap_.end())
			ttf += iter->second.ttf_filename;
		else
		{
			ttf += FontMap_["sansserif_normal"].ttf_filename; // if not found get default
			MagLog::warning() << "MingDriver: Font "<< font << " is not registered! Default font is used."<< endl;
		}

		SWFText *stext = new SWFText();
		SWFFont *sfont = new SWFFont((char*)ttf.c_str());
		if(sfont==NULL)
		{
		  MagLog::error() << "MingDriver -> Could not open TTF file "<<ttf<<" !"<< endl;
		}
		stext->moveTo( projectX(text[0].x()), projectY(text[0].y()));
		stext->setFont(sfont);
//		stext->setColor(0xff, 0, 0, 0xff);
		stext->setColor(currentColour_.red(),currentColour_.green(),currentColour_.blue(),0xff);
		stext->setHeight(text.getFontSize()*SCALE);
		stext->addString((*niceText).text().c_str(), NULL);//text.getText());
		movie_->add(stext);
	}
   } // end NICE text
/*
 $i->moveTo(160-$t->getWidth("fnar! fnar!")/2, 120+$t->getAscent()/2);

	SWFTextField *text = new SWFTextField();

	text->setFlags(SWFTEXTFIELD_NOEDIT);
	text->addString("The quick brown fox jumps over the lazy dog. 1234567890");
	m->add(text);*/
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
MAGICS_NO_EXPORT void MingDriver::circle(const float x, const float y, const float r, const int s) const
{
	MagLog::debug() << "MingDriver::circle needs implementing." <<endl;
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
MAGICS_NO_EXPORT bool MingDriver::renderPixmap(float x0,float y0,float x1,float y1,
                                            int w,int h,unsigned char* pixmap,int landscape, bool) const
{
	MagLog::debug() << "MingDriver::renderPixmap needs implementing." <<endl;
	return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are 
  mainly used for satellite data.

  \sa renderPixmap()

  \param image Object containing an image
*/
MAGICS_NO_EXPORT bool MingDriver::renderCellArray(const Image& image) const
{
	MagLog::debug() << "MingDriver::renderCellArray needs implementing." <<endl;
	return true;
}

/*!
  \brief render filled polygons

  This method renders filled polygons which were priviously defined in the BaseDriver.

  \sa BaseDriver::renderPolylineSets()
*/
MAGICS_NO_EXPORT void MingDriver::endPolygon() const
{
/*
	SWFFillStyle *fill =  SWFFillStyle::SolidFillStyle(255, 128, 0, 255);
	SWFShape *shape = new SWFShape();
	shape->setRightFillStyle(fill);
	
	shape->setLine(1, 0,0,0,255);
	shape->drawLine(100, 0);
	shape->drawLine(0, 100);
	shape->drawLine(-100, 0);
	shape->drawLine(0, -100);

	movie_->add(shape);
*/
}

/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void MingDriver::debugOutput(const string &s) const
{
	MagLog::debug() << s << endl;
}

/*!
  \brief class information are given to the output-stream
*/
void MingDriver::print(ostream& out)  const
{
	out << "MingDriver[";
	out << "]";
}

static SimpleObjectMaker<MingDriver, BaseDriver> Ming_driver("Ming");

