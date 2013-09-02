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

/*! \file BaseDriver.cc
    \brief Implementation of driver base class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

*/

#include <BaseDriver.h>

#include <Layout.h>
#include <Layer.h>
#include <Polyline.h>
#include <Text.h>
#include <BinaryObject.h>
#include <Colour.h>
#include <Arrow.h>
#include <Flag.h>
#include <ImportObject.h>
#include <Image.h>
#include <Symbol.h>
#include <PaperPoint.h>
#include "AnimationRules.h"

#include <BaseDriverSymbols.h>
#include <BaseDriverWind.h>
#include <BaseDriverImages.h>
#include <BaseDriverBinaryReader.cc>

#include <System.h>

using namespace magics;

int BaseDriver::numFiles_ = 0;



//! Constructor for all drivers
/*!
  Main job is to initialise most variables and read font information
*/
BaseDriver::BaseDriver():currentPage_(-1),fileName_(""),currentLayer_(""),
        currentLineType_(M_SOLID),currentLineWidth_(-1), currentLineStyle_(1),currentColour_(Colour("white")),
	coordRatioX_(1),coordRatioY_(1),
//	lastAreaHeightPercentage_(0.),lastAreaWidthPercentage_(0.),
	newPage_(true),//newLayout_(true), // external_(false),
	disabled_(false),alphaEnabled_(false),
	indexHatch_(0),currentShading_(M_SH_NONE),cmScale_(1.),
	xDeviceLength_(MagTranslator<double, double>().magics("SUPER_PAGE_X_LENGTH")),
	yDeviceLength_(MagTranslator<double, double>().magics("SUPER_PAGE_Y_LENGTH")),obs_distance_(-1.)
//	polylineAntialiasing_(false),
{
        //width in pixels  = cm * 2.54 * dpi
        //height in pixels = cm * 2.54 * dpi
/*        const string size = getSize();
        const MFloat cmdpi = 2.54 * 72.;
        if(!magCompare(size,"a4"))
        {
            string::size_type posX = size.find_first_of("x");
            if(posX != string::npos)
            {
                const string first = size.substr(0,posX);
                string::size_type posCM = size.find_first_of("c",posX+1);
                if(posCM != string::npos) // assume CM
                {
                   MFloat xcm=0.;
                   MFloat ycm=0.;
                   istringstream is(first);
                   is>>xcm;
                   xDeviceLength_ = static_cast<int>(xcm * cmdpi);
                   const string second = size.substr(posX+1,posCM);
                   istringstream iy(second);
                   iy>>ycm;
                   yDeviceLength_ = static_cast<int>(ycm * cmdpi);
                }
                else   // assume pixels
                {
                   xDeviceLength_ = atoi(first.c_str());
                   const string second = size.substr(posX+1);
                   yDeviceLength_ = atoi(second.c_str());
                }
            }
            else
            {
                if     (magCompare(size,"a4")) {xDeviceLength_= 21.  * cmdpi; yDeviceLength_= 29.7 * cmdpi;}
                else if(magCompare(size,"a3")) {xDeviceLength_= 29.7 * cmdpi; yDeviceLength_= 42.  * cmdpi;}
            }
        }
*/
}

BaseDriver::~BaseDriver()
{
  FontMap_.clear();
}


/*! \brief Method set solid fill shading properties

 \sa renderSimplePolygon
*/
void BaseDriver::shade(const FillShadingProperties& properties)  const
{
	currentShading_ = M_SH_SOLID;
	currentShadingProperties_ = &properties;
}

/*! \brief Method set hatch fill shading properties

 \sa renderSimplePolygon
*/
void BaseDriver::shade(const HatchShadingProperties& properties) const
{
	currentShading_ = M_SH_HATCH;
	currentShadingProperties_ = &properties;
}

/*! \brief Method set dot fill shading properties

 \sa renderSimplePolygon
*/
void BaseDriver::shade(const DotShadingProperties& properties)   const
{
	currentShading_ = M_SH_DOT;
	currentShadingProperties_ = &properties;
}

/*!
 \brief Method to print list of all generated files in the driver
*/
void BaseDriver::printOutputName(const std::string & str) const
{
	if(filelist_)
	{
		const SystemInfo info;
		fstream fs;
		if(numFiles_ == 0)
		{
			fs.open(filelist_name_.c_str(),fstream::out);
			fs << "# "<< getMagicsVersionString()<< " "<<info.getHostName() <<" " << info.getTime()<< "\n";
		}
		else
			fs.open(filelist_name_.c_str(),fstream::out | fstream::app);

		fs << info.getTime()<< " "<<str<< "\n";
		fs.close();
		numFiles_++;
	}
}


//! Method to read font information
/*!
 A hash table is produce to map font names to file names of TTF files.

  \todo make this a singleton!?
*/
void BaseDriver::readFonts() const
{
	const string s = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "Fonts.dat";
	ifstream psfile(s.c_str());

	if(psfile)
	{
		int	id;
		char	temp[128];
		char	magics_name[64];
		char	ps_name[128];
		char	ps_filename[128];
		char	ttf_filename[128];
		char	css_name[128];

		// read header (4 lines) and ignore
		psfile.getline(temp,128);psfile.getline(temp,128);
		psfile.getline(temp,128);psfile.getline(temp,128);

		while (!psfile.eof())
		{
			magFont font;
			psfile >> id >> magics_name >> ps_name >> ps_filename >> ttf_filename >> css_name;
			font.id = id;
			font.magics_name = magics_name;
			font.ps_name = ps_name;
			font.ps_filename = ps_filename;
			font.ttf_filename = ttf_filename;
			font.css_name = css_name;
			FontMap_[string(magics_name)]= font;
		}
		psfile.close();
	}
	else MagLog::error() << "BaseDriver::readFonts() --> Cannot open Font file " << s << " ! No text can be plotted.\n";
}

/*! \brief formulating a filename

  This method is used to add the page number for multipage output,
  such in PostScriptDriver (if EPS or splitted), GDDriver (if not animated)
  and SVGDriver.

  It preserves the file name extension, and it needs it !!!
*/
string BaseDriver::getFileName(const string &extension, const unsigned int no) const
{
	// offsetting the current page number if the users has set so
	const unsigned int no2 = (firstvalue_ >= 0) ? (no+firstvalue_-1) : no;

	string ext = "."+extension;
	bool full     = false;
	bool legacy   = false;

	string filename = name_;
	if(filename.empty())
	{
	  filename = fullname_;
	  if(!filename.empty()) full = true;
	}
	if(filename.empty())
	{
	  filename = legacyname_;
	  if(!filename.empty()) legacy = true;
	}
	if(filename.empty())
	{
	  filename = extension;
	}

	// name stays the same as given
	if( no==0 && (full || (legacy && extension=="ps")) ) return filename;
	if( no==0 && (extension=="ps" || extension=="pdf" || extension=="kmz") )  { filename += ext;return filename;}

	// if nothing is set
	if(filename=="") filename = "magics";

	if(full)
	{
		string::size_type pos = filename.find_last_of(".");
		if(pos != string::npos)
		{
			const string f = filename.substr(0,pos);
			const string t = filename.substr(pos);
			ext = t;
			filename = f;
		}
	}

	const int numberWidth = numberingwidth_;

	if( (no2 > 1) || firstnumber_ || legacy )
	{
		char *ostr;
		if(numberWidth==4)      {ostr=new char[5]; sprintf(ostr,"%04u",no2);}
		else if(numberWidth==3) {ostr=new char[4]; sprintf(ostr,"%03u",no2);}
		else if(numberWidth==2) {ostr=new char[3]; sprintf(ostr,"%02u",no2);}
		else                    {ostr=new char[2]; sprintf(ostr,"%u",no2);}
		filename += separator_+ostr+ext;
		delete [] ostr;
	}
	else // if first page no number
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
MAGICS_NO_EXPORT void BaseDriver::redisplay(const Layout& layout) const 
{
	project(layout);
	staLayouts_.push(&layout);
	layout.visit(*this);  // visit this layout!
	unproject();
}


MAGICS_NO_EXPORT void BaseDriver::redisplay(const RootLayout& root) const
{
	root.visit(*this);  // visit this ROOT layout!
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const LegendLayout& legend) const
{
	redisplay((const Layout&) legend);
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const SceneLayout& scene) const
{
	redisplay((const Layout&) scene);
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const StartPage& ) const
{
   startPage();
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const EndPage& ) const
{
   endPage();
   vecPoints_.clear();
}

MAGICS_NO_EXPORT void BaseDriver::redisplay(const ClearObject& ) const
{
   vecPoints_.clear();
}

void BaseDriver::redisplay(const PolylineSet& line) const
{
      line.visit(*this);
}

/*!
  \brief Decision how to stroke/fill simple polygon
  
  Overwritten in SVGDriver::redisplay(const Polyline& line) const
*/
void BaseDriver::redisplay(const Polyline& line) const
{
	if(line.isFilled())   renderSimplePolygon(line);
	if(line.isStroked())  printLine(line);
//	ArrowProperties* pArrowProperties = line.arrowProperties();
//	if(pArrowProperties)
}

void BaseDriver::redisplay(const Arrow& arrow) const
{
	renderWindArrow(arrow);
}

void BaseDriver::redisplay(const Flag& flag) const
{
	renderWindFlag(flag);
}

void BaseDriver::redisplay(const ImportObject& object) const
{
	renderImage(object);
}

void BaseDriver::redisplay(const Image& object) const
{
	renderCellArray(object);
}

void BaseDriver::redisplay(const Text& text) const
{
	renderText(text);
}

void BaseDriver::redisplay(const Symbol& symbol) const
{
	renderSymbols(symbol);
}

void BaseDriver::redisplay(const TextSymbol& symbol) const 
{
	renderTextSymbols(symbol);
}

void BaseDriver::redisplay(const ComplexSymbol& symbol) const 
{
	renderComplexSymbols(symbol);
}

void BaseDriver::redisplay(const TextItem& text, const ComplexSymbol& symbol) const
{
	renderTextItem(text, symbol);
}

void BaseDriver::redisplay(const SymbolItem& text, const ComplexSymbol& symbol) const
{
	renderSymbolItem(text, symbol);
}

void BaseDriver::redisplay(const FlagItem& text, const ComplexSymbol& symbol) const
{
	renderFlagItem(text, symbol);
}



/*
 \brief Least Square fit of a line.

 Returns the angle (in relation to horizon == 0 ).

 \sa printLine
*/
double BaseDriver::LSF(MFloat *x,MFloat *y, int i0) const
{
	double angle = 0.;
	double x_sum = 0.;
	double y_sum = 0.;
	const unsigned int n = 15;

	for(unsigned int r=0;r<n;r++)
	{
		x_sum += projectX(x[i0+r]);
		y_sum += projectY(y[i0+r]);
	}

	const double x_over_n = x_sum / n;
	const double y_over_n = y_sum / n;
	double sxx = 0;
	double sxy = 0;

	for(unsigned int r=0;r<n;r++)
	{
		const double xi = projectX(x[i0+r]) - x_over_n;
		const double yi = projectY(y[i0+r]) - y_over_n;
		sxx  += (xi*xi);
		sxy  += (xi*yi);
	}
	if(sxx != 0) angle = atan2( (sxy/sxx) ,1.);
	else MagLog::debug() << "BaseDriver: Devision through zero prevented in calculation of Label angle!" << endl;

	return angle;
}

/*!
  \brief Method plotting Polylines with Labels.

  \sa renderPolyline renderText

  \todo location memory for labels.
*/
void BaseDriver::printLine(const Polyline &line) const
{
    const unsigned long n = line.size();
    if(n < 1) return;

    MFloat *x      = new MFloat[n];
    MFloat *y      = new MFloat[n];

    for(unsigned long s=0;s<n;s++)
    {
        x[s] = line.get(s).x();
        y[s] = line.get(s).y();
    }

    // render line - driver specific part
    if(line.getThickness()>0  && !(line.getColour()==Colour("NONE")) )
    {
	setNewColour(line.getColour());
	currentLineStyle_ = setLineParameters(line.getLineStyle(),line.getThickness());

//	if(line.getAntiAliasing()) polylineAntialiasing_=true;
	renderPolyline(n, x, y);
//	polylineAntialiasing_=false;
    
	Polyline::Holes::const_iterator h  = line.beginHoles();
	Polyline::Holes::const_iterator he = line.endHoles();

	for (; h != he; ++h)
	{
		vector<double> xx;
		vector<double> yy;
		line.hole(h,xx,yy);
		const unsigned long nn = xx.size();
		MFloat *nx      = new MFloat[nn];
		MFloat *ny      = new MFloat[nn];
		for(unsigned int is=0;is<nn;is++)
		{
		  nx[is] = xx[is];
		  ny[is] = yy[is];
		}
		renderPolyline(nn, nx, ny);
		delete [] nx;
		delete [] ny;
	}
    }

    const unsigned int minimum_points_for_labelling = 32;  // must be at least -15 : see function LSF above

    if (line.getLabel().isVisible() && line.getLabel().getText() != "" && (n > minimum_points_for_labelling))
    {
	assert(staLayouts_.empty() == false);

	MFloat *labelx = new MFloat[n];  // in theory, we shouldn't need this many entries...
	MFloat *labely = new MFloat[n];  // in theory, we shouldn't need this many entries...

	// Store the minimum x,y corner, pretending that we're going to plot a label there.
	// We won't actually plot a label there though - it's just to make sure that
	// we don't plot one too close to that point.
	int num_labels = 1;
	labelx [0] = staLayouts_.top()->minX();
	labely [0] = staLayouts_.top()->minY();

	// Calculate how far apart the labels should be.
	// Our algorithm is to take the average of the width and height and divide by 2.
	// This should give us approximately 3 labels over the width of the page.
	const MFloat  coords_range_x = fabs(staLayouts_.top()->maxX()-staLayouts_.top()->minX());
	const MFloat  coords_range_y = fabs(staLayouts_.top()->maxY()-staLayouts_.top()->minY());
	const MFloat  min_distance_between_labels = (coords_range_x + coords_range_y) / 4.;
	const MFloat  min_square_distance_between_labels = (min_distance_between_labels * min_distance_between_labels);

	unsigned int i = 15;
	while(i < n-minimum_points_for_labelling)
	{
	  const double angle = LSF(x,y,i);
	  const double angle2 = LSF(x,y,i+1);
	  if(fabs(angle-angle2)< 0.01)
	  {
	    const MFloat THIS_X = x[i];
	    const MFloat THIS_Y = y[i];
	    const MFloat PREV_X = labelx[num_labels - 1];
	    const MFloat PREV_Y = labely[num_labels - 1];

	    const double distance_squared = ((THIS_X - PREV_X) * (THIS_X - PREV_X)) + ((THIS_Y - PREV_Y) * (THIS_Y - PREV_Y));

	    if (distance_squared > min_square_distance_between_labels)
	    {
                // line fitting
                MFloat pro_x = x[i+2];
                MFloat pro_y = y[i+2];  // lines not needed
                // end of line fitting
		
                Text text;
		PaperPoint pp(pro_x,pro_y);		
		text.push_back(pp);

		const Label label= line.getLabel();

		text.addText(label.getText(),label.getFontColour(),label.getFontSize()); 
		text.setBlanking(label.getBlanking());
		text.setJustification(label.getJustification());
		text.setVerticalAlign(MHALF);
		text.setAngle(-setAngleY(angle));	

		renderText(text);

		labelx [num_labels] = x[i];
		labely [num_labels] = y[i];
		num_labels++;
	     }
	  }//angles are not the same
	  i++;i++;
        }
        delete [] labelx;
        delete [] labely;
    }// endif enough points for a label

    delete [] x;
    delete [] y;

    currentColour_ = Colour("none");
}

void BaseDriver::renderPolyline(vector<PaperPoint> &vP) const
{
	const unsigned int size = vP.size();
	MFloat *x = new MFloat[size];
	MFloat *y = new MFloat[size];
	for(unsigned int i=0;i<size;i++)
	{
		x[i] = vP[i].x();
		y[i] = vP[i].y();
	}
	renderPolyline(size,x,y);

	delete [] x;
	delete [] y;
}

void BaseDriver::renderPolyline2(vector<PaperPoint> &vP) const
{
	const unsigned int size = vP.size();
	MFloat *x = new MFloat[size];
	MFloat *y = new MFloat[size];
	for(int unsigned i=0;i<size;i++)
	{
		x[i] = projectX(vP[i].x());
		y[i] = projectY(vP[i].y());
	}
	renderPolyline2(size,x,y);

	delete [] x;
	delete [] y;
}

void BaseDriver::renderSimplePolygon(vector<PaperPoint> &vP) const
{
	const unsigned int size = vP.size();
	MFloat *x = new MFloat[size];
	MFloat *y = new MFloat[size];
	for(unsigned int i=0;i<size;i++)
	{
		x[i] = vP[i].x();
		y[i] = vP[i].y();
	}
	renderSimplePolygon(size,x,y);

	delete [] x;
	delete [] y;
}

/*!
 Class information are given to the output-stream.
*/
void BaseDriver::print(ostream& out) const
{
	out << "BaseDriver";
}

string BaseDriver::getTmpName() const
{
	string stmp;
//	if(getTempdir()!="local") stmp=getTempdir();
//	string stmp = (tmpdir) ? tmpdir : tmp;
	stmp+= "magics_temp_ps_XXXXXX";
	char* mtmp = new char [stmp.length()+1];
	stmp.copy(mtmp,string::npos);
	mtmp[stmp.length()] = '\0';
#ifndef MAGICS_WINDOWS_CYGWIN
	const int m = mkstemp(mtmp);
	stmp = ( m ) ? mtmp : " ";
#endif
	delete [] mtmp;

	return stmp;
}

bool BaseDriver::renderCellArray(const Image& ) const
{
//	MagLog::dev() << "BaseDriver::renderCellArray" << endl;
	return true;
}

bool BaseDriver::renderPixmap(MFloat, MFloat, MFloat, MFloat, int, int, unsigned char*, int, bool) const
{
//	MagLog::dev() << "BaseDriver::renderPixmap" << endl;
	return true;
}


/*!
  Overwritten in KMLDriver
  
  \sa Layer
*/
MAGICS_NO_EXPORT void BaseDriver::redisplay(const Layer& layer) const 
{
  MagLog::dev() << "BaseDriver::redisplay( layer) > " << layer.name()<< endl;
}

void BaseDriver::redisplay(const SceneLayer& layer) const
{
	for (vector<Layer*>::iterator l = layer.beginLayer(); l != layer.endLayer(); ++l)
	{
		(*l)->redisplay(*this);
	}
}

void BaseDriver::redisplay(const StaticLayer& layer) const
{
	currentLayer_ = layer.name();
	layer.visit(*this);
}

void BaseDriver::redisplay(const NoDataLayer& layer) const
{
	currentLayer_ = layer.name();
	layer.visit(*this);
}

void BaseDriver::redisplay(const StepLayer& layer) const
{
  MagLog::dev() << "BaseDriver::redisplay( StepLayer&)" << layer.name()<< endl;
}
