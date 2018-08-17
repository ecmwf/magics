/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GeoJsonDriver.cc
    \brief Implementation of GeoJsonDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Jun 09 18:41:52 2015

*/

#include <GeoJsonDriver.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>
#include <Symbol.h>
#include <Layer.h>
#include <Arrow.h>
#include <Flag.h>


//! For generating ZIP files
extern "C"{
#include <sys/stat.h>
#include "minizip/zip.h"
#define MAXFILENAME 256
#define WRITEBUFFERSIZE 16384
#include <cstdio>    // BUFSIZ
#include <fcntl.h>   // open
#include <unistd.h>
}


using namespace magics;


/*!
  \brief Constructor
*/
GeoJsonDriver::GeoJsonDriver() : currentTimeBegin_(""),currentTimeEnd_(""),GeoJson_placemark_(false),
                         polyline_begin_(true),polygon_begin_(true),MultiGeometrySet_(false),layer_(false),
			 render_(true),ecmwf_logo_(false)
{
}

/*!
  \brief Destructor
*/
GeoJsonDriver::~GeoJsonDriver()
{
}

/*!
  \brief Opening the driver
*/
void GeoJsonDriver::open()
{
	currentPage_ = 0;

	fileName_ = getFileName("json");
	if(zip_) fileName_ = "doc.json";

	pFile_.open(fileName_.c_str());
	if(!pFile_)
	{
		MagLog::error() << " GeoJsonDriver --> Cannot write output file to what was specified: "<<fileName_<< endl;
		MagLog::error() << "";
		throw NoSuchFileException("Error opening GeoJson output file!");
	}
/*	pFile_	<< "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
		<< "<GeoJson xmlns=\"http://www.opengis.net/GeoJson/2.2\" \n"
		<< "     xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
		<< "<Document>\n"<< " <name>"<<title_<<"</name>\n"
		<< " <open>1</open>\n";
	pFile_  << " <atom:generator>"<<getMagicsVersionString()<<"</atom:generator>\n";
	if(!author_.empty()) pFile_ << " <atom:author><atom:name>"<<author_<<"</atom:name></atom:author>\n";
	if(!link_.empty())   pFile_ << " <atom:link href=\""<<link_<<"\" />\n";
	pFile_	<< " <description>\n"
		<< " <![CDATA["<<description_<<"]]>\n"
		<< " </description>\n"
		<< " <LookAt>\n"
		<< "\t<longitude>"<<longitude_<<"</longitude>\n"
		<< "\t<latitude>"<<latitude_<<"</latitude>\n"
		<< "\t<range>"<<range_*1000.<<"</range>\n"
		<< "\t<tilt>"<<tilt_<<"</tilt>\n"
		<< "\t<heading>0</heading>\n"    // always 0 = north!
		<< "\t<altitudeMode>absolute</altitudeMode>\n"    // possibly "relativeToGround"
		<< " </LookAt>\n"
		<< " <Style id=\"check-hide-children\">\n"
		<< "  <ListStyle>\n"
		<< "   <listItemType>checkHideChildren</listItemType>\n"
		<< "  </ListStyle>\n"
		<< " </Style>\n";
	*/
	GeoJson_placemark_=false;
}

/*!
  \brief Closing the driver

  While closing the driver all files generated (stored in GeoJson_output_resource_list_ )
  are put into one zip file. We use minizip (see minizip subdirectory to
  do so) since gnuzip is NOT sufficient!
*/
void GeoJsonDriver::close()
{
    if (GeoJson_placemark_) closePlacemark();

//    pFile_ << "</Document>\n</GeoJson>\n";
    pFile_.close();
    GeoJson_output_resource_list_.push_back(fileName_);

    /***********************  K M Z ********************************/
    if(zip_ && !GeoJson_output_resource_list_.empty())
    {
      fileName_ = getFileName("zip");
      zipFile zf;
      int err=0;

      zf = zipOpen(fileName_.c_str(),0);
      if (zf == 0)
      {
        printf("ERROR opening zip file %s\n",fileName_.c_str());
      }
      else
      {
	int size_buf = WRITEBUFFERSIZE;
	void* buf = (void*)malloc(size_buf);
	if (buf==0)
	{
		MagLog::error() <<"Error allocating memory for GeoJson ZIP generation!"<< std::endl;
		return;
	}
	stringarray::iterator it    = GeoJson_output_resource_list_.begin();
	stringarray::iterator itend = GeoJson_output_resource_list_.end();
	for(; it != itend; it++)
	{
		if(debug_) MagLog::dev()<< "GeoJsonDriver.close() > Start adding file " <<  *it << " to ZIP file.\n";
		FILE * fin;
		int size_read;

		const char *filename = (*it).c_str();

		err = zipOpenNewFileInZip(zf,filename, 0, 0, 0, 0, 0, 0, Z_DEFLATED, Z_DEFAULT_COMPRESSION);

		if(err != ZIP_OK)
			MagLog::error() << "Could NOT open ZIP file "<< filename << endl;
		else
		{
		  fin = fopen(filename,"rb");
		  if(fin==0)
		  {
		     MagLog::error() << "Open file "<<filename<<" to be added to ZIP FAILED!"<< endl;
		     return;
		  }
		  else
		  {
		    do{
			err=ZIP_OK;
			size_read = (int)fread(buf,1,size_buf,fin);
			if (size_read < size_buf)
			  if (feof(fin)==0)
			  {
				MagLog::error() << "Could NOT add "<<(*it) << endl;
				err = ZIP_ERRNO;
			  }

			if (size_read>0)
			{
			  err = zipWriteInFileInZip(zf,buf,size_read);
			  if (err<0)
			  {
				MagLog::error() << "Could NOT write ZIP file "<< fileName_<< endl;
			  }
			}
		     } while ((err==ZIP_OK) && (size_read>0));
		  }
		  if (fin)
			fclose(fin);
		}

		err = zipCloseFileInZip(zf);
		if (err!=ZIP_OK)
			MagLog::error() << "Could NOT close ZIP file "<< fileName_<< endl;
//		delete [] filename;
	}
	free(buf);

	err = zipClose(zf,0);
	if (err != ZIP_OK)
		MagLog::error() << "Could NOT close ZIP file "<< fileName_<< endl;
	else if (!debug_)
	{
		stringarray::iterator it = GeoJson_output_resource_list_.begin();
		stringarray::iterator itend = GeoJson_output_resource_list_.end();
		for(; it != itend; it++)
		{
			remove((*it).c_str());
		}
	}
	printOutputName("GeoJson zip "+fileName_);
   }// end Zipping ---> K M Z
  }
  else if(!zip_)
  {
	stringarray::iterator it = GeoJson_output_resource_list_.begin();
	stringarray::iterator itend = GeoJson_output_resource_list_.end();
	for(; it != itend; it++)
	{
		printOutputName("GeoJson misc "+(*it));
	}
  }
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.

  \note There is NO page concept in GeoJson!
*/
MAGICS_NO_EXPORT void GeoJsonDriver::startPage() const
{
	currentPage_++;

	debugOutput("Start Page");

	polyline_begin_ = true;
	polygon_begin_  = true;
	currentLayer_   = "Page";
	newLayer();
}

/*!
  \brief ending a page

  This method has to take care that for formats with multiple output
  files are closed.

  \note There is NO page concept in GeoJson!
*/
MAGICS_NO_EXPORT void GeoJsonDriver::endPage() const
{
	if (GeoJson_placemark_) closePlacemark();
	closeLayer();
	debugOutput("Close page");
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void GeoJsonDriver::project(const magics::Layout& layout) const
{
//MagLog::dev() << " GeoJsonDriver::project("<<layout.id()<<")"<< endl;
}

MAGICS_NO_EXPORT void GeoJsonDriver::redisplay(const magics::LegendLayout& layout) const
{
    MagLog::warning() << " GeoJsonDriver> legend could NOT be generated!"<< endl;
}

/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa Layer
*/
MAGICS_NO_EXPORT void GeoJsonDriver::redisplay(const StaticLayer& layer) const
{
	currentLayer_     = (layer.name().empty()) ? "StaticLayer" : layer.name();
	currentTimeBegin_ = layer.timeBegin();
	currentTimeEnd_   = layer.timeEnd();
	currentTimeStamp_ = layer.timeStamp();
	newLayer();
	layer.visit(*this);
	closeLayer();
}

MAGICS_NO_EXPORT void GeoJsonDriver::redisplay(const StepLayer& layer) const
{
	currentLayer_     = (layer.name().empty()) ? "Step" : layer.name();
	currentTimeBegin_ = layer.timeBegin();
	currentTimeEnd_   = layer.timeEnd();
	currentTimeStamp_ = layer.timeStamp();
	newLayer();
	layer.visit(*this);
	closeLayer();
}

MAGICS_NO_EXPORT void GeoJsonDriver::redisplay(const SceneLayer& layer) const
{
	currentLayer_     = (layer.name().empty()) ? "Scene" : layer.name();
//	currentTimeBegin_ = layer.timeBegin();
//	currentTimeEnd_   = layer.timeEnd();
//	currentTimeStamp_ = layer.timeStamp();
	newLayer();
	layer.visit(*this);
	closeLayer();
}

/*!
   \brief gets ignored in GeoJson

   \sa BaseDriver::redisplay(const NoDataLayer&)
 */
MAGICS_NO_EXPORT void GeoJsonDriver::redisplay(const NoDataLayer& layer) const
{
	if( coastlines_ )
	{
		currentLayer_     = (layer.name().empty()) ? "NoData" : layer.name();
		currentTimeBegin_ = layer.timeBegin();
		currentTimeEnd_   = layer.timeEnd();
		newLayer();
		layer.visit(*this);
		closeLayer();
	}
}

/*!
  \brief open new layer

*/
MAGICS_NO_EXPORT void GeoJsonDriver::newLayer() const
{
	if (GeoJson_placemark_) closePlacemark();

	// cut off path (especially for Metview
	unsigned found = currentLayer_.find_last_of("/\\");
	currentLayer_=currentLayer_.substr(found+1);

	debugOutput("Start Layer - "+currentLayer_);

	polyline_begin_=true;
	polygon_begin_=true;
	layer_=true;
	render_=true;
}


/*!
  \brief close the current layer

  This method will close an existing layer. This includes resets of existing boxes.

*/
MAGICS_NO_EXPORT void GeoJsonDriver::closeLayer() const
{
	if (GeoJson_placemark_) closePlacemark();

	layer_=false;
	render_=false;
//	pFile_	<< "</Folder>\n";
	debugOutput("Close Layer - "+currentLayer_);
}


MAGICS_NO_EXPORT void GeoJsonDriver::closePlacemark() const
{
}

/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  Expression is aabbggrr, where aa=alpha (00 to ff); bb=blue (00 to ff);
  gg=green (00 to ff); rr=red (00 to ff)

  \sa Colour
*/
MAGICS_NO_EXPORT void GeoJsonDriver::setNewColour(const Colour &colour) const
{
}

MAGICS_NO_EXPORT void GeoJsonDriver::writeColour(const Colour &col) const
{
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void GeoJsonDriver::setNewLineWidth(const MFloat width) const
{
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  param linestyle Object describing the line style
  param w width of the line

*/
MAGICS_NO_EXPORT int GeoJsonDriver::setLineParameters(const LineStyle , const MFloat w) const
{
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
MAGICS_NO_EXPORT void GeoJsonDriver::renderPolyline(const int n, MFloat *x, MFloat *y) const
{
	pFile_ << "{\n \"type\": \"Feature\", \"properties\": {\n    \"value\": \"2000\"\n  }, \"geometry\": { \n\t\"coordinates\": [\n  [\n";
	for(int is=0;is<n;is++)
	{
		pFile_ <<"   ["<< x[is]<<","<<y[is]<<"]";
		if(is<n-1) pFile_ <<",\n";
		else       pFile_ <<"\n";
	}
	pFile_ << "  ] ],\n  \"properties\": {\n    \"type\": \"isoline\"\n  },\n  \"type\": \"MultiLineString\"\n} },"<<endl;
}


/*!
  \brief renders a filled polygon
*/
MAGICS_NO_EXPORT void GeoJsonDriver::renderSimplePolygon(const int n, MFloat* xx, MFloat* yy) const
{
	if (!render_) return;
	if (GeoJson_placemark_) closePlacemark();

	pFile_ << "{\n \"coordinates\": [\n  [\n";
	for(int is=0;is<n;is++)
	{
		pFile_ <<"   ["<< xx[is]<<","<<yy[is]<<"]";
		if(is<n-1) pFile_ <<",\n";
		else       pFile_ <<"\n";
	}
	pFile_ << "  ],\n  \"properties\": {\n    \"type\": \"cold fronts\"\n  },\n  \"type\": \"MultiLineString\"\n}"<<endl;
}

void GeoJsonDriver::renderSimplePolygon(const Polyline& line) const
{
	if (!render_) return;
	if (GeoJson_placemark_) closePlacemark();
	const unsigned int n = line.size();
	if(n<3) return;

	Colour tmpcol = currentColour_;
	setNewColour(line.getFillColour());

	MFloat *x = new MFloat[n];
	MFloat *y = new MFloat[n];
	for(unsigned int i=0;i<n;i++)
	{
		const PaperPoint& pp = line.get(i);
		x[i] = pp.x();
		y[i] = pp.y();
	}

	pFile_ << "{\n \"type\": \"Feature\", \"properties\": {\n   ";
	pFile_ << " \"colour\": \"" << line.getFillColour().name() << "\"";

	pFile_ << "\n }, \"geometry\" : {\n \"coordinates\": [\n  [\n";
	for(int is=0;is<n;is++)
	{
		pFile_ <<"   ["<< x[is]<<","<<y[is]<<"]";
		if(is<n-1) pFile_ <<",\n";
		else       pFile_ <<"\n";

	}
	pFile_ << "  ] ],\n   \"type\": \"Polygon\"\n} },"<<endl;

	delete [] x;
	delete [] y;

/*
	Polyline::Holes::const_iterator h = line.beginHoles();
	Polyline::Holes::const_iterator he = line.endHoles();

	for (; h != he; ++h)
	{
	   pFile_ << "  <innerBoundaryIs>\n"
	          << "   <LinearRing>\n"
	          << "    <coordinates>\n";

	   vector<double> x;
	   vector<double> y;
	   line.hole(h,x,y);
	   if ( x.empty() )
	     continue;
	   vector<double>::const_iterator yt = y.begin();
	   vector<double>::const_iterator xt = x.begin();
	   for(int it=0;it<x.size();it++)
	    {
	       pFile_ <<"\t"<< x[it] <<","<< y[it] <<","<<height_*1000<<"\n";
	    }

	    pFile_ << "    </coordinates>\n"
	           << "   </LinearRing>\n"
	           << "  </innerBoundaryIs>\n";
	}
	pFile_ << "</Polygon>\n";
	pFile_ << "</MultiGeometry>\n</Placemark>";
*/
	setNewColour(tmpcol);
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void GeoJsonDriver::renderText(const Text& text) const
{
}



/*!
  \brief render pixmaps

  This method renders pixmaps. These are used for cell shading and raster input (GIFs and PNGs).

  \sa renderCellArray()

  param x0 x of lower corner
  param y0 y of lower corner
  param x1 x of higher corner
  param y1 y of higher corner
  param w width of pixmap
  param h height of pixmap
  param pixmap contents

*/
MAGICS_NO_EXPORT bool GeoJsonDriver::renderPixmap(MFloat x0,MFloat y0,MFloat x1,MFloat y1,
                                            int w,int h,unsigned char* pixmap,int, bool) const
{
  debugOutput("Start renderPixmap");
  if(render_)
  {
	   if (GeoJson_placemark_) closePlacemark();
	    MagLog::warning() << "Image import is not implemented for the used driver!!!" << endl; return false;
	    debugOutput("End renderPixmap");
  }
  return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  sa renderPixmap()

  param image Object containing an image
*/
MAGICS_NO_EXPORT bool GeoJsonDriver::renderCellArray(const Image& image) const
{
   debugOutput("Start renderCellArray");
   if(render_)
   {
	if (GeoJson_placemark_) closePlacemark();
	MagLog::warning() << "Image import is not implemented for the used driver!!!" << endl; return false;
   }
   debugOutput("End renderCellArray");
   return true;
}


//! Method to plot symbols
/*!
 Needs special treatment of MagLogo. The logo is added on close().

 \sa close
*/
MAGICS_NO_EXPORT void GeoJsonDriver::renderSymbols(const Symbol& symbol) const
{
	if(symbol.getSymbol()=="logo_ecmwf")
		ecmwf_logo_=true;
//	else
//		BaseDriver::renderSymbols(symbol);
}


MAGICS_NO_EXPORT void GeoJsonDriver::renderWindArrow(const Arrow &arrow) const
{
}


MAGICS_NO_EXPORT void GeoJsonDriver::renderWindFlag(const Flag &flag) const
{
}


/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void GeoJsonDriver::debugOutput(const string &s) const
{
}

/*!
  \brief class information are given to the output-stream
*/
void GeoJsonDriver::print(ostream& out)  const
{
	out << "GeoJsonDriver[";
	out << "]";
}

static SimpleObjectMaker<GeoJsonDriver, BaseDriver> GeoJson_driver("GeoJson");
