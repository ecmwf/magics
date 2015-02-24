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

/*! \file KMLDriver.cc
    \brief Implementation of KMLDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Thu Oct 18 18:41:52 2007

*/

#include <KMLDriver.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>
#include <Symbol.h>
#include <Layer.h>
#include <Arrow.h>
#include <Flag.h>


//! For generating KMZ files
extern "C"{
#include <sys/stat.h>
#include "minizip/zip.h"
#define MAXFILENAME 256
#define WRITEBUFFERSIZE 16384
#include <cstdio>    // BUFSIZ
#include <fcntl.h>   // open
#include <unistd.h>
}


#ifdef MAGICS_RASTER
#include <gd.h>
#endif

#ifdef MAGICS_CAIRO
#include <cairo.h>
#include <CairoDriver.h>
#endif

using namespace magics;

bool copy_file(string from, string to)
{
    char buf[BUFSIZ];
    size_t size;

    int source = open(from.c_str(), O_RDONLY, 0);
    int dest   = open(  to.c_str(), O_WRONLY | O_CREAT, 0644);

    if(source >= 0 && dest >= 0)
    {
      while ((size = read(source, buf, BUFSIZ)) > 0)
        write(dest, buf, size);
      close(source);
      close(dest);
    }
    else
    {
      MagLog::error() << "KMZ copy file> file "<< from << " could NOT be copied to "<< to << endl;
      close(source);
      close(dest);
      return false;
    }

    return true;
}

/*!
  \brief Constructor
*/
KMLDriver::KMLDriver() : currentTimeBegin_(""),currentTimeEnd_(""),kml_placemark_(false),
                         polyline_begin_(true),polygon_begin_(true),MultiGeometrySet_(false),layer_(false),
			 render_(true),ecmwf_logo_(false)
{
}

/*!
  \brief Destructor
*/
KMLDriver::~KMLDriver()
{
}

/*!
  \brief Opening the driver
*/
void KMLDriver::open()
{
	kmz_ = kmz_;
	currentPage_ = 0;

	fileName_ = getFileName("kml");
	if(kmz_) fileName_ = "doc.kml";

	pFile_.open(fileName_.c_str());
	if(!pFile_)
	{
		MagLog::error() << "KMLDriver::open() --> Cannot open KML output file: " << fileName_ << "!\n";
		MagLog::error() << "";  // to ensure that the error message is broadcast
		terminate();
	}
	pFile_	<< "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
		<< "<kml xmlns=\"http://www.opengis.net/kml/2.2\" \n"
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
	kml_placemark_=false;
}

/*!
  \brief Closing the driver

  While closing the driver all files generated (stored in kml_output_resource_list_ )
  are put into one KMZ zip like file. We use minizip (see minizip subdirectory to
  do so) since gnuzip is NOT sufficient!
*/
void KMLDriver::close()
{
    if (kml_placemark_) closePlacemark();

    // Wind icon file
    const string iconfile="magics_kml_icons.png";
    const string path=getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_;
    const string icon_path=path + iconfile;

    bool is_copied = copy_file(icon_path,iconfile);
    if(is_copied) kml_output_resource_list_.push_back(iconfile);

    if(ecmwf_logo_)
    {
       const string logofilename = "kml_logo.png";
       const string logofile = path + logofilename;
       is_copied = copy_file(logofile,logofilename);
       if(is_copied) kml_output_resource_list_.push_back(logofilename);
   
       pFile_ << "<ScreenOverlay id=\"logo\">\n"
              << "<name>MagLogo</name>\n"
              << "<Icon>\n"
              << " <href>"<<logofilename<<"</href>\n"
              << "</Icon>\n"
              << "<overlayXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>\n"
              << "<screenXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>\n"
              << "<size x=\"-1\" y=\"0.1\" xunits=\"fraction\" yunits=\"fraction\"/>\n"
              << "</ScreenOverlay>\n";
    }

    pFile_ << "</Document>\n</kml>\n";
    pFile_.close();
    kml_output_resource_list_.push_back(fileName_);

    /***********************  K M Z ********************************/    
    if(kmz_ && !kml_output_resource_list_.empty())
    {
      fileName_ = getFileName("kmz");
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
		MagLog::error() <<"Error allocating memory for KMZ generation!"<< std::endl;
		return;
	}
	stringarray::iterator it    = kml_output_resource_list_.begin();
	stringarray::iterator itend = kml_output_resource_list_.end();
	for(; it != itend; it++)
	{
		if(debug_) MagLog::dev()<< "KMLDriver.close() > Start adding file " <<  *it << " to KMZ file.\n";
		FILE * fin;
		int size_read;

		const char *filename = (*it).c_str();

		err = zipOpenNewFileInZip(zf,filename, 0, 0, 0, 0, 0, 0, Z_DEFLATED, Z_DEFAULT_COMPRESSION);

		if(err != ZIP_OK)
			MagLog::error() << "Could NOT open KMZ file "<< filename << endl;
		else
		{
		  fin = fopen(filename,"rb");
		  if(fin==0)
		  {
		     MagLog::error() << "Open file "<<filename<<" to be added to KMZ FAILED!"<< endl;
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
				MagLog::error() << "Could NOT write KMZ file "<< fileName_<< endl;
			  }
			}
		     } while ((err==ZIP_OK) && (size_read>0));
		  }
		  if (fin)
			fclose(fin);
		}

		err = zipCloseFileInZip(zf);
		if (err!=ZIP_OK)
			MagLog::error() << "Could NOT close KMZ file "<< fileName_<< endl;
//		delete [] filename;
	}
	free(buf);

	err = zipClose(zf,0);
	if (err != ZIP_OK)
		MagLog::error() << "Could NOT close KMZ file "<< fileName_<< endl;
	else if (!debug_)
	{
		stringarray::iterator it = kml_output_resource_list_.begin();
		stringarray::iterator itend = kml_output_resource_list_.end();
		for(; it != itend; it++)
		{
			remove((*it).c_str());
		}
	}
	printOutputName("KML kmz "+fileName_);
   }// end Zipping ---> K M Z
  }
  else if(!kmz_)
  {
	stringarray::iterator it = kml_output_resource_list_.begin();
	stringarray::iterator itend = kml_output_resource_list_.end();
	for(; it != itend; it++)
	{
		printOutputName("KML misc "+(*it));
	}
  }
}

/*!
  \brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.

  \note There is NO page concept in KML!
*/
MAGICS_NO_EXPORT void KMLDriver::startPage() const
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

  \note There is NO page concept in KML!
*/
MAGICS_NO_EXPORT void KMLDriver::endPage() const
{
	if (kml_placemark_) closePlacemark();
	closeLayer();
	debugOutput("Close page");
}

/*!
  \brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \sa Layout
*/
MAGICS_NO_EXPORT void KMLDriver::project(const magics::Layout& layout) const
{
//MagLog::dev() << " KMLDriver::project("<<layout.id()<<")"<< endl;
}

MAGICS_NO_EXPORT void KMLDriver::redisplay(const magics::LegendLayout& layout) const
{
#ifdef MAGICS_CAIRO
    //redisplay((const Layout&) legend);
    const string filename = "legend.png";
    cairo_surface_t* surface_ = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, layout.width(), layout.height());
    cairo_t* cr_ = cairo_create(surface_);
//    if(magCompare(transparent_,"off"))
    {
       cairo_set_source_rgb (cr_, 1.0, 1.0, 1.0); /* white */
    }
//    else
//    {
//       cairo_set_source_rgba (cr_, 1.0, 1.0, 1.0, 0.0); /* transparent */
//    }
    cairo_paint (cr_);
    
//    CairoDriver cD;
//    cD.redisplay((const Layout&) layout);

    cairo_surface_write_to_png(surface_, filename.c_str());
    kml_output_resource_list_.push_back(filename);
    pFile_  << "<ScreenOverlay id=\"legend\">\n"
            << "<name>Legend</name>\n"
            << "<Icon>\n"
            << " <href>"<<filename<<"</href>\n"
            << "</Icon>\n"
            << "<overlayXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>\n"
            << "<screenXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>\n"
            << "<size x=\"-1\" y=\"0.1\" xunits=\"fraction\" yunits=\"fraction\"/>\n"
            << "</ScreenOverlay>\n";
#else
    MagLog::warning() << " KMLDriver> legend could NOT be generated - Cairo support needs to be enabled!"<< endl;
#endif
}

/*!
  \brief setup a new layer

  This method will setup a new layer. Layers enable overlays of entities
  of information.

  \sa Layer
*/
MAGICS_NO_EXPORT void KMLDriver::redisplay(const StaticLayer& layer) const
{
	currentLayer_     = (layer.name().empty()) ? "StaticLayer" : layer.name();
	currentTimeBegin_ = layer.timeBegin();
	currentTimeEnd_   = layer.timeEnd();
	currentTimeStamp_ = layer.timeStamp();
	newLayer();
	layer.visit(*this);
	closeLayer();
}

MAGICS_NO_EXPORT void KMLDriver::redisplay(const StepLayer& layer) const
{
	currentLayer_     = (layer.name().empty()) ? "Step" : layer.name();
	currentTimeBegin_ = layer.timeBegin();
	currentTimeEnd_   = layer.timeEnd();
	currentTimeStamp_ = layer.timeStamp();
	newLayer();
	layer.visit(*this);
	closeLayer();
}

MAGICS_NO_EXPORT void KMLDriver::redisplay(const SceneLayer& layer) const
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
   \brief gets ignored in KML
   
   \sa BaseDriver::redisplay(const NoDataLayer&)
 */
MAGICS_NO_EXPORT void KMLDriver::redisplay(const NoDataLayer& layer) const
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
MAGICS_NO_EXPORT void KMLDriver::newLayer() const
{
	if (kml_placemark_) closePlacemark();

	// cut off path (especially for Metview
	unsigned found = currentLayer_.find_last_of("/\\");
	currentLayer_=currentLayer_.substr(found+1);

	debugOutput("Start Layer - "+currentLayer_);

	pFile_ << "<Folder>\n<name>Layer:"<<currentLayer_<<"</name>\n<open>0</open>\n"
	       << " <styleUrl>#check-hide-children</styleUrl>\n";
	if(!currentTimeBegin_.empty())
	{
		pFile_	<< "<TimeStamp>\n"
			<< " <when>"<<currentTimeStamp_<<"</when>\n"
 			<< "</TimeStamp>\n"
			<< "<styleUrl>#hiker-icon</styleUrl>\n";
	}
	else
	{
		pFile_	<< "<TimeSpan>\n"
			<< " <begin>"<<currentTimeBegin_<<"</begin>\n"
			<< " <end>"<<currentTimeEnd_<<"</end>\n"
 			<< "</TimeSpan>\n";
			//<< "<styleUrl>#hiker-icon</styleUrl>\n";
	}

	pFile_ << "<description><![CDATA[Layer:"<<currentLayer_<<"]]></description>\n";
	polyline_begin_=true;
	polygon_begin_=true;
	layer_=true;
	render_=true;
}


/*!
  \brief close the current layer

  This method will close an existing layer. This includes resets of existing boxes.

*/
MAGICS_NO_EXPORT void KMLDriver::closeLayer() const
{
	if (kml_placemark_) closePlacemark();

	layer_=false;
	render_=false;
	pFile_	<< "</Folder>\n";
	debugOutput("Close Layer - "+currentLayer_);
}


MAGICS_NO_EXPORT void KMLDriver::closePlacemark() const
{
	if(MultiGeometrySet_)
	{
		pFile_ << "</MultiGeometry>\n";
		MultiGeometrySet_=false;
	}
	pFile_ << "</Placemark>\n";
	kml_placemark_=false;
//	currentLayer_ = "non";

	if(!polygon_begin_) polygon_begin_=true;
}

/*!
  \brief sets a new colour

  This colour stays the default drawing colour until the painting in the
  current box is finished.

  Expression is aabbggrr, where aa=alpha (00 to ff); bb=blue (00 to ff);
  gg=green (00 to ff); rr=red (00 to ff)

  \sa Colour
*/
MAGICS_NO_EXPORT void KMLDriver::setNewColour(const Colour &colour) const
{
	currentColour_ = colour;
}

MAGICS_NO_EXPORT void KMLDriver::writeColour(const Colour &col) const
{
		const int r = (int)(col.red()*255.);
		const int g = (int)(col.green()*255.);
		const int b = (int)(col.blue()*255.);
		const int a = (int)(transparency_ * 2.55);

		pFile_	<< "\t<!-- r:"<<r<<" g:"<<g<<" b:"<<b <<" -->"<< endl;

		pFile_	<< "\t<color>" <<hex;
			if(a>15)	pFile_ <<a;
			else		pFile_ <<"0"<< a;
			if(b>15)	pFile_ <<b;
			else		pFile_ <<"0"<< b;
			if(g>15)	pFile_ <<g;
			else		pFile_ <<"0"<< g;
			if(r>15)	pFile_ <<r;
			else		pFile_ <<"0"<< r;
		pFile_	<< "</color>\n" << dec;
}

/*!
  \brief sets a new line width

  This line width stays the default width until the painting in the
  current box is finished.

  \sa setLineParameters()
*/
MAGICS_NO_EXPORT void KMLDriver::setNewLineWidth(const MFloat width) const
{
	currentLineWidth_ = (width<1) ? 1. : width;
}

/*!
  \brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the
  current box is finished.

  \sa LineStyle

  param linestyle Object describing the line style
  param w width of the line

*/
MAGICS_NO_EXPORT int KMLDriver::setLineParameters(const LineStyle , const MFloat w) const
{
	setNewLineWidth(w);

//	MagLog::debug() << "KMLDriver::setLineParameters needs implementing." << std::endl;
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
MAGICS_NO_EXPORT void KMLDriver::renderPolyline(const int n, MFloat *x, MFloat *y) const
{
  if(render_ && n > 1 )
  {
  	if(!kml_placemark_ || polyline_begin_)
	{
		if (kml_placemark_) closePlacemark();
		pFile_ << "<Placemark>\n";
		pFile_ << "<visibility>1</visibility>\n<open>0</open>\n";
		kml_placemark_=true;

		pFile_	<< "<Style>\n<LineStyle>\n";
		writeColour(currentColour_);
		pFile_	<< "\t<width>"<<currentLineWidth_<<"</width>\n"<<"</LineStyle>\n"
			<< "</Style>\n"
			<< "<MultiGeometry>\n";
		MultiGeometrySet_=true;
		polyline_begin_=false;
	}

	pFile_	<< "<LineString>\n"
		<< " <extrude>0</extrude>\n"
	//	<< " <altitudeMode>relativeToGround</altitudeMode>\n"
		<< " <altitudeMode>clampToGround</altitudeMode>\n"
		<< " <tessellate>0</tessellate>\n"
		<< " <coordinates>\n";

	for(int is=0;is<n;is++)
	{
		pFile_ <<"\t"<< x[is]<<","<<y[is]<<","<<height_*1000<<"\n";
	}
	pFile_ << " </coordinates>\n</LineString>\n";
  }
}



/*!
  \brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \sa setLineParameters()
  \param n number of points
  \param xx array of x values
  \param yy array of y values
*/
MAGICS_NO_EXPORT void KMLDriver::renderSimplePolygon(const int n, MFloat* xx, MFloat* yy) const
{
	if (!render_) return;
	if (kml_placemark_) closePlacemark();

	pFile_ << "<Placemark>\n";
	pFile_ << "<visibility>1</visibility>\n<open>0</open>\n";

	const int a = (int)(transparency_ * 2.55);

	pFile_	<< "<Style>\n<PolyStyle>\n";
	writeColour(currentColour_);
	pFile_	<< "\t<fill>1</fill>\n</PolyStyle>\n"
		<< "<LineStyle>\n"
		<< "\t<width>"<<2<<"</width>\n";
	writeColour(currentColour_);
	pFile_	<< "</LineStyle>\n"
		<< "</Style>\n"
		<< "<MultiGeometry>\n";
	MultiGeometrySet_=true;

	pFile_ << "<Polygon>\n"
	       << " <extrude>1</extrude>\n"
	//       << " <altitudeMode>relativeToGround</altitudeMode>\n"
	       << " <altitudeMode>clampToGround</altitudeMode>\n"
	       << " <tessellate>0</tessellate>\n"
	       << "  <outerBoundaryIs>\n"
	       << "   <LinearRing>\n"
	       << "    <coordinates>\n";

	for(int it=0;it<n;it++)
	{
		pFile_ <<"\t"<< xx[it] <<","<< yy[it] <<","<<height_*1000<<"\n";
	}

	pFile_ << "    </coordinates>\n"
	       << "   </LinearRing>\n"
	       << "  </outerBoundaryIs>\n";

	pFile_ << "</Polygon>\n";
	pFile_ << "</MultiGeometry>\n</Placemark>";
}

void KMLDriver::renderSimplePolygon(const Polyline& line) const
{
	if (!render_) return;
	if (kml_placemark_) closePlacemark();
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

	pFile_ << "<Placemark>\n";
	pFile_ << "<visibility>1</visibility>\n<open>0</open>\n";

	pFile_	<< "<Style>\n<PolyStyle>\n";
	writeColour(currentColour_);
	pFile_	<< "\t<fill>1</fill>\n</PolyStyle>\n"
		<< "<LineStyle>\n"
		<< "\t<width>"<<2<<"</width>\n";
	writeColour(currentColour_);
	pFile_	<< "</LineStyle>\n"
		<< "</Style>\n"
		<< "<MultiGeometry>\n";
	MultiGeometrySet_=true;

	pFile_ << "<Polygon>\n"
	       << " <extrude>1</extrude>\n"
	//       << " <altitudeMode>relativeToGround</altitudeMode>\n"
	       << " <altitudeMode>clampToGround</altitudeMode>\n"
	       << " <tessellate>0</tessellate>\n"
	       << "  <outerBoundaryIs>\n"
	       << "   <LinearRing>\n"
	       << "    <coordinates>\n";

	for(int it=0;it<n;it++)
	{
		pFile_ <<"\t"<< x[it] <<","<< y[it] <<","<<height_*1000<<"\n";
	}
	delete [] x;
	delete [] y;

	pFile_ << "    </coordinates>\n"
	       << "   </LinearRing>\n"
	       << "  </outerBoundaryIs>\n";

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
	setNewColour(tmpcol);
}

/*!
  \brief renders text strings

  This method renders given text strings.

  \sa Text
  \param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void KMLDriver::renderText(const Text& text) const
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
MAGICS_NO_EXPORT bool KMLDriver::renderPixmap(MFloat x0,MFloat y0,MFloat x1,MFloat y1,
                                            int w,int h,unsigned char* pixmap,int, bool) const
{
  debugOutput("Start renderPixmap");
  if(render_)
  {
	if (kml_placemark_) closePlacemark();
#ifndef MAGICS_CAIRO
#ifndef MAGICS_RASTER
	MagLog::warning() << "Image import is not implemented for the used driver!!!" << endl; return false;
#else
MagLog::dev()<< "KML driver Image import uses GD -> for better results use Cairo backend."<< endl;
	stringstream out;
	out << 15 * kml_output_resource_list_.size();
	const string filename = "KML_overlay_"+out.str()+"png";

	pFile_ << "<GroundOverlay>\n";

	const int a = (int)(transparency_ * 2.55);
	pFile_ << "<styleUrl>#hiker-icon</styleUrl>\n"
		<< "<color>"<<hex;
		if(a>15)	pFile_ <<a;
		else		pFile_ <<"0"<< a << dec;
	pFile_ << "ffffff</color>\n"
		<< "<visibility>1</visibility>\n"
		<< "<color>"<<hex;
			if(a>15)	pFile_ <<a;
			else		pFile_ <<"0"<< a << dec;
	pFile_ << "FFFFFF</color>\n"
		<< "<Icon>\n"
		<< "<href>"<<filename<<"</href>\n"
//		<< "<refreshMode>onInterval</refreshMode>\n"
//		<< "<refreshInterval>86400</refreshInterval>\n"
//		<< "<viewBoundScale>0.75</viewBoundScale>\n"
		<< "</Icon>\n"
		<< "<LatLonBox>\n"
		<< "   <north>"<<y0<<"</north>\n"
		<< "   <south>"<<y1<<"</south>\n"
		<< "   <east>"<<x1<<"</east>\n"
		<< "   <west>"<<x0<<"</west>\n"
		<< "   <rotation>0</rotation>\n"
		<< "</LatLonBox>\n";


	gdImagePtr im = gdImageCreateTrueColor(w,h);
	unsigned char *p = pixmap;
	gdImageColorAllocateAlpha(im, 255, 255, 255, 127);

	for(int i=h-1;i>=0;i--)
	{
		for(int j=0;j<w; j++)
		{
			const int r = (int) *(p++);
			const int g = (int) *(p++);
			const int b = (int) *(p++);
			if(r*g*b >= 0.)
			{
			  const int col = gdImageColorResolveAlpha(im,r,g,b,0);
			  gdImageSetPixel(im, w, h, col);
			}
		}
	}
	gdImageDestroy(im);
	gdImageAlphaBlending(im, 1);
	gdImageSaveAlpha(im, 1); // save transparency

	FILE *outFile = fopen(filename.c_str(),"wb");
	gdImagePng(im,outFile);
	fclose(outFile);
	kml_output_resource_list_.push_back(filename);

	pFile_	<< "</GroundOverlay>\n";
#endif
#else
	stringstream out;
	out << 15 * kml_output_resource_list_.size();
	string filename = "KML_overlay_"+out.str()+"png";

	pFile_	<< "<GroundOverlay>\n";

	const int a = (int)(transparency_ * 2.55);
	pFile_	<< "<styleUrl>#hiker-icon</styleUrl>\n"
		<< "<color>"<<hex;
		if(a>15)	pFile_ <<a;
		else		pFile_ <<"0"<< a << dec;
	pFile_ << "ffffff</color>\n"
		<< "<visibility>1</visibility>\n"
		<< "<color>"<<hex;
		if(a>15)	pFile_ <<a;
		else		pFile_ <<"0"<< a << dec;
	pFile_ << "FFFFFF</color>\n"
		<< "<Icon>\n"
		<< "<href>"<<filename<<"</href>\n"
//		<< "<refreshMode>onInterval</refreshMode>\n"
//		<< "<refreshInterval>86400</refreshInterval>\n"
//		<< "<viewBoundScale>0.75</viewBoundScale>\n"
		<< "</Icon>\n"
		<< "<LatLonBox>\n"
		<< "   <north>"<<y0<<"</north>\n"
		<< "   <south>"<<y1<<"</south>\n"
		<< "   <east>"<<x1<<"</east>\n"
		<< "   <west>"<<x0<<"</west>\n"
		<< "   <rotation>0</rotation>\n"
		<< "</LatLonBox>\n";

	cairo_surface_t* surface_ = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,w,h);
	cairo_t* cr_ = cairo_create(surface_);

// \todo specify layer transparency

	// set background to transparent
//	cairo_save (cr_);
	cairo_set_source_rgba (cr_, 1.0, 1.0, 1.0, 0.0);
//	cairo_set_operator (cr_, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr_);
//	cairo_restore (cr_);

	unsigned char *p = pixmap;
	const MFloat dx =  (x1 - x0)/w;
	const MFloat dy = -(y1 - y0)/h;   // Minus needed for Y axis correction

	const MFloat X0 = x0;
	const MFloat Y0 = y0;

	for(int i=h-1;i>=0;i--)
	{
		for(int j=0;j<w; x0+=dx,j++)
		{
			const MFloat r = *(p++);
			const MFloat g = *(p++);
			const MFloat b = *(p++);

			if( (r*g*b) >= 0.)
			{
				cairo_set_source_rgba(cr_,r,g,b,0.5);

				const MFloat x0 = X0+(j*dx)+.5;
				const MFloat y0 = Y0+(i*dy)+.5;
				cairo_rectangle (cr_, x0,y0,dx,-dy);
				cairo_stroke_preserve(cr_);
				cairo_fill (cr_);
			}
		}
		x0 = X0;
		y0 += dy;
	}
	cairo_surface_write_to_png (surface_, filename.c_str());
	cairo_destroy (cr_);
	cairo_surface_destroy (surface_);
	kml_output_resource_list_.push_back(filename);
	pFile_	<< "</GroundOverlay>\n";
#endif
   }
	debugOutput("End renderPixmap");
	return true;
}

/*!
  \brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are
  mainly used for satellite data.

  sa renderPixmap()

  param image Object containing an image
*/
MAGICS_NO_EXPORT bool KMLDriver::renderCellArray(const Image& image) const
{
   debugOutput("Start renderCellArray");
   if(render_)
   {
	if (kml_placemark_) closePlacemark();
#ifndef MAGICS_CAIRO
#ifndef MAGICS_RASTER
	MagLog::warning() << "Image import is not implemented for the used driver!!!" << endl; return false;
#else
MagLog::dev()<< "KML driver uses GD -> for better results use Cairo backend."<< endl;
	stringstream out;
	out << currentPage_;
	const string layer_name = (!currentLayer_.empty()) ? currentLayer_ : "default_page";
	const string filename = "KML_cell_overlay_"+layer_name+"_"+out.str()+".png";

	pFile_	<< "<GroundOverlay>\n";

	const int a = (int)(transparency_ * 2.55);
	pFile_	<< "<color>"<<hex;
		if(a>15)	pFile_ <<a;
		else		pFile_ <<"0"<< a << dec;
	pFile_ << "ffffff</color>\n"
		<< "<visibility>1</visibility>\n"
		<< "<Icon>\n"
		<< "<href>"<<filename<<"</href>\n"
//		<< "<refreshMode>onInterval</refreshMode>\n"
//		<< "<refreshInterval>86400</refreshInterval>\n"
//		<< "<viewBoundScale>0.75</viewBoundScale>\n"
		<< "</Icon>\n"
		<< "<LatLonBox>\n"
		<< "   <north>"<<image.getOrigin().y()<<"</north>\n"
		<< "   <south>"<<image.getOrigin().y()-image.getHeight()<<"</south>\n"
		<< "   <east>"<<image.getOrigin().x()+image.getWidth()<<"</east>\n"
		<< "   <west>"<<image.getOrigin().x()<<"</west>\n"
		<< "   <rotation>0</rotation>\n"
		<< "</LatLonBox>\n";

	ColourTable &lt = image.getColourTable();
	const int width = image.getNumberOfColumns();
	const int height = image.getNumberOfRows();

	gdImagePtr im = gdImageCreateTrueColor(width,height);
	gdImageColorAllocateAlpha(im, 255, 255, 255, 127);

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
			const int a = static_cast<int>(lt[c].alpha()*127.);

			if(r*g*b >= 0.)
			{
			  const int col = gdImageColorResolveAlpha(im,r,g,b,a);
			  gdImageSetPixel(im, j, i, col);
			}
		  }// point has colour
		}
	}

	gdImageAlphaBlending(im, 1);
	gdImageSaveAlpha(im, 1); // save transparency

	FILE *outFile = fopen(filename.c_str(),"wb");
	gdImagePng(im,outFile);
	fclose(outFile);
	gdImageDestroy(im);
	kml_output_resource_list_.push_back(filename);

	pFile_	<< "</GroundOverlay>\n";
#endif
#else
	stringstream out;
	out << currentPage_;
	const string layer_name = (!currentLayer_.empty()) ? currentLayer_ : "default_page";
	string filename = "KML_cell_overlay_"+layer_name+"_"+out.str()+".png";
	string::size_type loc=0;

	while(loc != string::npos)
	{
	  loc=filename.find( ":", 0);
          if( loc != string::npos ) filename.replace(loc, 1, "-");
        }

	pFile_	<< "<GroundOverlay>\n";

	const int a = (int)(transparency_ * 2.55);
	pFile_	<< "<color>"<<hex;
		if(a>15)	pFile_ <<a;
		else		pFile_ <<"0"<< a << dec;
	pFile_ << "ffffff</color>\n"
		<< "<visibility>1</visibility>\n"
		<< "<Icon>\n"
		<< "<href>"<<filename<<"</href>\n"
//		<< "<refreshMode>onInterval</refreshMode>\n"
//		<< "<refreshInterval>86400</refreshInterval>\n"
//		<< "<viewBoundScale>0.75</viewBoundScale>\n"
		<< "</Icon>\n"
		<< "<LatLonBox>\n"
		<< "   <north>"<<image.getOrigin().y()<<"</north>\n"
		<< "   <south>"<<image.getOrigin().y()-image.getHeight()<<"</south>\n"
		<< "   <east>"<<image.getOrigin().x()+image.getWidth()<<"</east>\n"
		<< "   <west>"<<image.getOrigin().x()<<"</west>\n"
		<< "   <rotation>0</rotation>\n"
		<< "</LatLonBox>\n";

	ColourTable &lt = image.getColourTable();
	const int w = image.getNumberOfColumns();
	const int h = image.getNumberOfRows();

	cairo_surface_t* surface_ = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,w,h);
	cairo_t* cr_ = cairo_create(surface_);

// \todo specify layer transparency

	// set background to transparent
//	cairo_save (cr_);
	cairo_set_source_rgba (cr_, 1.0, 1.0, 1.0, 0.0);
//	cairo_set_operator (cr_, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr_);
//	cairo_restore (cr_);

	const MFloat dx = 1.;
	const MFloat dy = 1.;

	for (int i=h-1;i>=0;i--)
	{
		for(int j=0;j<w; j++)
		{
		  const int in = w*i+j;
		  const short c = image[in];

 		  if(!(lt[c]=="undefined"))
		  {
			MFloat r = lt[c].red();
			MFloat g = lt[c].green();
			MFloat b = lt[c].blue();
			MFloat a = lt[c].alpha();

			if( (r*g*b>1.) || (r*g*b<0.) )
			{
				r = 1.;
				g = 1.;
				b = 1.;
				a = 0.;
//				MagLog::info()<< "PostScriptDriver-> Cellshading colour not defined in table! Colour index: "<<c<< std::endl;
//    PostScript will always 'overpaint' anything below missing data!!!!
//
			}
			cairo_set_source_rgba(cr_,r,g,b,a);

			const MFloat x0 = (j*dx);
			const MFloat y0 = (i*dy);
			cairo_rectangle (cr_, x0,y0,dx,-dy);
			cairo_stroke_preserve(cr_);
			cairo_fill (cr_);
//			gdImageSetPixel(im, j, i, col);
		  }// point has colour
		}
	}

	cairo_surface_write_to_png (surface_, filename.c_str());

	cairo_destroy (cr_);
	cairo_surface_destroy (surface_);

	kml_output_resource_list_.push_back(filename);
	pFile_	<< "</GroundOverlay>\n";
#endif
   }
   debugOutput("End renderCellArray");
   return true;
}


//! Method to plot symbols
/*!
 Needs special treatment of MagLogo. The logo is added on close().

 \sa close
*/
MAGICS_NO_EXPORT void KMLDriver::renderSymbols(const Symbol& symbol) const
{
	if(symbol.getSymbol()=="logo_ecmwf")
		ecmwf_logo_=true;
//	else
//		BaseDriver::renderSymbols(symbol);
}


MAGICS_NO_EXPORT void KMLDriver::renderWindArrow(const Arrow &arrow) const
{
	const unsigned int arrPoNo = arrow.size();
	if(arrPoNo<1) return;

//	const ArrowPosition pos = arrow.getArrowPosition();

    Arrow::const_iterator arr = arrow.begin();
	for(unsigned int pts=0;pts<arrPoNo;pts++)
	{
        pFile_  << "<Placemark>\n"
                << "<Style>\n"
                << "<IconStyle>\n";
        writeColour(arrow.getColour());
        pFile_  << "<heading>"<<90.+(arr->angle()*-57.29578)<<"</heading>\n"
                << "<scale>"<<arr->norm()*0.2<<"</scale>\n"
                << "<Icon>\n"
                << "<href>magics_kml_icons.png</href>\n"
                << "<x>0</x>\n"
                << "<y>576</y>\n"
                << "<w>64</w>\n"
                << "<h>64</h>\n"
                << "</Icon>\n"
                << "</IconStyle>\n"
                << "</Style>\n"
                << "<Point>\n"
                << "<coordinates>"<<arr->point_.x()<<","<<arr->point_.y()<<",0</coordinates>\n"
                << "</Point>\n"
                << "</Placemark>" << endl;
	  ++arr;
	}
}


MAGICS_NO_EXPORT void KMLDriver::renderWindFlag(const Flag &flag) const
{
/*
 *	const MFloat thickness = (flag.getThickness()==1) ? 1 : 2.*flag.getThickness();
	const LineStyle style = flag.getStyle();
	const MFloat length = convertCM(flag.getLength() / coordRatioX_ );
	const string marker = flag.getOriginMarker();
	const MFloat markerHeight = convertCM(flag.getOriginHeight() *.5 / coordRatioX_);
*/
	setNewColour(flag.getColour());
//	const unsigned int flaPoNo = flag.size();
//	Flag::const_iterator fla = flag.begin();

/*	Symbol origin;
	if(markerHeight>0.)
	{
		origin.setSymbol(marker);
		origin.setHeight(flag.getOriginHeight());
		origin.setColour(flag.getColour());
	}
*/
/*	for(unsigned int pts=0;pts<flaPoNo;pts++)
	{
		vector<PaperPoint> line;
		const MFloat angle = setAngleY(fla->angle());
		line.push_back(PaperPoint(-markerHeight,0.) );
		line.push_back(PaperPoint(-length,0.) );
		const MFloat ratio = (coordRatioY_==0) ? 1. : coordRatioX_/coordRatioY_;
		for_each(line.begin(),line.end(),rotate(angle,ratio) );
		for_each(line.begin(),line.end(),translate(fla->point_) );
		MFloat len = fla->norm();

		MFloat lev1 = 1.25;  // 2.5
		MFloat lev2 = 3.75;  // 5.
		MFloat lev3 = 23.;  // 25.
		MFloat slev1 = 2.5;
		MFloat slev2 = 5.;
		MFloat slev3 = 25.;
		
		if(flag.getConvention()==KNOTS)
		{
			len *= 1.94384466;
			lev1 = 3.;
			lev2 = 7.;
			lev3 = 47.;
			slev1 = 5.;
			slev2 = 10.;
			slev3 = 50.;
		}

		if(markerHeight>0.) origin.push_back(PaperPoint(fla->point_) );

		const int old_currentColourIndex = currentLineStyle_;
		currentLineStyle_ = setLineParameters(style,thickness);
		renderPolyline2(line);
		currentLineStyle_ = old_currentColourIndex;
		
		MFloat tmp = 0.;
		int i = 0;
		const MFloat lengthY = setY(length * ratio);
		const MFloat multi =  setFlagY((flag.getHemisphere()==NORTH) ? (0.2*lengthY) : (-0.2*lengthY));
		bool fl=false;

		if(len<lev2) i++;
		while(len>(lev1))
		{
			if(len > lev3)      {tmp=1.0; len-= slev3;fl=true;}
			else if(len >= lev2) {tmp=1.0; len-= slev2;fl=false;}
			else                 {tmp=0.5; len-= slev1;fl=false;}

			const MFloat dx   = sin(RAD(30.)) * tmp * 0.2 * length; // angle of flags
			const MFloat step = i * (0.1*length);

			if(!fl)
			{
				line.clear();
				line.push_back(PaperPoint( -(length-step),0.) );
				line.push_back(PaperPoint( -(length-step+dx),multi*tmp) );

				for_each(line.begin(),line.end(),rotate(angle,ratio) );
				for_each(line.begin(),line.end(),translate(fla->point_) );
				renderPolyline2(line);
			}
			else
			{
				line.clear();
				line.push_back(PaperPoint( -(length-step)   ,0.) );
				line.push_back(PaperPoint( -(length-step),multi*tmp) );
				line.push_back(PaperPoint( -(length-step-(0.1*length)),0.) );
				for_each(line.begin(),line.end(),rotate(angle,ratio) );
				for_each(line.begin(),line.end(),translate(fla->point_) );
				renderSimplePolygon(line);
				fl=false;
				i++;
			}
			i++;
		}// end while
		++fla;
	}// end for
	//if(markerHeight>0.) renderSymbols(origin);
*/}


/*!
  \brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \note This can increase file and log file sizes if you run Magics++ in debug mode!

  \param s string to be printed
*/
MAGICS_NO_EXPORT void KMLDriver::debugOutput(const string &s) const
{
	if(debug_) pFile_ << "<!-- "<<s<<" -->\n";
}

/*!
  \brief class information are given to the output-stream
*/
void KMLDriver::print(ostream& out)  const
{
	out << "KMLDriver[";
	out << "]";
}

static SimpleObjectMaker<KMLDriver, BaseDriver> KML_driver("KML");
