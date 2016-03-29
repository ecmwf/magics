/******************************** LICENSE ********************************


  Copyright 2009 European Centre for Medium-Range Weather Forecasts (ECMWF)

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

/*! \file BaseDriverImages.h
    \brief Implementation of methods to display images of driver base class.

    Magics Team - ECMWF 2005

    Started: March 2005

    Changes:

*/

#ifdef MAGICS_RASTER
#include <gd.h>
#endif

using namespace magics;


static inline int getRealLine(ifstream& I, char* cline)
{
	int k = 0;
	char ch;
	while ( I.get(ch) )
	{
		cline[k] = ch;
		if ( ch == '\n' || ch == '\0' || I.eof() )
			break;
		k++;
	}
	cline[k] = '\0';
	return k;
}

static inline int getGoodLine(ifstream& I, char* cline,const int& n)
{
	int k = 0;
	char ch;
	while(I.get(ch))
	{
		cline[k] = ch;
		if ( ch == '\n' || ch == '\0' || I.eof() || k == n ) break;
		k++;
	}
	cline[k] = '\0';
	return k;
}

static inline int check_ps(const string& file, int& boxx, int& boxy, int& orientation)
{
	ifstream I(file.c_str());
	if (!I) return 1;

	string::size_type x,y;
	char *cline;
	cline = new char[2048];
	int k = getGoodLine(I,cline,2048);
	string s(cline,k);

	// level 3 only "%!PS-Adobe-3.0";
	x = s.find("%!PS");
	if(x == string::npos) return 1;  //no PostScript

	// Looks like a PostScript file
	int r = 0;
	boxx = boxy = 0;
	orientation = 0; // Portrait
	int boundingbox_found = 1;
	int orientation_found = 1;
	MFloat xx = 0.0,yy = 0.0;

	while(!I.eof()) // look for  %%BoundingBox: %%Orientation:
	{
		k = getGoodLine(I, cline,2048);
		int i = ( k > 15 ) ? 15 : k;
		if ( i < 15 )  // dont bother
			continue;
		string f(cline,i);

		if ( boundingbox_found )
		{
			x = f.find("%%BoundingBox:");
			if ( x != string::npos  )
			{
				string str(cline,k);
				y = str.find("atend");
				if ( y != string::npos )
					continue;
				else
				{
					str.erase(0,x+15);
					istringstream ist(str.c_str());

					MFloat dum1, dum2;
					ist >>dum1 >> dum2 >> xx >> yy;
					xx = xx - dum1;
					yy = yy - dum2;
					boundingbox_found = 0;
				}
			}
		}
		if ( orientation_found )
		{
			x = f.find("%%Orientation:");
			if ( x != string::npos )
			{
				string str(cline,k);
				y = str.find("atend");
				if ( y != string::npos )
					continue;
				else
				{
					y = str.find("Landscape");
					orientation = ( y != string::npos  ) ? 1 : 0;
					orientation_found = 0;
				}
			}
		}
		if ( (orientation_found + boundingbox_found) == 0 ) break;
	}

	boxx = (int) xx;  boxy = (int) yy;
	if ( boxx == 0 || boxy == 0 ) // No boundingbox
	{
		boxx = 595;
		boxy = 841;
	}

	I.close();
	delete [] cline;
	return r;
}

static inline int check_ppmHeader(ifstream& I, int& col, int& row)
{
	int colours;
	char *cline = new char[2048];

	int k = getRealLine(I, cline);
	string s = cline;
	int r = 0;
	int x = s.find("P6");

	if(x<0) r = 1;// not PPM P6 format

	if(!r)
	{
		cline[0] = '#';
		while ( cline[0] == '#')  // skip comments
			k = getRealLine(I,cline);
		istringstream ist1(cline);
		ist1 >> col >> row;
		k = getRealLine(I,cline);
		istringstream ist2(cline);
		ist2 >> colours;

		if ( col == 0 || row == 0 || colours == 0 ) r =  1;
	}

	delete [] cline;
	return r;
}


/*!
  \brief Image render method for ALL drivers.

  This method should be used by all Magics++ drivers to render image objects.
*/
MAGICS_NO_EXPORT void BaseDriver::renderImage(const ImportObject& obj) const
{
	std::string f = obj.getFormat();
	GraphicsFormat format = PNG;
	if(magCompare(f,"ps")) format = PS;
	else if(magCompare(f,"eps")) format = EPS;
	else if(magCompare(f,"gif")) format = GIF;
	else if(magCompare(f,"jpeg") || magCompare(f,"jpg")) format = JPG;
	else if(magCompare(f,"png")) format = PNG;
	else if(magCompare(f,"svg")) format = SVG;

	MFloat width=0;
	MFloat height=0;

	if(obj.getWidth()==-1 && ( magCompare(f,"gif") || magCompare(f,"jpeg")|| magCompare(f,"jpg") ) )
	{
#ifndef MAGICS_RASTER
		MagLog::error() << "BaseDriverImages: Dimension is -1 and default size can not be determined (GD library required)!" << endl;
		return;
#else
		FILE *in = fopen(obj.getPath().c_str(), "rb");
		gdImagePtr image = 0;
		if(magCompare(f,"png")) image = gdImageCreateFromPng(in);
		else if(magCompare(f,"jpeg") || magCompare(f,"jpg")) image = gdImageCreateFromJpeg(in);
		else if(magCompare(f,"gif"))
		{
#ifdef MAGICS_GIF
			image = gdImageCreateFromGif(in);
#else
			MagLog::error() << "GIF import is not supported in this version! You need a GIF enabled GD library." << endl;
			return;
#endif
		}
		fclose(in);
		width  = gdImageSX(image);
		height = gdImageSY(image);
		gdImageDestroy(image);
#endif
	}
	else if(obj.getWidth()==-1 && magCompare(f,"png") )
	{
#ifndef HAVE_CAIRO
		MagLog::error() << "BaseDriverImages: Dimension is -1 and default size can not be determined for PNG (Cairo library required)!" << endl;
		return;
#else
		cairo_surface_t *cimage = cairo_image_surface_create_from_png(obj.getPath().c_str());
		if(cimage)
		{
			width  = cairo_image_surface_get_width(cimage);
			height = cairo_image_surface_get_height(cimage);
			cairo_surface_destroy (cimage);
		}
		else MagLog::warning() << "BaseDriverImage-> Could NOT read the image file "<< obj.getPath() << " with Cairo!" << endl;
#endif
	}	
	else
	{
		width  = obj.getWidth();
		height = obj.getHeight();
	}
	const MFloat ow = (obj.getWidth()<0)  ? convertCM(1./coordRatioX_) : width;
	const MFloat oh = (obj.getHeight()<0) ? convertCM(1./coordRatioY_) : height;

	const ImageProperties::OriginReference ori = obj.getOriginReference();
	if(ori == ImageProperties::centre)
		convertToPixmap(obj.getPath(),format, 300,
			projectX(obj.getOrigin().x()-(ow*.5)),
			projectY(obj.getOrigin().y()-(oh*.5)),
			projectX(obj.getOrigin().x()+(ow*.5)),
			projectY(obj.getOrigin().y()+(oh*.5)) );
	else
		convertToPixmap(obj.getPath(),format, 300,
			projectX(obj.getOrigin().x()),
			projectY(obj.getOrigin().y()),
			projectX(obj.getOrigin().x()+ow),
			projectY(obj.getOrigin().y()+oh) );
}//end BaseDriver::renderImage()


/*!
  \brief converting object to pixmap

  This method should be used by all Magics++ drivers

*/
MAGICS_NO_EXPORT bool BaseDriver::convertToPixmap(const string &fname, const GraphicsFormat format, const int reso,
		     const MFloat wx0, const MFloat wy0,const MFloat wx1,const MFloat wy1) const
{
	debugOutput("Start Image conversion");
	//cout << " >>> IMAGE >>> "<<fname<<"   xy: "<<wx0<<","<<wy0<< endl;

	int Landscape = 0;
	MFloat bx1=100.;
	MFloat by1=100.;
	unsigned char *image = 0;
	int col=0,row=0;
	int status = 0;
	string s2("");
	string pixmapFormat("rgb");

	if(format==PS || format==EPS) //File is PostScript
	{
		int x1 = 0;
		int y1 = 0;

		if(check_ps(fname,x1,y1,Landscape) )
		{
			MagLog::error() << "BaseDriverImages: Open of source PostScript file failed!" << endl;
			return false;
       	}

		s2 = getTmpName()+".ppm";
		if(s2=="")
		{
			MagLog::error() << "BaseDriverImages: Open of temp file failed!" << endl;
			return false;
		}

		const MFloat Xres = MFloat(reso);
		bx1 = MFloat(x1)/72.*Xres + 0.5;
		x1  = (int) bx1;
		by1 = MFloat(y1)/72.*Xres + 0.5;
		y1  = (int) by1;

		char boxx[5];
		char boxy[5];
		char boxz[5];
		sprintf(boxx,"%d",x1);
		sprintf(boxy,"%d",y1);
		sprintf(boxz,"%d",reso);

		string cmd = "( gs -q -dNOPAUSE -dSAFER -sDEVICE=ppmraw -sOutputFile=" + s2 +
			" -dGraphicsAlphaBits=4 -dTextAlphaBits=4 -dCOLORSCREEN -dBATCH -g" +
			boxx + "x" + boxy + " -r" + boxz + " " + fname + " < /dev/null )";

		status = system(cmd.c_str());

		if(status)
		{
			MagLog::error() << "BaseDriverImages: Command exit Not zero" << endl;
			return false;
		}
		ifstream I(s2.c_str());
		if(!I)
		{
			MagLog::error() << "BaseDriverImages: Incorrect PostScript Format!" << endl;
			return false;
		}
		if(check_ppmHeader(I,col,row) )
		{
			return 1;
		}

		image = new unsigned char [row*col*3];
		I.read((char*)image,row*col*3);
		I.close();
		remove(s2.c_str());
	}
	else if(format==PNG)
	{
#ifdef HAVE_CAIRO
		cairo_surface_t *cimage = cairo_image_surface_create_from_png(fname.c_str());

		if(cimage)
		{
			col = cairo_image_surface_get_width(cimage);
			row = cairo_image_surface_get_height(cimage);
			cairo_format_t cformat = cairo_image_surface_get_format(cimage);
			unsigned char *data = cairo_image_surface_get_data(cimage);
			bx1 = (MFloat) col;
			by1 = (MFloat) row;
			Landscape = 0;

			if(cformat == CAIRO_FORMAT_RGB24)
			{
				image = new unsigned char [row*col*3];
				unsigned char *p = image;
				for (int i = 0; i<row; i++)
				  for (int j = 0; j<col*4;)
				  {
					*(p++) = (unsigned char) data[(i*col*4)+j+2]; //r
					*(p++) = (unsigned char) data[(i*col*4)+j+1]; //g
					*(p++) = (unsigned char) data[(i*col*4)+j+0]; //b
					j+=4;
				  }
			}
			else if(cformat == CAIRO_FORMAT_ARGB32)
			{
				pixmapFormat="rgba";
				image = new unsigned char [row*col*4];
				unsigned char *p = image;
				for (int i = 0; i<row; i++)
				  for (int j = 0; j<col*4;)
				  {
					*(p++) = (unsigned char) data[(i*col*4)+j+2]; //r
					*(p++) = (unsigned char) data[(i*col*4)+j+1]; //g
					*(p++) = (unsigned char) data[(i*col*4)+j+0]; //b
					*(p++) = (unsigned char) 255-2*data[(i*col*4)+j+3]; //a
					j+=4;
				  }
			}
			else
			{
				MagLog::error() << "BaseDriverImage-> PNG format type ("<< cformat << ") not supported!" << endl;
				cairo_surface_destroy (cimage);
				return false;
			}
			cairo_surface_destroy (cimage);
		}
		else MagLog::warning() << "BaseDriverImage-> Could NOT read the image "<< fname << " with Cairo!" << endl;
#else
		MagLog::warning() << "BaseDriverImage-> Could NOT read the image "<< fname << ": No Cairo support!" << endl;
#endif
    }
#ifdef MAGICS_RASTER
	else if(format==GIF || format==JPG)
	{
		// convert png to ppm(raw) like image
		FILE* fd3 = fopen(fname.c_str(),"rb");
		if(!fd3)
		{
			MagLog::error() << "BaseDriverImages: Open failed for raster source > "<< fname << endl;
			return false;
		}
		gdImagePtr imp = 0;
		if(format==JPG)      imp = gdImageCreateFromJpeg(fd3);
		else if(format==PNG) imp = gdImageCreateFromPng(fd3);
		else if(format==GIF)
		{
#ifdef MAGICS_GIF
			imp = gdImageCreateFromGif(fd3);
#else
			MagLog::error() << "GIF pixmap import is not supported in this version! You need a GIF enabled GD library." << endl;
			return false;
#endif
		}
		fclose(fd3);
		if(!imp)
		{
			MagLog::error() << "BaseDriverImages: Incorrect raster file format (can not be handled by GD) !" << endl;
			return false;
		}

		col = gdImageSX(imp);
		row = gdImageSY(imp);
		bx1 = (MFloat) col;
		by1 = (MFloat) row;
		Landscape = 0;

		if(format == PNG && // imp->transparent == 1 &&
                   alphaEnabled_ == true)
		{
			pixmapFormat="rgba";

			image = new unsigned char [col*row*4];
			unsigned char *p = image;
			int i,j;
			for (i = 0; i<row; i++)
			for (j = 0; j<col; j++)
			{
				int c = gdImageGetPixel(imp,j,i);
				int r = gdImageRed(imp,c);
				int g = gdImageGreen(imp,c);
				int b = gdImageBlue(imp,c);
				int a = gdImageAlpha(imp,c);
				*(p++) = (unsigned char) r;
				*(p++) = (unsigned char) g;
				*(p++) = (unsigned char) b;
				*(p++) = (unsigned char) 255-2*a;
			}
		}
		else
		{
			image = new unsigned char [col*row*3];
			unsigned char *p = image;
			int i,j;
			for (i = 0; i<row; i++)
			for (j = 0; j<col; j++)
			{
				int c = gdImageGetPixel(imp,j,i);
				int r = gdImageRed(imp,c);
				int g = gdImageGreen(imp,c);
				int b = gdImageBlue(imp,c);
				*(p++) = (unsigned char) r;
				*(p++) = (unsigned char) g;
				*(p++) = (unsigned char) b;
			}
		}
		gdImageDestroy(imp);
	}
#endif
	else 
	{
		MagLog::warning() << "BaseDriverImages: graphics formats ("<<format<<") is NOT supported!" << endl;
		return 1;//No PPM
	}

	MFloat x0 = wx0; //Left
	MFloat x1 = wx1; //Right
	MFloat y0 = wy0;
	MFloat y1 = wy1;

	// const MFloat aspect = bx1/by1;
	//if ( Landscape == 1 )		y1 = y0 + abs(x1-x0)*aspect;
	//else if ( Landscape == 0 )	x1 = x0 + abs(y1-y0)*aspect;

	bool alpha = (pixmapFormat == "rgba");
	status = renderPixmap(x0,y0,x1,y1,col,row,image,Landscape,alpha);

	if(!status) MagLog::warning() <<"BaseDriver::convertToPixmap() -> no Pixmap could be drawn! Zero size of at least one dimension."<< std::endl;
	delete [] image;
	return status;
}
