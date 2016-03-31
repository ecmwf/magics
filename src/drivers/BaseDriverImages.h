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

using namespace magics;

/*!
  \brief Image render method for ALL drivers.

  This method should be used by all Magics++ drivers to render image objects.
*/
MAGICS_NO_EXPORT void BaseDriver::renderImage(const ImportObject& obj) const
{
#if CAIRO_VERSION > CAIRO_VERSION_ENCODE(1, 2, 0)
	if(!magCompare(obj.getFormat(),"png") )
#endif
	{
		MagLog::error() << "BaseDriver::renderImage - Only PNG format is support as image import!\n"
		                << "                           Please convert your image to PNG"<< endl;
		return;
	}	

	MFloat width=0;
	MFloat height=0;

    if(obj.getWidth()==-1)
	{
		cairo_surface_t *cimage = cairo_image_surface_create_from_png(obj.getPath().c_str());
		if(cimage)
		{
			width  = cairo_image_surface_get_width(cimage);
			height = cairo_image_surface_get_height(cimage);
			cairo_surface_destroy (cimage);
		}
		else MagLog::warning() << "BaseDriverImage-> Could NOT read the image file "<< obj.getPath() << " with Cairo!" << endl;
	}

cout << " ********* width/height: "<<width<<","<<height<< endl;
	const MFloat ow = (obj.getWidth() <0.) ? width  : convertCM(obj.getWidth() /coordRatioX_);
	const MFloat oh = (obj.getHeight()<0.) ? height : convertCM(obj.getHeight()/coordRatioY_);
cout << " ********* width/height: "<<ow<<","<<oh<< endl;
	const ImageProperties::OriginReference ori = obj.getOriginReference();
	if(ori == ImageProperties::centre)
		convertToPixmap(obj.getPath(),PNG, 300,
			projectX(obj.getOrigin().x()-(ow*.5)),
			projectY(obj.getOrigin().y()-(oh*.5)),
			projectX(obj.getOrigin().x()+(ow*.5)),
			projectY(obj.getOrigin().y()+(oh*.5)) );
	else
		convertToPixmap(obj.getPath(),PNG, 300,
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
cout << " >>> IMAGE >>> "<<fname<<"   xy: "<<wx0<<","<<wy0<<"   wh: "<<wx1<<","<<wy1<< endl;

	int Landscape = 0;
	unsigned char *image = 0;
	int status = 0;
	string pixmapFormat("rgb");

if(format==PNG)
	{
#if CAIRO_VERSION > CAIRO_VERSION_ENCODE(1, 2, 0)
		cairo_surface_t *cimage = cairo_image_surface_create_from_png(fname.c_str());

		if(!cimage) return 1;

		const int col = cairo_image_surface_get_width(cimage);
		const int row = cairo_image_surface_get_height(cimage);
		cairo_format_t cformat = cairo_image_surface_get_format(cimage);
		unsigned char *data = cairo_image_surface_get_data(cimage);
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

	    // const MFloat aspect = bx1/by1;
	    //if ( Landscape == 1 )		y1 = y0 + abs(x1-x0)*aspect;
	    //else if ( Landscape == 0 )	x1 = x0 + abs(y1-y0)*aspect;

	    bool alpha = (pixmapFormat == "rgba");
	    status = renderPixmap(wx0,wy0,wx1,wy1,col,row,image,Landscape,alpha);

	    if(!status) MagLog::warning() <<"BaseDriver::convertToPixmap() -> no Pixmap could be drawn! Zero size of at least one dimension."<< std::endl;
	    delete [] image;
#else
		MagLog::error() << "BaseDriverImage-> Could NOT read the image "<< fname << ": No Cairo support enabled!" << endl;
#endif
    }
	else 
	{
		MagLog::error() << "BaseDriver::convertToPixmap - only PNG format is supported for Image imports and symbols!\n" 
		                << "                               Please convert your image into PNG." << endl;
		return 1;
	}
	return status;
}
