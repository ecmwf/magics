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

/*! \file CairoDriverRaster.cc
    \brief Implementation of raster output using CairoDriver.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon Jul 12 2010

*/
 
 
#include <cstdio>
#include <cstdlib>

/* ####################################################################

           P N G

   #################################################################### */

#include "png.h"
/* 8 bits red, green and blue channel */
#define GETRED(col)    (((col) >> 0) & 0xFF)
#define GETGREEN(col)  (((col) >> 8) & 0xFF)
#define GETBLUE(col)   (((col) >> 16) & 0xFF)
#define GETALPHA(col)  (((col) >> 24) & 0xFF)


int write_png(cairo_surface_t *s,const char *name)
{
	// read cairo surface
	unsigned char * data = cairo_image_surface_get_data(s);
	const int width  = cairo_image_surface_get_width(s);
	const int height = cairo_image_surface_get_height(s);
	const int stride = cairo_image_surface_get_stride(s);
	
	cout << "PNG256> " <<width<<"x"<<height <<" @ "<<stride << endl;

unsigned int transparent = 255;

    set<unsigned int> palette;
    png_color pngpalette[256];
    png_byte trans[256];
    png_color_16 trans_values[1];
    int i, ncols;

    FILE *fp = fopen(name,"wb");
    if (fp == NULL) return 1;
    
    png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL)
    {
	fclose(fp);
	return 1;
    }
    
    // Allocate & initialize the image information data
    png_infop info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL)
    {
	png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
	fclose(fp);
	return 1;
    }

    // Set error handling.
#ifndef _AIX
    if (setjmp(png_ptr->jmpbuf))
    {
	png_destroy_write_struct(&png_ptr, &info_ptr);
	fclose(fp);
	return 1;
    }
#endif

    // I/O initialization functions
    png_init_io(png_ptr, fp);

    // Have we less than 256 different colors?
    ncols = 0;
//    if(transparent) palette[ncols++] = transparent & 0xFFFFFF;
    int mid = ncols;
    int high=0, low=0;
    bool have_alpha  = true;
    unsigned int col=0;
    unsigned int pal[256];
	unsigned int *data_int = (unsigned int*) data;

	for (i = 0; (i < height); i++)
	{
		for (int j = 0; (j < width); j++)
		{
		    col = data_int[i*stride+j];
		    palette.insert(col);
		    if (col < 255) cout <<col<<endl;
		}
	}
	
	bool withpalette = (palette.size()<256) ? true : false; 

	if(withpalette)
	{
		have_alpha &= (transparent == 0);

		png_set_IHDR(png_ptr, info_ptr, width, height, 8,
			PNG_COLOR_TYPE_PALETTE,
			 PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,
			 PNG_FILTER_TYPE_BASE);
		int u=0;
		for( set<unsigned int>::const_iterator iter = palette.begin();
		        iter != palette.end(); ++iter )
		{
			pal[u]=*iter;
			u++;
			cout <<u<<" - "<<*iter<< endl;
		}
		std::sort(pal, pal + u*sizeof(unsigned int));

		for(i=0;i<u;i++)
		{
			// PNG needs NON-premultiplied alpha
			unsigned int c = pal[i];
			int a = GETALPHA(c);
			trans[i] = a;
			if(a == 255 || a == 0)
			{
			    pngpalette[i].red   = GETRED(c);
			    pngpalette[i].green = GETGREEN(c);
			    pngpalette[i].blue  = GETBLUE(c);
			}
			else
			{
			    pngpalette[i].red   = 0.49 + 255.0*GETRED(c)/a;
			    pngpalette[i].green = 0.49 + 255.0*GETGREEN(c)/a;
			    pngpalette[i].blue  = 0.49 + 255.0*GETBLUE(c)/a;
			}
			i++;
		}

		png_set_PLTE(png_ptr, info_ptr, pngpalette, i);
		if (transparent || have_alpha)
		    png_set_tRNS(png_ptr, info_ptr, trans, i, trans_values);
	} // end if palette
	else
	{
		have_alpha &= (transparent == 0);

		/*! Set the image information here.
		  - Width and height are up to 2^31,
		  - bit_depth is one of 1, 2, 4, 8, or 16 - valid values depend on color_type
		  - color_type: PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB or PNG_COLOR_TYPE_RGB_ALPHA
		  - interlace is either PNG_INTERLACE_NONE or PNG_INTERLACE_ADAM7
		  - compression_type and filter_type MUST currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE.
		*/
		png_set_IHDR(png_ptr, info_ptr, width, height, 8,
			PNG_COLOR_TYPE_RGB_ALPHA,
			PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,
			PNG_FILTER_TYPE_BASE);
	/*
		// Deal with transparency
		if(transparent)
		{
			trans_values[0].red = GETRED(transparent);
			trans_values[0].blue = GETBLUE(transparent);
			trans_values[0].green = GETGREEN(transparent);
			png_set_tRNS(png_ptr, info_ptr, trans, ncols, trans_values);
		}
	*/
	}
    
	const int dpi=96;
	png_set_pHYs(png_ptr, info_ptr, dpi/0.0254, dpi/0.0254, PNG_RESOLUTION_METER);
	png_write_info(png_ptr, info_ptr); // Write the file header info

    //
    // Write the data
    //
    png_bytep pscanline, scanline = (png_bytep) calloc(stride,sizeof(png_byte));
    if (scanline == NULL) return 1;

    for (i = 0 ; i < height ; i++)
    {
	pscanline = scanline;
	for (int j = 0 ; j < stride ; j++)
	{
	    col = data_int[i*stride+j];

	    if (withpalette)
	    {
if(j==0) cout << i << " "<<ncols<<"      "<<col<< endl;
		// binary search the palette (the colour must be there)
		low = 0;  high = ncols - 1;
		while (low <= high)
		{
		    mid = (low + high)/2;
		    if      (col < pal[mid]) high = mid - 1;
		    else if (col > pal[mid]) low  = mid + 1;
		    else break;
		}
		*pscanline++ = (char)mid;
	    } 
	    else
	    {
// 		if(have_alpha)
// 		{
// 		    // PNG needs NON-premultiplied alpha
// 		    char a = col;
// 		    if(a == 255 || a == 0)
// 		    {
// 			*pscanline++ = GETRED(col) ;
// 			*pscanline++ = GETGREEN(col) ;
// 			*pscanline++ = GETBLUE(col) ;
// 			*pscanline++ =  a;
// 		    } else {
// 			*pscanline++ = 0.49 + 255.0*GETRED(col)/a ;
// 			*pscanline++ = 0.49 + 255.0*GETGREEN(col)/a ;
// 			*pscanline++ = 0.49 + 255.0*GETBLUE(col)/a ;
// 			*pscanline++ =  a;
// 		    }
// 		} else {
// 		    *pscanline++ = GETRED(col) ;
// 		    *pscanline++ = GETGREEN(col) ;
// 		    *pscanline++ = GETBLUE(col) ;
// 		}
//	    }

			png_byte tmp = (png_byte) data[i*stride+j++];
			*pscanline++ = (png_byte) data[i*stride+j++];
			*pscanline++ = (png_byte) data[i*stride+j++];
			*pscanline++ = (png_byte) data[i*stride+j];
			*pscanline++ = tmp;
		}
		}
		png_write_row(png_ptr, scanline);
	}

	png_write_end(png_ptr, info_ptr);   
	free(scanline);
	png_destroy_write_struct(&png_ptr, &info_ptr);

	fclose(fp);
	return 0;
}

/* ####################################################################

           J P E G

   #################################################################### */

/*
#include <jpeglib.h>

void argb32_to_rgb24(unsigned char *in, unsigned char *out, int n)
{
	unsigned char *in_d = in;
	while (n--) {
		unsigned char d = *in_d;
		out[0] = d >> 16 & 0xff;
		out[1] = d >> 8 & 0xff;
		out[2] = d >> 0 & 0xff;
		in_d++;
		out+=3;
	}
}

void write_jpeg(cairo_surface_t *s,const char *name, J_COLOR_SPACE in_color_space, J_COLOR_SPACE color_space, int components, boolean write_Adobe_marker, boolean write_JFIF_header)
{
	// read cairo surface
	unsigned char *data = cairo_image_surface_get_data(s);
	const int width  = cairo_image_surface_get_width(s);
	const int height = cairo_image_surface_get_height(s);
	const int stride = cairo_image_surface_get_stride(s);
	
	cout << "JPEG> " <<width<<"x"<<height <<" @ "<<stride << endl;

	struct jpeg_compress_struct cinfo;
	struct jpeg_error_mgr jerr;

	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);
	FILE *outfile = fopen(name, "wb");
	jpeg_stdio_dest(&cinfo, outfile);
	 cinfo.image_width  = width;
	 cinfo.image_height = height;
	 cinfo.input_components = components;
	 cinfo.in_color_space = in_color_space;

	jpeg_set_defaults(&cinfo);
	jpeg_set_colorspace(&cinfo, color_space);
	cinfo.write_Adobe_marker = write_Adobe_marker;
	cinfo.write_JFIF_header  = write_JFIF_header;
	jpeg_set_quality(&cinfo, 50, TRUE);
	jpeg_start_compress(&cinfo, TRUE);

	int ppb = components;
	unsigned char *rgb24_buffer = (unsigned char *) malloc(width * ppb);

	while (cinfo.next_scanline < cinfo.image_height)
	{
		unsigned char *row_pointer[1];
		if (components == 3)
		{
			argb32_to_rgb24(&data[cinfo.next_scanline * stride], rgb24_buffer, width);
			row_pointer[0] = rgb24_buffer;
		}
		else
		{
			row_pointer[0] = &data[cinfo.next_scanline * stride];
		}
		jpeg_write_scanlines(&cinfo, row_pointer, 1);
	}
	jpeg_finish_compress(&cinfo);
	fclose(outfile);
	jpeg_destroy_compress(&cinfo);
}
*/

/*
int main()
{
	write_jpeg("out-rgb-rgb-adobe.jpg", JCS_RGB, JCS_RGB, 3, true, false);
	write_jpeg("out-rgb-rgb.jpg", JCS_RGB, JCS_RGB, 3, false, false);
	write_jpeg("out-rgb-yuv-jfif.jpg", JCS_RGB, JCS_YCbCr, 3, false, true);
	write_jpeg("out-rgb-yuv.jpg", JCS_RGB, JCS_YCbCr, 3, false, false);
	// unsupported color space transformation 
	//write_jpeg("out-yuv-rgb-adobe.jpg", JCS_YCbCr, JCS_RGB, 3, true, false);
	//write_jpeg("out-yuv-rgb.jpg", JCS_YCbCr, JCS_RGB, 3, false, false);
	write_jpeg("out-yuv-yuv-jfif.jpg", JCS_YCbCr, JCS_YCbCr, 3, false, true);
	write_jpeg("out-yuv-yuv.jpg", JCS_YCbCr, JCS_YCbCr, 3, false, false);
	write_jpeg("out-cmyk-cmyk-adobe.jpg", JCS_CMYK, JCS_CMYK, 4, true, false);
	write_jpeg("out-cmyk-cmyk.jpg", JCS_CMYK, JCS_CMYK, 4, false, false);
	write_jpeg("out-ycck-ycck-adobe.jpg", JCS_YCCK, JCS_YCCK, 4, true, false);
	write_jpeg("out-ycck-ycck.jpg", JCS_YCCK, JCS_YCCK, 4, false, false);
	return 0;
}
*/
