/*Intel Corporation has released a number of extremely useful libraries and tools as part
of their performance suite.  These tools are designed to assist developers in taking
advantage of their advanced processors.

One excellent tool is the Intel Jpeg Library which provides a single small DLL which
will rapidly compress and decompress Jpeg images, a common graphics file format.

The following code snippet provides a static wrapper class for this library.  The 
only thing you need besides this code snippet is IJL.H, IJL.LIB, IJL.DLL which can
be found at the following URL.


http://www-cs.intel.com/support/performancetools/libraries/ijl/index.htm

When you see the quantity of code required just to compress or decompress a JPEG image
using IJL I think you will see the value of the following simplified wrapper layer.

John W. Ratcliff
jratcliff@verant.com
*/
//*** The JPEG.H wrapper layer header file.

#ifndef JPEG_H
#define JPEG_H

//############################################################################
//##                                                                        ##
//##  JPEG.H                                                                ##
//##                                                                        ##
//##  Wrapper class to compress or decompress a jpeg from a block of memory ##
//##  using the Intel Jpeg Library.                                         ##
//##  OpenSourced 2/4/2000 by John W. Ratcliff                              ##
//##                                                                        ##
//##  No warranty expressed or implied.  Released as part of the triangle   ##
//##  throughput testbed project.                                           ##
//############################################################################
//##                                                                        ##
//##  Contact John W. Ratcliff at jratcliff@verant.com                      ##
//############################################################################

class Jpeg
{
public:

  static bool  ReadFileParams(int &width,
						int &height,
						int &nchannels,
						char *name);

  static bool  ReadFile(int width,
						int height,
						int nchannels,
						char *name,
						unsigned char *pixbuff);

  static void *ReadImage(int &width,   // width of the image loaded.
                         int &height,  // height of the image loaded.
                         int &bpp,     // BYTES (not bits) PER PIXEL.
                         const void *buffer, // memory address containing jpeg compressed data.
                         int sizebytes); // size of jpeg compressed data.


  static void * Compress(const void *buffer, // address of image in memory
                         int width, // width of image in pixels
                         int height, // height of image in pixels.
                         int bpp, // *BYTES* per pixel of image 1 or 3
                         int &len, // returns length of compressed data
                         int quality=75); // image quality as a percentage

  static bool  WriteFile(const void *buffer, // address of image in memory
                         int width, // width of image in pixels
                         int height, // height of image in pixels.
                         int bpp, // *BYTES* per pixel of image 1 or 3
						 char *name,
                         int quality=75); // image quality as a percentage

  static bool Compress(const void *source,  // address of image in memory
					  unsigned char *dest,  // buffer to hold the compressed image
                      int width, // width of image in pixels
                      int height, // height of image in pixels
                      int bpp, // *BYTES* per pixel of image 1 or 3
                      unsigned long &len, // returns length of compressed data
                      int quality=75); // image quality as a percentage

  static bool ReadImage(int &width,   // width of the image loaded.
                         int &height,  // height of the image loaded.
                         int &bpp,     // BYTES (not bits) PER PIXEL.
                         const void *buffer, // memory address containing jpeg compressed data.
                         long sizebytes,   // size of jpeg compressed data.
   					     unsigned char* dest); // buffer to hold the uncompressed image

};

#endif

