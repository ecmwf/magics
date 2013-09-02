/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeRasterParams.h
    \brief This file contains definitions about parameters of a raster structure
*/
#ifndef  __TERRALIB_INTERNAL_RASTERPARAMS_H
#define  __TERRALIB_INTERNAL_RASTERPARAMS_H

#include "TeProjection.h"
#include "TeBox.h"
#include "TeDataTypes.h"

#include <string>
#include <list>
#include <vector>

using namespace std;

class TeDatabase;

//!  A class to handle the parameters set associated with a raster data
/*! 
This class is a data structure to store and manipulate a set of parameters and definitions that characterizes 
a raster data. It contains parameters relative to storaging, geographical or interpretation aspects.
Not all parameters are valid, or make sense, for all raster data.
Most parameters are public and can be accessed directly. The private ones should be acessed
through the methods provided.
\sa TeRaster
*/
class TL_DLL TeRasterParams
{
public:

	//! Pixels interleaving mode: used by rasters in memory and files in raw formats
	enum TeRasterInterLeavingMode
	{	
		TePerPixel, // BIP - Band Interleaved by Pixel
		TePerLine,	// BIL - Band Interleaved by Line
		TePerBand	// BSQ - Band Sequential
	};
	
	//! Photometric interpretation of a raster data
	enum TeRasterPhotometricInterpretation
	{ TeUnknown, TePallete, TeMultiBand, TeRGB };
	
	//! Status of a raster data, in terms of reading and writting possibilities
	enum TeRasterStatus
	{ TeNotReady, TeReadyToRead, TeReadyToWrite };

	//! Compression modes of a raster data 
	enum TeRasterCompressionMode
	{ TeNoCompression, TeZLib, TeJpeg, TeTiffCompression };
	
	//! Tiling type modes to store raster data in a TerraLib database 
	enum TeRasterTilingType
	{  TeExpansible=1, TeNoExpansible=2 };
	

	//! Default contructor
	TeRasterParams () :
		nlines_(0),
		ncols_(0),
		resx_(1),
		resy_(1),
		swap_(false),
		useDummy_(false),
		mode_('r'),
		status_(TeRasterParams::TeNotReady),
		resolution_(1),
		subBand_(0),
		offset_(0),
		tiling_type_(TeRasterParams::TeExpansible),
		blockHeight_(0),
		blockWidth_(0),
		nTilesInMemory_(0),
		blockId_(""),	
		decoderIdentifier_(""),	
		database_ (0),
		layerId_(-1),
		objectId_(""),
		interleaving_(TeRasterParams::TePerPixel),
		lutName_(""),
		dxJ_(0.0),
		dxI_(0.0),
		dyJ_(0.0),
		dyI_(0.0),
		x0_(0.0),
		y0_(0.0),
		box_(TeBox()),
		projection_(0)
	{
		nbands_= 1;
		nbitsperPixel_.resize(nbands_);
		vmin_.resize(nbands_);
		vmax_.resize(nbands_);
		bands_.resize(nbands_);
		photometric_.resize(nbands_);
		dataType_.resize(nbands_);
		dummy_.resize(nbands_);
		compression_.resize(nbands_);
		for (int i=0; i<nbands_; i++)
		{
			bands_[i] = i;
			vmin_[i] = TeMAXFLOAT;
			vmax_[i] = -TeMAXFLOAT;
			dataType_[i] = TeUNSIGNEDCHAR;
			photometric_[i] = TeRasterParams::TeMultiBand;
			dummy_[i] = 255;
		}
		hasSetDummy_ = false;
	}

	//! Copy constructor
	TeRasterParams(const TeRasterParams& other);

	//! Operator=
	TeRasterParams& operator=(const TeRasterParams& rhs);

	//! Destructor
	virtual ~TeRasterParams()
	{
		bands_.clear();
		vmin_.clear();
		vmax_.clear();
		if (!lutr_.empty())
		{
			lutr_.clear();
			lutg_.clear();
			lutb_.clear();
		}
		photometric_.clear();
		dataType_.clear();
		dummy_.clear();
		compression_.clear();
		if (projection_)
			delete projection_;
		hasSetDummy_ = false;
	}
	
	/** @name Extension
	*  Methods to deal with the spatial extensions of a raster data
	*/
	//@{	
	//! Returns the box based on the coordinates of center of the corner elements
	const TeBox& box()
	{ return box_; }

	//! Returns the outline box 
	TeBox boundingBox();

	//! Sets extension parameters
	/*
		This methods recalculates the number of lines and columns and box accordingly to 
		an input bounding box and X and Y resolutions
		\param x1 bounding box lower left point X coordinate 
		\param y1 bounding box lower left point Y coordinate 
		\param x2 bounding box upper right point X coordinate
		\param y2 bounding box upper right point Y coordinate
		\param resx X resolution
		\param resy Y resolution
		\param lock corner of the bounding box that should be preserved
	*/
	void boundingBoxResolution (double x1, double y1, double x2, double y2, double resx, double resy, TeBox::TeBoxCorner lock = TeBox::TeUPPERLEFT);

	//! Sets extension parameters
	/*
		This methods recalculates the resolutions and box accordingly to 
		a given bounding box and number of lines and columns
		\param x1 bounding box lower left point X coordinate 
		\param y1 bounding box lower left point Y coordinate 
		\param x2 bounding box upper right point X coordinate
		\param y2 bounding box upper right point Y coordinate
		\param nlines  number of lines
		\param ncols number of columns
		\param lock corner of the bounding box that should be preserved
	*/
	void boundingBoxLinesColumns (double x1, double y1, double x2, double y2, 
		                          int nlines, int ncols, TeBox::TeBoxCorner lock = TeBox::TeUPPERLEFT);

	//! Sets extension parameters
	/*
		This methods recalculates the number of lines and columns and box accordingly to 
		a given box and X and Y resolutions
		\param x1 box lower left point X coordinate 
		\param y1 box lower left point Y coordinate 
		\param x2 box upper right point X coordinate
		\param y2 box upper right point Y coordinate
		\param resx X resolution
		\param resy Y resolution
		\param lock corner of the bounding box that should be preserved
	*/
	void boxResolution (double x1, double y1, double x2, double y2, 
		                double resx, double resy, TeBox::TeBoxCorner lock = TeBox::TeUPPERLEFT);

	//! Sets extension parameters
	/*
		This methods recalculates the resolutions and box accordingly to 
		a given box and number of lines and columns
		\param x1 box lower left point X coordinate 
		\param y1 box lower left point Y coordinate 
		\param x2 box upper right point X coordinate
		\param y2 box upper right point Y coordinate
		\param nlines  number of lines
		\param ncols number of columns
		\par Calculates X resolution and Y resolution 
		\param lock corner of the bounding box that should be preserved
	*/
	void boxLinesColumns (double x1, double y1, double x2, double y2, 
		                  int nlines, int ncols, TeBox::TeBoxCorner lock = TeBox::TeUPPERLEFT);

	//! Sets extension parameters
	/*
		\param left lower left point X coordinate 
		\param top  upper right point Y coordinate
		\param resx X resolution
		\param resy Y resolution
		\param nlines  number of lines
		\param ncols number of columns
		\param coordIsCentrePixel flag to indicate that top left coordinate is centre of pixel
	*/
	void topLeftResolutionSize (double left, double top, double resx, double resy, int ncol, int nlin, bool coordIsCentrePixel=true);
		
	//! Sets extension parameters
	/*
		\param left lower left point X coordinate 
		\param lower lower left point Y coordinate
		\param resx X resolution
		\param resy Y resolution
		\param nlines  number of lines
		\param ncols number of columns
		\param coordIsCentrePixel flag to indicate that top left coordinate is centre of pixel
	*/
	void lowerLeftResolutionSize (double left, double lower, double resx, double resy, int ncol, int nlin, bool coordIsCentrePixel=true);

	//! Sets default extension parameters
	/* 
		Sets a valid bouding box, considering a resolution of 1x1 and
		that the coordinate of the center of lower-left element is (0.5,0.5)
		\param nlines  number of lines
		\param ncols number of columns
	*/
	void setNLinesNColumns(int nlines, int ncolumns);

	//! Resizes the raster bounding box to cut it exactly in tiles of a give size
	/*! 
		\param bb an initial bounding box
		\param bWidth block width (in number of elements/pixels)
		\param bHeight block height (in number of elements/pixels)
		\note keeps the defined X an Y resolutions
	*/ 
	void resizeToTiling(TeBox& bb, int bWidth, int bHeight);
	//@}

	//! Sets the number of bands, or dimentions in a raster data
	/*
		\param n number of bands
		\note If the required number of bands is greater than the 
		current number of bands, the last band characteristics 
		will be copied to the new defined bands.
	*/
	void nBands(int n);

	//! Sets the projection
	/*
		\param proj a pointer to a terralib projection instance
		A new instance of projection is created according to the parameters described by proj.
	*/
	void projection(TeProjection* proj);

	//! Returns the projection
	TeProjection* projection()
	{	return projection_; }
	

	//! Transform a coordinate from world domain to line/column domain
	TeCoord2D coord2Index (const TeCoord2D& pt) const;

	//! Transform a coordinate from line/column domain to world domain
	TeCoord2D index2Coord (const TeCoord2D& pt) const;


	int nlines_;			//!< number of lines
	int ncols_;			//!< number of columns

	double	resx_,			//!< horizontal resolution
		resy_;			//!< vertical resolution	

	bool swap_;			//!< a flag to indicate that the values of the elements of the raster are swapped
	bool useDummy_;			//!< a flag to indicate that raster has dummy values
	char	mode_;			//!< a character indicating the access mode to the raster data: 'r', 'w' or 'c'
	TeRasterStatus 	status_;	//! Status for reading and writing to raster	
	
	/** @name Parameters variable per band
	*/
	//@{	
	vector<int>	nbitsperPixel_;		//!< number of bits per pixel 
	vector<double>	vmin_;			//!< minimum value 
	vector<double>	vmax_;			//!< maximum value 
	vector<int>	bands_;			//!< bands information 
	vector<double>	dummy_;			//!< no data value
	vector<TeRasterPhotometricInterpretation>   photometric_;	//!< photometric interpretation
	vector<TeRasterCompressionMode>	compression_;	//!< compression type 
	vector<TeDataType> 		dataType_;	//!< computational size of elements

	//! Sets the size of the elements in a particular or in every band
	/*
		\param type the computational data type
		\param band number of the band. Default: all bands 
	*/
	void setDataType(TeDataType type, int band=-1);

	//! Returns the size in bytes of each raster element
	int elementSize(int band=0);

	//! Sets the photometric type in a particular or in every band
	/*
		\param photom the photometric interpretation
		\param band number of the band. Default: all bands 
	*/
	void setPhotometric(TeRasterPhotometricInterpretation photom, int band=-1);

	//! Sets the compression mode of each band
	/*
		\param cmode the compression mode
		\param band number of the band. Default: all bands 
	*/
	void setCompressionMode(TeRasterCompressionMode cmode, int band=-1);

	//! Sets the dummy value in each band
	/*
		\param dummy dummy value
		\param band number of the band. Default: all bands 
	*/
	void setDummy(double dummy, int band=-1);
	//@}

	/** @name Resolution level parameters
	* Used when raster has different levels of resolution
	*/
	//@{
	int resolution_;		//! resolution level 
	int subBand_;			//! sub band identification 
	//@}

	/** @name File parameters
	* Used when raster is stored in files
	*/
	//@{	
	string	fileName_;		//!< name of a raster file
	int offset_;			//!< offset 
	//@}
		
	/** @name Tilling parameters
	*/
	//@{
	TeRasterTilingType	tiling_type_; //!< raster tiling mode 
	int blockHeight_;		//!< tiles height 
	int blockWidth_;		//!< tiles width 
	int nTilesInMemory_;		//!< number of tiles to be kept in memory
	string blockId_;		//!< tiles identification  
	//@}

	string	decoderIdentifier_;	//!< decoder associated to his raster
	
	//! Returns the identifier of the decoder associated to the raster
	const string& decName() const 
	{ return decoderIdentifier_; }
	//set  the identifier of the decoder associated to the raster
	void  decName(const string& decoder)  
	{ decoderIdentifier_ = decoder; }
		
		
	/** @name Tilling parameters
	*/
	//@{
	TeDatabase*	database_;	//!< pointer to a TerraLib database where the raster is stored
	int 		layerId_;	//!< identification of the layer that contains the raster
	string 		objectId_;	//!< identification of an object associated to this raster geometry
	//@}

	TeRasterInterLeavingMode interleaving_;	//!< interleaving mode

	/** @name Raster pallete
	*  Look up table associated to a raster 
	*/
	//@{ 
	string	lutName_;		//!< name of a lut table associated to this raster
	vector<unsigned short> lutr_;  	//!< red pallete
	vector<unsigned short> lutg_;  	//!< green pallete
	vector<unsigned short> lutb_;  	//!< blue pallete

	//! Returns the name of the lut 
	const string& lutName() 
	{ return lutName_; }
	//@}

	/** @name Navigation parameters
	*  Parameters associated to translation/rotation of the positioning of the raster data
	*/
	//@{ 
	double dxJ_;	//!< X offset due to increase of one column position		       
	double dxI_;	//!< X offset due to increase of one line position		       
	double dyJ_;	//!< Y offset due to increase of one column position		       
	double dyI_;	//!< Y offset due to increase of one line position		       
	double x0_;		//!< X coordinate of the upper left raster element
	double y0_;		//!< Y coordinate of the upper left raster element

	/** Sets the navigation parameters
		\param nwf the navigation parameters set
		\note this method should be called after the number of lines and columns has been set

		\verbatim
		The Navigation parameters describe a transformation from column/line domain (i,j)
        to geographical world (x,y), so that:
		| x = x0 + i*dxJ + j*dxI
		| y = y0 + i*dyJ + j*dyI

		and:

		|i = (dyI*(x-x0) - dxI*(y-y0)) / (dxJ*dyI-dyJ*dxI)
		|j = (dyJ*(x-x0) - dxJ*(y-y0)) / (dyJ*dxI-dxJ*dyI)

		where:
		nwf[0] = dxJ : the offset in the X direction along each column
		nwf[1] = dxI : the offset in the X direction along each line
		nwf[2] = dyJ : the offset in the Y direction along each column
		nwf[3] = dyI : the offset in the Y direction along each line
		nwf[4] = x0  : X coordinate of the center of the upper left raster element
		nwf[5] = y0  : Y coordinate of the center of the upper left raster element
		\endverbatim
	*/
	void setNavigationParameters(const vector<double>& nwf);

	/** Returns the georeferecing parameters 
		\param nwf vector of double to return the navigation parameters set
		\verbatim
		The Navigation parameters describe a transformation from column/line domain (i,j)
        to geographical world (x,y), so that:
		| x = x0 + i*dxJ + j*dxI
		| y = y0 + i*dyJ + j*dyI

		and:

		|i = (dyI*(x-x0) - dxI*(y-y0)) / (dxJ*dyI-dyJ*dxI)
		|j = (dyJ*(x-x0) - dxJ*(y-y0)) / (dyJ*dxI-dxJ*dyI)

		where:
		nwf[0] = dxJ : the offset in the X direction along each column
		nwf[1] = dxI : the offset in the X direction along each line
		nwf[2] = dyJ : the offset in the Y direction along each column
		nwf[3] = dyI : the offset in the Y direction along each line
		nwf[4] = x0  : X coordinate of the center of the upper left raster element
		nwf[5] = y0  : Y coordinate of the center of the upper left raster element
		\endverbatim
	*/
	void getNavigationParameters(vector<double>& nwf);
	//@}

	//! Returns the number of bands of the raster
	int nBands() const
	{ return nbands_; }
	
	//! Saves the parameters in a ASCII File, in TerraLib format
	void writeParametersFile();
	
	//! Reads the parameters described in a ASCII File, in TerraLib format
	void readParametersFile();

	//! String that contains any error or warning message that raster manipulation might have detected
	string errorMessage_;
	
private:
	int	nbands_;		//!< number of bands
	TeBox	box_;			//!< raster box in center of pixel coordinates
	TeProjection* projection_;	//!< raster projection
	bool hasSetDummy_;
};

#endif

