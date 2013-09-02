/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include "TeDecoderTIFF.h"
#include "TeUtils.h"
#include "TeException.h"
#include <geovalues.h>
#include <geo_normalize.h>
#include <geo_tiffp.h>

#include <fstream>
#include <cassert>
using std::ifstream;
using std::ofstream;

// ---- Public Methods ----- 

TeDecoderTIFF::TeDecoderTIFF(const TeRasterParams& par):
	tif(0),
	gtif(0),
	dataBuffer_(0),
	nplanes_(0),
	isGeoTiff_(false),
	TImage_(0),
	planar_(PLANARCONFIG_CONTIG),
	isTiled_(false),
	tilew_(0),
	tileh_(0),
	tilesacross_(0),
	bytespertile_(0),
	TCurTile_(-1),
	rowsperstrip_(0),
	nstripsperplane_(0),
	stripsize_(0),
	TCurStrip_(-1),
	nBands_(1),
	data_TeUNSIGNEDCHAR_(0), 
	data_TeCHAR_(0),   
	data_TeUNSIGNEDSHORT_(0),  
	data_TeSHORT_(0),   
	data_TeINTEGER_(0),   
	data_TeUNSIGNEDLONG_(0),   
	data_TeLONG_(0),  
	data_TeFLOAT_(0), 
	data_TeDOUBLE_(0)  
{
	params_ = par;
	params_.status_= TeRasterParams::TeNotReady;
	if (params_.mode_ == 'r')		// file should exists
	{								// read tiff/geotiff parameters
		tif=XTIFFOpen(params_.fileName_.c_str(),"r");
		if (!tif)
			return;

		gtif = (GTIF*)GTIFNew(tif);
		readTiffDirectory();
		if (gtif)
		{
			GTIFFree(gtif);
			gtif = 0;
		}
		if (tif)
		{
			XTIFFClose(tif); 
			tif = 0;
		}
		params_.decoderIdentifier_ = "TIF";
	}
}

void
TeDecoderTIFF::init()
{
	clear();		// reset existent internal structures
	params_.status_= TeRasterParams::TeNotReady;

	// TIFF image files may not be opened for both reading and writing; 
	// there is no support for altering the contents of a TIFF file. So
	// mode == 'w' never initializes this decoder.
	if (params_.mode_ == 'w')
	{
		params_.errorMessage_ = "There is no support for altering the contents of a TIFF file.";
		return;
	}
	if (params_.mode_ != 'r' && params_.mode_ != 'c')	// unknown mode
	{
		params_.errorMessage_ = "Invalid access mode.";
		return;
	}

	// check the initial permissions on the file
	if (params_.mode_ == 'c')		// creating a new raster file
	{
		ofstream dataFile(params_.fileName_.c_str()); // try to (re)create the file
		if (!dataFile)
		{
			params_.errorMessage_ = "Fail to (re)create the raster the file:" + params_.fileName_;
			return;
		}
		dataFile.close();
		memManager_.updateParams(params_);
		memManager_.params().decoderIdentifier_ = "SMARTMEM";
		memManager_.params().mode_ = 'c';
		memManager_.init();
		if (memManager_.params().status_ != TeRasterParams::TeReadyToWrite)
		{
			params_.errorMessage_ = "Fail to create the raster the file:" + params_.fileName_;
			return;
		}
		else
			params_.status_ = TeRasterParams::TeReadyToWrite;

		tif=XTIFFOpen(params_.fileName_.c_str(),"a");
		if (!tif)
		{
			params_.errorMessage_ = "Fail to create a tiff file.";
			params_.status_= TeRasterParams::TeNotReady;
			return;
		}

		// try to create a geotiff instance on the tiff file 
		gtif = (GTIF*)GTIFNew(tif);
		if (!gtif)
		{
			params_.errorMessage_ = "Fail to instantiate a geotiff file.";
			params_.status_= TeRasterParams::TeNotReady;
			return;
		}
		return;
	}
	
	ifstream dataFile(params_.fileName_.c_str()); // check if file exists
	if (!dataFile)
	{
		params_.errorMessage_ = "Fail to open the raster the file:" + params_.fileName_;
		return; 
	}
	dataFile.close();				
	// try to open the tiff file with the desired mode
	tif=XTIFFOpen(params_.fileName_.c_str(),"r");
	if (!tif)
	{
		params_.errorMessage_ = "Fail to instantiate a tiff file.";
		return;
	}

	isTiled_ = TIFFIsTiled(tif) != 0;						// return non-zero if is organized in tiles 
	TIFFGetField(tif,TIFFTAG_PLANARCONFIG,&planar_);	// storage organization
	if (isTiled_ && (planar_== PLANARCONFIG_SEPARATE))
	{
		XTIFFClose(tif); 
		tif = 0;
		params_.errorMessage_ = "There is no support to decode tiff files that are tiled and organized in separate planes.";
		return;
	}

	// try to create a geotiff instance on the tiff file 
	gtif = (GTIF*)GTIFNew(tif);
	if (!gtif)
	{
		XTIFFClose(tif); 
		tif = 0;
		params_.errorMessage_ = "Fail to instantiate a geotiff file.";
		return;
	}
	readTiffDirectory();			// read tiff keys
	getGeoTIFF();					// read geotiff keys
	params_.errorMessage_.clear();
	if (allocateWorkMemory())
		params_.status_ = TeRasterParams::TeReadyToRead;
	else
	{
		XTIFFClose(tif); 
		tif = 0;
		GTIFFree(gtif);
		gtif = 0;
		params_.errorMessage_ = "Fail to allocate necessary work memory.";
	}
}


bool 
TeDecoderTIFF::clear()
{
	if (!tif || !gtif)
		return true;
    
  TeRasterParams& mmpars = memManager_.params();  

	if (mmpars.mode_== 'c')
	{
		setGeoKeys();
		int row, col, b;
		double val=0.0;
		int nb = mmpars.nBands();
		long ssize = TIFFScanlineSize(tif);
		unsigned char* obuf = (unsigned char*)_TIFFmalloc(ssize);
    
		for (row=0; row<mmpars.nlines_; ++row)
		{
			for (col=0; col<mmpars.ncols_; ++col)
			{			
				for (b=0; b<nb; ++b)
				{
					if (!memManager_.getElement(col,row,val,b))
						continue;

					if (mmpars.dataType_[0] == TeDOUBLE)			
					{
						((double*)obuf)[col*nb+b] = (double)val;
					}
					else if (mmpars.dataType_[0] == TeFLOAT)			
					{
						((float*)obuf)[col*nb+b] = (float)val;
					}
					else if (mmpars.dataType_[0] == TeLONG)	
					{
						((long*)obuf)[col*nb+b] = (long)val;
					}
					else if (mmpars.dataType_[0] == TeUNSIGNEDLONG)	
					{
						((unsigned long*)obuf)[col*nb+b] = (unsigned long)val;
					}
					else if (mmpars.dataType_[0] == TeINTEGER)	
					{
						((int*)obuf)[col*nb+b] = (int)val;
					}
					else if (mmpars.dataType_[0] == TeUNSIGNEDSHORT)	
					{
						((unsigned short*)obuf)[col*nb+b] = (unsigned short)val;
					}
					else if (mmpars.dataType_[0] == TeSHORT)	
					{
						((short*)obuf)[col*nb+b] = (short)val;
					}
					else if (mmpars.dataType_[0] == TeCHAR)											
					{
						obuf[col*nb+b] = (char)val;
					}
					else											
					{
						obuf[col*nb+b] = (unsigned char)val;
					}

				}
			}
			if (!TIFFWriteScanline(tif, obuf, row, 0))
			{
				TIFFFlushData(tif);
				_TIFFfree(obuf);
				return false;
			}
		}
		TIFFFlushData(tif);
		_TIFFfree(obuf);
	}
	
	if (dataBuffer_)
	{
		for (unsigned int p=0; p<nplanes_; ++p)
			delete []dataBuffer_[p];
		delete []dataBuffer_;
	}
	dataBuffer_ = 0;

	if (gtif)
		GTIFFree(gtif);
	gtif = 0;

	if (tif)
		XTIFFClose(tif); 
	tif = 0;

	memManager_.clear();
	params_.status_ = TeRasterParams::TeNotReady;
	params_.errorMessage_.clear();
	return true;
}

TeDecoderTIFF::~TeDecoderTIFF()
{
	clear();
}

bool
TeDecoderTIFF::setElement(int col, int lin, double val, int band)
{
	return memManager_.setElement(col,lin,val,band);
}


bool
TeDecoderTIFF::getElement(int col,int lin, double &val,int band)
{
	unsigned int pos;
	unsigned long plane;
	if (isTiled_)  // tiled organization
	{
		if (TCurTile_ != (long)(lin/tileh_))
		{
			if (!readTileImageContig(lin/tileh_))
				return false;
			else
				TCurTile_ = (long)(lin/tileh_);
		}
		plane = (col/tilew_);
		lin = lin - TCurTile_*tileh_;
		col = col - (int)(col/tilew_)*tilew_;
		pos = lin*tilew_*nBands_+col*nBands_+band; 
	}	
	else			// striped organization
	{
		if (TCurStrip_ != (long)(lin/rowsperstrip_))
		{
			if (!readStrip(lin/rowsperstrip_)) // strip is not in memory
				return false;
			else
				TCurStrip_ = (long)(lin/rowsperstrip_);
		}
		lin = lin-(TCurStrip_*rowsperstrip_);
		plane = 0;
		if (planar_ == PLANARCONFIG_CONTIG)
		{
			pos = lin*params_.ncols_*nBands_+col*nBands_+band;
		}
		else
		{
			pos = lin*params_.ncols_+col;
			plane = band;
		}
	}
	(this->*( getelement_ptr_))(plane,pos,val);
	return true;
}


// --- Internal Methods ---
void
TeDecoderTIFF::readTiffDirectory()
{
	int ncols=0, nlins = 0;
	TIFFGetField(tif,TIFFTAG_IMAGEWIDTH,&ncols);
	TIFFGetField(tif,TIFFTAG_IMAGELENGTH,&nlins);
	params_.ncols_ = ncols;
	params_.nlines_ = nlins;
		
	short samplesperPixel;
	if (!TIFFGetField(tif,TIFFTAG_SAMPLESPERPIXEL,&samplesperPixel) )
		params_.nBands(1);
	else
		params_.nBands(samplesperPixel);
	nBands_ = params_.nBands();

	unsigned long nbitsperpixel_ = 0;
	if( !TIFFGetField(tif,TIFFTAG_BITSPERSAMPLE,&nbitsperpixel_) )
	{
		nbitsperpixel_ = 1;
		params_.setDataType(TeBIT);
	}
	else
	{
		short sampleformat;
		if (!TIFFGetField(tif,TIFFTAG_SAMPLEFORMAT,&sampleformat))
			sampleformat = 0;

		TeDataType tifType;
		if (nbitsperpixel_ == 8)
		{
			if (sampleformat == SAMPLEFORMAT_INT)
				tifType = TeCHAR;
			else 
				tifType = TeUNSIGNEDCHAR;
		}
		else if (nbitsperpixel_ == 16)
		{
			if (sampleformat == SAMPLEFORMAT_INT)
				tifType = TeSHORT;
			else 
				tifType = TeUNSIGNEDSHORT;
		}
		else if (nbitsperpixel_ == 32)
		{
			if (sampleformat == SAMPLEFORMAT_INT)
				tifType = TeLONG;
			else if (sampleformat == SAMPLEFORMAT_UINT)
				tifType = TeUNSIGNEDLONG;
			else
				tifType = TeFLOAT;		
		}
		else 
			tifType = TeDOUBLE;
		params_.setDataType(tifType);
	}
	unsigned short	photom_;    // photometric interpretation
	TIFFGetField(tif,TIFFTAG_PHOTOMETRIC,&photom_);     // photometric interpretation
	if (samplesperPixel == 3 && 
	   (photom_ == PHOTOMETRIC_MINISWHITE || 
	    photom_ == PHOTOMETRIC_MINISBLACK))
		photom_ = PHOTOMETRIC_RGB;
	
	switch (photom_)
	{
	case PHOTOMETRIC_PALETTE:
		params_.setPhotometric(TeRasterParams::TePallete);
		TImage_ = 2;
		readLut();
		break;
	case PHOTOMETRIC_RGB:
		params_.setPhotometric(TeRasterParams::TeRGB);
		TImage_ = 1;
		break;
	default:
		params_.setPhotometric(TeRasterParams::TeMultiBand);
		TImage_ = 0;
	}
	TIFFGetField(tif,TIFFTAG_PLANARCONFIG,&planar_);	// storage organization
	isTiled_ = TIFFIsTiled(tif) != 0;						// return non-zero if is organized in tiles 
	if (isTiled_)
	{
		TIFFGetField(tif,TIFFTAG_TILEWIDTH,&tilew_);
		TIFFGetField(tif,TIFFTAG_TILELENGTH,&tileh_);
		bytespertile_ = TIFFTileSize(tif);
		tilesacross_ = (params_.ncols_ + tilew_ - 1) / tilew_;
		params_.blockWidth_ = tilew_;
		params_.blockWidth_ = tileh_;
	}
	else
	{
		TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &rowsperstrip_);
		nstripsperplane_ =  TIFFNumberOfStrips(tif)/nBands_;	
		stripsize_ = TIFFStripSize(tif);
	}

	unsigned short compress_;    // photometric interpretation
	TIFFGetField(tif, TIFFTAG_COMPRESSION, &compress_);
	if (compress_ != COMPRESSION_NONE)
		params_.setCompressionMode(TeRasterParams::TeTiffCompression);

	double *minmaxvalues = 0;
	int count=0;
	unsigned int b;
  	if (TIFFGetField(tif, TIFFTAG_MAXSAMPLEVALUE, &count, &minmaxvalues)==1)
	{
		for (b=0; b<nBands_; ++b)
			params_.vmax_[b]=minmaxvalues[b];
	}

  	if (TIFFGetField(tif, TIFFTAG_MINSAMPLEVALUE, &count, &minmaxvalues)==1)
	{
		for (b=0; b<nBands_; ++b)
			params_.vmin_[b]=minmaxvalues[b];
	}

	// Read georeference parameters
	getGeoTIFF();
	if (!params_.box().isValid())
		params_.lowerLeftResolutionSize(0.5,0.5,1,1,params_.ncols_,params_.nlines_);
}

bool 
TeDecoderTIFF::allocateWorkMemory()
{
	if (dataBuffer_ && nplanes_>0)	// release previously used buffer
	{
		for (unsigned int p=0; p<nplanes_; ++p)
			delete []dataBuffer_[p];
		delete []dataBuffer_;
	}
	dataBuffer_ = 0;
	unsigned long buffersize;
	if (isTiled_)			
	{
		nplanes_ = tilesacross_;
		buffersize = bytespertile_;
	}
	else 
	{
		if (planar_ == PLANARCONFIG_SEPARATE)
			nplanes_ = nBands_;
		else
			nplanes_ = 1;
		buffersize = stripsize_;
	}

	dataBuffer_ = new unsigned char*[nplanes_];
	unsigned int i;
	for (i=0; i<nplanes_; ++i)
	{
		dataBuffer_[i] = new unsigned char [buffersize];
		if (!dataBuffer_[i])
		{
			for (unsigned int j=0; j<i; ++j)
				delete []dataBuffer_[j];
			delete []dataBuffer_;
			dataBuffer_=0;
			return false;
		}
	}
	data_TeUNSIGNEDCHAR_ = (unsigned char**)dataBuffer_;
	data_TeCHAR_ = (char**)dataBuffer_;
	data_TeUNSIGNEDSHORT_ = (unsigned short**)dataBuffer_;
	data_TeSHORT_ = (short**)dataBuffer_;
	data_TeINTEGER_ = (int**)dataBuffer_;
	data_TeUNSIGNEDLONG_ = (unsigned long**)dataBuffer_;
	data_TeLONG_ = (long**)dataBuffer_;
	data_TeFLOAT_ = (float**)dataBuffer_;
	data_TeDOUBLE_ = (double**)dataBuffer_;

	switch ( params_.dataType_[0] ) 
	{
	case (TeUNSIGNEDCHAR):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeUNSIGNEDCHAR;
		break;
	case (TeCHAR) :
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeCHAR;
		break;
	case (TeUNSIGNEDSHORT):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeUNSIGNEDSHORT;
		break;
	case (TeSHORT):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeSHORT;
		break;
	case (TeINTEGER):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeINTEGER;
		break;
	case (TeUNSIGNEDLONG):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeUNSIGNEDLONG;
		break;
	case (TeLONG):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeLONG;
		break;
	case (TeFLOAT):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeFLOAT;
		break;
	case (TeDOUBLE):
		getelement_ptr_ = &TeDecoderTIFF::getElement_TeDOUBLE;
		break;
	default:
		throw TeException( UNKNOWN_ERROR_TYPE, "Invalid raster data type", false );
	}
	return true;
}
void 
TeDecoderTIFF::setGeoKeys()
{
  TeRasterParams& mmparams = memManager_.params();  
  
// Image Size
	TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, mmparams.ncols_);
	TIFFSetField(tif, TIFFTAG_IMAGELENGTH, mmparams.nlines_);

// Orientation
	TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);

// Number of bits
	int nb = mmparams.nbitsperPixel_[0];
	TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, nb);
	switch (mmparams.dataType_[0]) 
	{
		case TeUNSIGNEDCHAR:
		case TeUNSIGNEDSHORT:
		case TeUNSIGNEDLONG:
			TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);
			break;
		case TeCHAR:
		case TeSHORT:
		case TeINTEGER:
		case TeLONG:
			TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
			break;
		case TeFLOAT:
		case TeDOUBLE:
			TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
			break;
		default:
			TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_VOID);
			break;
	}
// Number of bands
	TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, mmparams.nBands() );

// Compression
	TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);

// Planar configuration
	TIFFSetField(tif,TIFFTAG_PLANARCONFIG,  PLANARCONFIG_CONTIG);

// Photometric type
	if (mmparams.nBands() == 1) // monobands
	{
		if (mmparams.photometric_[0] == TeRasterParams::TePallete)
		{
			TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
			saveLut();
			TImage_ = 2;
		} 
		else 
		{
			TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
			TImage_ = 0;
		}
	} 
	else if (mmparams.nBands() == 3) 
	{
		if ( mmparams.photometric_[0] == TeRasterParams::TeRGB ) 
		{
			TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
			TImage_ = 1;
		} 
		else 
		{
			TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
			TImage_ = 0;
		}
	} 
	else 
	{
		TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
		TImage_ = 0;
	}
	TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, 1);

	if (mmparams.dxI_ != 0. || mmparams.dyJ_ != 0.) // there is a rotation defined
	{
		//	It seems that most viewers are not able to deal with GTIFF_TRANSMATRIX...
		// ... so let's write a navigation file instead...
		string tfw = TeGetName(mmparams.fileName_.c_str())+".tfw";
		FILE* fp = fopen(tfw.c_str(),"w");
		if (fp)
		{
			fprintf(fp,"%.6f\n",mmparams.dxJ_);
			fprintf(fp,"%.6f\n",mmparams.dyI_);
			fprintf(fp,"%.6f\n",mmparams.dxI_);
			fprintf(fp,"%.6f\n",mmparams.dyJ_);
			fprintf(fp,"%.6f\n",mmparams.x0_);
			fprintf(fp,"%.6f\n",mmparams.y0_);
			
			fclose( fp );
		}
	}
	else // tf there is no rotation use one tie point and pixel scale
	{
		double tiepoint[6];
		tiepoint[0] = 0;
		tiepoint[1] = 0;
		tiepoint[2] = 0;
		tiepoint[3] = mmparams.boundingBox().x1_;
		tiepoint[4] = mmparams.boundingBox().y2_;
		tiepoint[5] = 0;
		TIFFSetField(tif, GTIFF_TIEPOINTS, 6,&tiepoint); 

		double resolution[3];
		resolution[0] = mmparams.resx_;
		resolution[1] = mmparams.resy_;
		resolution[2] = 0;
		TIFFSetField(tif, GTIFF_PIXELSCALE, 3,&resolution);
	}

	// projection parameters
	if (!mmparams.projection() || mmparams.projection()->name() == "NoProjection")
	{
		// at least try to write a tfw file
		string tfw = TeGetName(mmparams.fileName_.c_str())+".tfw";
		FILE* fp = fopen(tfw.c_str(),"w");
		if (fp)
		{
			vector<double> tfw;
			mmparams.getNavigationParameters(tfw);
			for (unsigned int nl=0; nl<6; ++nl)
				fprintf(fp,"%8.4f\n",tfw[nl]);
				
		  fclose( fp );
		}
		return;
	}
	GTIFKeySet(gtif, GTRasterTypeGeoKey, TYPE_SHORT, 1, RasterPixelIsArea);
	const TeProjectionParams& paramas = mmparams.projection()->params();
	const string& projname = mmparams.projection()->name();
	if (projname == "LatLong")
	{
		GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeGeographic);
		GTIFKeySet(gtif, GeogAngularUnitsGeoKey, TYPE_SHORT, 1, Angular_Degree);
	}
	else
	{
		GTIFKeySet(gtif, GTModelTypeGeoKey, TYPE_SHORT, 1, ModelTypeProjected);
		GTIFKeySet(gtif, ProjLinearUnitsGeoKey, TYPE_SHORT, 1,Linear_Meter);
		if (projname == "UTM")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_TransverseMercator);
			GTIFKeySet(gtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE,1,paramas.lat0*TeCRD);
			GTIFKeySet(gtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE,1,paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE,1,paramas.scale);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE,1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE,1,paramas.offy);
		}
		else if (projname == "Mercator")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_Mercator);
			GTIFKeySet(gtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE,1,paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE,1,paramas.stlat1*TeCRD);
			GTIFKeySet(gtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE,1,paramas.scale);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE,1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE,1,paramas.offy);
		}
		else if (projname == "LambertConformal")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_LambertConfConic_2SP);
			GTIFKeySet(gtif, ProjFalseOriginLatGeoKey,TYPE_DOUBLE, 1, paramas.lat0*TeCRD);
			GTIFKeySet(gtif, ProjFalseOriginLongGeoKey, TYPE_DOUBLE, 1, paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1, paramas.stlat1*TeCRD);
			GTIFKeySet(gtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1, paramas.stlat2*TeCRD);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey,TYPE_DOUBLE, 1,paramas.offy);
		}
		else if (projname == "Polyconic")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_Polyconic);
			GTIFKeySet(gtif, ProjNatOriginLatGeoKey,TYPE_DOUBLE, 1, paramas.lat0*TeCRD);
			GTIFKeySet(gtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1, paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey,TYPE_DOUBLE, 1,paramas.offy);
			GTIFKeySet(gtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE,1,paramas.scale);
		}
		else if (projname == "CylindricalEquidistant")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_Equirectangular);
			GTIFKeySet(gtif, ProjCenterLatGeoKey,TYPE_DOUBLE, 1, paramas.stlat1*TeCRD);
			GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey,TYPE_DOUBLE, 1,paramas.offy);
		}
		else if (projname == "PolarStereographic")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_PolarStereographic);
			GTIFKeySet(gtif, ProjNatOriginLatGeoKey, TYPE_DOUBLE,1,paramas.lat0*TeCRD);
			GTIFKeySet(gtif, ProjStraightVertPoleLongGeoKey, TYPE_DOUBLE,1,paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjScaleAtNatOriginGeoKey, TYPE_DOUBLE,1,paramas.scale);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE,1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey, TYPE_DOUBLE,1,paramas.offy);
		}
		else if (projname == "Albers")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_AlbersEqualArea);
			GTIFKeySet(gtif, ProjStdParallel1GeoKey, TYPE_DOUBLE, 1, paramas.stlat1*TeCRD);
			GTIFKeySet(gtif, ProjStdParallel2GeoKey, TYPE_DOUBLE, 1, paramas.stlat2*TeCRD);
			GTIFKeySet(gtif, ProjNatOriginLatGeoKey,TYPE_DOUBLE, 1, paramas.lat0*TeCRD);
			GTIFKeySet(gtif, ProjNatOriginLongGeoKey, TYPE_DOUBLE, 1, paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey,TYPE_DOUBLE, 1,paramas.offy);
		}
		else if (projname == "Miller")
		{
			GTIFKeySet(gtif, ProjCoordTransGeoKey, TYPE_SHORT, 1, CT_MillerCylindrical);
			GTIFKeySet(gtif, ProjCenterLatGeoKey, TYPE_DOUBLE, 1, paramas.lat0*TeCRD);
			GTIFKeySet(gtif, ProjCenterLongGeoKey, TYPE_DOUBLE, 1, paramas.lon0*TeCRD);
			GTIFKeySet(gtif, ProjFalseEastingGeoKey, TYPE_DOUBLE, 1,paramas.offx);
			GTIFKeySet(gtif, ProjFalseNorthingGeoKey,TYPE_DOUBLE, 1,paramas.offy);
		}
	}

	// datum parameters
	if (paramas.datum.name() == "CorregoAlegre")
		GTIFKeySet(gtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,Datum_Corrego_Alegre);
	else if (paramas.datum.name() == "WGS84")
		GTIFKeySet(gtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,Datum_WGS84);
	else if (paramas.datum.name() == "SAD69")
		GTIFKeySet(gtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,Datum_South_American_Datum_1969);
	else if (paramas.datum.name() == "Aratu")
		GTIFKeySet(gtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,Datum_Aratu);
	else
		GTIFKeySet(gtif, GeogGeodeticDatumGeoKey, TYPE_SHORT, 1,32767);

	GTIFKeySet(gtif, GeogCitationGeoKey, TYPE_ASCII,1,paramas.datum.name().c_str());
	GTIFKeySet(gtif, GeogSemiMajorAxisGeoKey,TYPE_DOUBLE, 1, paramas.datum.radius());
	GTIFKeySet(gtif, GeogSemiMinorAxisGeoKey,TYPE_DOUBLE, 1, paramas.datum.radius()*(1-paramas.datum.flattening()));
	GTIFWriteKeys(gtif);
}

bool
TeDecoderTIFF::getGeoTIFF()
{
	params_.resx_ = params_.resy_ = 1;

	//---------------- navigation parameters 

	bool isGeoreferenced_ = false;
	// The three tags defined below may be used for defining the relationship between R (raster space)
	// and M (model space), and the relationship may be diagrammed as: 
	//           ModelPixelScaleTag 
	//           ModelTiepointTag         
	//    R  ------------ OR --------------> M
	//  (I,J,K)  ModelTransformationTag   (X,Y,Z)

	double *tiepoints = 0;
	double *pixel_scale = 0;
	double *transform   = 0;

	int count=0, tiepoint_count=0, transform_count=0;
  	TIFFGetField(tif, GTIFF_TIEPOINTS, &tiepoint_count,&tiepoints);
	TIFFGetField(tif, GTIFF_PIXELSCALE, &count, &pixel_scale); 
 	TIFFGetField(tif, GTIFF_TRANSMATRIX, &transform_count, &transform); 

	GTIFDefn defn;
	int hasGTifDefn;
	hasGTifDefn = GTIFGetDefn(gtif, &defn);
	bool isCentreOfPixel = false;
	if (hasGTifDefn)
	{
		short mtype;
		GTIFKeyGet(gtif,GTRasterTypeGeoKey,&mtype,0,1);
		if (mtype == RasterPixelIsPoint)
			isCentreOfPixel = true;
	}

	if (count >= 3 && tiepoint_count >= 6) // if we have 1 tie point and pixel scale
	{
		params_.topLeftResolutionSize(tiepoints[3]-tiepoints[0]*pixel_scale[0],
			                            tiepoints[4]-tiepoints[1]*pixel_scale[1],
										pixel_scale[0],pixel_scale[1],
										params_.ncols_,params_.nlines_,isCentreOfPixel);
		isGeoreferenced_ = true;
	}
	else if (transform_count == 16) // if we have a transformation matrix
	{
		vector<double> wf;
		wf.push_back(transform[0]);
		wf.push_back(transform[1]);
		wf.push_back(transform[4]);
		wf.push_back(transform[5]);
		wf.push_back(transform[3]);
		wf.push_back(transform[7]);
		params_.setNavigationParameters(wf);
		isGeoreferenced_ = true;

	}
	// else if (tiepoint_count > 6)
	// {
	//		we should use the set of tiepoints to interpolate... not implemented yet...
	// }


	// if we couldn't get the georrefencing information from the tags
	// try to get if from an external (.tfw) world file
	if (!isGeoreferenced_) 
	{
		size_t dotpos = params_.fileName_.rfind('.');
		if (dotpos != std::string::npos)
		{
			vector<double> wf;
			wf.resize(6);
			string tfwfile = params_.fileName_.substr(0,dotpos+1) + "tfw";
			FILE* fp = fopen(tfwfile.c_str(),"r");
			if (!fp)
			{
				tfwfile = params_.fileName_.substr(0,dotpos+1) + "TFW";
				fp = fopen(tfwfile.c_str(),"r");
			}

			if (fp)
			{
				isGeoreferenced_ = true;
				char val[40];
				for (int ii=0; ii<6; ++ii)
				{
					if (fscanf(fp,"%s",val))
						wf[ii] = TeRoundD(atof(val)); 
					else
					{
						isGeoreferenced_ = false;
						break;
					}
				}
				fclose(fp);
			}
			if (isGeoreferenced_)
				params_.setNavigationParameters(wf);
		}
	}

	//---------------- projection parameters 
	if (!hasGTifDefn)
	{
		if (!params_.projection())
		{
			TeProjection* noproj = new TeNoProjection();
			params_.projection(noproj);
			delete noproj;
		}
		return false;
	}
	else
	{
		// 1) Get Datum parameters
		TeDatum mDatum;
		if (defn.Datum == Datum_South_American_Datum_1969)
			mDatum = TeDatumFactory::make("SAD69");
		else if (defn.Datum == Datum_WGS84)
			mDatum = TeDatumFactory::make("WGS84");
		else if (defn.Datum == Datum_Corrego_Alegre)
			mDatum = TeDatumFactory::make("CorregoAlegre");
		else if (defn.Datum == Datum_Aratu)
			mDatum = TeDatumFactory::make("Aratu");
		else
		{
			if(params_.projection())
				mDatum = params_.projection()->datum();
			else
            {
				double TISemiMajor = defn.SemiMajor, TISemiMinor = defn.SemiMinor;
				// unknown datum... try to find a similar one based on SemiAxis values
				if (defn.SemiMajor == 0. && defn.SemiMinor == 0.)
				{
					TISemiMajor =6.371000e+06;		// default parameter of a spherical ellipsoid
					TISemiMinor =6.371000e+06;            
				}
				if (!findDatum(TISemiMajor,(TISemiMajor - TISemiMinor)/TISemiMajor,mDatum))
					mDatum = TeDatum("UserDefined",TISemiMajor,(TISemiMajor - TISemiMinor)/TISemiMajor,0.,0.,0.);
			}
		}

		// 2) Get projection parameters
		// if geographical model
		short mtype;
		GTIFKeyGet(gtif, GTModelTypeGeoKey,&mtype, 0, 1 );
		if (mtype == ModelTypeGeographic)	 
		{
			TeProjection* latlong = new TeLatLong(mDatum);
			params_.projection(latlong);
			delete latlong;
			return true;
		}		

		if (mtype != ModelTypeProjected)
		{
			if (!params_.projection())
			{
				TeProjection* noproj = new TeNoProjection();
				params_.projection(noproj);
				delete noproj;
				return false;
			}
			return true;
		}
		TeProjectionParams mProjPars;	
		mProjPars.units = "Meters"; // == not necessarily true... we should check for the units as defined in geotiff	
		mProjPars.datum = mDatum;

		// just set all parameters available from defn... some of them will not be used by some projections
		mProjPars.lat0 = defn.ProjParm[0]*defn.UOMAngleInDegrees*TeCDR;
		mProjPars.lon0 = defn.ProjParm[1]*defn.UOMAngleInDegrees*TeCDR;
		mProjPars.stlat1 = defn.ProjParm[2]*defn.UOMAngleInDegrees*TeCDR;
		mProjPars.stlat2 = defn.ProjParm[3]*defn.UOMAngleInDegrees*TeCDR;
		mProjPars.scale = defn.ProjParm[4];
		mProjPars.offx = defn.ProjParm[5];
		mProjPars.offy = defn.ProjParm[6];
		if (mProjPars.offy > 0)
			mProjPars.hemisphere = TeSOUTH_HEM;
		else
			mProjPars.hemisphere = TeNORTH_HEM;

		// now make the correspondence between geotiff projections and terralib projections
		switch (defn.CTProjection)
		{
			case CT_TransverseMercator : 
				mProjPars.name = "UTM";
			break;
			case CT_Mercator :
				mProjPars.name = "Mercator";
			break;
			case CT_LambertConfConic_2SP :
				mProjPars.name = "LambertConformal";
			break;
			case CT_Polyconic : 
				mProjPars.name = "Polyconic";
			break;
			case CT_Equirectangular :
				mProjPars.name = "CylindricalEquidistant";
			break;
			case CT_PolarStereographic :
				mProjPars.name = "PolarStereographic";
			break;
			case CT_AlbersEqualArea :
				mProjPars.name = "Albers";
			break;
			case CT_MillerCylindrical:
				mProjPars.name = "Miller";
			break;
			case CT_Sinusoidal:
				mProjPars.name = "TeSinusoidal";
			break;
			default:
				TeBox bb = params_.boundingBox();
				if (bb.x1_ >= -180 && bb.x1_ <= 180 && bb.y1_ >= -90 && bb.y2_ <= 90)
					mProjPars.name = "LatLong";
				else
				{
					TeProjection* noproj = TeProjectionFactory::make(mProjPars);
					params_.projection(noproj);
					delete noproj;
					return false;
				}
			break;
		}
		TeProjection* proj = TeProjectionFactory::make(mProjPars);
		params_.projection(proj);
		delete proj;
		return true;
	}
}

bool 
TeDecoderTIFF::readStrip(unsigned long strip)
{
	assert(tif);
	if (planar_ == PLANARCONFIG_CONTIG)	// for planar config all planes are in the strip
		return (TIFFReadEncodedStrip(tif, strip, dataBuffer_[0], -1) > 0);
	
	for (unsigned int i=0; i<nBands_; ++i)	// for separated config we have to read each plane individually
	{
		if (TIFFReadEncodedStrip(tif, (strip+i*nstripsperplane_), dataBuffer_[i],-1) == -1)
			return false;
	}
	return true;
}

bool 
TeDecoderTIFF::readTileImageContig(unsigned long tiler) 
{
	tiler *= tilesacross_;
	unsigned long i;
	for (i=0; i<tilesacross_; ++i)
	{
		if (TIFFReadEncodedTile(tif,tiler+i,dataBuffer_[i],-1) == -1)
			return false;
	}
	return true;
}

// --- To read and set a LUT table ---

static int checkmap(int n, unsigned short *r, unsigned short *g, unsigned short *b,long val)
{
    while (n-- >= 0)
		if (*r++ > val || *g++ > val || *b++ > val)
			return (16);
    return (8);
}

static unsigned short CVT1 (unsigned long x,long value)	//CVT is used in DEC station
{
	unsigned long den = (1L << 16) -1L;
	unsigned short num = (unsigned short)((x*value)/den); // normalize lut value
	return num;
}

bool
TeDecoderTIFF::readLut()
{
	unsigned short *rmap,*gmap,*bmap;
	if (!TIFFGetField(tif, TIFFTAG_COLORMAP, &rmap, &gmap, &bmap))
		return false;
	params_.lutr_.clear();
	params_.lutb_.clear();
	params_.lutg_.clear();

	unsigned int n = params_.lutr_.size();
	
	int nindexes = (1 << params_.nbitsperPixel_[0]);
	long maxval = (long)pow(2.,params_.nbitsperPixel_[0])-1;

	int x;
	if (checkmap (nindexes,rmap,gmap,bmap,maxval) == 16)
	{
		for(x=0; x<nindexes; ++x)
		{
			params_.lutr_.push_back(CVT1(rmap[x],maxval)); 
			params_.lutg_.push_back(CVT1(gmap[x],maxval));
			params_.lutb_.push_back(CVT1(bmap[x],maxval));
		}
	}
	else
	{
		for(x=0; x<nindexes; ++x)
		{
			params_.lutr_.push_back(rmap[x]);
			params_.lutg_.push_back(gmap[x]);
			params_.lutb_.push_back(bmap[x]);
		}
	}
	n = params_.lutr_.size();
	return true;
}

void
TeDecoderTIFF::saveLut()
{
	if (!tif)
		return;
    
  TeRasterParams& mmparams = memManager_.params();  

	int nentries = mmparams.lutr_.size();
	if (nentries <= 0) 
		return;

	unsigned short* lutr = new unsigned short[nentries];
	unsigned short* lutg = new unsigned short[nentries];
	unsigned short* lutb = new unsigned short[nentries];

	for (int i=0; i<nentries; i++)
	{
		lutr[i] = mmparams.lutr_[i]*255;
		lutg[i] = mmparams.lutg_[i]*255;
		lutb[i] = mmparams.lutb_[i]*255;
	}
	TIFFSetField(tif, TIFFTAG_COLORMAP,lutr,lutg,lutb);
	delete [] lutr;
	delete [] lutg;
	delete [] lutb;
}


inline void TeDecoderTIFF::getElement_TeUNSIGNEDCHAR(const long& plane, const long& pos, double& val)
{
	assert(data_TeUNSIGNEDCHAR_ != 0 );
	val = (double)data_TeUNSIGNEDCHAR_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeCHAR(const long& plane, const long& pos, double& val)
{
	assert(data_TeCHAR_ != 0 );
	val = (double)data_TeCHAR_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeUNSIGNEDSHORT(const long& plane, const long& pos, double& val)
{
	assert(data_TeUNSIGNEDSHORT_ != 0 );
	val = (double)data_TeUNSIGNEDSHORT_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeSHORT(const long& plane, const long& pos, double& val)
{
	assert(data_TeSHORT_ != 0 );
	val = (double)data_TeSHORT_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeINTEGER(const long& plane, const long& pos, double& val)
{
	assert(data_TeINTEGER_ != 0 );
	val = (double)data_TeINTEGER_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeUNSIGNEDLONG(const long& plane, const long& pos, double& val)
{
	assert(data_TeUNSIGNEDLONG_ != 0 );
	val = (double)data_TeUNSIGNEDLONG_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeLONG(const long& plane, const long& pos,double& val)
{
	assert(data_TeLONG_ != 0 );
	val = (double)data_TeLONG_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeFLOAT(const long& plane, const long& pos, double& val)
{
	assert(data_TeFLOAT_ != 0 );
	val = (double)data_TeFLOAT_[plane][pos];
}


inline void TeDecoderTIFF::getElement_TeDOUBLE(const long& plane, const long& pos, double& val)
{
	assert(data_TeDOUBLE_ != 0 );
	val = (double)data_TeDOUBLE_[plane][pos];
}


TeDecoderTIFFFactory::TeDecoderTIFFFactory(const string& name): 
	TeDecoderFactory(name) 
{
	// Defines the decoder identifiers synonyms
	TeDecoderFactory::instanceName2Dec()["TIF"] = "TIF";	
	TeDecoderFactory::instanceName2Dec()["TIFF"] = "TIF";
}

