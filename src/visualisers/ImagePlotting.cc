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

/*! \file ImagePlotting.cc
    \brief Implementation of the Template class ImagePlotting.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 13-Apr-2005
    
    Changes:
    
*/

#include "ImagePlotting.h"
#include "Image.h"


#include "LookupTableMode.h"
#include "TeDataTypes.h"
#include "TeRasterParams.h"
#include "TeDecoderMemory.h"
#include "TeRasterRemap.h"
#include "TeProjection.h"
#include "LegendVisitor.h"
#include "RasterData.h"

using namespace magics;


ImagePlotting::ImagePlotting()
{
}


ImagePlotting::~ImagePlotting()
{
}

/*!
 Class information are given to the output-stream.
*/	

void ImagePlotting::print(ostream& out)  const
{
	out << "ImagePlotting[";
	ImagePlottingAttributes::print(out);
	out << "]";
}


void ImagePlotting::operator()(Data& data, BasicGraphicsObjectContainer& out)
{
	// Reproject input data using Terralib routines
	RasterData& rdin = data.raster(out.transformation());  //input data
	RasterData  rdout;                 //output data
	MagLog::dev() << "ImagePlotting::operator()---> NEEDS TO BE IMPLENTED" << endl;
	
	if ( !createOutputRaster(out, rdin,rdout) )
	{
	     MagLog::dev() << "ERROR: ImagePlotting::createOutputRaster:" << *this << "\n";
	     return;
	}

	// Create output image from Terralib reprojected data
	Image* object = new Image();
	PaperPoint pp(rdout.getLowerLeftCorner().x(),rdout.getUpperRightCorner().y(),0.);
	object->setOrigin(pp);
	object->setWidth(rdout.getUpperRightCorner().x()-rdout.getLowerLeftCorner().x());
	object->setHeight(rdout.getUpperRightCorner().y()-rdout.getLowerLeftCorner().y());
	object->set(rdout.getRows(),rdout.getColumns());
//	    object->setFormat(format_);


    ColourTable& table = object->getColourTable();
	table.push_back(Colour("none"));
	//count--;
        (*colourMethod_).set(table, count_-1);
//

	(*mode_)(*object, rdout);
	//table.visit(legend_);
	// When the image is ready, we give it to the task 
	//(the task will be responsible to send it to the driver when necessary)
	out.push_back(object);
	
}

//
// IMPORTANT: The 2 routines below (createOutputRaster and reproject) have
// two important issues that needs to be addressed in the future:
// 1. Speed performance: currently, the input image matrix (Magics++
//    structure) is copied to the Terralib environment and, after the 
//    processing, it is copied back to the Magics++ structure.
// 2. Pixel size: currently, this value is hardcoded to TeUNSIGNEDSHORT
//     *IF THIS IS CHANGED, THEN metview/src/Reprojection/ReprojectService.cc
//     WILL ALSO NEED TO BE UPDATED*

bool ImagePlotting::createOutputRaster(const BasicGraphicsObjectContainer& owner, RasterData& rdin, RasterData& rdout)
{
	int i,j;
	static TeDecoderMemoryFactory theDecoderMemoryFactory("MEM");
	
	// Initialize Terralib raster structure for input data
	// Initialise parameters
	
	TeRasterParams parin;
	parin.ncols_  = rdin.getColumns();
	parin.nlines_ = rdin.getRows();
	parin.resx_   = abs(rdin.getXResolution());
	parin.resy_   = abs(rdin.getYResolution());
	parin.decName("MEM");
	
	parin.projection(&rdin.getProjection());
	UserPoint ll = rdin.getLowerLeftCorner();
	UserPoint ur = rdin.getUpperRightCorner();
	parin.boundingBoxLinesColumns(ll.x(),ll.y(),ur.x(),ur.y(),rdin.getRows(),  rdin.getColumns(), TeBox::TeLOWERLEFT);
	parin.nBands(1);
	parin.setDataType(TeUNSIGNEDSHORT);
	parin.setCompressionMode(TeRasterParams::TeNoCompression);

	// Copy input image to the raster structure
	TeRaster rastin(parin);
	rastin.params().decName("MEM");
	bool ok = rastin.init(parin);
	for (i = 0; i < parin.nlines_; i++)
		for (j = 0; j < parin.ncols_; j++)
			rastin.setElement(j,i,rdin(i,j),0);

	// Initialize Terralib raster structure for output data
	// Initialise parameters
	
	TeRasterParams parout;
	
	const Transformation& transf = owner.transformation();
	TeProjection& projout = const_cast<Transformation * >(&transf)->getProjection();

	parout.projection(&projout);
	double minX = transf.getMinPCX();  
	double maxX = transf.getMaxPCX();  
	double minY = transf.getMinPCY();  
	double maxY = transf.getMaxPCY();  

	// Set output resolution and compute image size
	// Set the same x/y resolution
	double width  = owner.absoluteWidth();
	int ifreq = max(int(parin.ncols_/(pixelFrequency_*width)),1);
	int nnx   = (parin.ncols_ - 1) / ifreq + 1;
	double res = (maxX-minX)/double(nnx-1);

	parout.boundingBoxResolution(minX,minY,maxX,maxY,res,res, TeBox::TeLOWERLEFT);
	parout.decName("MEM");
	parout.nBands(1);
	parout.setDummy(0);
	parout.setDataType(TeUNSIGNEDSHORT); // see note 2 above
	parout.setCompressionMode(TeRasterParams::TeNoCompression);

	// Initialise raster structure
	TeRaster rastout(parout);
	rastout.params().decName("MEM");
	ok = rastout.init(parout);

	// Reproject input data
	TeRasterRemap reproj(&rastin,&rastout);
	ok = reproj.apply();

	// Copy output raster structure to output image
    rdout.setColumns(parout.ncols_);
	rdout.setRows(parout.nlines_);

	rdout.setXResolution(parout.resx_);
	rdout.setYResolution(parout.resy_);
	// boundingBox or box function ???
	rdout.setUpperRightCorner(parout.boundingBox().x2(), parout.boundingBox().y2());
	rdout.setLowerLeftCorner(parout.boundingBox().x1(), parout.boundingBox().y1());
	rdout.reserve(parout.nlines_*parout.ncols_);
	double val;
	for (i = 0; i < parout.nlines_; i++)
	{
		for (j = 0; j < parout.ncols_; j++)
		{
			if (!rastout.getElement(j,i,val,0) )
				val = 0;
           //points outside the rectangle image area is set
           //to 0 by the Terralib interpolation procedure
  		   rdout.push_back(val);
		}
	}

	return ok;
}


void ImagePlotting::visit(LegendVisitor& legend)
{
	//(*mode_)(legend);
}

bool ImagePlotting::reproject(RasterData& rdin, RasterData& rdout,
	Transformation& transformation, double resx, double resy)
{
	int i,j;
	
	static TeDecoderMemoryFactory theDecoderMemoryFactory("MEM");

	// Initialize Terralib raster structure for input data
	// Initialise parameters
	TeRasterParams parin;
	parin.ncols_  = rdin.getColumns();
	parin.nlines_ = rdin.getRows();
	parin.resx_   = abs(rdin.getXResolution());
	parin.resy_   = abs(rdin.getYResolution());
	parin.projection (&rdin.getProjection());
	UserPoint ll = rdin.getLowerLeftCorner();
	UserPoint ur = rdin.getUpperRightCorner();
	parin.boundingBoxResolution(ll.x(),ll.y(),ur.x(),ur.y(), parin.resx_, parin.resy_);
	parin.nBands(1);
	parin.setDataType(TeUNSIGNEDSHORT);
	//parin.compressionType_ = TeNoCompression;
    parin.setCompressionMode(TeRasterParams::TeNoCompression);
	parin.decName("MEM");

	// Copy input image to the raster structure
	TeRaster rastin(parin);
	bool ok = rastin.init(parin);
	for (i = 0; i < parin.nlines_; i++)
		for (j = 0; j < parin.ncols_; j++)
			rastin.setElement(j,i,rdin(i,j),0); //too slow???

	// Initialize Terralib raster structure for output data
	// Initialise parameters
	TeRasterParams parout;
	TeProjection& projout = const_cast<Transformation * >(&transformation)->getProjection();
	parout.projection(&projout);
	parout.decName("MEM");

	double minX = transformation.getMinPCX();  
	double maxX = transformation.getMaxPCX();  
	double minY = transformation.getMinPCY();  
	double maxY = transformation.getMaxPCY();  

	// Set output resolution
	parout.boundingBoxResolution(minX,minY,maxX,maxY,resx,resx);

	parout.nBands(1);
	parout.setDataType(TeUNSIGNEDSHORT);   // see note 2 above
	//parout.compressionType_ = TeNoCompression;
    parout.setCompressionMode(TeRasterParams::TeNoCompression);

	// Initialise raster structure
	TeRaster rastout(parout);
	ok = rastout.init(parout);

	// Reproject input data
	TeRasterRemap reproj(&rastin,&rastout);
	ok = reproj.apply();

	// Copy output raster structure to output image
        rdout.setColumns(parout.ncols_);
	rdout.setRows(parout.nlines_);
// no need	rdout.setProjection(&projout);
	rdout.setXResolution(parout.resx_);
	rdout.setYResolution(parout.resy_);
	// boundingBox or box function ???
	rdout.setUpperRightCorner(parout.boundingBox().x2(), parout.boundingBox().y2());
	rdout.setLowerLeftCorner(parout.boundingBox().x1(), parout.boundingBox().y1());
	rdout.reserve(parout.nlines_*parout.ncols_);
	double val;
	for (i = 0; i < parout.nlines_; i++)
		for (j = 0; j < parout.ncols_; j++)
		{
			rastout.getElement(j,i,val,0); //too slow???
			rdout.push_back(val);
		}

	return ok;
}
