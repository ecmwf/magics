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

#include "TeRasterRemap.h"
#include "TeDecoderDatabase.h"
#include "TeRaster.h"
#include "TeDecoderMemory.h" 
#include "TeRasterParams.h"
#include "TeProgress.h"
#include "TeVectorRemap.h"
#include "TeImportRaster.h"

//FAMI #include "TeLibJpegWrapper.h" //not used; otherwise, needs to define macro JPEG_DEFINED


TeLayer* 
TeImportRaster (const string& layerName, TeRaster* rasterIn, TeDatabase* database)
{
	if (!database || layerName.empty() || !rasterIn || 
		(rasterIn->params().status_ != TeRasterParams::TeReadyToRead && 
		 rasterIn->params().status_ != TeRasterParams::TeReadyToWrite))
		return 0;

	// find a valid layer name
	string newLayerName = layerName;
	TeLayerMap& layerMap = database->layerMap();
	TeLayerMap::iterator it;
	bool flag = true;
	int n = 1;
	while (flag)
	{
		for (it = layerMap.begin(); it != layerMap.end(); ++it)
		{
			if (TeStringCompare(it->second->name(),newLayerName))
				break;
		}
		if (it == layerMap.end())
			flag = 0;
		else
			newLayerName = layerName + "_" +Te2String(n);
		n++;	
	}

	TeLayer* newLayer = new TeLayer(newLayerName,database,rasterIn->projection());
	if (newLayer->id() <= 0 )
		return 0;				// couldn´t create new layer

	// block dimension guessing
	unsigned int bw = 512;
	unsigned int bh = 512;		// an initial guess...

	// if raster is up to 512x512 store it in one block
	if (rasterIn->params().nlines_< 512 && rasterIn->params().ncols_< 512)
	{
		bw = rasterIn->params().ncols_;
		bh = rasterIn->params().nlines_;
	}
	// if ncols<1024 build blocks of 1 line
	else if (rasterIn->params().ncols_ < 1024)  
	{
		bw = rasterIn->params().ncols_;
		bh = 1;
	}

	if (TeProgress::instance())
		TeProgress::instance()->reset();

	bool res = TeImportRaster(newLayer, rasterIn, bw, bh, TeRasterParams::TeNoCompression, 
		"", rasterIn->params().dummy_[0], rasterIn->params().useDummy_, TeRasterParams::TeNoExpansible);
	if (res)
		return newLayer;

	database->deleteLayer(newLayer->id());
	delete newLayer;
	return 0;
}


// This function mosaics an input raster to one previsously existing in a TerraLib layer
bool TeMosaicRaster(TeRaster* rasterIn, TeLayer* layer,  const string& objectId)
{

	// layer must exist and input raster should be read to be read
	if (!layer || !rasterIn  || rasterIn->params().status_ != TeRasterParams::TeReadyToRead)
		return false;
	
	TeRaster* rasterOut;
	if (!objectId.empty())	// if object id informed try to get raster associated to it
		rasterOut = layer->raster(objectId,'w');
	else					// else try to get first one found
		rasterOut = layer->raster();

	if (!rasterOut)			// no raster asked or no raster at all
		return false;				// can not do mosaic

	if (rasterOut->params().tiling_type_==TeRasterParams::TeNoExpansible)
		return false;				// can mosaic if is not possible

	// allows mosaic of same photometric types only
	if (rasterOut->params().photometric_[0] != rasterIn->params().photometric_[0])
		return false;

	// increases output box to: input raster box + output raster box 
	TeBox boxIn = rasterIn->params().box();

	// if necessary remap input box to output projection 
	if (!(*(rasterIn->projection()) == *(rasterOut->projection())))
		boxIn = TeRemapBox(boxIn,rasterIn->projection(),rasterOut->projection());

	// adjust it to expansible values
	boxIn = adjustToCut(boxIn, rasterOut->params().blockWidth_*rasterOut->params().resx_, 
							   rasterOut->params().blockHeight_*rasterOut->params().resy_);

	// sum stored and input boxes
	TeBox newBox = rasterOut->params().boundingBox();
	updateBox(newBox,boxIn);

	// update box in parameters of the raster and it's decoder
	rasterOut->params().boundingBoxResolution(newBox.x1_,newBox.y1_,newBox.x2_,newBox.y2_,
											  rasterOut->params().resx_, rasterOut->params().resy_);

	rasterOut->decoder()->params().boundingBoxResolution(newBox.x1_,newBox.y1_,newBox.x2_,newBox.y2_,
											  rasterOut->params().resx_, rasterOut->params().resy_);
	rasterOut->params().nTilesInMemory_ = 0;
	
	// creates a remapping from input raster to output raster
	TeRasterRemap remap(rasterIn, rasterOut);
	if (remap.apply(true))			// o.k.
	{
		TeRasterParams rp = rasterOut->params();

		// atualizes the representation box in memory
		TeRepresentation* repp = layer->getRepresentation(TeRASTER);
		TeBox boxNew = rp.box();
		updateBox(repp->box_,rp.box());
		repp->nCols_ = rp.ncols_;
		repp->nLins_ = rp.nlines_;
		repp->resX_ = rp.resx_;
		repp->resY_ = rp.resy_;

		// atualizes representation in the database
		layer->database()->updateRasterRepresentation(layer->id(),rp,objectId);
		layer->updateLayerBox(rp.boundingBox());
		rasterOut->clear();
		return true;
	}
	return false;
}


bool TeImportRaster (TeLayer* layer, TeRaster* rasterIn, 
					 unsigned int bWidth, unsigned int bHeight,TeRasterParams::TeRasterCompressionMode compress,
					 const string& objectId, double dummy, bool useDummy,
					 TeRasterParams::TeRasterTilingType indext)
{	

	if (!layer || !rasterIn)
		return false;

	string objId;
	if (objectId.empty())
		objId = "O1";
	else
		objId = objectId;


	// check if there  is a raster geometry to be modified
	string tableGeo;
	TeRepresentation* repp = 0;
	repp = layer->getRepresentation(TeRASTER);
	if (!repp || !layer->raster(objectId))	// layer doesn´t have any 
	{										// or the desired raster geometry
											// a new one should be created
		TeProjection* projIn = rasterIn->projection();	
		TeProjection* projOut = layer->projection();

		TeRasterParams parOut = rasterIn->params();		// parameters of the raster being created	
														// start with the same parameters as input raster
		
		parOut.setCompressionMode(compress);		// overwrites some parameters 
		parOut.nTilesInMemory_ = 0;					// according to the specified in the interface
		parOut.blockHeight_ = bHeight;
		parOut.blockWidth_ = bWidth;
		parOut.setDummy(dummy);
		parOut.useDummy_ = useDummy;
		parOut.tiling_type_ = indext;

		TeBox newBox = rasterIn->params().boundingBox();

		// if input raster has some rotation associated it has to be corrected 
		// during the importing
		if (parOut.dxI_ != 0. || parOut.dyJ_ != 0.)
		{
			parOut.boundingBoxResolution(newBox.x1_,newBox.y1_,newBox.x2_,newBox.y2_,parOut.resx_,parOut.resy_);
			parOut.dxI_= 0.0;							  
			parOut.dyJ_= 0.0;							 
			parOut.dxJ_ = 0.0;
			parOut.dyI_ = 0.0;	
		}
		
		bool diffProj = false;
		parOut.projection(projOut);
		if (projIn && projOut && !(*projIn == *projOut))
		{
			diffProj = true;
			TeBox boxIn = rasterIn->params().boundingBox();
			newBox = TeRemapBox (boxIn,projIn,projOut);
			parOut.resx_ = newBox.width()/parOut.ncols_;	// recalculates resolutions 
			parOut.resy_ = newBox.height()/parOut.nlines_;  // for the new projection
			parOut.boundingBoxResolution(newBox.x1_,newBox.y1_,newBox.x2_,newBox.y2_,parOut.resx_,parOut.resy_);
			newBox = parOut.box();
		}
	
		if (indext == TeRasterParams::TeExpansible)			// if mosaicable adjust box 
			parOut.resizeToTiling(newBox,bWidth,bHeight);	// to be cut in blocks of bWidth X bHeight (in projection units)

		parOut.decoderIdentifier_ = "DB";					// parameters of the decoder
		parOut.fileName_ = "RasterLayer" + Te2String(layer->id()) + "_R_" + objId;

		if (parOut.photometric_[0] == TeRasterParams::TePallete)
			parOut.lutName_ = parOut.fileName_ + "_LUT";

		if (!layer->addRasterGeometry(parOut,objId))		// creates the empty raster geometry
			return false;

		// create a decoder to raster in a TerraLib database 
		parOut.mode_ = 'w';
		TeDecoderDatabase *dbDecoder = new TeDecoderDatabase(parOut);
		dbDecoder->setDB(layer->database());

		// optimization: we don't need to go through remap routines.
		// just cut the input raster and save the tiles
		bool res=false;
		TeRasterParams parF;
		if (indext == TeRasterParams::TeNoExpansible && !diffProj)
		{
			int ntiles = (parOut.nlines_/parOut.blockHeight_)*(parOut.ncols_/parOut.blockWidth_);
			if(TeProgress::instance())
				TeProgress::instance()->setTotalSteps(ntiles);
			int count = 0;
			clock_t	ti, tf;
			int x=0, y=0, b, nb=parOut.nBands(),lx, ly;
			double val= parOut.dummy_[0];
			TeMemoryPage page(parOut.blockHeight_*parOut.blockWidth_,val,parOut.dataType_[0]);
			string index;
			ti = clock();
			while (y<parOut.nlines_)  // for each row of tiles
			{
				page.ulLin_ = y;
				x=0;
				while (x<parOut.ncols_)	// for each tile in a row
				{
					page.clear();
					page.ulCol_= x;
					for (b=0; b<nb; ++b)    // for each band;
					{
						for (ly=0; ly<parOut.blockHeight_; ++ly)
						{
							for (lx=0; lx<parOut.blockWidth_; ++lx)  // write the tile
							{
								if (rasterIn->getElement(x+lx,y+ly,val,b))
								{
									page.setVal(x+lx,y+ly,parOut.blockWidth_,val);
									if (val<parOut.vmin_[b])
										parOut.vmin_[b] = val;
									if (val>parOut.vmax_[b])
										parOut.vmax_[b] = val;
								}
							}
						}
						index = dbDecoder->codifyId(x,y,b,1,0);
						if (!dbDecoder->putRasterBlock(index,page.data_,page.pageSize()))
							res=false;
						page.clear();
					}
					x+= parOut.blockWidth_;
					++count;
					if(TeProgress::instance())
					{
						tf = clock();
						if (int((tf-ti)/CLOCKS_PER_SEC) > 3)
						{
							if (TeProgress::instance()->wasCancelled())
								break;
							TeProgress::instance()->setProgress(count);
							ti = tf;
						}
					}
				}
				y+= parOut.blockHeight_;
			}
			if (TeProgress::instance())
				TeProgress::instance()->reset();

			// atualizes the representation box in memory
			TeRepresentation* repp = layer->getRepresentation(TeRASTER);
			updateBox(repp->box_,parOut.box());
			repp->nCols_ = parOut.ncols_;
			repp->nLins_ = parOut.nlines_;
			repp->resX_ = parOut.resx_;
			repp->resY_ = parOut.resy_;
			layer->updateLayerBox(parOut.boundingBox());
			layer->database()->updateRasterRepresentation(layer->id(),dbDecoder->params(),objectId);
			// atualizes representation in the database
			for (b=0; b<nb; ++b)
			{
				dbDecoder->params().vmax_[b]=parOut.vmax_[b];
				dbDecoder->params().vmin_[b]=parOut.vmin_[b];
			}
			layer->database()->updateRasterRepresentation(layer->id(),dbDecoder->params(),objectId);
			dbDecoder->clear();
			res=true;
		}
		else
		{
			TeRaster* rasterOut = new TeRaster();
			rasterOut->setDecoder(dbDecoder);
			rasterOut->init();
			TeRasterRemap remap(rasterIn, rasterOut);
			if (remap.apply(true))
			{
				TeRepresentation* repp = layer->getRepresentation(TeRASTER);
				updateBox(repp->box_,rasterOut->params().box());
				repp->nCols_ = rasterOut->params().ncols_;
				repp->nLins_ = rasterOut->params().nlines_;
				repp->resX_ = rasterOut->params().resx_;
				repp->resY_ = rasterOut->params().resy_;

				// atualizes representation in the database
				layer->database()->updateRasterRepresentation(layer->id(),rasterOut->params(),objectId);
				layer->updateLayerBox(rasterOut->params().boundingBox());
				rasterOut->clear();
				res = true;
			}
		}
		if (res)			
		{
			//	create spatial index in the block box
			string tableR = layer->database()->getRasterTable(layer->id(),objectId);
			layer->database()->insertMetadata(tableR,layer->database()->getSpatialIdxColumn(TeRASTER), 0.000005,0.000005,layer->box());		
			layer->database()->createSpatialIndex(tableR,layer->database()->getSpatialIdxColumn(TeRASTER), (TeSpatialIndexType)TeRTREE);
		}
		delete dbDecoder;
		return res;
	}
	else
	{
		// layer contains already a raster representation associated to the
		// object id: calls mosaic operation
		return TeMosaicRaster(rasterIn,layer,objId);
	}
}

bool 
TeBuildLowerResolution(TeLayer* layer, TeRaster* rasterIn, int resFac, const string& objectId)
{
	if (!layer || !rasterIn)	// layer and input raster pointers shouldn't be null
		return false;

	string objId;				// if empty try to get the default object identification
	if (objectId.empty())
		objId = "O1";
	else
		objId = objectId;	
	
	string tableName;
	TeRepresentation* repp = layer->getRepresentation(TeRASTER);
	if (repp)
	{
		tableName = layer->database()->getRasterTable(layer->id(), objId);
		if (tableName.empty())	
			return false;		// layer doesn´t have a raster geometry to this object id
	}
	else					
		return false;			// layer doesn´t have any raster geometry

	// retrieves the original raster
	TeRaster* originalRaster = layer->raster(objId);
	TeBox bbOriginalRes = originalRaster->params().boundingBox();

	// builds some parameters relative to the new resolution
	TeRasterParams parNewRes = originalRaster->params();
	parNewRes.resolution_ = resFac;
	parNewRes.mode_ = 'w';
	parNewRes.boundingBoxResolution(bbOriginalRes.x1_, bbOriginalRes.y1_,bbOriginalRes.x2_, bbOriginalRes.y2_,
									parNewRes.resx_*resFac, parNewRes.resy_*resFac);

	// adjust box to tiling in geographical coordinates
	if (originalRaster->params().tiling_type_ == TeRasterParams::TeExpansible)
	{
		TeBox b = parNewRes.boundingBox();
		int w = parNewRes.blockWidth_;
		int h = parNewRes.blockHeight_;
		parNewRes.resizeToTiling(b, w, h);
	} 


	// create a decoder to raster in a TerraLib database 
	TeDecoderDatabase *dbDecoder = new TeDecoderDatabase(parNewRes);
	dbDecoder->setDB (layer->database());

	TeRaster* rasterOut = new TeRaster();
	rasterOut->setDecoder(dbDecoder);
	rasterOut->init();

	// remap;
	TeRasterRemap remap;
	remap.setInput(rasterIn);
	remap.setOutput(rasterOut);
	bool status = remap.apply(true);
	rasterOut->clear();
	delete dbDecoder;
	return status;
}



void
TeCalculateMean(TeDecoderMemory* decMem, vector<TeMemoryPage*> pages,int ulColStart,int ulLinStart)
{
	int i,j, c=ulColStart, l=ulLinStart;
	unsigned int b;
	int npixels;
	double mean,val;
	int nlines = decMem->params().nlines_-1;
	int ncols = decMem->params().ncols_-1;
	double dummy;

	for (b=0; b<pages.size(); ++b)
	{
		dummy = decMem->params().dummy_[b];
		l=ulLinStart;
		for (i=0; i<nlines; i+=2)
		{
			c=ulColStart;
			for (j=0; j<ncols; j+=2)
			{
				mean = 0.0;
				npixels = 0;
				if ((j>0) && (i>0) && decMem->getElement(j-1,i-1,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if ((i>0) && decMem->getElement(j,i-1,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if ((i>0) && (j<ncols-1) && decMem->getElement(j+1,i-1,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}	
				
				if ((j>0) && decMem->getElement(j-1,i,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if (decMem->getElement(j,i,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if ((j<ncols-1) && decMem->getElement(j+1,i,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}				
			
				if ((j>0) && (i<nlines-1) && decMem->getElement(j-1,i+1,val) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if ((i<nlines-1) && decMem->getElement(j,i+1,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if ((j<ncols-1) && (i<nlines-1) && decMem->getElement(j+1,i+1,val,b) && val != dummy)
				{
					mean += val;
					npixels ++;
				}
				if (npixels)
				{
					mean = mean/npixels;
					pages[b]->setVal(c,l,decMem->params().ncols_,mean);
				}
				c++;
			}
			l++;
		}
	}
}


/*
This routine builds a multi-resolution pyramid with nLevels in a raster representation.
The raster representation should be a raster stored in a TerraLib database.
The raster representation should have been built in blocks with width and height power of 2.
*/
bool TeBuildMultiResolutionPyramid(TeRaster* rst, int nLevels)
{
	if (!rst)
		return false;
	if (nLevels <= 1)
		return false;
	if (rst->params().decoderIdentifier_ != "DB")
		return false;
	int bw = rst->params().blockWidth_;
	int bh = rst->params().blockHeight_;

	/* check to see if bw and bh are power of 2 */
	if (bw == 0 || bh == 0 ||
	   (bw & (bw - 1)) != 0  || (bh & (bh - 1)) != 0)
	{
		return false;
	}
	
	TeBox rstBB = rst->params().boundingBox();

	// build a vector of pages in memory
	vector<TeMemoryPage*> pages;
	int b;
	for (b=0; b<rst->params().nBands(); ++b)
	{
		TeMemoryPage* block = new TeMemoryPage(bh*bw, 
											   rst->params().dummy_[b],
											   rst->params().dataType_[b]);
		block->ulCol_ = 0;
		block->ulLin_ = 0;
		pages.push_back(block);
	}

	// auxiliary variables
	int ulCol, 
		ulLin, 
		band, 
		res, 
		subb, 
		ulColStart, 
		ulLinStart;

	// parameters of a upper level
	TeRasterParams parUp = rst->params();
	parUp.mode_ = 'w';

	// a decoder im memory for the lower level blocks
	TeDecoderMemory*	decMem    = new TeDecoderMemory();

	// a decoder database for the upper level of the pyramid
	TeDecoderDatabase*	decDBUpperLevel = new TeDecoderDatabase(parUp);

	// more auxiliary variables
	int pxBUp=-1, pyBUp=-1;
	int xBUp, yBUp;
	int bUlx, bUly;

	bool flag = true;
	string bid;
	char baux[60];

	TeRasterParams parBlock;
	int r = 2;
	for (int nl=0; nl<nLevels; ++nl)
	{
		// select all the blocks from lower level
		if (!rst->selectBlocks(rstBB,r/2,parBlock) )
		{
			delete decMem;
			delete decDBUpperLevel;
			return false;
		}

		decMem->updateParams(parBlock);
		decMem->init();
		parUp.boundingBoxResolution(rstBB.x1_, rstBB.y1_, rstBB.x2_, rstBB.y2_,
	    							rst->params().resx_*r,rst->params().resy_*r,
									TeBox::TeLOWERLEFT);
		parUp.resolution_ = r;
		decDBUpperLevel->updateParams(parUp);
		
		pxBUp=-1;
		pyBUp=-1;
		// for each block of the lower level
		do
		{
			// retrieve the lower level block
			flag = rst->fetchRasterBlock(decMem);
			TeRasterParams& par = decMem->params();

			// get the coordinate of the  lower level block upper left corner
			TeBox blockBox = par.boundingBox();
			TeCoord2D auxC(blockBox.x1_+4*par.resx_,blockBox.y2_-4*par.resy_);
			TeCoord2D ij = parUp.coord2Index(auxC);
			ulColStart = TeRoundRasterIndex(ij.x_);
			ulLinStart = TeRoundRasterIndex(ij.y_);
			
			// find the correspondent upper level block that contains the coordinate
			bid = decDBUpperLevel->codifyId(ulColStart,ulLinStart,0,r,0);
			sscanf(bid.c_str(),"X%dY%d",&xBUp,&yBUp);
			decDBUpperLevel->decodifyId(bid,bUlx,bUly,band,res,subb);

			// check to which quadrant in the upper level block the lower level block fits
			if (ulColStart >= (bUlx+bw/2))
				ulColStart = bUlx+bw/2;
			else
				ulColStart = bUlx;

			if (ulLinStart >= (bUly+bh/2))
				ulLinStart = bUly+bh/2;
			else
				ulLinStart = bUly;

			// check if the upper level block is in already in memory
			if (pxBUp != xBUp || pyBUp != yBUp)
			{	
				for (b=0; b<rst->params().nBands(); ++b)
				{
					// if it is not the first blocks do the swapping from memory to db
					if (pxBUp >= 0 && pyBUp >= 0)
					{
						// save blocks from memory
						sprintf(baux,"X%dY%dB%dR%dS0",pxBUp, pyBUp,b,r);
						decDBUpperLevel->putRasterBlock(baux,pages[b]->data_, parUp.blockHeight_*parUp.blockWidth_);
					}
					// get the needed one
					pages[b]->clear();
					sprintf(baux,"X%dY%dB%dR%dS0",xBUp,yBUp,b,r);
					decDBUpperLevel->getRasterBlock(baux,pages[b]->data_);
					TeBlockIndex index;
					index.band_ = b;
					index.col_ = xBUp;
					index.lin_ = yBUp;
					decDBUpperLevel->blockIndexPos(index,ulCol,ulLin,b);
					pages[b]->ulLin_ = ulLin;
					pages[b]->ulCol_ = ulCol;
				}
				// keep track of the blocks in memory
				pxBUp = xBUp;
				pyBUp = yBUp;
			}
			// write lower level block into upper level block using a 3x3 smoothing window (mean)
			TeCalculateMean(decMem,pages,ulColStart,ulLinStart);

		}while (flag);

		// save the last blocks in memory
		for (b=0; b<rst->params().nBands(); ++b)
		{
			sprintf(baux,"X%dY%dB%dR%dS0",xBUp,yBUp,b,r);
			decDBUpperLevel->putRasterBlock(baux,pages[b]->data_, parUp.blockHeight_*parUp.blockWidth_);
		}
		// reset level selection
		rst->clearBlockSelection();
		decMem->resetMemory();
		r *= 2;
	}
	delete decMem;
	delete decDBUpperLevel;
	for (b=0; b<rst->params().nBands(); ++b)
		delete pages[b];
	return true;
}
