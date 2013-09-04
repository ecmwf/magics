/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include "TeDecoderDatabase.h"
#include "TeDecoderMemory.h"
#include "TeRasterRemap.h"
#include "TeVectorRemap.h"
#include "TeDataTypes.h"
#include "TeProgress.h"

#include <zlib.h>

#ifdef TEJPEG //FAMI
#include "TeLibJpegWrapper.h"
#endif

TeDecoderDatabase::TeDecoderDatabase(const TeRasterParams& par ):
	TeDecoderVirtualMemory(par),
	db_(par.database_),
	blockPortal_(0),
	memAux_(0),
	nSelectedBlocks_(0)
{
	params_ = par;
	params_.decoderIdentifier_ = "DB";
	params_.errorMessage_.clear();
}

TeDecoderDatabase::~TeDecoderDatabase()
{
	if (blockPortal_)
	{
		delete blockPortal_;
		blockPortal_ = 0;
	}
	clear();
}

bool
TeDecoderDatabase::create()
{
	if (db_ == 0)
		return false;

	// Create raster table in database
  	if (!db_->createRasterTable(params_.fileName_))
		return false;
	return true;
}

void
TeDecoderDatabase::init()
{
	if (blockPortal_)
	{
		blockPortal_->freeResult();
		delete blockPortal_;
		blockPortal_ = 0;
	}
	clear();
	params_.status_= TeRasterParams::TeNotReady;
	if (db_ == 0)			// there is not valid database connection
	{
		params_.errorMessage_ = "There is no database pointer associated to raster.";
		return;
	}

	if (params_.mode_ == 'c')
	{
		if (db_->tableExist(params_.fileName_))
		{
			string sql = "DROP TABLE " + params_.fileName_;
			db_->execute(sql);
		}
  		if (!db_->createRasterTable(params_.fileName_))
		{
			params_.errorMessage_ = "Fail to create the raster table: " + params_.fileName_;
			return;
		}
		else 
			params_.status_ = TeRasterParams::TeReadyToWrite;
	}
	else if (params_.mode_ == 'w')
	{
		if (db_->tableExist(params_.fileName_))
			params_.status_ = TeRasterParams::TeReadyToWrite;
	}
	else if (params_.mode_ == 'r')
	{
		if (db_->tableExist(params_.fileName_))
			params_.status_ = TeRasterParams::TeReadyToRead;		
	}
	TeDecoderVirtualMemory::init();
}

bool
TeDecoderDatabase::clear()
{
	if (db_ == 0)
		return false;
	TeDecoderVirtualMemory::clear();
	if (blockPortal_)
	{
		blockPortal_->freeResult();
	}
	if (memAux_)
		delete []memAux_;
	memAux_ = 0;
	params_.status_ = TeRasterParams::TeNotReady;
	if (params_.mode_ == 'c' || params_.mode_ == 'w' && 
	   (!params_.objectId_.empty() && params_.layerId_ > 0))
	{
		TeDatabasePortal* portal = db_->getPortal();
		string sql = "SELECT geom_table FROM te_representation WHERE geom_type = 512";
		sql += " AND layer_id = " + Te2String(params_.layerId_);
		if (portal->query(sql) && portal->fetchRow())
		{
			string tName = portal->getData(0);
			portal->freeResult();
			sql = "SELECT geom_id FROM " + tName + " WHERE object_id = '" + params_.objectId_ + "'";
			if (portal->query(sql) && portal->fetchRow())
			{
				tName += "_metadata";
				db_->updateRasterMetadata(tName,portal->getInt(0),params_);
			}
		}
		delete portal;
	}
	return true;
}

string 
TeDecoderDatabase::codifyId(int col, int lin, int band, int res, int subb)
{
	return codifyId(blockIndex(col, lin, band), res, subb);
}

string TeDecoderDatabase::codifyId(const TeBlockIndex& idx, int res, int subb)
{
	char id[32];
	sprintf(id, "X%dY%dB%dR%dS%d", idx.col_, idx.lin_, idx.band_, res, subb);
	return string(id);
}


TeBlockIndex 
TeDecoderDatabase::blockIndex(int col, int lin, int band)
{
	if (params_.tiling_type_ != TeRasterParams::TeExpansible)
		return TeDecoderVirtualMemory::blockIndex(col, lin, band);

	TeBlockIndex bl_idx;
	bl_idx.band_ = band;

	TeCoord2D tmpCoord = TeCoord2D(col,lin);
	TeCoord2D xy = params_.index2Coord(tmpCoord);

	double bXSize = params_.blockWidth_*params_.resx_;
	double bYSize = params_.blockHeight_*params_.resy_;

	if ( xy.x() < 0)
		bl_idx.col_ = (int)(xy.x()/bXSize - 1);
	else
		bl_idx.col_ = (int)(xy.x()/bXSize);

	if (xy.y() < 0)
		bl_idx.lin_ = (int) (xy.y()/bYSize-1);
	else
		bl_idx.lin_ = (int) (xy.y()/bYSize);

	return bl_idx;
}

void 
TeDecoderDatabase::decodifyId(const string& id, int& col,int& lin, int& band, int& res, int& subb)
{
	int r, s;
	TeBlockIndex bl_idx;
	sscanf(id.c_str(),"%*c%d%*c%d%*c%d%*c%d%*c%d", 
		&bl_idx.col_, &bl_idx.lin_, &bl_idx.band_, &r, &s);
	blockIndexPos(bl_idx, col, lin, band);
	res = r;
	subb = s;
}

void TeDecoderDatabase::blockIndexPos( const TeBlockIndex& index, int& ulCol, int& ulLin, int& band)
{
	if (params_.tiling_type_ != TeRasterParams::TeExpansible)
		return TeDecoderVirtualMemory::blockIndexPos(index, ulCol, ulLin, band);

	band = index.band();

	double bXSize = params_.blockWidth_*params_.resx_;
	double bYSize = params_.blockHeight_*params_.resy_;

	TeCoord2D xy(index.column()*bXSize+params_.resx_/2,index.line()*bYSize+params_.resy_/2);
	xy = params_.coord2Index(xy);
	ulLin = TeRound(xy.y())-params_.blockHeight_+1;
	ulCol = TeRound(xy.x());
}


bool TeDecoderDatabase::getRasterBlock(const TeBlockIndex& index, void *block)
{
	string index_str;
	index_str = codifyId(index, params_.resolution_);
	return getRasterBlock(index_str, block);
}

bool 
TeDecoderDatabase::getRasterBlock(const string& index, void *block)
{
	TeDatabasePortal* portal = db_->getPortal();
	if (!portal) 
		return false;
	
	string q;					// try to get the tile from databatase
	q ="SELECT * FROM " + params_.fileName_ + " WHERE block_id='" + index + "'";

	if (!portal->query(q))		// error submting query 
	{
		delete portal;
		return 0;
	}
	int band=0;
	if (!portal->fetchRow())	// tile is not in the database
	{
		delete portal;
		return 0;
	}

	unsigned char*  memread_ptr = 0;
	if(params_.compression_[band] != TeRasterParams::TeNoCompression)
	{
		if(!memAux_)
		{
			long size = params_.blockHeight_ * params_.blockWidth_ * params_.elementSize();
			memAux_ = new unsigned char[size];
		}
		memread_ptr = memAux_;
	}
	else
		memread_ptr = reinterpret_cast<unsigned char*>(block);

	// tile is already in the database
	unsigned long blobLen;
	if (!portal->getRasterBlock(blobLen,memread_ptr))
	{
		delete portal;
		return false;
	}

	// decompress blob into a tile (previously allocated) 
	if (params_.compression_[band] == TeRasterParams::TeZLib)		// zlib compression
	{
		unsigned long blockLen = params_.blockHeight_ * params_.blockWidth_ * params_.elementSize();	// size after decompression
		uncompress (reinterpret_cast<unsigned char*>(block),&blockLen,memread_ptr,blobLen);
	}
#ifdef TEJPEG //FAMI
	else if (params_.compression_[band] == TeRasterParams::TeJpeg) //jpeg compression
	{
		int nb;
		if (!Jpeg::DecompressBuffer(memAux_,blobLen,reinterpret_cast<unsigned char*>(block),params_.blockWidth_,params_.blockHeight_,nb))
		{
			delete portal;
			return false;
		}
	}
#endif
	delete portal;
	return true;
}

bool 
TeDecoderDatabase::putRasterBlock(const TeBlockIndex& index, void *block, long bsize)
{
	string index_str;
	index_str = codifyId(index, params_.resolution_);
	return putRasterBlock(index_str, block, bsize);
}

bool 
TeDecoderDatabase::putRasterBlock(const string& index, void *block, long bsize)
{
	if (db_ == 0 || index.empty())
		return false;

	// decodify tile parameters from index
	int res, subb, band, blin, bcol;
	decodifyId(index,bcol,blin,band,res, subb);

	// calculates the box coordinates of the tile
	TeCoord2D llt(bcol, blin+params_.blockHeight_-1);
	TeCoord2D urt(bcol+params_.blockWidth_-1, blin);
	TeCoord2D ll = params_.index2Coord(llt);
	TeCoord2D ur = params_.index2Coord(urt);

	TeBox blockbb(ll.x()-params_.resx_/2.,ll.y()-params_.resy_/2.,ur.x()+params_.resx_/2.,ur.y()+params_.resy_/2.);
	bool status;

	// verify compression option
	if (params_.compression_[band] == TeRasterParams::TeNoCompression)
	{
		status = db_->insertRasterBlock(params_.fileName_,index,blockbb.lowerLeft(),blockbb.upperRight(),reinterpret_cast<unsigned char*>(block),bsize,band,res,subb);
		return status;
	}
	unsigned long finalsize = bsize;  // final size after compression
	if (!memAux_)
	{
		long size = params_.blockHeight_ * params_.blockWidth_ * params_.elementSize();
		memAux_ = new unsigned char[int(size*1.1 + 12.)];
	}
	if (params_.compression_[band] == TeRasterParams::TeZLib)		// ZLib compression
	{
		// zlib needs some more space)
		finalsize = (unsigned long)(bsize*1.1 + 12.);
		compress (memAux_, &finalsize,reinterpret_cast<unsigned char*>(block),bsize);
		return db_->insertRasterBlock(params_.fileName_,index,blockbb.lowerLeft(),blockbb.upperRight(),memAux_,finalsize,band,res,subb);
	}
#ifdef TEJPEG //FAMI
	else if (params_.compression_[band] == TeRasterParams::TeJpeg)	// JPEG compression
	{
		int sizec = int(bsize*1.1 + 12.);
		if (!Jpeg::CompressToBuffer(reinterpret_cast<unsigned char*>(block),params_.blockWidth_,params_.blockHeight_,1,memAux_,sizec,75))
			return false;
		finalsize = sizec;
		return db_->insertRasterBlock(params_.fileName_,index,blockbb.lowerLeft(),blockbb.upperRight(),memAux_,finalsize,band,res,subb);
	}
#endif
	return false;
}

bool
TeDecoderDatabase::saveLUTTable()
{
	if (params_.lutName_.empty())
		return false;
	if (!db_ || !db_->createLUTTable(params_.lutName_))
		return false;
	for (unsigned int i=0; i<params_.lutb_.size(); i++)
	{
		string sql = "INSERT INTO " + params_.lutName_ + " VALUES(";
		sql += Te2String(i) + ", ";
		sql += Te2String(params_.lutr_[i]) + ", ";
		sql += Te2String(params_.lutg_[i]) + ", ";
		sql += Te2String(params_.lutb_[i]) + ")";
		if (!db_->execute(sql))
			return false;
	}
	return true;
}

bool
TeDecoderDatabase::selectBlocks(TeBox& bb, int resFac, TeRasterParams& parBlock)  
{ 
	if (!db_)
		return false;

	if (blockPortal_)
		delete blockPortal_;

	blockPortal_ = db_->getPortal();
	if (!blockPortal_)
		return false;

	string sql;
	string sel = db_->getSQLBoxSelect(params_.fileName_, TeRASTER);
	string where = db_->getSQLBoxWhere (bb, TeRASTER);

	sql = "SELECT "+ sel;
	sql += " FROM " + params_.fileName_;
	sql += " WHERE " + where + " AND resolution_factor = " + Te2String(resFac);
	sql += " ORDER BY lower_x, lower_y, upper_x, upper_y, block_id";
	
	if (!blockPortal_->query(sql))
	{
		delete blockPortal_;
		blockPortal_ = 0;
		return false;
	}
	nSelectedBlocks_ = blockPortal_->numRows();
	parBlock.projection(params_.projection());
	parBlock.resx_ = params_.resx_*resFac;
	parBlock.resy_ = params_.resy_*resFac;
	parBlock.nlines_ = params_.blockHeight_;
	parBlock.ncols_= params_.blockWidth_;
	parBlock.nBands(params_.nBands());
	parBlock.dataType_ = params_.dataType_;
	parBlock.dummy_ = params_.dummy_;
	parBlock.useDummy_ = params_.useDummy_;
	parBlock.photometric_ = params_.photometric_;
	parBlock.vmax_ = params_.vmax_;
	parBlock.vmin_ = params_.vmin_;
	parBlock.lutr_ = params_.lutr_;
	parBlock.lutg_ = params_.lutg_;
	parBlock.lutb_ = params_.lutb_;
	parBlock.interleaving_ = TeRasterParams::TePerBand;
	return blockPortal_->fetchRow();
}

bool
TeDecoderDatabase::getSelectedRasterBlock(TeDecoderMemory* memDec) 
{ 
	if (!blockPortal_)
		return 0;
	unsigned long srcLen;
	int i = 0;

	if (!memAux_ )
	{
		long size = params_.blockHeight_ * params_.blockWidth_ * params_.elementSize();
		memAux_ = new unsigned char[size];
	}
	int res;
	TeBox bbBlock;
	string blockId;
	do
	{
		double lx = 0;
		double ly = 0;
		double ux = 0;
		double uy = 0; 
		lx = blockPortal_->getDouble("lower_x");
		ly = blockPortal_->getDouble("lower_y");
		ux = blockPortal_->getDouble("upper_x");
		uy = blockPortal_->getDouble("upper_y");

		bbBlock = TeBox(lx, ly, ux, uy);

		blockId = blockPortal_->getData("block_id");
		res = blockPortal_->getInt("resolution_factor");

	int banda;
    if(params_.nBands() > 1)
    {
		  int col, lin, res, subb;
		  this->decodifyId(blockId, col, lin, banda, res, subb);
    }
	else
      banda = 0;
    
	if (params_.compression_[banda] == TeRasterParams::TeNoCompression)
		  blockPortal_->getRasterBlock(srcLen, reinterpret_cast<unsigned char*>(memDec->data(banda)));
    else
    {
		blockPortal_->getRasterBlock(srcLen,memAux_);

		if (params_.compression_[banda] == TeRasterParams::TeZLib)
		{
			TeRasterParams& par = memDec->params();
			unsigned long destLen = par.ncols_ * par.nlines_ * par.elementSize();
			int status;
			status = uncompress (reinterpret_cast<unsigned char*>(memDec->data(banda)),&destLen,memAux_,srcLen);
		}
#ifdef TEJPEG //FAMI
		else if (params_.compression_[banda] == TeRasterParams::TeJpeg)
		{
			int nb = 1;
			Jpeg::DecompressBuffer(memAux_,srcLen,reinterpret_cast<unsigned char*>(memDec->data(banda)),params_.blockWidth_,params_.blockHeight_,nb);
		}
#endif
    }
		i++;
	}while (i<params_.nBands() && blockPortal_->fetchRow());
	
	memDec->params().boundingBoxResolution(bbBlock.x1(),bbBlock.y1(),bbBlock.x2(),bbBlock.y2(),
		                                   params_.resx_*res,params_.resy_*res);
	memDec->params().blockId_ = blockId;
	return blockPortal_->fetchRow();
}

void
TeDecoderDatabase::clearBlockSelection() 
{
	if (blockPortal_)
		delete blockPortal_;
	blockPortal_ = 0;
	nSelectedBlocks_ = 0;
}

int 
TeDecoderDatabase::bestResolution(TeBox& bb, int ncols, int nlines, TeProjection* proj)
{
	TeBox box = bb;
	if (proj)
		box = TeRemapBox(bb, proj, params_.projection());
	
	double resx = box.width()/ncols;
	double resy = box.height()/nlines;
	
	double auxx = resx/params_.resx_;
	double auxy = resy/params_.resy_;

	int aux = (int) (min(auxx,auxy));

	string sql = "SELECT resolution_factor FROM " + params_.fileName_;
	sql += " WHERE resolution_factor <= " + Te2String(aux) + " ORDER BY resolution_factor DESC";

	TeDatabasePortal* portal = params_.database_->getPortal();
	if (!portal)
		return 1;

	if (!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return 1;
	}

	int res = atoi(portal->getData(0));
	delete portal;
	return res;
}


