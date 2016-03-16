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

#include <TeRaster.h>
#include <TeDecoder.h>
#include <TeUtils.h>
#include <TeStdFile.h>
#include <TeException.h>
#include "TeRasterRemap.h"
#include "TeDecoderMemory.h"
#include <TeInitRasterDecoders.h>
#include <TeVectorRemap.h>
#include <algorithm>


//!	virtual class that define the strategics of the iteratorPoly, each strategic is a functor
class TeStrategic
{
protected:
	TeRaster*		raster_;
	double			y_;
	TeCoordPairVect	SegOut_;   //line and column index 

public:
	TeStrategic(TeRaster* r=0, double y=0): raster_(r), y_(y) {}

    virtual ~TeStrategic() {}

	void Init(TeRaster* r, double y)
	{
		raster_ = r;
		y_ = y;
	}
	
	virtual void strateg(double xMin, double xMax, double y) = 0;  

	void operator() (TeCoordPair& pair)
	{
		//xmin and xmax of the segment (line and column index)
		TeCoord2D minSegLC = raster_->coord2Index(TeCoord2D(pair.pt1.x_, y_));
		TeCoord2D maxSegLC = raster_->coord2Index(TeCoord2D(pair.pt2.x_, y_));

		double xMinLCd = minSegLC.x_;
		double xMaxLCd = maxSegLC.x_;
		double yLC = maxSegLC.y_;
		
		//verify if is outside raster area 
		if(xMinLCd<-0.5)
		{
			if(xMaxLCd<-0.5)
				return;
			else
				xMinLCd=-0.5;
		}
		strateg(xMinLCd, xMaxLCd, yLC);
	}
	TeCoordPairVect result() const {return SegOut_;}
};


//!	functor TePixelBoxInPoly
/*
	return the elements of the raster which box is 
	contained in the polygon
*/
class TePixelBoxInPoly: public TeStrategic
{

public:
	TePixelBoxInPoly(TeRaster* r=0, double y=0): TeStrategic(r,y) {}

    virtual ~TePixelBoxInPoly() {}

    void strateg(double xMin, double xMax, double y)
	{
		//verify if contain the element center 
		int xMinLCi = TeRoundRasterIndex(xMin);
		int xMaxLCi = TeRoundRasterIndex(xMax);
		
		if (xMinLCi < xMin)
			xMinLCi++;

		if (xMaxLCi > xMax)
			xMaxLCi--;

		if (xMinLCi <= xMaxLCi)
		{
			//new segment 
			TeCoord2D minLC (xMinLCi, y);
			TeCoord2D maxLC (xMaxLCi, y);

			TeCoordPair res(minLC,maxLC);
			SegOut_.push_back (res);
		}
	}
	
};


//!	functor TePixelBBInterPoly
/*
	return the elements of the raster "cujo" boundingbox 
	intersects the polygon
*/
class TePixelBBInterPoly: public TeStrategic
{
	
public:
	TePixelBBInterPoly(TeRaster* r=0, double y=0): TeStrategic(r,y) {}

    virtual ~TePixelBBInterPoly() {}

	void strateg(double xMin, double xMax, double y)
	{		
		//element center 
		int xMinLCi = TeRoundRasterIndex(xMin);
		int xMaxLCi = TeRoundRasterIndex(xMax);

		if (SegOut_.size()>0)
		{
			TeCoordPair prev = SegOut_[SegOut_.size()-1];
			if (xMinLCi == prev.pt2.x())
				xMinLCi++;
			if (xMinLCi > xMaxLCi)
				return;
		}
		
		TeCoord2D minLC (xMinLCi, y);
		TeCoord2D maxLC (xMaxLCi, y);
			
		TeCoordPair res(minLC,maxLC);

		SegOut_.push_back (res);
	}
};


TeCoordPairVect 
applyStrategic(double& y, double ymin, double xmin, TeStrategicIterator st, TeRaster* raster, TePolygon& poly)
{
	TeCoordPairVect Segments = TeGetIntersections(poly, y);
	double resy = raster->params().resy_;

	//In polygon
	if((st==TeBoxPixelIn) || (st==TeBBoxPixelInters))
	{
		bool empty=false;
		if(Segments.empty())
		{
			empty = true;
			y -= resy; 

			while((y>=ymin) && (empty)) 
			{
				Segments = TeGetIntersections(poly, y);
				if(!Segments.empty())
					empty = false;
				else
					y -=resy;
			}
		}
		if(!empty)
		{
			if (st==TeBoxPixelIn)
			{
				TePixelBoxInPoly strat(raster,y);
				strat = for_each(Segments.begin (), Segments.end(), strat);
				return strat.result();
			}
			else
			{
				TePixelBBInterPoly strat(raster,y);
				strat = for_each(Segments.begin (), Segments.end(), strat);
				return strat.result();
			}
		}
	}
	//Out polygon
	else if((st==TeBoxPixelOut) || (st==TeBBoxPixelNotInters))
	{
		int nCols = raster->params().ncols_;
	
		//se o segmento for vazio, no h interseo entre essa linha e o polgono
		//fazer um segmento de todas as colunas
		if(Segments.empty())
		{
			//passar para indice: linha e coluna
			TeCoord2D coordCM (xmin, y);
			double linLC = (raster->coord2Index(coordCM)).y();
			
			int lin = TeRoundRasterIndex(linLC);
			
			TeCoord2D index1(0, lin);
			TeCoord2D index2(nCols-1,lin);
			
			TeCoordPair pair(index1,index2);

			Segments.push_back(pair);
			return Segments;
		}
		else
		{
			TeCoordPairVect segsIn, segResult;
			
			if(st==TeBoxPixelOut)
			{
				TePixelBoxInPoly strat(raster,y);
				//return the segments in the polygon
				strat = for_each(Segments.begin (), Segments.end(), strat);
				segsIn = strat.result();
			}
			else
			{
				TePixelBBInterPoly strat(raster,y);
				//return the segments in the polygon
				strat = for_each(Segments.begin (), Segments.end(), strat);
				segsIn = strat.result();
			}
			
			TeCoordPairVect::iterator it = segsIn.begin();

			double colMin = 0;
			double colMax = 0;
			double lin = 0;

			while(it!=segsIn.end())
			{
				TeCoord2D coord1 = (*it).pt1;
				TeCoord2D coord2 = (*it).pt2;
				lin = coord1.y();
				
				colMax = coord1.x()-1;

				if (colMax >= colMin)
				{
					TeCoord2D index1(colMin, lin);
					TeCoord2D index2(colMax, lin);
				
					TeCoordPair pair(index1,index2);

					segResult.push_back(pair);
				}
				colMin = coord2.x()+1;
				++it;
			}

			//Montar o ltimo segmento
			TeCoord2D index1(colMin, lin);
			TeCoord2D index2(nCols-1,lin);
			
			TeCoordPair pair(index1,index2);
			segResult.push_back(pair);
			return segResult;
		}
	}
	
	return Segments;
}



TeRaster::TeRaster( TeRasterParams& pars ) 
{
	TeInitRasterDecoders();
	decoder_ = 0;
	string decName;
	params_.status_ = TeRasterParams::TeNotReady;
	pars.errorMessage_ = "";
	if (pars.decoderIdentifier_.empty())   // the decoder is not specified in raster parameters
	{	
		if (!pars.fileName_.empty())     // try to guess from the file name
		{	
			string ext = TeConvertToUpperCase(TeGetExtension(pars.fileName_.c_str()));
			decName = TeDecoderFactory::instanceName2Dec()[ext];
			if ( decName.empty())
			{
				pars.errorMessage_ = "Can not instantiate a decoder from the raster file (or table) name.";
				return;
			}
			pars.decoderIdentifier_ = decName;
		}
		else // no decoder type, and no filename
		{
			pars.errorMessage_ = "Can not instantiate a decoder for the raster data.";
			return;
		}
	}
	decoder_ = TeDecoderFactory::make(pars);
	if (decoder_)
	{
		params_ = decoder_->params();
		box_ = decoder_->params().box();
	}
}

TeRaster::TeRaster( const string& filename, const char& mode ) 
{
	TeInitRasterDecoders();
	params_.fileName_ = filename;
	params_.mode_ = mode;
	decoder_ = 0;
	params_.status_ = TeRasterParams::TeNotReady;
	string decName;
	if (!filename.empty())
	{
		string ext = TeConvertToUpperCase(TeGetExtension(filename.c_str()));
		decName = TeDecoderFactory::instanceName2Dec()[ext];
		if (decName.empty())
		{
			params_.errorMessage_ = "Can not instantiate a decoder from the raster file (or table) name.";
			return;
		}
	}
	else
	{
		params_.errorMessage_ = "Can not instantiate a decoder for the raster data.";
		return;
	}
	params_.decoderIdentifier_= decName;
	decoder_ = TeDecoderFactory::make(params_);
	if (decoder_)
	{
		params_ = decoder_->params();
		box_ = decoder_->params().box();
	}
}


TeRaster::TeRaster(int ncols, int nlines, int nbands, TeDataType elemType)
{
	params_.decoderIdentifier_= "MEM";
	params_.ncols_ = ncols;
	params_.nlines_ = nlines;
	params_.nBands(nbands);
	params_.setDataType(elemType);
	params_.mode_ = 'c';
	params_.lowerLeftResolutionSize(0.5,0.5,1.0,1.0,params_.ncols_,params_.nlines_,true);
	TeProjection* proj = new TeNoProjection();
	params_.projection(proj);
	decoder_ = new TeDecoderMemory(params_);
	delete proj;
	if (decoder_)
	{
		params_ = decoder_->params();
		box_ = decoder_->params().box();
	}
}


TeRaster::~TeRaster( ) 
{
	if (decoder_)
		delete decoder_;
}

void TeRaster::updateParams( TeRasterParams& par)
{
	params_ = par;	// update raster parameters
	if (decoder_)
		decoder_->updateParams(par);		// update its decoder parameters
	box_ = params_.box();
}

void TeRaster::setDecoder( TeDecoder* dec )
{ 
	if (dec)
	{
		decoder_ = dec;					// set the decoder
		params_ = decoder_->params();	// copy decoder parameters to raster
		box_ = params_.box();
	}
}

void TeRaster::clear()
{
	if (decoder_)
		decoder_->clear();
}

TeRaster::iterator TeRaster::begin() 
{
	return iterator(0,0, params_.ncols_,params_.nlines_, params_.nBands(), this);
}

TeRaster::iterator TeRaster::end() 
{
	return iterator(params_.ncols_,params_.nlines_-1,params_.ncols_,params_.nlines_, params_.nBands(), this);
}

TeRaster::iteratorPoly TeRaster::end(TePolygon& poly, TeStrategicIterator st, int band) 
{
	return iteratorPoly(params_.ncols_,params_.nlines_-1,params_.ncols_,params_.nlines_, params_.nBands(), this, poly, st, band);
}

bool TeRaster::selectBlocks(TeBox& bb, int resFac, TeRasterParams& parBlock) 
{
	if (!decoder_)
		return false;
	return decoder_->selectBlocks(bb,resFac,parBlock);
}

int TeRaster::numberOfSelectedBlocks()
{
	if (!decoder_)
		return 0;
	return decoder_->numberOfSelectedBlocks();
}

bool TeRaster::fetchRasterBlock(TeDecoderMemory* memDec)
{
	if (!decoder_)
		return false;
	return decoder_->getSelectedRasterBlock(memDec);
}

void TeRaster::clearBlockSelection()
{
	if (!decoder_)
		return ;
	decoder_->clearBlockSelection();
}

void TeRaster::iterator::moveForward()
{
	this->operator++();
}

bool TeRaster::iterator::operator==(const iterator& rhs) const
{
	return (this->colCurrent_ == rhs.colCurrent_ && this->linCurrent_== rhs.linCurrent_);
}

bool TeRaster::iterator::operator!=(const iterator& rhs) const
{
	return (this->colCurrent_ != rhs.colCurrent_ || this->linCurrent_!= rhs.linCurrent_);
}

void TeRaster::iteratorPoly::moveForward()
{
	this->operator++();
}

bool TeRaster::init( TeRasterParams& pars )
{ 
	if (!decoder_ )	
	{
		params_.status_ = TeRasterParams::TeNotReady;
		string decName;
		if (params_.decoderIdentifier_.empty())   // the decoder is not especified in raster parameters
		{	
			if (!params_.fileName_.empty())     // try to guess from the file name
			{	
				string ext = TeConvertToUpperCase(TeGetExtension(params_.fileName_.c_str()));
				decName = TeDecoderFactory::instanceName2Dec()[ext];
				if ( decName.empty())
				{
					pars.errorMessage_ = "Can not instantiate a decoder from the raster file (or table) name.";
					return false;
				}
				params_.decoderIdentifier_ = decName;
			}
			else // no decoder type, and no filename
			{
				pars.errorMessage_ = "Can not instantiate a decoder for the raster data.";
				return false;
			}
		}
		decoder_ = TeDecoderFactory::make(params_);
		if (!decoder_)
		{
			pars.errorMessage_ = "Can not instantiate a decoder for the raster data.";
			return false;
		}
	}
	decoder_->init(pars);
	params_ = decoder_->params();
	box_ = decoder_->params().box();
	return (params_.status_ != TeRasterParams::TeNotReady);
}

bool TeRaster::init()
{ 
	if (decoder_)
	{
		decoder_->init(params_);
		params_ = decoder_->params();	
		return (params_.status_ != TeRasterParams::TeNotReady);
	}
	params_.errorMessage_ = "There is no decoder associated to the raster data.";
	return false;
}

bool TeRaster::setElement (int col, int lin, double val,int band)
{
	if (params_.status_!= TeRasterParams::TeReadyToWrite)
	{
		params_.errorMessage_ = "Raster is not properly initalizated to be written.";
		return false;
	}
	if ( col < 0 || col >= params_.ncols_ ||
		 lin < 0 || lin >= params_.nlines_ ||
		 band < 0 || band >= params_.nBands())
	{
		params_.errorMessage_ = "Fail to access element out of the range of the raster dimensions.";
		return false;
	}

	bool res = decoder_->setElement(col,lin,val,band);
	if ( res && (!params_.useDummy_ || val != params_.dummy_[band]))
	{
		// check if should update min and max values
		if (val < params_.vmin_[band])
		{
			params_.vmin_[band] = val;
			decoder_->params().vmin_[band] = val;
		}

		if (val > params_.vmax_[band])
		{
			params_.vmax_[band] = val;
			decoder_->params().vmax_[band] = val;
		}
	}
	return res;
}

bool TeRaster::getElement (int col, int lin, double& val,int band) 
{
	if (!params_.status_ || col < 0 || col >= params_.ncols_  || 
		 lin < 0 || lin >= params_.nlines_  ||
		 band < 0 || band >= params_.nBands())
	{
		params_.errorMessage_ = "Fail to access element out of the range of the raster dimensions.";
		return false;
	}

	if (decoder_->getElement (col,lin,val,band) && 
	   (!params_.useDummy_ || val != params_.dummy_[band]))	
		return true;
	else
		return false;
}

bool TeRaster::setElement(int col, int lin, double Rval, double Gval, double Bval, unsigned int transp) 
{
	if (params_.status_!= TeRasterParams::TeReadyToWrite)
	{
		params_.errorMessage_ = "Raster is not properly initalizated to be written.";
		return false;
	}
	if ( params_.nBands() != 3 ||
		 col < 0 || col >= params_.ncols_ ||
		 lin < 0 || lin >= params_.nlines_ )
	{
		params_.errorMessage_ = "Fail to access element out of the range of the raster dimensions.";
		return false;
	}
	return decoder_->setElementRGB(col, lin, Rval, Gval, Bval, transp);
}

TeRaster::iteratorPoly 
TeRaster::begin(TePolygon& poly, TeStrategicIterator st, int band)
{
	double	minLinLC, maxLinLC, minColLC, maxColLC; //index
	double	minLinCM, maxLinCM, minColCM, maxColCM; //world coordinates 
	int		nlines = 0; 
	int		ncols = 0;
	double	resy = params_.resy_;
	bool	end = false;

	//box (world coordinates) of the polygon
	TeBox boxPol = poly.box();
	if((st==TeBoxPixelOut) || (st==TeBBoxPixelNotInters))  //out polygon
	{
		minLinLC = (params_.nlines_-1);
		maxLinLC = 0.0;
		minColLC = 0.0;
		maxColLC = (params_.ncols_-1);
	}
	else  //in polygon
	{
		//change to line and column index
		TeCoord2D minColLinLC = coord2Index(boxPol.lowerLeft());
		TeCoord2D maxColLinLC = coord2Index(boxPol.upperRight());

		//segment that pass by center of the element  
		minLinLC = TeRound(minColLinLC.y());
		maxLinLC = TeRound(maxColLinLC.y());
		minColLC = TeRound(minColLinLC.x());
		maxColLC = TeRound(maxColLinLC.x());
		
		//number of lines and columns of the polygon box 
		nlines = (int)((minLinLC - maxLinLC)+1);
		ncols = (int)((maxColLC - minColLC)+1);


		//if is negative 
		if(minLinLC<-0.5)
		{
			if(maxLinLC<-0.5)
				end=true;
			else
				minLinLC = -0.5;
		}

		if(minColLC<-0.5)
		{
			if(maxColLC<-0.5)
				end=true;
			else
				minColLC = -0.5;
		}
	}
	
	TeCoord2D MinColLinLC(minColLC, minLinLC); 
	TeCoord2D MaxColLinLC(maxColLC, maxLinLC);

	//calculate the minimal and maximal line and minimal column (in world coordinates) 
	TeCoord2D MinColLinCM = index2Coord(MinColLinLC);
	minLinCM = MinColLinCM.y();
	minColCM = MinColLinCM.x();
	maxLinCM = index2Coord(MaxColLinLC).y();
	maxColCM = index2Coord(MaxColLinLC).x();

	TeCoordPairVect segRes;
	bool empty = true;
	bool first = true;
	
	while(empty && (maxLinCM >= minLinCM))  
	{
		if(!first)
		{
			maxLinCM -= resy;
			TeCoord2D maxColLinLC = coord2Index(TeCoord2D(maxColCM,maxLinCM));
			maxLinLC = TeRound(maxColLinLC.y());
		}
			
		segRes = applyStrategic(maxLinCM, minLinCM, minColCM, st, this, poly);
		empty = segRes.empty();
		first = false;
	}

	if(!segRes.empty())
	{
		minColLC = segRes[0].pt1.x();
		double lMin = segRes[0].pt1.y();
		maxColLC = segRes[0].pt2.x();
	
		int cCurr = TeRound(minColLC);  //column current
		int lCurr = TeRound(lMin);		//line current

		iteratorPoly itPoly(cCurr, lCurr, params_.ncols_, params_.nlines_, params_.nBands(), this, poly, 
						st, maxLinLC, minLinLC, minColLC, maxColLC, segRes, 
						0, nlines, ncols, end, minLinCM);
		return itPoly;
	
	}
	else
	{
		iteratorPoly itPoly = this->end(poly, st, band);
		return itPoly;
	}
}


bool
TeRaster::fillRaster(TeRaster* dstRaster, TeRasterTransform* transf, bool bestRes)
{	
	if (!dstRaster ||
		 dstRaster->params().status_ != TeRasterParams::TeReadyToWrite)
	{
		params_.errorMessage_ = "Destination raster non-existing or not ready to write";
		return false;
	}

	if (params_.status_ != TeRasterParams::TeReadyToRead &&
		params_.status_ != TeRasterParams::TeReadyToWrite)
	{
		params_.errorMessage_ = "Raster not ready to be extracted";
		return false;
	}

	int dt = CLOCKS_PER_SEC/4;
	int dt2 = CLOCKS_PER_SEC * 5;
	clock_t	t0, t1, t2;

	TeRasterRemap fillRemap;
	fillRemap.setOutput(dstRaster);
	fillRemap.setInput(this);
	if (transf)
		fillRemap.setTransformer(transf);
	int res = 1;
	TeBox b = dstRaster->params().boundingBox();
	if (bestRes)
	   res = decoder_->bestResolution(b, dstRaster->params().nlines_, dstRaster->params().ncols_,
									  dstRaster->params().projection());
	this->clearBlockSelection();
	bool result = true;
	TeRasterParams parBlock;
	if (selectBlocks(b, res, parBlock))
	{
		if (TeProgress::instance())
			TeProgress::instance()->setTotalSteps(this->numberOfSelectedBlocks());
		TeRaster* block = new TeRaster;
		TeDecoderMemory* decMem = new TeDecoderMemory(parBlock);
		decMem->init();
		int n = 0;
		t2 = clock();
		t0 = t1 = t2;
		bool flag = true;
		do
		{
			flag = fetchRasterBlock(decMem);
			block->setDecoder(decMem);
			fillRemap.setInput(block);
			if (!fillRemap.apply())
				break;
			n++;
			if (TeProgress::instance())
			{
				t2 = clock();
				if (int(t2-t1) > dt)
				{
					t1 = t2;
					if (TeProgress::instance()->wasCancelled())
						break;
					if((int)(t2-t0) > dt2)	// show progress 
						TeProgress::instance()->setProgress(n);
				}
			}
		}while (flag);
		decMem->clear();
		delete block;
	}
	else
		result =  fillRemap.apply();
	this->clearBlockSelection();
	if (TeProgress::instance())
		TeProgress::instance()->reset();
	return result;
}

//! Prefix move forward operator
TeRaster::iterator& 
TeRaster::iterator::operator++()
{
	if (++colCurrent_ == nCols_)
	{
		if (linCurrent_ < nLines_-1)
		{
			linCurrent_++;
			colCurrent_ = 0;
		}
	}
	return *this;
}

TeRaster::iterator
TeRaster::iterator::operator++(int)	
{
	iterator temp = *this;
	++(*this);
	return temp;
}

vector<double> 
TeRaster::iterator::operator*()
{
	vector<double> vt(nBands_);
	if ((linCurrent_ < nLines_) && (colCurrent_ < nCols_))
	{
		for (int n=0; n<nBands_; n++)
			raster_->getElement(colCurrent_,linCurrent_,vt[n],n);
	}
	return vt;
}

double 
TeRaster::iterator::operator*(int band)
{
	double val = this->raster_->params().dummy_[band];
	if ((linCurrent_ < nLines_) && (colCurrent_ < nCols_))
	{
		if (raster_->getElement(colCurrent_,linCurrent_,val,band))
			return val;
	}
	return val;
}

void 
TeRaster::iteratorPoly::getNewSegment(int linCurr)
{
	//change to world coordinates
	TeCoord2D coord(colMin_,linCurr);
	TeCoord2D colLinCM = raster_->index2Coord(coord);
	
	double linCM = colLinCM.y();
	double colMinCM = colLinCM.x();
	
	//applyStrategic: return the segments 
	segments_ = applyStrategic(linCM, linMinCM_, colMinCM, strategy_, raster_, poly_);

	if(segments_.empty())
	{
		colCurrent_=(int)colMax_;
		return;
	}
	
	colMin_ = segments_[0].pt1.x();
	colMax_ = segments_[0].pt2.x();
	
	colCurrent_=(int)colMin_;
	posSegments_ = 0;
	end_ = false;
}

//! Prefix move forward operator
TeRaster::iteratorPoly& 
TeRaster::iteratorPoly::operator++() //prefix
{
	if (++colCurrent_>colMax_)
	{
		if((++posSegments_ > (int)(segments_.size()-1)) || (segments_.size()<1))
		{
			if(++linCurrent_>linMax_)
			{
				end_ = true;
				*this = raster_->end(poly_,strategy_);
			}
			else
			{	
				segments_.clear();
				do
				{
					getNewSegment(linCurrent_);
					if (segments_.empty())
						linCurrent_++;
					else
						break;
				} while (linCurrent_<=linMax_);
				if (linCurrent_ > linMax_)
				{
					end_ = true;
					*this = raster_->end(poly_,strategy_);
				}
			}
		}
		else
		{
			colMin_ = segments_[posSegments_].pt1.x();
			colMax_ = segments_[posSegments_].pt2.x();
			colCurrent_=(int)colMin_;
		}
	}
	return *this;
}

TeRaster::iteratorPoly 
TeRaster::iteratorPoly::operator++(int)
{
	iteratorPoly temp = *this;
	++(*this);
	return temp;
}

double 
TeRaster::iteratorPoly::operator*() 
{
	double val = 0.0;
	if ((linCurrent_ < nLines_) && (colCurrent_ < nCols_))
	{
		if (raster_->getElement(colCurrent_,linCurrent_,val,band_))
			return val;
	}
	return 0.0;
}

//! Returns the value of a given band of the element pointed by the iterator 
double 
TeRaster::iteratorPoly::operator*(int band)
{
	double val = 0.0;
	if ((linCurrent_ < nLines_) && (colCurrent_ < nCols_))
	{
		if (raster_->getElement(colCurrent_,linCurrent_,val,band))
			return val;
	}
	return 0.0;
}

bool 
TeRaster::iteratorPoly::getProperty(TeProperty& prop) 
{
	double val = 0.0;
	prop.attr_.rep_.type_ = TeREAL;
	if (!raster_) prop.attr_.rep_.name_ = raster_->params().fileName_ + Te2String(band_);
	prop.value_ = Te2String (val);
	
	if ((linCurrent_ < nLines_) && (colCurrent_ < nCols_))
	{
		if (raster_->getElement(colCurrent_,linCurrent_,val,band_))
		{
			prop.value_ = Te2String(val);
			return true;
		}
	}
	return false;
}

TeRaster*
TeRasterClipping(TeRaster* whole, TePolygonSet& mask, TeProjection* geomProj, const string& clipName, double background, const string& decId)
{
  if( decId.empty() ) {
    throw TeException( UNKNOWN_DECODER_TECHNOLOGY_TYPE, "Invalid decoder", 
      false );
  }
  
  if( whole == 0 ) {
    throw TeException( UNKNOWN_ERROR_TYPE, "Invalid raster pointer", 
      false );
  }  

  if( mask.empty() ) {
    throw TeException( UNKNOWN_ERROR_TYPE, "Invalid polygon set mask", 
      false );
  }  
  
  if( ( decId != "MEM" ) && clipName.empty() ) {
    throw TeException( UNKNOWN_ERROR_TYPE, "Invalid clipname", 
      false );
  }    
    
  if( geomProj == 0 ) {
    throw TeException( UNKNOWN_ERROR_TYPE, "Invalid projection pointer", 
      false );
  }    

	if (whole->params().status_ != TeRasterParams::TeReadyToRead && 
		whole->params().status_ != TeRasterParams::TeReadyToWrite )
		return 0;
	
	bool isEqualProj = (*whole->projection() == *geomProj);
	if (!isEqualProj &&
		(geomProj->name() == "NoProjection" || whole->projection()->name() == "NoProjection"))
		return 0;

	TePolygonSet mask2;
	if (!isEqualProj)
		TeVectorRemap(mask,geomProj,mask2,whole->projection());
	else
		mask2 = mask;

	if (!TeIntersects(mask2.box(),whole->params().boundingBox()))
		return 0;

	TeRasterParams par = whole->params();
	if (!decId.empty())
	{
		par.decoderIdentifier_ = decId;
		if (decId == "DB")
		{
			if (!par.database_)
				return 0;
		}
	}
	par.fileName_ = clipName;
	par.mode_ = 'c';
	TeCoord2D bll = whole->coord2Index(mask2.box().lowerLeft());
	TeCoord2D bur = whole->coord2Index(mask2.box().upperRight());
	
	bll = whole->index2Coord(TeCoord2D(TeRoundRasterIndex(bll.x_),TeRoundRasterIndex(bll.y_)));
	bur = whole->index2Coord(TeCoord2D(TeRoundRasterIndex(bur.x_),TeRoundRasterIndex(bur.y_)));

	par.boxResolution(bll.x_, bll.y_, bur.x_, bur.y_,par.resx_, par.resy_);
	par.setDummy(background);
	TeRaster*  clip = new TeRaster(par);
	clip->init();
	if (clip->params().status_ != TeRasterParams::TeReadyToWrite)
		return 0;
	
	TeCoord2D cd = TeCoord2D(0,0);
	TeCoord2D ul = clip->params().index2Coord(cd);
	TeCoord2D dxdy = whole->params().coord2Index(ul);
	int j = TeRoundRasterIndex(dxdy.x_); // column delta
	int i = TeRoundRasterIndex(dxdy.y_); // line delta
	unsigned int np = mask2.size();
	unsigned int nb = whole->params().nBands();
	unsigned int band;
	double val;

	//progress bar
	if(TeProgress::instance())
	{
		string caption = "Raster Clipping operation";
		TeProgress::instance()->setCaption(caption.c_str());
		string msg = "Executing clipping operation. Please, wait!";
		TeProgress::instance()->setMessage(msg);
		TeProgress::instance()->setTotalSteps(np);
	}
	for (unsigned int p=0; p<np; ++p)
	{
		TeRaster::iteratorPoly it = whole->begin(mask2[p],TeBoxPixelIn);
		while (!it.end())
		{
			for(band = 0; band < nb; ++band)
			{
				if (whole->getElement(it.currentColumn(),it.currentLine(),val,band))
					clip->setElement(it.currentColumn()-j,it.currentLine()-i,val,band);
			}
			++it;
		}
		//progress bar
		if(TeProgress::instance())
		{
			if(TeProgress::instance()->wasCancelled())
			{
				TeProgress::instance()->reset();
				break;
			}
			TeProgress::instance()->setProgress(p);
		}
	}
	if(TeProgress::instance())
		TeProgress::instance()->reset();
	clip->clear();
	clip->params().mode_ = 'r';
	clip->init();
	return clip;
}
