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
/*! \file TeRaster.h
    \brief This file contains structures and definitions to deal with raster structures
*/
#ifndef  __TERRALIB_INTERNAL_RASTER_H
#define  __TERRALIB_INTERNAL_RASTER_H

#include "TeDecoder.h"
#include "TeDataTypes.h"
#include "TeCoord2D.h"
#include "TeAttribute.h"
#include "TeGeometry.h"
#include "TeGeometryAlgorithms.h"

class TeRasterTransform;

#include <map>

//! Strategy used to decide if a pixel is inside/outside a polygon
enum TeStrategicIterator
{	TeBoxPixelIn,			//!< center of pixel is inside the geometry
	TeBBoxPixelInters,		//!< bounding box of pixel intercepts the geometry
	TeBoxPixelOut,			//!< center of pixel is outside the geometry
	TeBBoxPixelNotInters	//!< bounding box of pixel do not intercept the geometry
};

/*! \fn bool TeCoordPairVect applyStrategic(double& y, double ymin, double xmin, TeStrategicIterator st, TeRaster* raster, TePolygon& poly);
    \brief Returns  
	\param y         
	\param ymin         
	\param xmin         
	\param st pixel in polygon strategy       
    \param raster pointer to raster
	\param poly polygon        
	*/
TL_DLL TeCoordPairVect applyStrategic(double& y, double ymin, double xmin, 
			   TeStrategicIterator st, TeRaster* raster, TePolygon& poly);

//! Support for a raster geometry 
/*
	\par A Raster Geometry is made of a matrix of ncolumns x nlines elements
	with nBands dimension. Each element, or pixel, of the raster its is
	indexed by a column, line and band and has a double value associated.
	\par Each Raster geometry has a decoder associated to it. Its decoder 
	knows how to get return the value (as a double) of each pixel.
	\par All information about the data is stored in a TeRasterParams structure
	that is duplicated in its decoder.
	\sa TeDecoder TeRasterParams
*/
class TL_DLL TeRaster : public TeGeometry
{
	TeRasterParams  params_;	//!< structure that describes all information about a raster 
	TeDecoder		*decoder_;	//!< pointer to a decoder of values

public:
	//! Constructor from file
	/*!
	  This method tries to instatiate a decoder for this data from the 
	  file extension of the filename. 
      \param filename the name of a file that contains the raster data
      \param mode file opening mode ('r', 'w' or 'c')
	  \par Obs: the method init should be called before any attempt to read or write this raster data 
	*/
	TeRaster(const string& filename, const char& mode = 'r');

	//! Constructor from parameters
	/*!
	  This method tries to instatiate a decoder for this data. It uses the identification
	  explicitly described or the extension of a file name specified in the parameters.
      \param pars a raster parameters structure that contains information about the raster
	  \par Obs: the method init should be called before any attempt to read or write data 
	  to the raster.
	*/
	TeRaster(TeRasterParams& pars);

	//! Contructor from commom parameters
	/*
		This method creates a raste data in memory, using only the most basic
		parameters:
		\par ncols number of columns
		\par nlines number of lines
		\par nbands number of bands
		\par elemType size of each element
	*/
	TeRaster(int ncols, int nlines, int nbands, TeDataType elemType);

	//! Empty constructor
	TeRaster() : decoder_(0) {}

	//! Destructor
	~TeRaster ();

	//! Returns the parameters of the raster
	TeRasterParams& params();
	
	//! Returns the number of lines of the raster
	int nBands();

	//! Update the parametes of a raster file
	/*!
      \param par a new raster parameters structure
	*/
	void updateParams(TeRasterParams& par);

	//! Returns the type of the geometry
	TeGeomRep elemType();

	//! Associate a decoder to a raster
	/*!
      \param dec pointer to a raster decoder
	*/
	void setDecoder(TeDecoder* dec);

	//! Returns status of the raster as a Boolean value
	/*
		\return true if raster is ready to be read or written and false otherwise
	*/
	bool status();

	//! Returns a pointer to the decoder associated to this raster
	TeDecoder* decoder();

	//! Returns a pointer to the raster projection
	TeProjection* projection(); 

	//! Sets the value of a element of the raster 
	/*!
      \param col column number
	  \param lin line number
	  \param val element value
	  \param band band number
	*/
	bool setElement (int col, int lin, double val, int band=0);

	//! Gets the value of a element of the raster 
	/*!
      \param col column number
	  \param lin line number
	  \param val element value
	  \param band band number
	*/
	bool getElement (int col, int lin, double& val, int band=0);

	//! Fills a destination raster with the raster elements
	/*
		\param dstRaster destination raster
		\param transf pixel evaluator to be used
		\param bestRes flag to indicate that it should use the best resolution available
	*/
	bool fillRaster (TeRaster* dstRaster, TeRasterTransform* transf=0, bool bestRes = true);

	//! Initalize the raster decoding tool from its parameters
	/*! The result of this method depends on the mode_ specified in the parameters. 
		\par 'r' : initializes a raster with reading only permission. If the source
		of the data does not exist it will fail.
		\par 'w' : initializes a raster with reading and writting permission. If the source
		of the data does not exist it will fail.
		\par 'c' : creates a new raster data with default values. Initializes it with 
		reading and writting permision. Fails if creation fails.
	*/
	bool init();

	//! Initalize the raster decoding tool from a raster parameters structure
	/*!
		\return true if raster is read to be read/written and false otherwise.
	*/
	bool init(TeRasterParams& pars);
  
	//! Clear internal structures and disable the raster decoding tool
	void clear();

	//! Transform a coordinate from line/column domain to projection domain
	TeCoord2D index2Coord(TeCoord2D pt);

	//! Transform a coordinate from projection domain domain to line/column
	TeCoord2D coord2Index(TeCoord2D pt);

	//----------------
	//! A raster iterator
	/*! 
		An strucuture that allows the traversal over the raster elements 
		(pixels) in a similar way as the STL iterators. 
	*/
	class TL_DLL iterator
	{
		public:
		//! Constructor
		/*!
			\param c0 initial column
			\param l0 initial line
			\param nc number of columns
			\param nl number of lines
			\param nb number of bands
			\param pt pointer to the raster to iterate
		*/
			iterator(int c0=0, int l0=0, int nc=0, int nl=0, int nb=0, TeRaster* pt=0) : 
			linCurrent_(l0),
			colCurrent_(c0),
			nLines_(nl),
			nCols_(nc),
			nBands_(nb),
			raster_(pt)
			{}
			
			//! Destructor
			virtual ~iterator()
			{}

			//! Move iterator forward
			virtual void moveForward();

			//! Prefix move forward operator
			iterator& operator++();
		
			//! Posfix move forward operator
			iterator operator++(int);

			//! Returns iterator current line 
			int currentLine();

			//! Returns iterator current column
			int currentColumn();

			//! Returns the number of bands
			int nBands(void);

			//! Iterator de-referencing operator
			/*!
				\return a vector (with nbands_ size) with the values in
				each band, of the element pointed by the iterator
			*/
			vector<double> operator*();
			
			//! Returns the value in a given band of a element pointed by the iterator 
			double operator*(int band);

			//! Returns whether two iterators point to the same element
			bool operator==(const iterator& rhs) const;

			//! Returns whether two iterators point to the different elements
			bool operator!=(const iterator& rhs) const;

		protected:
			int linCurrent_, colCurrent_;
			int nLines_, nCols_;
			int nBands_;
			TeRaster* raster_;
		};   

	// ---------------------  End of class raster Iterator

	//----------------
	//! A raster iterator with a restriction of area 
	/*! 
		A structure that allows to cover the raster elements 
		(pixels) that are IN or OUT a specific region (polygon)  
	*/
	class TL_DLL iteratorPoly : public iterator
	{
		public:
			//! Constructor
			/*!
			\param colCurr	initial column
			\param linCurr	initial line
			\param nc		number of columns
			\param nl		number of lines
			\param nb		number of bands
			\param pt		pointer to the raster that will be covered
			\param poly		polygon that defines a specific region 
			\param str		strategic of the iterator, IN or OUT the polygon 
			\param linMin	minimal line of the raster that will be covered
			\param linMax	maximal line of the raster that will be covered
			\param colMin	minimal column of the raster that will be covered
			\param colMax	maximal column of the raster that will be covered
			\param seg		the segments of intersection of the current line 
			\param posSeg	the current segment 
			\param nlInPoly		number of lines IN the polygon
			\param nColsInPoly	number of columns IN the polygon 
			\param e			iterator end
			\param minLinCM		minimal line (in world coordinates)
			\param band			band to iterate
			*/
			iteratorPoly(int colCurr, int linCurr, int nc, int nl, int nb, TeRaster* pt, TePolygon& poly,
						 TeStrategicIterator str, double linMin=0.0, double linMax=0.0, double colMin=0.0, 
						 double colMax=0.0, TeCoordPairVect seg=TeCoordPairVect(),  
						 int posSeg=0, int nlInPoly=0, int nColsInPoly=0, bool e=true, double minLinCM=0.0, int band = 0):
				iterator(colCurr,linCurr,nc,nl,nb,pt),
				linMin_(linMin),
				linMax_(linMax),
				colMin_(colMin),
				colMax_(colMax),
				segments_(seg),
				posSegments_(posSeg),
				poly_(poly),
				end_(e),
				strategy_(str),
				nLinesInPoly_(nlInPoly),
				nColsInPoly_(nColsInPoly),
				linMinCM_(minLinCM),
				band_(band)
				{}
			
			//! empty contructor
			iteratorPoly(TeRaster* raster = 0): 
				iterator(0,0,0,0,0,raster),
				linMin_(0.0),
				linMax_(0.0),
				colMin_(0.0), 
				colMax_(0.0),
				posSegments_(0),
				end_(true),
				nLinesInPoly_(0),
				nColsInPoly_(0),
				band_(0)
				{}

			//! destructor
			virtual ~iteratorPoly() {}

			void moveForward();
			
			//! Prefix move forward operator
			iteratorPoly& operator++();

			//! Prefix move orward operator
			iteratorPoly operator++(int);

			//! Dereferencing operator
			/*!
				This operator was designed for compatibility reasons with standard
				containers of STL. 
				If there is a band restriction defined it will return the value associated to that band.
				Otherwise will return the value associated to band 0.
			*/
			double operator*(); 

			//! Returns the value of a given band of the element pointed by the iterator 
			double operator*(int band);
		
		    //! Returns the value of of the element pointed by the iterator as TeProperty structure
			/*!
				If there is a band restriction defined it will return the value associated to that band.
				Otherwise will return the value associated to band 0.
			*/
			bool getProperty(TeProperty& prop);

			//! Returns the last past one element in the polygon
			bool end();

			//! Returns the number of lines covered by the iterator
			int nLinesInPoly();

			//! Returns the number of columns covered by the iterator
			int nColsInPoly();

		private:

			//! calculate the segment of the current line that intersect the polygon  					
			void getNewSegment(int linCurr);

			double				linMin_, linMax_;  
			double				colMin_, colMax_;
			TeCoordPairVect		segments_;
			int					posSegments_;
			TePolygon			poly_;
			bool				end_;
			TeStrategicIterator strategy_;
			int					nLinesInPoly_, nColsInPoly_;
			double				linMinCM_;	//minimal line in world coordinates 
			int					band_;		// band restriction
	};

	//! Returns  an iterator to the first element of the raster
	iterator begin();

	//! Returns an iterator to the first element of the raster IN or OUT the polygon 
	/*!
		\param poly polygon that delimitates the iterator traversal
		\param st stragetegy used to decide if a pixel is in or out a polygon
		\param band optional band restriction
	*/
	iteratorPoly begin(TePolygon& poly, TeStrategicIterator st, int band=0);

	//! Returns the end past one position of the elements of the raster
    iterator end() ;
	//! Returns an iterator to the end element of the raster 
	/*!
		\param poly polygon that delimitates the iterator traversal
		\param st stragetegy used to decide if a pixel is in or out a polygon
		\param band optional band restriction
	*/
	iteratorPoly end(TePolygon& poly, TeStrategicIterator st, int band=0); 

	//! Select all blocks of raster, in a certain resolution factor that intercepts a given bounding box
	bool selectBlocks(TeBox& bb, int resFac, TeRasterParams& parBlock);

	//! Returns the number of blocks selected by the last block selection
	int numberOfSelectedBlocks();

	//! Returns the current block of a set selected by the last block selection
	bool fetchRasterBlock(TeDecoderMemory* memDec);

	//! Clears the current selection of a set selected by the last block selection
	void clearBlockSelection();

	//! An optimizated method to set values raster images
	/*
		This method should be used only to set the values of images with 3 bands associated
		respectively to the three colour channes Red, Green and Blue
		\param col column of the image
		\param lin line of the image
		\param Rval the value associated to the band 0 (associated to the R colour Channel)
		\param Gval the value associated to the band 1 (associated to the G colour Channel)
		\param Bval the value associated to the band 2 (associated to the B colour Channel)
		\param transp an optional transparency degree, with a range of 0 (totally transparent) to 255 (totally opaque)
		\return true if if succeed and false otherwise
	*/
	bool setElement(int col, int lin, double Rval, double Gval, double Bval, unsigned int /*transp*/ = 255);

	//! Give access to the last message detected by the raster manipulation
	const string& errorMessage() const;
	
  private :
  
	  //! Building a raster instance from another one is forbiden !
	  TeRaster( const TeRaster& ) : TeGeometry()
	  {}

	  //! Copying a raster instance from another one is forbiden !
	  const TeRaster& operator=( const TeRaster& ) { return *this; };	

};
	
inline TeRasterParams& TeRaster::params()
{ return params_; }

inline int TeRaster::nBands()
{ return params_.nBands(); }

inline TeGeomRep TeRaster::elemType() 
{ return TeRASTER; }

inline bool TeRaster::status()
{	return params_.status_ != TeRasterParams::TeNotReady; }

inline TeDecoder* TeRaster::decoder()
{ return decoder_; }

inline TeProjection* TeRaster::projection() 
{ return params_.projection(); }

inline TeCoord2D TeRaster::index2Coord(TeCoord2D pt)
{ return decoder_->index2Coord(pt); }

inline TeCoord2D TeRaster::coord2Index(TeCoord2D pt)
{ return decoder_->coord2Index(pt); }

	
inline const string& TeRaster::errorMessage() const
{	return params_.errorMessage_; }

inline int TeRaster::iterator::currentLine()
{ return linCurrent_;}

inline int TeRaster::iterator::currentColumn()
{return colCurrent_; }

inline int TeRaster::iterator::nBands(void)
{	return nBands_; }

inline bool TeRaster::iteratorPoly::end()
{	return end_; }

inline int TeRaster::iteratorPoly::nLinesInPoly()
{ return nLinesInPoly_;}

inline int TeRaster::iteratorPoly::nColsInPoly()
{ return nColsInPoly_;}

/** @defgroup RasterFunc Raster functions
	A set of functions that manipulates raster representations
 *  @{
 */
/**	Clips a raster representation using a polygon set as a mask
	\param	whole		pointer to the raster to be clipped
	\param  mask		the polygon set used as the mask to clip the raaster
	\param  geomProj	pointer the projection of the polygons used as mask
	\param  clipName	name of the file (or database table) that will contains the clipped raster
	\param  background	value to be used as the background of the clipped raster
	\param	decId		optional parameter that explicitly indicates the decoder (or format) used to generate the clipped raster
	\return a pointer to the generated clipped raster if it succeed or a null pointer otherwise.
	
	If the decId is equal to "DB" it is assumed that the clipped raster will be generated
	in the same database as the input raster. If the input raster is not in a database, the 
	routine returns a error.
*/
TL_DLL TeRaster*
TeRasterClipping(TeRaster* whole, TePolygonSet& mask, TeProjection* geomProj, const string& clipName, double background, const string& decId="");

//@}

#endif


