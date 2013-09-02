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

/*! \file PpmDecoder.h
    \brief Definition of the Template class PpmDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 18-May-2005
    
    Changes:
    
*/

#ifndef PpmDecoder_H
#define PpmDecoder_H

#include "magics.h"

#include "PpmDecoderAttributes.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"

namespace magics {


class PpmDecoder: public PpmDecoderAttributes, public Decoder, public Data {

public:
	PpmDecoder();
	virtual ~PpmDecoder();
	  // implements BaseSceneObject interface
    virtual void set(const map<string, string>& params) 
    	{ PpmDecoderAttributes::set(params); }
  
    
    // implements Decoder interface
    void decode();
    void decodeRaster();
    void decodePoints();
    
    // implements Decoder  
    void visit(TitleBase&) {}
     
    PointsHandler<UserPoint>& points() { 
        decodePoints();
        PointsHandler<UserPoint> x(points_);
        pointsHandlers_.push_back(new PointsHandler<UserPoint>(points_)); 
        return *(pointsHandlers_.back()); 
    } 
    
    MatrixHandler<UserPoint>& matrix() {
        decode();
        matrixHandlers_.push_back(new MatrixHandler<UserPoint>(matrix_));         
        return *(matrixHandlers_.back()); 
    } 
  
    
    RasterData&  raster() {
        decodeRaster();
        return raster_; 
    } 
    

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 mutable Matrix     matrix_;  
     mutable RasterData raster_;
     mutable PointsList<UserPoint> points_;

private:
    //! Copy constructor - No copy allowed
	PpmDecoder(const PpmDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	PpmDecoder& operator=(const PpmDecoder&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PpmDecoder& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
