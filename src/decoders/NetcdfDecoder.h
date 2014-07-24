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

/*! \file NetcdfDecoder.h
    \brief Definition of the Template class NetcdfDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfDecoder_H
#define NetcdfDecoder_H

#include "magics.h"
#include "AnimationRules.h"

#include "Decoder.h"
#include "NetcdfDecoderAttributes.h"
#include "UserPoint.h"
#include "Data.h"
#include "PointsHandler.h"
#include "Factory.h"
#include "MagTranslator.h"



namespace magics {

class Transformation;


class NetcdfDecoder: public Decoder,
                     public Data,
                     public NetcdfDecoderAttributes
{

public:
	NetcdfDecoder();
	virtual ~NetcdfDecoder();
    
    void decode() {}
    // implements BaseSceneObject interface
    virtual void set(const map<string, string>& params) { NetcdfDecoderAttributes::set(params); }
    virtual void set(const XmlNode& node) { NetcdfDecoderAttributes::set(node); }
    virtual void visit(MagnifierVisitor&); 
    
    virtual PointsHandler& points(const Transformation&, bool);

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
       {
       	customisedPoints(t, n, out);
       }

    void getReady(const Transformation& transformation)
    {
    	(*interpretor_).getReady(transformation);
    }

    void visit(Transformation& transformation) {
    	 (*interpretor_).visit(transformation);    
    }
    
    MatrixHandler& matrix() {
        MagLog::dev() << "NetcdfDecoder::matrix! " << "\n";
        if ( !data_ )
        	valid_ = (*interpretor_).interpretAsMatrix(&data_);
        this->matrixHandlers_.push_back(new MatrixHandler(*data_));
        return *(this->matrixHandlers_.back());
    } 
    
    void customisedPoints(const std::set<string>& request, CustomisedPointsList& out) {
    	(*interpretor_).customisedPoints(request, out); 
    }
    void customisedPoints(const Transformation& transformation, const std::set<string>& request, CustomisedPointsList& out) {
       	(*interpretor_).customisedPoints(transformation, request, out); 
       }
    
    void visit(AnimationStep& step) {
    	   try {
    		   MatrixHandler& data = matrix() ;
    		   // Information about contains...
    		   	MagLog::dev() << "Netcdf::visit(AnimationRules&) --> " << endl;
    		   	
    		
    		   	step.xResolution(abs(data.XResolution()));
    		   	step.yResolution(abs(data.YResolution()));
    	         }
    	   catch (...)
    	   {
    		   
    	   }
    	   
    	        
    	         
    	
    }
    void visit(MetaDataCollector&);
    void visit(ValuesCollector&);
    void visit(TextVisitor&);
    
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 
	 PointsList points_;
	 Matrix*     data_;

private:
	//! Copy constructor - No copy allowed
	NetcdfDecoder(const NetcdfDecoder&);
	//! Overloaded << operator to copy - No copy allowed
	NetcdfDecoder& operator=(const NetcdfDecoder&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfDecoder& p)
		{ p.print(s); return s; }
};


class NetcdfLoop : public DataLoop
{
public:
	NetcdfLoop(NetcdfDecoder* netcdf): netcdf_(netcdf) {}
	virtual ~NetcdfLoop() {}
	void set(const map<string, string>& map) { } // NetcdfLoopAttributes::set(map); }
	void set(const XmlNode& node) { } // NetcdfLoopAttributes::set(node); }
	
    void set(NetcdfDecoder* netcdf) { netcdf_ = netcdf; }
	
	Data* current() {
		NetcdfDecoder* current = netcdf_;
		netcdf_ = 0;
		return current;
	}
	bool         hasMore() {
		return (netcdf_ != 0);
	}
	void         next() { netcdf_ = 0; }
	

   void visit(Transformation& transformation) { if ( netcdf_ ) netcdf_->visit(transformation); }
	

protected:
	virtual void print(ostream&) const {}
	NetcdfDecoder* netcdf_;
	
	
	
	
private:
	//! Copy constructor - No copy allowed
	NetcdfLoop(const NetcdfLoop&);
	//! Overloaded << operator to copy - No copy allowed
	NetcdfLoop& operator=(const NetcdfLoop&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfLoop& p)
		{ p.print(s); return s; }
	
};



} // namespace magics
#endif
