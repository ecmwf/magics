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

/*! \file OdaDecoder.h
    \brief Definition of the Template class OdaDecoder.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 16-Jan-2004
    
    Changes:
    
*/

#ifndef OdaDecoder_H
#define OdaDecoder_H

#include "magics.h"

#include "OdaGeoDecoderAttributes.h"
#include "OdaXYDecoderAttributes.h"
#include "Decoder.h"
#include "Data.h"
#include "UserPoint.h"
#include "UserPoint.h"
#include "BinningObject.h"

namespace magics {


class OdaMagException : public MagicsException
{
public:
	 OdaMagException( const string& why ):
		MagicsException("Oda MagException :  " + why ) {}
};  

class OdaGeoDecoder: public OdaGeoDecoderAttributes, 
                  public Decoder, 
                  public Data,
                  public PointsList
{

public:
	OdaGeoDecoder();
	virtual ~OdaGeoDecoder();
    
    //! Decoder interface
    virtual void decode(const Transformation&);
    virtual void decode();
    virtual void set(const map<string, string>& map ) { OdaGeoDecoderAttributes::set(map); }
    virtual void set(const XmlNode& node ) { OdaGeoDecoderAttributes::set(node); }
    
   PointsHandler& points(const Transformation& transformation) {
    	decode(transformation);
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    } 
   PointsHandler& points(const Transformation& transformation, bool) {
    	decode(transformation);
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    }
   PointsHandler& points() {
    	decode();
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    }
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& ); 
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    // implements Automatic title
    void visit(TextVisitor&);
    void visit(Transformation&);
    void visit(ValuesCollector&);
    void visit(MetaDataCollector&);
    void initInfo();	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void position(double& lat, double& lon);
	 
	 CustomisedPointsList customisedPoints_;

private:
    //! Copy constructor - No copy allowed
	OdaGeoDecoder(const OdaGeoDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	OdaGeoDecoder& operator=(const OdaGeoDecoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const OdaGeoDecoder& p)
		{ p.print(s); return s; }

};
class OdaXYDecoder: public OdaXYDecoderAttributes, 
                  public Decoder, 
                  public Data,
                  public PointsList
{
public:
	OdaXYDecoder();
	virtual ~OdaXYDecoder();
    
    //! Decoder interface
    virtual void decode(const Transformation&);
    virtual void decode();
    virtual void set(const map<string, string>& map ) { OdaXYDecoderAttributes::set(map); }
    virtual void set(const XmlNode& node ) { OdaXYDecoderAttributes::set(node); }
    void visit(Transformation&);
    MatrixHandler& matrix();

    
   PointsHandler& points(const Transformation& transformation) {
    	decode(transformation);
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    } 
    PointsHandler& points() {
    	decode();
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    } 
    PointsHandler& points(const Transformation& transformation, bool) {
    	decode(transformation);
    	pointsHandlers_.push_back(new PointsHandler(*this));
    	return *(pointsHandlers_.back());
    }
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool);
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& ); 
    void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    // implements Automatic title
    void visit(TextVisitor&);
    void visit(ValuesCollector&);
    void visit(MetaDataCollector&);
    void initInfo();	


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 Matrix *matrix_;


private:
    //! Copy constructor - No copy allowed
	OdaXYDecoder(const OdaXYDecoder&);
    //! Overloaded << operator to copy - No copy allowed
	OdaXYDecoder& operator=(const OdaXYDecoder&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const OdaXYDecoder& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
