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

/*! \file NetcdfInterpretor.h
    \brief Definition of the Template class NetcdfInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfInterpretor_H
#define NetcdfInterpretor_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "UserPoint.h"
#include "MatrixHandler.h"
#include "NetcdfInterpretorAttributes.h"
#include "CustomisedPoint.h"
#include "TagHandler.h"
#include "TextVisitor.h"

namespace magics {

class RasterData;

class Netcdf;
class NetcdfInterpretor: public NetcdfInterpretorAttributes {

public:
	NetcdfInterpretor();
	virtual ~NetcdfInterpretor();
	virtual void visit(Transformation&) {}
	virtual void getReady(const Transformation&) {}
    virtual bool interpretAsMatrix(Matrix**)
    	{ MagLog::dev() << "Method  NetcdfInterpretor::interpretAsMatrix() --> Not yet implemented.\n"; }
    virtual bool interpretAsVectors(Matrix**, Matrix**)
    	{ MagLog::dev() << "Method  NetcdfInterpretor::interpretAsVectors() --> Not yet implemented.\n"; }
    virtual bool interpretAsRaster(RasterData&)
    	{ MagLog::dev() << "Method  NetcdfInterpretor::interpretAsRaster() --> Not yet implemented.\n"; }
    virtual bool interpretAsPoints(PointsList&)
    	{ MagLog::dev() << "Method  NetcdfInterpretor::interpretAsPoints() --> Not yet implemented.\n"; }
    virtual void customisedPoints(const std::set<string>&, CustomisedPointsList&)  
        	{ MagLog::dev() << "Method  NetcdfInterpretor::customisedPoints() --> Not yet implemented.\n"; }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&)  
            	{ MagLog::dev() << "Method  NetcdfInterpretor::customisedPoints() --> Not yet implemented.\n"; }

    virtual bool interpretAsPoints(PointsList& points, const Transformation&)
    	{ interpretAsPoints(points);}
    virtual void set(const map<string, string>& params) { NetcdfInterpretorAttributes::set(params); }
    virtual void set(const XmlNode& node) { NetcdfInterpretorAttributes::set(node); }
     virtual bool accept(const string& node) { return NetcdfInterpretorAttributes::accept(node); }
    virtual NetcdfInterpretor* clone() const {
    	NetcdfInterpretor* object = new NetcdfInterpretor();
    	object->copy(*this);
    	return object;
    }
    
    virtual void statsData(map<string,vector<double> >&) {}
    virtual void visit(MetaDataCollector&) {}
    virtual void visit(ValuesCollector&,PointsList&) {};
    virtual void visit(TextVisitor&);
    double missing(Netcdf&) const;
    string getAttribute(const string&, const string&, const string&);
	bool cf_date(Netcdf& netcdf, const string&, const string&, string&, vector<double>&, vector<double>&);
	//return true, if the the data is date CF-compliant and the date axis has been set
	bool reference_date(Netcdf& netcdf, const string&, const string&, string&, vector<double>&, vector<double>&);
	//return true, if the the data is Metview-date compliant and the date axis has been set

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     void setDimensions(const stringarray&, map<string, string>& first, map<string, string>& last);
     void getAttributes(Netcdf&,const string&,string&,string&);

     string baseDateX_;
     string baseDateY_;
     string refDateX_;
     string refDateY_;

private:
    //! Copy constructor - No copy allowed
	NetcdfInterpretor(const NetcdfInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	NetcdfInterpretor& operator=(const NetcdfInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfInterpretor& p)
		{ p.print(s); return s; }
};
template<>
class MagTranslator<string, NetcdfInterpretor> { 
public:
	NetcdfInterpretor* operator()(const string& val ) {
		 return SimpleObjectMaker<NetcdfInterpretor>::create(val);
	}

	NetcdfInterpretor* magics(const string& param)
	{
		NetcdfInterpretor* object;
		ParameterManager::update(param, object);
		return object;
	}

};



class NetcdfTag: public XmlNodeVisitor
{
public:
	NetcdfTag(NetcdfInterpretor& netcdf, TagHandler& title) : netcdf_(netcdf), title_(title) {}

	~NetcdfTag() {}

	void visit(const XmlNode& node);

	void decode(const string& line);
     string str() const { return out.str(); }
protected :
	NetcdfInterpretor& netcdf_;
	TagHandler& title_;
	ostringstream out;
};
} // namespace magics
#endif
