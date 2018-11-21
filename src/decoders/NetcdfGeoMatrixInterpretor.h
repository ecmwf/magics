/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfGeoMatrixInterpretor.h
    \brief Definition of the Template class NetcdfGeoMatrixInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfGeoMatrixInterpretor_H
#define NetcdfGeoMatrixInterpretor_H

#include "magics.h"

#include "NetcdfInterpretor.h"
#include "Matrix.h"
#include "XmlNode.h"
#include <proj_api.h>

namespace magics {

class NetcdfGeoMatrixInterpretor:  public NetcdfInterpretor {

public:
	NetcdfGeoMatrixInterpretor();
	virtual ~NetcdfGeoMatrixInterpretor();
    
   
    static NetcdfInterpretor* guess(const NetcdfInterpretor&);
    void visit(Transformation& transformation);

    void set(const XmlNode& node) { 
        MagLog::debug() << "NetcdfGeoMatrixInterpretor::set(params)" << "\n";
        set(node); 
        XmlNode netcdf = node;
        netcdf.name("netcdf");
        set(netcdf); 
        
    }
	virtual NetcdfInterpretor* clone() const {
    	NetcdfGeoMatrixInterpretor* object = new NetcdfGeoMatrixInterpretor();
    	object->clone(*this);
    	return object;
    }
    void clone(const NetcdfGeoMatrixInterpretor& other) {
    	copy(other); 
    	
    }
    bool interpretAsMatrix(Matrix**);
    bool interpretAsPoints(PointsList&);
    UserPoint* newPoint(const string&, double, double, double);
    virtual void statsData(map<string,vector<double> >&);
    virtual void visit(MetaDataCollector&);
    virtual void visit(ValuesCollector&,PointsList&); 
    void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int);
    string proj4Detected(Netcdf& netcdf);
    void checkProj4Units(Netcdf& netcdf, const string& variable, vector<double>& data);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	Matrix* matrix_;
    projPJ proj4_;
    projPJ latlon_;    

private:
    //! Copy constructor - No copy allowed
	NetcdfGeoMatrixInterpretor(const NetcdfGeoMatrixInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	NetcdfGeoMatrixInterpretor& operator=(const NetcdfGeoMatrixInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfGeoMatrixInterpretor& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
