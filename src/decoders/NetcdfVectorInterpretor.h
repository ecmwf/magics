/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfVectorInterpretor.h
    \brief Definition of the Template class NetcdfVectorInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfVectorInterpretor_H
#define NetcdfVectorInterpretor_H

#include "magics.h"


#include "NetcdfInterpretor.h"
#include "PaperPoint.h"
#include "UserPoint.h"
#include "CustomisedPoint.h"
#include "XmlNode.h"
namespace magics {


class NetcdfVectorInterpretor:  public NetcdfInterpretor {

public:
	NetcdfVectorInterpretor();
	virtual ~NetcdfVectorInterpretor();
    
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfVectorInterpretor::set(params)" << "\n";
        XmlNode netcdf(node);
        netcdf.name("netcdf");
        NetcdfInterpretor::set(netcdf); 
        
        NetcdfInterpretor::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        return magCompare(node, "vector");
    }

    virtual NetcdfInterpretor* clone() const
    {
    	NetcdfVectorInterpretor* object = new NetcdfVectorInterpretor();
    	object->clone(*this);
    	return object;
    }

    void clone(const NetcdfVectorInterpretor& )
//    void clone(const NetcdfVectorInterpretor& other)
    {
    	NetcdfInterpretor::copy(*this); 
    	
    }
    virtual void customisedPoints(const std::set<string>&, CustomisedPointsList&);
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int);
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
};

class NetcdfGeoVectorInterpretor: public NetcdfInterpretor {

public:
	NetcdfGeoVectorInterpretor();
	virtual ~NetcdfGeoVectorInterpretor();
    
    
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfGeoVectorInterpretor::set(params)" << "\n";
        XmlNode netcdf(node);
        netcdf.name("netcdf");
        NetcdfInterpretor::set(netcdf); 
        NetcdfInterpretor::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        return magCompare(node,  "geovector");
    }
    static NetcdfInterpretor* guess(const NetcdfInterpretor& from);
    virtual NetcdfInterpretor* clone() const
    {
    	NetcdfGeoVectorInterpretor* object = new NetcdfGeoVectorInterpretor();
    	object->clone(*this);
    	return object;
    }

    void clone(const NetcdfGeoVectorInterpretor& )
//    void clone(const NetcdfGeoVectorInterpretor& other)
    {
    	NetcdfInterpretor::copy(*this); 
    	
    }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int);
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
  

// 
};



class NetcdfGeoPolarMatrixInterpretor: public NetcdfInterpretor {

public:
	NetcdfGeoPolarMatrixInterpretor();
	virtual ~NetcdfGeoPolarMatrixInterpretor();
    
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfGeoPolarMatrixInterpretor::set(params)" << "\n";
        XmlNode netcdf(node);
        netcdf.name("netcdf");
        NetcdfInterpretor::set(netcdf); 
        NetcdfInterpretor::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretor::accept(node) ) 
        	return true; 
        return magCompare(node, "geopolarvector");
    }

    virtual NetcdfInterpretor* clone() const
    {
    	NetcdfGeoPolarMatrixInterpretor* object = new NetcdfGeoPolarMatrixInterpretor();
    	object->clone(*this);
    	return object;
    }

    void clone(const NetcdfGeoPolarMatrixInterpretor& )
//    void clone(const NetcdfGeoPolarMatrixInterpretor& other)
    {
    	NetcdfInterpretor::copy(*this); 
    
    }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
  

// 
};



} // namespace magics
#endif
