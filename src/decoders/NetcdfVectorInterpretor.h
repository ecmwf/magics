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

/*! \file NetcdfVectorInterpretor.h
    \brief Definition of the Template class NetcdfVectorInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfVectorInterpretor_H
#define NetcdfVectorInterpretor_H

#include "magics.h"

#include "NetcdfVectorInterpretorAttributes.h"
#include "NetcdfGeoVectorInterpretorAttributes.h"
#include "NetcdfGeoPolarMatrixInterpretorAttributes.h"
#include "NetcdfInterpretor.h"
#include "PaperPoint.h"
#include "UserPoint.h"
#include "CustomisedPoint.h"
#include "XmlNode.h"
namespace magics {


class NetcdfVectorInterpretor: public NetcdfVectorInterpretorAttributes, public NetcdfInterpretor {

public:
	NetcdfVectorInterpretor();
	virtual ~NetcdfVectorInterpretor();
    
    void set(const map<string, string>& params)
    { 
        MagLog::debug() << "NetcdfVectorInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params); 
        NetcdfVectorInterpretorAttributes::set(params);
    }
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfVectorInterpretor::set(params)" << "\n";
        XmlNode netcdf(node);
        netcdf.name("netcdf");
        NetcdfInterpretorAttributes::set(netcdf); 
        
        NetcdfVectorInterpretorAttributes::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        return NetcdfVectorInterpretorAttributes::accept(node);
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
    	NetcdfInterpretorAttributes::copy(*this); 
    	NetcdfVectorInterpretorAttributes::copy(*this); 
    }
    virtual void customisedPoints(const std::set<string>&, CustomisedPointsList&);
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
};

class NetcdfGeoVectorInterpretor: public NetcdfGeoVectorInterpretorAttributes, public NetcdfInterpretor {

public:
	NetcdfGeoVectorInterpretor();
	virtual ~NetcdfGeoVectorInterpretor();
    
    void set(const map<string, string>& params)
    { 
        MagLog::debug() << "NetcdfGeoVectorInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params); 
        NetcdfGeoVectorInterpretorAttributes::set(params);
    }
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfGeoVectorInterpretor::set(params)" << "\n";
        XmlNode netcdf(node);
        netcdf.name("netcdf");
        NetcdfInterpretorAttributes::set(netcdf); 
        NetcdfGeoVectorInterpretorAttributes::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        return NetcdfGeoVectorInterpretorAttributes::accept(node);
    }

    virtual NetcdfInterpretor* clone() const
    {
    	NetcdfGeoVectorInterpretor* object = new NetcdfGeoVectorInterpretor();
    	object->clone(*this);
    	return object;
    }

    void clone(const NetcdfGeoVectorInterpretor& )
//    void clone(const NetcdfGeoVectorInterpretor& other)
    {
    	NetcdfInterpretorAttributes::copy(*this); 
    	NetcdfGeoVectorInterpretorAttributes::copy(*this); 
    }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
  

// 
};



class NetcdfGeoPolarMatrixInterpretor: public NetcdfGeoPolarMatrixInterpretorAttributes, public NetcdfInterpretor {

public:
	NetcdfGeoPolarMatrixInterpretor();
	virtual ~NetcdfGeoPolarMatrixInterpretor();
    
    void set(const map<string, string>& params)
    { 
        MagLog::debug() << "NetcdfGeoPolarMatrixInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params); 
        NetcdfGeoPolarMatrixInterpretorAttributes::set(params);
    }
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfGeoPolarMatrixInterpretor::set(params)" << "\n";
        XmlNode netcdf(node);
        netcdf.name("netcdf");
        NetcdfInterpretorAttributes::set(netcdf); 
        NetcdfGeoPolarMatrixInterpretorAttributes::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        return NetcdfGeoPolarMatrixInterpretorAttributes::accept(node);
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
    	NetcdfInterpretorAttributes::copy(*this); 
    	NetcdfGeoPolarMatrixInterpretorAttributes::copy(*this); 
    }
    virtual void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&);
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
  

// 
};



} // namespace magics
#endif
