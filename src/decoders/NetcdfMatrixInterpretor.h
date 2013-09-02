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

/*! \file NetcdfMatrixInterpretor.h
    \brief Definition of the Template class NetcdfMatrixInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfMatrixInterpretor_H
#define NetcdfMatrixInterpretor_H

#include "magics.h"

#include "NetcdfMatrixInterpretorAttributes.h"
#include "NetcdfInterpretor.h"
#include "Matrix.h"
#include "PaperPoint.h"
#include "XmlNode.h"

namespace magics {

class NetcdfMatrixInterpretor: public NetcdfMatrixInterpretorAttributes, public NetcdfInterpretor {

public:
	NetcdfMatrixInterpretor();
	virtual ~NetcdfMatrixInterpretor();
    
    void set(const map<string, string>& params)
    { 
        MagLog::debug() << "NetcdfMatrixInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params); 
        NetcdfMatrixInterpretorAttributes::set(params);
    }
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfMatrixInterpretor::set(params)" << "\n";
        XmlNode netcdf = node;
         netcdf.name("netcdf");
         NetcdfInterpretorAttributes::set(netcdf); 
        NetcdfMatrixInterpretorAttributes::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        return NetcdfMatrixInterpretorAttributes::accept(node);
    }

    virtual NetcdfInterpretor* clone() const
    {
    	NetcdfMatrixInterpretor* object = new NetcdfMatrixInterpretor();
    	object->clone(*this);
    	return object;
    }

    void clone(const NetcdfMatrixInterpretor& )
//    void clone(const NetcdfMatrixInterpretor& other)
    {
    	NetcdfInterpretorAttributes::copy(*this); 
    	NetcdfMatrixInterpretorAttributes::copy(*this); 
    }
    virtual bool interpretAsMatrix(Matrix**);
    virtual bool interpretAsPoints(PointsList& points, const Transformation&);
    virtual void visit(Transformation&);
    virtual void getReady(const Transformation&);
    virtual void visit(MetaDataCollector&);
    virtual void visit(ValuesCollector&,PointsList&);

    virtual void statsData(map<string,vector<double> >&);
	virtual bool x();
	virtual bool y();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 Matrix* matrix_;
	 vector<double> columns_;
	 vector<double> rows_;
	 vector<double> dateRows_; // Used when dealing with date dimensions to store the original values
	 vector<double> dateColumns_; // Used when dealing with date dimensions to store the original values

	 string geoMinX_;
	 string geoMaxX_;
	 string geoMinY_;
	 string geoMaxY_;

private:
    //! Copy constructor - No copy allowed
	NetcdfMatrixInterpretor(const NetcdfMatrixInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
	NetcdfMatrixInterpretor& operator=(const NetcdfMatrixInterpretor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfMatrixInterpretor& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
