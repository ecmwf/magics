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

/*! \file NetcdfGeoMatrixInterpretor.h
    \brief Definition of the Template class NetcdfGeoMatrixInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfGeoMatrixInterpretor_H
#define NetcdfGeoMatrixInterpretor_H

#include "magics.h"

#include "NetcdfGeoMatrixInterpretorAttributes.h"
#include "NetcdfInterpretor.h"
#include "Matrix.h"
#include "XmlNode.h"


namespace magics {

class NetcdfGeoMatrixInterpretor: public NetcdfGeoMatrixInterpretorAttributes, public NetcdfInterpretor {

public:
	NetcdfGeoMatrixInterpretor();
	virtual ~NetcdfGeoMatrixInterpretor();
    
    void set(const map<string, string>& params) { 
        MagLog::debug() << "NetcdfGeoMatrixInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(params); 
        NetcdfGeoMatrixInterpretorAttributes::set(params);
    }
    void set(const XmlNode& node) { 
        MagLog::debug() << "NetcdfGeoMatrixInterpretor::set(params)" << "\n";
        NetcdfInterpretorAttributes::set(node); 
        XmlNode netcdf = node;
        netcdf.name("netcdf");
        NetcdfInterpretorAttributes::set(netcdf); 
        NetcdfGeoMatrixInterpretorAttributes::set(node);
    }
	virtual NetcdfInterpretor* clone() const {
    	NetcdfGeoMatrixInterpretor* object = new NetcdfGeoMatrixInterpretor();
    	object->clone(*this);
    	return object;
    }
    void clone(const NetcdfGeoMatrixInterpretor& other) {
    	NetcdfInterpretorAttributes::copy(other); 
    	NetcdfGeoMatrixInterpretorAttributes::copy(other); 
    }
    bool interpretAsMatrix(Matrix**);
    bool interpretAsPoints(PointsList&);

    virtual void statsData(map<string,vector<double> >&);
    virtual void visit(MetaDataCollector&);
    virtual void visit(ValuesCollector&,PointsList&); 

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 Matrix* matrix_;
    
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
