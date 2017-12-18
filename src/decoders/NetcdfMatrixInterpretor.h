/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfMatrixInterpretor.h
    \brief Definition of the Template class NetcdfMatrixInterpretor.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfMatrixInterpretor_H
#define NetcdfMatrixInterpretor_H

#include "magics.h"


#include "NetcdfInterpretor.h"
#include "Matrix.h"
#include "PaperPoint.h"
#include "XmlNode.h"

namespace magics {

class NetcdfMatrixInterpretor:  public NetcdfInterpretor {

public:
	NetcdfMatrixInterpretor();
	virtual ~NetcdfMatrixInterpretor();
    
   
    
    void set(const XmlNode& node)
    { 
        MagLog::debug() << "NetcdfMatrixInterpretor::set(params)" << "\n";
        XmlNode netcdf = node;
         netcdf.name("netcdf");
        NetcdfInterpretor::set(netcdf); 
        NetcdfInterpretor::set(node);
    }
    
    bool accept(const string& node)
    { 
        if ( NetcdfInterpretorAttributes::accept(node) ) 
        	return true; 
        if ( magCompare(node, "matrix")  )
            return true;

       
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
    	NetcdfInterpretor::copy(*this); 
    	
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
	void customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, int thinning);

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
