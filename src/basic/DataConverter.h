/*! \file DataConverter.h
    \brief Definition of the Template class DataConverter.
    
    Magics Team - ECMWF 2009
    
    Started: Wed 22-Jul-2009
    
    Changes:
    
*/

#ifndef DataConverter_H
#define DataConverter_H

#include "magics.h"
#include "Data.h"


namespace magics {

class DataConverter : public Data {

public:
	DataConverter(Data*);
	virtual ~DataConverter();
    virtual MatrixHandler& matrix() {  throw MethodNotYetImplemented("DataConverter::matrix");}

    //! Methods to access the data as a 2Dmatrix Used by pwind action routine
    virtual MatrixHandler& xComponent() {  throw MethodNotYetImplemented("DataConverter::yComponent");}
    virtual MatrixHandler& yComponent() { throw MethodNotYetImplemented("DataConverter::yComponent"); }

    //! Method to access the data as a list of points : Used by psymb.
    virtual PointsHandler& points();


    virtual void getReady(const Transformation&) {}
    virtual void visit(Transformation&)          {}


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     Data* data_;
     PointsList points_;
private:
    //! Copy constructor - No copy allowed
	DataConverter(const DataConverter&);
    //! Overloaded << operator to copy - No copy allowed
	DataConverter& operator=(const DataConverter&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const DataConverter& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
