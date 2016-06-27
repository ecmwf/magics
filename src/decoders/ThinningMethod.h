/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ThinningMethod.h
    \brief Definition of the Template class ThinningMethod.
    
    Magics Team - ECMWF 2010
    
    Started: Thu 28-Oct-2010
    
    Changes:
    
*/

#ifndef ThinningMethod_H
#define ThinningMethod_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"

#include "CustomisedPoint.h"
#include "Transformation.h"


namespace magics {

class Data;
class UserPoint;
class UserPoint;

struct ThinningMethodUI {
	int nbPoints_; // For automatic Method by cm !
	int factor_;
	bool rawOnly_;
	double  height_; // hight of the plit in  cm !
	double  width_; // hight of the plit in  cm !
};

class ThinningMethod {

public:
	ThinningMethod();
	virtual ~ThinningMethod();
    
	void set2D() { twoD_ = false; }
	
	virtual void set(const ThinningMethodUI&) {}
	virtual void operator()(Data&, const Transformation&, const std::set<string>&, CustomisedPointsList&);

	double units() const { return units_; }
	void units(double units) const { units_ = units; }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 bool twoD_;
	 mutable double units_;
private:
    //! Copy constructor - No copy allowed
	ThinningMethod(const ThinningMethod&);
    //! Overloaded << operator to copy - No copy allowed
	ThinningMethod& operator=(const ThinningMethod&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ThinningMethod& p)
		{ p.print(s); return s; }

};

class AutomaticThinningMethod: public ThinningMethod
{
public:
	AutomaticThinningMethod();
	virtual ~AutomaticThinningMethod();
	
	virtual void set(const ThinningMethodUI&);
	virtual void operator()(Data&, const Transformation&, const std::set<string>&, CustomisedPointsList&);

	int points() const { return nbPoints_; }
	void points(int points) { nbPoints_ = points; }
	bool rawOnly() const { return rawOnly_; }

	int x() const { return x_; }
	int y() const { return y_; }

protected: 
	int nbPoints_;
	bool rawOnly_;
	int x_;
	int y_;

};

class BasicThinningMethod: public ThinningMethod
{
public:
	BasicThinningMethod();
	virtual ~BasicThinningMethod();
	
	virtual void set(const ThinningMethodUI&);
	virtual void operator()(Data&, const Transformation&, const std::set<string>&, CustomisedPointsList&);

    int factor() const { return factor_; }
    void factor(int factor) { factor_ = factor; }
protected: 
	int factor_;
};

template <>
class MagTranslator<string, ThinningMethod> { 
public:
	ThinningMethod* operator()(const string& val )
	{
		return SimpleObjectMaker<ThinningMethod>::create(val);
	}     

	ThinningMethod* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
