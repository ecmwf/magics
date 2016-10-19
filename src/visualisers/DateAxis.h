/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DateAxisMethod.h
    \brief Definition of the Template class DateAxisMethod.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/

#ifndef DateAxisMethod_H
#define DateAxisMethod_H

#include "magics.h"

#include "AxisMethod.h"
#include "DateAxisMethodAttributes.h"

namespace magics {

class DateAxisMethod: public AxisMethod, public DateAxisMethodAttributes {

public:
	DateAxisMethod();
	virtual ~DateAxisMethod();
	
	virtual void set(const map<string, string>& map) {
		AxisMethod::set(map);
		DateAxisMethodAttributes::set(map);
	}
	virtual void set(const XmlNode& node) {
		AxisMethod::set(node);
		DateAxisMethodAttributes::set(node);
	}
	virtual double getMin() const;
    virtual double getMax() const;
    
     
    void prepare(const Axis&, AxisItems&);
   
    void updateX(const Transformation&);
    void updateY(const Transformation&);


protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void update() const;
	 mutable DateTime from_;
	 mutable DateTime to_;
	 mutable DateTime base_;
	double interval_;

	typedef void (DateAxisMethod::*DateHelper)(AxisItems&);

	map<string,  DateHelper> dateCreators_;
	 
	void automatic(AxisItems&);
	void years(AxisItems&);
	void months(AxisItems&);
	void days(AxisItems&);
	void hours(AxisItems&);

	void years_label(AxisItems&, AxisItems&);
	void months_label(AxisItems&, AxisItems&);
	void days_label(AxisItems&, AxisItems&);
	void hours_label(AxisItems&, AxisItems&);


private:
    //! Copy constructor - No copy allowed
	DateAxisMethod(const DateAxisMethod&);
    //! Overloaded << operator to copy - No copy allowed
	DateAxisMethod& operator=(const DateAxisMethod&);
	int position_;
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const DateAxisMethod& p)
		{ p.print(s); return s; }

};

class HyperAxis: public AxisMethod {

public:
	HyperAxis();
	virtual ~HyperAxis();
	
	void set(const map<string, string>& map) {  }
	void set(const XmlNode& node)                  { }
	
    void prepare(double, double, AxisItems&) const;
    void updateX(const Transformation&);
    void updateY(const Transformation&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void update() const;
	
	 
private:
    //! Copy constructor - No copy allowed
	HyperAxis(const DateAxisMethod&);
    //! Overloaded << operator to copy - No copy allowed
	HyperAxis& operator=(const DateAxisMethod&);



};

} // namespace magics
#endif
