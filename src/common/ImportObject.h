/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImportObject.h
    \brief Definition of the Template class ImportObject.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 6-Apr-2005
    
    Changes:
    
*/

#ifndef ImportObject_H
#define ImportObject_H

#include "magics.h"
#include "ImageProperties.h"
#include "BaseDriver.h"

namespace magics {



class ImportObject: public ImageProperties {

public:
	ImportObject() {}
	virtual ~ImportObject() {}
	
	// Implement the BaseGraphics Interface 
	virtual void redisplay(const BaseDriver& driver) const { driver.redisplay(*this); } 
	
	void setPath(const string& path) { path_ = path; }
	string getPath() const          { return path_; }
	
	void setFormat(const string& format) { format_ = format; }
	string getFormat() const          { return format_; }


protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}
	 string path_;
	 string format_;

private:
	//! Copy constructor - No copy allowed
	ImportObject(const ImportObject&);
	//! Overloaded << operator to copy - No copy allowed
	ImportObject& operator=(const ImportObject&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ImportObject& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
