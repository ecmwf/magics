
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileXmlViewNodeAttributes.h
    \brief Definition of XmlViewNode Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef XmlViewNodeAttributes_H
#define XmlViewNodeAttributes_H

#include "magics.h"
#include "Transformation.h"
namespace magics {

class XmlNode;
class XmlViewNodeAttributes 
{
public:
//  --  constructor
    XmlViewNodeAttributes();
    
//  --  destructor
    virtual ~XmlViewNodeAttributes();
    
    virtual void set(const std::map<std::string, std::string>&);
    virtual void set(const XmlNode&);
    virtual void copy(const XmlViewNodeAttributes&);
    virtual bool accept(const std::string&);

    void setTag(const std::string& tag) { tag_ = tag; }

public:
	//  --  method
	virtual void print(std::ostream&) const;
	virtual void toxml(std::ostream& out) const;
	//  --  members:
	string tag_;
	string fitted_;
	unique_ptr<Transformation> transformation_;
	 

private:
	friend ostream& operator<<(ostream& s,const XmlViewNodeAttributes& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif
