/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImportObjectHandler.h
    \brief Definition of the Template class ImportObjectHandler.
    
    Magics Team - ECMWF 2010
    
    Started: Wed 10-Feb-2010
    
    Changes:
    
*/

#ifndef ImportObjectHandler_H
#define ImportObjectHandler_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "Layout.h"
#include "ImportObjectHandlerAttributes.h"

namespace magics {

class ImportObjectHandler: public BasicSceneNode, 
	public Layout, 
	public ImportObjectHandlerAttributes 
{

public:
	ImportObjectHandler();
	virtual ~ImportObjectHandler();
	
	void getReady();  
	void visit(SceneLayer&, vector<LayoutVisitor*>&);
	void visit(MetaDataCollector& collector);
	void set(const XmlNode& node) { ImportObjectHandlerAttributes::set(node); }
	void set(const map<string, string>& map) { ImportObjectHandlerAttributes::set(map); }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	ImportObjectHandler(const ImportObjectHandler&);
    //! Overloaded << operator to copy - No copy allowed
	ImportObjectHandler& operator=(const ImportObjectHandler&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ImportObjectHandler& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
