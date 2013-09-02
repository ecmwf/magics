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
